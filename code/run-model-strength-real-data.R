library(ggplot2)
library(tidyr)
library(dplyr)
library(summarytools)
library(rstan)
library(ggmcmc)
rm(list = ls())
source("code/function-horseracing.R")
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

set.seed(123)

# d <- read.csv(file='../real/input/horseracing2000-2018.csv',header = TRUE)
# 
# #対象レースを選択する
# d_select <- d %>% dplyr::filter(between(年,17,19))
# write.csv(d_select,'input/horseracing2017-2019.csv',quote = FALSE, row.names = FALSE)


d_select <- read.csv(file='input/horseracing2017-2019.csv',header = TRUE) %>%
    dplyr::filter(between(年,17,19) &
                    クラス名 %in% c("Ｇ１","Ｇ２","Ｇ３","ｵｰﾌﾟﾝ") &
                    確定着順 != 0) %>%
    dplyr::mutate(horseID  = as.numeric(factor(馬名,levels=unique(馬名))),
                  jockeyID = as.numeric(factor(騎手,levels=unique(騎手))),
                  race     = factor(paste(日付Ｓ,場所,レース番号,sep = '_'))) %>%
    dplyr::rename(OoA = 確定着順) %>%
    dplyr::arrange(レースＩＤ.新) %>%
    dplyr::mutate(RaceID = as.numeric(factor(race,levels=unique(race))))


d_predall  <- d_select  %>% dplyr::filter(クラス名 %in% c("Ｇ１") & 年 == 18 & 月 == 12) 
ID_predall <- d_predall %>% dplyr::distinct(RaceID) %>% as.matrix()

BettingResults <- list()
for (k in 1:length(ID_predall)){
  d_train <- d_select %>% dplyr::filter(RaceID <  ID_predall[k])
  d_pred  <- d_select %>% dplyr::filter(RaceID == ID_predall[k])
  
  fitfile <- paste0('output/fit-real-',as.character(k),'.RData')
  if(file.exists(fitfile)){
    load(fitfile)
  }else{
    # 出走馬数ごとにレース結果を整理
    n_rph      <- numeric(18)
    Rrslt      <- vector("list", 18)
    R_HorseID  <- array(0,dim=c(max(d_train$RaceID),18,18))
    R_JockeyID <- array(0,dim=c(max(d_train$RaceID),18,18))
    for (i in 2:18){
      Rrslt[[i]] <- d_train                            %>%
        dplyr::select(RaceID,OoA,horseID,jockeyID)     %>%
        dplyr::group_by(RaceID)                        %>%
        dplyr::distinct(OoA, .keep_all = TRUE)         %>% #同着がいる場合は、片方の馬を削除
        dplyr::do(data.frame(horseID  = .$horseID,
                             jockeyID = .$jockeyID,
                             OoA      = rank(.$OoA)))  %>% #削除したことで生じた順位の空白を埋める
        dplyr::filter(n() == i)                        %>%  #出走馬数がi頭のレースのみ抽出
        dplyr::ungroup()                               
      if (nrow(Rrslt[[i]]) == 0){
        n_rph[i] <- 0
      }else{
        n_rph[i] <- Rrslt[[i]] %>% dplyr::distinct(RaceID, .keep_all = FALSE) %>% nrow()  
        R_HorseID[1:n_rph[i],1:i,i] <- Rrslt[[i]]        %>% 
          dplyr::select(-jockeyID)                       %>%
          tidyr::spread(key = OoA, value = horseID)      %>%
          dplyr::select(-RaceID)                         %>%
          data.matrix() %>% apply(1,rev) %>% t() #stanのmodelの都合上、着順が遅い順に列を並び変える
        R_JockeyID[1:n_rph[i],1:i,i] <- Rrslt[[i]]       %>% 
          dplyr::select(-horseID)                        %>%
          tidyr::spread(key = OoA, value = jockeyID)     %>%
          dplyr::select(-RaceID)                         %>%
          data.matrix() %>% apply(1,rev) %>% t() #stanのmodelの都合上、着順が遅い順に列を並び変える
      }
    }
    
    # stan実行
    data <- list(N        = max(d_select$horseID), 
                 M        = max(d_select$jockeyID),
                 G        = n_rph,
                 HorseID  = R_HorseID,
                 JockeyID = R_JockeyID,
                 N_new        = nrow(d_pred),
                 HorseID_new  = d_pred$horseID,
                 JockeyID_new = d_pred$jockeyID,
                 Null_HorseID =  !(d_pred$horseID %in% d_train$horseID),
                 Null_JockeyID = !(d_pred$jockeyID %in% d_train$jockeyID)
    )
    stanmodel <- stan_model(file='code/model/model-strength.stan')
    fit <- sampling(stanmodel, data=data, pars=c('mu_h','mu_j','pf','mu_h_null','mu_j_null','s_pf_h_null','s_pf_j_null'), seed=1234, iter=1000, chains=4)
    
    # 収束判定
    print(all(summary(fit)$summary[,"Rhat"] <= 1.10, na.rm = T))
    
    # 推定結果を保存する
    write.table(data.frame(summary(fit)$summary),
                file=paste0('output/fit-summary-real',as.character(i),'.txt'), sep='\t', quote=FALSE, col.names=NA)
    save(fit,file=paste0('output/fit-real-',as.character(k),'.RData'))
  }

  
  ####馬券一覧の作成####
  ms <- rstan::extract(fit)
  ms_rank <- t(apply(-ms$pf, 1, rank)) #着順
  pWin    <- apply(ms_rank,2,function(x){sum(x==1)/nrow(ms_rank)}) #単勝の勝率
  Items   <- data.frame(weight = rep(1,length=ncol(ms_rank)),prob = pWin, odds = d_pred$単勝オッズ) %>%
    dplyr::mutate(value = prob*odds-1, ID = d_pred$horseID)
  W      <- 100 #total weight
  alpha  <- 0.1
  Items  <- lapply(Items,rep,length=W*nrow(Items)) %>% as.data.frame()
  Items2 <- data.frame(matrix(rep(NA, ncol(Items)), nrow=1))[numeric(0), ]
  for (i in unique(Items$ID)){
    ItemsTemp <- Items %>% dplyr::filter(ID == i)
    for (j in 2:nrow(ItemsTemp)){
      ItemsTemp$weight[j] <- j*ItemsTemp$weight[1]
      ItemsTemp$value[j] <- j*ItemsTemp$value[1]
    }
    Items2 <- rbind(Items2,ItemsTemp)
  }
  ####不要な馬券を省く####
  Kmin   <- 1
  Ptotal <- 0
  Dprob  <- sort(Items2[!duplicated(Items2$ID),"prob"],decreasing = T)
  while (Ptotal < 1-alpha){
    Kmin   <- Kmin + 1
    Ptotal <- sum(Dprob[1:Kmin])
  }
  UnitW <- 5 #問題を簡単にするため１口UnitW*100円とする
  ####馬券組合せ最適化###
  BettingComb <- BranchAndBound(Items2 %>% dplyr::filter( (weight %% UnitW == 0) & (weight <= W-UnitW*(Kmin-1)) ),W,alpha)
  Cost     <- sum(BettingComb$OptItem$weight)*100
  WinItem  <- BettingComb$OptItem %>% dplyr::filter(ID == d_pred[d_pred$OoA == 1,"horseID"])
  if (nrow(WinItem) == 0){
    Refund <- 0
  }else{
    Refund <- WinItem$odds * WinItem$weight * 100
  }
  Profit <- Refund - Cost   #[yen]
  # ROI    <- Refund/Cost*100 #[%]
  
  BettingResult <- list(Balance = data.frame(Cost = Cost, Refund = Refund, Profit = Profit), 
                        Betting = BettingComb, 
                        Inputs  = list(Item = Items2, TotalWeight = W, alpha = alpha))
  BettingResults <- append(BettingResults,list(BettingResult))
  # browser()
}

save(BettingResults,file=paste0('output/BettingResults-alpha',as.character(alpha*100),'.RData'))

#損をしたレースの分析
test_ <- dplyr::left_join(BettingResults[[1]]$Inputs$Item %>% dplyr::distinct(ID,.keep_all=T) %>% dplyr::select(ID,prob,odds),
                         BettingResults[[1]]$Betting$OptItem,
                         by = c("ID","prob","odds"))
test  <- dplyr::left_join(d_predall %>% dplyr::filter(RaceID == 537) %>% dplyr::select(horseID,馬名,OoA),
                         test_, 
                         by=c("horseID"="ID"))%>%
         dplyr::arrange(OoA) %>%
         dplyr::select(-c(horseID,value))

test2_ <- dplyr::left_join(BettingResults[[5]]$Inputs$Item %>% dplyr::distinct(ID,.keep_all=T) %>% dplyr::select(ID,prob,odds),
                          BettingResults[[5]]$Betting$OptItem,
                          by = c("ID","prob","odds"))
test2  <- dplyr::left_join(d_predall %>% dplyr::filter(RaceID == 552) %>% dplyr::select(horseID,馬名,OoA),
                          test2_, 
                          by=c("horseID"="ID"))%>%
          dplyr::arrange(OoA) %>%
          dplyr::select(-c(horseID,value))

# test3_ <- dplyr::left_join(BettingResults[[3]]$Inputs$Item %>% dplyr::distinct(ID,.keep_all=T) %>% dplyr::select(ID,prob,odds),
#                            BettingResults[[3]]$Betting$OptItem,
#                            by = c("ID","prob","odds"))
# test3  <- dplyr::left_join(d_predall %>% dplyr::filter(RaceID == 547) %>% dplyr::select(horseID,馬名,OoA),
#                            test3_, 
#                            by=c("horseID"="ID"))%>%
#           dplyr::arrange(OoA) %>%
#           dplyr::select(-c(horseID,value))

