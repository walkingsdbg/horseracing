library(ggplot2)
library(tidyr)
library(dplyr)
library(summarytools)
library(rstan)
library(ggmcmc)
source("code/common.R")
rm(list = ls())
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

set.seed(123)

d <- read.csv(file='input/sim-data.csv',header = TRUE)

# 出走馬数ごとにレース結果を整理
n_rph      <- numeric(18)
Rrslt      <- vector("list", 18)
R_HorseID  <- array(0,dim=c(max(d$RaceID),18,18))
R_JockeyID <- array(0,dim=c(max(d$RaceID),18,18))
for (i in 2:18){
  Rrslt[[i]] <- d                                              %>%
                dplyr::select(RaceID,OoA,horseID,jockeyID)     %>%
                dplyr::group_by(RaceID)                        %>%
                dplyr::filter(n() == i)                        %>%  #出走馬数がi頭のレースのみ抽出
                dplyr::ungroup()                               
  if (nrow(Rrslt[[i]]) == 0){
    n_rph[i] <- 0
  }else{
    n_rph[i] <- Rrslt[[i]] %>% dplyr::distinct(RaceID, .keep_all = FALSE) %>% nrow()  
    R_HorseID[1:n_rph[i],1:i,i] <- Rrslt[[i]]                                     %>% 
                                   dplyr::select(-jockeyID)                       %>%
                                   tidyr::spread(key = OoA, value = horseID)      %>%
                                   dplyr::select(-RaceID)                         %>%
                                   data.matrix() %>% apply(1,rev) %>% t() #stanのmodelの都合上、着順が遅い順に列を並び変える
    R_JockeyID[1:n_rph[i],1:i,i] <- Rrslt[[i]]                                     %>% 
                                   dplyr::select(-horseID)                       %>%
                                   tidyr::spread(key = OoA, value = jockeyID)      %>%
                                   dplyr::select(-RaceID)                         %>%
                                   data.matrix() %>% apply(1,rev) %>% t() #stanのmodelの都合上、着順が遅い順に列を並び変える
  }
}


# stan実行
data <- list(N        = max(d$horseID), 
             M        = max(d$jockeyID),
             G        = n_rph,
             HorseID  = R_HorseID,
             JockeyID = R_JockeyID
)
stanmodel <- stan_model(file='code/model/model-strength.stan')
fit <- sampling(stanmodel, data=data, pars=c('mu_h','s_mu_h','s_pf_h','mu_j','s_mu_j','s_pf_j'), seed=1234, iter=1000, chains=4)

# 収束判定
all(summary(fit)$summary[,"Rhat"] <= 1.10, na.rm = T)
traceplot(fit,pars="mu_h[1]")
traceplot(fit,pars="mu_j[1]")

# 推定結果を保存する
write.table(data.frame(summary(fit)$summary),
            file='output/fit-summary-sim.txt', sep='\t', quote=FALSE, col.names=NA)
save.image('output/result-model-strength-sim.RData')


# 推定結果の妥当性を検証（強さ上位５頭が事前知識と合っているか）
ms              <- rstan::extract(fit)
d_qua_h         <- data.frame(nid = 1:max(d$horseID),  t(apply(ms$mu_h, 2, quantile, prob=c(0.05, 0.5, 0.95))))
d_qua_j         <- data.frame(nid = 1:max(d$jockeyID), t(apply(ms$mu_j, 2, quantile, prob=c(0.05, 0.5, 0.95))))
colnames(d_qua_h)   <- c('horseID',  'mu_p05', 'mu_p50', 'mu_p95')
colnames(d_qua_j)   <- c('jockeyID', 'mu_p05', 'mu_p50', 'mu_p95')
d_top5_h            <- head(d_qua_h[rev(order(d_qua_h$mu_p50)),], 5)
d_top5_j            <- head(d_qua_j[rev(order(d_qua_j$mu_p50)),], 5)

# 推定された上位5頭
d_top5_h[,c("horseID","mu_p50")]                 
# 実際の上位5頭
d2 <- dplyr::distinct(d,horseID,.keep_all = TRUE)
d2[order(-d2$mu_horse)[1:5],c("horseID","mu_horse")]

# 推定された上位5人
d_top5_j[,c("jockeyID","mu_p50")]                 
# 実際の上位5人
d2 <- dplyr::distinct(d,jockeyID,.keep_all = TRUE)
d2[order(-d2$mu_jockey)[1:5],c("jockeyID","mu_jockey")]


# 実際の上位5頭のレース数
d %>% 
  dplyr::select(horseID,RaceID,OoA) %>%
  dplyr::filter(horseID == 616 | horseID == 1324 | horseID == 1080 | horseID == 955 | horseID == 1246) %>%
  dplyr::group_by(horseID) %>%
  dplyr::summarise(n_race = n())
# 実際の上位5頭の「強さ」の推定値
d_qua_h %>% 
  dplyr::filter(horseID == 616 | horseID == 1324 | horseID == 1080 | horseID == 955 | horseID == 1246) %>%
  dplyr::mutate(BayesConf_90 = mu_p95 - mu_p05)

# 実際の上位5人のレース数
d %>% 
  dplyr::select(jockeyID,RaceID,OoA) %>%
  dplyr::filter(jockeyID == 94 | jockeyID == 48 | jockeyID == 29 | jockeyID == 136 | jockeyID == 44) %>%
  dplyr::group_by(jockeyID) %>%
  dplyr::summarise(n_race = n())
# 実際の上位5人の「強さ」の推定値
d_qua_j %>% 
  dplyr::filter(jockeyID == 94 | jockeyID == 48 | jockeyID == 29 | jockeyID == 136 | jockeyID == 44) %>%
  dplyr::mutate(BayesConf_90 = mu_p95 - mu_p05)
  


