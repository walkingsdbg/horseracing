library(ggplot2)
library(tidyr)
library(dplyr)
library(summarytools)
# library(knitr)
rm(list = ls())

set.seed(111)

d <- read.csv(file='../real/input/horseracing2000-2018.csv',header = TRUE)

# 対象レースを選択する
d_select <- d %>% 
            dplyr::filter(between(年,17,19) & 
                          クラス名 %in% c("Ｇ１","Ｇ２","Ｇ３") & 
                          確定着順 != 0) %>%
            dplyr::mutate(horseID  = as.numeric(factor(馬名,levels=unique(馬名))),
                          jockeyID = as.numeric(factor(騎手,levels=unique(騎手))),
                          race     = factor(paste(日付Ｓ,場所,レース番号,sep = '_')))

# # 各馬の出走回数を計算する
# d_horse <- d_select %>% 
#   dplyr::group_by(馬名) %>% 
#   dplyr::summarise(race_cnt = n())

# # 出走回数・馬のＩＤ・レース名追加
# d_select2 <- dplyr::inner_join(d_select,d_horse,by = '馬名') %>%
#            # dplyr::filter(race_cnt > 8) %>%
#              dplyr::mutate(horseID = as.numeric(factor(馬名,levels=unique(馬名))),
#                            race    = factor(paste(日付Ｓ,場所,レース番号,sep = '_')))

# レースごとの出走馬数
d_race <- d_select %>%
          dplyr::group_by(race) %>%
          dplyr::summarise(n_hpr = n())

# レース数を出走馬数ごとに集計（中央競馬は最大18頭）
n_rph <- numeric(18)
for (i in 1:length(n_rph)){
  n_rph[i] <- sum(d_race$n_hpr == i)
}

###### シミュレーションデータ生成 #######

# 馬の「強さ」を生成する
n_horse <- length(unique(d_select$horseID))
sd_str  <- rgamma(n_horse, shape = 10, rate = 10)
sd_mu   <- 1
mu_str  <- rnorm(n_horse, mean = 0, sd = sd_mu)

# 騎手の「強さ」を生成する
n_jockey        <- length(unique(d_select$jockeyID))
sd_str_jockey   <- rgamma(n_jockey, shape = 10, rate = 10)
sd_mu_jockey    <- 1
mu_str_jockey   <- rnorm(n_jockey, mean = 0, sd = sd_mu_jockey)

# レースごとの馬のパフォーマンス・レース結果を生成する
Rrslt <- vector("list", 18)
for (i in 1:nrow(d_race)) {
  horseID_pr  <- d_select %>% 
                 dplyr::filter(race == d_race$race[i]) %>%
                 dplyr::select(horseID)
  jockeyID_pr <- d_select %>% 
                 dplyr::filter(race == d_race$race[i]) %>%
                 dplyr::select(jockeyID)
  pf <- NULL
  mu <- NULL
  pf_jockey <- NULL
  mu_jockey <- NULL
  for (j in 1:d_race$n_hpr[i]) {
    pf[j]        <- rnorm(1,mean = mu_str[horseID_pr[j,1]], sd = sd_str[horseID_pr[j,1]])
    mu[j]        <- mu_str[horseID_pr[j,1]]
    pf_jockey[j] <- rnorm(1,mean = mu_str_jockey[jockeyID_pr[j,1]], sd = sd_str_jockey[jockeyID_pr[j,1]])
    mu_jockey[j] <- mu_str_jockey[jockeyID_pr[j,1]]
  }
  Rrslt_pr <- data.frame(RaceID = i, OoA = rank(-(pf+pf_jockey)), horseID_pr, jockeyID_pr, 
                         mu_horse = mu, mu_jockey = mu_jockey, pf_horse = pf, pf_jockey = pf_jockey, pf_all = pf+pf_jockey)
  # Rrslt_pr <- data.frame(OoA = rank(-pf), horseID_pr, pf = pf)
  Rrslt[[1]] <- rbind(Rrslt[[1]], Rrslt_pr[order(Rrslt_pr[,"OoA"]),] )
  # 出走馬数ごとにレース着順を集計する
  Rrslt[[nrow(Rrslt_pr)]] = rbind(Rrslt[[nrow(Rrslt_pr)]], 
                                  Rrslt_pr[order(Rrslt_pr[,"OoA"],decreasing=T),"horseID"])
}

for (i in 2:18){
  if (is.null(Rrslt[[i]])){
    Rrslt[[i]] = data.frame(matrix(rep(NA, i), nrow=1))[numeric(0), ]
  }
}

# データの確認
dfSummary(Rrslt[[1]])
# knitr::kable(skimr::skim_to_wide(Rrslt[[1]]))

write.table(Rrslt[[1]], file = "input/sim-data.csv", quote=F, row.names=F, col.names=T, sep=",")
