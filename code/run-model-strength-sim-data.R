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
n_rph <- numeric(18)
Rrslt <- vector("list", 18)
for (i in 2:18){
  Rrslt[[i]] <- d                                              %>%
                dplyr::select(RaceID,OoA,horseID)              %>%
                dplyr::group_by(RaceID)                        %>%
                dplyr::filter(n() == i)                        %>% #出走馬数がi頭のレースのみ抽出
                dplyr::ungroup()                               %>%
                tidyr::spread(key = OoA, value = horseID)      %>%
                dplyr::select(-RaceID)                         %>%
                data.matrix()
  if (nrow(Rrslt[[i]]) == 0){
    Rrslt[[i]] <- data.frame(matrix(rep(NA, i), nrow=1))[numeric(0), ]
  }
  Rrslt[[i]] = Rrslt[[i]][,i:1] #stanのmodelの都合上、着順が遅い馬を先頭列に移動しておく
  n_rph[i] <- nrow(Rrslt[[i]]) 
}


# stanで馬の「強さ」を推定する
data <- list(N=max(d$horseID), 
             G=n_rph,
             LW2 = Rrslt[[2]],
             LW3 = Rrslt[[3]], 
             LW4 = Rrslt[[4]],
             LW5 = Rrslt[[5]],
             LW6 = Rrslt[[6]],
             LW7 = Rrslt[[7]],
             LW8 = Rrslt[[8]],
             LW9 = Rrslt[[9]],
             LW10 = Rrslt[[10]],
             LW11 = Rrslt[[11]],
             LW12 = Rrslt[[12]],
             LW13 = Rrslt[[13]],
             LW14 = Rrslt[[14]],
             LW15 = Rrslt[[15]],
             LW16 = Rrslt[[16]],
             LW17 = Rrslt[[17]],
             LW18 = Rrslt[[18]]
)
stanmodel <- stan_model(file='code/model/model-strength.stan')
fit <- sampling(stanmodel, data=data, pars=c('mu','s_mu','s_pf'), seed=1234, iter=1000, chains=4)

# 収束判定
all(summary(fit)$summary[,"Rhat"] <= 1.10, na.rm = T)
traceplot(fit,pars="mu[1]")

# 推定結果を保存する
write.table(data.frame(summary(fit)$summary),
            file='output/fit-summary-sim.txt', sep='\t', quote=FALSE, col.names=NA)
save.image('output/result-model-strength-sim.RData')


# 推定結果の妥当性を検証（強さ上位５頭が事前知識と合っているか）
ms              <- rstan::extract(fit)
d_qua           <- data.frame(nid = 1:max(d$horseID), t(apply(ms$mu, 2, quantile, prob=c(0.05, 0.5, 0.95))))
colnames(d_qua) <- c('horseID', 'mu_p05', 'mu_p50', 'mu_p95')
d_top5          <- head(d_qua[rev(order(d_qua$mu_p50)),], 5)
# 推定された上位5頭
d_top5[,c("horseID","mu_p50")]                 
# 実際の上位5頭
d2 <- dplyr::distinct(d,horseID,.keep_all = TRUE)
d2[order(-d2$mu)[1:5],c("horseID","mu")]
# 実際の上位5頭のレース数
d %>% 
  dplyr::select(horseID,RaceID,OoA) %>%
  dplyr::filter(horseID == 616 | horseID == 1324 | horseID == 1080 | horseID == 955 | horseID == 1246) %>%
  dplyr::group_by(horseID) %>%
  dplyr::summarise(n_race = n())
# 実際の上位5頭の「強さ」の推定値
d_qua %>% 
  dplyr::filter(horseID == 616 | horseID == 1324 | horseID == 1080 | horseID == 955 | horseID == 1246) %>%
  dplyr::mutate(BayesConf_90 = mu_p95 - mu_p05)
  


