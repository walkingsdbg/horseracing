data {
  int N;      // num of horses
  int G[18];  // num of races
  int<lower=1, upper=N> LW7[G[7],7];  // order of arrival of each race (7th:LW7[,1],6th:LW[,2],...1st:LW7[,7])
  int<lower=1, upper=N> LW8[G[8],8];  
  int<lower=1, upper=N> LW9[G[9],9];  
  int<lower=1, upper=N> LW10[G[10],10];  
  int<lower=1, upper=N> LW11[G[11],11];  
  int<lower=1, upper=N> LW12[G[12],12];  
  int<lower=1, upper=N> LW13[G[13],13];  
  int<lower=1, upper=N> LW14[G[14],14];  
  int<lower=1, upper=N> LW15[G[15],15];  
  int<lower=1, upper=N> LW16[G[16],16];  
  int<lower=1, upper=N> LW17[G[17],17];  
  int<lower=1, upper=N> LW18[G[18],18];  
}

parameters {
  ordered[7] performance7[G[7]];
  ordered[8] performance8[G[8]];
  ordered[9] performance9[G[9]];
  ordered[10] performance10[G[10]];
  ordered[11] performance11[G[11]];
  ordered[12] performance12[G[12]];
  ordered[13] performance13[G[13]];
  ordered[14] performance14[G[14]];
  ordered[15] performance15[G[15]];
  ordered[16] performance16[G[16]];
  ordered[17] performance17[G[17]];
  ordered[18] performance18[G[18]];
  
  vector[N] mu;
  real<lower=0> s_mu;
  vector<lower=0>[N] s_pf;
}

model {
  for (r in 2:18){
    for (g in 1:G[r]){
      for (i in 1:r){
        if (r==7)
          performance7[g,i] ~ normal(mu[LW7[g,i]], s_pf[LW7[g,i]]);
        else if (r==8)
          performance8[g,i] ~ normal(mu[LW8[g,i]], s_pf[LW8[g,i]]);
        else if (r==9)
          performance9[g,i] ~ normal(mu[LW9[g,i]], s_pf[LW9[g,i]]);
        else if (r==10)
          performance10[g,i] ~ normal(mu[LW10[g,i]], s_pf[LW10[g,i]]);
        else if (r==11)
          performance11[g,i] ~ normal(mu[LW11[g,i]], s_pf[LW11[g,i]]);
        else if (r==12)
          performance12[g,i] ~ normal(mu[LW12[g,i]], s_pf[LW12[g,i]]);
        else if (r==13)
          performance13[g,i] ~ normal(mu[LW13[g,i]], s_pf[LW13[g,i]]);
        else if (r==14)
          performance14[g,i] ~ normal(mu[LW14[g,i]], s_pf[LW14[g,i]]);
        else if (r==15)
          performance15[g,i] ~ normal(mu[LW15[g,i]], s_pf[LW15[g,i]]);
        else if (r==16)
          performance16[g,i] ~ normal(mu[LW16[g,i]], s_pf[LW16[g,i]]);
        else if (r==17)
          performance17[g,i] ~ normal(mu[LW17[g,i]], s_pf[LW17[g,i]]);
        else if (r==18)
          performance18[g,i] ~ normal(mu[LW18[g,i]], s_pf[LW18[g,i]]);
      }
    }
  }

  mu ~ normal(0, s_mu);
  s_pf ~ gamma(10, 10);
}

