data {
  int N;      // num of horses
  int M;      // num of jockeys
  int G[18];  // num of races
  int HorseID[sum(G),18,18];
  int JockeyID[sum(G),18,18];
  int N_new;  // num of horses(jockeys) for prediction
  int HorseID_new[N_new];
  int JockeyID_new[N_new];
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
  
  vector[N] mu_h;
  vector[M] mu_j;
  real<lower=0> s_mu_h;
  real<lower=0> s_mu_j;
  vector<lower=0>[N] s_pf_h;
  vector<lower=0>[M] s_pf_j;
}

model {
  for (r in 2:18){
    for (g in 1:G[r]){
      for (i in 1:r){
        if (r==7)
          performance7[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==8)
          performance8[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==9)
          performance9[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==10)
          performance10[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==11)
          performance11[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==12)
          performance12[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==13)
          performance13[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==14)
          performance14[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==15)
          performance15[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==16)
          performance16[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==17)
          performance17[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
        else if (r==18)
          performance18[g,i] ~ normal(mu_h[HorseID[g,i,r]]+mu_j[JockeyID[g,i,r]], sqrt( s_pf_h[HorseID[g,i,r]]^2 + s_pf_j[JockeyID[g,i,r]]^2 ) );
      }
    }
  }

  mu_h ~ normal(0, s_mu_h);
  mu_j ~ normal(0, s_mu_j);
  s_pf_h ~ gamma(10, 10);
  s_pf_j ~ gamma(10, 10);
}

generated quantities{
  real pf[N_new];
  for (n in 1:N_new){
    pf[n] = normal_rng( mu_h[HorseID_new[n]]+mu_j[JockeyID_new[n]], sqrt(s_pf_h[HorseID_new[n]]^2 + s_pf_j[JockeyID_new[n]]^2) );
  }
}
