## @knitr stan_lognormal_rtp
data {
  int<lower=0> N;
  real rtp[N];
}
parameters {
  real<lower = 0> mu;
  real<lower = 0> sigma;
}
model {
  rtp ~ lognormal(log(mu) - sigma * sigma / 2, sigma);
}
generated quantities {
  real simulated[25];
  for (i in 1:25) {
    simulated[i] = lognormal_rng(log(mu) - sigma * sigma / 2, sigma);
  }
}
