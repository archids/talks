## @knitr stan_normal_rtp_hit_rate
data {
  int<lower=0> N;
  real rtp[N];
}
parameters {
  real<lower=0, upper=1> theta[6];
  real<lower=0> sigma;
}
transformed parameters {
  real<lower=0> mu;
  mu = theta[1] * 1 +                  // Payline 1 -> 1x
       theta[2] * 2 +                  // Payline 2 -> 2x
       theta[3] * 5 +                  // Payline 3 -> 5x
       theta[4] * 10 +                 // Payline 4 -> 10x
       theta[5] * 20 +                 // Payline 5 -> 20x
       theta[6] * 100;                 // Payline 6 -> 100x
}
model {
  rtp ~ lognormal(log(mu) - sigma * sigma / 2, sigma);
  theta[1] ~ beta(2, 2);               // Mode = 0.5
  theta[2] ~ beta(2, 4);               // Mode = 0.25
  theta[3] ~ beta(2, 5);               // Mode = 0.2
  theta[4] ~ beta(2, 8);               // Mode = 0.125
  theta[5] ~ beta(2, 10);              // Mode = 0.1
  theta[6] ~ beta(2, 16);              // Mode = 0.0625
}