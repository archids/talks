// This is ABC work!

data {
  int<lower=0> N;
  int count[N];
  int total[N];
}
parameters {
  real<lower=0,upper=1> mu;
  real<lower=0.001> ss;
  real<lower=0,upper=1> prob[N];
}
model {
  mu ~ uniform(0, 1);
  ss ~ gamma(0.01, 0.01);
  prob ~ beta(mu*ss, (1-mu)*ss);
  count ~ binomial(total, prob);
}
