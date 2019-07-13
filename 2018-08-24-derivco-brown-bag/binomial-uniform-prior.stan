## @knitr stan_binomial_uniform_prior
data {
  int<lower=0> N;
  int hits[N];
  int spins[N];
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  hits ~ binomial(spins, theta);       // Likelihood
  theta ~ uniform(0, 1);               // Prior
}
