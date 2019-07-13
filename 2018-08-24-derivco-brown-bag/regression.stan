## @knitr stan_regression
data {
  int<lower=0> N;
  vector[N] y;                         // Total wager
  vector[N] x;                         // Number of spins (standardised)
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma); // Likelihood
  //
  alpha ~ normal(0, 10);               // Prior (Normal)
  beta ~ normal(0, 10);                // Prior (Normal)
  sigma ~ cauchy(0, 5);                // Prior (Cauchy)
}