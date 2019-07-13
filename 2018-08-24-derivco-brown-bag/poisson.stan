data {
  int<lower=0> N;
  int spins[N];
}
parameters {
  real<lower=0,upper=1000> lambda;
}
model {
  spins ~ poisson(lambda);
}
