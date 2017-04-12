data {
  int N;
  vector[N] x;
  vector[N] y;
  vector[N] uy;
  int N2;
  vector[N2] x_new;
  int<lower=0, upper=1> inadequacy;
}
parameters {
  real eps;
  real sig;
}
model {
  for (i in 1:N)
    y[i] ~ normal(phys_mod(x[i],eps,sig,inadequacy), uy[i]);
}
generated quantities{
  vector[N]  resid;
  vector[N2] y_conf;
  vector[N2] y_pred;
  real br;
  real vy;
  real uyp;

  # Residuals
  resid = y - phys_mod_vec(x,eps,sig,inadequacy);

  # Birge ratio
  {
    vector[N]  Vm1;
    for (i in 1:N)
      Vm1[i] = 1/uy[i]^2;
    br = quad_form(diag_matrix(Vm1),resid) / (N-2);
  }
  
  # Predicted data
  for (i in 1:N2) {
    vy  = phys_mod(x_new[i],eps,sig,inadequacy);
    uyp = fmax(min(uy),vy * mean(uy ./ y));
    y_conf[i] = vy;
    y_pred[i] = normal_rng(y_conf[i],uyp);
  }

}
