functions { 
  /* cratio-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: linear predictor
   *   thres: ordinal thresholds
   *   disc: discrimination parameter
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cratio_logit_lpmf(int y, real mu, vector thres, real disc) {
     int ncat = num_elements(thres) + 1;
     vector[ncat] p;
     vector[ncat - 1] q;
     int k = 1;
     while (k <= min(y, ncat - 1)) {
       q[k] = inv_logit(disc * (mu - thres[k]));
       p[k] = 1 - q[k];
       for (kk in 1:(k - 1)) p[k] = p[k] * q[kk];
       k += 1;
     }
     if (y == ncat) {
       p[ncat] = prod(q);
     }
     return log(p[y]);
   } 
  /* cratio-logit log-PDF for a single response
   * including category specific effects
   * Args:
   *   y: response category
   *   mu: linear predictor
   *   mucs: predictor for category specific effects
   *   thres: ordinal thresholds
   *   disc: discrimination parameter
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cratio_logit_cs_lpmf(int y, real mu, row_vector mucs, vector thres, real disc) {
     int ncat = num_elements(thres) + 1;
     vector[ncat] p;
     vector[ncat - 1] q;
     int k = 1;
     while (k <= min(y, ncat - 1)) {
       q[k] = inv_logit(disc * (mu + mucs[k] - thres[k]));
       p[k] = 1 - q[k];
       for (kk in 1:(k - 1)) p[k] = p[k] * q[kk];
       k += 1;
     }
     if (y == ncat) {
       p[ncat] = prod(q);
     }
     return log(p[y]);
   } 
} 
data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=2> ncat;  // number of categories 
  int<lower=1> K;  // number of population-level effects 
  matrix[N, K] X;  // population-level design matrix 
  real<lower=0> disc;  // discrimination parameters 
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
  int Kc = K - 1; 
  matrix[N, K - 1] Xc;  // centered version of X 
  vector[K - 1] means_X;  // column means of X before centering 
  for (i in 2:K) { 
    means_X[i - 1] = mean(X[, i]); 
    Xc[, i - 1] = X[, i] - means_X[i - 1]; 
  } 
} 
parameters { 
  vector[Kc] b;  // population-level effects 
  vector[ncat-1] temp_Intercept;  // temporary thresholds 
} 
transformed parameters { 
} 
model { 
  vector[N] mu = Xc * b;
  // priors including all constants 
  target += student_t_lpdf(temp_Intercept | 3, 0, 10); 
  // likelihood including all constants 
  if (!prior_only) { 
    for (n in 1:N) {
      target += cratio_logit_lpmf(Y[n] | mu[n], temp_Intercept, disc);
    }
  } 
} 
generated quantities { 
  // compute actual thresholds 
  vector[ncat - 1] b_Intercept = temp_Intercept + dot_product(means_X, b); 
} 