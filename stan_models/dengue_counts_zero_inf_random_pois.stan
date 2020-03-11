
data {

  int<lower=0> N;               // number of observations of Dengue
  int<lower=0> K_den;		// number of predictors for dengue counts
  int<lower=0> K_obs;		// number of observations for k_den predictors
  int<lower=0> K_mos_theta;	// number of predictors from the mosquito model for dengue 0s
  int<lower=0> K_mos_lambda;	// number of predictors from the mosquito model for dengue counts
  int<lower=0> y[N];            // Dengue case counts
  int<lower=0> N_loc;		// number of locations where counts are repeated (for the random effect)
  int<lower=0> loc_id[N];	// ID of each location 
  matrix[N, K_den] x;			// fixed effects matrix
  matrix[N, K_mos_theta] z_theta;	// fixed effects matrix from mosquito model predictors for theta
  matrix[N, K_mos_lambda] z_lambda;	// fixed effects matrix from mosquito model predictors for lambda
  matrix[N, K_obs] x_obs;		// fixed effects matrix for 
  vector[N] offset;

 // int<lower=0> N_pred;
 // matrix[N_pred, K_den] x_pred;
 // matrix[N_pred, K_mos_theta] z_theta_pred;
 // matrix[N_pred, K_mos_lambda] z_lambda_pred;
 // matrix[N_pred, K_obs] x_obs_pred;
 // vector[N_pred] offset_pred;

}

parameters {

  real alpha_theta_bar;        
  real alpha_lambda_bar;
  vector[K_den] beta_theta;   
  vector[K_den] beta_lambda;
  vector[K_obs] beta_obs;      
  vector[K_mos_theta] beta_theta_mos; 
  vector[K_mos_lambda] beta_lambda_mos; 

// random effect structure
  real<lower=0> sigma_alpha_theta;
  vector[N_loc] eps_alpha_theta;

  real<lower=0> sigma_alpha_lambda;
  vector[N_loc] eps_alpha_lambda;
  
}

transformed parameters {

  vector[N] theta;
  vector[N] lambda_log;

//  vector[N_pred] theta_pred;
//  vector[N_pred] lambda_log_pred;

  real alpha_theta[N_loc];
  real alpha_lambda[N_loc];

// random effects structure

for (i in 1:N_loc) {

  alpha_theta[i]  = alpha_theta_bar  + sigma_alpha_theta  * eps_alpha_theta[i];
  alpha_lambda[i] = alpha_lambda_bar + sigma_alpha_lambda * eps_alpha_lambda[i];

}
    
// Model fit to day

for (j in 1:N) {

  theta[j]      = inv_logit(alpha_theta[loc_id[j]] + x[j, ] * beta_theta + z_theta[j, ] * beta_theta_mos);
  lambda_log[j] = alpha_lambda[loc_id[j]] + x[j, ] * beta_lambda + z_lambda[j, ] * beta_lambda_mos + x_obs[j, ] * beta_obs + offset[j]; 

}

// For predictions ignoring the random effect intercept adjustment, so don't need loop

//  theta_pred      = inv_logit(alpha_theta_bar + x_pred * beta_theta + z_theta_pred * beta_theta_mos);
//  lambda_log_pred = alpha_lambda_bar + x_pred * beta_lambda + z_lambda_pred * beta_lambda_mos + x_obs_pred * beta_obs + offset_pred;

}

model {

// priors. Break up the beta's according to some other independent data sources...

// intercept
   alpha_theta_bar ~ normal(0, 3) ;
   alpha_lambda_bar ~ normal(0, 3) ;

// slopes
   beta_theta ~ normal(0, 3) ;
   beta_lambda ~ normal(0, 3) ;
   beta_obs ~ normal(0, 3) ;
   beta_theta_mos ~ normal(0, 3) ;    
   beta_lambda_mos ~ normal(0, 3) ; 

// random effects
   eps_alpha_theta ~ normal(0, 1);
   eps_alpha_lambda ~ normal(0, 1);
   sigma_alpha_theta ~ cauchy(0, 5);
   sigma_alpha_lambda ~ cauchy(0, 5);


// modify the likelihood
   for(n in 1:N) {
    
if (y[n] == 0) {
     
      target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),                 
                             bernoulli_lpmf(0 | theta[n])
                              + poisson_log_lpmf(y[n] | lambda_log[n]));


    } else {

      target += bernoulli_lpmf(0 | theta[n])                              
                  + poisson_log_lpmf(y[n] | lambda_log[n]);

    }
  }
}

generated quantities{

// simulating values

 // int<lower=0> y_sim[N_pred];
//    int zero[N_pred];

//  for(n in 1:N_pred){

 //  zero[n]     = bernoulli_rng(theta_pred[n]); 
// y_sim[n] = (1 - zero) * neg_binomial_2_log_rng(lambda_log_pred[n], phi);

//  }

}

