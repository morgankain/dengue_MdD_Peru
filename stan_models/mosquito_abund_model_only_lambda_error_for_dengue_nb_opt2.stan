data {
  int<lower=0> N;                     // number of observations
  int<lower=0> N_pred_d;              // number of observations for predictions for dengue count model
  int<lower=0> N_pred_PM;             // number of observations for predictions for Puerto Maldonado
  int<lower=0> K_lambda;
  vector[N] pop;  
  vector[N_pred_d] x_pred_d;          // vals for prediction for dengue counts
  vector[N_pred_PM] x_pred_PM;        // vals for prediction for PM
  int<lower=0> y[N];                  // outcome
  vector[N] ae_pri;                   // data directly affecting the zero inflation portion of the model
  vector[N_pred_d] ae_pri_pred_d;     // Other piece needed for predictions for d and PM
  vector[N_pred_PM] ae_pri_pred_PM;          
  real<lower=0> theta_sigma;

}

parameters {

  real alpha_lambda;             // intercept of poisson counts
  vector[K_lambda] beta_lambda;  // predictors of poisson counts  
  real theta_bias;               // bias in suitability from the data
  vector[N] theta_eps;           // standard normal uncertainty on predicted suitability for the logit scale   
  real<lower=0> reciprocal_phi;

}

transformed parameters {
  vector[N] theta ;
  vector[N] lambda_log ; 
  real<lower=0> phi;
 
for (i in 1:N) {
  theta[i] = inv_logit(ae_pri[i] + theta_bias + theta_sigma*theta_eps[i]);
}

// effects of predictors on count on the log scale, transform to count scale for lambda	  
  lambda_log = alpha_lambda + pop * beta_lambda[1] ;   

  phi = 1. / reciprocal_phi;
          
}

model {

// priors. Break up the beta's according to some other independent data sources...

// intercept
   alpha_lambda ~ normal(0, 5) ;

// slopes
   beta_lambda[1] ~ normal(0, 5) ; 

// uncertainty and bias
   theta_bias  ~ normal(0, 1);
   reciprocal_phi ~ cauchy(0., 5);

   theta_eps ~ normal(0, 1);

// modify the likelihood
   for(n in 1:N){

// Need to somehow fit theta directly with Chris's measures and with the counts...

    if(y[n] == 0) {

// A count of zero can arise from a bernoulli draw of a 0 bernoulli draw of a 1 and poisson draw of a 0
// A higher theta thus means a higher probability of drawing a 0
     
       target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),                 
                            bernoulli_lpmf(0 | theta[n])
                              + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi));


    } else {

// A count of 1+ arises from a bernoulli draw of and poisson draw of a positive number

      target += bernoulli_lpmf(0 | theta[n])                              
                  + neg_binomial_2_log_lpmf(y[n] | lambda_log[n], phi);

    }
  }
}

generated quantities{

// simulating values for dengue count model just for the original data for diagnostic purposes
 int<lower=0> y_sim[N];
 int zero;
 vector[N] mu;

 mu = exp(lambda_log);

 for(n in 1:N) {

  zero     = bernoulli_rng(theta[n]); 
  y_sim[n] = (1 - zero) * neg_binomial_2_rng(mu[n], phi);

  }

}

