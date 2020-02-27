
data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // tech chosen
    real y[N,K];        // observed personal yields of techs 1-K
    real q[N,K];       // observed payoff social variables of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // bout or fruit
    int id[N];          // player id
    int N_effects;      // number of learning parameters to estimate

}
parameters {
    vector[N_effects] mu;                   // average effects
    matrix[N_effects,J] zed;                // individual z-scores
    cholesky_factor_corr[N_effects] L_Rho;  // correlation matrix
    vector<lower=0>[N_effects] sigma;       // standard deviations

}
transformed parameters{
    matrix[J,N_effects] a_id;
    a_id = (diag_pre_multiply(sigma,L_Rho) * zed)';
}
model {
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] lin_mod;

    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real beta;     // conform exponent

    //priors
    mu[1] ~ normal(0,0.6);
    mu[2] ~ normal(0,1);
    mu[3] ~ normal(0,1);
    mu[4] ~ normal(0,1);

    sigma ~ exponential(1);
    to_vector(zed) ~ normal(0,1);
    L_Rho ~ lkj_corr_cholesky(3);

    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp(mu[1]+  a_id[id[i],1]);
            phi = inv_logit( mu[2] + a_id[id[i],2]   );
            gamma = inv_logit( mu[3] + a_id[id[i],3] );
            beta = ( mu[4] + a_id[id[i],4] );

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta*q[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                PrS = lin_mod[tech[i]]/sum(lin_mod);
                
                target += ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) );

            } else {
                target += ( logPrA );
            }
        } else {
            target += ( logPrA );
         }
     }//i  

}//end of model

generated quantities{
    vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] lin_mod;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real beta;     // conform 
    matrix[N_effects,N_effects] Rho;

    Rho = L_Rho * L_Rho';


    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp(mu[1]+  a_id[id[i],1]);
            phi = inv_logit( mu[2] + a_id[id[i],2]   );
            gamma = inv_logit( mu[3] + a_id[id[i],3] );
            beta = ( mu[4] + a_id[id[i],4] );

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //only socially learn if there is social info
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( beta*q[i,j]);
                }
                lin_mod[1] = 1; // aliased outcome
                // compute frequency cue
                PrS = lin_mod[tech[i]]/sum(lin_mod);

                log_lik[i] = ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) ) ;   

            } else {
                 log_lik[i] = (logPrA);
            }
        } else {
                 log_lik[i] = (logPrA);         }
     }//i  

}//end of model
