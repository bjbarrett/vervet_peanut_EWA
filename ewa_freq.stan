data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // tech chosen
    real y[N,K];        // observed personal yields of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // bout or fruit
    int id[N];          // player id
    int N_effects;      // number of learning parameters to estimate
    int male[N];
    int adult[N];
}

parameters {
    vector[N_effects] mu;                   // average effects
    vector[N_effects] delta_a;               // age index variable
    vector[N_effects] delta_m;               // sex index variable
    vector<lower=0>[N_effects] sigma;       // standard deviations
    matrix[N_effects,J] zed;                // individual z-scores
    cholesky_factor_corr[N_effects] L_Rho;  // correlation matrix
}

transformed parameters{
    matrix[J,N_effects] a_id;
    a_id = (diag_pre_multiply(sigma,L_Rho) * zed)';
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    real PrS;        // social learning temp
    vector[K] s_temp;        
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;     // conform exponent

   
    //priors
    mu[1] ~ normal(1,0.6);
    mu[2] ~ normal(0,1);
    mu[3] ~ normal(0,1);
    mu[4] ~ normal(0,0.8);
    delta_a ~ normal(0,0.5);
    delta_m ~ normal(0,0.5);

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
            lambda = exp( mu[1] +  a_id[id[i],1] + delta_a[1]*adult[i] + delta_m[1]*male[i] ) ;
            phi = inv_logit( mu[2] + a_id[id[i],2] + delta_a[2]*adult[i] + delta_m[2]*male[i] );
            gamma = inv_logit(mu[3] + a_id[id[i],3] + delta_a[3]*adult[i] + delta_m[3]*male[i]);
            fc = exp(mu[4] + a_id[id[i],4] + delta_a[4]*adult[i] + delta_m[4]*male[i]);

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {


                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);

                PrS = s_temp[tech[i]]/sum(s_temp);
                
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
    vector[K] s_temp;        
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;     // conform exponent
    matrix[N_effects,N_effects] Rho;
    vector[N_effects] Sigma;

    Sigma = sigma;
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
            lambda = exp( mu[1] +  a_id[id[i],1] + delta_a[1]*adult[i] + delta_m[1]*male[i] ) ;
            phi = inv_logit( mu[2] + a_id[id[i],2] + delta_a[2]*adult[i] + delta_m[2]*male[i] );
            gamma = inv_logit(mu[3] + a_id[id[i],3] + delta_a[3]*adult[i] + delta_m[3]*male[i]);
            fc = exp(mu[4] + a_id[id[i],4] + delta_a[4]*adult[i] + delta_m[4]*male[i]);

        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                PrS = s_temp[tech[i]]/sum(s_temp);
                
                log_lik[i] = ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) ) ;  
            } else {
                 log_lik[i] = (logPrA);            }
        } else {
                 log_lik[i] = (logPrA);         }
     }//i  

}//end of model
