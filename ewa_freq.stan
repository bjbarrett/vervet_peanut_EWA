data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real y[N,K];        // observed personal yields of techs (1/0)
    real s[N,K];        // observed number of ttimes observing behaviors
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
    int sex_index[N];   //index variable for sex; 1 is female 2 is male
    int age_index[N];   //index variable for age; 1 is female 2 is male
}

parameters {
    matrix[N_effects,2] A;                  //age means
    matrix[N_effects,2] S;                  //sex  means
    vector<lower=0>[N_effects] sigma;       // standard deviations of varying effects
    matrix[N_effects,J] zed;                // individual z-scores for cholesky decomp
    cholesky_factor_corr[N_effects] L_Rho;  // correlation matrix
}

transformed parameters{
    matrix[J,N_effects] I;              //define varying effects for individuals
    I = (diag_pre_multiply(sigma,L_Rho) * zed)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp       
    real phi;           // stickiness parameter to recent experience
    real lambda;        // sensitivity to attraction scores
    real gamma;         // social weight
    real fc;            // conform exponent

    //priors
    to_vector(A[1,]) ~ normal(0,0.6);
    to_vector(S[1,]) ~ normal(0,0.6);
    to_vector(A[2,]) ~ normal(0,1);
    to_vector(S[2,]) ~ normal(0,1);
    to_vector(A[3,]) ~ normal(0,1);
    to_vector(S[3,]) ~ normal(0,1);
    to_vector(A[4,]) ~ normal(0,0.8);
    to_vector(S[4,]) ~ normal(0,0.8);
    sigma ~ exponential(1);
    to_vector(zed) ~ normal(0,1);
    L_Rho ~ lkj_corr_cholesky(4);
    //liklihood loop
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
            lambda = exp( I[id[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(I[id[i],2] + A[2,age_index[i]] + S[2,sex_index[i]] );
            gamma = inv_logit(I[id[i],3] + A[3,age_index[i]] + S[3,sex_index[i]] );
            fc = exp(I[id[i],4] + A[4,age_index[i]] + S[4,sex_index[i]]);
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                PrS = s_temp[tech[i]]/sum(s_temp);
                target += log( (1-gamma)*exp(logPrA) + gamma*PrS ) ;
            } else {
                target += logPrA;
            }
        } else {
            target += logPrA;
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
            lambda = exp( I[id[i],1] + A[1,age_index[i]] + S[1,sex_index[i]] ) ;
            phi= inv_logit(I[id[i],2] + A[2,age_index[i]] + S[2,sex_index[i]] );
            gamma = inv_logit(I[id[i],3] + A[3,age_index[i]] + S[3,sex_index[i]] );
            fc = exp(I[id[i],4] + A[4,age_index[i]] + S[4,sex_index[i]]);
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { //only socially learn when there is social information
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                PrS = s_temp[tech[i]]/sum(s_temp);
                //loglik for info criteria
                log_lik[i] = log( (1-gamma)*exp(logPrA) + gamma*PrS ) ;  
            } else {
                 log_lik[i] = logPrA;            }
        } else {
                 log_lik[i] = logPrA;         }
     }//i  

}//end of model
