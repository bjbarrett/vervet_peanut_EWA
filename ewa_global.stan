
data {
    int K;              // num options(gnomes)
    int N;              // num observations
    int J;              // num individuals
    int tech[N];        // tech chosen
    real y[N,K];        // observed personal yields of techs 1-K
    real f[N,K];       // observed female social variables of techs 1-K
    real k[N,K];       // observed kin-bias social variables of techs 1-K
    real p[N,K];       // observed payoff social variables of techs 1-K
    real r[N,K];       // observed rank-bias social variables of techs 1-K
    real s[N,K];        // observed number of ttimes observing behaviors
    real x[N,K];        // sex-bias
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
    vector[K] lin_mod;
    vector[K] s_temp;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;             //frequency dependence
    real bf;             // female-bias
    real bk;             // kin-bias
    real bp;             // payoff-bias
    real br;             // rank-bias
    real bx;             // sex-bias

    //priors
    mu[1] ~ normal(0,0.6);
    mu[2] ~ normal(0,1);
    mu[3] ~ normal(0,1);
    mu[4] ~ normal(0,0.5);
    mu[5] ~ normal(0,1);
    mu[6] ~ normal(0,1);
    mu[7] ~ normal(0,1);
    mu[8] ~ normal(0,1);
    mu[9] ~ normal(0,1);

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
            bf =  mu[5] + a_id[id[i],5] + delta_a[5]*adult[i] + delta_m[5]*male[i] ; // female
            bk =  mu[6] + a_id[id[i],6] + delta_a[6]*adult[i] + delta_m[6]*male[i] ; // kin
            bp =  mu[7] + a_id[id[i],7] + delta_a[7]*adult[i] + delta_m[7]*male[i] ; // payoff
            br =  mu[8] + a_id[id[i],8] + delta_a[8]*adult[i] + delta_m[8]*male[i] ; // rank
            bx =  mu[9] + a_id[id[i],9] + delta_a[9]*adult[i] + delta_m[9]*male[i] ; // same sex
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( bf*f[i,j] + bk*k[i,j] + bp*p[i,j] + br*r[i,j] + bx*x[i,j] );
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                for ( j in 1:K ) lin_mod[j] = lin_mod[j] * s_temp[j];
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
    vector[K] s_temp;
    real lambda;           // stickiness parameter
    real phi;           // stickiness parameter
    real gamma;         // social weight
    real fc;             //frequency dependence
    real bf;             // female-bias
    real bk;             // kin-bias
    real bp;             // payoff-bias
    real br;             // rank-bias
    real bx;             // sex-bias
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
            lambda = exp( mu[1] +  a_id[id[i],1] + delta_a[1]*adult[i] + delta_m[1]*male[i] ) ;
            phi = inv_logit( mu[2] + a_id[id[i],2] + delta_a[2]*adult[i] + delta_m[2]*male[i] );
            gamma = inv_logit(mu[3] + a_id[id[i],3] + delta_a[3]*adult[i] + delta_m[3]*male[i]);
            fc = exp(mu[4] + a_id[id[i],4] + delta_a[4]*adult[i] + delta_m[4]*male[i]);
            bf =  mu[5] + a_id[id[i],5] + delta_a[5]*adult[i] + delta_m[5]*male[i] ; // female
            bk =  mu[6] + a_id[id[i],6] + delta_a[6]*adult[i] + delta_m[6]*male[i] ; // kin
            bp =  mu[7] + a_id[id[i],7] + delta_a[7]*adult[i] + delta_m[7]*male[i] ; // payoff
            br =  mu[8] + a_id[id[i],8] + delta_a[8]*adult[i] + delta_m[8]*male[i] ; // rank
            bx =  mu[9] + a_id[id[i],9] + delta_a[9]*adult[i] + delta_m[9]*male[i] ; // same sex
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );

        //social learning below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) { // only socially learn if there is social info

                // compute non-frequency cues as log-linear model
                for ( j in 2:K ) {
                    lin_mod[j] = exp( bf*f[i,j] + bk*k[i,j] + bp*p[i,j] + br*r[i,j] + bx*x[i,j] );
                }
                lin_mod[1] = 1; // aliased outcome

                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],fc);
                for ( j in 1:K ) lin_mod[j] = lin_mod[j] * s_temp[j];
                PrS = lin_mod[tech[i]]/sum(lin_mod);

                log_lik[i] = ( log( (1-gamma)*exp(logPrA) + gamma*PrS ) ) ;   

            } else {
                 log_lik[i] = (logPrA);
            }
        } else {
                 log_lik[i] = (logPrA);         }
     }//i  

}//end of model
