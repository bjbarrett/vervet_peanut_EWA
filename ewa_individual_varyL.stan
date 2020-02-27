data {
int K;              // num options
int N;              // num observations
int J;              // num individuals
int tech[N];        // tech chosen
real y[N,K];        // observed personal yields of techs
int bout[N];      // bout
int id[N];          // player id
int N_effects;      // number of learning parameters to estimate

}
parameters {
vector[N_effects] mu;                   // average effects
real a_id[J];         //varying effects for individual leanring
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
real phi;		// stickiness parameter

//priors
    mu[1] ~ normal(0,0.6);
    mu[2] ~ normal(0,1);
    sigma ~ exponential(1);
    to_vector(zed) ~ normal(0,1);
    L_Rho ~ lkj_corr_cholesky(3);

for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			phi= inv_logit( mu[2] + a_id[id[i]] );
			AC[j]= (1-phi)*AC[j] + phi*y[i-1,j];
		} else {
			AC[j]= 0;
		}
	}//j

	lambda = exp(mu[1] +  a_id[id[i],1]);
	logPrA= lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	target += ( logPrA );

	}//i
}

generated quantities {
vector[K] AC;       // attraction scores
real logPrA;        // individual learning temp
real temp;		// stickiness parameter
vector[N] log_lik;


for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			phi= inv_logit( mu[2] + a_id[id[i]] );
			AC[j]= (1-phi)*AC[j] + phi*y[i-1,j];
		} else {
			AC[j]= 0;
		}
	}//j

	lambda = exp(mu[1] +  a_id[id[i],1]);
	logPrA= lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	log_lik[i]= logPrA ;

}//end of model
