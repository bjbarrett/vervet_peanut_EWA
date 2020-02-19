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
real<lower=0> lambda;   // mutlinomial error parameter
real mu;                   // average effects
real a_id[J];         //varying effects for individual leanring
real<lower=0> sigma;
}
model {
vector[K] AC;       // attraction scores
real logPrA;        // individual learning temp
real temp;		// stickiness parameter

//priors
lambda ~ exponential(1);
sigma ~ exponential(1);
mu ~ normal(0,1);
a_id ~ normal(0,sigma);
for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			temp= inv_logit( mu + a_id[id[i]] );
			AC[j]= (1-temp)*AC[j] + temp*y[i-1,j];
		} else {
			AC[j]= 0;
		}
	}//j
	logPrA= lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	target += ( logPrA );

	}//i
}

generated quantities {
real dev; // deviance
vector[K] AC;       // attraction scores
real logPrA;        // individual learning temp
real temp;		// stickiness parameter
vector[N] log_lik;
//vector[J] phi_i;          // stickiness parameter ve
//real phi;
real Sigma;
Sigma = sigma;



dev= 0;
for ( i in 1:N ) {
//update attractions
	for ( j in 1:K ) {
		if ( bout[i] > 1 ) {
			temp= inv_logit( mu + a_id[id[i]] );
			AC[j]= (1-temp)*AC[j] + temp*y[i-1,j];
		} else {
			AC[j]= 0;
		}
	}//j
	logPrA= lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
	dev= dev + -2*logPrA;
	log_lik[i]= logPrA ;

	}//i
	//phi= inv_logit(alpha);
	//for (j in 1:J){
	//	phi_i[j]= inv_logit( alpha + a_indiv[J] );
	//	}
}//end of model
