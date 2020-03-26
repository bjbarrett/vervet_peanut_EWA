library(rstan)
library(rethinking)
library(RColorBrewer)
##if running locally, choose which model files
setwd("~/Dropbox/Vervets/vervet_peanut_EWA/slope models")
d <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Peanut_Vervet_20min.csv")
options(mc.cores = parallel::detectCores())

##if running on server
options(mc.cores = parallel::detectCores())
d <- read.csv("~/Peanut_Vervet_30min.csv")

#################################
##########slope models###########
#################################

parlistS=c("a_id" , "mu", "delta_m" , "delta_a", "sigma" ,"Rho" , "log_lik")

datalist_i <- list(
N = nrow(d),                                  #length of dataset
J = length( unique(d$ID_actor_index) ),       #number of individuals
K = max(d$technique_index),                   #number of processing techniques
tech = d$technique_index,                     #technique index
y = cbind( d$y1 , d$y2 , d$y3 ),              #individual payoff at timestep (1 if succeed, 0 is fail)
bout = d$forg_bout,                          #processing bout unique to individual J
id = d$ID_actor_index ,                      #individual ID
male=d$male,
adult=d$adult,
N_effects=2                               #number of parameters to estimates
)

##single strategy social learning
datalist_s <- list(
N = nrow(d),                            #length of dataset
J = length( unique(d$ID_actor_index) ),  #number of individuals
K = max(d$technique_index),         #number of processing techniques
tech = d$technique_index,           #technique index
y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
s = cbind(d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
q = cbind(d$pay1 , d$pay2 , d$pay3 ),
bout = d$forg_bout,#bout is forg index  unique to individual J
id = d$ID_actor_index ,                                           #individual ID
male=d$male,
adult=d$adult,
N_effects=4                                                                        #number of parameters to estimates
)

datalist_s$q <- datalist_s$q / max(datalist_s$q)
datalist_s$s <- datalist_s$s/ max(datalist_s$s)

##global model
datalist_g <- list(
N = nrow(d),                            #length of dataset
J = length( unique(d$ID_actor_index) ),  #number of individuals
K = max(d$technique_index),         #number of processing techniques
tech = d$technique_index,           #technique index
y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
s = cbind( d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
f = cbind( d$fem1 , d$fem2 , d$fem3 ),
k = cbind( d$kin1 , d$kin2 , d$kin3 ),
p = cbind( d$pay1 , d$pay2 , d$pay3 ),
r = cbind( d$rank1 , d$rank2 , d$rank3 ),
x = cbind( d$sex1 , d$sex2 , d$sex3 ),
bout = d$forg_bout, #bout is forg index  unique to individual J
id = d$ID_actor_index ,                                           #individual ID
male=d$male,
adult=d$adult,
N_effects=9                                                                        #number of parameters to estimates
)

datalist_g$s <- datalist_g$s/ max(datalist_g$s)
datalist_g$f <- datalist_g$f / max(datalist_g$f)
datalist_g$k <- datalist_g$k/ max(datalist_g$k)
datalist_g$p <- datalist_g$p/ max(datalist_g$p)
datalist_g$r <- datalist_g$r/ max(datalist_g$r)
datalist_g$x <- datalist_g$x/ max(datalist_g$x)


fit_i = stan( file = 'ewa_individual.stan', data = datalist_i ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.95) , pars=parlistS, refresh=100)

fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.95 ) , pars=parlistS, refresh=100 , init=0)
#same-sex bias
datalist_s$q =cbind(d$sex1 , d$sex2 , d$sex3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_sex = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=100 , init=0)
#rank_bias
datalist_s$q =cbind(d$rank1 , d$rank2 , d$rank3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_rank = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=100 , init=0)
#kin-bias
datalist_s$q =cbind(d$kin1 , d$kin2 , d$kin3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_kin = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=100, init=0)
###female bias
datalist_s$q = cbind(d$fem1 , d$fem2 , d$fem3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_fem = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=100 , init=0)
###Freq dep
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.999 ,  max_treedepth = 15) , pars=parlistS, refresh=100 , init=0)
#global model
fit_global = stan( file = 'ewa_global.stan', data = datalist_g ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.999 ,  max_treedepth = 15) , pars=parlistS, refresh=100 , init=0)

save(d,fit_i,fit_s,fit_fem,fit_pay,fit_kin,fit_rank,fit_sex,fit_global,file="5min_slopes_VervetPNUTnoglobal.rdata")


precis(fit_i , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_s , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_fem , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_kin , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_pay , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_rank , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_sex , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_global , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)

compare(fit_i, fit_s, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global)

###IL PLOTS##
post <- extract.samples(fit_i)
col.pal <- brewer.pal(4, "Paired")
##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)

#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

#delta_a
dens(post$delta_a[,1] , main="delta_a" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_a[,1]) ) 
shade( density(post$delta_a[,1]) , lim= as.vector(HPDI(post$delta_a[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
abline(v=median(post$delta_a[,2]) ) 
shade( density(post$delta_a[,2]) , lim= as.vector(HPDI(post$delta_a[,2], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi"), col = col.pal[1:2], pch = c(19,19), bty = "n", pt.cex = 0.5, cex = 0.5 )

#delta_m
dens(post$delta_m[,1] , main="delta_m" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_m[,1]) ) 
shade( density(post$delta_m[,1]) , lim= as.vector(HPDI(post$delta_m[,1], prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
abline(v=median(post$delta_m[,2]) ) 
shade( density(post$delta_m[,2]) , lim= as.vector(HPDI(post$delta_m[,2], prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi"), col = col.pal[3:4], pch = c(19,19), bty = "n", pt.cex = 0.5, cex = 0.5 )


##sigmas
dens(post$sigma[,1] , main="sigmas" , xlim=c(0,2) , col="white")##phi
shade( density(post$sigma[,1]) , lim= as.vector(HPDI(post$sigma[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
shade( density(post$sigma[,2]) , lim= as.vector(HPDI(post$sigma[,2], prob=0.999999999)) , col = col.alpha(col.pal[3], 0.25))
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )
legend("topright", legend = c("lambda", "phi"), col = col.pal[3:4], pch = c(19,19), bty = "n", pt.cex = 0.5, cex = 0.5 )

##FREQ_DEP_PLOTS##
post <- extract.samples(fit_s)
col.pal <- brewer.pal(8, "Paired")

##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)

#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)


#gamma
dens(logistic(post$mu[,3]) , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social info" , col="white")##phi
abline(v=median(logistic(post$mu[,3])) ) 
shade( density(logistic(post$mu[,3] )) , lim= as.vector(HPDI(logistic(post$mu[,3]), prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

##f
dens(exp(post$mu[,4]) , main=expression(paste(f_c)) , xlim=c(0,15) , xlab="strength of freq dep" , col="white")##phi
abline(v=median(exp(post$mu[,4])) ) 
shade( density(exp(post$mu[,4] )) , lim= as.vector(HPDI(exp(post$mu[,4]), prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( exp(rnorm( 20000 , 0 , 0.8)) , lty=2  , col="black" , adj=1 , add=TRUE)

#delta_a
dens(post$delta_a[,1] , main="delta_a" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_a[,1]) ) 
shade( density(post$delta_a[,1]) , lim= as.vector(HPDI(post$delta_a[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
abline(v=median(post$delta_a[,2]) ) 
shade( density(post$delta_a[,2]) , lim= as.vector(HPDI(post$delta_a[,2], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post$delta_a[,3]) ) 
shade( density(post$delta_a[,3]) , lim= as.vector(HPDI(post$delta_a[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
abline(v=median(post$delta_a[,4]) ) 
shade( density(post$delta_a[,4]) , lim= as.vector(HPDI(post$delta_a[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi", "gamma","f"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#delta_m
dens(post$delta_m[,1] , main="delta_m" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_m[,1]) ) 
shade( density(post$delta_m[,1]) , lim= as.vector(HPDI(post$delta_m[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
abline(v=median(post$delta_m[,2]) ) 
shade( density(post$delta_m[,2]) , lim= as.vector(HPDI(post$delta_m[,2], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post$delta_m[,3]) ) 
shade( density(post$delta_m[,3]) , lim= as.vector(HPDI(post$delta_m[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
abline(v=median(post$delta_m[,4]) ) 
shade( density(post$delta_m[,4]) , lim= as.vector(HPDI(post$delta_m[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi", "gamma","f"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

##sigmas
dens(post$sigma[,1] , main="sigmas" , xlim=c(0,2) , col="white")##phi
shade( density(post$sigma[,1]) , lim= as.vector(HPDI(post$sigma[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
shade( density(post$sigma[,2]) , lim= as.vector(HPDI(post$sigma[,2], prob=0.999999999)) , col = col.alpha(col.pal[3], 0.25))
shade( density(post$sigma[,3]) , lim= as.vector(HPDI(post$sigma[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
shade( density(post$sigma[,4]) , lim= as.vector(HPDI(post$sigma[,4], prob=0.999999999)) , col = col.alpha(col.pal[7], 0.25))
legend("topright", legend = c("lambda", "phi", "gamma","f"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )

##cue-bias of some kind
post <- extract.samples(fit_pay)
col.pal <- brewer.pal(8, "Paired")

##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)

#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)


#gamma
dens(logistic(post$mu[,3]) , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social info" , col="white")##phi
abline(v=median(logistic(post$mu[,3])) ) 
shade( density(logistic(post$mu[,3] )) , lim= as.vector(HPDI(logistic(post$mu[,3]), prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

##kappa
dens(post$mu[,4] , main=expression(paste(kappa)) , xlim=c(0,15) , xlab="strength of pay-off bias" , col="white")##phi
abline(v=median(post$mu[,4]) ) 
shade( density(post$mu[,4] ) , lim= as.vector(HPDI(post$mu[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

#delta_a
dens(post$delta_a[,1] , main="delta_a" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_a[,1]) ) 
shade( density(post$delta_a[,1]) , lim= as.vector(HPDI(post$delta_a[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
abline(v=median(post$delta_a[,2]) ) 
shade( density(post$delta_a[,2]) , lim= as.vector(HPDI(post$delta_a[,2], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post$delta_a[,3]) ) 
shade( density(post$delta_a[,3]) , lim= as.vector(HPDI(post$delta_a[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
abline(v=median(post$delta_a[,4]) ) 
shade( density(post$delta_a[,4]) , lim= as.vector(HPDI(post$delta_a[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi", "gamma","f"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#delta_m
dens(post$delta_m[,1] , main="delta_m" , xlim=c(-2,2) , col="white")##phi
abline(v=median(post$delta_m[,1]) ) 
shade( density(post$delta_m[,1]) , lim= as.vector(HPDI(post$delta_m[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
abline(v=median(post$delta_m[,2]) ) 
shade( density(post$delta_m[,2]) , lim= as.vector(HPDI(post$delta_m[,2], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post$delta_m[,3]) ) 
shade( density(post$delta_m[,3]) , lim= as.vector(HPDI(post$delta_m[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
abline(v=median(post$delta_m[,4]) ) 
shade( density(post$delta_m[,4]) , lim= as.vector(HPDI(post$delta_m[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( rnorm( 20000 , 0 , 0.5) , lty=2  , col="black" , adj=1 , add=TRUE)
legend("topright", legend = c("lambda", "phi", "gamma","f"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

##sigmas
dens(post$sigma[,1] , main="sigmas" , xlim=c(0,4) , col="white")##phi
shade( density(post$sigma[,1]) , lim= as.vector(HPDI(post$sigma[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
shade( density(post$sigma[,2]) , lim= as.vector(HPDI(post$sigma[,2], prob=0.999999999)) , col = col.alpha(col.pal[3], 0.25))
shade( density(post$sigma[,3]) , lim= as.vector(HPDI(post$sigma[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
shade( density(post$sigma[,4]) , lim= as.vector(HPDI(post$sigma[,4], prob=0.999999999)) , col = col.alpha(col.pal[7], 0.25))
legend("topright", legend = c("lambda", "phi", "gamma","k_pay"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )


precis(fit_sex , depth=3)
post <- extract(fit_i)

mean(exp(post3$mu[1]))#group
mean(exp(post3$mu[,1] + post3$delta_a[,1,1]))#juvenile
mean(exp(post3$mu[,1] + post3$delta_a[,1,2]))#adult

mean(exp(post2$mu[,1]))#juv
mean(exp(post2$mu[,1] + post2$delta_a[,1]))#adult

mean(exp(post1$mu[1]))#group
mean(exp(post1$mu[,1] + post1$delta_a[,1,1]))#juvenie
mean(exp(post1$mu[,1] + post1$delta_a[,1,2]))#adult

mean(exp(post0$mu[1]))#group

mean(logistic(post2$mu[,2]))
mean(logistic(post2$mu[,2] + post2$delta_a[,2]))

mean(exp(post$mu[,1]))

