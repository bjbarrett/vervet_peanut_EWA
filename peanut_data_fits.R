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


precis(fit_i , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_s , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_fem , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_kin , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_pay , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_rank , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_sex , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)
precis(fit_global , pars=c('mu' , 'sigma', 'delta_m' , 'delta_a', 'a_id') , depth=3)

compare(fit_i, fit_s, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global)
################################
############IL PLOTS############
####################### #########

post <- extract.samples(fit_i)
col.pal <- brewer.pal(4, "Paired")
##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)


#sex diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*0)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*0)
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*1)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 )
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

#sex diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*0)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*0)
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

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
shade( density(post$sigma[,1]) , lim= as.vector(HPDI(post$sigma[,1], prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
shade( density(post$sigma[,2]) , lim= as.vector(HPDI(post$sigma[,2], prob=0.999999999)) , col = col.alpha(col.pal[3], 0.25))
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )
legend("topright", legend = c("lambda", "phi"), col = col.pal[2:3], pch = c(19,19), bty = "n", pt.cex = 0.5, cex = 0.5 )

############################
##########FREQ_DEP_PLOTS####
############################

post <- extract.samples(fit_s)
col.pal <- brewer.pal(8, "Paired")

##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)

#sex diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*0)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*0)
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*1)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 )
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )


#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)


#sex diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*0)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*0)
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

####gamma####
dens(logistic(post$mu[,3]) , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social info" , col="white")
abline(v=median(logistic(post$mu[,3])) ) 
shade( density(logistic(post$mu[,3] )) , lim= as.vector(HPDI(logistic(post$mu[,3]), prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

#sex diff
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*0)
post_plot <- logistic(post$mu[,3]  + post$delta_m[,3]*0)
dens(post_plot , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*1)
post_plot <- logistic(post$mu[,3]  + post$delta_m[,3]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 + post$delta_m[,3]*mean(d$male) )
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 )
dens(post_plot , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 + post$delta_m[,3]*mean(d$male) )
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

##f
dens(exp(post$mu[,4]) , main=expression(paste(f_c)) , xlim=c(0,15) , xlab="strength of freq dep" , col="white")##phi
abline(v=median(exp(post$mu[,4])) ) 
shade( density(exp(post$mu[,4] )) , lim= as.vector(HPDI(exp(post$mu[,4]), prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( exp(rnorm( 20000 , 0 , 0.8)) , lty=2  , col="black" , adj=1 , add=TRUE)


#sex diff
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*0)
post_plot <- exp(post$mu[,4] + post$delta_m[,4]*0)
dens(post_plot , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*1)
post_plot <- exp(post$mu[,4] + post$delta_m[,4]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*0 + post$delta_m[,4]*mean(d$male))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*0 )
dens(post_plot , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*1 + post$delta_m[,4]*mean(d$male))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#######just priors

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

##########################
##cue-bias of some kind###
##########################
precis(fit_fem , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_kin , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_pay , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_rank , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)
precis(fit_sex , pars=c('mu' , 'sigma', 'delta_s' , 'delta_a', 'a_id') , depth=3)

model_list <- list( fit_fem , fit_kin, fit_pay, fit_rank, fit_sex)
kpar_names <- c( "k_fem" , "k_kin" , "k_pay" , "k_rank" , "k_sex")
col.pal <- brewer.pal(8, "Paired")



for (i in 1:5){
  post <- extract.samples(model_list[[i]])

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
  dens(post$mu[,4] , main=kpar_names[i] , xlim=c(0,15) , xlab="strength of cue bias" , col="white")##phi
  abline(v=median(post$mu[,4]) ) 
  shade( density(post$mu[,4] ) , lim= as.vector(HPDI(post$mu[,4], prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  dens( rnorm( 20000 , 0 , 1) , lty=2  , col="black" , adj=1 , add=TRUE)
  
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
  legend("topright", legend = c("lambda", "phi", "gamma",kpar_names[i]), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
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
  legend("topright", legend = c("lambda", "phi", "gamma",kpar_names[i]), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  ##sigmas
  dens(post$sigma[,1] , main="sigmas" , xlim=c(0,4) , col="white")##phi
  shade( density(post$sigma[,1]) , lim= as.vector(HPDI(post$sigma[,1], prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
  shade( density(post$sigma[,2]) , lim= as.vector(HPDI(post$sigma[,2], prob=0.999999999)) , col = col.alpha(col.pal[3], 0.25))
  shade( density(post$sigma[,3]) , lim= as.vector(HPDI(post$sigma[,3], prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
  shade( density(post$sigma[,4]) , lim= as.vector(HPDI(post$sigma[,4], prob=0.999999999)) , col = col.alpha(col.pal[7], 0.25))
  legend("topright", legend = c("lambda", "phi", "gamma",kpar_names[i]), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )
}

####comparisons
for (i in 1:5){
    post <- extract.samples(model_list[[i]])
    ##lambda
    #sex diff
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*0)
    post_plot <- exp(post$mu[,1]  + post$delta_m[,1]*0)
    dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*1)
    post_plot <- exp(post$mu[,1] + post$delta_m[,1]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 + post$delta_m[,1]*mean(d$male))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 )
    dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 + post$delta_m[,1]*mean(d$male))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 )
    
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    
    #phi
    #sex diff
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*0)
    post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*0)
    dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
    post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
    dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 )
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    ####gamma####
    #sex diff
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*0)
    post_plot <- logistic(post$mu[,3] + post$delta_m[,3]*0)
    dens(post_plot , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*1)
    post_plot <- logistic(post$mu[,3] + post$delta_m[,3]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 + post$delta_m[,3]*mean(d$male) )
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 )
    dens(post_plot , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 + post$delta_m[,3]*mean(d$male) )
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 )
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    ##kappa of some kind
    
    kpar_names[i]
    #sex diff
    post_plot <- post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*0
    post_plot <- post$mu[,4] + post$delta_m[,4]*0
    dens(post_plot , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
    post_plot <- post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*1
    post_plot <- post$mu[,4] + post$delta_m[,4]*1
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- post$mu[,4] + post$delta_a[,4]*0 + post$delta_m[,4]*mean(d$male)
    post_plot <- post$mu[,4] + post$delta_a[,4]*0 
    dens(post_plot , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
    post_plot <- post$mu[,4] + post$delta_a[,4]*1 + post$delta_m[,4]*mean(d$male)
    post_plot <- post$mu[,4] + post$delta_a[,4]*1
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
}

####################################
##################global model######
####################################

post <- extract.samples(fit_global)
col.pal <- brewer.pal(8, "Paired")

##lambda
dens(exp(post$mu[,1]) , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(exp(post$mu[,1])) ) 
shade( density(exp(post$mu[,1] )) , lim= as.vector(HPDI(exp(post$mu[,1]), prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
dens( exp(rnorm( 20000 , 1 , 0.6)) , lty=2  , col="black" , adj=1 , add=TRUE)

#sex diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*0)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*0)
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*1)
post_plot <- exp(post$mu[,1] + post$delta_m[,1]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 )
dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 + post$delta_m[,1]*mean(d$male))
post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )


#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)


#sex diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*0)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*0)
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

####gamma####
dens(logistic(post$mu[,3]) , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social info" , col="white")
abline(v=median(logistic(post$mu[,3])) ) 
shade( density(logistic(post$mu[,3] )) , lim= as.vector(HPDI(logistic(post$mu[,3]), prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)

#sex diff
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*0)
post_plot <- logistic(post$mu[,3]  + post$delta_m[,3]*0)
dens(post_plot , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*1)
post_plot <- logistic(post$mu[,3]  + post$delta_m[,3]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 + post$delta_m[,3]*mean(d$male) )
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 )
dens(post_plot , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 + post$delta_m[,3]*mean(d$male) )
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

##f
dens(exp(post$mu[,4]) , main=expression(paste(f_c)) , xlim=c(0,15) , xlab="strength of freq dep" , col="white")##phi
abline(v=median(exp(post$mu[,4])) ) 
shade( density(exp(post$mu[,4] )) , lim= as.vector(HPDI(exp(post$mu[,4]), prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
dens( exp(rnorm( 20000 , 0 , 0.8)) , lty=2  , col="black" , adj=1 , add=TRUE)


#sex diff
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*0)
post_plot <- exp(post$mu[,4] + post$delta_m[,4]*0)
dens(post_plot , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*1)
post_plot <- exp(post$mu[,4] + post$delta_m[,4]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*0 + post$delta_m[,4]*mean(d$male))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*0 )
dens(post_plot , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*1 + post$delta_m[,4]*mean(d$male))
post_plot <- exp(post$mu[,4] + post$delta_a[,4]*1 )
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
legend("topright", legend = c("juvenile", "adult"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#######just priors

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


##kappa
col.pal2 <- brewer.pal(8 ,"Dark2")
for (i in 1:5){
  dens(post$mu[,4 + i] , main=kpar_names[i] , xlim=c(-2,5) , xlab="strength of cue bias" , col="white")##phi
  abline(v=median(post$mu[,4 + i]) , col=col.pal2[i] ) 
  shade( density(post$mu[,4+i] ) , lim= as.vector(HPDI(post$mu[,5], prob=0.9999)) , col = col.alpha(col.pal2[i], 0.25))
  dens( rnorm( 100000 , 0 , 1) , lty=2  , col=1 , adj=1 , add=TRUE) 
}

##sigmas
dens(post$sigma , main="sigmas" , xlim=c(0,6) , col="white" , ylim=c(0,2.5) )##phi
for(i in 1:9){
  shade( density(post$sigma[,i]) , lim= as.vector(HPDI(post$sigma[,i], prob=0.9999)) , col = col.alpha(col.pal2[i], 0.25))
}
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )
legend("topright", legend = c("lambda", "phi", "gamma",kpar_names), col = col.pal2, pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#############################3
########raw data plots#########
################################

library(RColorBrewer)
col.pal=brewer.pal(3,"Accent")
d$group_index <- as.integer(d$group)
d$nobs_group_all <- d$nobs_group_ch <- d$nobs_group_cms <- d$nobs_group_cmt <- d$nobs_id_all <- d$nobs_id_ch <- d$nobs_id_cms <- d$nobs_id_cmt <-  66

for (i in 1:nrow(d) ) {
  d$nobs_group_all[i] <- length(unique(d$obs_index[d$group_index==d$group_index[i] & d$date_index==d$date_index[i] ] ) )
  d$nobs_group_ch[i] <- length(unique(d$obs_index[d$group_index==d$group_index[i] & d$date_index==d$date_index[i] & d$technique_index==1 ] ) )
  d$nobs_group_cms[i] <- length(unique(d$obs_index[d$group_index==d$group_index[i] & d$date_index==d$date_index[i] & d$technique_index==2 ] ) )
  d$nobs_group_cmt[i] <- length(unique(d$obs_index[d$group_index==d$group_index[i] & d$date_index==d$date_index[i] & d$technique_index==3 ] ) )
  d$nobs_id_all[i] <- length(unique(d$obs_index[d$ID_actor==d$ID_actor[i] & d$date_index==d$date_index[i] ] ) )
  d$nobs_id_ch[i] <- length(unique(d$obs_index[d$ID_actor==d$ID_actor[i] & d$date_index==d$date_index[i] & d$technique_index==1 ] ) )
  d$nobs_id_cms[i] <- length(unique(d$obs_index[d$ID_actor==d$ID_actor[i] & d$date_index==d$date_index[i] & d$technique_index==2 ] ) )
  d$nobs_id_cmt[i] <- length(unique(d$obs_index[d$ID_actor==d$ID_actor[i] & d$date_index==d$date_index[i] & d$technique_index==3 ] ) )
}

dg <- aggregate(cbind(d$nobs_group_ch , d$nobs_group_cms , d$nobs_group_cmt ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , nobs_group_ch=d$nobs_group_ch , nobs_group_cms=d$nobs_group_cms , nobs_group_cmt=d$nobs_group_cmt , nobs_group_all=d$nobs_group_all ) , mean ) #gets neat summary table for plot
dg$V1 <-  dg$V1/dg$nobs_group_all
dg$V2 <-  dg$V2/dg$nobs_group_all
dg$V3 <-  dg$V3/dg$nobs_group_all
dg<- dg[order( dg$date_index),]

##group level noha plots raw data
dgNoha <- dg[dg$group=="Noha",]
dgKubu <- dg[dg$group=="Kubu",]


##individual
di <- aggregate(cbind(d$nobs_id_ch , d$nobs_id_cms , d$nobs_id_cmt ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , nobs_group_ch=d$nobs_group_ch , nobs_group_cms=d$nobs_group_cms , nobs_group_cmt=d$nobs_group_cmt , nobs_group_all=d$nobs_group_all , ID_actor=d$ID_actor , ID_actor_index= d$ID_actor_index ,  nobs_id_ch=d$nobs_id_ch , nobs_id_cms=d$nobs_id_cms , nobs_id_cmt=d$nobs_id_cmt , nobs_id_all=d$nobs_id_all)  , mean ) #gets neat summary table for plot
di$V1 <-  di$V1/di$nobs_id_all
di$V2 <-  di$V2/di$nobs_id_all
di$V3 <-  di$V3/di$nobs_id_all
di<- di[order( di$date_index),]
di<- di[order( di$ID_actor_index),]

##group level noha plots raw data
diNoha <- di[di$group=="Noha",]
diKubu <- di[di$group=="Kubu",]

##group level plots
dev.off()
plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="Noha" ) 
lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l")
points(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l")
points(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l")

plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all) , main="Kubu" ) 
points(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , type="l", lty=3)
points(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , type="l", lty=3)
points(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , type="l" , lty=3)

###individual
plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="Noha" ) 
lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l" , lw=3)
points(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l" , lw=3)
points(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l" , lw=3)

for (i in sort(unique(diNoha$ID_actor_index)) ) {
  lines(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[1] , type="l" , lty=1 , lw=0.25)
  lines(diNoha[diNoha$ID_actor_index==i,]$V2 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[2] , type="l" , lty=1 , lw=0.25)
  lines(diNoha[diNoha$ID_actor_index==i,]$V3 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[3] , type="l" , lty=1 , lw=0.25)
  # points(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[1] , pch=18 )
  # points(diNoha[diNoha$ID_actor_index==i,]$V2 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[2] , pch=18)
  # points(diNoha[diNoha$ID_actor_index==i,]$V3 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[3] , pch=18)
}

###individual
plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all) , main="Kubu" ) 
lines(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , type="l" , lw=3)
points(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , pch=19 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , type="l" , lw=3)
points(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , pch=19 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , type="l" , lw=3)

for (i in sort(unique(diKubu$ID_actor_index)) ) {
  lines(diKubu[diKubu$ID_actor_index==i,]$V1 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[1] , type="l" , lty=1 , lw=0.25)
  lines(diKubu[diKubu$ID_actor_index==i,]$V2 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[2] , type="l" , lty=1 , lw=0.25)
  lines(diKubu[diKubu$ID_actor_index==i,]$V3 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[3] , type="l" , lty=1 , lw=0.25)
  # points(diKubu[diKubu$ID_actor_index==i,]$V1 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[1] , pch=18 )
  # points(diKubu[diKubu$ID_actor_index==i,]$V2 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[2] , pch=18)
  # points(diKubu[diKubu$ID_actor_index==i,]$V3 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[3] , pch=18)
}


###plot individual predictions for Noha
plot.new()
par(mfrow = c(9, 3))
par(cex = 0.7)
par(mar = c(2, 2, 0, 0), oma = c(2, 2, 1, .1))

plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day" , type="l") 
title( main="Group Average", outer=FALSE , line=-0.75 , cex=0.5)

lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l" , lw=2)
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l" , lw=2)
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l" , lw=2)

for (i in sort(unique(diNoha$ID_actor_index)) ) {
  plot(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col="white" , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day" , cex.lab=0.5 ) 
  title( main=unique(diNoha$ID_actor[diNoha$ID_actor_index==i]) , outer=FALSE , line=-0.75 , cex=0.5)
  lines(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[1] , type="l" , lty=1 , pch=19)
  lines(diNoha[diNoha$ID_actor_index==i,]$V2 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[2] , type="l" , lty=1 , , pch=19)
  lines(diNoha[diNoha$ID_actor_index==i,]$V3 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[3] , type="l" , lty=1 , , pch=19)
}


###plot individual predictions for Noha
plot.new()
par(mfrow = c(9, 3))
par(cex = 0.7)
par(mar = c(2, 2, 0, 0), oma = c(2, 2, 1, .1))

plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day") 
title( main="Group Average", outer=FALSE , line=-0.75 , cex=0.5)

lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l" , lw=2)
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l" , lw=2)
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l" , lw=2)

for (i in sort(unique(diNoha$ID_actor_index)) ) {
  plot(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col="white" , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day" , cex.lab=0.5 ) 
  title( main=unique(diNoha$ID_actor[diNoha$ID_actor_index==i]) , outer=FALSE , line=-0.75 , cex=0.5)
  lines(diNoha[diNoha$ID_actor_index==i,]$V1 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[1] , type="b" , lty=1 , pch=19)
  lines(diNoha[diNoha$ID_actor_index==i,]$V2 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[2] , type="b" , lty=1 , , pch=19)
  lines(diNoha[diNoha$ID_actor_index==i,]$V3 ~ diNoha[diNoha$ID_actor_index==i,]$date_index , col=col.pal[3] , type="b" , lty=1 , , pch=19)
}

###Kubu

###plot individual predictions for Kobo
plot.new()
par(mfrow = c(4, 3))
par(cex = 0.7)
par(mar = c(2, 2, 0, 0), oma = c(2, 2, 1, .1))

plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day") 
title( main="Group Average", outer=FALSE , line=-0.75 , cex=0.5)

lines(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , type="l" , lw=2)
lines(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , type="l" , lw=2)
lines(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , type="l" , lw=2)

for (i in sort(unique(diKubu$ID_actor_index)) ) {
  plot(diKubu[diKubu$ID_actor_index==i,]$V1 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col="white" , ylim=c(0,1) , xlim=c(1,18) ,  ylab="prob individual uses technique" , xlab="experimental day" , cex.lab=0.5 ) 
  title( main=unique(diKubu$ID_actor[diKubu$ID_actor_index==i]) , outer=FALSE , line=-0.75 , cex=0.5)
  lines(diKubu[diKubu$ID_actor_index==i,]$V1 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[1] , type="b" , lty=1 , pch=19)
  lines(diKubu[diKubu$ID_actor_index==i,]$V2 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[2] , type="b" , lty=1 , , pch=19)
  lines(diKubu[diKubu$ID_actor_index==i,]$V3 ~ diKubu[diKubu$ID_actor_index==i,]$date_index , col=col.pal[3] , type="b" , lty=1 , , pch=19)
}



