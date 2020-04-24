library(rstan)
library(rethinking)

##if running on server
options(mc.cores = parallel::detectCores())
d <- read.csv("~/Peanut_Vervet_20min.csv")

#################################
##########slope models###########
#################################

d$sex_index <- d$male + 1
d$age_index <- d$adult + 1
d$group_index <- as.integer(d$group)

load("/Users/BJB/Downloads/vervet_peanut_ewa_global_20min_24April2020.rdata")##if working with existing workspace

datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$ID_actor_index) ),       #number of individuals
  K = max(d$technique_index),                   #number of processing techniques
  tech = d$technique_index,                     #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),              #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,                          #processing bout unique to individual J
  id = d$ID_actor_index ,                      #individual ID
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
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
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
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
  sex_index=d$sex_index,
  age_index=d$age_index,
  group_index=d$group_index,
  N_effects=9                                                                        #number of parameters to estimates
)

datalist_g$s <- datalist_g$s/ max(datalist_g$s)
datalist_g$f <- datalist_g$f / max(datalist_g$f)
datalist_g$k <- datalist_g$k/ max(datalist_g$k)
datalist_g$p <- datalist_g$p/ max(datalist_g$p)
datalist_g$r <- datalist_g$r/ max(datalist_g$r)
datalist_g$x <- datalist_g$x/ max(datalist_g$x)

save(fit_global , d , file="vervet_peanut_ewa_global_20min_24April2020.rdata")
#########code to plot preds#############
fit_i_p = stan( file = 'fit_i_preds.stan', data = datalist_i ,iter = 1000, warmup=500, chains=5, cores=5, control=list(adapt_delta=0.95) , pars=parlist, refresh=100)
plot(precis(fit_i_p , pars=c('A' , 'S' , 'G') , depth=3))
post <- extract(fit_i_p)
preds<-post$PrPreds

plot(1:length(x1) , x1 , col="red" , xlim=c(0,1000) , ylim=c(0,1))
points(1:length(x2) , x2 , col="blue")
points(1:length(x3) , x3, col="green")
###########################################
parlist <- c("A" ,"G" , "S" , "I", "sigma_i" ,"Rho_i" , "sigma_g" ,"Rho_g" , "log_lik" ,"PrPreds" )

fit_i = stan( file = 'ewa_individual.stan', data = datalist_i ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.95) , pars=parlist, refresh=100)
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
fit_freq = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.9999 ,  max_treedepth = 15) , pars=c("A" ,"S" , "I", "sigma", "Rho" , "log_lik" ), refresh=100 , init=0)

#same-sex bias
datalist_s$q =cbind(d$sex1 , d$sex2 , d$sex3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_sex=stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
#rank_bias
datalist_s$q =cbind(d$rank1 , d$rank2 , d$rank3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_rank = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
#kin-bias
datalist_s$q =cbind(d$kin1 , d$kin2 , d$kin3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_kin = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
###female bias
datalist_s$q = cbind(d$fem1 , d$fem2 , d$fem3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_fem = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.99) , pars=parlist, refresh=100 , init=0)
fit_global = stan( file = 'ewa_global.stan', data = datalist_g ,iter = 6000, warmup=3000, chains=4, cores=4, control=list(adapt_delta=0.9999 ,  max_treedepth = 15) , pars=parlist, refresh=100 , init=0)


########3inspect parameters and fits########
precis(fit_global, depth=3 , pars=c('A' , 'S'))
precis(fit_global, depth=3 , pars=c('I'))
precis(fit_global, depth=3 , pars=c('G'))
precis(fit_global, depth=3 , pars=c('sigma_i' , 'sigma_g') )
precis(fit_global, depth=3 , pars=c('Rho_i' ) )
precis(fit_global, depth=3 , pars=c('Rho_g' ) )
########info criteria#########
WAICtab <- compare(fit_i, fit_freq, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global)
#######plot main effects######

post <- extract(fit_global)


pp <- list(
  # lambda_female = exp(post$S[,1,1] + apply(post$A[,1,] , 1 ,mean) ), 
  # lambda_male = exp(post$S[,1,2] + apply(post$A[,1,] , 1 ,mean) ), 
  # lambda_juv = exp(post$A[,1,1] + apply(post$S[,1,] , 1 ,mean) ), 
  # lambda_adult = exp(post$A[,1,2] + apply(post$S[,1,] , 1 ,mean) ), 
  phi_female = logistic(post$S[,2,1] + apply(post$A[,2,] , 1 ,mean) ), 
  phi_male = logistic(post$S[,2,2] + apply(post$A[,2,] , 1 ,mean) ), 
  phi_juv = logistic(post$A[,2,1] + apply(post$S[,2,] , 1 ,mean) ), 
  phi_adult = logistic(post$A[,2,2] + apply(post$S[,2,] , 1 ,mean) ),
  gamma_female = logistic(post$S[,3,1] + apply(post$A[,3,] , 1 ,mean) ), 
  gamma_male = logistic(post$S[,3,2] + apply(post$A[,3,] , 1 ,mean) ), 
  gamma_juv = logistic(post$A[,3,1] + apply(post$S[,3,] , 1 ,mean) ), 
  gamma_adult = logistic(post$A[,3,2] + apply(post$S[,3,] , 1 ,mean) )
  # fc_female = exp(post$S[,4,1] + apply(post$A[,4,] , 1 ,mean) ), 
  # fc_male = exp(post$S[,4,2] + apply(post$A[,4,] , 1 ,mean) ), 
  # fc_juv = exp(post$A[,4,1] + apply(post$S[,4,] , 1 ,mean) ), 
  # fc_adult = exp(post$A[,4,2] + apply(post$S[,4,] , 1 ,mean) ),
  # k_fem_female = post$S[,5,1] + apply(post$A[,5,] , 1 ,mean) , 
  # k_fem_male = post$S[,5,2] + apply(post$A[,5,] , 1 ,mean) , 
  # k_fem_juv = post$A[,5,1] + apply(post$S[,5,] , 1 ,mean) , 
  # k_fem_adult = post$A[,5,2] + apply(post$S[,5,] , 1 ,mean) , 
  # k_kin_female = post$S[,6,1] + apply(post$A[,6,] , 1 ,mean) , 
  # k_kin_male = post$S[,6,2] + apply(post$A[,6,] , 1 ,mean) , 
  # k_kin_juv = post$A[,6,1] + apply(post$S[,6,] , 1 ,mean) , 
  # k_kin_adult = post$A[,6,2] + apply(post$S[,6,] , 1 ,mean) , 
  # k_pay_female = post$S[,7,1] + apply(post$A[,7,] , 1 ,mean) , 
  # k_pay_male = post$S[,7,2] + apply(post$A[,7,] , 1 ,mean) , 
  # k_pay_juv = post$A[,7,1] + apply(post$S[,7,] , 1 ,mean) , 
  # k_pay_adult = post$A[,7,2] + apply(post$S[,7,] , 1 ,mean),
  # k_rank_female = post$S[,8,1] + apply(post$A[,8,] , 1 ,mean) , 
  # k_rank_male = post$S[,8,2] + apply(post$A[,8,] , 1 ,mean) , 
  # k_rank_juv = post$A[,8,1] + apply(post$S[,8,] , 1 ,mean) , 
  # k_rank_adult = post$A[,8,2] + apply(post$S[,8,] , 1 ,mean),  
  # k_sex_female = post$S[,9,1] + apply(post$A[,9,] , 1 ,mean) , 
  # k_sex_male = post$S[,9,2] + apply(post$A[,9,] , 1 ,mean) , 
  # k_sex_juv = post$A[,9,1] + apply(post$S[,9,] , 1 ,mean) , 
  # k_sex_adult = post$A[,9,2] + apply(post$S[,9,] , 1 ,mean)  
)
plot(precis(pp , depth=2) )
precis(pp)


##########individual-predictions#############

l4p <- unique(subset( d , select=c("sex_index" , "age_index" , "ID_actor_index")))

lambda_list <- phi_list <- gamma_list <- fc_list <- kappa_list <-  lambda_list_CI <- phi_list_CI <- gamma_list_CI <- fc_list_CI <- kappa_list_CI  <- rep(0,nrow(l4p))
for (i in 1:nrow(l4p)){
  lambda_list[i] = mean(exp( post$I[,l4p$ID_actor_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] )) 
  phi_list[i] = mean(logistic( post$I[,l4p$ID_actor_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) )
  gamma_list[i] = mean(logistic( post$I[,l4p$ID_actor_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) )
  fc_list[i] = mean(exp( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] )) 
  kappa_list[i] = mean( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] ) 
  
  lambda_list_CI[i] = HPDI(exp( post$I[,l4p$ID_actor_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] ) )
  phi_list_CI[i] = HPDI(logistic( post$I[,l4p$ID_actor_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) )
  gamma_list_CI[i] = HPDI(logistic( post$I[,l4p$ID_actor_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) )
  fc_list_CI[i] = HPDI(exp( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] )) 
  kappa_list_CI[i] = mean( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] ) 
  
}
phi_list
lambda_list
gamma_list


#######CHECK PRIORS#####





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

#phi
dens(logistic(post$mu[,2]) , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(logistic(post$mu[,2])) ) 
shade( density(logistic(post$mu[,2] )) , lim= as.vector(HPDI(logistic(post$mu[,2]), prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
dens( logistic(rnorm( 20000 , 0 , 1)) , lty=2  , col="black" , adj=1 , add=TRUE)


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
median(post_plot)
HPDI(post_plot)

dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
median(post_plot)
post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
median(post_plot)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
#post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
#post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1)
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
post_plot <- logistic(post$mu[,3]  + post$de lta_m[,3]*1)
shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

#age diff
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 + post$delta_m[,3]*mean(d$male) )
#post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 )
dens(post_plot , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 + post$delta_m[,3]*mean(d$male) )
#post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 )
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
model_list <- list(  fit_pay)
kpar_names <- c( "k_pay" )

col.pal <- brewer.pal(8, "Paired")



for (i in 1:1){
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
    #post_plot <- exp(post$mu[,1]  + post$delta_m[,1]*0)
    dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*mean(d$adult) + post$delta_m[,1]*1)
    #post_plot <- exp(post$mu[,1] + post$delta_m[,1]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 + post$delta_m[,1]*mean(d$male))
    #post_plot <- exp(post$mu[,1] + post$delta_a[,1]*0 )
    dens(post_plot , main=expression(paste(lambda)) , xlim=c(0,15) , xlab="sensitivity to attraction scores" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[1], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
    post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 + post$delta_m[,1]*mean(d$male))
    #post_plot <- exp(post$mu[,1] + post$delta_a[,1]*1 )
    
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[2], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    
    #phi
    #sex diff
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*0)
    #post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*0)
    dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*mean(d$adult) + post$delta_m[,2]*1)
    #post_plot <- logistic(post$mu[,2] + post$delta_m[,2]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 + post$delta_m[,2]*mean(d$male))
    #post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*0 )
    dens(post_plot, main=expression(paste(phi)) , xlim=c(0,0.5) , xlab="weight given to recent experience" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[3], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
    post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 + post$delta_m[,2]*mean(d$male))
   # post_plot <- logistic(post$mu[,2] + post$delta_a[,2]*1 )
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[4], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    ####gamma####
    #sex diff
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*0)
    #post_plot <- logistic(post$mu[,3] + post$delta_m[,3]*0)
    dens(post_plot , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*mean(d$adult) + post$delta_m[,3]*1)
   # post_plot <- logistic(post$mu[,3] + post$delta_m[,3]*1)
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 + post$delta_m[,3]*mean(d$male) )
   # post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*0 )
    dens(post_plot , main=expression(paste(phi)) , xlim=c(0,1) , xlab="weight given to social information" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[5], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
    post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 + post$delta_m[,3]*mean(d$male) )
    #post_plot <- logistic(post$mu[,3] + post$delta_a[,3]*1 )
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[6], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    ##kappa of some kind
    
    kpar_names[i]
    #sex diff
    post_plot <- post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*0
    #post_plot <- post$mu[,4] + post$delta_m[,4]*0
    dens(post_plot , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , col="white")##phi
    abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
    post_plot <- post$mu[,4] + post$delta_a[,4]*mean(d$adult) + post$delta_m[,4]*1
    #post_plot <- post$mu[,4] + post$delta_m[,4]*1
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
    abline(v=median(post_plot) , col = col.alpha(col.pal[8], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot <- post$mu[,4] + post$delta_a[,4]*0 + post$delta_m[,4]*mean(d$male)
    #post_plot <- post$mu[,4] + post$delta_a[,4]*0 
    dens(post_plot , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , col="white")
    abline(v=median(post_plot) ,col = col.alpha(col.pal[7], 0.5) ) 
    shade( density(post_plot) , lim= as.vector(HPDI(post_plot, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
    post_plot <- post$mu[,4] + post$delta_a[,4]*1 + post$delta_m[,4]*mean(d$male)
    #post_plot <- post$mu[,4] + post$delta_a[,4]*1
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

