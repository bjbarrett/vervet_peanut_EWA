library(rstan)
library(rethinking)

##if running locally, choose which model files
setwd("~/Dropbox/Vervets/vervet_peanut_EWA/log_lik_no_slopes")
d <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Peanut_Vervet_10min.csv")
options(mc.cores = parallel::detectCores())

##if running on server
options(mc.cores = parallel::detectCores())
d <- read.csv("~/Peanut_Vervet_10min.csv")

###run if dropping Kubu for speed and diagnosis stuff
# d <- droplevels(d[d$group=="Noha",])
# d$ID_actor_index <- as.integer(as.factor(d$ID_actor)) #index across all foraging individuals
# d <-d[with(d, order(ID_actor_index, forg_bout)),] #sort by actor index, then forg_bout,


################################################
###########Non-categorical models###############
################################################

###################
####fixed lambda###
###################

setwd("~/Dropbox/Vervets/vervet_peanut_EWA/log_lik_no_slopes")
parlistI=c("a_id" , "mu", "lambda", "sigma" , "log_lik")
parlistS=c("a_id" , "mu", "lambda", "sigma" ,"Rho" , "log_lik")

datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$ID_actor_index) ),       #number of individuals
  K = max(d$technique_index),                   #number of processing techniques
  tech = d$technique_index,                     #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),              #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,                          #processing bout unique to individual J
  id = d$ID_actor_index ,                      #individual ID
  N_effects=1                                  #number of parameters to estimates
)

##frequency dependent learning
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
  N_effects=3                                                                        #number of parameters to estimates
)
datalist_s$q <- datalist_s$q / max(datalist_s$q)
datalist_s$s <- datalist_s$s/ max(datalist_s$s)

##model fits
fit_i = stan( file = 'ewa_individual.stan', data = datalist_i ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9) , pars=parlistI, refresh=50)
##freq-bias
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.999 ,  max_treedepth = 15) , pars=parlistS, refresh=50)
##payoff bias
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9 ) , pars=parlistS, refresh=50)
#female bias
datalist_s$q = cbind(d$fem1 , d$fem2 , d$fem3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_fem = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9 ) , pars=parlistS, refresh=50)
#same-sex bias
datalist_s$q =cbind(d$sex1 , d$sex2 , d$sex3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_sex = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9 ) , pars=parlistS, refresh=50)
#rank_bias
datalist_s$q =cbind(d$rank1 , d$rank2 , d$rank3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_rank = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9 ) , pars=parlistS, refresh=50)
#kin-bias
datalist_s$q =cbind(d$kin1 , d$kin2 , d$kin3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_kin = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.9 ) , pars=parlistS, refresh=50)

precis(fit_i , depth=2 , pars=c('mu' , 'a_id' , "lambda" , "sigma"))
traceplot(fit_i , pars=c('mu','sigma'))
#traceplot(fit_i , pars=c('a_id'))
#pairs(fit_i , pars=c('mu','sigma'))

precis(fit_s , depth=2 , pars=c('mu' , 'a_id' , "lambda" , "sigma", "Rho"))
traceplot(fit_s , pars=c('mu','sigma'))


##################################################################
#######vary lambda################
##################################################################

setwd("~/Dropbox/Vervets/vervet_peanut_EWA/log_lik_no_slopes_varyL")
parlistS=c("a_id" , "mu", "sigma" ,"Rho" , "log_lik")

datalist_i <- list(
  N = nrow(d),                                  #length of dataset
  J = length( unique(d$ID_actor_index) ),       #number of individuals
  K = max(d$technique_index),                   #number of processing techniques
  tech = d$technique_index,                     #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),              #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,                          #processing bout unique to individual J
  id = d$ID_actor_index ,                      #individual ID
  N_effects=2                                  #number of parameters to estimates
)

##frequency dependent learning
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
  N_effects=9                                                                        #number of parameters to estimates
)

datalist_g$s <- datalist_g$s/ max(datalist_g$s)
datalist_g$f <- datalist_g$f / max(datalist_g$f)
datalist_g$k <- datalist_g$k/ max(datalist_g$k)
datalist_g$p <- datalist_g$p/ max(datalist_g$p)
datalist_g$r <- datalist_g$r/ max(datalist_g$r)
datalist_g$x <- datalist_g$x/ max(datalist_g$x)

##model fits
fit_i = stan( file = 'ewa_individual.stan', data = datalist_i ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.95) , pars=parlistS, refresh=50)
##freq-bias
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.999 ,  max_treedepth = 15) , pars=parlistS, refresh=50)
##payoff bias
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.95 ) , pars=parlistS, refresh=50)
#female bias
datalist_s$q = cbind(d$fem1 , d$fem2 , d$fem3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_fem = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=50)
#same-sex bias
datalist_s$q =cbind(d$sex1 , d$sex2 , d$sex3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_sex = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, init=0, refresh=50)
#rank_bias
datalist_s$q =cbind(d$rank1 , d$rank2 , d$rank3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_rank = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=50)
#kin-bias
datalist_s$q =cbind(d$kin1 , d$kin2 , d$kin3 )
datalist_s$q <- datalist_s$q / max(datalist_s$q)
fit_kin = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.99 ) , pars=parlistS, refresh=50)
#global model
fit_global = stan( file = 'ewa_global.stan', data = datalist_g ,iter = 6000, warmup=2000, chains=5, cores=5, control=list(adapt_delta=0.999 ,  max_treedepth = 15) , pars=parlistS, refresh=50)

precis(fit_i , depth=2 , pars=c('mu' , 'a_id' , "lambda" , "sigma"))
traceplot(fit_i , pars=c('mu','sigma'))
#traceplot(fit_i , pars=c('a_id'))
#pairs(fit_i , pars=c('mu','sigma'))

precis(fit_s , depth=3 , pars=c('mu' , 'a_id' , "sigma", "Rho"))
traceplot(fit_s , pars=c('mu','sigma'))

precis(fit_pay , depth=3 , pars=c('mu' , 'a_id' , "sigma", "Rho"))
traceplot(fit_pay , pars=c('mu','sigma'))

precis(fit_fem , depth=3 , pars=c('mu' , 'a_id', "sigma", "Rho"))
traceplot(fit_fem , pars=c('mu','sigma'))

precis(fit_sex , depth=3 , pars=c('mu' , 'a_id', "sigma", "Rho"))
traceplot(fit_sex , pars=c('mu','sigma'))

precis(fit_rank , depth=3 , pars=c('mu' , 'a_id', "sigma", "Rho"))
traceplot(fit_rank , pars=c('mu','sigma'))

precis(fit_kin , depth=3 , pars=c('mu' , 'a_id', "sigma", "Rho"))
traceplot(fit_kin , pars=c('mu','sigma'))

compare(fit_i, fit_s, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem)

c <- rnorm(20000,mean=0,sd=1)
dens(c)
dens(exp(c))
median(exp(c))

