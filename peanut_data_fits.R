library(rstan)
library(rethinking)

##if running locally
setwd("~/Dropbox/Vervets/vervet_peanut_EWA/log_lik_no_slopes")
d <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Peanut_Vervet_10min.csv")
options(mc.cores = parallel::detectCores())
##if running on server
# options(mc.cores = parallel::detectCores())
# d <- read.csv("~/Peanut_Vervet_20min.csv")

#create yield columns
d$y1 <- ifelse(d$succeed==1 & d$technique_index==1 , 1, 0)
d$y2 <- ifelse(d$succeed==1 & d$technique_index==2 , 1, 0)
d$y3 <- ifelse(d$succeed==1 & d$technique_index==3 , 1, 0)

###run if dropping Kubu
d <- droplevels(d[d$group=="Noha",])
d$ID_actor_index <- as.integer(as.factor(d$ID_actor)) #index across all foraging individuals

#sort by actor index, then forg_bout
d <-d[with(d, order(ID_actor_index, forg_bout)),]
###########Non-categorical models###############

##use same data list that was used when fitting stan model
datalist_i <- list(
  N = nrow(d),                                                                        #length of dataset
  J = length( unique(d$ID_actor_index) ),                                             #number of individuals
  K = max(d$technique_index),                                                          #number of processing techniques
  tech = d$technique_index,                                                            #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),                                                    #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,#bout is forg index here                                         #processing bout unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=1                                                                        #number of parameters to estimates
)

###Individual Learning
parlistI=c( "lambda", "mu" , "a_id" , "sigma", "log_lik") #add_log_lik later
fit_i = stan(file = 'ewa_individual.stan', data = datalist_i ,iter = 1600, warmup=800, chains=2,  pars=parlistI , control=list(adapt_delta=0.99) , cores=2)

precis(fit_i , depth=2 , pars=c('mu' , 'a_id' , "lambda" , "sigma"))

#diagnosis plots
traceplot(fit_i , pars=c('mu','sigma'))
traceplot(fit_i , pars=c('a_id'))
pairs(fit_i , pars=c('mu','sigma'))

##frequency dependent learning
datalist_s2 <- list(
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

datalist_s2 <- list(
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
datalist_s2$q <- datalist_s2$q / max(datalist_s2$q)
datalist_s2$s <- datalist_s2$s/ max(datalist_s2$s)

parlistF=c("a_id" , "mu", "lambda", "sigma" ,"L_Rho" , "log_lik")
parlistF2=c("a_id" , "mu", "sigma" ,"L_Rho" , "log_lik")

###frequency dependent
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 1600, warmup=800, chains=2, control=list(adapt_delta=0.99 ,  max_treedepth = 15) , pars=parlistF, cores=2 , refresh=10)
fit_s2 = stan( file = 'ewa_freq_varyL.stan', data = datalist_s2 ,iter = 1600, warmup=800, chains=2, control=list(adapt_delta=0.99 ,  max_treedepth = 15) , pars=parlistF2, cores=2 , refresh=10)
precis(fit_s2 , depth=3 , pars=c('mu', 'a_id'))
traceplot(fit_s2 , pars='mu')
pairs(fit_s , pars=c('mu','sigma'))
traceplot( fit_i , pars=c('mu','Sigma') )

###payoff-bias
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99) , pars=parlistF, cores=2)
traceplot(fit_pay , pars='mu')
traceplot(fit_s , pars='mu')
precis(fit_pay , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho'))

##all models
fit_i = stan(file = 'ewa_individual.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99) , cores=2)
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99, max_treedepth = 15) , pars=parlistF, cores=2 )
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99) , pars=parlistF, cores=2)


save(d,fit_pay,fit_s, fit_i, file="20daysPNUT.rdata")

##################################################################
#######Code to fit Model which we will comment out################
##################################################################
##use same data list that was used when fitting stan model
datalist_i <- list(
  N = nrow(d),                                                                        #length of dataset
  J = length( unique(d$ID_actor_index) ),                                             #number of individuals
  K = max(d$technique_index),                                                          #number of processing techniques
  tech = d$technique_index,                                                            #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),                                                    #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,#bout is forg index here                                         #processing bout unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=1                                                                        #number of parameters to estimates
)
###Individual Learning
parlistI=c( "lambda", "mu" , "a_id" , "sigma") #add_log_lik later

##social learning
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

#female bias
datalist_fem <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$ID_actor_index) ),  #number of individuals
  K = max(d$technique_index),         #number of processing techniques
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$fem1 , d$fem2 , d$fem3 ),
  bout = d$forg_bout,#bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=3                                                                        #number of parameters to estimates
)

datalist_fem$q <- datalist_s_fem$q / max(datalist_s_fem$q)

#same sex bias
datalist_sex <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$ID_actor_index) ),  #number of individuals
  K = max(d$technique_index),         #number of processing techniques
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$sex1 , d$sex2 , d$sex3 ),
  bout = d$forg_bout,#bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=3                                                                        #number of parameters to estimates
)

datalist_sex$q <- datalist_sex$q / max(datalist_sex$q)

#rank bias
datalist_rank <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$ID_actor_index) ),  #number of individuals
  K = max(d$technique_index),         #number of processing techniques
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$rank1 , d$rank2 , d$rank3 ),
  bout = d$forg_bout,#bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=3                                                                        #number of parameters to estimates
)

datalist_rank$q <- datalist_rank$q / max(datalist_rank$q)

#kin bias
datalist_kin <- list(
  N = nrow(d),                            #length of dataset
  J = length( unique(d$ID_actor_index) ),  #number of individuals
  K = max(d$technique_index),         #number of processing techniques
  tech = d$technique_index,           #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 ), #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$kin1 , d$kin2 , d$kin3 ),
  bout = d$forg_bout,#bout is forg index  unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=3                                                                        #number of parameters to estimates
)

datalist_kin$q <- datalist_kin$q / max(datalist_kin$q)

parlistF=c("a_id" , "mu", "lambda", "sigma" ,"L_Rho" )
initlist <- function () (list( mu=c( -3 , -1.2, 0) ))
fit_i = stan(file = 'ewa_individual2.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=4,  pars=parlistI , control=list(adapt_delta=0.99) , cores=4)
fit_pay = stan( file = 'ewa_cue2.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=4, control=list(adapt_delta=0.99) , cores=4 , pars=parlistF)

fit_s2 = stan( file = 'ewa_freq2.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.999, max_treedepth=15) , pars=parlistF, cores=2)
fit_fem = stan( file = 'ewa_cue2.stan', data = datalist_fem ,iter = 2000, warmup=1000, chains=4, control=list(adapt_delta=0.95) , cores=4 , pars=parlistF)
fit_sex = stan( file = 'ewa_cue2.stan', data = datalist_sex ,iter = 2000, warmup=1000, chains=4, control=list(adapt_delta=0.95) , cores=4 , pars=parlistF)
fit_rank = stan( file = 'ewa_cue2.stan', data = datalist_rank ,iter = 2000, warmup=1000, chains=4, control=list(adapt_delta=0.95) , cores=4 , pars=parlistF)
fit_kin = stan( file = 'ewa_cue2.stan', data = datalist_kin ,iter = 2000, warmup=1000, chains=4, control=list(adapt_delta=0.95) , cores=4 , pars=parlistF)

traceplot(fit_s , pars='a_id')
traceplot(fit_s , pars='mu')
pairs(fit_pay , pars=c('mu','sigma' , 'L_Rho'),  las = 1)
pairs(fit_pay , pars=c('mu','sigma'),  las = 1)


precis(fit_i , depth=3 , pars=c('mu', 'a_id', 'sigma','lambda'))
precis(fit_pay , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho','lambda'))
precis(fit_s , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho','lambda'))
precis(fit_fem , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho','lambda'))
precis(fit_sex , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho','lambda'))
precis(fit_rank , depth=3 , pars=c('mu', 'a_id', 'sigma','L_Rho','lambda'))

c <- rnorm(20000,mean=0,sd=1)
dens(c)
dens(exp(c))
median(exp(c))

###################vary lambda#######################
datalist_i <- list(
  N = nrow(d),                                                                        #length of dataset
  J = length( unique(d$ID_actor_index) ),                                             #number of individuals
  K = max(d$technique_index),                                                          #number of processing techniques
  tech = d$technique_index,                                                            #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),                                                    #individual payoff at timestep (1 if succeed, 0 is fail)
  bout = d$forg_bout,#bout is forg index here                                         #processing bout unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=2                                                                        #number of parameters to estimates
)
parlistI=c( "logit_phi" , "log_lambda", "a_id" , "sigma" , "L_Rho") #add_log_lik later
fit_i = stan(file = 'ewa_individual_reparam.stan', data = datalist_i ,iter = 2000, warmup=1000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99) , cores=2)
precis(fit_i , depth=3)


##social learning
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
parlistF=c(  "log_lambda", "logit_phi" , "logit_gamma" , "log_f" , "a_id" , "sigma" , "L_Rho") #add_log_lik later
fit_s = stan( file = 'ewa_freq_reparam.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99, max_treedepth=15) , pars=parlistF, cores=2)


test <- rnorm(20000, mean = 0, sd = 0.4)
dens(exp(test))
apply(mean(d$s))

