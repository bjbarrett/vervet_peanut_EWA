library(rstan)
library(rethinking)
setwd("~/Dropbox/Vervets/vervet_peanut_EWA")

d <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Peanut_Vervet_10min.csv")
#create yield columns
d$y1 <- ifelse(d$succeed==1 & d$technique_index==1 , 1, 0)
d$y2 <- ifelse(d$succeed==1 & d$technique_index==2 , 1, 0)
d$y3 <- ifelse(d$succeed==1 & d$technique_index==3 , 1, 0)
#sort by actor index, then forg_bout
d <-d[with(d, order(ID_actor_index, forg_bout)),]

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

parlistI=c( "lambda","phi_i", "dev" , "log_lik" , "alpha" , "a_indiv" , "sigma_indiv")

fit_i = stan(file = 'ewa_individual.stan', data = datalist_i ,iter = 4000, warmup=2000, chains=2,  pars=parlistI , control=list(adapt_delta=0.95) , cores=2)

precis(fit_i , depth=2 , pars='a_indiv')
traceplot(fit_i , pars='alpha')

##frequency dependent learning
datalist_s <- list(
  N = nrow(d),                                                                        #length of dataset
  J = length( unique(d$ID_actor_index) ),                                             #number of individuals
  K = max(d$technique_index),                                                          #number of processing techniques
  tech = d$technique_index,                                                            #technique index
  y = cbind( d$y1 , d$y2 , d$y3 ),                                                    #individual payoff at timestep (1 if succeed, 0 is fail)
  s = cbind(d$freq1 , d$freq2 , d$freq3 ),                                                     #observed counts of all K techniques to individual J (frequency-dependence)
  q = cbind(d$pay1 , d$pay2 , d$pay3 ),                                                     #observed counts of all K techniques to individual J (frequency-dependence)
  bout = d$forg_bout,#bout is forg index here                                         #processing bout unique to individual J
  id = d$ID_actor_index ,                                           #individual ID
  N_effects=3                                                                        #number of parameters to estimates
)

parlistF=c("a_id" , "mu", "lambda", "Sigma" , "Rho", "dev" , "log_lik")

###frequency dependent
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99) , pars=parlistF, cores=2)
precis(fit_s , depth=3 , pars=c('mu', 'a_id'))
traceplot(fit_s , pars='mu')
###payoff-bias
datalist_s$q <- datalist_s$q / max(datalist_s$q)
parlistP=c("a_id" , "mu", "lambda" , "Sigma" , "Rho", "dev" , "log_lik" )
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 2000, warmup=1000, chains=2, control=list(adapt_delta=0.99) , pars=parlistP, cores=2)

fit_i = stan(file = 'ewa_individual.stan', data = datalist_i ,iter = 4000, warmup=2000, chains=2,  pars=parlistI , control=list(adapt_delta=0.99) , cores=2)
fit_s = stan( file = 'ewa_freq.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=2, control=list(adapt_delta=0.99) , pars=parlistF, cores=2)
fit_pay = stan( file = 'ewa_cue.stan', data = datalist_s ,iter = 4000, warmup=2000, chains=2, control=list(adapt_delta=0.99) , pars=parlistP, cores=2)

##################################################################
#######Code to fit Model which we will comment out################
##################################################################

# #parameter list to save in posterior extractions
# parlistglobalage=c("lambda" ,"a_id" , "mu" ,"Bpay","Bkin","Bpres","Bcoho","Byob", "fconf", "dev" , "log_lik" , "b_age" , "Rho" , "sigma" )

# #global model assuming file is in working directory
# fit_global_age <- stan( file = 'PN_social_global_age.stan', data = d1 , 
#     iter = 2000, warmup=1000 , chains=3, cores=3,
#     control=list( adapt_delta=0.98 ) ,pars=parlistglobalage )

