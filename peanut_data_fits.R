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

load("/Users/BJB/Downloads/vervet_peanut_ewa_20min_13May2020.rdata")##if working with existing workspace

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

###########################################model fits###########################################
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

WAICtab <- compare(fit_i, fit_freq, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global)
library(rethinking)
save(d,fit_i, fit_freq, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global,WAICtab, file="vervet_peanut_ewa_20min_25April2020.rdata")


########inspect parameters and fits########
precis(fit_global, depth=3 , pars=c('A' , 'S'))
precis(fit_global, depth=3 , pars=c('I'))
precis(fit_global, depth=3 , pars=c('G'))
precis(fit_global, depth=3 , pars=c('sigma_i' , 'sigma_g') )
precis(fit_global, depth=3 , pars=c('Rho_i' ) )
precis(fit_global, depth=3 , pars=c('Rho_g' ) )
########info criteria#########
WAICtab <- compare(fit_i, fit_freq, fit_pay, fit_rank, fit_kin, fit_sex , fit_fem, fit_global)



# ##########individual-predictions#############
# 
# l4p <- unique(subset( d , select=c("sex_index" , "age_index" , "ID_actor_index" , "group_index")))
# 
# lambda_list <- phi_list <- gamma_list <- fc_list <- kappa_list <-  lambda_list_CI <- phi_list_CI <- gamma_list_CI <- fc_list_CI <- kappa_list_CI  <- rep(0,nrow(l4p))
# for (i in 1:nrow(l4p)){
#   lambda_list[i] = median(exp( post$I[,l4p$ID_actor_index[i],1] + post$G[,l4p$group_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] )) 
#   phi_list[i] = median(logistic( post$I[,l4p$ID_actor_index[i],2] + post$G[,l4p$group_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) )
#   gamma_list[i] = median(logistic( post$I[,l4p$ID_actor_index[i],3] + post$G[,l4p$group_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) )
#   fc_list[i] = median(exp( post$I[,l4p$ID_actor_index[i],4] + post$G[,l4p$group_index[i],4] + post$A[,4,l4p$age_index[i]]  + post$S[,4,l4p$sex_index[i]] )) 
#   kappa_list[i] = median( post$I[,l4p$ID_actor_index[i],4] + post$G[,l4p$group_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] ) 
#   
#   # lambda_list_CI[i] = HPDI(exp( post$I[,l4p$ID_actor_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] ) )
#   # phi_list_CI[i] = HPDI(logistic( post$I[,l4p$ID_actor_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) )
#   # gamma_list_CI[i] = HPDI(logistic( post$I[,l4p$ID_actor_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) )
#   # fc_list_CI[i] = HPDI(exp( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] )) 
#   # kappa_list_CI[i] = mean( post$I[,l4p$ID_actor_index[i],4] + post$A[,4,l4p$age_index[i]] + post$S[,4,l4p$sex_index[i]] ) 
#   
# }
# phi_list
# lambda_list
# gamma_list
# fc_list

##sigmas
dens(post$sigma , main="sigmas" , xlim=c(0,6) , col="white" , ylim=c(0,2.5) )##phi
for(i in 1:9){
  shade( density(post$sigma[,i]) , lim= as.vector(HPDI(post$sigma[,i], prob=0.9999)) , col = col.alpha(col.pal2[i], 0.25))
}
curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )
legend("topright", legend = c("lambda", "phi", "gamma",kpar_names), col = col.pal2, pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )

