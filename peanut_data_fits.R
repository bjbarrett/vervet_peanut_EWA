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

##############################
#######plot main effects######
#############################
post <- extract(fit_global)

plambda <- list(
  lambda_female =  exp(post$S[,1,1] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_male = exp(post$S[,1,2] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_juv = exp(post$A[,1,1] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_adult = exp(post$A[,1,2] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
)
plot(precis(plambda , depth=2) )
precis(plambda)

plogits <- list(
  phi_female = logistic(post$S[,2,1] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )), 
  phi_male = logistic(post$S[,2,2] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )), 
  phi_juv =  logistic(post$A[,2,1] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  phi_adult = logistic(post$A[,2,2] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  gamma_female = logistic(post$S[,3,1] + apply(post$A[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean )),
  gamma_male = logistic(post$S[,3,2] + apply(post$A[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean )),
  gamma_juv = logistic(post$A[,3,1] + apply(post$S[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean )),
  gamma_adult = logistic(post$A[,3,2] + apply(post$S[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean ))
)
plot(precis(plogits , depth=2) )
precis(plogits)


pfc <- list(
  fc_female = exp(post$S[,4,1] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )),
  fc_male = exp(post$S[,4,2] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )),
  fc_juv = exp(post$A[,4,1] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )),
  fc_adult = exp(post$A[,4,2] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ))
  # fc_Kubu= exp( post$G[,1,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean )),
  # fc_Noha= exp( post$G[,2,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean ))
)

plot(precis(pfc , depth=2) )
precis(pfc)

pbeta <- list(
  beta_fem_female = post$S[,5,1] + apply(post$A[,5,] + post$G[,,5] , 1 , mean) + apply( post$I[,,5], 1 ,mean ) ,
  beta_fem_male = post$S[,5,2] + apply(post$A[,5,] + post$G[,,5] , 1 , mean) + apply( post$I[,,5], 1 ,mean ) ,
  beta_fem_juv = post$A[,5,1] + apply(post$S[,5,] + post$G[,,5] , 1 , mean) + apply( post$I[,,5], 1 ,mean ) ,
  beta_fem_adult = post$A[,5,2] + apply(post$S[,5,] + post$G[,,5] , 1 , mean) + apply( post$I[,,5], 1 ,mean ) ,
  beta_kin_female = post$S[,6,1] + apply(post$A[,6,] + post$G[,,6] , 1 , mean) + apply( post$I[,,6], 1 ,mean ) ,
  beta_kin_male = post$S[,6,2] + apply(post$A[,6,] + post$G[,,6] , 1 , mean) + apply( post$I[,,6], 1 ,mean ) ,
  beta_kin_juv = post$A[,6,1] + apply(post$S[,6,] + post$G[,,6] , 1 , mean) + apply( post$I[,,6], 1 ,mean ) ,
  beta_kin_adult = post$A[,6,2] + apply(post$S[,6,] + post$G[,,6] , 1 , mean) + apply( post$I[,,6], 1 ,mean ) ,
  beta_pay_female = post$S[,7,1] + apply(post$A[,7,] + post$G[,,7] , 1 , mean) + apply( post$I[,,7], 1 ,mean ) ,
  beta_pay_male = post$S[,7,2] + apply(post$A[,7,] + post$G[,,7] , 1 , mean) + apply( post$I[,,7], 1 ,mean ) ,
  beta_pay_juv = post$A[,7,1] + apply(post$S[,7,] + post$G[,,7] , 1 , mean) + apply( post$I[,,7], 1 ,mean ) ,
  beta_pay_adult = post$A[,7,2] + apply(post$S[,7,] + post$G[,,7] , 1 , mean) + apply( post$I[,,7], 1 ,mean ) ,
  beta_rank_female = post$S[,8,1] + apply(post$A[,8,] + post$G[,,8] , 1 , mean) + apply( post$I[,,8], 1 ,mean ) ,
  beta_rank_male = post$S[,8,2] + apply(post$A[,8,] + post$G[,,8] , 1 , mean) + apply( post$I[,,8], 1 ,mean ) ,
  beta_rank_juv = post$A[,8,1] + apply(post$S[,8,] + post$G[,,8] , 1 , mean) + apply( post$I[,,8], 1 ,mean ) ,
  beta_rank_adult = post$A[,8,2] + apply(post$S[,8,] + post$G[,,8] , 1 , mean) + apply( post$I[,,8], 1 ,mean ) , 
  beta_sex_female = post$S[,9,1] + apply(post$A[,9,] + post$G[,,9] , 1 , mean) + apply( post$I[,,9], 1 ,mean ) ,
  beta_sex_male = post$S[,9,2] + apply(post$A[,9,] + post$G[,,9] , 1 , mean) + apply( post$I[,,9], 1 ,mean ) ,
  beta_sex_juv = post$A[,9,1] + apply(post$S[,9,] + post$G[,,9] , 1 , mean) + apply( post$I[,,9], 1 ,mean ) ,
  beta_sex_adult = post$A[,9,2] + apply(post$S[,9,] + post$G[,,9] , 1 , mean) + apply( post$I[,,9], 1 ,mean ) 
)

plot(precis(pbeta , depth=2) )
precis(pbeta)

#################################################
##########plots varying effects##################
#################################################
plot(precis(fit_global , pars='sigma' , depth=3))
l4p <- unique(subset( d , select=c("sex_index" , "age_index" , "ID_actor_index" , "group_index" , "ID_actor")))

lambda_list <- phi_list <- gamma_list <- fc_list <- beta_fem_list <- beta_kin_list <- beta_pay_list <- beta_rank_list <- beta_sex_list <-  as.list(as.data.frame(matrix(0, nrow=length(post$I[,1,1]) , ncol=nrow(l4p) + 2  )))


for (i in 1:2){
  lambda_list[[i]] = exp( post$G[,i,1] + apply( post$A[,1,] + post$S[,1,] , 1 ,mean) + apply( post$I[,,1], 1 ,mean ))
  phi_list[[i]] = logistic( post$G[,i,2] + apply( post$A[,2,] + post$S[,2,] , 1 ,mean) + apply( post$I[,,2], 1 ,mean ))
  gamma_list[[i]] = logistic( post$G[,i,3] + apply( post$A[,3,] + post$S[,3,] , 1 ,mean) + apply( post$I[,,3], 1 ,mean ))
  fc_list[[i]] = exp( post$G[,i,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean ))
  beta_fem_list[[i]] = post$G[,i,5] + apply( post$A[,5,] + post$S[,5,] , 1 ,mean) + apply( post$I[,,5], 1 ,mean )
  beta_kin_list[[i]] = post$G[,i,6] + apply( post$A[,6,] + post$S[,6,] , 1 ,mean) + apply( post$I[,,6], 1 ,mean )
  beta_pay_list[[i]] = post$G[,i,7] + apply( post$A[,7,] + post$S[,7,] , 1 ,mean) + apply( post$I[,,7], 1 ,mean )
  beta_rank_list[[i]] = post$G[,i,8] + apply( post$A[,8,] + post$S[,8,] , 1 ,mean) + apply( post$I[,,8], 1 ,mean )
  beta_sex_list[[i]] = post$G[,i,9] + apply( post$A[,9,] + post$S[,9,] , 1 ,mean) + apply( post$I[,,9], 1 ,mean )
}

for (i in 1:34){
  lambda_list[[i+2]] = exp( post$I[,l4p$ID_actor_index[i],1] + post$G[,l4p$group_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] )
  phi_list[[i+2]] = logistic( post$I[,l4p$ID_actor_index[i],2] + post$G[,l4p$group_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) 
  gamma_list[[i+2]] = logistic( post$I[,l4p$ID_actor_index[i],3] + post$G[,l4p$group_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) 
  fc_list[[i+2]] = exp( post$I[,l4p$ID_actor_index[i],4] + post$G[,l4p$group_index[i],4] + post$A[,4,l4p$age_index[i]]  + post$S[,4,l4p$sex_index[i]] )
  beta_fem_list[[i+2]] = post$I[,l4p$ID_actor_index[i],5] + post$G[,l4p$group_index[i],5] + post$A[,5,l4p$age_index[i]] + post$S[,5,l4p$sex_index[i]] 
  beta_kin_list[[i+2]] = post$I[,l4p$ID_actor_index[i],6] + post$G[,l4p$group_index[i],6] + post$A[,6,l4p$age_index[i]] + post$S[,6,l4p$sex_index[i]] 
  beta_pay_list[[i+2]] = post$I[,l4p$ID_actor_index[i],7] + post$G[,l4p$group_index[i],7] + post$A[,7,l4p$age_index[i]] + post$S[,7,l4p$sex_index[i]] 
  beta_rank_list[[i+2]] = post$I[,l4p$ID_actor_index[i],8] + post$G[,l4p$group_index[i],8] + post$A[,8,l4p$age_index[i]] + post$S[,8,l4p$sex_index[i]] 
  beta_sex_list[[i+2]] = post$I[,l4p$ID_actor_index[i],9] + post$G[,l4p$group_index[i],9] + post$A[,9,l4p$age_index[i]] + post$S[,9,l4p$sex_index[i]] 
}

labels <- paste( "lambda" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(lambda_list) , labels=labels , xlim=c(0,60))

labels <- paste( "phi" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(phi_list) , labels=labels )

labels <- paste( "gamma" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(gamma_list), labels=labels)

labels <- paste( "fc" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(fc_list) , labels=labels, xlim=c(0,20))

labels <- paste( "beta_fem" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(beta_fem_list) ,labels=labels)

labels <- paste( "beta_kin" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(beta_kin_list) ,labels=labels)

labels <- paste( "beta_pay" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(beta_pay_list) ,labels=labels)

labels <- paste( "beta_rank" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(beta_rank_list) ,labels=labels)

labels <- paste( "beta_sex" , c( levels(d$group) , 1:34) , sep="_"  )
plot(precis(beta_sex_list) ,labels=labels)

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

