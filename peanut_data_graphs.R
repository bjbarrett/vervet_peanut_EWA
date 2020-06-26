library(rethinking)
library(RColorBrewer)
#load("/Users/BJB/Downloads/20min_slopes_VervetPNUTindex.rdata")

load("/Users/BJB/Downloads/vervet_peanut_ewa_20min_13May2020.rdata")##if working with existing workspace
post <- extract(fit_global)

sigmas <- precis(fit_global , pars=c("sigma_i" , "sigma_g" ) , depth=3 , hist=FALSE)
plot(sigmas)

########################################################
###############Dot Plots Main Effects All Groups#######
#######################################################

##############################
#######plot main effects######
#############################

plambda <- list(
  lambda_female =  exp(post$S[,1,1] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_male = exp(post$S[,1,2] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_juv = exp(post$A[,1,1] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean )),
  lambda_adult = exp(post$A[,1,2] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
)
plot(precis(plambda , depth=2) )
precis(plambda)
precis(plambda)

plogits <- list(
  phi_female = logistic(post$S[,2,1] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  phi_male = logistic(post$S[,2,2] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  phi_juv =  logistic(post$A[,2,1] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  phi_adult = logistic(post$A[,2,2] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean )),
  gamma_female = logistic(post$S[,3,1] + apply(post$A[,3,] + post$G[,,3] , 1 , mean)  ),
  gamma_male = logistic(post$S[,3,2] + apply(post$A[,3,]+ post$G[,,3] , 1 , mean)  ),
  gamma_juv = logistic(post$A[,3,1] + apply(post$S[,3,] + post$G[,,3], 1 , mean)   ),
  gamma_adult = logistic(post$A[,3,2] + apply(post$S[,3,] + post$G[,,3], 1 , mean)  )
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

biglist <- list(plambda, plogits , pfc, pbeta )
biglist2tab <- precis(biglist , depth=3 , ci=TRUE , digits=2 , hist=FALSE)
write.csv(biglist2tab , file="globalparams.csv")
sigmalist2tab <- precis(fit_global , pars=c("sigma_i" , "sigma_g") , depth=3 , ci=TRUE , digits=2 , hist=FALSE)

write.csv(sigmalist2tab , file="globalparamssigmas.csv")


##############################
#######plot main effects noha only######
#############################
# 
# plambda <- list(
#   lambda_female =  exp(post$S[,1,1] + apply(post$A[,1,] , 1 , mean)  + post$G[,2,1] ),
#   lambda_male = exp(post$S[,1,2] + apply(post$A[,1,] , 1 , mean)  + post$G[,2,1] ),
#   lambda_juv = exp(post$A[,1,1] + apply(post$S[,1,] , 1 , mean)  + post$G[,2,1] ),
#   lambda_adult = exp(post$A[,1,2] + apply(post$S[,1,] , 1 , mean)  + post$G[,2,1]  )
# )
# plot(precis(plambda , depth=2) )
# precis(plambda , ci=FLASE)
# 
# 
# plogits <- list(
#   phi_female = logistic(post$S[,2,1] + apply(post$A[,2,] , 1 , mean)  + post$G[,2,2] ), 
#   phi_male = logistic(post$S[,2,2] + apply(post$A[,2,] , 1 , mean)  + post$G[,2,2]), 
#   phi_juv =  logistic(post$A[,2,1] + apply(post$S[,2,] , 1 , mean)  + post$G[,2,2]),
#   phi_adult = logistic(post$A[,2,2] + apply(post$S[,2,] , 1 , mean)  + post$G[,2,2]),
#   gamma_female = logistic(post$S[,3,1] + apply(post$A[,3,] , 1 , mean)  + post$G[,2,3]),
#   gamma_male = logistic(post$S[,3,2] + apply(post$A[,3,] , 1 , mean)  + post$G[,2,3]),
#   gamma_juv = logistic(post$A[,3,1] + apply(post$S[,3,] , 1 , mean)  + post$G[,2,3] ),
#   gamma_adult = logistic(post$A[,3,2] + apply(post$S[,3,] , 1 , mean)  + post$G[,2,3] )
# )
# plot(precis(plogits , depth=2) )
# precis(plogits)
# 
# 
# pfc <- list(
#   fc_female = exp(post$S[,4,1] + apply(post$A[,4,] , 1 , mean)  + post$G[,2,4]),
#   fc_male = exp(post$S[,4,2] + apply(post$A[,4,] , 1 , mean)  + post$G[,2,4]),
#   fc_juv = exp(post$A[,4,1] + apply(post$S[,4,] , 1 , mean)  + post$G[,2,4] ),
#   fc_adult = exp(post$A[,4,2] + apply(post$S[,4,] , 1 , mean)  + post$G[,2,4] )
# )
# 
# plot(precis(pfc , depth=2) )
# precis(pfc)
# 
# pbeta <- list(
#   beta_fem_female = post$S[,5,1] + apply(post$A[,5,] , 1 , mean)  + post$G[,2,5]  ,
#   beta_fem_male = post$S[,5,2] +  apply(post$A[,5,] , 1 , mean)  + post$G[,2,5] ,
#   beta_fem_juv = post$A[,5,1]  + apply(post$S[,5,] , 1 , mean) + post$G[,2,5] ,
#   beta_fem_adult = post$A[,5,2] +  apply(post$S[,5,] , 1 , mean) + post$G[,2,5] ,
#   beta_kin_female = post$S[,6,1] + apply(post$A[,6,] , 1 , mean)  + post$G[,2,6] ,
#   beta_kin_male = post$S[,6,2] + apply(post$A[,6,] , 1 , mean)  + post$G[,2,6] ,
#   beta_kin_juv = post$A[,6,1] + apply(post$S[,6,] , 1 , mean)  + post$G[,2,6] ,
#   beta_kin_adult = post$A[,6,2] +  apply(post$S[,6,] , 1 , mean)  + post$G[,2,6] ,
#   beta_pay_female = post$S[,7,1] + apply(post$A[,7,] , 1 , mean)  + post$G[,2,7] ,
#   beta_pay_male = post$S[,7,2] + apply(post$A[,7,] , 1 , mean)  + post$G[,2,7] ,
#   beta_pay_juv = post$A[,7,1] + apply(post$S[,7,] , 1 , mean)  + post$G[,2,7],
#   beta_pay_adult = post$A[,7,2] + apply(post$S[,7,] , 1 , mean)  + post$G[,2,7] ,
#   beta_rank_female = post$S[,8,1] + apply(post$A[,8,] , 1 , mean)  + post$G[,2,8] ,
#   beta_rank_male = post$S[,8,2] + apply(post$A[,8,] , 1 , mean)  + post$G[,2,8] ,
#   beta_rank_juv = post$A[,8,1] + apply(post$S[,8,] , 1 , mean)  + post$G[,2,8],
#   beta_rank_adult = post$A[,8,2] + apply(post$S[,8,] , 1 , mean)  + post$G[,2,8] , 
#   beta_sex_female = post$S[,9,1] + apply(post$A[,9,] , 1 , mean)  + post$G[,2,9] ,
#   beta_sex_male = post$S[,9,2] + apply(post$A[,9,] , 1 , mean)  + post$G[,2,9] ,
#   beta_sex_juv = post$A[,9,1] + apply(post$S[,9,] , 1 , mean)  + post$G[,2,9] ,
#   beta_sex_adult = post$A[,9,2] + apply(post$S[,9,] , 1 , mean)  + post$G[,2,9] 
# )
# 
# plot(precis(pbeta , depth=2) )
# precis(pbeta)

#################################################
##########dotplots varying effects##################
#################################################
plot(precis(fit_global , pars='sigma_i' , depth=3))

l4p <- unique(subset( d , select=c("sex_index" , "age_index" , "ID_actor_index" , "group_index" , "ID_actor")))

lambda_list <- phi_list <- gamma_list <- fc_list <- beta_fem_list <- beta_kin_list <- beta_pay_list <- beta_rank_list <- beta_sex_list <-  as.list(as.data.frame(matrix(0, nrow=length(post$I[,1,1]) , ncol=nrow(l4p) + 6  )))
str(lambda_list)
for(i in 1:4){ lambda_list[[i]] <- plambda[[i]]}
for(i in 1:4){ phi_list[[i]] <- plogits[[i]] }
for(i in 1:4){ gamma_list[[i]] <- plogits[[i+4]] }
for(i in 1:4){ fc_list[[i]] <- pfc[[i]] }
for(i in 1:4){ beta_fem_list[[i]] <- pbeta[[i]] }
for(i in 1:4){ beta_kin_list[[i]] <- pbeta[[4+i]] }
for(i in 1:4){ beta_pay_list[[i]] <- pbeta[[8+i]] }
for(i in 1:4){ beta_rank_list[[i]] <- pbeta[[12+i]] }
for(i in 1:4){ beta_sex_list[[i]] <- pbeta[[16+i]] }


for (i in 1:2){
  lambda_list[[i+4]] = exp( post$G[,i,1] + apply( post$A[,1,] + post$S[,1,] , 1 ,mean) + apply( post$I[,,1], 1 ,mean ))
  phi_list[[i+4]] = logistic( post$G[,i,2] + apply( post$A[,2,] + post$S[,2,] , 1 ,mean) + apply( post$I[,,2], 1 ,mean ))
  gamma_list[[i+4]] = logistic( post$G[,i,3] + apply( post$A[,3,] + post$S[,3,] , 1 ,mean) + apply( post$I[,,3], 1 ,mean ))
  fc_list[[i+4]] = exp( post$G[,i,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean ))
  beta_fem_list[[i+4]] = post$G[,i,5] + apply( post$A[,5,] + post$S[,5,] , 1 ,mean) + apply( post$I[,,5], 1 ,mean )
  beta_kin_list[[i+4]] = post$G[,i,6] + apply( post$A[,6,] + post$S[,6,] , 1 ,mean) + apply( post$I[,,6], 1 ,mean )
  beta_pay_list[[i+4]] = post$G[,i,7] + apply( post$A[,7,] + post$S[,7,] , 1 ,mean) + apply( post$I[,,7], 1 ,mean )
  beta_rank_list[[i+4]] = post$G[,i,8] + apply( post$A[,8,] + post$S[,8,] , 1 ,mean) + apply( post$I[,,8], 1 ,mean )
  beta_sex_list[[i+4]] = post$G[,i,9] + apply( post$A[,9,] + post$S[,9,] , 1 ,mean) + apply( post$I[,,9], 1 ,mean )
}

for (i in 1:34){
  lambda_list[[i+6]] = exp( post$I[,l4p$ID_actor_index[i],1] + post$G[,l4p$group_index[i],1] + post$A[,1,l4p$age_index[i]] + post$S[,1,l4p$sex_index[i]] )
  phi_list[[i+6]] = logistic( post$I[,l4p$ID_actor_index[i],2] + post$G[,l4p$group_index[i],2] + post$A[,2,l4p$age_index[i]] + post$S[,2,l4p$sex_index[i]] ) 
  gamma_list[[i+6]] = logistic( post$I[,l4p$ID_actor_index[i],3] + post$G[,l4p$group_index[i],3] + post$A[,3,l4p$age_index[i]] + post$S[,3,l4p$sex_index[i]] ) 
  fc_list[[i+6]] = exp( post$I[,l4p$ID_actor_index[i],4] + post$G[,l4p$group_index[i],4] + post$A[,4,l4p$age_index[i]]  + post$S[,4,l4p$sex_index[i]] )
  beta_fem_list[[i+6]] = post$I[,l4p$ID_actor_index[i],5] + post$G[,l4p$group_index[i],5] + post$A[,5,l4p$age_index[i]] + post$S[,5,l4p$sex_index[i]] 
  beta_kin_list[[i+6]] = post$I[,l4p$ID_actor_index[i],6] + post$G[,l4p$group_index[i],6] + post$A[,6,l4p$age_index[i]] + post$S[,6,l4p$sex_index[i]] 
  beta_pay_list[[i+6]] = post$I[,l4p$ID_actor_index[i],7] + post$G[,l4p$group_index[i],7] + post$A[,7,l4p$age_index[i]] + post$S[,7,l4p$sex_index[i]] 
  beta_rank_list[[i+6]] = post$I[,l4p$ID_actor_index[i],8] + post$G[,l4p$group_index[i],8] + post$A[,8,l4p$age_index[i]] + post$S[,8,l4p$sex_index[i]] 
  beta_sex_list[[i+6]] = post$I[,l4p$ID_actor_index[i],9] + post$G[,l4p$group_index[i],9] + post$A[,9,l4p$age_index[i]] + post$S[,9,l4p$sex_index[i]] 
}

pdf("lambdadotplots.pdf" , width=7 , height=7)
labels <- c( names(plambda) , paste( "lambda" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(lambda_list) , labels=labels , xlim=c(0,60))
str(lambda_list)
pp <- precis(lambda_list)
colz <- c(4,4,4,4,11,11,rep(1, 34))
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("phidotplots.pdf" , width=7 , height=7)
labels <- c( names(plogits[1:4]) , paste( "phi" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(phi_list) , labels=labels )
pp <- precis(phi_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("gammadotplots.pdf" , width=7 , height=7)
labels <- c( names(plogits[5:8]) , paste( "gamma" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(gamma_list), labels=labels)
pp <- precis(gamma_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("fcdotplots.pdf" , width=7 , height=7)
labels <- c( names(pfc) , paste( "fc" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(fc_list) , labels=labels, xlim=c(0,15))
abline(v=1 , col="red" , lty=3)
pp <- precis(fc_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("beta_fem_dotplots.pdf" , width=7 , height=7)
labels <- c( names(pbeta[1:4]) , paste( "beta_fem" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(beta_fem_list) ,labels=labels)
pp <- precis(beta_fem_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("beta_kin_dotplots.pdf" , width=7 , height=7)
labels <- c( names(pbeta[5:8]) , paste( "beta_kin" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(beta_kin_list) ,labels=labels)
pp <- precis(beta_kin_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("beta_pay_dotplots.pdf" , width=7 , height=7)
labels <- c( names(pbeta[9:12]) , paste( "beta_pay" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(beta_pay_list) ,labels=labels)
pp <- precis(beta_pay_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("beta_rank_dotplots.pdf" , width=7 , height=7)
labels <- c( names(pbeta[13:16]) , paste( "beta_rank" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(beta_rank_list) ,labels=labels)
pp <- precis(beta_rank_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()

pdf("beta_sex_dotplots.pdf" , width=7 , height=7)
labels <- c( names(pbeta[17:20]) , paste( "beta_sex" , c( levels(d$group) , levels(d$ID_actor) ) , sep="_"  ) )
plot(precis(beta_sex_list) ,labels=labels)
pp <- precis(beta_sex_list)
points( pp[[1]] , 40:1  , col=colz , pch=19)
dev.off()


###################
####heatplot#######
###################
col.pal=brewer.pal(3,"Accent")

dh <- aggregate(cbind( d$x1 , d$x2 , d$x3 ) , list(ID_actor=d$ID_actor , ID_actor_index=d$ID_actor_index, Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , ID_noha_index=d$ID_noha_index  ) , mean ) #gets neat summary table for plot
dhN <- dh[dh$group=="Noha",]
dhN$date_index2 <- as.integer(as.factor(dhN$date_index))
dhN<- droplevels(dhN)
dhN<-dhN[order(dhN$date_index2),]#order by this index, dates will be ok
dhN$ID_noha_index2 <- as.integer(as.factor(dhN$ID_noha_index))

# #get column with max value
# for (i in 1:nrow(dhN)){
#   dhN$day_tech_max[i] <-which.max(dhN[i,8:10])
# }
# 
# #transparency all 3 techs
# plot( (dhN$date_index2-0.2) , (dhN$ID_noha_index2), col=col.pal[1], pch=21 , bg=col.alpha(col.pal[1], dhN$V1)  , xlab="Date" ,  xlim=c(0.5 , 11.5))
# points(dhN$date_index2 ,dhN$ID_noha_index2, col=col.pal[2], pch=21 , bg=col.alpha(col.pal[2], dhN$V2) )
# points( (dhN$date_index2 + 0.2) ,dhN$ID_noha_index2 , col=col.pal[3], pch=21 , bg=col.alpha(col.pal[3], dhN$V3) )

#cex size all 3 techs, this seems the best

pdf("Noha_heatplot.pdf" , width=7 , height=5)
par( mar=c(3,3,1,1) , mgp=c(0,0.1,0.1))
plot( (dhN$date_index2-0.2) , (dhN$ID_noha_index2), col=col.pal[1]   , pch=19  , cex=1.9*dhN$V1 , ylab="" , xlab="" , xlim=c(0.75 , 11.25),  xaxt='n' , yaxt='n' , ylim=c(1, 26))
points(dhN$date_index2 ,dhN$ID_noha_index2, col=col.pal[2] , pch=19 , cex=1.9*dhN$V2 )
points( (dhN$date_index2 + 0.2) ,dhN$ID_noha_index2 , col=col.pal[3]  , pch=19  , cex=1.9*dhN$V3)
axis(1 , at=seq(1:max(dhN$date_index2)) , labels=as.vector(unique(dhN$Date)) , cex.axis=0.6 , tick=FALSE)
axis(2 , at=seq(1:length(unique(dhN$ID_noha_index2))) , labels=as.vector(sort(unique(dhN$ID_actor))) , cex.axis=0.6 , las=1 , tick=FALSE , gap.axis=.01)
title(ylab="ID", line=2)
title(xlab="Date (dd.mm.yy)", line=2)
legend("topleft", inset=-.01, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=0.8,bty = "n" )

dev.off()
#cex size all 3 techs
# plot( dhN$date_index2 , dhN$ID_noha_index, col=col.pal[dhN$day_tech_max]   , pch=19  , ylab="Individual id" , xlab="date" , xlim=c(0.5 , 11.5) )



####Kubu
dh2 <- aggregate(cbind( d$x1 , d$x2 , d$x3 ) , list(ID_actor=d$ID_actor , ID_actor_index=d$ID_actor_index, Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , ID_kubu_index=d$ID_kubu_index  ) , mean ) #gets neat summary table for plot

dhK <- dh2[dh2$group=="Kubu",]
dhK$date_index2 <- as.integer(as.factor(dhK$date_index))

dhK$ID_kubu_index2 <- as.integer(as.factor(dhK$ID_kubu_index))
dhK<- droplevels(dhK)
dhK<-dhK[order(dhK$date_index2),]#order by this index, dates will be ok


#cex size all 3 techs, this seems the best

plot( (dhK$date_index2-0.2) , (dhK$ID_kubu_index2), col=col.pal[1]   , pch=19  , cex=2*dhK$V1 , ylab="" , xlab="" , xlim=c(0.5 , 7.5),  xaxt='n' , yaxt='n' , ylim=c(1, 10))
points(dhK$date_index2 ,dhK$ID_kubu_index2, col=col.pal[2] , pch=19 , cex=2*dhK$V2 )
points( (dhK$date_index2 + 0.2) ,dhK$ID_kubu_index2 , col=col.pal[3]  , pch=19  , cex=2*dhK$V3)
axis(1 , at=seq(1:max(dhK$date_index2)) , labels=as.vector(unique(dhK$Date)) , cex.axis=0.75 , tick=FALSE)
axis(2 , at=seq(1:length(unique(dhK$ID_kubu_index2))) , labels=as.vector(sort(unique(dhK$ID_actor))) , cex.axis=0.75 , las=1 , tick=FALSE , gap.axis=.01)
title(ylab="ID", line=1.75)
title(xlab="Date", line=1.5)
legend("topleft", inset=-.01, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=0.8,bty = "n" )

######################################################################
######individual level plots from posterior predictions#############
######################################################################
preds<-post$PrPreds
str(post$PrPreds)
x1 <- post$PrPreds[,,1]
x2 <- post$PrPreds[,,2]
x3 <- post$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

ci1 <- apply( x1 , 2 ,HPDI)
ci2 <- apply( x2 , 2 ,HPDI)
ci3 <- apply( x3 , 2 ,HPDI)

d$x1 <- m1
d$x2 <- m2
d$x3 <- m3

col.pal=brewer.pal(3,"Accent")

d$seq <- 1:nrow(d) #for graphing

pdf("individual_peanut_vervet_preds_ci.pdf",width=11,height=8.5)
par( mfrow=c(12, 3) , mar=c(0.1,0.1,1,0.1)+0.1 , oma=c(3,3,0,0)+0.02 )
par(cex = 0.5)
par(tcl = -0.2)
par(mgp = c(2, 0.6, 0))

for(i in 1:max(d$ID_actor_index)){

  plot( x1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1.15) , ylab="" , xlab="", type='l' , yaxt='n' , xaxt='n' )

  
  lines(x1 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1)
  lines(x2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1)
  lines(x3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1)
  axis(2 , at=seq(from=0 , to=1 , by=0.2) , cex.axis=0.5  , tick=TRUE , gap.axis=.01 , labels=FALSE)   
  axis(1 , at=seq(from=min(d$forg_bout[d$ID_actor_index==i] ) , to=max(d$forg_bout[d$ID_actor_index==i] ) , by=10) , cex.axis=0.5  , tick=TRUE , gap.axis=0.3 , labels=FALSE)
  
#   if(is.integer( (i-1)/3 )==TRUE){
#     axis(2 , at=seq(from=0 , to=1 , by=0.2) , cex.axis=0.5  , tick=TRUE  , labels=TRUE)
# }
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  #raw data
  nobsi <- nrow(d[d$ID_actor_index==i,])
  points(rep(1.1, nobsi) ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch= 1 + 18*d$succeed[d$ID_actor_index==i] , col=col.pal[d$technique_index[d$ID_actor_index==i]] , cex=0.25 , lwd=0.2) #empty circels are failure, filled are successes
  abline(h=1)
  
  dstmp <- d$date_index[d$ID_actor_index==i]
  dstmp_lab <- d$Date[d$ID_actor_index==i]
  dstmp_i <- c(1,1+which(diff(dstmp)!=0)) #gives cutpoints of changes (to append dated)
  abline(v=(dstmp_i-0.5) , col="grey" , lw=0.5)
  text(x=(dstmp_i-0.5) , y=rep(1.15 , length(dstmp_i)) , labels=dstmp_lab[dstmp_i] , cex=0.25 , pos=3 , srt=20 , adj=0.5 , xpd=NA)
  
  mtext(unique(d$ID_actor[d$ID_actor_index==i]), side = 3, line = -1.7, adj = 0.005, cex = .36) 
  mtext(unique(d$group[d$ID_actor_index==i]), side = 3, line = -2, adj = 0.005, cex = .18) 
  
  
}
plot.new()
plot.new()
legend("left", inset=0.1, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=1.4,bty = "n" )
legend("right", inset=0.1, c("failure","success") , pch=c(1,19), horiz=TRUE,cex=1.4,bty = "n")

mtext("foraging bout", side = 1, line = 1, cex = 1.2, outer=TRUE) 
mtext("probability of choosing technique", side = 2, line = 1, cex = 1.2 , outer=TRUE) 

dev.off()


#plot of individual subset for main figure

#############comparison of multipl model predictions#############
#assume above code has been run

####payoff model

post.pay  <- extract(fit_pay)
preds.pay<-post.pay$PrPreds
str(post.pay$PrPreds)
x1 <- post.pay$PrPreds[,,1]
x2 <- post.pay$PrPreds[,,2]
x3 <- post.pay$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

ci1.pay <- apply( x1 , 2 ,HPDI)
ci2.pay <- apply( x2 , 2 ,HPDI)
ci3.pay <- apply( x3 , 2 ,HPDI)


d$x1.pay <- m1
d$x2.pay <- m2
d$x3.pay <- m3

#####rank model
post.rank  <- extract(fit_rank)
preds.rank<-post.rank$PrPreds
str(post.pay$PrPreds)
x1 <- post.rank$PrPreds[,,1]
x2 <- post.rank$PrPreds[,,2]
x3 <- post.rank$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

ci1.rank <- apply( x1 , 2 ,HPDI)
ci2.rank <- apply( x2 , 2 ,HPDI)
ci3.rank <- apply( x3 , 2 ,HPDI)

d$x1.rank <- m1
d$x2.rank <- m2
d$x3.rank <- m3

####freq-dep
post.freq  <- extract(fit_freq)
preds.freq <- post.freq$PrPreds
str(post.freq$PrPreds)
x1 <- post.freq$PrPreds[,,1]
x2 <- post.freq$PrPreds[,,2]
x3 <- post.freq$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

d$x1.freq <- m1
d$x2.freq <- m2
d$x3.freq <- m3

ci1.freq <- apply( x1 , 2 ,HPDI)
ci2.freq <- apply( x2 , 2 ,HPDI)
ci3.freq <- apply( x3 , 2 ,HPDI)

###kin-bias
post.kin  <- extract(fit_kin)
preds.kin <- post.kin$PrPreds
str(post.kin$PrPreds)
x1 <- post.kin$PrPreds[,,1]
x2 <- post.kin$PrPreds[,,2]
x3 <- post.kin$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

d$x1.kin <- m1
d$x2.kin <- m2
d$x3.kin <- m3

ci1.kin <- apply( x1 , 2 ,HPDI)
ci2.kin <- apply( x2 , 2 ,HPDI)
ci3.kin <- apply( x3 , 2 ,HPDI)

###same sex bias
post.sex  <- extract(fit_sex)
preds.sex <- post.sex$PrPreds
str(post.sex$PrPreds)
x1 <- post.sex$PrPreds[,,1]
x2 <- post.sex$PrPreds[,,2]
x3 <- post.sex$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

d$x1.sex <- m1
d$x2.sex <- m2
d$x3.sex <- m3

ci1.sex <- apply( x1 , 2 ,HPDI)
ci2.sex <- apply( x2 , 2 ,HPDI)
ci3.sex <- apply( x3 , 2 ,HPDI)

###female-bias
post.fem  <- extract(fit_fem)
preds.fem <- post.fem$PrPreds
str(post.fem$PrPreds)
x1 <- post.fem$PrPreds[,,1]
x2 <- post.fem$PrPreds[,,2]
x3 <- post.fem$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

d$x1.fem <- m1
d$x2.fem <- m2
d$x3.fem <- m3

ci1.fem <- apply( x1 , 2 ,HPDI)
ci2.fem <- apply( x2 , 2 ,HPDI)
ci3.fem <- apply( x3 , 2 ,HPDI)

###IL
post.i <- extract(fit_i)
preds.i <- post.i$PrPreds
str(post.i$PrPreds)
x1 <- post.i$PrPreds[,,1]
x2 <- post.i$PrPreds[,,2]
x3 <- post.i$PrPreds[,,3]

m1 <- apply( x1 , 2 ,mean)
m2 <- apply( x2 , 2 ,mean)
m3 <- apply( x3 , 2 ,mean)

d$x1.i <- m1
d$x2.i <- m2
d$x3.i <- m3

ci1.i <- apply( x1 , 2 ,HPDI)
ci2.i <- apply( x2 , 2 ,HPDI)
ci3.i <- apply( x3 , 2 ,HPDI)

######plots
for(i in 1:max(d$ID_actor_index)){
  plot( x1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1.15) , ylab="prob using technique" , xlab="foraging bout",  main=unique(d$ID_actor[d$ID_actor_index==i]) , type='l'  )
  lines(x1 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1)
  lines(x2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1)
  lines(x3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1)
  lines(x1.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=2)
  lines(x2.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=2)
  lines(x3.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=2)
  lines(x1.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=3)
  lines(x2.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=3)
  lines(x3.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=3)
  lines(x1.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=4)
  lines(x2.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=4)
  lines(x3.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=4)
  lines(x1.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=5)
  lines(x2.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=5)
  lines(x3.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=5)
  

  #raw data
  nobsi <- nrow(d[d$ID_actor_index==i,])
  points(rep(1.1, nobsi) ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch= 1 + 18*d$succeed[d$ID_actor_index==i] , col=col.pal[d$technique_index[d$ID_actor_index==i]]) #empty circels are failure, filled are successes
  abline(h=1)
  
  # dstmp <- d$date_index[d$ID_actor_index==i]
  # dstmp_lab <- d$Date[d$ID_actor_index==i]
  # dstmp_i <- c(1,1+which(diff(dstmp)!=0)) #gives cutpoints of changes (to append dated)
  # abline(v=(dstmp_i-0.5) , col="grey")
  # text(x=(dstmp_i-0.5) , y=rep(1.2 , length(dstmp_i)) , labels=dstmp_lab[dstmp_i] , cex=0.5 , pos=3 , srt=20 , adj=0.5 , xpd=NA)
  
}


######multipanel plots#####
##this is for understanding model and is super infomrative. too insane for supplemental material but a motivated person could run it
pdf("multimodel_pred_comparisons_all_indiv.pdf",width=8.5,height=11)

par(mfrow=c(8,1) , mar=c(0,2,0,0) +.1 , oma=c(3.5,2.5,0,0) +0.1)
#for(i in min(d$ID_actor_index[d$ID_actor=="Upps"])){
for(i in 1: max(d$ID_actor_index)){
  #global
  plot( x1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1.15) , ylab="" , xlab="" , type='l' , main=unique(d$ID_actor[d$ID_actor_index==i]) , xaxt='n')
  lines(x1 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1)
  lines(x2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1)
  lines(x3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1)
  #CI plots
  text(-10,0.9, "a. Global" , cex=0.95,pos=4)
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #raw data
  nobsi <- nrow(d[d$ID_actor_index==i,])
  points(rep(1.1, nobsi) ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch= 1 + 18*d$succeed[d$ID_actor_index==i] , col=col.pal[d$technique_index[d$ID_actor_index==i]] , cex=0.4 , lwd=0.7) #empty circels are failure, filled are successes
  abline(h=1)
  
  #payoff
  plot( x1.pay ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout" , type='l' , xaxt='n' )
  lines(x1.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "b. Payoff-bias" , cex=0.95,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }

  #rank
  plot( x1.rank ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "c. Rank-bias" , cex=0.95,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
#freq
plot( x1.freq ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "d. Frequency-dependence" , cex=0.95 ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
#kin
  plot( x1.kin ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "e. Kin-bias" , cex=0.95, pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
##sex
  plot( x1.sex ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "f. Sex-bias" , cex=0.95  ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #fem
  plot( x1.fem ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "g. Female-bias" , cex=0.95  ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }

plot( x1.i ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l'  )
  lines(x1.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "h. Individual learning" , cex=0.95 ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
}

mtext("foraging bout", side = 1, line = 2, cex = 1, outer=TRUE) 
mtext("probability of choosing technique", side = 2, line = .75, cex = 1 , outer=TRUE) 

dev.off()

######individual plot for upps for paper
pdf("multimodel_pred_comparisons_upps.pdf",width=8.5,height=11)

par(mfrow=c(8,1) , mar=c(0,2,0,0) +.1 , oma=c(3.5,2.5,0,0) +0.1)
for(i in min(d$ID_actor_index[d$ID_actor=="Upps"])){
  #global
  plot( x1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1.15) , ylab="" , xlab="" , type='l', xaxt='n')
  lines(x1 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1)
  lines(x2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1)
  lines(x3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1)
  #CI plots
  text(-10,0.9, "a. Global" , cex=0.95,pos=4)
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #raw data
  nobsi <- nrow(d[d$ID_actor_index==i,])
  points(rep(1.1, nobsi) ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch= 1 + 18*d$succeed[d$ID_actor_index==i] , col=col.pal[d$technique_index[d$ID_actor_index==i]] , cex=0.4 , lwd=0.7) #empty circels are failure, filled are successes
  abline(h=1)
  
  #payoff
  plot( x1.pay ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout" , type='l' , xaxt='n' )
  lines(x1.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.pay ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "b. Payoff-bias" , cex=0.95,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.pay[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #rank
  plot( x1.rank ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.rank ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "c. Rank-bias" , cex=0.95,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.rank[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #freq
  plot( x1.freq ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.freq ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "d. Frequency-dependence" , cex=0.95 ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.freq[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #kin
  plot( x1.kin ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.kin ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "e. Kin-bias" , cex=0.95, pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.kin[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  ##sex
  plot( x1.sex ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.sex ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "f. Sex-bias" , cex=0.95  ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.sex[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  #fem
  plot( x1.fem ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l' , xaxt='n' )
  lines(x1.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.fem ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "g. Female-bias" , cex=0.95  ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.fem[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
  plot( x1.i ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1) , ylab="prob using technique" , xlab="foraging bout", type='l'  )
  lines(x1.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l",lw=1 , lty=1)
  lines(x2.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l", lw=1, lty=1)
  lines(x3.i ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l" , lw=1, lty=1)
  text(-10,0.9, "h. Individual learning" , cex=0.95 ,pos=4)
  
  #CI plots
  rr <- range(d$seq[d$ID_actor_index==i])
  ff <- range(d$forg_bout[d$ID_actor_index==i])
  if(diff(ff) > 0){
    shade(ci1.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[1], 0.15) )
    shade(ci2.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[2], 0.15)  )
    shade(ci3.i[,rr[1]:rr[2]] , seq(ff[1]:ff[2]) , col=col.alpha(col.pal[3], 0.15)  )
  }
  
}

mtext("foraging bout", side = 1, line = 2, cex = 1, outer=TRUE) 
mtext("probability of choosing technique", side = 2, line = .75, cex = 1 , outer=TRUE) 

dev.off()
######################################################################
################global predictions across days per group##############
######################################################################

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

dg2 <- aggregate(cbind(d$nobs_group_ch , d$nobs_group_cms , d$nobs_group_cmt, d$x1 , d$x2 , d$x3 ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , nobs_group_ch=d$nobs_group_ch , nobs_group_cms=d$nobs_group_cms , nobs_group_cmt=d$nobs_group_cmt , nobs_group_all=d$nobs_group_all ) , mean ) #gets neat summary table for plot


dg2$V1 <-  dg2$V1/dg2$nobs_group_all
dg2$V2 <-  dg2$V2/dg2$nobs_group_all
dg2$V3 <-  dg2$V3/dg2$nobs_group_all
dg2 <- dg2[order( dg2$date_index),]


##group level noha plots raw data
dgNoha <- dg2[dg2$group=="Noha",]
dgKubu <- dg2[dg2$group=="Kubu",]

##group level plots for noha
pdf("noha_preds_gegen_rawdata.pdf",width=8,height=5)
par(mar=c(4,4,1,1))
plot(dgNoha$V1~seq(1:length(dgNoha$Date)) , col=col.pal[1] , pch=1 , ylim=c(0,1.1) , xlim=c(1,11) , ylab="frequency of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="" , xaxt='n') 
lines(dgNoha$V1~seq(1:length(dgNoha$Date)) , col=col.pal[1] , type="l" , lty=3, lw=2)
points(dgNoha$V2~seq(1:length(dgNoha$Date)) , col=col.pal[2] , pch=1 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V2~seq(1:length(dgNoha$Date)) , col=col.pal[2] , type="l" , lty=3, lw=2)
points(dgNoha$V3~seq(1:length(dgNoha$Date)) , col=col.pal[3] , pch=1 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V3~seq(1:length(dgNoha$Date)) , col=col.pal[3] , type="l" , lty=3, lw=2)
points(dgNoha$V4~seq(1:length(dgNoha$Date)) , col=col.pal[1] , pch=19 )
lines(dgNoha$V4~seq(1:length(dgNoha$Date)) , col=col.pal[1] , type="l", lw=2)
points(dgNoha$V5~seq(1:length(dgNoha$Date)) , col=col.pal[2] , pch=19 )
lines(dgNoha$V5~seq(1:length(dgNoha$Date)) , col=col.pal[2] , type="l", lw=2)
points(dgNoha$V6~seq(1:length(dgNoha$Date)) , col=col.pal[3] , pch=19 )
lines(dgNoha$V6~seq(1:length(dgNoha$Date)) , col=col.pal[3] , type="l", lw=2)
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=1 , pch=19 , horiz=TRUE )
legend ("topleft" , legend=c("raw probabilities" , "model predictions") ,  col=1 , bty='n', cex=1 , pch=c(1,19), lty=c(3,1), horiz=TRUE, lw=2)
axis(1 , at=seq(1:length(dgNoha$Date)) , labels=dgNoha$Date , cex.axis=0.75)
dev.off()

##group level plots for Kubu
plot(dgKubu$V1~seq(1:length(dgKubu$Date)) , col=col.pal[1] , pch=1 , ylim=c(0,1.1) , xlim=c(1,7) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all) , main="Kubu" , xaxt='n') 
lines(dgKubu$V1~seq(1:length(dgKubu$Date)) , col=col.pal[1] , type="l" , lty=3 , lw=2)
points(dgKubu$V2~seq(1:length(dgKubu$Date)) , col=col.pal[2] , pch=1 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V2~seq(1:length(dgKubu$Date)) , col=col.pal[2] , type="l" , lty=3, lw=2)
points(dgKubu$V3~seq(1:length(dgKubu$Date)) , col=col.pal[3] , pch=1 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V3~seq(1:length(dgKubu$Date)) , col=col.pal[3] , type="l" , lty=3, lw=2)
points(dgKubu$V4~seq(1:length(dgKubu$Date)) , col=col.pal[1] , pch=19 )
lines(dgKubu$V4~seq(1:length(dgKubu$Date)) , col=col.pal[1] , type="l", lw=2)
points(dgKubu$V5~seq(1:length(dgKubu$Date)) , col=col.pal[2] , pch=19 )
lines(dgKubu$V5~seq(1:length(dgKubu$Date)) , col=col.pal[2] , type="l", lw=2)
points(dgKubu$V6~seq(1:length(dgKubu$Date)) , col=col.pal[3] , pch=19 )
lines(dgKubu$V6~seq(1:length(dgKubu$Date)) , col=col.pal[3] , type="l", lw=2)
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=1 , pch=19 , horiz=TRUE )
legend ("topleft" , legend=c("raw probabilities" , "model predictions") ,  col=1 , bty='n', cex=1 , pch=c(1,19), lty=c(3,1), horiz=TRUE lw=2)
axis(1 , at=seq(1:length(dgKubu$Date)) , labels=dgKubu$Date , cex.axis=0.75)

####################################
###########plots of parameters######
####################################

col.pal <- brewer.pal(12, "Paired")

#function to default maximize window lims to posterior density max
densmax_y <- function(e,f){
  b <- max(density(e)$y)
  a <- max(density(f)$y)
  ymax <- ifelse(a>b, a , b)
  ymax
}

#lambda#
dev.off()

lambda_plots <- function(x,extract=TRUE){ 
  
  if (!extract) {
   post <- post
  } else {
    post <- extract(x)
  }
    #sex diff
  post_plot1 <- exp(post$S[,1,1] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
  post_plot2 <- exp(post$S[,1,2] + apply(post$A[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
  dens(post_plot1 , main=expression(paste(lambda)) , xlim=c(0,25) , ylim=c(0,densmax_y(post_plot1, post_plot2) ) , xlab="sensitivity to attraction scores" , col="white")##phi
  abline(v=median(post_plot1) ,col = col.alpha(col.pal[1], 0.5) ) 
  shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[1], 0.5))
  shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
  abline(v=median(post_plot2) , col = col.alpha(col.pal[2], 0.5) ) 
  legend("topright", legend = c("female", "male"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #age diff
  post_plot3 <- exp(post$A[,1,1] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
  post_plot4 <- exp(post$A[,1,2] + apply(post$S[,1,] + post$G[,,1] , 1 , mean) + apply( post$I[,,1], 1 ,mean ))
  dens(post_plot3 , main=expression(paste(lambda)) , xlim=c(0,25) , ylim=c(0,densmax_y(post_plot3, post_plot4)) , xlab="sensitivity to attraction scores" , col="white")##phi
  abline(v=median(post_plot3) ,col = col.alpha(col.pal[1], 0.5) ) 
  shade( density(post_plot3) , lim= as.vector(HPDI(post_plot3, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
  shade( density(post_plot4) , lim= as.vector(HPDI(post_plot4, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
  abline(v=median(post_plot4) , col = col.alpha(col.pal[2], 0.5) ) 
  legend("topright", legend = c("juvenile", "adult"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #group_diff
  post_plot5 <- exp( post$G[,1,1] + apply( post$A[,1,] + post$S[,1,] , 1 ,mean) + apply( post$I[,,1], 1 ,mean ))
  post_plot6 <- exp( post$G[,2,1] + apply( post$A[,1,] + post$S[,1,] , 1 ,mean) + apply( post$I[,,1], 1 ,mean ))
  dens(post_plot5 , main=expression(paste(lambda)) , xlim=c(0,20) , ylim=c(0,densmax_y(post_plot5, post_plot6)) , xlab="sensitivity to attraction scores" , col="white")##phi
  abline(v=median(post_plot5) ,col = col.alpha(col.pal[1], 0.5) ) 
  shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[1], 0.25))
  shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[2], 0.25))
  abline(v=median(post_plot6) , col = col.alpha(col.pal[2], 0.5) ) 
  legend("topright", legend = c("Kubu", "Noha"), col = col.pal[1:2] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
}

phi_plots <- function(x,extract=TRUE){ 
  if (!extract) {
    post <- post
  } else {
    post <- extract(x)
  }
  #sex diff
  post_plot1 <- logistic(post$S[,2,1] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean ))
  post_plot2 <- logistic(post$S[,2,2] + apply(post$A[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean ))
  dens(post_plot1, main=expression(paste(phi)) , xlim=c(0,0.5) ,  ylim=c(0,densmax_y(post_plot1, post_plot2)) , xlab="weight given to recent experience" , col="white")##phi
  abline(v=median(post_plot1) ,col = col.alpha(col.pal[3], 0.5) ) 
  shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
  shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
  abline(v=median(post_plot2) , col = col.alpha(col.pal[4], 0.5) ) 
  legend("topright", legend = c("female", "male"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #age diff
  post_plot3 <- logistic(post$A[,2,1] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean ))
  post_plot4 <- logistic(post$A[,2,2] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean ))
  dens(post_plot3, main=expression(paste(phi)) , xlim=c(0,0.5) , ylim=c(0,densmax_y(post_plot3, post_plot4)) , xlab="weight given to recent experience" , col="white")##phi
  abline(v=median(post_plot3) ,col = col.alpha(col.pal[3], 0.5) ) 
  shade( density(post_plot3) , lim= as.vector(HPDI(post_plot3, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
  shade( density(post_plot4) , lim= as.vector(HPDI(post_plot4, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
  abline(v=median(post_plot4) , col = col.alpha(col.pal[4], 0.5) ) 
  legend("topright", legend = c("juvenile", "adult"), col = col.pal[3:4] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  
  #group_diff
  post_plot5 <- logistic( post$G[,1,2] + apply( post$A[,2,] + post$S[,2,] , 1 ,mean)  + apply( post$I[,,2], 1 ,mean ))
  post_plot6 <- logistic( post$G[,2,2] + apply( post$A[,2,] + post$S[,2,] , 1 ,mean)  + apply( post$I[,,2], 1 ,mean ))
  dens(post_plot5, main=expression(paste(phi)) , xlim=c(0,0.5) , ylim=c(0,densmax_y(post_plot5, post_plot6)) , xlab="weight given to recent experience" , col="white")##phi
  abline(v=mean(post_plot5) ,col = col.alpha(col.pal[3], 0.5) ) 
  shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[3], 0.25))
  shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[4], 0.25))
  abline(v=mean(post_plot6) , col = col.alpha(col.pal[3], 0.5) ) 
  legend("topright", legend = c("Kubu", "Noha"), col = col.pal[3:4] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
}

gamma_plots <- function(x,extract=TRUE){ 
  if (!extract) {
    post <- post
  } else {
    post <- extract(x)
  }
  #sex diff
  post_plot1 <- logistic(post$S[,3,1] + apply(post$A[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean ))
  post_plot2 <- logistic(post$S[,3,2] + apply(post$A[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean ))
  
  dens(post_plot1 , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" , ylim=c(0,densmax_y(post_plot1, post_plot2)) , col="white")##phi
  abline(v=median(post_plot1) ,col = col.alpha(col.pal[5], 0.5) ) 
  shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
  shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2 , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
  abline(v=median(post_plot2) , col = col.alpha(col.pal[6], 0.5) ) 
  legend("topright", legend = c("female", "male"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  #age diff
  post_plot3 <- logistic(post$A[,3,1] + apply(post$S[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean ))
  post_plot4 <- logistic(post$A[,3,2] + apply(post$S[,3,] + post$G[,,3] , 1 , mean) + apply( post$I[,,3], 1 ,mean ))
  dens(post_plot3 , main=expression(paste(gamma)) , xlim=c(0,1) , xlab="weight given to social information" ,  ylim=c(0,densmax_y(post_plot3, post_plot4)) , col="white")##phi
  abline(v=median(post_plot3) ,col = col.alpha(col.pal[5], 0.5) ) 
  shade( density(post_plot3) , lim= as.vector(HPDI(post_plot3, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
  shade( density(post_plot4) , lim= as.vector(HPDI(post_plot4 , prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
  abline(v=median(post_plot4) , col = col.alpha(col.pal[6], 0.5) ) 
  legend("topright", legend = c("juvenile", "adult"), col = col.pal[5:6] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #group_diff
  post_plot5 <- logistic( post$G[,1,3] + apply( post$A[,3,] + post$S[,3,] , 1 ,mean)  + apply( post$I[,,3], 1 ,mean ))
  post_plot6 <- logistic( post$G[,2,3] + apply( post$A[,3,] + post$S[,3,] , 1 ,mean)  + apply( post$I[,,3], 1 ,mean ))
  dens(post_plot5, main=expression(paste(gamma)) , xlim=c(0,0.5) , ylim=c(0,densmax_y(post_plot5, post_plot6)) , xlab="weight given to social information" , col="white")
  abline(v=mean(post_plot5) ,col = col.alpha(col.pal[5], 0.5) ) 
  shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[5], 0.25))
  shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[6], 0.25))
  abline(v=mean(post_plot6) , col = col.alpha(col.pal[6], 0.5) ) 
  legend("topright", legend = c("Kubu", "Noha"), col = col.pal[5:6] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
}

fc_plots <- function(x,extract=TRUE){ 
  if (!extract) {
    post <- post
  } else {
    post <- extract(x)
  }
  #sex diff
  post_plot1 <- exp(post$S[,4,1] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ))
  post_plot2 <- exp(post$S[,4,2] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ))
  dens(post_plot1 , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , ylim=c(0,densmax_y(post_plot1, post_plot2)) , col="white")##phi
  abline(v=median(post_plot1) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot2) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #age diff
  post_plot3 <- exp(post$A[,4,1] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ))
  post_plot4 <- exp(post$A[,4,2] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ))
  dens(post_plot3 , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , ylim=c(0,densmax_y(post_plot3, post_plot4)) , col="white")##phi
  abline(v=median(post_plot3) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot3) , lim= as.vector(HPDI(post_plot3, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot4) , lim= as.vector(HPDI(post_plot4, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot4) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("juvenile", "adult"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  #group_diff
  post_plot5 <- exp( post$G[,1,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean ))
  post_plot6 <- exp( post$G[,2,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean ))
  dens(post_plot5 , main=expression(paste(f_c)) , xlim=c(0,5) , xlab="strength of freq dependence" , ylim=c(0,densmax_y(post_plot3, post_plot4)) , col="white")##phi
  abline(v=median(post_plot5) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot6) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("Kubu", "Noha"), col = col.pal[7:8] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
}

kappa_plots <- function(x,extract=TRUE){
  if (!extract) {
    post <- post
  } else {
    post <- extract(x)
  }
  ##add a conditional thing on X
  post_plot1 <- post$S[,4,1] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )
  post_plot2 <-post$S[,4,2] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )
  dens(post_plot1 , main="kappa" , xlim=c(0,5) , xlab="strength of cue-bias" , ylim=c(0,densmax_y(post_plot1, post_plot2)) , col="white")##phi
  abline(v=median(post_plot1) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot2) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("female", "male"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #age diff
  post_plot_j <- post$S[,4,1] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )
  post_plot_a <-post$S[,4,2] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean )
  dens(post_plot_a , main="kappa" , xlim=c(0,5) , xlab="strength of cue-bias" , ylim=c(0,densmax_y(post_plot_a, post_plot_j)), col="white")
  abline(v=median(post_plot_j) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot_j) , lim= as.vector(HPDI(post_plot_j, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot_a) , lim= as.vector(HPDI(post_plot_a, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot_a) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("juvenile", "adult"), col = col.pal[7:8] , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
  #group_diff
  post_plot5 <- post$G[,1,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean )
  post_plot6 <- post$G[,2,4] + apply( post$A[,4,] + post$S[,4,] , 1 ,mean) + apply( post$I[,,4], 1 ,mean )
  dens(post_plot5 , main="kappa" , xlim=c(0,5) , xlab="strength of freq dependence" , ylim=c(0,densmax_y(post_plot5, post_plot6)) , col="white")##phi
  abline(v=median(post_plot5) ,col = col.alpha(col.pal[7], 0.5) ) 
  shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[7], 0.25))
  shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[8], 0.25))
  abline(v=median(post_plot6) , col = col.alpha(col.pal[8], 0.5) ) 
  legend("topright", legend = c("Kubu", "Noha"), col = col.pal[7:8] , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
  
}

######cues for global models


kappa_global_plots <- function(x,extract=TRUE){
  if (!extract) {
    post <- post
  } else {
    post <- extract(x)
  }
  for (i in 1:5){
    kpar_names <- c( "k_fem" , "k_kin" , "k_pay" , "k_rank" , "k_sex")
    
    post_plot1 <- post$S[,4+i,1] + apply(post$A[,4+i,] + post$G[,,4+i] , 1 , mean) + apply( post$I[,,4+i], 1 ,mean )
    post_plot2 <-post$S[,4+i,2] + apply(post$A[,4+i,] + post$G[,,4+i] , 1 , mean) + apply( post$I[,,4+i], 1 ,mean )
    dens(post_plot1 , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , ylim=c(0,densmax_y(post_plot1, post_plot2)) , col="white")##phi
    abline(v=median(post_plot1) ,col = col.alpha(col.pal[2*i-1], 0.5) ) 
    shade( density(post_plot1) , lim= as.vector(HPDI(post_plot1, prob=0.9999)) , col = col.alpha(col.pal[2*i-1], 0.25))
    shade( density(post_plot2) , lim= as.vector(HPDI(post_plot2, prob=0.9999)) , col = col.alpha(col.pal[2*i], 0.25))
    abline(v=median(post_plot2) , col = col.alpha(col.pal[2*i], 0.5) ) 
    legend("topright", legend = c("female", "male"), col = c(col.pal[2*i-1], col.pal[2*i]) , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #age diff
    post_plot_j <- post$S[,4+i,1] + apply(post$A[,4+i,] + post$G[,,4+i] , 1 , mean) + apply( post$I[,,4+i], 1 ,mean )
    post_plot_a <-post$S[,4+i,2] + apply(post$A[,4+i,] + post$G[,,4+i] , 1 , mean) + apply( post$I[,,4+i], 1 ,mean )
    dens(post_plot_a , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , ylim=c(0,densmax_y(post_plot_a, post_plot_j)), col="white")
    abline(v=median(post_plot_j) ,col = col.alpha(col.pal[2*i-1], 0.5) ) 
    shade( density(post_plot_j) , lim= as.vector(HPDI(post_plot_j, prob=0.9999)) , col = col.alpha(col.pal[2*i-1], 0.25))
    shade( density(post_plot_a) , lim= as.vector(HPDI(post_plot_a, prob=0.9999)) , col = col.alpha(col.pal[2*i], 0.25))
    abline(v=median(post_plot_a) , col = col.alpha(col.pal[2*i], 0.5) ) 
    legend("topright", legend = c("juvenile", "adult"), col = c(col.pal[2*i-1], col.pal[2*i]) , pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    #group_diff
    post_plot5 <- post$G[,1,4+i] + apply( post$A[,4+i,] + post$S[,4+i,] , 1 ,mean) + apply( post$I[,,4+i], 1 ,mean )
    post_plot6 <- post$G[,2,4+i] + apply( post$A[,4+i,] + post$S[,4+i,] , 1 ,mean) + apply( post$I[,,4+i], 1 ,mean )
    dens(post_plot5 , main=kpar_names[i] , xlim=c(0,5) , xlab="strength of cue-bias" , ylim=c(0,densmax_y(post_plot5, post_plot6)) , col="white")##phi
    abline(v=median(post_plot5) ,col = col.alpha(col.pal[2*i-1], 0.5) ) 
    shade( density(post_plot5) , lim= as.vector(HPDI(post_plot5, prob=0.9999)) , col = col.alpha(col.pal[2*i-1], 0.25))
    shade( density(post_plot6) , lim= as.vector(HPDI(post_plot6, prob=0.9999)) , col = col.alpha(col.pal[2*i], 0.25))
    abline(v=median(post_plot6) , col = col.alpha(col.pal[2*i], 0.5) ) 
    legend("topright", legend = c("Kubu", "Noha"), col = c(col.pal[2*i-1], col.pal[2*i]) , pch = 19, bty = "n", pt.cex = 0.5, cex = 0.5 )
    
    
  }
}

#IL
post <- extract(fit_i)
lambda_plots(fit_i,extract=FALSE)
phi_plots(fit_i,extract=FALSE)
##Freq dep
post <- extract(fit_freq)
lambda_plots(fit_freq,extract=FALSE)
phi_plots(fit_freq,extract=FALSE)
gamma_plots(fit_freq,extract=FALSE)
fc_plots(fit_freq,extract=FALSE)

c( "k_fem" , "k_kin" , "k_pay" , "k_rank" , "k_sex")

##female-bias
post <- extract(fit_fem)
lambda_plots(fit_fem,extract=FALSE)
phi_plots(fit_fem,extract=FALSE)
gamma_plots(fit_fem,extract=FALSE)
kappa_plots(fit_fem,extract=FALSE)

##kin-bias
post <- extract(fit_kin)
lambda_plots(fit_kin,extract=FALSE)
phi_plots(fit_kin,extract=FALSE)
gamma_plots(fit_kin,extract=FALSE)
kappa_plots(fit_kin,extract=FALSE)

##payoff-bias
post <- extract(fit_pay)
lambda_plots(fit_pay,extract=FALSE)
phi_plots(fit_pay,extract=FALSE)
gamma_plots(fit_pay,extract=FALSE)
kappa_plots(fit_pay,extract=FALSE)

##rank-bias
post <- extract(fit_rank)
lambda_plots(fit_rank,extract=FALSE)
phi_plots(fit_rank,extract=FALSE)
gamma_plots(fit_rank,extract=FALSE)
kappa_plots(fit_rank,extract=FALSE)

##sex-bias
post <- extract(fit_sex)
lambda_plots(fit_sex,extract=FALSE)
phi_plots(fit_sex,extract=FALSE)
gamma_plots(fit_sex,extract=FALSE)
kappa_plots(fit_sex,extract=FALSE)

lambda_plots(fit_global,extract=FALSE)
phi_plots(fit_global,extract=FALSE)
gamma_plots(fit_global,extract=FALSE)
kappa_global_plots(fit_global, extract=FALSE)

######get descriptive stats of techniques
ddesc <- aggregate(cbind( d$y1 , d$y2 , d$y3 ) , list( group=d$group) , mean ) #gets neat summary table for plot, calculations used in text

ddesc2 <- aggregate(cbind( d$y1 , d$y2 , d$y3 ) , list( group=d$group , date_index=d$date_index, Date=d$Date , nobs_group_ch=d$nobs_group_ch , nobs_group_cms=d$nobs_group_cms , nobs_group_cmt=d$nobs_group_cmt , nobs_group_all=d$nobs_group_all ) , mean )

ddesc2$p1 <-  ddesc2$nobs_group_ch/ddesc2$nobs_group_all
ddesc2$p2 <-  ddesc2$nobs_group_cms/ddesc2$nobs_group_all
ddesc2$p3 <-  ddesc2$nobs_group_cmt/ddesc2$nobs_group_all
ddesc2 <- ddesc2[order(ddesc2$date_index),]

plot(V1 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col="white" , xlab="experimental date" , ylab="prob technique is succesful")
lines(V1 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[1], lw=2)
lines(V2 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[2], lw=2)
lines(V3 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[3], lw=2)
lines(p1 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[1], lty=2 , lw=2)
lines(p2 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[2], lty=2 , lw=2)
lines(p3 ~ date_index , data=ddesc2[ddesc2$group=="Noha",] , ylim=c(0,1) , col=col.pal[3], lty=2 , lw=2)
legend("topleft", inset=-.01, c("prob. technique succesful","freq. technique in population") , horiz=TRUE , cex=1,bty = "n" , lty=c(1,3) , lw=2 ) 
legend("topright", inset=-.01, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=1,bty = "n" )

plot(V1 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col="white" , xlab="experimental date" , ylab="prob technique is succesful")
lines(V1 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[1], lw=2)
lines(V2 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[2], lw=2)
lines(V3 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[3], lw=2)
lines(p1 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[1], lty=2 , lw=2)
lines(p2 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[2], lty=2 , lw=2)
lines(p3 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[3], lty=2 , lw=2)
legend("topleft", inset=-.01, c("prob. technique succesful","freq. technique in population") , horiz=TRUE , cex=1,bty = "n" , lty=c(1,3) , lw=2 ) 
legend("topright", inset=-.01, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=1,bty = "n" )


# lines(V1 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[1], lty=2)
# lines(V2 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[2], lty=2)
# lines(V3 ~ date_index , data=ddesc2[ddesc2$group=="Kubu",] , ylim=c(0,1) , col=col.pal[3] , lty=2)

#sumamry stats of groups
length(unique(d$ID_actor[d$group=="Kubu"]))
length(unique(d$ID_actor[d$group=="Noha"]))
nrow(d[d$group=="Kubu",])
nrow(d[d$group=="Noha",])
range(d$peanut_bout)
##export WAICtab
write.csv(WAICtab , file="WAICtab_20days.csv")

precis(fit_global)
###################################3

##############################
#######plot main effects######
#############################
#note this was done somewhat by hand and csvs were saved to correspond to each model
post <- extract(fit_fem)
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
  phi_adult = logistic(post$A[,2,2] + apply(post$S[,2,] + post$G[,,2] , 1 , mean) + apply( post$I[,,2], 1 ,mean ))
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
  beta_female = post$S[,4,1] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ) ,
  beta_male = post$S[,4,2] + apply(post$A[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ) ,
  beta_juv = post$A[,4,1] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ) ,
  beta_adult = post$A[,4,2] + apply(post$S[,4,] + post$G[,,4] , 1 , mean) + apply( post$I[,,4], 1 ,mean ) 

)


plot(precis(pbeta , depth=2) )
precis(pbeta)

biglist <- list(plambda, plogits,pbeta )
biglist2tab <- precis(biglist , depth=3 , ci=TRUE , digits=2 , hist=FALSE)
write.csv(biglist2tab , file="femparams.csv")

sigmalist2tab <- precis(fit_global , pars=c("sigma_i" , "sigma_g") , depth=3 , ci=TRUE , digits=2 , hist=FALSE)
labels <- c("sigma_i_lambda" , "sigma_i_phi" , "sigma_i_gamma" , "sigma_i_fc" , "sigma_i_beta_fem" , "sigma_i_beta_kin" , "sigma_i_beta_pay" , "sigma_i_beta_rank" , "sigma_i_beta_sex" , "sigma_g_lambda" , "sigma_g_phi" , "sigma_g_gamma" , "sigma_g_fc" , "sigma_g_beta_fem" , "sigma_g_beta_kin" , "sigma_g_beta_pay" , "sigma_g_beta_rank" , "sigma_g_beta_sex")

pdf("sigmadotplotsglobal.pdf" , width=7 , height=5)
plot(sigmalist2tab , labels=labels)
dev.off()

write.csv(sigmalist2tab , file="globalparamssigmas.csv")

str(post)