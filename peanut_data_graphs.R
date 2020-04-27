library(rethinking)
library(RColorBrewer)
#load("/Users/BJB/Downloads/20min_slopes_VervetPNUTindex.rdata")
load("~/Downloads/vervet_peanut_ewa_20min_25April2020.rdata")
###############################
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

##group level plots for noha
dev.off()
plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="Noha" ) 
lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l")
points(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l")
points(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , pch=19 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l")
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=0.75 , pch=19 , horiz=TRUE )

##group level plots for kubu
plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all) , main="Kubu" ) 
points(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , type="l", lty=3)
points(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , type="l", lty=3)
points(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , pch=18 , cex=100*dgKubu$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , type="l" , lty=3)
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=0.75 , pch=19 , horiz=TRUE )

###individual
plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="Noha" ) 
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
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=0.75 , pch=19 , horiz=TRUE )

###individual
plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=19 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all) , main="Kubu" ) 
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
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=0.75 , pch=19 , horiz=TRUE )

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
title( main="Noha Group Average", outer=FALSE , line=-0.75 , cex=0.5)

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
title( main="Kubu Group Average", outer=FALSE , line=-0.75 , cex=0.5)

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
####################################
###########plots of parameters######
####################################
col.pal <- brewer.pal(12, "Paired")

pre_lab_i <- c("log(lambda_female)" , "log(lambda_male)" , "logit(phi_female)" , "logit(phi_male)" , "log(lambda_juvenile)" , "log(lambda_adult)" , "logit(phi_juvenile)" , "logit(phi_adult)")
plot(precis(fit_i, pars=c("S","A") , depth=3) ,  labels= pre_lab_i)
pre_lab_f <- c("log(lambda_female)" , "log(lambda_male)" , "logit(phi_female)" , "logit(phi_male)" , "logit(gamma_female)" , "logit(gamma_male)", "log(fc_female)" , "log(fc_male)" ,"log(lambda_juvenile)" , "log(lambda_adult)" , "logit(phi_juvenile)" , "logit(phi_adult)", "logit(gamma_juvenile)" , "logit(gamma_adult)" ,  "log(fc_juvenile)" , "log(fc_adult)") 
plot(precis(fit_freq, pars=c("S","A") , depth=3) ,  labels= pre_lab_f)
pre_lab_cue <- c("log(lambda_female)" , "log(lambda_male)" , "logit(phi_female)" , "logit(phi_male)" , "logit(gamma_female)" , "logit(gamma_male)", "kappa_female" , "kappa_male" ,"log(lambda_juvenile)" , "log(lambda_adult)" , "logit(phi_juvenile)" , "logit(phi_adult)", "logit(gamma_juvenile)" , "logit(gamma_adult)" ,  "kappa_juvenile" , "kappa_adult") 
plot(precis(fit_pay, pars=c("S","A") , depth=3) ,  labels= pre_lab_cue , main="fit_pay")
plot(precis(fit_rank, pars=c("S","A") , depth=3) ,  labels= pre_lab_cue , main="fit_rank")
plot(precis(fit_sex, pars=c("S","A") , depth=3) ,  labels= pre_lab_cue , main="fit_sex")
plot(precis(fit_kin, pars=c("S","A") , depth=3) ,  labels= pre_lab_cue , main="fit_kin")
plot(precis(fit_fem, pars=c("S","A") , depth=3) ,  labels= pre_lab_cue , main="fit_fem")

#function to default maximize window lims to posterior density max
densmax_y <- function(e,f){
  b <- max(density(e)$y)
  a <- max(density(f)$y)
  ymax <- ifelse(a>b, a , b)
  ymax
}

#lambda#
dev.off()
plot(precis(fit_pay , pars='S' , depth=3))
plot(precis(fit_pay , pars='A' , depth=3))
plot(precis(fit_i , pars=c('S' , 'A') , depth=3))

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

####sigma_plots
# dens(post$sigma[,1] , main="sigmas" , xlim=c(0,4) , col="white")##phi
# 
# for (i in 1:ncol(post$sigma)){
# shade( density(post$sigma[,i]) , lim= as.vector(HPDI(post$sigma[,i], prob=0.9999)) , col = col.alpha(col.pal[2*i-1], 0.25))
# abline(v=mean(post$sigma[,i]) , col = col.pal[2*i-1] )
# }
# legend("topright", legend = c("lambda", "phi", "gamma", "placeholder"), col = rbind(col.pal[1],col.pal[3],col.pal[5], col.pal[7]), pch = 9, bty = "n", pt.cex = 0.5, cex = 0.5 )
# curve( dexp( x , rate=1) , lty=2 , add=TRUE , col="black" )

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

######################################
#########model predictions#############
######################################
preds<-post$PrPreds
d$x1 <- apply( post$PrPreds[,,1] , 2 ,mean)
d$x2 <- apply( post$PrPreds[,,2] , 2 ,mean)
d$x3 <- apply( post$PrPreds[,,3] , 2 ,mean)

plot(1:length(x1) , x1 , col="red" , xlim=c(0,1000) , ylim=c(0,1))
points(1:length(x2) , x2 , col="blue")
points(1:length(x3) , x3, col="green")
col.pal=brewer.pal(3,"Accent")
plot(d$date_index, d$x1 , col=col.alpha(col.pal[1], 0.01) , pch=19)
plot(d$date_index, d$x2 , col=col.alpha(col.pal[2], 0.01) , pch=19)
plot(d$date_index, d$x3 , col=col.alpha(col.pal[3], 0.01) , pch=19)
dgg <- aggregate(cbind(d$x1 , d$x2 , d$x3 ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index) , mean ) #gets neat summary table for plot
