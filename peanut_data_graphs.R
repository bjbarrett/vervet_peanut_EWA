library(rethinking)
library(RColorBrewer)
#load("/Users/BJB/Downloads/20min_slopes_VervetPNUTindex.rdata")

load("/Users/BJB/Dropbox/Vervets/vervet_peanut_EWA/vervet_peanut_ewa_20min_25April2020.rdata")
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

######################################
#########model predictions#############
######################################
preds<-post$PrPreds
str(post$PrPreds)
x1 <- post$PrPreds[,,1]
x2 <- post$PrPreds[,,2]
x3 <- post$PrPreds[,,3]

x1 <- apply( x1 , 2 ,mean)
x2 <- apply( x2 , 2 ,mean)
x3 <- apply( x3 , 2 ,mean)
d$x1 <- x1
d$x2 <- x2
d$x3 <- x3

col.pal=brewer.pal(3,"Accent")
plot(1:length(x1) ,x1 , col=col.pal[1] , xlim=c(0,100) , ylim=c(0,1) , pch=19)
points(1:length(x2) , x2 , col=col.pal[2] , pch=19)
points(1:length(x3) , x3, col=col.pal[3] , pch=19)
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=0.75 , pch=19 , horiz=TRUE )

plot(d$date_index, d$x1 , col=col.alpha(col.pal[1], 0.01) , pch=19)
plot(d$date_index, d$x2 , col=col.alpha(col.pal[2], 0.01) , pch=19)
plot(d$date_index, d$x3 , col=col.alpha(col.pal[3], 0.01) , pch=19)
dp <- aggregate(cbind(d$x1 , d$x2 , d$x3 ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index) , mean ) #gets neat summary table for plot

i <- 4
plot( x1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=19 , col=col.pal[1] , ylim=c(0,1) )
points(x2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=19 , col=col.pal[2])
points(x3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=19 , col=col.pal[3])

######################get predictions for each individual##################
dg <- list(
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

dg$s <- dg$s/ max(dg$s)
dg$f <- dg$f / max(dg$f)
dg$k <- dg$k/ max(dg$k)
dg$p <- dg$p/ max(dg$p)
dg$r <- dg$r/ max(dg$r)
dg$x <- dg$x/ max(dg$x)
dg$obs_index <-  d$obs_index

AC=PrS=PrA=lin_mod=s_temp=rep(0,3) #stroage slots for calculating predictions

Preds = array(0,dim=c(nrow(d),3,max(d$ID_actor_index))) #predictions for all individuals, all techniques, across all timesteps
Preds2 = array(0,dim=c(nrow(d),3)) ##predictions for all individuals, all techniques, at times when they foraged

for ( i in 1:dg$N ) {
  
  if ( dg$bout[i]==1 ) { ##see if this needs to be removed
    
    lambda = median( exp( post$I[,dg$id[i],1] + post$G[,dg$group_index[i],1] + post$A[,1,dg$age_index[i]] + post$S[,1,dg$sex_index[i]] ) )
    phi= median(inv_logit(  post$I[,dg$id[i],2] + post$G[,dg$group_index[i],2] +  post$A[,2,dg$age_index[i]] + post$S[,2,dg$sex_index[i]] ) )
    gamma = median(inv_logit(post$I[,dg$id[i],3] + post$G[,dg$group_index[i],3] + post$A[,3,dg$age_index[i]] + post$S[,3,dg$sex_index[i]] ) )
    fc =  median(exp(post$I[,dg$id[i],4] + post$G[,dg$group_index[i],4] + post$A[,4,dg$age_index[i]] + post$S[,4,dg$sex_index[i]]) )
    bf =  median(post$I[,dg$id[i],5] + post$G[,dg$group_index[i],5] + post$A[,5,dg$age_index[i]] + post$S[,5,dg$sex_index[i]] )
    bk =  median(post$I[,dg$id[i],6] + post$G[,dg$group_index[i],6] + post$A[,6,dg$age_index[i]] + post$S[,6,dg$sex_index[i]] )
    bp =  median(post$I[,dg$id[i],7] + post$G[,dg$group_index[i],7] + post$A[,7,dg$age_index[i]] + post$S[,7,dg$sex_index[i]] )
    br =  median(post$I[,dg$id[i],8] + post$G[,dg$group_index[i],8] + post$A[,8,dg$age_index[i]] + post$S[,8,dg$sex_index[i]] )
    bx =  median(post$I[,dg$id[i],9] + post$G[,dg$group_index[i],9] + post$A[,9,dg$age_index[i]] + post$S[,9,dg$sex_index[i]] )
  }
  
  for ( j in 1:3 ) {
    if ( dg$bout[i] > 1 ) {
      AC[j] = (1-phi)*AC[j] + phi*dg$y[i-1,j]
    } else {
      AC[j] = 0
    }
  }
  
  for (j in 1:3){ PrA[j] = exp(lambda*AC[j])/sum(exp(lambda*AC))}
  
  if ( dg$bout[i] > 1 ) {
    if (sum( dg$s[i,] ) > 0 ) { 
      for ( j in 2:3 ) {
        lin_mod[j] = exp( bf*dg$f[i,j] + bk*dg$k[i,j] + bp*dg$p[i,j] + br*dg$r[i,j] + bx*dg$x[i,j] )
      }
      lin_mod[1] = 1
      
      for ( j in 1:3 ){  s_temp[j] = dg$s[i,j]^fc }
      for ( j in 1:3 ){ lin_mod[j] = lin_mod[j] * s_temp[j] }
      
      for (j in 1:3){PrS[j] = lin_mod[j]/sum(lin_mod)}
      
      
      for(j in 1:3){ Preds[i,j,dg$id[i]] = (1-gamma)*PrA[j] + gamma*PrS[j] 
      Preds2[i,j] = (1-gamma)*PrA[j] + gamma*PrS[j]
      }
      
    } else {
      for(j in 1:3){ Preds[i,j,dg$id[i]]= PrA[j] 
      Preds2[i,j] = PrA[j]}
    }
  } else {
    for(j in 1:3){ Preds[i,j,dg$id[i]]= PrA[j]
    Preds2[i,j] = PrA[j] }
  }
}







str(Preds2)
plot(1:3000,Preds2[1:3000,1] , col= col.pal[1] , pch=19 , ylim=c(0,1))
points(1:3000,Preds2[1:3000,2] , col= col.pal[2] , pch=19 )
points(1:3000,Preds2[1:3000,3] , col= col.pal[3] , pch=19 )

d$p1 <- Preds2[,1]
d$p2 <- Preds2[,2]
d$p3 <- Preds2[,3]

for(k in 1:nrow(d)){
  d$succeed[k] <- ifelse(sum(d$y1[k],d$y2[k],d$y3[k])>0 , 1 , 0) ##0 if failure, 1 if success
  }

d$succeed[d$ID_actor_index==i]

pdf("individual_peanut_vervet_preds.pdf",width=8.5,height=11) 
par( mfrow=c(8, 1) , mar=c(3,3,3,1) , oma=c(4,4,.5,.5) )
par(cex = 0.5)
par(tcl = -0.2)
par(mgp = c(2, 0.6, 0))
plot.new()
legend("top", inset=0.1, c("ch","cms","cmt") , fill=col.pal,border=col.pal, horiz=TRUE,cex=2,bty = "n" )
legend("bottom", inset=0.1, c("failure","success") , pch=c(1,19), horiz=TRUE,cex=2,bty = "n")

for(i in 1:max(d$ID_actor_index)){
  plot( p1 ~ forg_bout , data=d[d$ID_actor_index==i,]  , pch=20 , col=col.pal[1] , ylim=c(0,1.15) , ylab="prob using technique" , xlab="foraging bout",  main=unique(d$ID_actor[d$ID_actor_index==i]))
  lines(p1 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[1] , type="l")
  points(p2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2])
  lines(p2 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[2] , type="l")
  points(p3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3])
  lines(p3 ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch=20 , col=col.pal[3] , type="l")
  nobsi <- nrow(d[d$ID_actor_index==i,])
  points(rep(1.1, nobsi) ~ forg_bout , data=d[d$ID_actor_index==i,] ,  pch= 1 + 18*d$succeed[d$ID_actor_index==i] , col=col.pal[d$technique_index[d$ID_actor_index==i]]) #empty circels are failure, filled are successes
  abline(h=1)
}

dev.off()


################global predictions across days##############
dg2 <- aggregate(cbind(d$nobs_group_ch , d$nobs_group_cms , d$nobs_group_cmt, d$p1 , d$p2 , d$p3 ) , list(Date=d$Date , date_index=d$date_index , group=d$group, group_index=d$group_index , nobs_group_ch=d$nobs_group_ch , nobs_group_cms=d$nobs_group_cms , nobs_group_cmt=d$nobs_group_cmt , nobs_group_all=d$nobs_group_all ) , mean ) #gets neat summary table for plot


dg2$V1 <-  dg2$V1/dg2$nobs_group_all
dg2$V2 <-  dg2$V2/dg2$nobs_group_all
dg2$V3 <-  dg2$V3/dg2$nobs_group_all
dg2 <- dg2[order( dg2$date_index),]


##group level noha plots raw data
dgNoha <- dg2[dg2$group=="Noha",]
dgKubu <- dg2[dg2$group=="Kubu",]


##group level plots for noha
plot(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , pch=1 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all) , main="Noha" ) 
lines(dgNoha$V1~dgNoha$date_index , col=col.pal[1] , type="l" , lty=3)
points(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , pch=1 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V2~dgNoha$date_index , col=col.pal[2] , type="l" , lty=3)
points(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , pch=1 , cex=3*dgNoha$nobs_group_all/max(dgNoha$nobs_group_all))
lines(dgNoha$V3~dgNoha$date_index , col=col.pal[3] , type="l" , lty=3)
points(dgNoha$V4~dgNoha$date_index , col=col.pal[1] , pch=19 )
lines(dgNoha$V4~dgNoha$date_index , col=col.pal[1] , type="l")
points(dgNoha$V5~dgNoha$date_index , col=col.pal[2] , pch=19 )
lines(dgNoha$V5~dgNoha$date_index , col=col.pal[2] , type="l")
points(dgNoha$V6~dgNoha$date_index , col=col.pal[3] , pch=19 )
lines(dgNoha$V6~dgNoha$date_index , col=col.pal[3] , type="l")
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=1 , pch=19 , horiz=TRUE )
legend ("topleft" , legend=c("raw probabilities" , "model predictions") ,  col=1 , bty='n', cex=1 , pch=c(1,19), lty=c(3,1), horiz=TRUE)

##group level plots for kubu
##group level plots for Kubu
plot(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , pch=1 , ylim=c(0,1.1) , xlim=c(0,18) , ylab="freq of behavior in group" , xlab="experimental day" , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all) , main="Kubu" ) 
lines(dgKubu$V1~dgKubu$date_index , col=col.pal[1] , type="l" , lty=3)
points(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , pch=1 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V2~dgKubu$date_index , col=col.pal[2] , type="l" , lty=3)
points(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , pch=1 , cex=3*dgKubu$nobs_group_all/max(dgKubu$nobs_group_all))
lines(dgKubu$V3~dgKubu$date_index , col=col.pal[3] , type="l" , lty=3)
points(dgKubu$V4~dgKubu$date_index , col=col.pal[1] , pch=19 )
lines(dgKubu$V4~dgKubu$date_index , col=col.pal[1] , type="l")
points(dgKubu$V5~dgKubu$date_index , col=col.pal[2] , pch=19 )
lines(dgKubu$V5~dgKubu$date_index , col=col.pal[2] , type="l")
points(dgKubu$V6~dgKubu$date_index , col=col.pal[3] , pch=19 )
lines(dgKubu$V6~dgKubu$date_index , col=col.pal[3] , type="l")
legend ("topright" , legend=c("ch" , "cms" ,"cmt") ,  col=col.pal , bty='n', cex=1 , pch=19 , horiz=TRUE )
legend ("topleft" , legend=c("raw probabilities" , "model predictions") ,  col=1 , bty='n', cex=1 , pch=c(1,19), lty=c(3,1), horiz=TRUE)