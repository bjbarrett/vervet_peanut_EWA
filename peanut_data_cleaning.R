####Vervet Peanut Data EWA Anaylysis 
####10 Experiments in 2 groups, each tesst session is 1-2 hours, opem diffusiom, every success and attempt recorded
####3 techniques: crack with mouth from side; crack with mouth from top; crack with hand
####Info on who manipulated and who attended
####Strategies to Analyze: 1_ Frequency dependence; 2_PayoffBias; 3) Rank Bias; 4) Kin Bias; 5) Sex Bias; 
#### Interested in sex and age variation
#### load packages + data
rm(list = ls())
library(lubridate)
library(RColorBrewer)
library(beepr)
library(gtools)
#do <- read.csv(text=getURL("https://raw.githubusercontent.com/eslrworkshop/resources-2019/master/day3/panama_data_14days.csv"), header=T)
dkubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Kubu_time_ordered.csv", header=T, sep=";")
dnoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Noha_time_ordered.csv", header=T, sep=";")
ILVKubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/ILV_Kubu_2018.csv", header=T, sep=",")
ILVNoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/ILV_Noha_2018.csv", header=T, sep=",")
KinNoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/kinship_matrix_noha_2018.csv", header=T, sep=",")
KinKubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/kinship_matrix_kubu_2018.csv", header=T, sep=",") #need to trncate still

length(unique(dnoha$ID_actor))
length(unique(dkubu$ID_actor))
length(unique(ILVNoha$ID_actor))
length(unique(ILVKubu$ID_actor))
length(unique(KinNoha$X))
length(unique(KinKubu$X))
# 44 individuals in both datasets in focal entry, 52 in ILV and Kin columns
sort(unique(dnoha$ID_actor))
sort(unique(ILVNoha$ID_actor))
sort(unique(dkubu$ID_actor))
sort(unique(ILVNoha$ID_actor))
##create master ILV sheet
ILVKubu$ID_kubu_index <- as.integer(ILVKubu$ID_actor) #for kubu graphs
ILVNoha$ID_noha_index <- as.integer(ILVNoha$ID_actor) #for noha graphs
ILVKubu$group <- "Kubu"
ILVNoha$group <- "Noha"
ILV <- as.data.frame(smartbind(ILVKubu,ILVNoha))
ILV$ID_actor <- as.character(ILV$ID_actor)
ILV$ID_all_index <- as.integer(as.factor(ILV$ID_actor)) #index across all possible individuals, use for social info dataframe
##merge group data and merge ILV 
dkubu$ID_Attending11 <- NA #add column so merge correctly
d <- rbind(dkubu,dnoha)
d <- merge(d,ILV, by="ID_actor")
d$male <- ifelse(d$Sex=="M" , 1 , 0)
d$female <- ifelse(d$Sex=="F" , 1 , 0)
d$adult <- ifelse(d$Age=="A" , 1 , 0)

###get rows where behaviors of interest are present
d <- droplevels(subset(d, subset = Event %in% c('ach','acms','acmt','sch','scms','scmt') ))

###do individuals in raw data and new data match up?-- no, need to account for when processing data for social info, not all individuals use the 3 proposed techniques
d$ID_actor <- as.character(d$ID_actor)
d$ID_actor_index <- as.integer(as.factor(d$ID_actor)) #index across all foraging individuals
d$timestamp <- with(d, dmy(Date) + hms(Time)) #apply date and time timestamp to each observation
d <- d[order(d$timestamp),] #order by timestap
d$obs_index <- seq(1:nrow(d)) #unique sequential value to each row after ordering dataframe by timestamp
d$succeed <- ifelse(d$Event=='sch'| d$Event=='scms' | d$Event=='scmt', 1 , 0) # column of 1/0 succeed for behavior
d$technique <- sub(".", "", d$Event)   #trim 1st charachter 
d$technique_index <- as.integer(as.factor(d$technique))  #technique index
d$date_index <- as.integer(as.factor(date(d$timestamp)))

# ##create dot plot huge
# col_pal <- brewer.pal(n=3, name='Set2') #color pallette, but spelled right
# 
# pdf('noha_peanut_raw_dotplot.pdf' , width=150 , height = 10 )
# 
# plot(ID_actor_index ~ obs_index , data=d , col=col_pal[d$technique_index] , pch=ifelse(d$succeed==1, 19, 1), cex=0.4, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index")
# axis(2, at=1:length(unique(d$ID_actor)),labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
# #lines between each date of testing
# for (i in 1:max(d$date_index)){
#   abline(v=min(d$obs_index[d$date_index==i]))
# }
# legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
# legend("topright", legend=c("ch", "cms" , "cmt"),
#          col=col_pal , pch=15 , cex=1 ,  bty='n')
# 
# dev.off()
# 
# ##multi plots per day
# 
# ###correct multipanel
# pdf('noha_peanut_raw_dotplot_multipanel.pdf' , width=48 , height = 24 )
# 
# par(mfrow=c(11,1) , mar=c(1,0,1,0) , oma=c(2,3,2,3))
# plot(ID_actor_index ~ obs_index , data=d[d$date_index==1,] , col=col_pal[d[d$date_index==1,]$technique_index] , pch=ifelse(d[d$date_index==1,]$succeed==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index" , main=min(date(d$timestamp[d$date_index==1,])) , ylim=c(1,max(d$ID_actor_index)))
# axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
# legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
# legend("topright", legend=c("ch", "cms" , "cmt") , col=col_pal ,pch=15 , cex=1 ,  bty='n')
# 
# for(i in 2:max(d$date_index)){
#   plot(ID_actor_index ~ obs_index , data=d[d$date_index==i,] , col=col_pal[d[d$date_index==i,]$technique_index] , pch=ifelse(d[d$date_index==i,]$succeed ==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index", main=min(date(d$timestamp[d$date_index==i,])) , ylim=c(1,max(d$ID_actor_index)))
#   axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
#   legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
#   legend("topright", legend=c("ch", "cms" , "cmt") , col=col_pal ,pch=15 , cex=1 ,  bty='n')
#   }
# 
# dev.off()

#create foraging bout for each time an individual updates personal info // this is time series that is looped over
d$forg_bout <- rep(0,nrow(d))
ff <- rep(0,length(unique(d$ID_actor)))
for (r in 1:nrow(d)) {
  for(i in 1:length(unique(d$ID_actor))) {
    if( d[r,"ID_actor_index"]==i){
      ff[i] <-ff[i] + 1
      d$forg_bout[r] <- ff[i]
    }
  }
}
beep(5)


####Create Blank Social Matrix to Populate at each timestep

## make an array as wide as the number of foraging individuals and long as the number of obsetrvations // think about both groups later
o_freq <- o_sex <- o_fem <- array(0 , dim=c( nrow(d) , length(unique(ILV$ID_actor)) , max(d$technique_index) ) ) ##sum values where we tally up ones
o_pay <- o_rank <- o_kin  <- array(NA , dim=c( nrow(d) , length(unique(ILV$ID_actor)) , max(d$technique_index) ) ) ##mean or median values, remove NaNs later
ILV <- ILV[order(ILV$ID_actor),] #order by ID_actor 

for( nobs in 1:nrow(d) ){
  for (i in 1:nrow(ILV)){
    if (
           isTRUE(ILV[i,1]==d[nobs,"ID_attending1"] ) || isTRUE(ILV[i,1]==d[nobs,"ID_attending2"] ) ||
           isTRUE(ILV[i,1]==d[nobs,"ID_attending3"] ) || isTRUE(ILV[i,1]==d[nobs,"ID_attending4"] ) ||
           isTRUE(ILV[i,1]==d[nobs,"ID_attending5"] ) || isTRUE(ILV[i,1]==d[nobs,"ID_attending6"] ) ||
           isTRUE(ILV[i,1]==d[nobs,"ID_attending7"] ) || isTRUE(ILV[i,1]==d[nobs,"ID_attending8"] ) ||
           isTRUE(ILV[i,1]==d[nobs,"ID_attending9"] ) || isTRUE(ILV[i,1]==d[nobs,"ID_Attending10"] ) ||
           isTRUE(ILV[i,1]==d[nobs,"ID_Attending11"] ) #case difference in 10 and 11 not a mistake
           #don't use grepl if there are partial matches i.e. XIAN and XIA
         ){
      o_freq[nobs,i,] <- 0 #assigns a 0 to all options if individual i is observing foraging bout nobs, ok for sums
      o_freq[nobs,i,d$technique_index[nobs]] <- 1 #assigns a 1 to observed option for individual i is observing foraging bout nobs
      o_pay[nobs,i,d$technique_index[nobs]] <- d$succeed[nobs] #adds a 1 to observations of a technique where a success was observed, 0 where failure with tecnique
      #fix below when multiple groups in dataset to adjust max in both groups
      o_rank[nobs,i,d$technique_index[nobs]] <-  1/d$Rank[nobs] 
      # o_rank[nobs,i,d$technique_index[nobs]] <- ( (length(unique(ILVNoha$ID_actor)) + 1 - d$Rank[nobs] ) ) /  length(unique(ILVNoha$ID_actor)) #assigns values to each actor--1 is highest individual, close to zero is no individual
      o_sex[nobs,i,] <- 0 # this works in 1/0 cases when there arent all zeros
      o_sex[nobs,i,d$technique_index[nobs]]  <- ifelse(d$Sex[nobs]==ILV$Sex[i] , 1 , 0) # if sex at observation is same as sex of audience member, give 1, otherwise NA
      #for NA colums when calculating means sum w/in category over sums across all categories
      o_fem[nobs,i,] <- 0 # this works in 1/0 cases when there arent all zeros
      o_fem[nobs,i,d$technique_index[nobs]]  <- ifelse(d$Sex[nobs]=="F" , 1 , 0) #copy only females
      #o_kin[nobs,i,d$technique_index[nobs]] <- KinNoha[i, 1 + d$ID_actor_index[nobs]] #input r value
      o_kin[nobs,i,d$technique_index[nobs]] <- ifelse(ILV$group[i]=="Noha" , KinNoha[min(ILV$ID_noha_index[ILV$ID_all_index==i]), 1 + d$ID_noha_index[nobs]] , KinKubu[min(ILV$ID_kubu_index[ILV$ID_all_index==i]), 1 + d$ID_kubu_index[nobs]] )#input r value // this is crazy complicated, do an edge list in future
     }	
  }
}
beep(4)



# o_sex[1:20,,2]
# o_freq[1:20,,2]
# o_pay[1:20,,2]
# o_kin[1:20,,2]
# o_kin[1:50,7,]


#add social info into dataframe

win_width <- 30*60 #social info memory window in seconds (num_min*60secs)


for (nobs in 1:nrow(d)){
  #zz<-min(d$obs_index[d$Date==d$Date[nobs]]) #starts on the same day and counts from there
  zz <- min(d$obs_index[as.numeric(as.duration(d$timestamp[nobs] - d$timestamp)) <= win_width ]) #what is minimal value or earliest observation that occured within the window width

  d$freq1[nobs] <- sum( o_freq[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] )
  d$freq2[nobs] <- sum( o_freq[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] ) 
  d$freq3[nobs] <- sum( o_freq[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] ) 
  d$fem1[nobs] <- sum( o_fem[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] )
  d$fem2[nobs] <- sum( o_fem[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] ) 
  d$fem3[nobs] <- sum( o_fem[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] ) 
  d$sex1[nobs] <- sum( o_sex[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] )
  d$sex2[nobs] <- sum( o_sex[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] ) 
  d$sex3[nobs] <- sum( o_sex[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] ) 
  d$pay1[nobs] <- mean( o_pay[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] , na.rm = TRUE)
  d$pay2[nobs] <- mean( o_pay[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] , na.rm = TRUE) 
  d$pay3[nobs] <- mean( o_pay[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] , na.rm = TRUE) 
  d$kin1[nobs] <- mean( o_kin[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] , na.rm = TRUE)
  d$kin2[nobs] <- mean( o_kin[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] , na.rm = TRUE) 
  d$kin3[nobs] <- mean( o_kin[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] , na.rm = TRUE) 
  d$rank1[nobs] <- mean( o_rank[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 1 ] , na.rm = TRUE)
  d$rank2[nobs] <- mean( o_rank[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 2 ] , na.rm = TRUE) 
  d$rank3[nobs] <- mean( o_rank[ zz : (d$obs_index[nobs] - 1) , d$ID_all_index[nobs] , 3 ] , na.rm = TRUE) 
}

beep(2)

#get rid of NaNs and make value zero so it does not affect behavior
d$pay3[is.nan(d$pay3)] <- 0
d$pay2[is.nan(d$pay2)] <- 0
d$pay1[is.nan(d$pay1)] <- 0

d$kin3[is.nan(d$kin3)] <- 0
d$kin2[is.nan(d$kin2)] <- 0
d$kin1[is.nan(d$kin1)] <- 0

d$rank3[is.nan(d$rank3)] <- 0
d$rank2[is.nan(d$rank2)] <- 0
d$rank1[is.nan(d$rank1)] <- 0

write.csv(d,"Peanut_Vervet_30min.csv")
