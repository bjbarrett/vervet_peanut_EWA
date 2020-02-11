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
#do <- read.csv(text=getURL("https://raw.githubusercontent.com/eslrworkshop/resources-2019/master/day3/panama_data_14days.csv"), header=T)
dkubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Kubu_time_ordered.csv", header=T, sep=";")
dnoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Noha_time_ordered.csv", header=T, sep=";")
dkubu <- dkubu[1:304,] #drop NA on tail of datasheet
ILVKubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/ILV_Kubu_2018.csv", header=T, sep=",")
ILVNoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/ILV_Noha_2018.csv", header=T, sep=",")
KinNoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/kinship_matrix_noha_trunc_2018.csv", header=T, sep=",")
KinKubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/kinship_matrix_kubu_2018.csv", header=T, sep=",") #need to trncate still
#merge ILV to Data
dnoha  <- merge(dnoha,ILVNoha, by="ID_actor")
dkubu  <- merge(dkubu,ILVKubu, by="ID_actor")

length(unique(dnoha$ID_actor))
length(unique(dkubu$ID_actor))
sort(unique(dnoha$ID_actor))
sort(unique(ILVNoha$ID_actor))
sort(unique(dkubu$ID_actor))
sort(unique(ILVNoha$ID_actor))

d <- dnoha #only analyze noha
d$ID_all_index <- as.integer(d$ID_actor) #index for all 30 individuals, need to see if this matches rank + sex files
d$male <- ifelse(d$Sex=="M" , 1 , 0)
d$female <- ifelse(d$Sex=="F" , 1 , 0)
d$adult <- ifelse(d$Age=="A" , 1 , 0)

###get rows where behaviors of interest are present
d <- droplevels(subset(dnoha, subset = Event %in% c('ach','acms','acmt','sch','scms','scmt') ))

###do individuals in raw data and new data match up?-- no, need to account for when processing data for social info, not all individuals use the 3 proposed techniques
isTRUE(length(unique(d$ID_actor))==length(unique(dnoha$ID_actor)))

d$ID_actor_index <- as.integer(d$ID_actor) #unique integer for each actor vervet that used one of 3 techniques

d$timestamp <- with(d, dmy(Date) + hms(Time)) #apply date and time timestamp to each observation
d <- d[order(d$timestamp),] #order by timestap
d$obs_index <- seq(1:nrow(d)) #unique sequential value to each row after ordering dataframe by timestamp
d$succeed <- ifelse(d$Event=='sch'| d$Event=='scms' | d$Event=='scmt', 1 , 0) # column of 1/0 succeed for behavior
d$technique <- sub(".", "", d$Event)   #trim 1st charachter 
d$technique_index <- as.integer(as.factor(d$technique))  #technique index
d$date_index <- as.integer(as.factor(date(d$timestamp)))

##create dot plot huge
col_pal <- brewer.pal(n=3, name='Set2') #color pallette, but spelled right

pdf('noha_peanut_raw_dotplot.pdf' , width=150 , height = 10 )

plot(ID_actor_index ~ obs_index , data=d , col=col_pal[d$technique_index] , pch=ifelse(d$succeed==1, 19, 1), cex=0.4, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index")
axis(2, at=1:length(unique(d$ID_actor)),labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
#lines between each date of testing
for (i in 1:max(d$date_index)){
  abline(v=min(d$obs_index[d$date_index==i]))
}
legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
legend("topright", legend=c("ch", "cms" , "cmt"),
         col=col_pal , pch=15 , cex=1 ,  bty='n')

dev.off()

##multi plots per day

###correct multipanel
pdf('noha_peanut_raw_dotplot_multipanel.pdf' , width=48 , height = 24 )

par(mfrow=c(11,1) , mar=c(1,0,1,0) , oma=c(2,3,2,3))
plot(ID_actor_index ~ obs_index , data=d[d$date_index==1,] , col=col_pal[d[d$date_index==1,]$technique_index] , pch=ifelse(d[d$date_index==1,]$succeed==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index" , main=min(date(d$timestamp[d$date_index==1,])) , ylim=c(1,max(d$ID_actor_index)))
axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
legend("topright", legend=c("ch", "cms" , "cmt") , col=col_pal ,pch=15 , cex=1 ,  bty='n')

for(i in 2:max(d$date_index)){
  plot(ID_actor_index ~ obs_index , data=d[d$date_index==i,] , col=col_pal[d[d$date_index==i,]$technique_index] , pch=ifelse(d[d$date_index==i,]$succeed ==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index", main=min(date(d$timestamp[d$date_index==i,])) , ylim=c(1,max(d$ID_actor_index)))
  axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
  legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
  legend("topright", legend=c("ch", "cms" , "cmt") , col=col_pal ,pch=15 , cex=1 ,  bty='n')
  }

dev.off()

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
o_freq <- array(0 , dim=c( nrow(d) , length(unique(d$ID_actor)) , max(d$technique_index) ) )
o_pay <- o_rank <- o_kin <- o_sex <- array(NA , dim=c( nrow(d) , length(unique(d$ID_actor)) , max(d$technique_index) ) )

# o_age <- o_coho <- o_kin <- array(NA,dim=c(nrow(d),length(unique(d$mono_index)),6 ))

foc <- data.frame("ID_actor" = as.vector(sort(unique(d$ID_actor))), "ID_actor_index" = as.integer(as.factor(as.vector(sort(unique(d$ID_actor))))))
foc <- merge (foc,ILVNoha)

#double check code
for( nobs in 1:nrow(d) ){
  for (i in 1:nrow(foc)){
    if (
          grepl(foc[i,1],d[nobs,"ID_attending1"])==TRUE || grepl(foc[i,1],d[nobs,"ID_attending2"])==TRUE ||
           grepl(foc[i,1],d[nobs,"ID_attending3"])==TRUE || grepl(foc[i,1],d[nobs,"ID_attending4"])==TRUE ||
           grepl(foc[i,1],d[nobs,"ID_attending5"])==TRUE || grepl(foc[i,1],d[nobs,"ID_attending6"])==TRUE ||
           grepl(foc[i,1],d[nobs,"ID_attending7"])==TRUE || grepl(foc[i,1],d[nobs,"ID_attending8"])==TRUE ||
           grepl(foc[i,1],d[nobs,"ID_attending9"])==TRUE || grepl(foc[i,1],d[nobs,"ID_Attending10"])==TRUE ||
           grepl(foc[i,1],d[nobs,"ID_Attending11"])==TRUE #case difference in 10 and 11 not a mistake
         ){
      o_freq[nobs,i,] <- 0 #assigns a 0 to all options if individual i is observing foraging bout nobs
      o_freq[nobs,i,d$technique_index[nobs]] <- 1 #assigns a 1 observed option for individual i is observing foraging bout nobs
      o_pay[nobs,i,d$technique_index[nobs]] <- d$succeed[nobs] #adds a 1 to observations of a technique where a success was observed, 0 where failure with tecnique
      #fix below when multiple groups in dataset to adjust max in both groups
      #o_rank[nobs,i,d$technique_index[nobs]] <- 0
      o_rank[nobs,i,d$technique_index[nobs]] <- ( (length(unique(ILVNoha$ID_actor)) + 1 - d$Rank[nobs] ) ) /  length(unique(ILVNoha$ID_actor)) 
      #o_sex[nobs,i,] <- 0 # this works in 1/0 cases when there arent all zeros
      o_sex[nobs,i,d$technique_index[nobs]]  <- ifelse(d$Sex[nobs]==foc$Sex[i] , 1 , NA) # if sex at observation is same as sex of audience member, give 1, otherwise NA
      #for NA colums when calculating means sum w/in category over sums across all categories
      o_kin[nobs,i,d$technique_index[nobs]] <- KinNoha[i, 1 + d$ID_actor_index[nobs]]
    }	
  }
}
beep(4)


o_sex[1:20,,2]
o_freq[1:20,,2]
o_pay[1:20,,2]
o_kin[1:20,,2]

