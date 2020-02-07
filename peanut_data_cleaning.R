####Vervet Peanut Data EWA Anaylysis 
####10 Experiments in 2 groups, each tesst session is 1-2 hours, opem diffusiom, every success and attempt recorded
####3 techniques: crack with mouth from side; crack with mouth from top; crack with hand
####Info on who manipulated and who attended
####Strategies to Analyze: 1_ Frequency dependence; 2_PayoffBias; 3) Rank Bias; 4) Kin Bias; 5) Sex Bias; 
#### Interested in sex and age variation
#### load packages + data
library(lubridate)
library(RColorBrewer)
#do <- read.csv(text=getURL("https://raw.githubusercontent.com/eslrworkshop/resources-2019/master/day3/panama_data_14days.csv"), header=T)
dkubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Kubu_time_ordered.csv", header=T, sep=";")
dnoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/raw_data/Data_peanuts_exp_Noha_time_ordered.csv", header=T, sep=";")
dkubu <- dkubu[1:304,] #drop NA on tail of datasheet
d <- dnoha #only analyze noha
d$ID_index <- as.integer(d$ID_actor) #index for all 30 individuals, need to see if this matches rank + sex files

###get rows where behaviors of interest are present
d <- droplevels(subset(dnoha, subset = Event %in% c('ach','acms','acmt','sch','scms','scmt') ))

###do individuals in raw data and new data match up?-- no, need to account for when processing data for social info, not all individuals use the 3 proposed techniques
isTRUE(length(unique(d$ID_actor))==length(unique(dnoha$ID_actor)))

d$ID_actor_index <- as.integer(d$ID_actor) #unique integer for each actor vervet that used one of 3 techniques

d$timestamp <- with(d, dmy(Date) + hms(Time)) #apply date and time timestamp to each observation
d[order(d$timestamp),]#order by timestap
d$obs_index <- seq(1:nrow(d)) #unique sequential value to each row after ordering dataframe by timestamp
d$succeed <- ifelse(d$Event=='sch'| d$Event=='scms' | d$Event=='scmt', 1 , 0) # column of 1/0 succeed for behavior
d$technique <- sub(".", "", d$Event)   #trim 1st charachter 
d$technique_index <- as.integer(as.factor(d$technique))  #technique index
d$date_index <- as.integer(as.factor(date(d$timestamp)))

##create dot plot huge
col_pal <- brewer.pal(n=3, name='Set2') #color pallette, but spelled right

pdf('noha_peanut_raw_dotplot.pdf' , width=150 , height = 10 )

plot(ID_actor_index ~ obs_index , data=d , col=col_pal[d$technique_index] , pch=ifelse(d$succeed==1, 19, 1), cex=0.4, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index")
axis(2, at=1:length(unique(d$ID_actor)), s labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
#lines between each date of testing
for (i in 1:max(d$date_index)){
  abline(v=min(d$obs_index[d$date_index==i]))
}
legend("topleft", legend=c("Success", "Attempt") , pch=c s(19,1), cex=1 , bty='n' )
legend("topright", legend=c("ch", "cms" , "cmt"),
         col=col_pal , pch=15 , cex=1 ,  bty='n')

dev.off()

##multi plots per day
pdf('noha_peanut_raw_dotplot_multipanel.pdf' , width=30 , height = 20 )

par(mfrow=c(11,1) , mar=c(1,0,1,0) , oma=c(2,3,2,2))
plot(ID_actor_index ~ obs_index , data=d[d$date_index==1,] , col=col_pal[d$technique_index] , pch=ifelse(d$succeed==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index" , main=min(date(d$timestamp[d$date_index==1,])))
axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
legend("topleft", legend=c("Success", "Attempt") , pch=c(19,1), cex=1 , bty='n' )
legend("topright", legend=c("ch", "cms" , "cmt"),
       col=col_pal ,pch=15 , cex=1 ,  bty='n')

for(i in 2:max(d$date_index)){
  plot(ID_actor_index ~ obs_index , data=d[d$date_index==i,] , col=col_pal[d$technique_index] , pch=ifelse(d$succeed==1, 19, 1), cex=0.5, yaxt='n' , ylab="Individual ID" , xlab="Peanut Index", main=min(date(d$timestamp[d$date_index==i,])))
  axis(2, at=1:length(unique(d$ID_actor)), labels=sort(unique(d$ID_actor)), las=2 , cex.axis=0.75)
}

dev.off()





