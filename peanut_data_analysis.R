####Vervet Peanut Data EWA Anaylysis 
####10 Experiments in 2 groups, each tesst session is 1-2 hours, opem diffusiom, every success and attempt recorded
####3 techniques: crack with mouth from side; crack with mouth from top; crack with hand
####Info on who manipulated and who attended
####Strategies to Analyze: 1_ Frequency dependence; 2_PayoffBias; 3) Rank Bias; 4) Kin Bias; 5) Sex Bias; 
#### Interested in sex and age variation

#### load packages + data
#do <- read.csv(text=getURL("https://raw.githubusercontent.com/eslrworkshop/resources-2019/master/day3/panama_data_14days.csv"), header=T)
dkubu <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Data_peanuts_exp_Kubu_time_ordered.csv", header=T, sep=";")
dnoha <- read.csv("~/Dropbox/Vervets/vervet_peanut_EWA/Data_peanuts_exp_Noha_time_ordered.csv", header=T, sep=";")
dkubu <- dkubu[1:304,] #drop NA on tail of datasheet
str(dkubu)
str(dnoha)
d <- dnoha #only analyze noha
d$ID_index <- as.integer(d$ID_actor) #add actor index

