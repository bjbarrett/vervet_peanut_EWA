##authored by charlotte canteloup

## Set working directory
setwd("XXX")


library(lme4)
library(multcomp)

#### Load data with latency in seconds
data1<-read.csv("latency_success_peanuts.csv",header=T,sep=";",dec=",")
str(data1)
summary(data1)


#### Model GLM to test the effects of group, age, sex and rank on the latency of first success 

# we need to use quasipoisson distribution instead of poisson to deal with over dispersion 
# the dispersion parameter with a poisson distribution is bigger than 1 (residual deviance/(n-p)) where n is nb of observation (here:53) and p nb of parameters (here:4) --> over dispersion

GLM1 <- glm(Latency_success~standardized_rank+Group+Age+Sex,data=data1,family="quasipoisson")
summary(GLM1)
drop1(GLM1,test="Chisq")
summary(glht(GLM1,linfct=mcp(Age="Tukey")))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         9.7801     0.2972  32.902   <2e-16 ***
#  standardized_rank  -0.1103     0.4244  -0.260   0.7968    
#GroupNoha          -0.1583     0.2356  -0.672   0.5073    
#Ageinfant           0.1257     0.3057   0.411   0.6841    
#Agejuvenile        -0.4487     0.2467  -1.819   0.0801 .  
#Sexmale             0.1180     0.2089   0.565   0.5767   

#Df Deviance scaled dev. Pr(>Chi)  
#<none>                 126200                       
#standardized_rank  1   126446      0.0679  0.79448  
#Group              1   127819      0.4464  0.50407  
#Age                2   152922      7.3685  0.02512 *
 # Sex                1   127358      0.3192  0.57210 

#Estimate Std. Error z value Pr(>|z|)  
#infant - adult == 0      0.1257     0.3057   0.411   0.9097  
#juvenile - adult == 0   -0.4487     0.2467  -1.819   0.1605  
#juvenile - infant == 0  -0.5744     0.2460  -2.335   0.0501 .


#### Model GLM to test the effects of group, age, sex and rank on the rate of success of extracting and eating peanuts

# 'Time' is the time each individual had after its first success 
# we put the put the log of Time as an offset in the model to effectively modelling the rate at which each individual solved the task once it had first solved it

GLM2 <- glm(Nb_success~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM2)
drop1(GLM2,test="Chisq")
summary(glht(GLM2,linfct=mcp(Group="Tukey")))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -6.7835     1.2511  -5.422 9.82e-06 ***
#  standardized_rank  -2.6676     0.6668  -4.000 0.000442 ***
#  GroupNoha           2.1475     1.2246   1.754 0.090853 .  
#Ageinfant          -1.7619     0.9830  -1.792 0.084284 .  
#Agejuvenile        -0.6184     0.3063  -2.019 0.053517 .  
#Sexmale             1.1221     0.3150   3.562 0.001393 ** 

#Df Deviance scaled dev.  Pr(>Chi)    
#<none>                 975.25                          
#standardized_rank  1  1726.16     17.8621 2.375e-05 ***
# Group              1  1242.02      6.3457  0.011766 *  
#Age                2  1273.66      7.0984  0.028748 *  
#Sex                1  1512.83     12.7875  0.000349 ***

#Estimate Std. Error z value Pr(>|z|)  
#infant - adult == 0     -1.7619     0.9830  -1.792   0.1555  
#juvenile - adult == 0   -0.6184     0.3063  -2.019   0.0954 .
#juvenile - infant == 0   1.1435     0.9750   1.173   0.4457

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0    2.147      1.225   1.754   0.0795 .


#### Model GLM to test the effects of group, rank, age, sex on the rate of manipulation of peanuts (attempts and success)

GLM3 <- glm(Nb_manip~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM3)
drop1(GLM3,test="Chisq")
summary(glht(GLM3,linfct=mcp(Group="Tukey")))

##  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -5.9760     0.9395  -6.361 8.22e-07 ***
# standardized_rank  -2.2248     0.6534  -3.405  0.00208 ** 
# GroupNoha           1.7028     0.9047   1.882  0.07064 .  
#Ageinfant          -0.7893     0.6542  -1.206  0.23810    
#Agejuvenile        -0.4266     0.3020  -1.413  0.16918    
#Sexmale             0.9308     0.3158   2.947  0.00654 ** 

#Df Deviance scaled dev.  Pr(>Chi)    
#<none>                 1585.7                          
#standardized_rank  1   2452.1     12.8134 0.0003441 ***
# Group              1   1993.7      6.0341 0.0140321 *  
#Age                2   1769.4      2.7167 0.2570825    
#Sex                1   2176.7      8.7402 0.0031126 ** 

#Estimate Std. Error z value Pr(>|z|)
#infant - adult == 0     -0.7893     0.6542  -1.206    0.434
#juvenile - adult == 0   -0.4266     0.3020  -1.413    0.319
#juvenile - infant == 0   0.3627     0.6357   0.571    0.829

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0   1.7028     0.9047   1.882   0.0598 .


#### Model GLM to test effects of sociodemographic factors on nb of successes observed

GLM4 <- glm(Nb_Observations_success~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM4)
drop1(GLM4,test="Chisq")
summary(glht(GLM4,linfct=mcp(Group="Tukey")))

##  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -7.027452   1.696531  -4.142 0.000303 ***
#standardized_rank -1.636513   0.858007  -1.907 0.067167 .  
#GroupNoha          2.785052   1.655672   1.682 0.104076    
#Ageinfant          0.635458   0.588632   1.080 0.289892    
#Agejuvenile       -0.007275   0.418324  -0.017 0.986253    
#Sexmale            0.806464   0.436919   1.846 0.075914 .  

#Df Deviance scaled dev. Pr(>Chi)   
#<none>                 4262.9                        
#standardized_rank  1   4981.1      3.9279 0.047490 * 
#  Group              1   5700.3      7.8616 0.005049 **
#  Age                2   4524.2      1.4288 0.489486   
#Sex                1   4897.9      3.4727 0.062388 .

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0    2.785      1.656   1.682   0.0925 .


#### Model GLM to test effects of sociodemographic factors on nb of manipulations observed

GLM5 <- glm(Nb_Observations_manip~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM5)
drop1(GLM5,test="Chisq")
summary(glht(GLM5,linfct=mcp(Group="Tukey")))

##  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -6.19227    1.40581  -4.405 0.000151 ***
#standardized_rank -1.56485    0.82683  -1.893 0.069180 .  
#GroupNoha          2.42062    1.35903   1.781 0.086145 .  
#Ageinfant          0.56736    0.57790   0.982 0.334928    
#Agejuvenile       -0.01234    0.40618  -0.030 0.975984    
#Sexmale            0.79337    0.42227   1.879 0.071105 .  

#Df Deviance scaled dev. Pr(>Chi)   
#<none>                 6599.3                        
#standardized_rank  1   7679.7      3.8553 0.049590 * 
# Group              1   8667.5      7.3801 0.006595 **
#Age                2   6935.7      1.2005 0.548661   
#Sex                1   7608.5      3.6012 0.057738 . 

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0    2.421      1.359   1.781   0.0749 .


#### Model GLM to test effects of sociodemographic factors on nb of times being observed when succeeding

GLM6 <- glm(Nb_being_obs_success~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM6)
drop1(GLM6,test="Chisq")
summary(glht(GLM6,linfct=mcp(Group="Tukey")))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -6.0644     1.6530  -3.669  0.00106 ** 
#  standardized_rank  -3.6099     0.6477  -5.573 6.56e-06 ***
#  GroupNoha           2.7711     1.6366   1.693  0.10192    
#Ageinfant          -1.4988     0.8749  -1.713  0.09815 .  
#Agejuvenile        -0.9069     0.2910  -3.116  0.00431 ** 
#  Sexmale             1.1531     0.2745   4.200  0.00026 ***

#Df Deviance scaled dev.  Pr(>Chi)    
#<none>                 2049.2                          
#standardized_rank  1   5536.0      36.822 1.294e-09 ***
# Group              1   2801.0       7.939  0.004837 ** 
#Age                2   3167.2      11.807  0.002730 ** 
#Sex                1   3705.2      17.489 2.890e-05 ***

#Estimate Std. Error z value Pr(>|z|)   
#infant - adult == 0     -1.4988     0.8749  -1.713  0.18288   
#juvenile - adult == 0   -0.9069     0.2910  -3.116  0.00406 **
# juvenile - infant == 0   0.5919     0.8739   0.677  0.76301 

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0    2.771      1.637   1.693   0.0904 .


#### Model GLM to test effects of sociodemographic factors on nb of times being observed when manipulating

GLM7 <- glm(Nb_being_obs_manip~standardized_rank+Group+Age+Sex+offset(log(Time)),data=data1,family="quasipoisson")
summary(GLM7)
drop1(GLM7,test="Chisq")
summary(glht(GLM7,linfct=mcp(Group="Tukey")))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -5.3330     1.3058  -4.084 0.000354 ***
#  standardized_rank  -3.3008     0.6494  -5.083 2.44e-05 ***
#  GroupNoha           2.4424     1.2854   1.900 0.068145 .  
#Ageinfant          -0.6773     0.6305  -1.074 0.292274    
#Agejuvenile        -0.7310     0.2860  -2.556 0.016538 *  
#  Sexmale             1.0235     0.2820   3.629 0.001171 ** 

#Df Deviance scaled dev.  Pr(>Chi)    
#<none>                 3219.5                          
#standardized_rank  1   7915.3     30.0598 4.189e-08 ***
# Group              1   4551.7      8.5281 0.0034971 ** 
#Age                2   4291.7      6.8635 0.0323302 *  
#Sex                1   5243.2     12.9549 0.0003191 ***

#Estimate Std. Error z value Pr(>|z|)  
#infant - adult == 0    -0.67727    0.63054  -1.074    0.515  
#juvenile - adult == 0  -0.73105    0.28603  -2.556    0.026 *
# juvenile - infant == 0 -0.05377    0.62388  -0.086    0.996

#Estimate Std. Error z value Pr(>|z|)  
#Noha - Kubu == 0    2.442      1.285     1.9   0.0574 .

