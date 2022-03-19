library(dplyr)
install.packages("pylr")

library(readxl)
library(pylr)

setwd("D:/jb/Learning/SimpliLearn/Learning/R/Project~/Project/Projects for Submission/Insurance/Insurance")
getwd()


#1 ques
#The committee is interested to know each field of the data collected through descriptive analysis
#to gain basic insights into the data set and to prepare for further analysis.
Insurance <- read.csv("D:/jb/Learning/SimpliLearn/Learning/R/Project~/Project/Projects for Submission/Insurance/Insurance/SwedishMotorInsurance.csv")
summary(Insurance)


#2 ques
# The total value of payment by an insurance company is an important factor to be monitored.
#So the committee has decided to find whether this payment is related to number of claims 
#and the number of insured policy years. They also want to visualize the results for better understanding.
lm1 <-lm(Insurance$Payment~Insurance$Claims+Insurance$Insured)
summary(lm1)



#3ques
#The committee wants to figure out the reasons for insurance payment increase and decrease.
#So they have decided to find whether distance, location, bonus, make, and insured amount or 
#claims are affecting the payment or all or some of these are affecting it. 
#Independent variable: insured, claims, make, bonus, zone, and kilometersb 
# Dependent variable: payment
lm2<-lm(Insurance$Payment~.,data=Insurance)
summary(lm2)


#4ques
#The insurance company is planning to establish a new branch office, so they are interested 
#to find at what location, kilometer, and bonus level their insured amount, claims, 
#and payment get increased.  
grupzone<-apply(Insurance[,c(5,6,7)], 2, function(x) tapply(x, Insurance$Zone, mean)) 
grupzone


# Zone 4 has the highest number of claims, and thus payment as well. 
# Zones 1-4 have more insured years, claims, and payments.  
grupkil<-apply(Insurance[,c(5,6,7)],2,function(x)tapply(x,Insurance$Kilometres,mean))
grupkil



# Kilometer group 2 has the maximum payments. Though the insured number of years is lesser than kilometre 1, the claims and payments are higher for group 2
grupbon<-apply(Insurance[,c(5,6,7)],2,function(x)tapply(x,Insurance$Bonus,mean))
grupbon


#The committee wants to understand what affects their claim rates so as to decide the right
#premiums for a certain set of situations. Hence, they need to find whether the insured 
#amount, zone, kilometer, bonus, or make affects the claim rates and to what extent. 
reg<-lm(Claims~Kilometres+Zone+Bonus+Make+Insured,data=Insurance)
summary(reg)

#Dependent variable: claims Independent variable: kilometres, zone, bonus, make, and insured 
#The results provides the intercept and estimated value and this in turn shows
#that all the p values of independent variables, such as kilometres, zone, bonus, make, and 
#insured are highly significant and are making an impact on the claims. 