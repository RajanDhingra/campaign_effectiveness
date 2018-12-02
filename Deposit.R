#Calling libraries required
library(ggplot2)
library(corrplot)
library('car')      #for vif values
library(tidyverse) # for ggplot, purrr etc.
library(caret)
# short for C_lassification A_nd RE_gression T_raining (confusionMatrix, sample.split)
library(ROCR)
library(reshape2) # for melt()
library(car) # for vif values
library(corrplot) # for correlation plots
library(caTools) # for colAUC
library(DMwR)

#Set the working directory and read the file
setwd("E:/EBAC/FBA/Assignment 2 Rita/bank-additional/bank-additional")
Deposit= read.csv("bank-additional-full.csv")
attach(Deposit)
boxplot(Deposit$age)
plot(D)


Deposit$agecat<-cut(Deposit$age, c(15,25,60,100))
table(Deposit$agecat)
round(prop.table(table(Deposit$agecat)),2)

hist(Deposit$age)
ggplot(Deposit, aes(x=age))+geom_histogram(binwidth=10)
Deposit$Agecat1<-cut(Deposit$age, c(15,25,60,100))
round(prop.table(table(Deposit$Agecat1)),2)
glm.fit_age= glm(formula=y ~ Agecat1, family= binomial, data=Deposit)
summary(glm.fit_age)  

#********Plotting job***************
library(dplyr)
#reorder the table and reset the factor to that ordering
Deposit %>%
  group_by(job) %>%                              # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts) %>%                                # sort by counts
  mutate(Position = factor(job, job)) %>%   # reset factor
  ggplot(aes(x=job, y=counts)) +                 # plot 
  geom_bar(stat="identity")                         # plot histogram

thetable <- within(thetable, 
                   Deposit$job <- factor(Deposit$job, 
                                      levels=names(sort(table(Position), 
                                                        decreasing=TRUE))))
Deposit$job <- factor(Deposit$job, levels = Deposit$job[order(Deposit$val)])
ggplot(Deposit, aes(x = reorder(job, -table(job)[job]))) + geom_bar() +theme_bw()+ order(Deposit$job)
round(prop.table(table(Deposit$job)),1)

table(education)

euribor3m_fact=as.factor(euribor3m)
table(euribor3m_fact, y)
euribor3m_fact1<-cut(Deposit$euribor3m, c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5.1))

#set initial seed
set.seed(123)
#?sample.split
splitData = sample.split(Deposit$y, SplitRatio = 0.7)
head(splitData)

#create train data set
train = Deposit[splitData,]
nrow(train)/nrow(Deposit)

# create test dataset
test = Deposit[!splitData,]
nrow(test)/nrow(Deposit)

# **************Analysing factor : Age ***********************

#understanding the numerical variables
summary(age)
#Age is right hand skew with Mode=31, Median=38 & Mean=40
boxplot(age)
#there are many outliers in the age

glm.fit_age= glm(formula=y ~ Agecat1, family= binomial, data=Deposit)
summary(glm.fit_age)  

#**************Analysing factor : marital********************
plot(marital,y)
table(marital,y)
#divorced & married have a similar % of the deposit rate
# so it's better if you can combine both
Deposit$single=marital
levels(Deposit$single)<- c("N","N", "Y", "unknown")


#**************Analysing factor : Job& education**************
train$education_mod= train$education
levels(train$education_mod)<- c("basic.education","basic.education", "basic.education", "high.school", "basic.education", "university.level", "university.level", "unknown")

train$education_job <- paste0(train$job,train$education_mod)
train$education_job= as.factor(train$education_job)
write.csv(train$education_job, 'education_job.csv')
levels(train$education_job)=c("admin.basic.education","admin.high.school","admin.university.level","others","blue-collarbasic.education","blue-collarhigh.school","others","others","others","others","entrepreneuruniversity.level","others","housemaidbasic.education","others","others","others","others","others","managementuniversity.level","others","retiredbasic.education","others","others","others","others","others","self-employeduniversity.level","others","servicesbasic.education","serviceshigh.school","others","others","others","others","others","others","others","technicianhigh.school","technicianuniversity.level","others","others","others","others","others","others","others","others","others"
)


#applying smote(sampling minority oversampling technique) on the training dataset
trainSpilt= SMOTE(y ~., train, perc.over = 100, perc.under = 200)
table(trainSpilt$y)

plot(trainSpilt$emp.var.rate, trainSpilt$cons.price.idx)
points(trainSpilt$month, cex = .5, col = "dark red")
cor(trainSpilt$emp.var.rate, trainSpilt$cons.price.idx)


#Feature engineering.
trainSpilt$agecat<-cut(trainSpilt$age, c(15,25,60,100))

#Education has no implication
trainSpilt$educationlevel = trainSpilt$education
levels(trainSpilt$educationlevel)<- c("basic.education","basic.education", "basic.education", "high.school", "basic.education", "university.level", "university.level", "university.level")
table(trainSpilt$educationlevel)


glm.fit_train= glm(formula=y ~ ., family= binomial, data=trainSpilt)
summary(glm.fit_train)

#age is not significant.
glm.fit_train1= glm(formula=y ~ .-age, family= binomial, data=trainSpilt)
summary(glm.fit_train1)
#marital, day of week,contct telephone, housing,education, marital are not signficant
glm.fit_train2= glm(formula=y ~ .-age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train2)

#AIC for the model is 8221.1
corrplot(cor(Deposit[sapply(trainSpilt, is.numeric)]), method = "number", type='upper')

#as emp.var.rate is 0.97 correlated with euribor3m so dropping one of them
glm.fit_train3= glm(formula=y ~ .-euribor3m -age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train3)

vif(glm.fit_train3)

#after removing euribor3m now nr.employed becomes insignifant
glm.fit_train4= glm(formula=y ~ .-nr.employed -euribor3m -age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train4)

#poutcome is insignificant too now 
glm.fit_train5= glm(formula=y ~ .-poutcome -nr.employed -euribor3m -age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train5)


#cons. price index & cons. confidence index is also correalted by 0.69
# so dividing both to check

trainSpilt$conf_price = trainSpilt$cons.conf.idx*trainSpilt$cons.price.idx
trainSpilt$conf_norm= (trainSpilt$cons.conf.idx- mean(trainSpilt$cons.conf.idx))/sd(trainSpilt$cons.conf.idx)
trainSpilt$price_norm= (trainSpilt$cons.price.idx- mean(trainSpilt$cons.price.idx))/sd(trainSpilt$cons.price.idx)
trainSpilt$conf_price_norm_add = trainSpilt$conf_norm + trainSpilt$price_norm

#with duration
glm.fit_train6= glm(formula=y ~ . -poutcome -nr.employed -euribor3m -age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train6)
vif(glm.fit_train6)

#without duration
glm.fit_train7= glm(formula=y ~ . -emp.var.rate -previous  -duration -poutcome -nr.employed -euribor3m -age -marital -day_of_week -contact -housing -education -marital, family= binomial, data=trainSpilt)
summary(glm.fit_train7)
vif(glm.fit_train7)

#und
glm.fit_train8= glm(formula=y ~ emp.var.rate, family= binomial, data=trainSpilt)
summary(glm.fit_train8)

glm.fit_train9= glm(formula=y ~ cons.price.idx, family= binomial, data=trainSpilt)
summary(glm.fit_train9)

glm.fit_train10= glm(formula=y ~ cons.price.idx+emp.var.rate, family= binomial, data=trainSpilt)
summary(glm.fit_train10)

glm.fit_train11= glm(formula=y ~ cons.price.idx+emp.var.rate+cons.price.idx/emp.var.rate, family= binomial, data=trainSpilt)
summary(glm.fit_train11)

write.csv(trainSpilt,'trainSpilt.csv')


#now do the prediction
#Confusion Matrix Train 
#reduced the precentage rate to get more less false positive.
trainPredict =predict(glm.fit_train6, newdata=trainSpilt, type='response')
p_class = ifelse(trainPredict>0.5,"yes","no")
confusionMatrix(p_class,trainSpilt$y, positive="yes")

auc = colAUC(trainPredict, trainSpilt$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=1)


#now do the prediction
#Confusion Matrix Train 
#Creation of age category in Train
#reduced the precentage rate to get more less false positive.
#now without the duration

trainPredict =predict(glm.fit_train7, newdata=trainSpilt, type='response')
p_class = ifelse(trainPredict>0.5,"yes","no")
confusionMatrix(p_class,trainSpilt$y, positive="yes")

auc = colAUC(trainPredict, trainSpilt$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=1)

table(agecat,y)

#Prediction on the test

#Removing test with the level of yes
#train = train[train$default!='yes' , ]
test$agecat<-cut(test$age, c(15,25,35,45,55,65,75,85,100))
table(test$agecat,test$y)

testPredict = predict(glm.fit_train6, newdata=test, type = 'response')
testPredict = predict(glm.fit_train7, newdata =test, type = 'response')
p_class2 = ifelse(testPredict > 0.5, "yes","no")
confusionMatrix(p_class2, test$y, positive = "yes")

auc = colAUC(testPredict, test$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=1)




#AUC is coming around 77%
cor(Deposit$cons.conf.idx, Deposit$cons.price.idx)
#so my final set of variables 

#



#checking the education as a factor 
glm.fit_education= glm(formula=y ~ education, family= binomial, data=train)
summary(glm.fit_education)  
#checking the job as a factor 
glm.fit_job= glm(formula=y ~ job, family= binomial, data=train)
summary(glm.fit_job)  

#checking the education as a factor 
glm.fit_education_job= glm(formula=y ~ education_job, family= binomial, data=train)
summary(glm.fit_education_job)  

#checking the education as a factor 
glm.fit_camp= glm(formula=y ~ campaign+previous, family= binomial, data=train)
summary(glm.fit_camp)  

#creating field= campaign+previous & campaign*Previous
train$camp_pre_mul= train$campaign*train$previous
train$camp_pre_add= train$campaign+train$previous
train$camp_pre_div= train$previous/train$campaign

#checking the education as a factor 
glm.fit_camp= glm(formula=y ~previous, family= binomial, data=train)
summary(glm.fit_camp)  
train$pconnected=train$campaign
train$pconnected[train$pdays>30]=0
train$pconnected[train$pdays<=30]=1

train$poutcome_num=train$campaign
train$poutcome_num1[train$poutcome=="failure"]=-1
train$poutcome_num1[train$poutcome=="nonexistent"]=0
train$poutcome_num1[train$poutcome=="success"]=1

train$poutcome_previous= train$poutcome_num1*train$previous
train$poutcome_thistime= train$poutcome_num1*train$campaign


#only previous as a factor is important; even this campign has a negative effect, the previous
#has a AIC of 19314 and z value of 31.67 which is pretty good

#now the current campign has a effect on the dencreasing the percentage.
#so it's better to see if the previous divided by this campign has a positive
glm.fit_camp= glm(formula=y ~previous+campaign+poutcome_thistime, family= binomial, data=train)
summary(glm.fit_camp)  



#only previous as a factor is important; even this campign has a negative effect, the previous
#has a AIC of 19314 and z value of 31.67 which is pretty good

#now the current campign has a effect on the dencreasing the percentage.
#so it's better to see if the previous divided by this campign has a positive
glm.fit_camp= glm(formula=y ~previous+campaign+poutcome, family= binomial, data=train)
summary(glm.fit_camp)  





#**************** Trying to remove the age factor *************
Deposit_filterd = Deposit_total[!age<30,]

#checking the age as a factor 
glm.fit_age= glm(formula=y ~ age
                 , family= binomial, data=Deposit_total)
summary(glm.fit_age)  


# **************Analysing factor : Job ***********************
table(job)
#80% of the people are from admin, bluecollar, technician, service, manager

#Age & Job correlation. Broadly 3 types of ppl student, working & Retired
ggplot(Deposit, aes(x =job, y = age)) + geom_boxplot()




#***************Analysing factor : Marital *******************
table(marital)
#60.5% people are married; 28% people are single; 11% People are divorced

#Age & Marital Correlation
ggplot(Deposit, aes(x =marital, y = age)) + geom_boxplot()



#**************percentage of people having which job ************
tab = with(Deposit, table(job))
round(tab/sum(tab), 3)


#**************percentage of people having which qualification ************
tab = with(Deposit, table(education))
round(tab/sum(tab), 3)

#**********conversion of pdays into the discrete variables*************
Deposit$pdayscontact= ifelse(pdays<30,1,0)
Deposit$pdayscontact=as.numeric(Deposit$pdayscontact)
#Deposit$totalconnection = Deposit$pdayscontact * Deposit$campaign
Deposit$totalconnection = Deposit$pdayscontact * Deposit$campaign
table(Deposit$totalconnection,Deposit$y)
#to know relationship of Connection with the 
data= with(Deposit, table(Deposit$totalconnection, y))

data=table(Deposit$totalconnection,y)
data=data.frame(unclass(data))
data$id=row.name(data)
plot(Deposit$totalconnection,y)


#**************** chi square test ************************************
chisq.test(job,education)
chisq.test(job,marital)
chisq.test(education, marit)

#**************gg plot of the age & those who accepted it ************
plot= ggplot(Deposit,aes(job,y))+geom_point(aes(color=y))+
  geom_smooth(method=lm)+ geom_point(shape=2)+
  labs(y="Campign", 
       x = "Age in years")+ 
  ggtitle("Age & Campign")
print(plot)

plot(education,y)
plot(Deposit_filterd$job,Deposit_filterd$y)




#**************Analysing factor : Job& education**************
train$education_comb=train$education
levels(train$education_comb)<- c("basic.education","basic.education", "basic.education", "high.school", "illiterate", "professional.course", "university.degree", "unknown")
library(forcats)
train$combField <- paste0(train$job,train$education_comb)
levels(fct_infreq(train$combField))
write.csv(train$combField, "test1.csv")
levels(train$combField)<- c("others","admin.high.school","others","admin.university.degree","others","blue-collarbasic.education","blue-collarhigh.school","others","others","others","others","others","others","others","others","others","others","others","others","others","others","others","others","others","others","others","managementuniversity.degree","others","retiredbasic.education","others","others","others","others","others","others","others","others","others","others","others","others","serviceshigh.school","others","others","others","others","others","others","others","others","others","technicianhigh.school","technicianprofessional.course","technicianuniversity.degree","others","others","others","others","others","others","others","others","others","others","others")


#******Analysing factor : Adding the previous & campign **********
train$totalConnection= train$previous+train$campaign

plot(train$y,train$totalConnection)


glm.fit= glm(formula=y ~ job+ education
             , family= binomial, data=train)
summary(glm.fit)

glm.fit= glm(formula=y ~ combField
             , family= binomial, data=train)
summary(glm.fit)

# checking of logistic regression with age & job
glm.fit= glm(formula=y ~ job+marital+age+education+default+housing+loan+contact+
               month+day_of_week +duration+campaign+pdays+previous+poutcome+
               emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed
             , family= binomial, data=train)
summary(glm.fit)
#AIC with all variable coming around 17184 for main data and for train data it's 12019

# checking of logistic regression with age & job
glm.fit2= glm(formula=y ~ job+marital+age+education+ Deposit$pdayscontact
             , family= binomial, data=train)
summary(glm.fit2)
#AIC with all variable coming around 28089 for main data & for train data it's 25955

# checking of logistic regression with age & job
glm.fit3= glm(formula=y ~ job+marital+education+ Deposit$pdayscontact+previous
             , family= binomial, data=train)
summary(glm.fit3)
#AIC with all variable coming around 25955, and for the test data it's 25910
# single element improved AIC  BY 3000 points after addition of previous

# checking of logistic regression with age & job
glm.fit4= glm(formula=y ~ job+marital+education+ pdays+previous+poutcome+contact+month
             +day_of_week+emp.var.rate+cons.price.idx+euribor3m+default
             , family= binomial, data=train)
summary(glm.fit4)
#AIC with all variable coming around 16099

vif(glm.fit4)
#vif is coming high for the emp.var.index.

#removing the vif emp.var.rate from there 

# checking of logistic regression with age & job
glm.fit5= glm(formula=y ~ job+marital+education+ train$totalconnection+previous+poutcome+contact+month+campaign
              +day_of_week+cons.price.idx+euribor3m+default
              , family= binomial, data=train)
summary(glm.fit5)
#AIC Is coming around the 16286 which is in the range required

# checking of logistic regression with combining 
glm.fit6= glm(formula=y ~ job+marital+education+ pdays+previous+poutcome+contact+month+campaign
              +day_of_week+cons.price.idx+euribor3m+default
              , family= binomial, data=train)
summary(glm.fit5)
#AIC Is coming around the 16286 which is in the range required

#Confusion Matrix Train 
#reduced the precentage rate to get more less false positive.
trainPredict =predict(glm.fit5, newdata=train, type='response')
p_class = ifelse(trainPredict>0.2,"yes","no")
confusionMatrix(p_class,train$y, positive="yes")

auc = colAUC(trainPredict, train$y, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)


#Confusion matrix test
testPredict =predict(glm.fit5, newdata=test, type='response')
p_class = ifelse(testPredict>0.2,"yes","no")
confusionMatrix(p_class,test$y, positive='yes')
abline(a=0,b=1,lwd=2,lty=2,col="red")

#auc test
auc_test = colAUC(testPredict, test$y, plotROC = TRUE)
legend(0.1,0.9, round(auc_test,4), title= 'AUC', cex=.5)
abline(a=0,b=1,lwd=2,lty=2,col="red")



corrplot(cor(Deposit[sapply(Deposit, is.numeric)]), method = "number", type='upper')

# based on the correlation plot & the output of regression removing marital;age;
# housing;loan;duration(as per notes);previous.
glm.fit= glm(formula=y ~ job+education+default+contact+
               month+day_of_week +campaign+pdays+poutcome+
               emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed
             , family= binomial, data=Deposit)
summary(glm.fit)
#AIC with all variable coming around 22804

#understanding the outliers in case of age data
quantile(age,c(0.01,0.99))
#only one percentage is above 71 & one percentage is below 23
#filtering the same out
Deposit_filterd = Deposit[!age>71&!age<23,]

glm.fit3= glm(formula=Deposit_filterd$y ~ Deposit_filterd$education
             , family= binomial, data=Deposit)
summary(glm.fit3)



