# Increase the Campaign Effectiveness
This study is of a Portuguese banking institution’s telemarketing campaign wherein our aim is to build a predictive model that will determine the major contributors to customers investing in term-deposits.

Determining these factors will help the banking institution to take actionable steps and maximize the probability of reaching the right clients. 

The analytical model will increase campaign efficiency by targeting prospective clients effectively with the help of logistic regression.

The data set has 41,188 observations and 21 variables. There are 10 continuous variables and 10 categorical variables. The target response (y) is a binary response indicating whether the client subscribed to a term deposit or not. ‘Yes’ indicates that the client has subscribed to a term deposit and ‘No’ indicates that the client did not subscribe to a term deposit.

As in our dataset target value ‘y’ has ~89 % of the cases as ‘no’. So, taking observed scenarios in same proportion will lead to inefficiency. Thus, we perform sampling techniques by taking appropriate proportion of major and minor outcomes. 

Amongst over sampling, under sampling, mix sampling and SMOTE, we are using SMOTE, which generates some new examples of the minority class using the nearest neighbours of these cases sequentially. 

The majority class examples are also under-sampled, leading to a more balanced dataset.
