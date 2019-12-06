###################Assignment 1###################

#load the dataset
data.ZK = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")



# activate packages and set working directory
library(psych)
library(lm.beta)
library(dplyr)
library(gsheet)
library(car)
library(ggplot2)
library(rgl)
source("GraphPlot.R")


options(digits = 6)

#####check and clean the dataset #####
# structure and describe the data set to check if there any missing or impossible values 
str(data.ZK) 


describe(data.ZK) #check the mins and max for to low or to high values. 
# in the Variable STAI_cat is one to low value --> the ID will be completely excluded to clean the dataset. 
# also there is one suspicious value regarding ID_15, where a man with an age of 42 weights only around 39 kilo. But since this is still possible I will not exclude this subject from the dataset. 

#clean the data -> delete ID18 
clean.data.ZK<-data.ZK[!data.ZK$STAI_trait < 20,]


#model1: it contains age and sex as predictors for experienced perioperativ pain
clean.data.ZK %>%  
ggplot() +  
aes(x=age) +
geom_histogram(bins=30)

# error using bins = 30,  so I change the bins size to 30
# age seems to be normally distributed 
# it helps to to visual impression about the distribution

clean.data.ZK %>% 
ggplot() +
aes(x=pain) + 
geom_histogram()

# since sex is a nominal variable it can not be visualized in the histogram 
# the plots show that there are some extreme values but the check of the data before showed that those are still in a possible frame of value 

 
###### creating the first Model and running the assumptions######

#name the Model
# dependend variable = pain, independent variable = age and sex
Model1 <- lm(pain ~ age + sex, data = clean.data.ZK) 


#checking for outliers - outliers could have an huge impact on the linearity which need to be checked. A large Cooks distance, which is shown in plot, implies that the Subject has in at least on variable a high outlaier value and a high leverage on the regression. Overfitting will lead to a non-valid prediction.
x11()
cooks.distance( model = Model1 )
plot(x = Model1, which = 4) 
# when we open the graph, it says that ID 28, 74 and 88 have outlier values. It is true that those subjects have extrem values in specific variables but all those values are still in a possible frame and therefore non of them will be exluced. 


# visual check for linearity
clean.data.ZK %>% 
  ggplot() +
  aes(y=pain, x=age) + 
  geom_point() +
  geom_smooth(method="lm",se=FALSE) 

# statistical check for linearity: Tukey test
# the test proofs if the regression line is curved
residualPlots(Model1) 
# the p-value is =.46 and therefore non-significant. The Regression Model is linear. 


# check the normality of residuals 
# first name the residuals of the model
residualModel1<-residuals(Model1)

#visualize the distribution of normality of the residuals 
hist(residualModel1) 

# statistical proofment of normality
shapiro.test(residualModel1) 
# in this test the null-hypothesis implies that there is a normality in the model. Since the p-value is =.65, the result is not significant,  which means that H0 is true and therefore the residuals of the model are normally distributed. 


# check the collinearity of the predictors by using the variance of inflation factors 
vif(Model1) 
# for an exact value the square root of the value has to interpreted. The rule of thumb says that values which are close to ten indicate a collinearity. Since the result is close to 1, we can assume that there is no colinearity. 


#Testing the homogeneity of variance with the non-constant variance test: it actually proofs the heteroscedasticity of the distribution of residuals 
ncvTest(Model1) 
# as p-value =.49 the test is non-significant, which means the variance are not heterogenous but homogenous 

#model is checked: it has a normality of residuals; it is linear, there is a homogenity of variance and no multicollinearity

summary(Model1) 
# for getting F-value, p-value, R^2


#####building the second model with the same database##### 

# building Model2
# this time, all variables of the dataset will be included as predictors 
Model2<- lm(pain~age + sex +STAI_trait+ pain_cat + cortisol_serum + cortisol_saliva + mindfulness,  data = clean.data.ZK)


#####Model assumption of the second Model#####

#checking for outliers 
x11()
cooks.distance( model = Model2 ) 
plot(x = Model2, which = 4) 
# when we open the graph, it says that ID 74,88 and 123 have outlier values. It is true that those subjects have extrem values in specific variables but all those values are still in a possible frame and therefore non of them will be exluced. 

# check the collinearity of the predictors by using the variance of inflation factors 
vif(Model2) 
# The both cortisolvalues (saliva and serum) correlate highly by implication with each other, because they measure both the cortisol level. To not violating the regression model by this, saliva will be excluded, because it has the higher vif value and in medical studies serum is often regarded to be more reliable. 

# excluding saliva from Model2
Model2<- lm(pain~age + sex +STAI_trait+ pain_cat + cortisol_serum + mindfulness,  data = clean.data.ZK)

# check for linearity
residualPlots(Model2) 
#STAI_trait and pain_cat show significance when it comes to be curved and not linear, though, it is assumed that the values and the devation from linearity is pretty small and therefore not to be concered about. 


# check for normal distribution of the residuals 
residualModel2<-residuals(Model2)
hist(residualModel2) 
shapiro.test(residualModel2) 
# Since the p-value is =.99, the result is not significant,  which means that H0 is true and therefore the residuals of the model are normally distributed. 

#Testing the homogeneity of variance with the non-constant variance test: it actually proofs the heteroscedasticity of the distribution of residuals 
ncvTest(Model2) 
# zeigt 1 an???
# as p-value = .1 the test is non-significant, which means the variance are not heterogenous but homogenous 


summary(Model2)


##### Model comparison#####
# since we are working with hierachial regression, AIC and anova are the functions to use for the comparison

# comparing the prediction performance of the models models with the anova
anova(Model1,Model2) 
# the comparison shows that the Model2 with the added predictors lead to a significantly better fit than the first Model. 

# the Akaike-Information-Criterion for further comparison
AIC(Model1,Model2) 
# the Model with the lower value is seen as the better model and therefore Model 2 is the better choice for predicting perioperative pain.  



#####Final coefficient tables#####

lm.beta(Model1)
lm.beta(Model2) 

confint(Model1) 
confint(Model2)

coef_table = function(Model1){
  require(lm.beta)
  mod_sum = summary(Model1)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))	
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))	
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"	
  
  
mod_sum_table = cbind(as.data.frame(round(cbind(coef(Model1), confint(Model1), c(0, lm.beta(Model1)$standardized.coefficients[c(2:length(Model1$coefficients))])), 2)), mod_sum_p_values)	
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
  mod_sum_table["(Intercept)","Std.Beta"] = "0"	
  return(mod_sum_table)
}

coef_table(Model1)
coef_table(Model2)



###################Assignment 2###################
# Question 2 
# activate packages 
# set working directory

library(psych)
library(lm.beta)
library(dplyr)
library(gsheet)
library(car)
library(ggplot2)
library(rgl)
source("GraphPlot.R")

##### check and clean the data regarding the new predictors in account #####
View(clean.data.ZK) 
# Reminder: ID 18 was excluded before as there is one wrong value (3.5 in STAI-Trait)

#Check new variables regarding wrong/missing values 
str(clean.data.ZK)
describe(clean.data.ZK) # through the min and max we see that there is one negative Income houshold value -> this ID 49 will be excluded


# exclude the ID with the wrong Income and the to low STAI_trait value 
clean.data.ZK<-data.ZK[!data.ZK$household_income < 0,]
more.clean.data.ZK<- clean.data.ZK[!clean.data.ZK$STAI_trait < 20,]

View(more.clean.data.ZK)


##### create the new Model 3#####

# create the Model 3 with the new suggested predictors
Model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = more.clean.data.ZK)
# the researcher excluded the saliva 


##### assumptions of  Model 3 #####

#cooks distance for checking outliers 
x11()
cooks.distance(model = Model3)
plot(x = Model3, which = 4) 
# even though there are 3 IDs (55, 74 and 88) which influnences the regression highly, I still let them exist, because they are possible achievable values 

# checking the normality of residuals 
residual3 <- residuals(Model3)
shapiro.test(residual3) # p-value  (=.69) is not significant, so there is normal distribution

# checking the Intercorrelation
vif(Model3) # they are all close to one, so there is no Intercorrelation between the predictors 

#checking the Heterogeneity 
ncvTest(Model3) # the variance score test shows no significance regarding the heteroscedasticity and therefore the homogeneity of variance is not violated. 

#checking the linearity 
x11()
residualPlots(Model3) 
# the predictor pain catastrophizing is significanrt but regarding to Navarro, the deviation from linearity is small and therefore not necessary to care about it 

summary(Model3)

coef_table(Model3)

#include the more.clean.data set in Model 2
Model2<- lm(pain~age + sex +STAI_trait+ pain_cat + cortisol_serum + mindfulness,  data = more.clean.data.ZK) # we took saliva out, because it intecorelates with serum and is not as reliable than serum

#residualPlots(Model2) #now something is significant, whtat am I suppose to do?

#residual2<-residuals(Model2)
#hist(residual2) #visualisierung von Normalverteilung
#shapiro.test(residual2) #statistische Überprüfung der Normalverteilung; wenn nicht-signifikant dann normalverteilt! ich will die H0 behalten - daher nicht sig benutzt
#vif(Model2) # weil cortisolwerte natürlicherweise miteinander korrelieren, nehmen wir den höheren Wert (Saliva) raus, auch weil Serum als realiabler gilt
#ncvTest(Model2) # für Heterogenität/Homogenität - nicht sig, daher homogen und nicht heterogen

#cooks.distance( model = Model2 ) #es gibt Extremwerte, sie sind aber immer noch in einem realistischen Rahmen, weshalb ich sie nicht rausnehme 
#openGraph()
#plot(x = Model2, which = 4) 

#summary(Model2)


##########backward regression of Model3##############
step(Model3, direction = "backward") #the output reports the AIC value for the current best model 
# best Model is with the following predictors: weight + age + sex + mindfulness + pain_cat + cortisol_serum with AIC 58.36 - Model3 has an AIC of 61.71

####### creating best Model named backward model and name Model2 to TheoryBasedModel ################
BackwardModel<- lm(formula = pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + weight, data = more.clean.data.ZK)
TheoryBasedModel<-Model2

###### Anova with BackwardModel and Model 3#####
anova(Model3,BackwardModel)
# there is no significant difference between the Model 3 and the Backward Model 

####### compare both Models##############
AIC(BackwardModel,TheoryBasedModel)
anova(BackwardModel,TheoryBasedModel)
lm.beta(BackwardModel)
lm.beta(TheoryBasedModel)
confint(BackwardModel)
confint(TheoryBasedModel)
summary(BackwardModel)
summary(TheoryBasedModel)

coef_table(BackwardModel)
coef_table(TheoryBasedModel)



#########put the two models to the test some new data##############
#the data were collected the same way like the data from the Q1 

second.data.ZK <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
View(second.data.ZK)

#check the Data, if there any missing or wrong values 
summary(second.data.ZK)
# the Mins and Maxs dont show any irregularities 
str(second.data.ZK)
# also the structure of the data look fine 

#######make predictions on pain with the first data set########
print(TheoryBasedModel)
summary(TheoryBasedModel)
print(BackwardModel)
summary(BackwardModel)



#########compare the prediction of the two models: which predicted values closer to the real values ##########
pred_test<-predict(TheoryBasedModel,second.data.ZK)
pred_test_back<-predict (BackwardModel,second.data.ZK)
RSS_test =sum((second.data.ZK[,"pain"]-pred_test)^2)
RSS_test_back = sum((second.data.ZK[,"pain"]- pred_test_back)^2)

#This term is the sum of squares error, or SSE. The error is the difference between the observed value and the predicted value.
#We usually want to minimize the error. The smaller the error, the better the estimation power of the regression. Finally, I should add that it is also known as RSS or residual sum of squares. Residual as in: remaining or unexplained.


# keep in mind: first rule of model selection: always go with the model that is grounded in theory and prior research, result-driven model selection can lead to bad predictions on new datasrts due to overfitting 



###################Assignment 3####################


#load Packages and set working directory 

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library (MuMIn) # for r.squaredGLMM 
library (influence.ME) # for checking influental outliers 
library(gridExtra)
library(olsrr)


#load data (use the updated links in canvas)
data.ZK.sample3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")


#check the data set
options(digits = 6)
describe(data.ZK.sample3)
str(data.ZK.sample3)
summary(data.ZK.sample3) 

# there is a to high mindfulness value (6.05)
cleaned.data.ZK.sample3<-data.ZK.sample3[!data.ZK.sample3$mindfulness > 6,]

# there is one wrong "Female"
cleaned.data.ZK.sample3 <- cleaned.data.ZK.sample3 %>% mutate(sex = droplevels(replace(sex,sex == "Female", "female")))
summary(cleaned.data.ZK.sample3)

# create the grouping hospital
cleaned.data.ZK.sample3 %>% 	
  mutate(hospital = factor(hospital))	


#  build linear mixed model on data file 3 with the Theorybased Model predictors 
random_intercept_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = cleaned.data.ZK.sample3)

# save the resiudals in a variable fur further assumption
cleaned.data.ZK.sample3 = cleaned.data.ZK.sample3 %>% mutate(resid = residuals(random_intercept_model))

######assumptions######

# checking for outliers - takes a minute! 
influence_observation= influence(random_intercept_model, obs = TRUE)$alt.fixed
influence_group= influence(random_intercept_model, group ="hospital")$alt.fixed
data_plot_influence = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	
data_plot_influence %>% 	
  ggplot() +aes(x = 1, y = coefficient, group = predictor) +	geom_violin() +	facet_wrap( ~ predictor, scales = "free")	

#Normality of residuals 
x11()
qqmath(random_intercept_model, hospital=0.05,col="pink")
qqmath(ranef(random_intercept_model),col="pink")	
# The points should roughly fit on the straight line, which is the case. 

#Linearity
x11()
plot(random_intercept_model, arg = "pearson",col="darkblue")	

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = pain, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = age, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = sex, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = STAI_trait, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = pain_cat, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = cortisol_serum, y = residuals (random_intercept_model)) + geom_point()

cleaned.data.ZK.sample3 %>% ggplot() + aes(x = mindfulness, y = residuals (random_intercept_model)) + geom_point()

#Homoscedasticity
x11()
plot(random_intercept_model, arg = "pearson")	
homosced_mod = lm(resid^2 ~ hospital, data = cleaned.data.ZK.sample3)	
summary(homosced_mod)	

# a funnel shape would indicate heteorcedasticity. Homogeneity of variances is also not violated since the p value is = .43


# Multicollinearity 
x11()
pairs.panels(cleaned.data.ZK.sample3[,c("pain", "sex", "age", "STAI_trait","pain_cat","mindfulness", "cortisol_serum")], col = "red", lm = T)



# for model coefficients 

confint(random_intercept_model)
summary(random_intercept_model)
r2beta(random_intercept_model)
r.squaredGLMM(random_intercept_model)

coef_CI = suppressWarnings(confint(random_intercept_model))	
coef_CI	

