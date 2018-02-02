library(Hmisc) #rcorr function
library(car) #leveneTest function
library(gmodels) #CrossTable function
library(effects) #effect function in checking partial residuals
library(brglm) #brglm for capital gains and losses
library(ROCR) #Used for prediction
library(pROC) #Used for statistics on test set

#Importing the CSV file
RTItable <- read_csv("~/Documents/IAA Documents/Job Docs/RTI/RTItable.csv")
data <- RTItable

#Checking the dimensions
dim(data)
#Number of rows and columns are expected

####################################################################
# EXPLORATORY DATA ANALYSIS
####################################################################

#MISSING VALUES
#Before looking further into the variables, I want to know more about missing values
#First need to convert '?' to NA
data[data=='?'] <- NA
#Counting the number of NAs for each var
sapply(data,function(x) sum(is.na(x)))
#workclass and occupation have over 1000 NAs, country has 857
#Is this a lot? Can check this by looking at the proportion of NAs by var
sapply(data,function(x) sum(is.na(x))/length(x))
#About 6% of workclass and occupation are missing, and 2% of country is missing
#Not necessarily enough to be super concerned, but could consider looking into:
#1. Imputing the missing data
#2. Determining if the data was Missing at Random or missing for other reasons

#CONTINUOUS VARIABLES
convars <- data[,c("age","education_num","capital_gain","capital_loss","hours_week")]

#Looking at summaries of continuous variables
summary(convars)
#Min and max values seem reasonable (though 99 hours per week is extreme)

#Look at distribution for continuous variables
hist(convars)
#Concerning is capital_loss and capital_gain which have a significant proportion as 0 and a right skew

#Correlation matrix to test relationships between continuous variables
#Using rcorr as it also includes significance values
rcorr(as.matrix(convars))
#Results: the largest correlation is 0.14 between hours_week and education_num
#The significance test suggets all correlations are significantly different from 0
#Results suggest that analyst may want to consider interactions in model building

#CATEGORICAL VARIABLES
catvars <- data[,c("workclass", "education_level","marital_status","occupation","race","relationship","sex","country","cgaingroup","clossgroup")]

#Looking at a table and barplot for each variable
for (i in 1:length(catvars)){
  print(colnames(catvars)[i])
  print(table(catvars[i]))
  barplot(table(catvars[i]))
}
#Notable groups: some countries of origin had very small numbers, while the US had many (43832)
#Tempted to bin them by region for this analysis, but with a caveat that I may not recommend this with a larger dataset
#Since my experience has been that individuals are often much more tied to their previous home nation than region

data$region <- NA
data$region[data$country == 'England' | data$country == 'France' | data$country == 'Germany' | data$country == 'Greece' | data$country == 'Holand-Netherlands' | data$country == 'Hungary' | data$country == 'Ireland' | data$country == 'Italy' | data$country == 'Poland' | data$country == 'Portugal' | data$country == 'Scotland' | data$country == 'Yugoslavia'] <- "Europe"
data$region[data$country == 'Cambodia' | data$country == 'China' | data$country == 'Hong' | data$country == 'India' | data$country == 'Iran' | data$country == 'Japan' | data$country == 'Laos' | data$country == 'Philippines' | data$country == 'South' | data$country == 'Taiwan' | data$country == 'Thailand' | data$country == 'Vietnam'] <- "Asia"
data$region[data$country == 'Columbia' | data$country == 'Cuba' | data$country == 'Dominican-Republic' | data$country == 'Ecuador' | data$country == 'El-Salvador' | data$country == 'Guatemala' | data$country == 'Haiti' | data$country == 'Honduras' | data$country == 'Jamaica' | data$country == 'Nicaragua' | data$country == 'Peru' | data$country == 'Trinadad&Tobago'] <- "Central/South America-Caribbean"
data$region[data$country == 'Canada' | data$country == 'Mexico'] <- 'North America(non-US)'
data$region[data$country == 'Outlying-US(Guam-USVI-etc)' | data$country == 'Puerto-Rico'] <- "Outlying-US"
data$region[data$country == 'United-States'] <- "United States"

#Also large numbers in the 'Private' category with small numbers in 'never-worked' and 'without pay'
#Combining 'never-worked' and 'without-pay' into 'Other'
data$workclass[data$workclass == 'Never-worked' | data$workclass == 'Without-pay'] <- "Other"

#Note that there are more men than women in this sample which might be unexpected for a census sample (true pop is 51/49)

#Now attaching the dataset after bin adjustments
attach(data)

#Exploring some possible relationships between categorical variables:
ggplot(catvars, aes(race, ..count..)) + geom_bar(aes(fill = sex, position = "dodge"))
#Far more white men than white women, smaller difference in other categories
ggplot(catvars, aes(occupation, ..count..)) + geom_bar(aes(fill = education_level, position = "dodge"))
#Not many surprises: industries I would expect to require more education generally had more education

#COMPARING CONTINUOUS AND CATEGORICAL VARIABLES
boxplot(age~marital_status)
#As expected, average age for 'never married' and 'married-af' is younger, 'widowed' is older
boxplot(age~education_level)
#Not really a clear trend
boxplot(capital_gain~occupation)
boxplot(capital_loss~occupation)
#Outliers in every group
boxplot(education_num~race)
#Similar means but different variances

#RESPONSE VARIABLE: OVER_50K

table(over_50k)
summary(over_50k)
#23.9% of observations are over 50k

#Over 50k versus age
boxplot(age~over_50k)
#average age appears to be higher for those over 50k

#Over 50k versus number of years of ed
boxplot(education_num~over_50k)
#average numbers of higher ed appear to be higher for those over 50k

#Over 50k versus capital gains and losses

boxplot(capital_gain~over_50k)
boxplot(capital_loss~over_50k)

################################# Update: I initially ran this as a numeric variable but had problems with standard errors and interpretation
#So I split into categorical groups
#Max loss is $3900, so dividing at approximately half ($2000)
data$clossgroup <- NA
data$clossgroup[data$capital_loss == 0] <- "NoLoss"
data$clossgroup[ 0 < data$capital_loss & data$capital_loss < 2000] <- "LossBelow2K"
data$clossgroup[ 2000 <= capital_loss] <- "Loss2KorMore"

################################################ Update:
#Initally to also have a group just for 99999, but its a perfect predictive level and had huge standard errors
#Gains 0, 0-5000, 5000-10000, 10000+
data$cgaingroup <- NA
data$cgaingroup[data$capital_gain == 0] <- "NoGain"
data$cgaingroup[data$capital_gain > 0 & data$capital_gain < 5000] <- "Gain0to5K"
data$cgaingroup[data$capital_gain >= 5000 & data$capital_gain < 10000] <- "Gain5Kto10K"
data$cgaingroup[data$capital_gain >= 10000] <- "Gain10K+"

CrossTable(over_50k,cgaingroup,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = cgaingroup, position = "dodge"))

CrossTable(over_50k,clossgroup,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = clossgroup, position = "dodge"))
#Those with losses hae higher proportion in over 50k

#Over 50k versus hours worked per week
boxplot(hours_week~over_50k)
#Same mean, but differences in spread

#Over 50k versus workclass
CrossTable(over_50k,workclass,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(data$over_50k, ..count..)) + geom_bar(aes(fill = data$workclass, position = "dodge"))
#Interestingly the federal government has the highest proportion in over_50k (0.392) while 'Other' has the smallest

#Over 50 k versus education level
CrossTable(over_50k,education_level,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = education_level, position = "dodge"))
#As expected, a larger proportion of those with more education were in over 50k, the highest in the professional category

#################### Came back to make adjustment here:
# Discovered that preschool is nearly a perfect predictor level
# Tried to use a penalized model (brglm) but didn't work
# Decided to merge Preschool with 1st-4th
data$education_level[data$education_level == "Preschool"] <- "1st-4th"


#Over 50k versus marital status
CrossTable(over_50k,marital_status,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = marital_status, position = "dodge"))
#Married observations (both AF and Civil) have higher proportions

#Over 50k versus occupation
CrossTable(over_50k,occupation,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = occupation, position = "dodge"))
#Professional services, executive management, and armed forces had the highes prop above 50k

#Over 50k versus race
CrossTable(over_50k,race,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = race, position = "dodge"))
#Highest props above 50k for White and Asian-Pac-Islander

#Over 50k versus sex
CrossTable(over_50k,sex,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = sex, position = "dodge"))
#Men: 0.3 above 50k versus Women: 0.11 above 50k. Significantly different?
chisq.test(over_50k,race) #Yes

#Over 50k versus country
CrossTable(over_50k,country,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = country, position = "dodge"))
#Fair amount of variation between countries

#Over 50k versus region
CrossTable(over_50k,region,prop.r=FALSE,prop.c = TRUE,prop.t=FALSE, prop.chisq = FALSE)
ggplot(catvars, aes(over_50k, ..count..)) + geom_bar(aes(fill = region, position = "dodge"))
#Asia, Europe, and US had highest proportions over 50k


#Relationship plot for report:
#Making this into a nicer plot for report
reportplot <- ggplot(data,aes(x=as.factor(over_50k),y=capital_gain),col=factor(cgaingroup)) + geom_boxplot()
reportplot <- reportplot + ggtitle("Capital Gains by Income Status")
reportplot <- reportplot + scale_x_discrete(name="Income Over $50K per Year",labels=c("No","Yes")) + scale_y_continuous(name="Capital Gain ($)")
reportplot <- reportplot + geom_point(aes(color=factor(cgaingroup)))
reportplot <- reportplot + theme(legend.title=element_blank())
reportplot <- reportplot + scale_fill_manual(breaks=c("NoGain","Gain0to5K","Gain5Kto10K","Gain10K+"))
reportplot

#######################################################################
#DIVIDING INTO TRAIN, VALIDATION, AND TEST
#USING 70% TRAIN, 15% VALIDATION, 15% TEST
#Source from StackOverflow:
#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function

#First setting Categorical Vars as Factors so I can set reference levels in logistic regression
#Chose reference levels by most common choice
data$workclass <- factor(data$workclass) #Ref level: Private
data$education_level <- factor(data$education_level) #Ref level: HS-grad
data$marital_status <- factor(data$marital_status) #Ref level: Married-civ-spouse
data$occupation <- factor(data$occupation) #Ref level: Adm-clerical
data$race <- factor(data$race) #Ref level: White
data$sex <- factor(data$sex) #Ref level: Male
data$region <- factor(data$region) #Ref level: United States
data$clossgroup <- factor(data$clossgroup) #Ref level: NoLoss
data$cgaingroup <- factor(data$cgaingroup) #Ref level: NoGain

#Setting the fraction sizes
fracTrain <- 0.7
fracValid <- 0.15
fracTest <- 0.15

#Computing the sample sizes
sampsizeTrain <- floor(fracTrain * nrow(data))
sampsizeValid <- floor(fracValid * nrow(data))
sampsizeTest <- floor(fracTest * nrow(data))

#Creating the randomly-sampled indices, use setdiff() to avoid overlapping sets
# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(data)), size=sampsizeTrain))
indicesNotTraining <- setdiff(seq_len(nrow(data)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampsizeValid))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
dataTraining   <- data[indicesTraining, ]
dataValidation <- data[indicesValidation, ]
dataTest       <- data[indicesTest, ]


########################################################################
# LOGISTIC REGRESSION TO PREDICT ABOVE 50K PER YEAR
########################################################################
# First model: built off intuition and exploratory data analysis
# Simple to add other characteristics

fit1 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse"), family = binomial(link = "logit"), data=dataTraining)
summary(fit1)

#First question: does this model meet linearity assumption?
# Use Box-Tidwell transformation to check
fit1lncheck <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+I(age*log(age)),family = binomial(link = "logit"), data=dataTraining)
summary(fit1lncheck)
#Age does NOT meet linearity assumption
#Will note this and continue moving forward

#Second question: does this model do better than just the intercept?
#Check with Likelihood Ratio Test
logL.model <- fit1$deviance
df.model <- fit1$df.residual
logL.intonly <- fit1$null.deviance
df.intonly <- fit1$df.null
pchisq(logL.intonly - logL.model, df.intonly - df.model, lower.tail = FALSE)
#Yes, added terms are significant

#Third question: should I remove any variables currently included?
#These are: age, sex, education_level, marital_status

#Is age significant?
#Model without age
fit1A <- glm(over_50k ~ relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse"),family = binomial(link = "logit"), data=dataTraining)
#Testing LRT
logL.full <- fit1$deviance
df.full <- fit1$df.residual
logL.reduced <- fit1A$deviance
df.reduced <- fit1A$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Yes, age is significant

#Is sex significant?
fit1S <- glm(over_50k ~ age+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit1$deviance
df.full <- fit1$df.residual
logL.reduced <- fit1S$deviance
df.reduced <- fit1S$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Yes, sex is a significant addition

#Is education_level a significant addition?
fit1E <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(marital_status,"Married-civ-spouse"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit1$deviance
df.full <- fit1$df.residual
logL.reduced <- fit1E$deviance
df.reduced <- fit1E$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Yes, education level is significant

#Is marital status significant?
fit1M <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit1$deviance
df.full <- fit1$df.residual
logL.reduced <- fit1M$deviance
df.reduced <- fit1M$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Yes, it is significant

#Fourth question: are there other variables I should add?
#Possible additions:
#workclass, education_num, occupation, race, capital_gain, capital_loss, hours_week, country

#workclass
fit2 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(workclass,"Private"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit2$deviance
df.full <- fit2$df.residual
logL.reduced <- fit1$deviance
df.reduced <- fit1$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Not a significant addition

#Doesn't make sense to use education_num because I am already using education_level

#occupation
fit3 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(occupation,"Adm-clerical"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit3$deviance
df.full <- fit3$df.residual
logL.reduced <- fit1$deviance
df.reduced <- fit1$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Not a significant addition

#race
fit4 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit4$deviance
df.full <- fit4$df.residual
logL.reduced <- fit1$deviance
df.reduced <- fit1$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#YES, race is significant

#fit4 is current model

#capital gain
fit5 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain"), family = binomial(link = "logit"), data=dataTraining)
logL.full <- fit5$deviance
df.full <- fit5$df.residual
logL.reduced <- fit1$deviance
df.reduced <- fit1$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#capital gain IS a significant addition
#Made fit5 the new model

#capital loss
#Same issue with quasi-separation
fit6 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit6$deviance
df.full <- fit6$df.residual
logL.reduced <- fit5$deviance
df.reduced <- fit5$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Capital loss IS a significant addition
#Made fit6 a new model

#hours week
fit7 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss")+hours_week,family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit7$deviance
df.full <- fit7$df.residual
logL.reduced <- fit6$deviance
df.reduced <- fit6$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#hours per week IS significant
#made fit7 the new model

#region
fit8 <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss")+hours_week+relevel(region,"United States"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit8$deviance
df.full <- fit8$df.residual
logL.reduced <- fit7$deviance
df.reduced <- fit7$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Region is NOT significant

#Current model
summary(fit7)

#Fourth question: are there significant interactions?
#Variables in current model: age, sex, education_level, race, cgaingroup, clossgroup, hours_week
#Variables with largest effects: Gain, Education Level, Marriage
#Start by looking at interactions between these three variables

#Possibly an interaction between capital_gain and education_level?
fit7J <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+capital_gain+capital_loss+hours_week+relevel(cgaingroup,"NoGain")*relevel(education_level,"HS-grad"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit7J$deviance
df.full <- fit7J$df.residual
logL.reduced <- fit7$deviance
df.reduced <- fit7$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#A significant addition

#Interaaction between gain and marriage?
fit7K <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+capital_gain+capital_loss+hours_week+relevel(cgaingroup,"NoGain")*relevel(marital_status,"Married-civ-spouse"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit7K$deviance
df.full <- fit7K$df.residual
logL.reduced <- fit7$deviance
df.reduced <- fit7$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#A significant addition

#Interaaction between marriage and ed level?
fit7L <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+capital_gain+capital_loss+hours_week+relevel(education_level,"HS-grad")*relevel(marital_status,"Married-civ-spouse"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit7L$deviance
df.full <- fit7L$df.residual
logL.reduced <- fit7$deviance
df.reduced <- fit7$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#NOT a significant addition

#Do we need both the gain/ed level and gain/marriage interactions?
fit7JK <- glm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+capital_gain+capital_loss+hours_week+relevel(cgaingroup,"NoGain")*relevel(education_level,"HS-grad")+relevel(cgaingroup,"NoGain")*relevel(marital_status,"Married-civ-spouse"),family = binomial(link = "logit"),data=dataTraining)
logL.full <- fit7JK$deviance
df.full <- fit7JK$df.residual
logL.reduced <- fit7J$deviance
df.reduced <- fit7J$df.residual
pchisq(logL.reduced - logL.full, df.reduced - df.full, lower.tail = FALSE)
#Yes, both terms contribute significantly to model

#BIG NOTE: the interactions do complicate the model quite a bit
#They also don't contribute to enormous decreases in AIC
fit7$aic
fit7J$aic
fit7JK$aic

#Going to stick with fit7 because it is simpler and still accomplishes the job
#Current model:
summary(fit7)

#Discovered quasi-separation from capital gains
#Tried a fix to penalize
#No change
fit7br <- brglm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss")+hours_week,family = binomial(link = "logit"),data=dataTraining)



#Fifth question: how does this model compare to a computer generated model?
# Will compare to backwards model (with backwards also made on the training and compared on the validation)
fullmodel <- glm(over_50k~age+relevel(workclass,"Private")+relevel(education_level,"HS-grad")+education_num+relevel(marital_status,"Married-civ-spouse")+relevel(occupation,"Adm-clerical")+relevel(race,"White")+relevel(sex,"Male")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss")+hours_week+relevel(region,"United States"),family = binomial(link = "logit"),data=dataTraining)
backwards <- step(fullmodel,data=dataTraining,direction = "backward")
summary(backwards)

#Getting AIC for each on training data:
fit7$aic
backwards$aic
#AIC is definitely better for the one selected by backwards selection
#But let's see performance on validation data

#Validation data

backpredict <- predict(backwards,newdata=dataValidation,type="response")
backpredictdf <- cbind(dataValidation,backpredict)
fitback.roc <- roc(backpredictdf$over_50k, backpredictdf$backpredict)

fit7predict <- predict(fit7,newdata=dataValidation,type="response")
fit7predictdf <- cbind(dataValidation,fit7predict)
fit7.roc <- roc(fit7predictdf$over_50k, fit7predictdf$fit7predict)

plot(x = 1-fitback.roc$spec, y = fitback.roc$sens, 
     type = "l", col = "red", xlab = "1 - specificity", ylab = "sensitivity")
lines(x = 1-fit7.roc$spec, y = fit7.roc$sens, col = "blue")
abline(a = 0, b = 1)

# Brier score for fit7JK
# estimated probabilities from fitted:
p.hat <- predict(fit7, data = dataValidation, type = "response")
# Brier score = mean((yi-pi)^2)
brier <- mean((fit7$y - p.hat)^2)
# scaled: max = mean(pi)(1-mean(pi)), scaled = 1-(brier/max)
brier.max <- (mean(fit7$y)*(1-mean(fit7$y))^2)+((1-mean(fit7$y))*mean(fit7$y)^2)
brier.scaled <- 1 - (brier/brier.max)
brier.scaled

# Brier score for backwards
# estimated probabilities from fitted:
p.hat <- predict(backwards, data = dataValidation, type = "response")
# Brier score = mean((yi-pi)^2)
brier <- mean((backwards$y - p.hat)^2)
# scaled: max = mean(pi)(1-mean(pi)), scaled = 1-(brier/max)
brier.max <- (mean(backwards$y)*(1-mean(backwards$y))^2)+((1-mean(backwards$y))*mean(backwards$y)^2)
brier.scaled <- 1 - (brier/brier.max)
brier.scaled

# The backwards selection model had better AIC on the training data
# But the original model has a better Brier Score, indicating better discrimination and calibration
# Choosing the Original Model

#Sixth question: how does the final model do on the test dataset?
#Brier Score:
p.hat <- predict(fit7, data = dataTest, type = "response")
# Brier score = mean((yi-pi)^2)
brier <- mean((fit7$y - p.hat)^2)
# scaled: max = mean(pi)(1-mean(pi)), scaled = 1-(brier/max)
brier.max <- (mean(fit7$y)*(1-mean(fit7$y))^2)+((1-mean(fit7$y))*mean(fit7$y)^2)
brier.scaled <- 1 - (brier/brier.max)

# boxplot of predicted probabilities by outcome
boxplot(plogis(fit$linear)~fit$y, 
        xlab = "outcome", ylab = "predicted probability")
# discrimination slope = mean(p1) - mean(p0)
mean(plogis(fit$linear[fit$y==1]))-mean(plogis(fit$linear[fit$y==0]))


fit7br <- brglm(over_50k ~ age+relevel(sex,"Female")+relevel(education_level,"HS-grad")+relevel(marital_status,"Married-civ-spouse")+relevel(race,"White")+relevel(cgaingroup,"NoGain")+relevel(clossgroup,"NoLoss")+hours_week,family = binomial(link = "logit"),data=dataTraining)

