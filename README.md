title: "Machine learning example in R Example"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
The code below is an example of using machine learning to predict housing generally based on Kuhn (2019) guide: https://topepo.github.io/caret/

telehealth_noms_wide_noms is a data set from the national outcomes measures (NOMS), and the codebook is available here: link 

In the section below, I am sub-setting the variables that I want to include.  The variables consist of counts, ordinal, and binary variables.  These data are matched pairs from intake to 6-month (.x is at intake, and .y is 6-month).

Below I demonstrate first how I selected variables that myself and a team of content experts believed were potentially predictive of housing at 6-months.  I then identified that all the "VT" variables, school and work, and living conditions satisfaction variables were missing more significantly more than 50%.  Given the VT variables were missing significantly more than 50% and research generally discourages imputing variables (discussed later) with more than 50% missing data, we removed those variables (Manly & Wells, 2015).  

We then created several binary demographics from nominal variables.  First, we evaluated the top three primary diagnoses at baseline and created binary variables for those diagnoses.  We also created a sexual identity variable as another sexual identity and heterosexual and a gender variable as male and all other gender identities.  We also included housing at baseline and 6-months (i.e., the outcome variable). 
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
telehealth_noms_wide_noms = read.csv("telehealth_noms_wide_noms.csv", header = TRUE)
machine_dat =  telehealth_noms_wide_noms[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "SchoolOrWork.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "ViolenceTrauma.x", "VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "PhysicallyHurt.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "EnoughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x", "telehealth.x", "Housing.y")]
library(naniar)
miss_var_summary(machine_dat)
# Remove variables missing significantly more than 50%
machine_dat[,c("VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "SchoolOrWork.x", "LivingConditionsSatisfaction.x")] = list(NULL)


#DiagnosisOne.x 62 = anxiety, 59 = mdd recurrent, 58 mdd single episode
machine_dat$anxiety = ifelse(machine_dat$DiagnosisOne.x == 62, 1, 0) 
machine_dat$mdd_r= ifelse(machine_dat$DiagnosisOne.x == 59, 1, 0)
machine_dat$mdd_s = ifelse(machine_dat$DiagnosisOne.x == 58, 1, 0)
machine_dat$DiagnosisOne.x = NULL
#SexualIdentity.x 1,2,3,4 create another category for 3 and 4
machine_dat$another_sex_ident =  ifelse(machine_dat$SexualIdentity.x > 1, 1, 0)
machine_dat$SexualIdentity.x = NULL

### Change gender to male or female / another gender identity
machine_dat$Gender.x = ifelse(machine_dat$Gender.x == 1,1,0)

#Housing.x and y 1 = OWNED OR RENTED HOUSE, APARTMENT, TRAILER, ROOM
machine_dat$Housing.x = ifelse(machine_dat$Housing.x == 1, 1,0)
machine_dat$Housing.y = ifelse(machine_dat$Housing.y == 1, 1,0)



```
We next continued to pre-process the data by identifying near-zero variance variables.  We used the nearZeroVar function in caret to identify variables with large (i.e., higher than 19) ratios of the first to second most frequent variables.

We found two variables (Hispanic and physical hurt) the team decided were not critical, and we had no way of increasing the variance.

Next, we identified that drug use, nights/times in the hospital or ER, and times arrested and nights in jail all had low variance.  Given the importance of these variables, we summed each of them, respectively.  Although some variables had mixed types (i.e., nights and times), our goal is to predict housing not necessarily to have meaningful variables.  Also, after summation, jail/arrests and drug use were still near-zero variance.  However, because of their importance in predicting housing, we kept all the summed variables.
```{r}
library(caret)

nzv = nearZeroVar(machine_dat, saveMetrics = TRUE)
nzv

# HispanicLatino.x, PhysicallyHurt.x
machine_dat$HispanicLatino.x = NULL
machine_dat$PhysicallyHurt.x = NULL


machine_dat$drug_use = machine_dat$Cocaine_Use.x + machine_dat$Meth_Use.x + machine_dat$StreetOpioids_Use.x + machine_dat$RxOpioids_Use.x
machine_dat$Cocaine_Use.x = NULL
machine_dat$Meth_Use.x = NULL
machine_dat$StreetOpioids_Use.x = NULL
machine_dat$RxOpioids_Use.x = NULL


machine_dat$er_hos_use_base = machine_dat$NightsDetox.x + machine_dat$NightsHospitalMHC.x + machine_dat$TimesER.x
### Drop other variables
machine_dat[,c("NightsDetox.x", "NightsHospitalMHC.x", "TimesER.x")] = list(NULL)


machine_dat$jail_arrest_base = machine_dat$NumTimesArrested.x + machine_dat$NightsJail.x
machine_dat$NightsJail.x = NULL
machine_dat$NumTimesArrested.x = NULL
```
Our next step is to identify if there are any high (i.e., .90 or greater) spearman correlations between variables.  We used a Spearman correlation, as most variables are generally not normally distributed.  We found no correlation over .9.  

Additionally, we evaluated the variance inflation factors (VIFs) for a logistic regression model with housing as the outcome variable.  This logistic regression model will be similar to the final model used; therefore, the VIFs can provide some insight into potential multicollinearity.  Overall we only found two VIFs above five quarter at 6.12 and telehealth at 6.4, providing evidence that multicollinearity is generally not a concern. 

```{r}
descCor = cor(machine_dat, use = "pairwise.complete.obs", method = "spearman")
hig_corr = findCorrelation(descCor)
hig_corr
library(car)
machine_dat
vif_model = glm(Housing.y ~ ., data = machine_dat)
vif_list =  vif(vif_model)
vif_list = data.frame(vif_list) 
vif_list = subset(vif_list, vif_list > 5)
vif_list
```



Next, we are evaluating the missing data using the Amelia package with five imputations.  The Amelia package has advantages over other packages (e.g., MICE), because we can set the type of variable (i.e., nominal, log).  In my personal experience, I have found the noms function works for binary variables better than binary, and a nominal regression with binary data reduces to a logistic regression.  More information on data imputation is available in the Amelia package documentation: link
```{r}
library(Amelia)
library(prettyR)

#a.out_noms = amelia(x = machine_dat, m = 5, noms = c("Gender.x", "RaceWhite.x", "RaceBlack.x", "Housing.x", "telehealth.x", "Housing.y", "anxiety", "mdd_r", "mdd_s", "another_sex_ident"), ords = c("Quarter.x", "Agegroup.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "ViolenceTrauma.x", "EnoughMoneyForNeeds.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x"), logs = c("drug_use" ,"er_hos_use_base", "jail_arrest_base"))

#saveRDS(a.out_noms, file = "a.out_noms.rds")
a.out_noms = readRDS(file = "a.out_noms.rds")
impute_dat_noms = a.out_noms$imputations
```
We also compared the densities of the observed data versus the model imputed.  Overall the distributions match with some imputations having flatter distributions where some variables spike. 
```{r}

miss_var_summary(machine_dat)
compare.density(a.out_noms, var = "GetsAlongWithFamily.x")
compare.density(a.out_noms, var = "RelationshipSatisfaction.x")
compare.density(a.out_noms, var = "drug_use")
compare.density(a.out_noms, var = "Housing.y")

test_dat = impute_dat_noms$imp1
```
Now, because we have five data sets, we need to conduct all the remaining analyses five times. 
First, we need to make sure R is treating each variable as correct variable type (e.g., factor, numeric).  Therefore, we use the apply function on the factor (i.e., binary)
```{r}
impute_dat_noms$imp1

impute_dat_noms$imp1[,-c(2:30, 33:39)] = apply(test_dat[,-c(1,5:30, 33:42, 49)], 2, function(x){as.factor(x)})

test_dat[,c(1,5:30, 33:42, 49)] = apply(test_dat[,c(1,5:30, 33:42, 49)], 2, function(x){as.numeric(x)})

### Create separate data sets to you don't have to write every variable out
test_dat_Housing.y = test_dat 
test_dat_Housing.y$Housing.y = as.factor(test_dat_Housing.y$Housing.y)


```


Next we create a testing and training data set.  We take 75% of the data for training and 25% for testing.  
```{r}
test_dat_house_index =  createDataPartition(test_dat_Housing.y$Housing.y, p = .75,list = FALSE, times = 1)
train = test_dat_Housing.y[test_dat_house_index,]
testing = test_dat_Housing.y[-test_dat_house_index,]

```
Then we set the train control settings.  In this setting we conducted a repeated cross-validation where we create 10 cross validation data sets and repeat this process 10 times.

When we 
```{r}
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

set.seed(825)
gbmFit_house <- train(Housing.y ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

summary(gbmFit_house)

300/5
```
Then evaluate the model fit.  We get the predicted values from the gbm model with the testing data.  To see what the probability.

```{r}
plsProbs <- predict(gbmFit_house, newdata = testing, type = "prob")
plsClasses <- predict(gbmFit_house, newdata = testing)

confusionMatrix(data = plsClasses, reference = testing$Housing.y)
```



