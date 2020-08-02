title: "telehealth_noms"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Stack variables

Post stacking transformations

All other grants
0600 = Baseline
0601 = 6 month resasessment


CCBHC
For assessment see below 3-months is actually vitals and so on
3-month vitals
6 month reassessment
9-month vitals
12 month reassessment

Assessment: 
0600 = Baseline Assessment
0301 = 3 Month Reassessment (vitals)
0302 = 6 Month Reassessment
0303 = 9 Month Reassessment (vitals)
0304 = 12 Month Reassessment
0699 = clincial discharge

Assessment_new
0 = Baseline
1 = 3 month reassessment (vitals)
2 = 6 month reassessment (302 from CCBHC and 601 from all other grants)
3 = 9 month reassessment (vitals)
4 = 12 month reassessment
5 = clinical discharge

Now review the missing data
Only include Baseline and 6-month
telehealth.y means they were in telehealth at 6 months which is what we want

# Data mergeing
For CCBHC IN, IL all the same

telehealth: Telehealth = 1; Pre-telehealth = 0 telehealth defined as those with any assessment date on or after 4-2-2020

### Run this prior to any analysis to load data ####
```{r}
library(prettyR)
library(see)
library(performance)
###
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
IN =  read.csv("CCBHC_IN_5.28.20.csv", header = TRUE, na.strings =  c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FHHC = read.csv("fhhc_noms_5_27_20.csv", header= TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
ICP = read.csv("SPARS Data Download 5.23.2020_ICP.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
SOCAT = read.csv("SOCAT NOMs download 5.27.20.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_adult = read.csv("data down 5.26.20 adult CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
IL_youth = read.csv("data down 5.26.20 child CCBHC IL.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))
FL_ACT = read.csv("FL-ACT SPARS data download  5.28.2020.csv", header = TRUE, na.strings = c(-99, -98, -1, -2, -3, -4, -5, -6, -7, -8, -9))

## Now stack them
### Create an empty data and then fill it with NAs.  Keep the first 44 those are correct and match
SOCAT$RespondentType = NULL
SOCAT_matrix = matrix(NA, ncol = 185-43, nrow = dim(SOCAT)[1])
SOCAT_matrix = data.frame(SOCAT_matrix)
colnames(SOCAT_matrix) = colnames(ICP[,44:185])
SOCAT_full = data.frame(SOCAT[,1:43], SOCAT_matrix)
dim(SOCAT_full)
### Change variables that match
SOCAT_full$Nervous = SOCAT$Nervous
SOCAT_full$Hopeless = SOCAT$Hopeless
SOCAT_full$Restless = SOCAT$Restless
SOCAT_full$Depressed = SOCAT$Depressed
SOCAT_full$EverythingEffort = SOCAT$EverythingEffort
SOCAT_full$Worthless = SOCAT$Worthless
SOCAT_full$Tobacco_Use = SOCAT$Tobacco_Use
SOCAT_full$Alcohol_Use = SOCAT$Alcohol_Use
SOCAT_full$StreetOpioids_Use = SOCAT$StreetOpioids_Use
SOCAT_full$RxOpioids_Use = SOCAT$RxOpioids_Use
SOCAT_full$NightsHomeless = SOCAT$NightsHomeless
SOCAT_full$NightsHospitalMHC = SOCAT$NightsHospitalMHC
SOCAT_full$NightsDetox = SOCAT$NightsDetox
SOCAT_full$NightsJail = SOCAT$NightsJail
SOCAT_full$TimesER = SOCAT$TimesER
SOCAT_full$Housing = SOCAT$Housing
SOCAT = SOCAT_full

IL_youth$RespondentType = NULL
IL_youth_matrix = matrix(NA, ncol = 185-43, nrow = dim(IL_youth)[1])
IL_youth_matrix = data.frame(IL_youth_matrix)
colnames(IL_youth_matrix) = colnames(ICP[,44:185])
IL_youth_full = data.frame(IL_youth[,1:43], IL_youth_matrix)
dim(IL_youth_full)
### Change variables that match
IL_youth_full$Nervous = IL_youth$Nervous
IL_youth_full$Hopeless = IL_youth$Hopeless
IL_youth_full$Restless = IL_youth$Restless
IL_youth_full$Depressed = IL_youth$Depressed
IL_youth_full$EverythingEffort = IL_youth$EverythingEffort
IL_youth_full$Worthless = IL_youth$Worthless
IL_youth_full$Tobacco_Use = IL_youth$Tobacco_Use
IL_youth_full$Alcohol_Use = IL_youth$Alcohol_Use
IL_youth_full$StreetOpioids_Use = IL_youth$StreetOpioids_Use
IL_youth_full$RxOpioids_Use = IL_youth$RxOpioids_Use
IL_youth_full$NightsHomeless = IL_youth$NightsHomeless
IL_youth_full$NightsHospitalMHC = IL_youth$NightsHospitalMHC
IL_youth_full$NightsDetox = IL_youth$NightsDetox
IL_youth_full$NightsJail = IL_youth$NightsJail
IL_youth_full$TimesER = IL_youth$TimesER
IL_youth_full$Housing = IL_youth$Housing
IL_youth = IL_youth_full

IN_IL_KY_CCBHC = rbind(IN[,1:185], IL_youth[,1:185], IL_adult[,1:185])
dim(IN_IL_KY_CCBHC)
FHHC = FHHC[,1:185]
ICP = ICP[,1:185]
FL_ACT = FL_ACT[,1:185]
dim(ICP)
dim(SOCAT)
### Add grant ID
IN_IL_KY_CCBHC$grant = rep("IN_IL_KY_CCBHC", dim(IN_IL_KY_CCBHC)[1])
FHHC$grant = rep("FHHC", dim(FHHC)[1])
ICP$grant = rep("ICP", dim(ICP)[1])
SOCAT$grant = rep("SOCAT", dim(SOCAT)[1])
FL_ACT$grant = rep("FL_ACT", dim(FL_ACT)[1])
dim(SOCAT)
telehealth_noms = rbind(IN_IL_KY_CCBHC, FHHC, ICP, SOCAT, FL_ACT)
dim(telehealth_noms)
### Create a new ConsumerID that is a mix of grant and ConsumerID
telehealth_noms$ConsumerID_grant = paste0(telehealth_noms$ConsumerID, telehealth_noms$GrantID)

### Figure out how you can stack FHHC data
dim(telehealth_noms)

## Rename to the above

## No one has multiple reassessments


## Create recoded assessment variable
telehealth_noms$Assessment_new = ifelse(telehealth_noms$Assessment == 600, 0, ifelse(telehealth_noms$Assessment == 301, 1, ifelse(telehealth_noms$Assessment == 302, 2, ifelse(telehealth_noms$Assessment == 303, 3, ifelse(telehealth_noms$Assessment == 601,2, NA)))))
telehealth_noms$Assessment_new = as.numeric(telehealth_noms$Assessment_new)
describe.factor(telehealth_noms$Assessment_new, decr.order= FALSE)
### Create full date variable
telehealth_noms$date = paste0(telehealth_noms$FFY, "-", telehealth_noms$Month, "-", "01")
library(lubridate)
telehealth_noms$date = ymd(telehealth_noms$date)
head(telehealth_noms$date)

telehealth_noms$telehealth = ifelse(telehealth_noms$date >= "2020-04-01", 1, 0)
telehealth_noms[c("date","telehealth")]
### Cannot be greater than 2020-09-30 last day of grant
#telehealth_noms = subset(telehealth_noms, date < "2020-09-30")
## Check that all dates post 2014 most grants are for at most five years
#telehealth_noms = subset(telehealth_noms, date > "2014-01-01")
telehealth_noms[c("date","telehealth")]
dim(telehealth_noms)
describe.factor(telehealth_noms$grant)
range(telehealth_noms$date)
### Create a NOMS data set  
telehealth_noms_wide = subset(telehealth_noms, Assessment_new == 0 | Assessment_new == 2)
dim(telehealth_noms)[1]
describe.factor(telehealth_noms$Assessment_new)


####################
library(naniar)
miss_var_summary(telehealth_noms)

head(telehealth_noms)
miss_var_summary(subset(telehealth_noms, Assessment_new == 2))
miss_var_summary(subset(telehealth_noms, Assessment_new == 0))

### These people have two baselines delete them 'A00276''A00295''A00298'

telehealth_noms_wide = telehealth_noms_wide[order(telehealth_noms_wide$ConsumerID),]
telehealth_noms_wide_test = subset(telehealth_noms_wide, ConsumerID == "'A00276'" | ConsumerID == "'A00295'" | ConsumerID == "'A00298'")
## If there is no interview then delete the second, if there is only one interview delete the none interview, if there are two interviews for baseline delete the second see conductedinterview variable
telehealth_noms_wide[c(1942, 1960, 1965),]
telehealth_noms_wide = telehealth_noms_wide[-c(1942, 1960, 1965),] 


#telehealth_noms_wide = telehealth_noms_wide[-c(3754, 3779),] 
telehealth_noms_base_noms = subset(telehealth_noms_wide,Assessment_new == 0)
telehealth_noms_month6_noms = subset(telehealth_noms_wide,Assessment_new == 2)
describe.factor(telehealth_noms_base_noms$grant)
describe.factor(telehealth_noms_month6_noms$grant)

head(telehealth_noms_base_noms)
dim(telehealth_noms_month6_noms)
telehealth_noms_wide_noms = merge(telehealth_noms_base_noms, telehealth_noms_month6_noms, by = "ConsumerID_grant", all.y = TRUE)
dim(telehealth_noms_wide_noms)
telehealth_noms_wide_noms = telehealth_noms_wide_noms[order(telehealth_noms_wide_noms$ConsumerID_grant),]
telehealth_noms_month6_noms = telehealth_noms_month6_noms[order(telehealth_noms_month6_noms$ConsumerID_grant),]
telehealth_noms_month6_noms$ConsumerID_grant == telehealth_noms_wide_noms$ConsumerID_grant

head(telehealth_noms_month6_noms)
describe.factor(telehealth_noms_month6_noms$telehealth)
describe.factor(telehealth_noms_month6_noms$grant)
describe.factor(telehealth_noms_wide_noms$telehealth.y)

### Gender is female
telehealth_noms_wide_noms$Gender.y = ifelse(telehealth_noms_wide_noms$Gender.y == 2, 1, 0)

#### Create diagnosis variables
describe.factor(telehealth_noms_wide_noms$DiagnosisOne.y)
test_dat = subset(telehealth_noms_wide_noms, DiagnosisOne.y == "59")
describe.factor(test_dat$telehealth.y)

### Enough 62 which is 62 = F40-F48 – Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders
#59 = F33 – Major depressive disorder, recurrent
#57 = F31 – Bipolar disorder
telehealth_noms_wide_noms$dep = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 59, 1, 0)
telehealth_noms_wide_noms$bipolar = ifelse(telehealth_noms_wide_noms$DiagnosisOne.y == 57, 1, 0)
describe.factor(telehealth_noms_wide_noms$dep)
telehealth_noms_wide_noms$InterviewDate.y
dim(telehealth_noms_wide_noms)
write.csv(telehealth_noms_wide_noms, "telehealth_noms_wide_noms.csv", row.names = FALSE)
```
This is an example of using machine learning to predict housing generally based on Kuhn (2019) guide: 

telehealth_noms_wide_noms is a data from the national outcomes measures (NOMS) and the codebook is available here: https://topepo.github.io/caret/pre-processing.html

In the section below, I am subsetting the variables that I want to include.  The variables consistent of counts, ordinal, and binary variables.  These data are matched pairs from intake to 6-month (.x is intake and .y is 6-month).

Below I demonstrate first how I selected variables that myself and a team of content experts believed were potentially predictive of housing at 6-months.  I then identified that all the "VT" variables were missing (insert number).  Given the VT variables were missing more than 50% and research generally discourages imputing data (discussed later) with more than 50% data, we removed those variables.

We then created several binary demographics from nominal variables.  First we evaluated the top three primary diagnoses as baseline and created binary variables for those diagnoses.  We also created a sexual identity as another sexual identity and heterosexual and a gender variable as male and all other gender identities.  Finally, we created the outcome variable housing, which is one for housed and zero for not housed.  
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
telehealth_noms_wide_noms = read.csv("telehealth_noms_wide_noms.csv", header = TRUE)
machine_dat =  telehealth_noms_wide_noms[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "SchoolOrWork.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "ViolenceTrauma.x", "VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "PhysicallyHurt.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "EnoughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x", "telehealth.x", "NightsHospitalMHC.y", "NightsDetox.y", "NightsJail.y", "TimesER.y", "Housing.y", "NumTimesArrested.y")]
library(naniar)
head(machine_dat)
miss_var_summary(machine_dat)
# All the VT's are missing a lot of data
machine_dat = machine_dat[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "PhysicallyHurt.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "EnoughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x", "telehealth.x", "NightsHospitalMHC.y", "NightsDetox.y", "NightsJail.y", "TimesER.y", "Housing.y", "NumTimesArrested.y")]
miss_var_summary(machine_dat)

apply(machine_dat,2, function(x){describe.factor(x)})

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
machine_dat$hous


```
############
Pre processing the rest of data according to CARET: 

We then took additional steps to pre-process the data.  First, we identified variables with near zero variance.  

We found to variables that the team decided we not critical and we had no way of increasing the variance (hispanic and physical hurt) from the data set.

Next we identified that drug use, nights / times in the hospital or ER, and times arrested and nights in jail as low variance.  Given the importance of these variables, we combined each of them respectively.  Although, some variables had mixed types (i.e., nights and times), our goal is to predict housing not necessarily to have meaningful variables.  
```{r}
library(caret)
## Identify zero or near zero variance predictors
nzv = nearZeroVar(machine_dat, saveMetrics = TRUE)
nzv
### Drop these variables as they are not critical to the analysis
# HispanicLatino.x, PhysicallyHurt.x
machine_dat$HispanicLatino.x = NULL
machine_dat$PhysicallyHurt.x = NULL

# Try adding these variables together Cocaine_Use.x, Meth_Use.x, StreetOpioids_Use.x
machine_dat$drug_use = machine_dat$Cocaine_Use.x + machine_dat$Meth_Use.x + machine_dat$StreetOpioids_Use.x + machine_dat$RxOpioids_Use.x
machine_dat$Cocaine_Use.x = NULL
machine_dat$Meth_Use.x = NULL
machine_dat$StreetOpioids_Use.x = NULL
machine_dat$RxOpioids_Use.x = NULL

### Try adding these variables together NightsHospitalMHC.x, NightsDetox.x, and TimesER.x for binary variable
machine_dat$er_hos_use_base = machine_dat$NightsDetox.x + machine_dat$NightsHospitalMHC.x + machine_dat$TimesER.x
machine_dat$er_hos_use_base = ifelse(machine_dat$er_hos_use > 0, 1, 0)
### Drop other variables
machine_dat[,c("NightsDetox.x", "NightsHospitalMHC.x", "TimesER.x")] = list(NULL)

### Try adding these variables together NumTimesArrested.x and NightsJail.x
machine_dat$jail_arrest_base = machine_dat$NumTimesArrested.x + machine_dat$NightsJail.x
machine_dat$jail_arrest_base = ifelse(machine_dat$jail_arrest_base > 0,1,0)
machine_dat$NightsJail.x = NULL
machine_dat$NumTimesArrested.x = NULL

nzv = nearZeroVar(machine_dat, saveMetrics = TRUE)
nzv

```
Our next step is to identify if there are any high (i.e., .75 or greater) correlation between variables.  We used a spearman correlation as most variables are not normally distriubted and also a (insert) for the correlation between the binary variables and ordinal as well as a (insert) for the binary and binary correlations 
```{r}
descCor = cor(machine_dat, use = "pairwise.complete.obs", method = "spearman")
hig_corr = findCorrelation(descCor)
hig_corr
```

Next we are evaluating the missing data.  We are using the Amelia package with five imputations.  The Amila package has advantages over other packages (MICE), because we can set the type of variable (i.e., nominal, log).  In my personal experience I have found the noms function works for binary variables better than binary and a ordinal regression with binary data reduces to a logistic regression (cite).  More information on data imputation from the Centerstone team is availble here
and package documentation is avialable here

Additionally, we reviewed the densities of imputed and actual values finding general overlap
```{r}
miss_var_summary(machine_dat)
prop_miss_case(machine_dat)

library(Amelia)
machine_dat
miss_var_summary(machine_dat)
a.out_noms = amelia(x = machine_dat, m = 5, noms = c("Gender.x", "RaceWhite.x", "RaceBlack.x", "Housing.x", "Housing.y", "anxiety", "mdd_r", "mdd_s", "another_sex_ident", "er_hos_use_base", "jail_arrest_base", "er_hos_use_month6", "jail_arrest_month6"), ords = c("LivingConditionsSatisfaction.x"), logs = c("drug_use"))
saveRDS(a.out_noms, file = "a.out_noms.rds")

compare.density(a.out_noms, var = "LivingConditionsSatisfaction.x")
compare.density(a.out_noms, var = "GetsAlongWithFamily.x")
compare.density(a.out_noms, var = "RelationshipSatisfaction.x")
compare.density(a.out_noms, var = "drug_use")

a.out_noms = readRDS(file = "a.out_noms.rds")
impute_dat_noms = a.out_noms$imputations

test_dat = impute_dat_noms$imp1
hist(as.numeric(test_dat$LivingConditionsSatisfaction.x))
range(as.numeric(test_dat$LivingConditionsSatisfaction.x))

```
I made sure that all factor (i.e., binary variables) were treated as binary and that al numeric (i.e., nominal and cout) were treated as counts
```{r}

test_dat[,-c(1,5:30, 33:42, 49)] = apply(test_dat[,-c(1,5:30, 33:42, 49)], 2, function(x){as.factor(x)})

test_dat[,c(1,5:30, 33:42, 49)] = apply(test_dat[,c(1,5:30, 33:42, 49)], 2, function(x){as.numeric(x)})

write.csv(test_dat, "test_dat.csv", row.names = FALSE)

test_dat = read.csv("test_dat.csv", header  =TRUE)
### Create seperate data sets to you don't have to write every variable out
test_dat_Housing.y = test_dat 
test_dat_Housing.y$jail_arrest_month6 = NULL
test_dat_Housing.y$er_hos_use_month6 = NULL
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



