### Data Preprocessing ###

#install.packages("haven") #for reading SPSS data and STATA data
library(haven)
#install.packages("lubridate") #for dealing with dates and ages
library(lubridate)
#install.packages("tidyr") 
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("car")
library(car)

#path analysis packages
library(lavaan)
library(semPlot)
library(semTools)

### reading in data
adult_brief <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\adult brief.dta")
age <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\age.dta")
prenatal_exp <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\prenatal exposure.dta")
partner_relationship <-read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\partner relationship.dta")
ctq <-read_dta ("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\ctq.dta")
child_brief <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\brief.dta")
birthweight <- read_csv("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\raw data\\birthweights.csv")



### cleaning childhood trauma questionnaire data###
ctq_clean <- ctq[,c(1,5,46)]
head(ctq_clean) #now just includes total ctq, the date of the ctq (to calculate the age of ctq) and PID
## calculating age of CTQ
age_clean <- age[,c(1,3)]
## merging with ctq_clean
ctq_clean <- merge(ctq_clean, age_clean, by.x = c("id"), sort = TRUE)
## removing duplicates
ctq_clean <- ctq_clean %>% distinct() #420 rows
ctq_clean <- na.omit(ctq_clean) #242 complete cases with CTQ data - fine for this data because it was collected at a single timepoint
## calculating age
ctq_clean$CTQage = ctq_clean$ctqyear-ctq_clean$yob
## summary of age at which CTQ was done
summary(ctq_clean$CTQage) #done at a mean age of 15.59 years of age



### child BRIEF (early adolescent EF) ###
## summary of year of completion
summary(child_brief$briefyear) #year range between 2007 and 2016
## selecting only necessary columns
child_BRIEF <- child_brief[,c(1,3,c(6:85))] #just the columns we need
## calculating total BRIEF and average BRIEF
child_BRIEF$BRIEFtotal <- rowSums(child_BRIEF[,c(3:82)])
child_BRIEF$BRIEFave = child_BRIEF$BRIEFtotal/80
## cleaning columns
childBRIEF <- child_BRIEF[,c(1,2,84)] 
## merging childBRIEF and yob
briefchild <- merge(childBRIEF, age_clean, by.x = "id", sort = TRUE)
## removing duplicates
briefchild <- briefchild %>% distinct() #1362 rows
briefchild <- na.omit(briefchild) #633 rows
length(unique(briefchild$id)) #219 ppts
## calculating age
briefchild$BRIEFage = briefchild$briefyear-briefchild$yob
## summary of ages
summary(briefchild$BRIEFage) #age ranges between 14 and 22
## subsetting a group of 14 and 15 year olds ##
early_adolescent_BRIEF <- subset(briefchild, briefchild$BRIEFage == "14" | briefchild$BRIEFage =="15")
summary(early_adolescent_BRIEF) #checking that this only has 14 and 15 year olds 
## merging with CTQ
master_data <- merge(early_adolescent_BRIEF, ctq_clean, by.x = c("id","yob"), by.y = c("id", "yob"), sort = TRUE) #155 rows
head(master_data)



### adult BRIEF (late adolescent EF) ###

## selecting columns
adult_BRIEF <- adult_brief[,c(1,3,c(7:81))]
## computing total and average BRIEF in adulthood
adult_BRIEF$adultBRIEFtotal <- rowSums(adult_BRIEF[,c(3:77)])
adult_BRIEF$adultBRIEFave <- adult_BRIEF$adultBRIEFtotal/75
adultBRIEF <- adult_BRIEF[,c(1,2,79)] #selecting just the columns we need

## merging adultBRIEF and yob
briefadult <- merge(adultBRIEF, age_clean, by.x = "id", sort = TRUE)
#removing duplicates
briefadult <- briefadult %>% distinct() #1633 rows
briefadult <- na.omit(briefadult) #722 rows

## calculating age
briefadult$adultBRIEFage = briefadult$aefyear-briefadult$yob
summary(briefadult$adultBRIEFage) #age ranges between 18 and 29 (mean is 22.08)
## subsetting a group of 18 and 19 year olds ##
late_adolescent_BRIEF <- subset(briefadult, adultBRIEFage >= 17 & adultBRIEFage <= 20)
summary(late_adolescent_BRIEF) #confirming that the range is now 18-19
length(unique(late_adolescent_BRIEF$id)) #139 ppts
## merging with master data
master_data_2 <- merge(late_adolescent_BRIEF, master_data, by.x = c("id","yob"), by.y = c("id", "yob"), sort = TRUE) #191 rows
head(master_data_2)



### partner relationship (young adult relationship violence) ###
## selecting the columns we need
datingviolence <- partner_relationship[,c(1,3,83,89)]
## calculating total dating violence
datingviolence$DatingViolenceTotal = datingviolence$pertotal + datingviolence$victotal
head(datingviolence)
## calculating age
datingviolence <- merge(datingviolence, age_clean, by.x = "id", sort = TRUE)
#removing duplicates
datingviolence <- datingviolence %>% distinct() #2204 rows
datingviolence <- na.omit(datingviolence) #1221 rows
datingviolence$datingviolenceage = datingviolence$parrelyear-datingviolence$yob
summary(datingviolence$datingviolenceage) #age ranges between 15 and 29 (mean is 20.82)
## subsetting a group of 21 to 25 year olds ##
adolescent_datingviolence <- subset(datingviolence, datingviolenceage >= 21 & datingviolenceage <= 25)
summary(adolescent_datingviolence) #confirming that the range is now 21 - 25 which it is
length(unique(adolescent_datingviolence$id)) #166 ppts
## merging with master data
master_data_3 <- merge(adolescent_datingviolence, master_data_2, by.x = c("id","yob"), by.y = c("id", "yob"), sort = TRUE) #332 rows
head(master_data_3)


### prenatal exposure
## selecting columns
prenatalexp_clean <- prenatal_exp[,c(1,2,4,5,10,16)]
head(prenatalexp_clean)
#0 is no drug use
#1 is prenatal drug use but not cocaine
#2 is cocaine use
#1 is African American
#2 is Hispanic
#3 White
## merging
master_data_4 <- merge(master_data_3, prenatalexp_clean, by.x = c("id", "yob"), by.y = c("id","yob"), sort = TRUE)



## merging with  averages_master_comp
master_data_5 <- merge(averages_master_comp, birthweight, by.x = c("id"), by.y = c("id"), sort = TRUE)
colnames(master_data_5)[20] = "sex"
write.csv(master_data_5, file = "C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\master_data_5.csv")
#sample size is now 98
averages_master_comp <- na.omit(master_data_5)


### dummy coding
averages_master$mdrug <- factor(averages_master$mdrug,
                    levels = c(0,1,2),
                    labels = c("no substance exposure", "non-cocaine substance", "cocaine"))

averages_master$mrace <- factor(averages_master$mrace,
                    levels = c(1,2,3),
                    labels = c("African American", "Hispanic", "Caucasian"))

averages_master$mhiedu <- factor(averages_master$mhiedu,
                    levels = c(0,1),
                    labels = c("below highschool", "at least highschool"))

averages_master$male <- factor(averages_master$male,
                    levels = c(0,1),
                    labels = c("female", "male"))


averages_master_dummy = cbind(averages_master, dummy.code(averages_master$mdrug))
averages_master_dummy = cbind(averages_master_dummy, dummy.code(averages_master$mrace))
averages_master_dummy = cbind(averages_master_dummy, dummy.code(averages_master$mhiedu))
averages_master_dummy = cbind(averages_master_dummy, dummy.code(averages_master$male))

## complete cases only
averages_master_comp <- na.omit(averages_master_dummy)

## scaling variables (z-transforms so that they are on the same scale)
averages_master_comp$DatingViolenceTotal <- scale(averages_master_comp$DatingViolenceTotal)
averages_master_comp$combinedBRIEF <- scale(averages_master_comp$combinedBRIEF)
averages_master_comp$birthweight <- scale(averages_master_comp$birthweight)
averages_master_comp$ctqtotal <- scale(averages_master_comp$ctqtotal)
averages_master_comp$adultBRIEFave <- scale(averages_master_comp$adultBRIEFave)
averages_master_comp$BRIEFave <- scale(averages_master_comp$BRIEFave)

## renaming some variables to not have spaces
colnames(averages_master_comp)[23] ="nosubstanceexposure"
colnames(averages_master_comp)[24] ="noncocainesubstance"
colnames(averages_master_comp)[28] ="highschool"
colnames(averages_master_comp)[29] ="belowhighschool"
colnames(averages_master_comp)[25] ="AfricanAmerican"

