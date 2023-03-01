### Data Preprocessing ###

install.packages("haven") #for reading SPSS data and STATA data
library(haven)

install.packages("lubridate") #for dealing with dates and ages
library(lubridate)

install.packages("tidyr") 
library(tidyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("car")
library(car)

install.packages("dplyr")
library(dplyr)

install.packages("corrr")
library(corrr)

#path analysis packages
install.packages("lavaan")
library(lavaan)
install.packages("semPlot")
library(semPlot)
install.packages("semTools")
library(semTools)

### reading in data
#adult behavior rating inventory of executive function
adult_brief <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\adult brief.dta")
#age
age <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\age.dta")
#prenatal exposure status
prenatal_exp <- read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\prenatal exposure.dta")
#conflict in adolescent dating relationships data
partner_relationship <-read_dta("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\partner relationship.dta")
#childhood maltreatment 
ctq <-read_dta ("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\ctq.dta")
#birthweight data
birthweight <- read_csv("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\raw data\\birthweights.csv")
#adolescent substance use data
tasi <- read_csv("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\raw data\\TASI1.csv")


### cleaning childhood trauma questionnaire data ###

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
summary(briefadult$adultBRIEFage) #age ranges between 17 and 22 (mean is 22.08)
## subsetting a group of 18 and 19 year olds ##
late_adolescent_BRIEF <- subset(briefadult, adultBRIEFage >= 17 & adultBRIEFage <= 22)
summary(late_adolescent_BRIEF) #confirming the age range
length(unique(late_adolescent_BRIEF$id))

## merging with ctq_clean
master_data_2 <- merge(late_adolescent_BRIEF, ctq_clean, by.x = c("id","yob"), by.y = c("id", "yob"), sort = TRUE) #191 rows
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
## subsetting a group of 22 to 25 year olds ##
adolescent_datingviolence <- subset(datingviolence, datingviolenceage >= 22 & datingviolenceage <= 25)
summary(adolescent_datingviolence) #confirming that the range is now 21 - 25 which it is
length(unique(adolescent_datingviolence$id)) #166 ppts
## merging with master data
master_data_3 <- merge(adolescent_datingviolence, master_data_2, by.x = c("id","yob"), by.y = c("id", "yob"), sort = TRUE) #332 rows
head(master_data_3)
# sample size
length(unique(master_data_3$id)) #we have 136 ppts when we use adultBRIEF only (with range between 17-22), and dv between 22 and 25



### prenatal exposure ###
## selecting columns
prenatalexp_clean <- prenatal_exp[,c(1,2,4,5,10,16)]
head(prenatalexp_clean)
#0 is no drug use
#1 is prenatal drug use but not cocaine
#2 is cocaine use

## merging
master_data_4 <- merge(master_data_3, prenatalexp_clean, by.x = c("id", "yob"), by.y = c("id","yob"), sort = TRUE)
#1 is African American
#2 is Hispanic
#3 White

#sample size
length(unique(master_data_4$id)) #still 136 ppts



### computing averageBRIEF and DV by participant ID ###
averageDV <- as.data.frame(aggregate(DatingViolenceTotal ~ id, master_data_4, mean ))
averageBRIEF <- as.data.frame(aggregate(adultBRIEFave ~ id, master_data_4, mean))                          
## keeping only the necessary columns
master_data_5 <- master_data_4[,c(1,2,c(12:17))]
## merging with averages
averages_master <- merge(master_data_5, averageDV, by.x = c("id"), by.y = c("id"), sort = TRUE)
averages_master <- merge(averages_master, averageBRIEF, by.x = c("id"), by.y = c("id"), sort = TRUE)
## removing duplicates
averages_master_clean <- averages_master %>% distinct() 
averages_master_clean <- na.omit(averages_master_clean) #124  complete cases

### merging with birthweight ###
averages_master_clean <- merge(averages_master_clean,birthweight, by.x = c("id"), by.y = c("id"), sort = TRUE)

### renaming the 'male' column to 'sex' ###
#1 = male, 0 = female
colnames(averages_master_clean)[8] = "sex"

### writing a csv (excluding adolescent substance use data)
write.csv(averages_master_clean, file = "C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\averages_master_clean_Feb.csv")

### adolescent substance use data ###
colnames(tasi)[1] ="id" #making the ppt id column name "id"
summary(tasi)
## retaining only participants with subsance use that began below the age of 18 and participants that never used substance
adolescent_tasi <- subset(tasi, tasi$SubstanceUseInitAge <= 17  |is.na(tasi$SubstanceUseInitAge))
## returns 1 if there is any substance use reported and 0 if not
tasi_clean <- adolescent_tasi %>% group_by(id) %>% summarize(max_SubstanceUseAny = max(SubstanceUseAny)) 
## merging
master_data_6 <- merge(averages_master_clean, tasi_clean, by.x = c("id"), by.y = c("id"), sort = TRUE)
length(unique(master_data_6$id)) #121 ppts
master_data_6 <- na.omit(master_data_6) 
summary(master_data_6) #106 complete cases
## exporting as csv
write.csv(master_data_6, file = "C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\master with substance use.csv")

### dummy coding ###
## making categorical variables factors with levels
master_data_6$mdrug <- factor(master_data_6$mdrug, labels = c("nosubstance", "noncocainesubstance","cocaine"))
master_data_6$sex <- factor(master_data_6$sex, labels = c("female","male"))
master_data_6$mhiedu <- factor(master_data_6$mhiedu, labels = c("belowhighschool","highschool"))
master_data_6$mrace <- factor(master_data_6$mrace, labels = c("AfricanAmerican", "Hispanic","Caucasian"))
master_data_6$max_SubstanceUseAny <- factor(master_data_6$max_SubstanceUseAny, labels = c("noSU","SU"))


### z-scaling variables ###
#scaling variables (z-transforms so that they are on the same scale)
master_data_6 = cbind(master_data_6, scale_DatingViolenceTotal = scale(master$DatingViolenceTotal))
master_data_6 = cbind(master_data_6, scale_combinedBRIEF = scale(master$adultBRIEFave))
master_data_6 = cbind(master_data_6, scale_ctqtotal = scale(master$ctqtotal))
master_data_6 = cbind(master_data_6, scale_birthweight = scale(master$birthweight))


