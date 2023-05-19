install.packages("ltm")
library(ltm)
library(haven)
library(foreign)
library(tidyr)
library(tidyverse)
cleanID <- read_csv("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\averages_master_clean_April.csv")

subset_BRIEF <- subset(adult_BRIEF, id %in% cleanID$id)
subset_BRIEF <- as.data.frame(subset_BRIEF[, -(c(1,2,78,79))])

cronbach.alpha(subset_BRIEF, CI = TRUE, 
               probs = c(0.025, 0.975), na.rm = TRUE)

#0.969