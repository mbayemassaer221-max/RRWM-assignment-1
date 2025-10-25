library(tidyverse)
#Step 1, rename the dataset-----------------------------------
RRWM <- pumf_98M0001_E_2016_individuals_F1

view(RRWM)

RRWM

#Step 2, clean the variables------------------------------------
names(RRWM)

#Sex variable cleaning
summary(RRWM$Sex)
table(RRWM$Sex, useNA = "ifany")
RRWM$Sex[RRWM$Sex == "*"] <- NA
table(RRWM$Sex, useNA = "ifany")
sum(is.na(RRWM$Sex)) #no missing value 

#HDGREE variable cleaning 
summary(RRWM$HDGREE)
table(RRWM$HDGREE, useNA = "ifany")
RRWM$HDGREE[RRWM$HDGREE == "*"] <- NA
RRWM$HDGREE[RRWM$HDGREE == "99"] <- NA
table(RRWM$HDGREE, useNA = "ifany") #157131 missing values 

library(dplyr)

RRWM <- RRWM %>%
  filter(!is.na(HDGREE))
table(RRWM$HDGREE, useNA = "ifany") #missing values removed 

#AGEGRP variable cleaning
summary(RRWM$AGEGRP)
table(RRWM$AGEGRP, useNA = "ifany")
RRWM$AGEGRP[RRWM$AGEGRP == "*"] <- NA
RRWM$AGEGRP[RRWM$AGEGRP == "88"] <- NA
table(RRWM$AGEGRP, useNA = "ifany") #8672 missing values 

RRWM <- RRWM %>%
  filter(!is.na(AGEGRP))
table(RRWM$AGEGRP, useNA = "ifany")

unique(RRWM$AGEGRP)
RRWM <- dplyr::filter(RRWM, !is.na(AGEGRP))
sum(is.na(RRWM$AGEGRP))
table(RRWM$AGEGRP, useNA = "ifany") #missing values removed

#EmpIn variable cleaning 
summary(RRWM$EmpIn)
table(RRWM$EmpIn, useNA = "ifany")
RRWM$EmpIn[RRWM$EmpIn == "*"] <- NA
RRWM$EmpIn[RRWM$EmpIn == "99999999"] <- NA
RRWM$EmpIn[RRWM$EmpIn == "88888888"] <- NA

table(RRWM$EmpIn, useNA = "ifany") #  157131 missing values

RRWM <- RRWM %>%
  filter(!is.na(EmpIn))
table(RRWM$EmpIn, useNA = "ifany")
sum(is.na(RRWM$EmpIn)) #missing values removed 



#Step 3, Create a sample composed exclusively of women between the age of 18 and 59---------

unique(RRWM$AGEGRP)

RRWM_filtered <- RRWM %>%
  filter(AGEGRP >= 7 & AGEGRP <= 15)

RRWM <- RRWM_filtered

table(RRWM$AGEGRP, useNA = "ifany")

#Convert AGEGRP into a factor 

RRWM$AGEGRP <- factor(RRWM$AGEGRP)

#Convert HDGREE into a factor 

RRWM$HDGREE <- factor(RRWM$HDGREE)

#Drop men respondents 

RRWM_filtered <- RRWM %>%
  filter(Sex != 2)
RRWM <- RRWM_filtered

unique(RRWM$Sex)
n_women <- sum(RRWM$Sex != 1)

table(RRWM$Sex, useNA = "ifany") # n = 220 517 women between 18 and 59 years old 

#Step 4, Create a summary statistics table for the new sample respondents ----------------- 

library(janitor)

#Frequencies (n, %) for variables AGEGRP and HDGREE

RRWM %>%
  tabyl(AGEGRP) %>%    
  adorn_pct_formatting(digits = 1) 

RRWM %>%
  tabyl(HDGREE) %>%    
  adorn_pct_formatting(digits = 1) 

#Calculate mean, minimum, maximum, standard deviation and interquartile range for variable

RRWM %>%
  summarise(
    mean = mean(EmpIn, na.rm = TRUE),
    min = min(EmpIn, na.rm = TRUE),
    max = max(EmpIn, na.rm = TRUE),
    sd = sd(EmpIn, na.rm = TRUE),
    iqr = IQR(EmpIn, na.rm = TRUE)
  )

#Step 5, Compare income by highest degree completed--------------------------------

library(ggplot2)

model <- lm(EmpIn ~ HDGREE, data = RRWM)
summary(model) #HDGREE is statistically significant (p < 0.05), higher education levels are strongly associated with higher income

#Step 6, Compare income by age groups 

model <- lm(EmpIn ~ AGEGRP, data = RRWM)
summary(model) # Age explains ~9.8% of income variation, statistically significant (p < 0.05)
