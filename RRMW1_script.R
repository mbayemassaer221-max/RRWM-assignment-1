##############################################################################
##                           RRWM 1 activity 2025                           ##                              
##                   by Massaer Mbaye, ESG - UQAM                           ##
##############################################################################
##                          Program and codes                               ##
##############################################################################



# Program -----------------------------------------------------------------

#Objective: This program aims to study the impact 
# of the level of education on wages

#Data: we used the 2016 Canadian population census microdata

#Descriptive statistics: we focused on the following variables
# education level, wage, age, sex, and province

#we used these variables to compute some descriptive statistics
#to get an overview of our topic

#Selected descriptive statistics: education and sex, income and sex,
#income and province, education and province, and income and education

#Model: we used a simple OLS with fixed effects

#Regression 1: relationship between education and income
#Regression 2: R1 + robustness
#Regression 3: R1 + controls (sex)
#Regression 4: R3 + robustness
#Regression 5: R3 + province fixed effects
#Regression 6: R5 + age fixed effects

# Install Packages --------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(haven)
library(dplyr)
library(scales)
library(sf)
library(lmtest)
library(sandwich)
library(fixest)

options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(c("rnaturalearth", "rnaturalearthdata"))
install.packages(c("mapcan"))
install.packages("fixest")

# Load data ---------------------------------------------------------------
census_2016<- read_csv('census_2016.csv')

view(census_2016)

rm(my_data)
# Create my own dataset -----------------------------------

my_data <- 
  census_2016 |> 
  select(PPSORT, Sex, MarStH, PR, DPGRSUM, AGEGRP, POB, EmpIn, HDGREE)

my_data


# Clean my_data -----------------------------------------------------------

#Clean sex variable
my_data |> count(Sex)

my_data |>
  mutate(Sex = factor(Sex, levels = c(1, 2), labels = c("Female", "Male"))) |>
  count(Sex, name = "n")

#Clean education variable

my_data <-
 my_data |> 
  rename(Education = HDGREE)

 
my_data |> count(Education)

primary <- c(1, 2, 3, 4, 5, 6)

secondary <- c(7, 8, 9, 10, 11)

tertiary <- c(12, 13)


my_data <- my_data |>  # replace with your object name
  mutate(
    Education = as.integer(Education),
    Education_lev = case_when(
      Education %in% primary      ~ "primary",
      Education %in% secondary    ~ "secondary",
      Education %in% tertiary  ~ "tertiary",
      is.na(Education)               ~ NA_character_,
      TRUE                         ~ "Other"
    ),
    Education_lev = factor(
      Education_lev,
      levels = c("primary","secondary","tertiary")
    )
  )

my_data <- my_data |>
  drop_na(Education_lev) 


# Clean income variable ---------------------------------------------------

my_data <-
  my_data |> 
  rename(Income = EmpIn)

my_data <- my_data |>
  filter(!(Income %in% c(99999999, 99999998, 99999997)))


# Clean age variable
my_data <- my_data |>
  filter(!is.na(AGEGRP) & AGEGRP >= 15)

my_data <-
  my_data |> 
  rename(age = AGEGRP)


# Clean Province variable ----------------------------------------
my_data <-
  my_data |> 
  rename(province = PR)

my_data |> count(province)


prov_labels <- c(
  `10` = "Newfoundland and Labrador",
  `11` = "Prince Edward Island",
  `12` = "Nova Scotia",
  `13` = "New Brunswick",
  `24` = "Quebec",
  `35` = "Ontario",
  `46` = "Manitoba",
  `47` = "Saskatchewan",
  `48` = "Alberta",
  `59` = "British Columbia",
  `70` = "Northern Canada"
)

my_data <- my_data %>%
  mutate(
    province = factor(
      as.integer(province),
      levels = as.integer(names(prov_labels)),
      labels = unname(prov_labels)
    )
  )


# Descriptives Statistics -------------------------------------------------

#Education & sex
summary(my_data$Education_lev)
summary(my_data$Sex)


plot_df <- my_data |>
  mutate(
    Sex = factor(Sex, levels = c(1, 2), labels = c("Female", "Male")),
    Education_lev = factor(Education_lev, levels = c("primary","secondary","tertiary"))
  ) |>
  tidyr::drop_na(Sex, Education_lev)

ggplot(plot_df, aes(x = Education_lev, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Education level", y = "Count", fill = "Sex",
       title = "Education by sex") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

#income & sex

my_data |>
  mutate(Sex = factor(as.integer(Sex), levels = c(1, 2),
                      labels = c("Female", "Male"))) |>
  group_by(Sex) |>
  summarise(mean_income = mean(Income, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = Sex, y = mean_income, fill = Sex)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Mean income by sex", x = NULL, y = "Mean income") +
  theme_minimal() 

#income and province

my_data |>
  filter(!is.na(Income), !is.na(province), Income > 0) |>
  mutate(province = fct_reorder(province, Income, .fun = median, na.rm = TRUE)) |>
  ggplot(aes(x = province, y = Income)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.2) +
  coord_flip() +
  scale_y_log10(labels = dollar_format()) +
  labs(title = "Income by province (log scale)", x = NULL, y = "Income (log10)") +
  theme_minimal(base_size = 12)


#Education_lev and province
my_data |> 
  filter(!is.na(province), !is.na(Education_lev)) |>
  mutate(Education_lev = factor(Education_lev, levels = c("primary","secondary","tertiary"))) |>
  ggplot(aes(x = province, fill = Education_lev)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Répartition de l'éducation par province",
       x = NULL, y = "Part", fill = "Niveau d'éducation") +
  coord_flip() +
  theme_minimal(base_size = 12)

my_data |>
  filter(!is.na(province), !is.na(Education_lev)) |>
  mutate(Education_lev = factor(Education_lev, levels = c("primary","secondary","tertiary"))) |>
  ggplot(aes(x = Education_lev)) +
  geom_bar(width = 0.7, fill = "#4C78A8") +
  facet_wrap(~ province, ncol = 3) +
  labs(title = "Effectifs par niveau d'éducation, par province",
       x = "Niveau d'éducation", y = "Effectif") +
  theme_minimal(base_size = 12)

#income and education
my_data |>
  filter(!is.na(Income), !is.na(Education_lev)) |>
  group_by(Education_lev) |>
  summarise(mean_income = mean(Income), .groups = "drop") |>
  ggplot(aes(x = factor(Education_lev, c("primary","secondary","tertiary")),
             y = mean_income, fill = Education_lev)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Mean income by education level", x = NULL, y = "Mean income") +
  theme_minimal(base_size = 12)


# Regressions -------------------------------------------------------------
df <- my_data |>
  filter(!is.na(Income), Income > 0, !is.na(Education_lev)) |>
  mutate(
    Education_lev = factor(Education_lev, levels = c("primary","secondary","tertiary"))
  )

# OLS sur log(Income) ~ Education (base variable : primary and female)
m <- lm(log(Income) ~ Education_lev, data = df)

#Naive
coeftest(m)

#Robustesse
coeftest(m, vcov = vcovHC(m, type = "HC1"))


m1 <- lm(log(Income) ~ Education_lev + Sex, data = df)

coeftest(m1)

coeftest(m1, vcov = vcovHC(m, type = "HC1"))

#province fixed effets
m1_fe <- feols(log(Income) ~ Education_lev + Sex | province, data = df)
summary(m1_fe, cluster = "province")

#age fixed effets
df$age_fac <- factor(df$age)
m_age_fe <- feols(log(Income) ~ Education_lev + Sex | province + age_fac, data = df)
summary(m_age_fe, cluster = "province")



class(df$age)
