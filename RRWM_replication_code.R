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
RRWM<- read_csv('census_2016.csv')

table(RRWM$AGEGRP, useNA = "ifany")


view(RRWM)

# Create my own dataset -----------------------------------

my_data_1 <- 
  RRWM |> 
  select(PPSORT, Sex, AGEGRP, EmpIn, HDGREE)

view(my_data_1)


# Clean my_data -----------------------------------------------------------

#Clean sex variable
my_data_1 |> count(Sex)

my_data_1 |>
  mutate(Sex = factor(Sex, levels = c(1, 2), labels = c("Female", "Male"))) |>
  count(Sex, name = "n")

#Clean education variable

my_data_1 <-
  my_data_1 |> 
  rename(Education = HDGREE)


my_data_1 |> count(Education)

primary <- c(1, 2, 3, 4, 5, 6)

secondary <- c(7, 8, 9, 10, 11)

tertiary <- c(12, 13)


my_data_1 <- my_data_1 |>  # replace with your object name
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

my_data_1 <- my_data_1 |>
  drop_na(Education_lev) 

my_data_1 |> count(Education)

# Clean income variable ---------------------------------------------------

my_data_1 <-
  my_data_1 |> 
  rename(Income = EmpIn)

my_data_1 <- my_data_1 |>
  filter(!(Income %in% c(1, 99999999, 99999998, 99999997, 88888888)) & Income >= 0)

# Clean age variable
my_data_1 <- my_data_1 |>
  filter(!is.na(AGEGRP) & AGEGRP >= 6)

my_data_1 <-
  my_data_1 |> 
  rename(age = AGEGRP)

my_data_1 <- my_data_1 |>
  drop_na(age) 


# 3.	Create a sample composed exclusively of women between 18 - 59
my_data_2 <- my_data_1 |> 
  filter(age >= 7 & age <= 15)|> 
  filter(Sex == 1)

sum(my_data_2$age)

view(my_data_2)


# 4.	Create a table of summary statistics with the new sample resp 

summary(my_data_2[, c("Sex", "age", "Income", "Education_lev")])


my_data_2 |> 
  count(Education_lev) |> 
  mutate(percent = round(100 * n / sum(n), 1))

my_data_2 |> 
  count(age) |> 
  mutate(percent = round(100 * n / sum(n), 1))

my_data_2 |> 
  summarise(
    mean_income = mean(Income, na.rm = TRUE),
    sd_income   = sd(Income, na.rm = TRUE)
  )

# 5.	Compare income by highest degree completed

a <- lm(log(Income) ~ Education_lev, data = my_data_2)
coeftest(a)


my_data_2 |>
  filter(!is.na(Income), !is.na(Education_lev)) |>
  group_by(Education_lev) |>
  summarise(mean_income = mean(Income), .groups = "drop") |>
  ggplot(aes(x = factor(Education_lev, c("primary","secondary","tertiary")),
             y = mean_income, fill = Education_lev)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Mean income by education level", x = NULL, y = "Mean income") +
  theme_minimal(base_size = 12)


# 6.	Compare income by age groups.
view(my_data_2)
a1 <- lm(log(Income) ~ age, data = my_data_2)
coeftest(a1)


my_data_2 |> 
  filter(!is.na(Income), !is.na(age)) |> 
  group_by(age) |> 
  summarise(mean_income = mean(Income, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = age, y = mean_income, fill = age)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Mean income by age group",
       x = "Age group",
       y = "Mean income") +
  theme_minimal(base_size = 12)
