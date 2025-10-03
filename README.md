# Education and Wages – Analysis

## Objective
This project studies the impact of education level on wages.

## Data
We use Canadian population census microdata (2016).

## Descriptive Statistics
We focus on the following variables:
- Education level  
- Wage  
- Age  
- Sex  
- Province  

Exploratory statistics include:
- Education × Sex  
- Income × Sex  
- Income × Province  
- Education × Province  
- Income × Education  

## Model
We estimate a simple OLS model with fixed effects.

### Regression specifications
1. Education → Income  
2. R1 + robust standard errors  
3. R1 + control for sex  
4. R3 + robust standard errors  
5. R3 + province fixed effects  
6. R5 + age fixed effects  
