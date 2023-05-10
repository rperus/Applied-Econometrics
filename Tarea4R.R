rm(list = ls())
#install.packages("plm")
library(AER)
library(sandwich)
library(stargazer)
library(plm)

# I opened the data 
ds4 <- read.csv("Z:\\OneDrive\\Ms Fintech\\Applied Econometrics 1\\Tarea 4\\dataset4.csv",  header = TRUE)

#1 subset of the data that includes only the year 2015
ds2015 <- ds4[ds4$year==2015, ]

#1a 
#Regress log(output) on a constant, log(employment) and foreign dummy variable
reg_1a <- lm(log_output ~ log_employment + foreign, data=ds2015)
coeftest(reg_1a,vcov = vcovHC, type = "HC1")

#1b
#Regress log(output) on a constant and the foreign dummy variable
reg_1b <- lm(log_output ~ foreign, data=ds2015)
coeftest(reg_1b,vcov = vcovHC, type = "HC1")
#1b Calculate the residuals
residuals_1b <- reg_1b$residuals  
View(residuals_1b)

#1c
#Regress the log(employment) on a constant and the foreign dummy variable. 
reg_1c <- lm(log_employment ~ foreign, data=ds2015)
coeftest(reg_1c,vcov = vcovHC, type = "HC1")
#1c Calculate the residuals
residuals_1c <- reg_1c$residuals  
View(residuals_1c)

#4d
reg_1d <- lm(residuals_1b ~ residuals_1c  , data=ds2015)
coeftest(reg_1d,vcov = vcovHC, type = "HC1")