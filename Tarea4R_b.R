rm(list = ls())
#install.packages("plm")
#install.packages("dplyr")
#install.packages("tidyr")
library(AER)
library(sandwich)
library(stargazer)
library(plm)
library(dplyr)
library(tidyr)
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

#1d
reg_1d <- lm(residuals_1b ~ residuals_1c  , data=ds2015)
coeftest(reg_1d,vcov = vcovHC, type = "HC1")


#2ai- APENDIX E
#2ai association of log(employment) and log(capital) to log(output) 
reg_2ai <- lm(formula =log_output ~  log_employment+log_capital+(log_employment*foreign)+(log_capital*foreign), data = ds2015)
coeftest(reg_2ai)


## 2aii intercepts vary between foreign- and domestic-owned plants
reg_2aii <- lm(formula =log_output ~ log_employment + log_capital + foreign, data = ds2015)
coeftest(reg_2aii)

## 2aiii intercepts and slopes vary between foreign- and domestic-owned plants. 
reg_2aiii <- lm(formula =log_output ~ log_employment + log_capital + foreign +(log_employment*foreign) + (log_capital*foreign), data = ds2015)
coeftest(reg_2aiii)



#2bii Test for the equality of all coefficients between foreign-owned and domestic-owned plants.
#association of log(employment) and log(capital) to log(output) 
linearHypothesis(reg_2ai,c("foreign=0"), vcov. = vcovHC, type ="HC1")

#intercepts vary between foreign- and domestic-owned plants
linearHypothesis(reg_2aii,c("foreign=0"), vcov. = vcovHC, type ="HC1")

#intercepts and slopes vary between foreign- and domestic-owned plants. 
linearHypothesis(reg_2aiii,c("foreign=0"), vcov. = vcovHC, type ="HC1")


#3- APENDIX J

reg_3 <- lm(log_output ~hiemployment*hicapital,data=ds2015)
coeftest(reg_3,vcov = vcovHC, type = "HC1")

predict(reg_3, newdata = data.frame("hiemployment" = 0, "hicapital" = 0))
predict(reg_3, newdata = data.frame("hiemployment" = 0, "hicapital" = 1))
predict(reg_3, newdata = data.frame("hiemployment" = 1, "hicapital" = 0))
predict(reg_3, newdata = data.frame("hiemployment" = 1, "hicapital" = 1))


#4
# Is it a balaced Panel?
year  <- length(levels(ds4$year))
id <- length(levels(ds4$id))
year*id == nrow(ds4)

# base model
reg4 <- lm(log(output) ~ age, data = ds4)
coeftest(reg4, vcov. = vcovHC, type = "HC1")
table(ds4$id, ds4$year)

# Balance panel
unbalanced.panel<-  ds4
unbalanced.panel

balanced.panel <- unbalanced.panel %>%  
  complete(nesting(id), year = full_seq(year, period = 1))
balanced.panel

year  <- length(levels(balanced.panel$year))
id <- length(levels(balanced.panel$id))
year*id == nrow(balanced.panel)

# model with entity and time fixed effects
reg4a <- plm(log(output) ~ age, data = balanced.panel, 
             index = c("id", "year"), 
             model = "random",
             effect = "twoways")




# summary 
coeftest(reg4a, vcov. = vcovHC, type = "HC1", cluster="group")



# test if the effects ars jointly significant from zero
pFtest(model_sete, model,vcov = vcovHC, type = "HC1", cluster="group")


#5a
#average plant production function by regressing log_output on the logarithmic-transformed expenditure
reg5 <- lm(log_output ~ log_research, data = ds4)
coeftest(reg5, vcov. = vcovHC, type = "HC1")

#5b
# model with entity and time fixed effects
reg5b <- plm(log_output ~ log_research, data = ds4, index = c("id", "year"), 
             model = "within", effect = "twoways")
coeftest(reg5b, vcov. = vcovHC, type = "HC1", cluster="group")


pFtest(reg5b, reg5, vcov = vcovHC, type = "HC1", cluster="group")




#5c
#Expand your model by including adding BOTH entity and time fixed effects.
#install.packages("Hmisc")
library("Hmisc")
rcorr(as.matrix(ds4))
library(stargazer)

#1OLS
reg5c0 <- lm(log_output ~ log_research, data = ds4)
summary(reg5c0)
coeftest(reg5c0, vcov. = vcovHC, type = "HC1", cluster="group")

#1Time Fixed Effect
reg5c0b <- plm(log_output ~ log_research, data = ds4)
summary(reg5c0b)
coeftest(reg5c0b, vcov. = vcovHC, type = "HC1", cluster="group")

#3Time and entity fixed effect
reg5c0c <- plm(log_output ~ log_research, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0c)
coeftest(reg5c0c, vcov. = vcovHC, type = "HC1", cluster="group")

#4 All the conditions
reg5c0d <- plm(log_output ~ log_research+log_employment+log_capital+export+import+foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0d)
coeftest(reg5c0d, vcov. = vcovHC, type = "HC1", cluster="group")

#5 Exclude plant ownership, log capital, import
reg5c0e <- plm(log_output ~ log_research+log_employment+log_capital, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0e)
coeftest(reg5c0e, vcov. = vcovHC, type = "HC1", cluster="group")

#6
reg5c0f <- plm(log_output ~ log_research+log_capital+export+import+foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0f)
coeftest(reg5c0f, vcov. = vcovHC, type = "HC1", cluster="group")

#7 
reg5c0g <- plm(log_output ~ log_research+log_employment+export+import+foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0g)
coeftest(reg5c0g, vcov. = vcovHC, type = "HC1", cluster="group")

#8 
reg5c0h <- plm(log_output ~ log_research+log_employment+log_capital+export+import+foreign+export*foreign+import*foreign+log_research*foreign+log_employment*foreign+log_capital*foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c0h)
coeftest(reg5c0h, vcov. = vcovHC, type = "HC1", cluster="group")



rob_se <- list(sqrt(diag(vcovHC(reg5c0, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0b, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0c, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0d, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0e, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0f, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0g, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c0h, type = "HC1"))))


fit <- stargazer(reg5c0, reg5c0b, reg5c0c, reg5c0d, reg5c0e, reg5c0f,reg5c0g,reg5c0h,
                 digits = 3,
                 header = FALSE,
                 type = "text", 
                 align=TRUE,
                 column.sep.width = "0.5pt",
                 no.space = TRUE,
                 se = rob_se,
                 font.size = "large",
                 title = "Regression results.",
                 model.numbers = FALSE,
                 dep.var.labels = "Average Test Scores",
                 column.labels = c("OLS", "Time Fix", "T&E Fix", "4", "5", "6", "7", "8","9"),
                 omit.stat = c("f", "ser"))














reg5c1 <- plm(log_output ~ log_employment+log_capital+export+import+log_research+log_employment*foreign+log_capital*foreign+export*foreign+import*foreign+log_research*foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c1)
coeftest(reg5c1, vcov. = vcovHC, type = "HC1", cluster="group")

reg5c2 <- plm(log_output ~ log_employment+log_capital+export+import+log_research+log_employment*foreign+log_capital*foreign+export*foreign+import*foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c2)
coeftest(reg5c2, vcov. = vcovHC, type = "HC1", cluster="group")

reg5c3 <- plm(log_output ~ log_employment+log_capital+export+import+log_research+log_employment*foreign+export*foreign+import*foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c3)
coeftest(reg5c3, vcov. = vcovHC, type = "HC1", cluster="group")

reg5c4 <- plm(log_output ~ log_employment+log_capital+export+import+log_research+export*foreign+import*foreign, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c4)
coeftest(reg5c4, vcov. = vcovHC, type = "HC1", cluster="group")

reg5c5 <- plm(log_output ~ log_employment+log_capital+export+import+log_research, data = ds4, index = c("id", "year"),model = "within", effect = "twoways")
summary(reg5c5)
coeftest(reg5c5, vcov. = vcovHC, type = "HC1", cluster="group")


rob_se <- list(sqrt(diag(vcovHC(reg5c1, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c2, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c3, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c4, type = "HC1"))),
               sqrt(diag(vcovHC(reg5c5, type = "HC1"))))


fit <- stargazer(reg5c1, reg5c2, reg5c3, reg5c4, reg5c5, 
                 digits = 3,
                 header = FALSE,
                 type = "text", 
                 align=TRUE,
                 column.sep.width = "0.5pt",
                 no.space = TRUE,
                 se = rob_se,
                 font.size = "large",
                 title = "Regression results.",
                 model.numbers = FALSE,
                 dep.var.labels = "Average Test Scores",
                 column.labels = c("1", "2", "3", "4", "5"),
                 omit.stat = c("f", "ser"))



