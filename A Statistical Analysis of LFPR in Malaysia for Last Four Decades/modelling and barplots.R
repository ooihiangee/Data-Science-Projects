setwd("C:/Users/User10/Desktop/CA2/Datasets")
library(tidyverse)
library(readxl)
library(foreign)
library(gplots)
library(plm)
library(stargazer)

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

# Read Male LFPR Details
data <- read_excel("overall.xlsx", sheet = "Male LFPR (No NA)")
states <- data$States
years <- data$Years
male_lfpr <- data$Overall
primary <- data$Primary
secondary <- data$Secondary
tertiary <- data$Tertiary
single <- data$`Never Married`
married <- data$Married
widowed <- data$Widowed
divorced <- data$Divorced
young <- data$`Young Age`
middle <- data$`Middle Age`
old <- data$`Old Age`

# Means of Male LFPR
plotmeans(lfpr ~ years, xlab = "Years", ylab = "Male LFPR (%)", main = "Heterogeineity of Male LFPR across years from 2016 to 2021", data = data)

# Bar Plots
data %>% select(Years, Primary, Secondary, Tertiary) %>% group_by(Years) %>% summarise(primary = mean(Primary), secondary = mean(Secondary), tertiary = mean(Tertiary)) %>% 
  pivot_longer(cols = primary:tertiary,
               names_to = "educational attainment", 
               values_to = "LFPR") %>% 
  ggplot(aes(x = Years, y = LFPR, fill = `educational attainment`)) + geom_bar(position = "dodge", stat = "Identity") + theme(legend.position="top") +
  xlab("Years") + ylab("Male Mean LFPR (%)") + ggtitle("Male Mean LFPR by Educational Attainment")

data %>% select(Years, `Never Married`, Married, Widowed, Divorced) %>% group_by(Years) %>% summarise(single = mean(`Never Married`), married = mean(Married), widowed = mean(Widowed), divorced = mean(Divorced)) %>% 
  pivot_longer(cols = single:divorced,
               names_to = "marital status", 
               values_to = "LFPR") %>% 
  ggplot(aes(x = Years, y = LFPR, fill = `marital status`)) + geom_bar(position = "dodge", stat = "Identity") + theme(legend.position="top") +
  xlab("Years") + ylab("Male Mean LFPR (%)") + ggtitle("Male Mean LFPR by Marital Status")

# OLS Model (Male)
ols_male <- plm(male_lfpr ~ primary + secondary + tertiary + single + married + widowed + divorced + young + middle + old,
                data = data, index = c("States","Years"), model="pooling")

# Fixed Effects Model (Male)
fixed_male <- plm(male_lfpr ~ primary + secondary + tertiary + single + married + widowed + divorced + young + middle + old,
                  data = data, index = c("States","Years"), model="within")

# ------------------------------------------------------------------ #

# Read Female LFPR Details
data2 <- read_excel("overall.xlsx", sheet = "Female LFPR")
states <- data2$States
years <- data2$Years
female_lfpr <- data2$Overall
primary <- data2$Primary
secondary <- data2$Secondary
tertiary <- data2$Tertiary
single <- data2$`Never Married`
married <- data2$Married
widowed <- data2$Widowed
divorced <- data2$Divorced
young <- data2$`Young Age`
middle <- data2$`Middle Age`
old <- data2$`Old Age`

data2 %>% select(Years, Primary, Secondary, Tertiary) %>% group_by(Years) %>% summarise(primary = mean(Primary), secondary = mean(Secondary), tertiary = mean(Tertiary)) %>% 
  pivot_longer(cols = primary:tertiary,
               names_to = "educational attainment", 
               values_to = "LFPR") %>% 
  ggplot(aes(x = Years, y = LFPR, fill = `educational attainment`)) + geom_bar(position = "dodge", stat = "Identity") + theme(legend.position="top") +
  xlab("Years") + ylab("Female Mean LFPR (%)") + ggtitle("Female Mean LFPR by Educational Attainment")

data2 %>% select(Years, `Never Married`, Married, Widowed, Divorced) %>% group_by(Years) %>% summarise(single = mean(`Never Married`), married = mean(Married), widowed = mean(Widowed), divorced = mean(Divorced)) %>% 
  pivot_longer(cols = single:divorced,
               names_to = "marital status", 
               values_to = "LFPR") %>% 
  ggplot(aes(x = Years, y = LFPR, fill = `marital status`)) + geom_bar(position = "dodge", stat = "Identity") + theme(legend.position="top") +
  xlab("Years") + ylab("Female Mean LFPR (%)") + ggtitle("Female Mean LFPR by Marital Status")

# OLS Model (Female)
ols_female <- plm(female_lfpr ~ primary + secondary + tertiary + single + married + widowed + divorced + young + middle + old,
                 data = data2, index = c("States","Years"), model="pooling")

# Fixed Effects Model (Female)
fixed_female <- plm(female_lfpr ~ primary + secondary + tertiary + single + married + widowed + divorced + young + middle + old,
                  data = data2, index = c("States","Years"), model="within")

stargazer(ols_male, fixed_male, ols_female, fixed_female,  
          se=list(clse(ols_male),clse(fixed_male),clse(ols_female),clse(fixed_female)), 
          title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled OLS", "Fixed Effects", "Pooled OLS", "Fixed Effects"), 
          df=FALSE, digits=4)

# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed_male, ols_male) # fixed effects model should be a better choice
pFtest(fixed_female, ols_female) # fixed effects model should be a better choice