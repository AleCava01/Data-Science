
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------
getwd()
setwd("C:/Users/aless/Desktop/Business Data Analytics/Esercizi/")
rm(list = ls()) #cleans the environment from any object previously defined
library(knitr)
library(corrplot)
library(ggplot2)
library(sjPlot)
library(stargazer)
library(MatchIt)
library(MASS)
library(mvtnorm)
library(ivreg)
library(rdrobust)
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

#dataset loading
data("lalonde", package = "MatchIt")
head(lalonde)
help(lalonde)

lalonde$treat <- factor(lalonde$treat)

## ---------------------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(lalonde, aes(x=treat, y=age, fill=treat, group = treat)) + 
  geom_boxplot() 

ggplot(lalonde, aes(x=treat, y=educ, fill=treat, group = treat)) + 
  geom_boxplot() 

ggplot(lalonde, aes(x=treat, y=re75, fill=treat, group = treat)) + 
  geom_boxplot() 

ggplot(lalonde, aes(x=treat, y=re74, fill=treat, group = treat)) + 
  geom_boxplot() 

## ---- pre-matching analysis-----------------------------------------------------------------------------------------------------------
lalonde_treated = lalonde[which(lalonde$treat == 1), ]
lalonde_untreated = lalonde[which(lalonde$treat == 0), ]

t.test(lalonde_treated$age, lalonde_untreated$age) #ok, not great
t.test(lalonde_treated$educ, lalonde_untreated$educ) #good!
t.test(lalonde_treated$nodegree, lalonde_untreated$nodegree) #ok
t.test(lalonde_treated$re75, lalonde_untreated$re75) #at most
t.test(lalonde_treated$re74, lalonde_untreated$re74) #very bad

#the sample is not randomized
library(sjPlot)

naive_estimator = mean(lalonde_treated$re78) - mean(lalonde_untreated$re78)
naive_estimator

age = scale(lalonde$age)
educ = scale(lalonde$educ)
re75 = scale(lalonde$re75)
re74 = scale(lalonde$re74)
re78 = scale(lalonde$re78)
treat = lalonde$treat
nodegree = factor(lalonde$nodegree)

f_ne = re78 ~ treat + age + educ + re75 + re74 + nodegree
dataset = data.frame(re78,treat, age, educ, re74, re75, nodegree)
mod_ne = lm(f_ne, data = dataset)
summary(mod_ne)
#summary shows that treatment has no effect on re78


library(stargazer)

# stargazer serve per comparare i risultati dei modelli in forma ordinata
stargazer(mod_ne, type='text')


## ---- PSM ------------------------------------------------------------------------------------------------------------------------
library(MatchIt)

psm_result = matchit(
  lalonde$treat ~ lalonde$age + lalonde$educ + lalonde$re75 + lalonde$re74 + lalonde$nodegree, 
  method = "nearest")
?matchit
#visualizza ogni unità del gruppo trattato a quale unità del gruppo di controllo è stata associata
psm_result$match.matrix


## ---- Analisi post-matching -----------------------------------------------------------------------------------------------------------
#per tenere solo le entità che sono state matchate si utilizza match.data
db_post_psm = match.data(psm_result, data=lalonde)

t.test(age ~ treat, data=db_post_psm)#good
t.test(educ ~ treat, data=db_post_psm)#very good
t.test(re75 ~ treat, data=db_post_psm)#very good
t.test(re74 ~ treat, data=db_post_psm)#very good
t.test(nodegree ~ treat, data=db_post_psm)#very good

naive_estimator_post_psm = mean(db_post_psm[which(db_post_psm$treat==1),]$re78) - mean(db_post_psm[which(db_post_psm$treat==0),]$re78)
naive_estimator_post_psm

age_post_psm = scale(db_post_psm$age)
treat_post_psm = db_post_psm$treat
educ_post_psm = scale(db_post_psm$educ)
re74_post_psm = scale(db_post_psm$re74)
re75_post_psm = scale(db_post_psm$re75)
re78_post_psm = scale(db_post_psm$re78)
nodegree_post_psm = scale(db_post_psm$nodegree)

f_ne_post_psm = re78_post_psm ~ treat_post_psm + age_post_psm + educ_post_psm + re74_post_psm + re75_post_psm + nodegree_post_psm
dataset_post_psm = data.frame(age_post_psm,treat_post_psm, educ_post_psm, re74_post_psm, re75_post_psm, re78_post_psm,nodegree_post_psm)
mod_ne_post_psm = lm(f_ne_post_psm, data = dataset_post_psm)
summary(mod_ne_post_psm)

# stargazer serve per comparare i risultati dei modelli in forma ordinata
stargazer(mod_ne_post_psm, type='text')
