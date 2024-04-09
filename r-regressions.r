rm(list = ls())
library(tidyr)
library(httpgd)
library(zoib)
library(arrow)
library(brms)
setwd("~/Sebastian Masterthesis/Git repo/masterthesis")
df<-read_feather("long_df.feather")
df$Country<-factor(df$Country)

## test of using zero inflated beta regression: 
  ## most stupid version
fit_zib <-brm(unweighted_ministries~mvw+Seats,data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib)
conditional_effects(fit_zib)

  ## using conditional dependencies 
fit_zib_zi <-brm(bf(unweighted_ministries~mvw+Seats,zi~mvw),data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib)
fit_zib_zi_alt <-brm(bf(unweighted_ministries~mvw+Seats,zi~Seats),data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib_zi_alt)
  ## using fixed effects 
fit_zib_fe<-brm(unweighted_ministries~(1+mvw+Seats|Country),data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
  ## fixed effects with conditional zero-inflation prediction 
fe_formula=bf(unweighted_ministries~(1+mvw+Seats|ID1|Country),(zi~mvw|ID1|Country))
fit_zib_fe_zi<-brm(fe_formula,data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"),control = list(adapt_delta = 0.90))
summary(fit_zib_fe_zi)

fit_zib_fe_zi2<- update(fit_zib_fe_zi, formula=unweighted_ministries~mvw+(Seats|ID1|Country),zi~mvw,control = list(adapt_delta = 0.90))

## compare Loo Method: 
loo(fit_zib,fit_zib_zi) #fit_zib_zi is better fitted than fit_zib
loo(fit_zib_zi_alt,fit_zib_zi) #fit_zib_zi is better fitted than fit_zib_zi_alt
###
fit_zoib <-zoib(unweighted_ministries~mvw+Seats|1|mvw+Seats|1,
                data = df,
                zero.inflation = T,
                one.inflation = F,
                joint=F,
                random = 1,
                EUID = df$Country,
                n.iter = 5000,
                n.burn=500,
                n.thin=5)
coeffs_zoib<-fit_zoib$coeff
summary(coeffs_zoib)
###  saving ####
saveRDS(fit_zib_fe_zi, file = "zib_fe_zi.RDS") 
saveRDS(fit_zib_zi, file = "zib_fe_zi.RDS")
saveRDS(fit_zoib, file = "zoib.RDS") 

