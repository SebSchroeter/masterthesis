rm(list = ls())
library(tidyr)
library(httpgd)
#library(zoib)
library(arrow)
library(brms)
setwd("~/Sebastian Masterthesis/Git repo/masterthesis")
df<-read_feather("long_df.feather")
df_regression <-df[df$mvw<1,]
## test of using zero inflated beta regression: 
fit_zib <-brm(unweighted_ministries~mvw+Seats,data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib)
plot(fit_zib,variable = c("b_mvw","b_Seats"))
