rm(list = ls())
library(tidyr)
library(httpgd)
library(zoib)
library(arrow)
library(brms)
library(stargazer)
library(texreg)
setwd("~/Sebastian Masterthesis/Git repo/masterthesis")
df<-read_feather("long_df.feather")
df$Country<-factor(df$Country)
df$pm<-factor(df$pm)

############# ols ###############
fit_ols<-lm(unweighted_ministries~mvw+factor(pm),data = df)
summary(fit_ols)

fit_ols_fe<-lm(unweighted_ministries~mvw+factor(pm)+factor(Country),data = df)
summary(fit_ols_fe)

fit_ols_seats<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = df)
summary(fit_ols_seats)

#weighted versions
fit_ols_weighted<-lm(weighted_ministries~mvw+factor(pm),data = df)
fit_ols_fe_weighted<-lm(weighted_ministries~mvw+factor(pm)+factor(Country),data = df)
fit_ols_seats_weighted<-lm(weighted_ministries~mvw+Seats+factor(pm),data = df)

#output
stargazer(fit_ols,fit_ols_fe,fit_ols_seats,fit_ols_seats_weighted,
          title = "OLS Results",
          align = T,
          omit = "Country",
          omit.stat=c("adj.rsq","ser","f"),
          dep.var.labels = c("Share of Ministries","Weighted Share"),
          column.labels=c("Basic","FE","Seat Share","Combined","Combined"),
          no.space = T,
          style = "qje"
          #float.env = "sidewaystable"
          )
# using power indices
fit_1<-lm(unweighted_ministries~mvw,data = df)
fit_2<-lm(unweighted_ministries~df$`SS-I`,data = df)
fit_3<-lm(unweighted_ministries~df$`PB-I`,data = df)
fit_4<-lm(unweighted_ministries~mvw+Seats,data = df)
fit_5<-lm(unweighted_ministries~df$`SS-I`+Seats,data = df)
fit_6<-lm(unweighted_ministries~df$`PB-I`+Seats,data = df)
fit_7<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = df)
fit_8<-lm(unweighted_ministries~df$`SS-I`+Seats+factor(pm),data = df)
fit_9<-lm(unweighted_ministries~df$`PB-I`+Seats+factor(pm),data = df)
texreg(list(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9),
       custom.model.names = c("MWV","SS-I","PB-I","MWV","SS-I","PB-I","MWV","SS-I","PB-I"),
       custom.header = list("Plain"=1:3,"With Seat Share"=4:6,"With Seat Share and PM- Indicator"=7:9),
       custom.coef.names = c("Intercept","MVW","SS-I","PB-I","Seat Share","Prime Minister"),
       dcolumn = T,booktabs = T, 
       caption = "Comparison of Power Indices", 
       sideways = T, 
       bold = 0.05,
       omit.coef = "Country", 
       digits = 3,
       include.adjrs=F, include.nobs=F)

############ Monte Carlo with independent formateur draws ##############
## not really useful study, since formateur draws should be dependend on previous parliamentary formateur draws --> only 1 formateur per draw
ols<-function(df){
  #generate sample
  df$formateur<-rbinom(n=nrow(df),size = 1,prob = df$Seats)
  #call lm
  model<-lm(unweighted_ministries~mvw+Seats+factor(formateur),data = df)
  #collect coeffitients and p values
  estimates<-summary(model)$coefficients[,1]
  p_values<-summary(model)$coefficients[,4]
  return(list("estimates"=estimates,"p_values"=p_values))
}

ols_per_parliament<-function(df){
  #overwrite formateurs
  df$formateur<- NA
  #list all parliaments 
  parliaments <- unique(df$Parliament)
  #loop over each parliament
  for (parliament in parliaments){
    indice<-which(df$Parliament==parliament)
    #sample 1 formateur per batch as previously
    formateurs<-sample(indice,size = 1,prob = df$Seats[indice])
    df$formateur[indice]<-0
    df$formateur[formateurs]<-1
  }
  model<-lm(unweighted_ministries~mvw+Seats+factor(formateur),data = df)
  #collect coeffitients and p values
  estimates<-summary(model)$coefficients[,1]
  p_values<-summary(model)$coefficients[,4]
  return(list("estimates"=estimates,"p_values"=p_values))
}
monte_carlo_ols <- function(df, n) {
  # storage
  all_estimates <- list()
  all_p_values <- list()
  # loop
  for (i in 1:n) {
    ## decide whether to use independent formateurs or per-parliament wise: by using ols(df) or ols_per_parliament(df) 
          #careful: ols_per_parliament takes much longer due to looping over all parliaments n times.
    result <- ols_per_parliament(df)
    all_estimates[[i]] <- result$estimates
    all_p_values[[i]] <- result$p_values
  }
  # Convert list to DataFrame
  estimates_df <- do.call(rbind, all_estimates)
  p_values_df <- do.call(rbind, all_p_values)
  # Return
  return(list("estimates_df" = estimates_df, "p_values_df" = p_values_df))
}

mc_results<-monte_carlo_ols(df,5000)
hist(mc_results$estimates_df[,4],xlim = c(-0.1,0.1),breaks = 20,
     main="Formateur-Coefficient Distribution",
     xlab="Formateur-Coeffiecients",)
hist(mc_results$p_values_df[,4],breaks = 20,
     main="Formateur-Coefficient p-Values",
     xlab="p-values of fomateur-coefficients",)

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
fit_zib_fe<-brm(unweighted_ministries~1+mvw+(Seats|Country),data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
  ## fixed effects with conditional zero-inflation prediction 
fe_formula=bf(unweighted_ministries~(1+mvw+Seats|ID1|Country),(zi~mvw|ID1|Country))
fit_zib_fe_zi<-brm(fe_formula,data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"),control = list(adapt_delta = 0.90))
summary(fit_zib_fe_zi)

fit_zib_fe_zi2<- update(fit_zib_fe_zi, formula=unweighted_ministries~mvw+(Seats|ID1|Country),zi~mvw,control = list(adapt_delta = 0.90))

## compare Loo Method: 
loo(fit_zib,fit_zib_zi) #fit_zib_zi is better fitted than fit_zib
loo(fit_zib_zi_alt,fit_zib_zi) #fit_zib_zi is better fitted than fit_zib_zi_alt



############ using zoib ############

#basic
fit_zoib <-zoib(unweighted_ministries~mvw+Seats|1|mvw+Seats|0,
                data = df,
                zero.inflation = T,
                one.inflation = F,
                joint=F,
                random = 0,
                n.iter = 5000,
                n.burn=500,
                n.thin=5)
coeffs_zoib<-fit_zoib$coeff
summary(coeffs_zoib)


#fe
# fit_zoib_fe <-zoib(unweighted_ministries~mvw+Seats+factor(Country)|1|mvw+Seats+factor(Country)|0,
#                 data = df,
#                 zero.inflation = T,
#                 one.inflation = F,
#                 joint=F,
#                 random = 0,
#                 n.iter = 5000,
#                 n.burn=500,
#                 n.thin=5)

# coeffs_zoib_fe<-fit_zoib_fe$coeff
# summary(coeffs_zoib_fe)
############  saving #########
saveRDS(fit_zib_fe_zi, file = "zib_fe_zi.RDS") 
saveRDS(fit_zib_zi, file = "zib_fe_zi.RDS")
saveRDS(fit_zoib, file = "zoib.RDS") 

fit_zib<-readRDS("zib_fe_zi.RDS")
fit_zoib<-readRDS("zoib.RDS")
coeffs_zoib<-fit_zoib$coeff
summary(coeffs_zoib)
summary(fit_zib)
conditional_effects(fit_zib)
