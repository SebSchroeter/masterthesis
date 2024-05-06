rm(list = ls())
library(tidyr)
library(dplyr)
library(lmtest)
library(httpgd)
library(zoib)
library(arrow)
library(brms)
library(stargazer)
library(texreg)
library(ggplot2)
library(ggcorrplot)
library(nlme)
library(lsmeans)
library(patchwork)

setwd("~/Sebastian Masterthesis/Git repo/masterthesis")
df<-read_feather("long_df.feather")
monte_carlo_df<-read_feather("monte_carlo_df.feather")
######prelims#####
df$Country<-factor(df$Country)
df$pm<-factor(df$pm)
df$is_gov<-as.integer(df$unweighted_ministries>0)
##subsetting only government coalition parties
gov_df=df %>% filter(is_gov==1)
#############correlation##########
names(df)[names(df) == "mvw"] <- "MVW"
names(df)[names(df) == "Seats"] <- "Seat Share"
corr<-cor(df[,3:6],use = "complete.obs")
head(corr[, 1:3])
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE,lab_size = 5,
           legend.title = "Correlation Coeff.",
           colors = c("blue4","white","red"),
           ggtheme = ggplot2::theme_light,
           tl.cex = 13)
           
names(df)[names(df) == "Seat Share"] <- "Seats"
names(df)[names(df) == "PB-I"] <- "PBI"
names(df)[names(df) == "SS-I"] <- "SSI"

      ##plotting all power indices
ggplot(df,aes(x=Seats))+
        geom_point(aes(y=MVW,color='MVW'),size=2,alpha=0.5)+
        geom_point(aes(y=PBI,color='PB-I'),size=2,alpha=0.5)+
        geom_point(aes(y=SSI,color='SS-I'),size=2,alpha=0.5)+
        geom_smooth(aes(y=MVW,color='MVW'),method = "glm")+
        geom_smooth(aes(y=PBI,color='PB-I'),method = "glm")+
        geom_smooth(aes(y=SSI,color='SS-I'),method = "glm")+
        theme_light()+
        theme(
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 13)
        )+
        scale_x_continuous(limits=c(0, 0.5), breaks=seq(0, 0.5, by=0.1), labels=seq(0, 0.5, by=0.1))+
        scale_color_manual(values=c("MVW"="black", "PB-I"="blue", "SS-I"="red"),
                     name="Variable",
                     breaks=c("MVW", "PB-I", "SS-I"),
                     labels=c("MVW", "PB-I", "SS-I"))+
        labs(x = "Seat Share",y = "Power index")


    ##plotting only MVW of all 
cor_mvw_all<-cor(df$Seats,df$unweighted_ministries,use = "complete.obs")
scatter_mvw_all<-ggplot(df,aes(x=Seats))+
  geom_point(aes(y=MVW),color='black',size=1.5,alpha=1)+
  geom_text(aes(x = Inf, y = Inf, label = sprintf("Corr: %.2f", cor_mvw_all)), 
            hjust = 2, vjust = 35, size = 6, color = 'blue')+
  theme_light()+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none")+
  labs(x = "Seat Share all Parties",y = "MVW")


    ##plotting only MVW from Governing Parties 
cor_mvw_gov<-cor(gov_df$Seats,gov_df$unweighted_ministries,use = "complete.obs")
scatter_mvw_gov<-ggplot(gov_df,aes(x=Seats))+
  geom_point(aes(y=MVW),color='black',size=1.5,alpha=1)+
  geom_text(aes(x = Inf, y = Inf, label = sprintf("Corr: %.2f", cor_mvw_gov)), 
            hjust = 2, vjust = 35, size = 6, color = 'blue')+
  theme_light()+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none")+
  labs(x = "Seat Share Governing Parties",y = "MVW")
    ##combining plots 
mvw_plots<-scatter_mvw_all+scatter_mvw_gov+plot_layout(ncol=2)
print(mvw_plots)
############# ols ###############
#baseline- replication
base_ols<-lm(unweighted_ministries~mvw+factor(pm),data = df)
summary(base_ols)
gov_base_ols<-lm(unweighted_ministries~mvw+factor(pm),data = gov_df)
summary(gov_base_ols)
  
#endogenous ministries
endo_ols<-lm(endo~mvw+factor(pm),data = df)
summary(endo_ols)
gov_endo_ols<-lm(endo~mvw+factor(pm),data = gov_df)
summary(gov_endo_ols)


#testing country fe
base_fe_ols<-lm(unweighted_ministries~mvw+factor(pm)+factor(Country),data = df)
endo_fe_ols<-lm(endo~mvw+factor(pm)+factor(Country),data = df)

#accounting for seat share
base_seat_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = df)
endo_seat_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = df)
gov_base_seat_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = gov_df)
gov_endo_seat_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm),data = gov_df)


#seatshare with fe 
base_seat_fe_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm)+factor(Country),data = df)
endo_seat_fe_ols<-lm(endo~mvw+Seats+factor(pm)+factor(Country),data = df)
summary(endo_seat_fe_ols)

###testing for significant differences of PM coeff###
#constructing stacked data 
y_unweighted<-df$unweighted_ministries
y_endo<-df$endo
y<-c(y_unweighted,y_endo)
x_mvw<-c(df$mvw,df$mvw)
x_seats<-c(df$Seats,df$Seats)
x_pm<-c(df$pm,df$pm)
countries<-c(df$Country,df$Country)
# creating dummy for unweighted/endo
dummy.var<-factor(c(rep(0,nrow(df)),rep(1,nrow(df))))
#fitting gls with different residual variances between groups
test_fit<-gls(y~x_mvw+x_seats+x_pm*dummy.var,weights = varIdent(form = ~1|dummy.var),na.action = na.omit)
summary(test_fit)

#weighted ministry versions
fit_ols_weighted<-lm(weighted_ministries~mvw+factor(pm),data = df)
fit_ols_fe_weighted<-lm(weighted_ministries~mvw+factor(pm)+factor(Country),data = df)
fit_ols_seats_weighted<-lm(weighted_ministries~mvw+Seats+factor(pm),data = df)

##output##
#list base+endo: 
first_list<-list(base_ols,gov_base_ols,endo_ols,gov_base_ols)
second_list<-list(base_ols,endo_ols,base_seat_ols,endo_seat_ols)
third_list<-list(base_ols,endo_ols,base_fe_ols,endo_fe_ols,base_seat_fe_ols,endo_seat_fe_ols,fit_ols_seats_weighted)
#tex outputs
texreg(first_list,
          ci.force.level = 0.95,
          ci.force = T,
          custom.model.names = c("All Parties","Only Gov. Parties","All Parties","Only Gov. Parties"),
          custom.header = list("All Ministries"=1:2,"Without PM"=3:4),
          custom.coef.names = c("Intercept","MVW","Prime Minister"),
          dcolumn = T,booktabs = T, 
          caption = "Influence of MVW on Cabinet Shares", 
          sideways = F, 
          #bold = 0.05,
          omit.coef = "Country", 
          digits = 3,
          include.adjrs=F, include.nobs=F)

screenreg(second_list,
          custom.model.names = c("Base","Endog. PM","Base","Endog. PM"),
          custom.header = list("Plain"=1:2,"with Seat Share"=3:4),
          custom.coef.names = c("Intercept","MVW","Prime Minister","Seat Share"),
          ci.force.level = 0.95,
          ci.force = T,
          dcolumn = T,booktabs = T, 
          #caption = "Comparison of Power Indices", 
          #sideways = T, 
          #bold = 0.05,
          omit.coef = "Country", 
          digits = 3,
          include.adjrs=F, include.nobs=F)
screenreg(third_list,
          custom.model.names = c("Base","Endog. PM","Base","Endog. PM","Base","Endog. PM","Weighted PM"),
          custom.header = list("Plain"=1:2,"Country Fixed Effects"=3:4,"Country Fixed Effects + Seat Shares"=5:7),
          custom.coef.names = c("Intercept","MVW","Prime Minister","Seat Share"),
          custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item p-values for all Country Fixed Effects exceed $>0.5$, for this reason these lines are omitted.\\item Exact estimates are available from the author.\\item Columns 1,3,5 use the share of ministries as dependent variable. Columns 2,4,6 consider use cabinets without the Prime Minister position. \\item Column 7 gives triple weight to the Prime Minister position.",
          ci.force.level = 0.95,
          ci.force = T,
          dcolumn = T,booktabs = T, 
          caption = "Robustness Tests OLS",
          caption.above = T,  
          label = "Tab: Robustness",
          sideways = T, 
          threeparttable = T,
          #bold = 0.05,
          omit.coef = "Country", 
          digits = 3,
          include.adjrs=F, include.nobs=F)

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
#output
screenreg(list(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9),
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
##j&cox tests# #
  #mvw <-> ssi
jtest(fit_1,fit_2)
coxtest(fit_1,fit_2)
  #mvi <-> pbi 
jtest(fit_1,fit_3)
coxtest(fit_1,fit_3)
  #ssi <-> pbi
jtest(fit_2,fit_3)
coxtest(fit_2,fit_3)
############ Monte Carlo ##############
## with independent formateur draws 
ols<-function(df){
  #generate sample
  df$formateur<-rbinom(n=nrow(df),size = 1,prob = df$mvw_i)
  #call lm
  model<-lm(gov_share~mvw_i+Seats+factor(formateur),data = df)
  #collect coeffitients and p values
  estimates<-summary(model)$coefficients[,1]
  p_values<-summary(model)$coefficients[,4]
  return(list("estimates"=estimates,"p_values"=p_values))
}
## formateur per parliament ##
ols_per_parliament<-function(df){
  #overwrite formateurs
  df$formateur<- NA
  #list all parliaments 
  parliaments <- unique(df$Parliament)
  #loop over each parliament
  for (parliament in parliaments){
    indice<-which(df$Parliament==parliament)
    #sample 1 formateur per batch as previously
    formateurs<-sample(indice,size = 1,prob = df$mvw_i[indice])
    df$formateur[indice]<-0
    df$formateur[formateurs]<-1
  }
  model<-lm(gov_share~x+factor(formateur),data = df)
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
          #careful: ols_per_parliament takes long due to looping over all parliaments n times.
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
## draws & study ##
monte_carlo_df$x=monte_carlo_df$lambda_prop*monte_carlo_df$mvw_i
set.seed(123)
mc_results<-monte_carlo_ols(monte_carlo_df,5000)
## histograms ##
est_df<-as.data.frame(mc_results$estimates_df)
names(est_df)[names(est_df) == "factor(formateur)1"] <- "formateur"
mean_formateur<-mean(est_df$formateur)
hist(mc_results$estimates_df[,3],xlim = c(-0.15,0.15),breaks = 20)

ggplot(est_df,aes(x=formateur))+geom_histogram(
  color="grey",
  bins = 30)+
  geom_vline(aes(xintercept=mean(formateur)),
             linetype="dashed",size=1.5)+
  theme_light()+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  labs(x = "Formateur Coefficients \u03B2_1",y = "Frequency")
hist(mc_results$estimates_df[,2],breaks = 20,
     main="Weight-Coefficient Distribution",
     xlab="Weight-Coeffiecients",)
hist(mc_results$estimates_df[,1],breaks = 20,
     main="Constant-Coefficient Distribution",
     xlab="Constant-Coeffiecients",)

 hist(mc_results$p_values_df[,3],xlim = c(0,1),breaks = 100,
     main="Formateur-Coefficient p-Values",
     xlab="p-values of fomateur-coefficients",)


percent_significant_formateur<-sum(mc_results$p_values_df[,3]<=0.05)/length(mc_results$p_values_df[,3])
c<-mean(mc_results$estimates_df[,2])*mean(monte_carlo_df$lambda_prop)
##### test of using zero inflated beta regression:  #####
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



#####testing########
mc_results$estimates_df
