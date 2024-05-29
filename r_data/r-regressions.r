######beginning #####
rm(list = ls())

## dependencies ##
library(tidyr)
library(dplyr)
library(broom)
library(texreg)
library(ggplot2)
library(ggcorrplot)
library(nlme)
library(lsmeans)
library(patchwork)
library(arrow)
##only install for replication of Cutler at al (2016): 
  library(zoib)
  library(brms)


setwd("E:/googledrive daten/masterarbeit r")
df<-read_feather("long_df.feather")
monte_carlo_df<-read_feather("monte_carlo_df.feather")
######prelims#####
df$Country<-factor(df$Country)
df$pm<-factor(df$pm)
df$is_gov<-as.integer(df$unweighted_ministries>0)
##subsetting only government coalition parties
gov_df=df %>% filter(is_gov==1)
monte_carlo_df$x=monte_carlo_df$mvw_i*monte_carlo_df$lambda_prop
monte_carlo_df$endo=df$endo
monte_carlo_df$pm=df$pm
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


    ##plotting only MVW of all parties
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
print(scatter_mvw_all)
scatter_seats_gamma<-ggplot(monte_carlo_df, aes(x=Seats))+
  geom_point(aes(y=x))
print(scatter_seats_gamma)
    ##plotting only MVW from Governing Parties 
cor_mvw_gov<-cor(gov_df$Seats,gov_df$unweighted_ministries,use = "complete.obs")
scatter_mvw_gov<-ggplot(gov_df,aes(x=Seats))+
  geom_point(aes(y=mvw),color='black',size=1.5,alpha=1)+
  geom_text(aes(x = Inf, y = Inf, label = sprintf("Corr: %.2f", cor_mvw_gov)), 
            hjust = 2, vjust = 35, size = 6, color = 'blue')+
  theme_light()+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none")+
  labs(x = "Seat Share Governing Parties",y = "MVW")
print(scatter_mvw_gov)    

    
##combining plots 
mvw_plots<-scatter_mvw_all+scatter_mvw_gov+plot_layout(ncol=2)
print(mvw_plots)

############# ols ###############
#baseline- replication
gov_base_ols<-lm(unweighted_ministries~factor(pm)+mvw,data = gov_df)
summary(gov_base_ols)
  #testing for \beta_2=2
  beta_2<-coef(summary(gov_base_ols))["mvw","Estimate"]
  se_2<-coef(summary(gov_base_ols))["mvw","Std. Error"]
  tstat<-(beta_2-2)/se_2
  p_value<-pt(tstat,df=df.residual(gov_base_ols))  

  #endogenous ministries
gov_endo_ols<-lm(endo~factor(pm)+mvw,gov_df)
summary(gov_endo_ols)
  #testing for \beta_2=2
  beta_2<-coef(summary(gov_endo_ols))["mvw","Estimate"]
  se_2<-coef(summary(gov_endo_ols))["mvw","Std. Error"]
  tstat<-(beta_2-2)/se_2
  p_value<-pt(tstat,df=df.residual(gov_endo_ols))  
  
#accounting for seat share
gov_base_seat_ols<-lm(unweighted_ministries~factor(pm)+mvw+Seats,data = gov_df)
gov_endo_seat_ols<-lm(endo~factor(pm)+mvw+Seats,data = gov_df)
summary(gov_base_seat_ols)

#full dataset with \mu# 

  #21
y_noint<-lm(gov_share~factor(pm)+x+Seats,data = monte_carlo_df)
y_hat_noint<-lm(endo~factor(pm)+x+Seats,data = monte_carlo_df)
y_int<-lm(gov_share~factor(pm)+mvw_i+Seats+lambda_prop+mvw_i*lambda_prop,data = monte_carlo_df)
y_hat_int<-lm(endo~factor(pm)+mvw_i+Seats+lambda_prop+mvw_i*lambda_prop,data = monte_carlo_df)

#comparison of power indices 
# using power indices
fit_1<-lm(unweighted_ministries~mvw,data = df)
fit_2<-lm(unweighted_ministries~df$`SS-I`,data = df)
fit_3<-lm(unweighted_ministries~df$`PB-I`,data = df)
fit_4<-lm(unweighted_ministries~mvw+Seats,data = df)
fit_5<-lm(unweighted_ministries~df$`SS-I`+Seats,data = df)
fit_6<-lm(unweighted_ministries~df$`PB-I`+Seats,data = df)

  #testing the SS-I
  summary(fit_5)
  coef(summary(fit_5))
  beta_seats<-coef(summary(fit_5))["Seats","Estimate"]
  se_seats<-coef(summary(fit_5))["Seats","Std. Error"]
  tstat<-(beta_seats)/se_seats
  p_value<-pt(tstat,df=df.residual(fit_5))  

  ##j&cox tests# #
#mvw <-> ssi
jtest(fit_1,fit_2)
coxtest(fit_1,fit_2)
jtest(fit_4,fit_5)
coxtest(fit_4,fit_5)
#mvi <-> pbi 
jtest(fit_1,fit_3)
coxtest(fit_1,fit_3)
#ssi <-> pbi
jtest(fit_2,fit_3)
coxtest(fit_2,fit_3)


##output##
#list base+endo: 
first_list<-list(gov_base_ols,gov_endo_ols)
seats_list<-list(gov_base_seat_ols,gov_endo_seat_ols)
third_list<-list(y_noint,y_hat_noint,y_int,y_hat_int)
pi_list<-list(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6)
#tex outputs
texreg(first_list,
          ci.force.level = 0.95,
          ci.force = T,
          custom.model.names = c("All Ministries","Without PM"),
          custom.coef.names = c("Intercept","Prime Minister","MVW-I"),
          dcolumn = T,booktabs = T, 
          caption = "Influence of MVW on Cabinet Shares", 
          label = "table:ols-1",
          sideways = F, 
          threeparttable = T,
       custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item Left column dep. var. is full cabinet share. \\item Right column dep. var. is cabinet share without prime minister.\\item Nr. of obs. 425",
          #bold = 0.05,
          omit.coef = "Country", 
          digits = 3,
          include.adjrs=F, include.nobs=F)
texreg(seats_list,
       ci.force.level = 0.95,
       ci.force = T,
       custom.model.names = c("All Ministries","Without PM"),
       custom.coef.names = c("Intercept","Prime Minister","MVW-I","Seat Share"),
       dcolumn = T,booktabs = T, 
       caption = "Effect of Seat Shares and MVW", 
       label = "table:ols-2",
       sideways = F, 
       threeparttable = T,
       custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item Left column dep. var. is full cabinet share. \\item Right column dep. var. is cabinet share without prime minister.\\item Nr. of obs. 425",
       #bold = 0.05,
       omit.coef = "Country", 
       digits = 3,
       include.adjrs=F, include.nobs=F)
texreg(third_list,
       custom.model.names = c("I","II","III","IV"),
       custom.header = list("Eq. (21)"=1:2,"Eq. (22)"=3:4),
       custom.coef.names = c("Intercept","Prime Minister","$\\tilde{x}$","Seat Share","MVW-I","$\\gamma$","MVW-I$\\ast\\gamma$"),
       ci.force.level = 0.95,
       ci.force = T,
       dcolumn = T,booktabs = T, threeparttable = T,
       caption = "Comparison between cond. Propability $\\gamma$ as a Constant or Variable", 
       sideways = T, 
          #bold = 0.05,
          #omit.coef = "Country", 
       custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item Columns I,III dep. var. is full cabinet share. \\item Columns II,IV dep. var. is cabinet share without prime minister.\\item Nr. of obs. 1192.",
       digits = 3,
       label = "table:ols-3",
       include.adjrs=T,
       include.rsquared=F,
       include.nobs=F,
       #no.margin=T
       )

texreg(pi_list,
          custom.model.names = c("MWV","SS-I","PB-I","MWV","SS-I","PB-I"),
          custom.header = list("Plain"=1:3,"With Seat Share"=4:6),
          custom.coef.names = c("Intercept","MVW-I","SS-I","PB-I","Seat Share"),
          dcolumn = T,booktabs = T,  threeparttable = T,
          caption = "Comparison of Power Index Prediciton Power", 
          custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item dep. var. is full cabinet share. \\item Nr. of obs. 1192.",
          sideways = T, 
       label = "table:ols-4",
          ci.force.level = 0.95,
          ci.force = T,
          #bold = 0.05,
          digits = 3,
          include.adjrs=T, include.nobs=F,include.rsquared=T)

######ML ####
names(gov_df)[names(gov_df) == "mvw"] <- "mvw_i"

ex_postpost<-glm(endo~factor(pm)+mvw_i,data=gov_df,family=gaussian)

ex_post<-glm(endo~factor(pm)+mvw_i+Seats+lambda_prop+mvw_i*lambda_prop,data = monte_carlo_df,family=gaussian)

ex_ante<-glm(unweighted_ministries~df$`SS-I`,data = df,family=gaussian)


##aic criterion
aic_1<-summary(ex_postpost)$aic
aic_2<-summary(ex_post)$aic
aic_3<-summary(ex_ante)$aic
pr_1=exp((aic_2-aic_1)/2)
pr_2=exp((aic_2-aic_3)/2)

texreg(list(ex_postpost,ex_post,ex_ante),
       ci.force.level = 0.95,
       ci.force = T,
       custom.model.names = c("$C^\\ast known$","Ex post", "Ex ante"),
       custom.coef.names = c("Intercept","Prime Minister","MVW-I", "Seat Share", "$\\lambda$","$\\text{MVW-I}\\ast\\lambda$","SS-I"),
       dcolumn = T,booktabs = T, 
       caption = "Comparison of model information", 
       label = "table:gls-1",
       #sideways = F, 
       threeparttable = T,
       custom.note = "\\item $95\\%$ confidence intervals in brackets. \\item %stars \\item Dep. var. is cabinet share without prime minister.",
       #bold = 0.05,
       #omit.coef = "Country", 
       digits = 3,
       include.deviance=F, include.nobs=T)
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
ols_per_parliament<-function(df,y_column,x_column,x_column_2,weight_column,prop,int_term){
  #overwrite formateurs
  df$formateur<- NA
  #list all parliaments 
  parliaments <- unique(df$Parliament)
  #loop over each parliament
  for (parliament in parliaments){
    indice<-which(df$Parliament==parliament)
    #check whether proportional or uniform probabilities
    if (prop==T){
      formateurs<-sample(indice,size = 1,prob = weight_column[indice])} else {
      formateurs<-sample(indice,size = 1)}
    df$formateur[indice]<-0
    df$formateur[formateurs]<-1
  }
  if (int_term==T){
    model<-lm(y_column~x_column*x_column_2+factor(formateur),data = df)} else {
    model<-lm(y_column~x_column+factor(formateur),data = df)}
  #collect coeffitients and p values
  estimates<-summary(model)$coefficients[,1]
  p_values<-summary(model)$coefficients[,4]
  adj_r_sq<-summary(model)$'adj.r.squared'
  return(list("estimates"=estimates,"p_values"=p_values,"r_squared"=adj_r_sq))
}
monte_carlo_ols <- function(df,y_column,x_column,x_column_2,weight_column,prop=T,n,int_term) {
  # storage
  all_estimates <- list()
  all_p_values <- list()
  all_r_squared<-list()
  # loop
  for (i in 1:n) {
          #careful: ols_per_parliament takes long due to looping over all parliaments n times.
    result <- ols_per_parliament(df,y_column,x_column,x_column_2,weight_column,prop,int_term)
    all_estimates[[i]] <- result$estimates
    all_p_values[[i]] <- result$p_values
    all_r_squared[[i]]<-result$r_squared
  }
  # Convert list to DataFrame
  estimates_df <- do.call(rbind, all_estimates)
  p_values_df <- do.call(rbind, all_p_values)
  r_sq_df<-do.call(rbind,all_r_squared)
  # Return
  return(list("estimates_df" = estimates_df, "p_values_df" = p_values_df,"r_sq_df"=r_sq_df))
}
    ## draws & study ##
set.seed(1000)
##1. MC from gov parties ##
gov_party_mc_prop<-monte_carlo_ols(gov_df,y_column=gov_df$unweighted_ministries,x_column=gov_df$mvw,weight_column=gov_df$mvw,n=20000,int_term = F,x_column_2 = NA)
gov_party_mc_uniform<-monte_carlo_ols(gov_df,y_column=gov_df$unweighted_ministries,x_column=gov_df$mvw,weight_column=gov_df$mvw,n=20000,int_term = F,x_column_2 = NA,prop = F)

  #1.1 comparison#
  significant_formateur_prop_gov<-sum(gov_party_mc_prop$p_values_df[,3]<=0.05)/length(gov_party_mc_prop$p_values_df[,3])
  significant_formateur_uni_gov<-sum(gov_party_mc_uniform$p_values_df[,3]<=0.05)/length(gov_party_mc_uniform$p_values_df[,3])
  mean_r_sq_prop_gov<-mean(gov_party_mc_prop$r_sq_df)
  mean_r_sq_uni_gov<-mean(gov_party_mc_uniform$r_sq_df)  
  mean_formateur_prop_gov<-mean(gov_party_mc_prop$estimates_df[,3])
  mean_formateur_uni_gov<-mean(gov_party_mc_uniform$estimates_df[,3])
  
##2. MC from all parties##
#2.1 prop
all_party_mc_prop<-monte_carlo_ols(monte_carlo_df,y_column = monte_carlo_df$gov_share,x_column = monte_carlo_df$mvw_i,weight_column = monte_carlo_df$mvw_i,x_column_2=monte_carlo_df$lambda_prop,prop = T,n=10000,int_term=T)
#2.2 uniform
uniform_df<-monte_carlo_df %>% filter(crit_n==1)
all_party_mc_uniform<-monte_carlo_ols(uniform_df,y_column = uniform_df$gov_share,x_column = uniform_df$mvw_i,weight_column = uniform_df$mvw_i,x_column_2=uniform_df$lambda_uni,prop = F,n=10000,int_term = T)
est_df<-as.data.frame(all_party_mc_prop$estimates_df)
## histograms ##
monte_carlo_histogram<-function(list,name="\u03B2_1 estimates"){
  est_df<-as.data.frame(list$estimates_df)
  names(est_df)[names(est_df) == "factor(formateur)1"] <- "formateur"
  mean_formateur<-mean(est_df$formateur)
  hist(est_df[,3],xlim = c(-0.15,0.15),breaks = 20)
  #plot
  hist_prop<-ggplot(est_df,aes(x=formateur))+geom_histogram(
    color="grey",
    bins = 30)+
    geom_vline(aes(xintercept=mean(formateur)),
               linetype="dashed",size=1.5)+
    theme_light()+
    theme(
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15))+
    labs(x = name,y = "Frequency")
  return(hist_prop)
}
mc_plot_1<-monte_carlo_histogram(all_party_mc_prop,name="\u03B2_1 est. - proportional draws, all parties")
mc_plot_2<-monte_carlo_histogram(gov_party_mc_prop,name="\u03B2_1 est. - proportional draws, only government parties")
mc_plot_3<-monte_carlo_histogram(all_party_mc_uniform,name="\u03B2_1 estimates from uniform draw, all parties")
mc_plot_4<-monte_carlo_histogram(gov_party_mc_uniform,name="\u03B2_1 est. - uniform draws, only government parties")
wrap_plots(mc_plot_2,mc_plot_4)
wrap_plots(mc_plot_1,mc_plot_2,mc_plot_3,mc_plot_4)

##significance levels

significant_formateur_prop_all<-sum(all_party_mc_prop$p_values_df[,3]<=0.05)/length(gov_party_mc_prop$p_values_df[,3])

## implied cost per vote
c_prop_all<-mean(all_party_mc_prop$estimates_df[,2])*mean(monte_carlo_df$lambda_prop)
c_prop_gov<-mean(gov_party_mc_prop$estimates_df[,2])
##average estimates


est_df_uni<-as.data.frame(mc_uniform_results$estimates_df)
names(est_df_uni)[names(est_df_uni) == "factor(formateur)1"] <- "formateur"

#plot
hist_uni<-ggplot(est_df_uni,aes(x=formateur))+geom_histogram(
  color="grey",
  bins = 30)+
  geom_vline(aes(xintercept=mean(formateur)),
             linetype="dashed",size=1.5)+
  theme_light()+
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  labs(x = "Uniform Formateur \u03B2_1",y = "Frequency")

hist_plots<-hist_prop+hist_uni+plot_layout(ncol=2)
print(hist_plots)

##### test of using zero inflated beta regression:  #####
  ## most stupid version
fit_zib <-brm(unweighted_ministries~mvw+Seats,data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib)
conditional_effects(fit_zib)

  ## using conditional dependencies 
fit_zib_zi <-brm(bf(unweighted_ministries~mvw+Seats,zi~mvw),data = df,family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"))
summary(fit_zib_zi)
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
coeffs_zoib<-zoib$coeff
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
####  saving ###
saveRDS(fit_zib_fe_zi, file = "zib_fe_zi.RDS") 
saveRDS(fit_zib_zi, file = "zib_fe_zi.RDS")
saveRDS(fit_zoib, file = "zoib.RDS") 

fit_zib<-readRDS("zib_fe_zi.RDS")
fit_zoib<-readRDS("zoib.RDS")
coeffs_zoib<-fit_zoib$coeff
summary(coeffs_zoib)
summary(fit_zib)
conditional_effects(fit_zib)



####discussion ####
discussion_lm<-lm(endo~Seats+mvw_i+pm,data=monte_carlo_df)
summary(discussion_lm)
# test step-wise aic crit 

# test linearity assumptions 
  #interaction terms
  full_lm<-lm(endo~Seats*mvw_i+Seats*lambda_prop+Seats*pm+mvw_i*lambda_prop+mvw_i*pm+lambda_prop*pm+Seats+mvw_i+lambda_prop+pm+I(Seats^2)+I(mvw_i^2)+I(lambda_prop^2), data = monte_carlo_df)
  summary(full_lm)
  #heteroskedasticity
  fitted_data<-augment(gov_endo_ols,data=gov_df)
  ggplot(fitted_data,aes(x=.fitted,y=.resid))+ 
    geom_point(aes(color=Country))+
    geom_smooth(method = "lm")
  #linear dep of errors
  set.seed(1000)
  gov_df_corrected<-gov_df %>%
    group_by(Parliament) %>%
    mutate(id_row=row_number())%>%
    filter(!id_row %in% sample(id_row,1))%>%
    select(-id_row)
  gov_corrected<-lm(endo~factor(pm)+mvw,gov_df_corrected)
  summary(gov_corrected)
  gov_seat_corrected<-lm(endo~factor(pm)+mvw+Seats,data = gov_df_corrected)
  summary(gov_seat_corrected)
  mc_df_corrected<-monte_carlo_df %>%
    group_by(Parliament) %>%
    mutate(id_row=row_number())%>%
    filter(!id_row %in% sample(id_row,1))%>%
    select(-id_row)
  y_hat_corrected<-lm(endo~factor(pm)+mvw_i+Seats+lambda_prop+mvw_i*lambda_prop,data = mc_df_corrected)
  summary(y_hat_corrected)
  #####old code########
#ols#

#testing country fe
base_fe_ols<-lm(unweighted_ministries~mvw+factor(pm)+factor(Country),data = df)
endo_fe_ols<-lm(endo~mvw+factor(pm)+factor(Country),data = df)

#seatshare with fe 
base_seat_fe_ols<-lm(unweighted_ministries~mvw+Seats+factor(pm)+factor(Country),data = df)
endo_seat_fe_ols<-lm(endo~mvw+Seats+factor(pm)+factor(Country),data = df)
summary(endo_seat_fe_ols)




