
setwd("/Users/wj93/Library/CloudStorage/Dropbox/RA Paula/git replication")

## import functions
source("codes/01_weighted_kim_function.R")

## import dataset
df <- read_dta("/Users/wj93/Library/CloudStorage/Dropbox/RA Paula/R&R/Codes/00_cleaned/master_1_01.dta")
df <- read_dta("data/processed/master_1_01.dta")

## dummy potential years of experience
df <- df %>%
  mutate(lmexp = case_when(lmexp>=0&lmexp<15~0,
                           lmexp>=15&lmexp<30~1,
                           lmexp>=30&lmexp<45~2,
                           lmexp>=45~3,
  ))

## change is_PM_egp (1 is non-PM, 2 is PM)
df$is_PM_egp <- df$is_PM_egp + 1

## drop military
df <- df %>% filter(ind1990_agg != 13)
df <- df %>% select(
  -edu_attain_3cat1,-edu_attain_3cat2,-edu_attain_3cat3,
  -edu_attain_3cat_v2,-edu_attain_3cat_v21,-edu_attain_3cat_v22,-edu_attain_3cat_v23,
  -lmexp_sq
)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ 
################# Descriptive of Decomposed ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## Table 1
df %>%
  filter(year==1980|year==2019) %>%
  group_by(year,is_PM_egp) %>%
  summarize(wage=round(weighted.mean(income_pce_cap_ln,weight,na.rm=T),3),
            edu_attain_3cat_0=round(weighted.mean(edu_attain_3cat==0,weight,na.rm=T),3),
            edu_attain_3cat_1=round(weighted.mean(edu_attain_3cat==1,weight,na.rm=T),3),
            edu_attain_3cat_2=round(weighted.mean(edu_attain_3cat==2,weight,na.rm=T),3)) %>%
  as.data.frame()

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Main Decomposition ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## Table 2 main decomposition with revised normalized weight
## the output does not include bootstrap
## The results also include all numbers that appear in Table A4
kim_decomposition_longitudinal(data=df,year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)


## Table A5 decomposition by gender 
## decomposition for men
kim_decomposition_longitudinal(data=df[df$female==0,],year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

## decomposition for women
kim_decomposition_longitudinal(data=df[df$female==1,],year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# P and M separately Table A7 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## decomposition with weight with only M
kim_decomposition_longitudinal(data=df %>% filter(occ2010<500|is_PM_egp==1),
                               year_start=1980,year_end=2019,
                               dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

## decomposition with weight with only P
kim_decomposition_longitudinal(data=df %>% filter(occ2010>=500),
                               year_start=1980,year_end=2019,
                               dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# Backward Coding to 1980 Table A10 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## since 1980 occupation coding is almost the same as 1990, we inherit the 1990 codes
## however, the egp scheme is significantly different if we use 1980/1990 rather than
## 2010 occupation coding; we used the crosswalk created by Mike Hout and included in the 
## replication package of Mitnik, Pablo A. and Erin Cumberworth. 2018. "Measuring Social Class with Changing 
## Occupational Classifications: Reliability, Competing Measurement Strategies, and the 1970-1980 
## U.S. Classification Divide." Sociological Methods and Research. 

egp80 <- read_dta("data/raw/egp_h_gss_1980.dta")
egp80 <- egp80[which(egp80$selfemp==0),c("occ1980","egp_h_9")]

## merge df with EGP
df <- merge(df, egp80,
            by.x = "occ1990",
            by.y = "occ1980",
            all.x = T)
df[which(df$egp_h_9>3),"is_PM_egp"] <- 1
df[which(df$egp_h_9<=3),"is_PM_egp"] <- 2

## decomposition with weight
df <- df %>% filter(year==1980|year==2019)
kim_decomposition_longitudinal(data=df,year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# detailed education separately Table A6 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## decomposition with weight with pooled graduate (original but start in 1990)
kim_decomposition_longitudinal(data=df,
                               year_start=1990,year_end=2019,
                               dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

## decomposition with weight with master degree and professional or doctoral separated
kim_decomposition_longitudinal(data=df %>% mutate(edu_attain_4cat =
                                                    case_when(edu_years==17~2,
                                                              edu_years>18~3,
                                                              edu_years==16~1,
                                                              edu_years<16~0)),
                               year_start=1990,year_end=2019,
                               dv="income_pce_cap_ln",
                               iv=c("edu_attain_4cat","lmexp","race","public","ind1990_agg","female"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# pooled returns Table A9 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## to replicate Table A9, the only needed change is to revise how returns are 
## computed in the weighted kim decomposition function
## the revised codes SI_pooled_return.R implements pooled return for PM and keep NPM-specific return

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Top 10 MSAs drop or dummy Table A8 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## identify the top 10 MSAs in 1980-2010
top10_per_year <- df %>%
  filter(year<2019) %>%
  group_by(year, metaread) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_weight, n = 10, with_ties = FALSE) %>%
  filter(metaread!=0) %>%
  mutate(top10_flag = 1)

## flag out
filtered_df_1980_2010 <- df %>%
  left_join(top10_per_year %>% select(year, metaread, top10_flag), 
            by = c("year", "metaread")) %>%
  mutate(top10_flag = if_else(is.na(top10_flag), 0L, top10_flag)) %>%
  filter(year<2019)

## identify the top 10 MSAs in 2019
top10_per_year <- df %>%
  filter(year==2019) %>%
  group_by(year, met2013) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_weight, n = 10, with_ties = FALSE) %>%
  filter(met2013!=0) %>%
  mutate(top10_flag = 1)

## flag out
filtered_df_2019 <- df %>%
  left_join(top10_per_year %>% select(year, met2013, top10_flag), 
            by = c("year", "met2013")) %>%
  mutate(top10_flag = if_else(is.na(top10_flag), 0L, top10_flag))
filtered_df_2019 <- filtered_df_2019 %>% filter(year==2019)

## decomposition in either way
kim_decomposition_longitudinal(data=rbind(filtered_df_2019,
                                          filtered_df_1980_2010),year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female","top10_flag"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# TWFE regressing wages on education, occupation level ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## group by occ2010 and calculate percent graduate and returns to graduate
results <- data.frame()
for (y in c(1980,1990,2000,2010,2019)){
  
  ## select occupations in PM and with two or more education categories
  occ <- 
    df %>% filter(is_PM_egp==2) %>%
    filter(year==y) %>%
    group_by(occ2010) %>%
    dplyr::summarize(n=length(unique(edu_attain_3cat)),
                     size=sum(weight)) %>%
    filter(n>=2) %>%
    mutate(year=y) %>%
    select(-n)
  
  ## calculate percent graduate, percent BA, wages, race
  ## public, female, and industry
  mean_var <- 
    df %>%
    filter(year==y) %>%
    filter(is_PM_egp==2&occ2010%in%occ$occ2010) %>%
    group_by(occ2010) %>%
    dplyr::summarize(pgrad = sum(weight[edu_attain_3cat==2])/sum(weight),
                     pBA = sum(weight[edu_attain_3cat==1])/sum(weight),
                     wage = weighted.mean(income_pce_cap_ln,w=weight),
                     lmexp_1 = weighted.mean(lmexp==0,w=weight),
                     lmexp_2 = weighted.mean(lmexp==1,w=weight),
                     lmexp_3 = weighted.mean(lmexp==2,w=weight),
                     lmexp_4 = weighted.mean(lmexp==3,w=weight),
                     race_1 = sum(weight[race==1])/sum(weight),
                     race_2 = sum(weight[race==2])/sum(weight),
                     race_3 = sum(weight[race==3])/sum(weight),
                     race_4 = sum(weight[race==4])/sum(weight),
                     race_5 = sum(weight[race==5])/sum(weight),
                     public_1 = sum(weight[public==1])/sum(weight),
                     female_1 = sum(weight[female==1])/sum(weight),
                     ind1990_1 = sum(weight[ind1990_agg==1])/sum(weight),
                     ind1990_2 = sum(weight[ind1990_agg==2])/sum(weight),
                     ind1990_3 = sum(weight[ind1990_agg==3])/sum(weight),
                     ind1990_4 = sum(weight[ind1990_agg==4])/sum(weight),
                     ind1990_5 = sum(weight[ind1990_agg==5])/sum(weight),
                     ind1990_6 = sum(weight[ind1990_agg==6])/sum(weight),
                     ind1990_7 = sum(weight[ind1990_agg==7])/sum(weight),
                     ind1990_8 = sum(weight[ind1990_agg==8])/sum(weight),
                     ind1990_9 = sum(weight[ind1990_agg==9])/sum(weight),
                     ind1990_10 = sum(weight[ind1990_agg==10])/sum(weight),
                     ind1990_11 = sum(weight[ind1990_agg==11])/sum(weight),
                     ind1990_12 = sum(weight[ind1990_agg==12])/sum(weight))
  mean_var <- merge(mean_var, occ, by="occ2010", all.x=T)
  
  ## save results
  results <- rbind(results,mean_var)
}
library(lfe)
model1 <- felm(wage ~ pgrad + pBA +
                 lmexp_2 + lmexp_3 + lmexp_4 +
                 race_1 + race_2 + race_3 + race_4 + race_5 +
                 public_1 + female_1 +
                 ind1990_1 + ind1990_2 + ind1990_3 + ind1990_4 + ind1990_5 + 
                 ind1990_6 + ind1990_7 + ind1990_8 + ind1990_9 + ind1990_10 + 
                 ind1990_11 + ind1990_12
               | occ2010 + year | 0 | occ2010,
               data = results,
               weight = results$weight)

model2 <- felm(wage ~ pgrad + pBA +
                 lmexp_2 + lmexp_3 + lmexp_4 +
                 race_1 + race_2 + race_3 + race_4 + race_5 +
                 public_1 + female_1 +
                 ind1990_1 + ind1990_2 + ind1990_3 + ind1990_4 + ind1990_5 + 
                 ind1990_6 + ind1990_7 + ind1990_8 + ind1990_9 + ind1990_10 + 
                 ind1990_11 + ind1990_12
               | 0 | 0 | occ2010,
               data = results,
               weight = results$weight)

model3 <- felm(wage ~ pgrad + pBA +
                 lmexp_2 + lmexp_3 + lmexp_4 +
                 race_1 + race_2 + race_3 + race_4 + race_5 +
                 public_1 + female_1 +
                 ind1990_1 + ind1990_2 + ind1990_3 + ind1990_4 + ind1990_5 + 
                 ind1990_6 + ind1990_7 + ind1990_8 + ind1990_9 + ind1990_10 + 
                 ind1990_11 + ind1990_12
               | occ2010 | 0 | occ2010,
               data = results,
               weight = results$weight)

model4 <- felm(wage ~ pgrad + pBA +
                 lmexp_2 + lmexp_3 + lmexp_4 +
                 race_1 + race_2 + race_3 + race_4 + race_5 +
                 public_1 + female_1 +
                 ind1990_1 + ind1990_2 + ind1990_3 + ind1990_4 + ind1990_5 + 
                 ind1990_6 + ind1990_7 + ind1990_8 + ind1990_9 + ind1990_10 + 
                 ind1990_11 + ind1990_12
               | year | 0 | occ2010,
               data = results,
               weight = results$weight)

library(stargazer)
stargazer(model2, model3, model4, model1,
          type="text",
          star.cutoffs = c(0.05, 0.01, 0.001))

