
## import functions
source("/Users/wenhao/Library/CloudStorage/Dropbox/RA Paula/New SF R&R/codes/weighted_kim_function.R")

## import dataset
df <- read_dta("/Users/wenhao/Dropbox/RA Paula/New SF R&R/data/master_1_01.dta")
df <- read_dta("/Users/wenhao/Library/CloudStorage/Dropbox/RA Paula/R&R/Codes/00_cleaned/master_1_01.dta")

## dummy potential years of experience
df <- df %>%
  mutate(lmexp = case_when(lmexp>=0&lmexp<15~0,
                           lmexp>=15&lmexp<30~1,
                           lmexp>=30&lmexp<45~2,
                           lmexp>=45~3,
  ))

## change is_PM_egp
df$is_PM_egp <- df$is_PM_egp + 1

## drop military
df <- df %>% filter(ind1990_agg != 13)
df <- df %>% select(
  -edu_attain_3cat1,-edu_attain_3cat2,-edu_attain_3cat3,
  -edu_attain_3cat_v2,-edu_attain_3cat_v21,-edu_attain_3cat_v22,-edu_attain_3cat_v23,
  -lmexp_sq
)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# 
################# Without Skill Measures ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#  

## decomposition with no weight
kim_decomposition_longitudinal(data=df,year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg"),
                               decomp_var="is_PM_egp",
                               normal_weight=FALSE)

## decomposition with weight


## 1980-2010
top10_per_year <- df %>%
  filter(year<2019) %>%
  group_by(year, metaread) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_weight, n = 10, with_ties = FALSE) %>%
  filter(metaread!=0) %>%
  mutate(top10_flag = 1)

# Step 2: Filter the original df to exclude those top 10 metareads per year
filtered_df_1980_2010 <- df %>%
  anti_join(top10_per_year, by = c("year", "metaread"))

filtered_df_1980_2010 <- df %>%
  left_join(top10_per_year %>% select(year, metaread, top10_flag), 
            by = c("year", "metaread")) %>%
  mutate(top10_flag = if_else(is.na(top10_flag), 0L, top10_flag)) %>%
  filter(year<2019)

## 2019
top10_per_year <- df %>%
  filter(year==2019) %>%
  group_by(year, met2013) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_weight, n = 10, with_ties = FALSE) %>%
  filter(met2013!=0) %>%
  mutate(top10_flag = 1)

# Step 2: Filter the original df to exclude those top 10 metareads per year
filtered_df_2019 <- df %>%
  filter(year==2019) %>%
  anti_join(top10_per_year, by = c("year", "met2013"))

filtered_df_2019 <- df %>%
  left_join(top10_per_year %>% select(year, met2013, top10_flag), 
            by = c("year", "met2013")) %>%
  mutate(top10_flag = if_else(is.na(top10_flag), 0L, top10_flag))
filtered_df_2019 <- filtered_df_2019 %>% filter(year==2019)


kim_decomposition_longitudinal(data=rbind(filtered_df_2019,
                                          filtered_df_1980_2010),year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female","top10_flag"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)


## decomposition with weight - men
kim_decomposition_longitudinal(data=df[df$female==0,],year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

## decomposition with weight - women
kim_decomposition_longitudinal(data=df[df$female==1,],year_start=1980,year_end=2019,dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg"),
                               decomp_var="is_PM_egp",
                               normal_weight=TRUE)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# P and M separately ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

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

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# detailed education separately ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

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

## decomposition with weight with professional or doctoral
kim_decomposition_longitudinal(data=df %>% filter(occ2010>=500),
                               year_start=1980,year_end=2019,
                               dv="income_pce_cap_ln",
                               iv=c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
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
