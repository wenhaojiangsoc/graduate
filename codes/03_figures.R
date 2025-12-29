## codes the replicate the figures that appear in the paper
library(ggplot2)
library(ggrepel)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Figure 1 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

plot_data <- df %>%
  mutate(
    is_M_egp = case_when(
      occ2010 >= 10 & occ2010 <= 430 & is_PM_egp == 2 ~ 1,
      TRUE ~ 0
    ),
    is_P_egp = case_when(
      occ2010 > 430 & is_PM_egp == 2 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(year) %>%
  summarize(
    prop_M = sum(weight[is_M_egp == 1]) / sum(weight),
    se_M   = sqrt(sum(weight^2 * (is_M_egp - prop_M)^2)) / sum(weight),
    prop_P = sum(weight[is_P_egp == 1]) / sum(weight),
    se_P   = sqrt(sum(weight^2 * (is_P_egp - prop_P)^2)) / sum(weight),
    .groups = "drop"
  ) %>%
  filter(year >= 1980)


figure1 <- ggplot(plot_data, aes(x = year)) +
  geom_line(aes(y = prop_M, color = "management"),size=0.6) +
  geom_ribbon(aes(ymin = prop_M - 4.4172 * se_M, ymax = prop_M + 4.4172 * se_M, fill = "management"), alpha = 0.2) +
  geom_line(aes(y = prop_P, color = "professional")) +
  geom_ribbon(aes(ymin = prop_P - 4.4172 * se_P, ymax = prop_P + 4.4172 * se_P, fill = "professional"), alpha = 0.2) +
  scale_color_manual(name = "", values = c("management" = "blue", "professional" = "red")) +
  scale_fill_manual(name = "", values = c("management" = "blue", "professional" = "red")) +
  labs(x = "year", y = "proportion of workforce", title = "Panel A") +
  geom_text_repel(
    data = plot_data,
    inherit.aes = FALSE,
    aes(x = year, y = prop_M, label = sprintf("%.3f", prop_M)),
    nudge_y = 0.015,
    direction = "y",
    box.padding = 0.35,
    point.padding = 0.6, 
    segment.size = 0.25,
    segment.color = "gray50",
    force = 12,
    size = 3,
    color = "blue",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = plot_data,
    inherit.aes = FALSE,
    aes(x = year, y = prop_P, label = sprintf("%.3f", prop_P)),
    nudge_y = -0.015, 
    direction = "y",
    box.padding = 0.35,
    point.padding = 0.6,
    segment.size = 0.25,
    segment.color = "gray50",
    force = 12,
    size = 3,
    color = "red",
    show.legend = FALSE
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, color="white"))

## read education data for the US
usedu <- read.csv("data/raw/us_education.csv") %>%
  filter(year!="" & year!=" ")
usedu$year <- substr(usedu$year, 1, 4)
usedu <- usedu %>%
  mutate(across(everything(), ~ gsub(",", "", .))) %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(bachelor=bachelor/1000,
         master=master/1000,
         PhD=PhD/1000)


## panel B of figure 1
figure2 <- usedu %>%
  pivot_longer(cols = c(bachelor, master, PhD), names_to = "degree", values_to = "count") %>%
  mutate(degree=case_when(
    degree=="bachelor"~"Bachelor's",
    degree=="master"~"Master's",
    .default = degree
  )) %>%
  ggplot(aes(x = year, y = count, color = degree)) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  labs(x = "year", y = "total degrees conferred (in 1,000)", color = "",
       title="Panel B") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12, color="white"))
library(ggpubr)
ggarrange(figure1,figure2)
ggsave("figures/figure1_degree_number.tiff",
       width = 21.6, height = 10.5, units = "cm", dpi = 300)

figure1
ggsave("figures/figure1_panelA.tiff",
       width = 10.8, height = 10.5, units = "cm", dpi = 300)
figure2
ggsave("figures/figure1_panelB.tiff",
       width = 10.8, height = 10.5, units = "cm", dpi = 300)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Figure 2 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## mean wages by PM and NPM
df_summary <-
  df %>%
  group_by(sex, year, is_PM_egp) %>%
  filter(year>=1980&!is.na(income_pce_cap)) %>%
  summarize(
    se = sd(income_pce_cap, na.rm = TRUE) / sqrt(sum(!is.na(income_pce_cap))),
    income_pce_cap = weighted.mean(income_pce_cap, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(is_PM_egp=as.factor(is_PM_egp))
  
df_summary %>% 
  mutate(is_PM_egp = case_when(
    is_PM_egp == 1 ~ "NPM",
    is_PM_egp == 2 ~ "PM"
  )) %>%
  ggplot(aes(x = year, group=is_PM_egp)) +
  geom_line(aes(y = income_pce_cap, color = is_PM_egp),size=0.6) +
  geom_ribbon(aes(ymin = income_pce_cap - 6.4172 * se, 
                  ymax = income_pce_cap + 6.4172 * se, fill = is_PM_egp), alpha = 0.2) +
  labs(x = "", y = "wages in 2019 USD", title = "") +
  theme_bw() +
  facet_grid(~sex, labeller = labeller(sex = c(`1` = "Male", `2` = "Female"))) +
  scale_color_manual(
    values = c("NPM" = "blue", "PM" = "red"),
    name = "") +
  scale_fill_manual(
    values = c("NPM" = "blue", "PM" = "red"),
    name = "") +
  geom_text_repel(
    aes(x = year, y = income_pce_cap, label = sprintf("%.2f", income_pce_cap), color = is_PM_egp),
    nudge_y = 0.02, 
    direction = "y",
    box.padding = 0.35,
    point.padding = 0.6,
    segment.size = 0.25,
    segment.color = "gray50",
    force = 12,
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12),
        axis.text.x = element_text(angle=45,vjust=0.5),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(2, "lines"))
ggsave("figures/figure2_PMwages.tiff",
       width = 19.6, height = 10, units = "cm", dpi = 300)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Figure 3 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## mean wages by education level
df_summary <-
  df %>%
  group_by(sex, year, edu_attain_3cat) %>%
  filter(year>=1980&!is.na(income_pce_cap)) %>%
  summarize(
    se = sd(income_pce_cap, na.rm = TRUE) / sqrt(sum(!is.na(income_pce_cap))),
    income_pce_cap = weighted.mean(income_pce_cap, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(edu_attain_3cat=as.factor(edu_attain_3cat))

df_summary %>% 
  mutate(edu_attain_3cat = case_when(
    edu_attain_3cat == 0 ~ "Less than BA/BS",
    edu_attain_3cat == 1 ~ "BA/BS",
    edu_attain_3cat == 2 ~ "Graduate"
  )) %>%
  ggplot(aes(x = year, group=edu_attain_3cat)) +
  geom_line(aes(y = income_pce_cap, color = edu_attain_3cat),size=0.6) +
  geom_ribbon(aes(ymin = income_pce_cap - 4.4172 * se, 
                  ymax = income_pce_cap + 4.4172 * se, fill = edu_attain_3cat), alpha = 0.2) +
  labs(x = "", y = "wages in 2019 USD", title = "") +
  theme_bw() +
  facet_grid(~sex, labeller = labeller(sex = c(`1` = "Male", `2` = "Female"))) +
  scale_color_manual(
    values = c("Less than BA/BS" = "blue", "BA/BS" = "green3", "Graduate" = "red"),
    name = "") +
  scale_fill_manual(
    values = c("Less than BA/BS" = "blue", "BA/BS" = "green3", "Graduate" = "red"),
    name = "") +
  geom_text_repel(
    aes(x = year, y = income_pce_cap, label = sprintf("%.2f", income_pce_cap), color = edu_attain_3cat),
    nudge_y = 0.02, 
    direction = "y",
    box.padding = 0.35,
    point.padding = 0.6,
    segment.size = 0.25,
    segment.color = "gray50",
    force = 12,
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=10.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=12),
        axis.text.x = element_text(angle=45,vjust=0.5),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(2, "lines"))
ggsave("figures/figure3_graduate_wages.tiff",
       width = 19.6, height = 10, units = "cm", dpi = 300)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Figure 4 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

library(reshape2)

## calculate proportion of workers in each category
prop <-
  rbind(
    df %>%
      dplyr::group_by(year,is_PM_egp) %>%
      dplyr::summarize(graduate = sum(weight[edu_attain_3cat==2])/sum(weight),
                       college = sum(weight[edu_attain_3cat==1])/sum(weight),
                       BC = sum(weight[edu_attain_3cat==0])/sum(weight)) %>%
      as.data.frame(),
    df %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(graduate = sum(weight[edu_attain_3cat==2])/sum(weight),
                       college = sum(weight[edu_attain_3cat==1])/sum(weight),
                       BC = sum(weight[edu_attain_3cat==0])/sum(weight)) %>%
      as.data.frame() %>% mutate(is_PM_egp=3)
  )

## reshape and plot
prop <-
  melt(prop, id.vars=c("year","is_PM_egp"))
prop %>%
  rename(education=variable,
         PM=is_PM_egp) %>%
  mutate(education = case_when(
    education == "BC" ~ "below college",
    .default = education
  )) %>%
  mutate(education = case_when(education=="below college"~"below BA/BS",
                               education=="college"~"BA/BS",
                               education=="graduate"~"graduate")) %>%
  mutate(education = factor(education, levels=c("below BA/BS","BA/BS","graduate"))) %>%
  filter(year>=1980) %>%
  ggplot(aes(x = year, y = value, color = factor(PM))) +
  geom_line() +
  geom_point(size=1.3) +
  facet_grid(.~education) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  scale_color_manual(values=c("blue3","red3","grey60"),labels=c("NPM","PM","pooled"),name="") +
  ylab("Proportion") +
  xlab("") +
  xlim(1977,2023) +
  theme_bw() +
  geom_text_repel(
    aes(label = sprintf("%.3f", value), color = factor(PM)),
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  theme(text = element_text(family="Times"),
        legend.position="bottom",
        plot.title = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6),
        axis.text.y = element_text(size=10, angle=90, hjust=0.5),
        axis.text.x = element_text(size=10),
        axis.title.x = element_blank(),
        axis.title=element_text(size=14,hjust=0.5),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) + 
  guides(colour = guide_legend(override.aes = list(linewidth = 0.8, size = 2)))

## save
ggsave("figures/figure4_composition_edu.png", width = 15, height = 10, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
################# Figure 5 ##################
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## store results
result <-
  data.frame(
    year = rep(c(seq(1980,2010,10),2019),3),
    coef = rep(NA,15),
    PM = c(rep(1,5),rep(2,5),rep(3,5))
  )
result <- rbind(result,result,result)
result$education <- c(rep("below college",15),rep("college",15),rep("graduate",15))

for (y in c(seq(1980,2010,10),2019)){
  for (pm in c(1,2)){
    
    ## normalize returns using the same 1980 and 2019 data
    returns <- normalize_coefficient(data_reg = df %>% filter(year==y&is_PM_egp==pm), 
                                     data_weight = df %>% filter(year==1980|year==2019),
                                     dv = "income_pce_cap_ln",
                                     columns = c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                                     normal_weight = TRUE)
    
    ## below college
    result[result$year==y&result$PM==pm&result$education=="below college","coef"] <-
      returns[1,2]
    
    ## college
    result[result$year==y&result$PM==pm&result$education=="college","coef"] <-
      returns[2,2]
    
    ## graduate
    result[result$year==y&result$PM==pm&result$education=="graduate","coef"] <-
      returns[3,2]
  }
  
  ## pooled returns
  returns <- normalize_coefficient(data_reg = df %>% filter(year==y), 
                                   data_weight = df %>% filter(year==1980|year==2019),
                                   dv = "income_pce_cap_ln",
                                   columns = c("edu_attain_3cat","lmexp","race","public","ind1990_agg","female"),
                                   normal_weight = TRUE)
  
  ## below college
  result[result$year==y&result$PM==3&result$education=="below college","coef"] <-
    returns[1,2]
  
  ## college
  result[result$year==y&result$PM==3&result$education=="college","coef"] <-
    returns[2,2]
  
  ## graduate
  result[result$year==y&result$PM==3&result$education=="graduate","coef"] <-
    returns[3,2]
  
  print(paste(y, "is done!"))
}

## Figure 5
result %>%
  mutate(education = case_when(education=="below college"~"below BA/BS",
                               education=="college"~"BA/BS",
                               education=="graduate"~"graduate")) %>%
  mutate(education = factor(education, levels=c("below BA/BS","BA/BS","graduate"))) %>%
  ggplot(aes(x = year, y = coef, color = factor(PM))) +
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_line() +
  geom_point(size=1.3) +
  facet_grid(.~education) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 20)) +
  scale_color_manual(values=c("blue3","red3","grey60"),labels=c("NPM","PM","Pooled"),name="") +
  ylab("wage returns") +
  xlab("") +
  xlim(1977,2023) +
  theme_bw() +
  geom_text_repel(
    aes(label = sprintf("%.3f", coef), color = factor(PM)),
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  theme(text = element_text(family="Times"),
        legend.position="bottom",
        plot.title = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6),
        axis.text.y = element_text(size=10, angle=90, hjust=0.5),
        axis.text.x = element_text(size=10),
        axis.title.x = element_blank(),
        axis.title=element_text(size=14,hjust=0.5),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) + 
  guides(colour = guide_legend(override.aes = list(linewidth = 0.8, size = 2)))

## save results
ggsave("figures/figure5_return_edu_normalized.png", 
       width = 13, height = 9, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
################# Figure 6 Predicting PM Occupations ###############
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## rescale weight so that weight sums to mean in each year
## this is to ensure reasonable model convergence
df <- df %>%
  group_by(year) %>%
  mutate(weight = weight/mean(weight))

## run multinomial every year
results_detailed <- data.frame()
library(nnet) 
df_multi <- df %>% mutate(P_M_egp = case_when(
  occ2010 >= 10 & occ2010 <= 430 & is_PM_egp == 2 ~ 1, ## management
  (occ2010 > 430 & is_PM_egp == 2) ~ 2,      ## professional 
  .default = 0                                  ## non-PM
))

for (year in c(1980,1990,2000,2010,2019)) {
  
  ## subset once for this year
  dsub <- df_multi[df_multi$year==year,]
  
  ## specify model and predict (use weights=)
  model <- multinom(
    P_M_egp ~ factor(edu_attain_3cat) + factor(lmexp) + factor(race) +
      factor(public) + factor(ind1990_agg),
    data = dsub,
    weights = weight
  )
  
  ## get predicted probs for the same rows in dsub
  preds <- predict(model, newdata = dsub, type = "probs")
  
  ## summarize (columns will be named "0","1","2" if your factor is coded that way)
  result <- dsub %>%
    bind_cols(as.data.frame(preds)) %>%
    group_by(edu_attain_3cat) %>%
    summarize(
      prob_P = weighted.mean(`2`, weight),
      prob_M = weighted.mean(`1`, weight),
      prob_O = weighted.mean(`0`, weight),
      .groups = "drop"
    ) %>%
    mutate(year=year)
  
  results_detailed <- rbind(results_detailed, result)
}

## plot the results
library(ggrepel)
results_detailed %>%
  mutate(prob_PM = prob_P + prob_M) %>%
  mutate(
    edu_attain_3cat = factor(
      edu_attain_3cat,
      levels = c(0, 1, 2),
      labels = c("less than BA/BS", "BA/BS", "graduate")
    )
  ) %>%
  pivot_longer(
    cols = starts_with("prob_"),     # prob_P, prob_M, prob_O
    names_to = "category",
    values_to = "prob"
  ) %>%
  filter(category!="prob_O") %>%
  mutate(category = factor(category,
                           levels = c("prob_PM", "prob_P", "prob_M"))) %>%
  ggplot(aes(x = year, y = prob)) +
  geom_line(aes(color = edu_attain_3cat)) +
  geom_point(aes(color = edu_attain_3cat)) +
  geom_text_repel(
    aes(label = sprintf("%.3f", prob), color = edu_attain_3cat),
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  theme_bw() +
  facet_grid(~category, labeller = labeller(category = c("prob_PM" = "PM combined", 
                                                         "prob_P" = "professional",
                                                         "prob_M" = "managerial"))) +
  scale_color_manual(
    name = "", 
    labels = c("less than BA/BS", "BA/BS", "graduate"),
    values = c("less than BA/BS" = "blue",
               "BA/BS"  = "green3",
               "graduate" = "red")
  ) +
  theme(text = element_text(family="Times"),
        legend.position="bottom",
        plot.title = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 14, colour = "black", face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6),
        axis.text.y = element_text(size=10, angle=90, hjust=0.5),
        axis.text.x = element_text(size=10),
        axis.title.x = element_blank(),
        axis.title=element_text(size=14,hjust=0.5),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) + 
  guides(colour = guide_legend(override.aes = list(linewidth = 0.8, size = 2))) +
  labs(
    color = "",
    x = "",
    y = "Predicted Probability"
  ) +
  ylim(0,0.9) +
  xlim(1979,2021)

ggsave("figures/figure6_predictedprob.png",
       width = 15, height = 10, units = "cm", dpi = 300)

## print out and summarise results in a table in Appendix
s <- summary(model)
coef_mat <- s$coefficients       # (K-1) x p
se_mat   <- s$standard.errors    # same dims

## RRRs and CIs
RRR <- exp(coef_mat)
lower <- exp(coef_mat - 1.96 * se_mat)
upper <- exp(coef_mat + 1.96 * se_mat)
pval <- 2 * pnorm(-abs(coef_mat / se_mat))

## reshape into a tidy data.frame for printing
library(tibble)
library(dplyr)
df_out <- as.data.frame(do.call(rbind, lapply(1:nrow(coef_mat), function(i) {
  data.frame(
    comparison = rownames(coef_mat)[i],
    term = colnames(coef_mat),
    coef = coef_mat[i, ],
    se = se_mat[i, ],
    RRR = RRR[i, ],
    RRR_lo = lower[i, ],
    RRR_hi = upper[i, ],
    p = pval[i, ],
    stringsAsFactors = FALSE
  )
})), stringsAsFactors = FALSE)

## optionally format and print
df_out %>% arrange(comparison, term)
