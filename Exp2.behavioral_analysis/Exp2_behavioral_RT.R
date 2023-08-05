library('languageR')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('rstatix')
library(ggpubr)
install.packages('EBImage')
library("EBImage")

rt <- read.csv('r_rt.csv')
rt <- rt %>%
  mutate(Honorific = case_when(si == 1  ~ 'Honorific',
                               si == 0  ~ 'Nonhonorific'),
         Degree_of_Relatedness = case_when(rel == 0 ~ 'Low',
                                           rel == 1 ~ 'High')) %>%
  rename("RT" = "rt")

## Removing outliers
outliers <- rt %>%
  mutate(HH_rt = if_else(rel == 1 & si == 1, RT, NA),
         HN_rt = if_else(rel == 1 & si == 0, RT, NA),
         LH_rt = if_else(rel == 0 & si == 1, RT, NA),
         LN_rt = if_else(rel == 0 & si == 0, RT, NA)) %>%
  mutate(HH_above = case_when(rel == 1 & si == 1 ~ mean(HH_rt,na.rm = TRUE) + 3*sd(HH_rt,na.rm = TRUE)),
         HN_above = case_when(rel == 1 & si == 0 ~ mean(HN_rt,na.rm = TRUE) + 3*sd(HN_rt,na.rm = TRUE)),
         LH_above = case_when(rel == 0 & si == 1 ~ mean(LH_rt,na.rm = TRUE) + 3*sd(LH_rt,na.rm = TRUE)),
         LN_above = case_when(rel == 0 & si == 0 ~ mean(LN_rt,na.rm = TRUE) + 3*sd(LN_rt,na.rm = TRUE))) %>%
  mutate(HH_below = case_when(rel == 1 & si == 1 ~ mean(HH_rt,na.rm = TRUE) - 3*sd(HH_rt,na.rm = TRUE)),
         HN_below = case_when(rel == 1 & si == 0 ~ mean(HN_rt,na.rm = TRUE) - 3*sd(HN_rt,na.rm = TRUE)),
         LH_below = case_when(rel == 0 & si == 1 ~ mean(LH_rt,na.rm = TRUE) - 3*sd(LH_rt,na.rm = TRUE)),
         LN_below = case_when(rel == 0 & si == 0 ~ mean(LN_rt,na.rm = TRUE) - 3*sd(LN_rt,na.rm = TRUE)))

rt_without_outliers <- outliers %>%
  filter(HH_rt < HH_above & HH_rt > HH_below | HN_rt < HN_above & HN_rt > HN_below | LH_rt < LH_above & LH_rt > LH_below | LN_rt < LN_above & LN_rt > LN_below)
nrow(rt_without_outliers)
nrow(rt) # 3 outliers were removed

## Boxplot
rt_without_outliers$Honorific <- factor(rt_without_outliers$Honorific, levels=c("Honorific","Nonhonorific"))
levels(rt_without_outliers$Honorific)

ggplot(rt_without_outliers, aes(x=Honorific, y=RT, group = Honorific, color = Honorific)) + geom_boxplot() + facet_wrap(~Degree_of_Relatedness)


## Shapiro-Wilk test
rt_without_outliers %>%
  group_by(Honorific, Degree_of_Relatedness) %>%
  shapiro_test(RT)

## ggplot - approximately normal
ggqqplot(rt_without_outliers, "RT", ggtheme = theme_bw()) +
  facet_grid(Degree_of_Relatedness ~ Honorific, labeller = "label_both")

## ANOVA
rt_without_outliers <- rt_without_outliers %>%
  mutate(Honorific = Honorific %>% as_factor(),
         Degree_of_Relatedness = Degree_of_Relatedness %>% as_factor(),
         RT = RT %>% as.numeric())

rt_without_outliers.aov <- aov(RT~Honorific*Degree_of_Relatedness, data = rt_without_outliers)
summary(rt_without_outliers.aov)

## Post-hoc
TukeyHSD(rt_without_outliers.aov, conf.level=.95)
