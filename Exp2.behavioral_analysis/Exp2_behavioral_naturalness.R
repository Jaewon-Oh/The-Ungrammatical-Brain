library('languageR')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('rstatix')
library(ggpubr)

natu <- read.csv('r_natural.csv')
natu <- natu %>%
  mutate(Honorific = case_when(si == 1  ~ 'Honorific',
                               si == 0  ~ 'Nonhonorific'),
         Degree_of_Relatedness = case_when(rel == 0 ~ 'Low',
                                           rel == 1 ~ 'High')) %>%
  rename("Natural" = "natural")

## Removing outliers
outliers <- natu %>%
  mutate(HH_natural = if_else(rel == 1 & si == 1, Natural, NA),
         HN_natural = if_else(rel == 1 & si == 0, Natural, NA),
         LH_natural = if_else(rel == 0 & si == 1, Natural, NA),
         LN_natural = if_else(rel == 0 & si == 0, Natural, NA)) %>%
  mutate(HH_above = case_when(rel == 1 & si == 1 ~ mean(HH_natural,na.rm = TRUE) + 3*sd(HH_natural,na.rm = TRUE)),
         HN_above = case_when(rel == 1 & si == 0 ~ mean(HN_natural,na.rm = TRUE) + 3*sd(HN_natural,na.rm = TRUE)),
         LH_above = case_when(rel == 0 & si == 1 ~ mean(LH_natural,na.rm = TRUE) + 3*sd(LH_natural,na.rm = TRUE)),
         LN_above = case_when(rel == 0 & si == 0 ~ mean(LN_natural,na.rm = TRUE) + 3*sd(LN_natural,na.rm = TRUE))) %>%
  mutate(HH_below = case_when(rel == 1 & si == 1 ~ mean(HH_natural,na.rm = TRUE) - 3*sd(HH_natural,na.rm = TRUE)),
         HN_below = case_when(rel == 1 & si == 0 ~ mean(HN_natural,na.rm = TRUE) - 3*sd(HN_natural,na.rm = TRUE)),
         LH_below = case_when(rel == 0 & si == 1 ~ mean(LH_natural,na.rm = TRUE) - 3*sd(LH_natural,na.rm = TRUE)),
         LN_below = case_when(rel == 0 & si == 0 ~ mean(LN_natural,na.rm = TRUE) - 3*sd(LN_natural,na.rm = TRUE)))

natu_without_outliers <- outliers %>%
  filter(HH_natural < HH_above & HH_natural > HH_below | HN_natural < HN_above & HN_natural > HN_below | LH_natural < LH_above & LH_natural > LH_below | LN_natural < LN_above & LN_natural > LN_below)
nrow(natu_without_outliers)
nrow(natu) # 3 outliers were removed

## Boxplot
natu_without_outliers$Honorific <- factor(natu_without_outliers$Honorific, levels=c("Honorific","Nonhonorific"))
levels(natu_without_outliers$Honorific)

ggplot(natu_without_outliers, aes(x=Honorific, y=Natural, group = Honorific, color = Honorific)) + geom_boxplot() + facet_wrap(~Degree_of_Relatedness)


## Shapiro-Wilk test
natu_without_outliers %>%
  group_by(Honorific, Degree_of_Relatedness) %>%
  shapiro_test(Natural)

## ggplot - approximately normal
ggqqplot(natu_without_outliers, "Natural", ggtheme = theme_bw()) +
  facet_grid(Degree_of_Relatedness ~ Honorific, labeller = "label_both")


## ANOVA
natu_without_outliers <- natu_without_outliers %>%
  mutate(Honorific = Honorific %>% as_factor(),
         Degree_of_Relatedness = Degree_of_Relatedness %>% as_factor(),
         Natural = Natural %>% as.numeric())

natu_without_outliers.aov <- aov(Natural~Honorific*Degree_of_Relatedness, data = natu_without_outliers)
summary(natu_without_outliers.aov)

## Post-hoc
TukeyHSD(natu_without_outliers.aov, conf.level=.95)
