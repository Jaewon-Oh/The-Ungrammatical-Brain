library('languageR')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('rstatix')
library('ggpubr')
library('olsrr')

## Read data
exp1 <- read.csv('Exp1.data.csv')
exp1 <- exp1 %>%
  mutate(Insertion_of_SI = case_when(SI == 1  ~ 'Honorific',
                               SI == 0  ~ 'Nonhonorific'),
         Degree_of_Relatedness = case_when(HL == 0 ~ 'Low_degree_of_relatedness',
                                           HL == 1 ~ 'High_degree_of_relatedness')) %>%
  rename("Natural" = "NAT")


## Removing outliers
outliers <- exp1 %>%
  mutate(HH_natural = if_else(HL == 1 & SI == 1, Natural, NA),
         HN_natural = if_else(HL == 1 & SI == 0, Natural, NA),
         LH_natural = if_else(HL == 0 & SI == 1, Natural, NA),
         LN_natural = if_else(HL == 0 & SI == 0, Natural, NA)) %>%
  mutate(HH_above = case_when(HL == 1 & SI == 1 ~ mean(HH_natural,na.rm = TRUE) + 3*sd(HH_natural,na.rm = TRUE)),
         HN_above = case_when(HL == 1 & SI == 0 ~ mean(HN_natural,na.rm = TRUE) + 3*sd(HN_natural,na.rm = TRUE)),
         LH_above = case_when(HL == 0 & SI == 1 ~ mean(LH_natural,na.rm = TRUE) + 3*sd(LH_natural,na.rm = TRUE)),
         LN_above = case_when(HL == 0 & SI == 0 ~ mean(LN_natural,na.rm = TRUE) + 3*sd(LN_natural,na.rm = TRUE))) %>%
  mutate(HH_below = case_when(HL == 1 & SI == 1 ~ mean(HH_natural,na.rm = TRUE) - 3*sd(HH_natural,na.rm = TRUE)),
         HN_below = case_when(HL == 1 & SI == 0 ~ mean(HN_natural,na.rm = TRUE) - 3*sd(HN_natural,na.rm = TRUE)),
         LH_below = case_when(HL == 0 & SI == 1 ~ mean(LH_natural,na.rm = TRUE) - 3*sd(LH_natural,na.rm = TRUE)),
         LN_below = case_when(HL == 0 & SI == 0 ~ mean(LN_natural,na.rm = TRUE) - 3*sd(LN_natural,na.rm = TRUE)))

exp1_without_outliers <- outliers %>%
  filter(HH_natural < HH_above & HH_natural > HH_below | HN_natural < HN_above & HN_natural > HN_below | LH_natural < LH_above & LH_natural > LH_below | LN_natural < LN_above & LN_natural > LN_below)
nrow(exp1_without_outliers)
nrow(exp1) # 3 outliers were removed


## Releveling
exp1_without_outliers$Insertion_of_SI <- factor(exp1_without_outliers$Insertion_of_SI, levels=c("Honorific","Nonhonorific"))
levels(exp1_without_outliers$Insertion_of_SI)

## Boxplot
ggplot(exp1_without_outliers, aes(x=Insertion_of_SI, y=Natural, group = Insertion_of_SI, color = Insertion_of_SI)) + geom_boxplot() + facet_wrap(~Degree_of_Relatedness)


## Shapiro-Wilk test
exp1_without_outliers %>%
  group_by(Insertion_of_SI, Degree_of_Relatedness) %>%
  shapiro_test(Natural)

## ggplot - approximately normal
ggqqplot(exp1_without_outliers, "Natural", ggtheme = theme_bw()) +
  facet_grid(Degree_of_Relatedness ~ Insertion_of_SI, labeller = "label_both")

## ANOVA
exp1_without_outliers <- exp1_without_outliers %>%
  mutate(Insertion_of_SI = Insertion_of_SI %>% as_factor(),
         Degree_of_Relatedness = Degree_of_Relatedness %>% as_factor(),
         Natural = Natural %>% as.numeric())

exp1_without_outliers.aov <- aov(Natural~Insertion_of_SI*Degree_of_Relatedness, data = exp1_without_outliers)
summary(exp1_without_outliers.aov)

## Post-hoc
TukeyHSD(exp1_without_outliers.aov, conf.level=.95)

