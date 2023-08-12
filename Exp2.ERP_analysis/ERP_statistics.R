library('languageR')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('rstatix')
library(ggpubr)

## Read data and tidy
ave1234 <- read.csv('ERP_data.csv')
ave1234 <- ave1234 %>%
  select(File,starts_with('F7')|starts_with('F3')|starts_with('Fz')|starts_with('F4')|starts_with('F8')|starts_with('FC5')|starts_with('FC1')|starts_with('FC2')|starts_with('FC6')|starts_with('T7')|starts_with('C3')|starts_with('Cz')|starts_with('C4')|starts_with('T8')|starts_with('CP5')|starts_with('CP1')|starts_with('CP2')|starts_with('CP6')|starts_with('P7')|starts_with('P3')|starts_with('Pz')|starts_with('P4')|starts_with('P8'))%>%
  pivot_longer(cols=-File, names_to=c("electrode","condition"), names_sep ='\\.', values_to = 'amplitude')

ave1234 <- na.omit(ave1234)
write.csv(ave1234, file = 'amiright.csv')

## Add columns indicating each factor
ave1234_data <- ave1234%>%
  mutate(relatedness = case_when(condition == 'ave1'~ 'High',
                                 condition == 'ave2'~ 'High',
                                 condition == 'ave3'~ 'Low',
                                 condition == 'ave4'~ 'Low'))%>%
  mutate(honorific = case_when(condition == 'ave1' ~ 'Honor',
                               condition == 'ave3' ~ 'Honor',
                               condition == 'ave2' ~ 'Non',
                               condition == 'ave4' ~ 'Non'))

left <- c('F7', 'F3', 'FC5', 'FC1', 'T7', 'C3', 'CP5', 'CP1', 'P7', 'P3')
right <- c('F4', 'F8', 'FC2', 'FC6', 'C4', 'T8', 'CP2', 'CP6', 'P4', 'P8')
mid <- c('Fz', 'Cz', 'Pz')

ave1234_data <- ave1234_data %>%
  mutate(lmr = case_when(electrode %in% left ~ 'Left',
                         electrode %in% right ~ 'Right',
                         electrode %in% mid ~ 'Mid'))

############# Lateral P600 Analysis #############
lateral <- c('F7', 'F8', 'FC5', 'FC6', 'T7', 'T8', 'CP5', 'CP6', 'P7', 'P8')
medial <- c('F3', 'F4', 'FC1', 'FC2', 'C3', 'C4', 'CP1', 'CP2', 'P3', 'P4')

ant <- c('F7', 'F3', 'F4', 'F8')
antcen <- c('FC5','FC1', 'FC2', 'FC6')
cen <- c('T7', 'C3', 'C4', 'T8')
cenpost <- c('CP5', 'CP1', 'CP2', 'CP6')
post <- c('P7', 'P3', 'P4', 'P8')

ave1234_lateral_data <- ave1234_data %>%
  filter(lmr == 'Left'|lmr == 'Right') %>%
  mutate(lat = case_when(electrode %in% lateral ~ 'Lateral',
                         electrode %in% medial ~ 'Medial')) %>%
  mutate(antpost = case_when(electrode %in% ant ~ 'Ant',
                             electrode %in% antcen ~ 'AntCen',
                             electrode %in% cen ~ 'Cen',
                             electrode %in% cenpost ~ 'CenPost',
                             electrode %in% post ~ 'Post')) %>%
  rename(participant = File)

write.csv(ave1234_lateral_data, file = 'check.csv')

## Shapiro-Wilk test
ave1234_lateral_data %>%
  group_by(relatedness, honorific, lmr, lat, antpost) %>%
  shapiro_test(amplitude)

## Draw a plot
ggqqplot(ave1234_lateral_data, "amplitude", ggtheme = theme_bw()) +
  facet_grid(relatedness + honorific ~ lmr + lat + antpost, labeller = "label_both")

## ANOVA test
ave1234_lateral.aov <- anova_test(
  data = ave1234_lateral_data, dv = amplitude, wid = participant,
  within = c(relatedness, honorific, lmr, lat, antpost)
)
get_anova_table(ave1234_lateral.aov) #relatedness:lmr 0.033, lmr:antpost 0.045, lat:antpost 0.037

# Pairwise comparisons
pwc <- ave1234_lateral_data %>%
  group_by(lmr) %>%
  pairwise_t_test(amplitude ~ relatedness, paired = TRUE, p.adjust.method = "bonferroni")

pwc

############# Midline P600 Analysis #############

ave1234_midline_data <- ave1234_data %>%
  filter(lmr == 'Mid') %>%
  mutate(fcp = case_when(electrode == 'Fz' ~ 'F',
                         electrode == 'Cz' ~ 'C',
                         electrode == 'Pz' ~ 'P'))%>%
  rename(participant = File)

write.csv(ave1234_midline_data, file = 'checkmid.csv')

## Shapiro-Wilk test
ave1234_midline_data %>%
  group_by(relatedness, honorific, fcp) %>%
  shapiro_test(amplitude)

## Draw a plot
ggqqplot(ave1234_midline_data, "amplitude", ggtheme = theme_bw()) +
  facet_grid(relatedness + honorific ~ fcp, labeller = "label_both")

## ANOVA test
ave1234_midline.aov <- anova_test(
  data = ave1234_midline_data, dv = amplitude, wid = participant,
  within = c(relatedness, honorific, fcp)
)
get_anova_table(ave1234_midline.aov) #relatedness:fcp 1.56e-05

#two-way interaction
two.way <- ave1234_midline_data %>%
  group_by(fcp) %>%
  anova_test(dv = amplitude, wid = participant, within = c(relatedness, honorific))
two.way
get_anova_table(two.way) #P relatedness 0.006

# Pairwise comparisons
pwc <- ave1234_midline_data %>%
  group_by(fcp, honorific) %>%
  pairwise_t_test(amplitude ~ relatedness, paired = TRUE, p.adjust.method = "bonferroni")
# Show comparison results for "P" groups
pwc %>% filter(fcp == "P") %>%
  select(-p)     # remove p columns

#honorific fcp   .y.       group1 group2    n1    n2 p.adj p.adj.signif
#<chr>     <chr> <chr>     <chr>  <chr>  <int> <int> <dbl> <chr>       
#  1 Honor     P     amplitude High   Low       20    20 0.15  ns          
#2 Non       P     amplitude High   Low       20    20 0.008 **      
