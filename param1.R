# Import dataset
library(tidyverse)
library(readxl)
library(DescTools)

param1_df <- read_excel("Parameters.xlsx", sheet = "Param1")

groups <- param1_df %>% pull(Group) %>% unique()

## Summary statistics ----------or

group_by(param1_df, Group) %>%
  summarise(
    count_0h = n(),
    count_12h = n(),
    mean_0h = mean(Parameter1_0h, na.rm = TRUE),
    mean_12h = sd(Parameter1_0h, na.rm = TRUE),
    sd_0h = sd(Parameter1_0h),
    sd_12h = sd(Parameter1_12h)
  )

boxplot(
  Parameter1_0h ~ Group,
  data = param1_df,
  xlab = "Group",
  ylab = "Parameter1_0h",
  frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07")
)

boxplot(
  Parameter1_12h ~ Group,
  data = param1_df,
  xlab = "Group",
  ylab = "Parameter1_12h",
  frame = FALSE, col = c("#00AFBB", "#FEB100", "#FC4E07")
)

## Checking for Normality
    ## Observed homogeneity of variance
## Test of Normality ----

## 3/4 Groups pass normality for each time point for parameter 1
group_by(param1_df, Group) %>%
  summarise(
    normality_pvalue_0h = shapiro.test(Parameter1_0h)$p.value,
    normality_p_value_12h = shapiro.test(Parameter1_12h)$p.value,
    corr_coeff_0h_12h = cor(Parameter1_0h, Parameter1_12h, method = "pearson")
  )

## To determine the significance of all groups and placebo, I would do a OneWay
## anova on the average of both observations, which I will call "Parameter1_6h"

param1_modified_df <- param1_df %>%
  mutate(Parameter1_6h = (Parameter1_0h + Parameter1_12h) / 2)

aov_param1 <- aov(Parameter1_6h ~ Group, data = param1_modified_df)
summary_aov_param1 <- aov_param1 %>% summary()

## Validity of the model
plot(aov_param1, 1) # This gives you the Residual Plot to see that they are randomly distribute
                    # above the line, although I do see a coning effect which might force one to
                    # transform the response variable.

## I would use Dunnet's test to compare each treatment against a control.
DunnettTest(x = param1_modified_df$Parameter1_6h, g = param1_modified_df$Group, control = "Placebo")

## I would then use

## I'm doing 3 multiple comparisons, adjust my p-value via a holm correction which is good
## for maintaining power, at the cost of type I error, which is not so important here.

param1_modified_nocontrol_df <- param1_modified_df %>% filter(Group != "Placebo")

pairwise.t.test(
  param1_modified_nocontrol_df$Parameter1_6h,
  param1_modified_nocontrol_df$Group,
  p.adjust.method = "holm"
)
