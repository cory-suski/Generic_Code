# Getting Started ---------------------------------------------------------
getwd()
library(tidyverse)
library(car)
library(usethis)
library(gitcreds)
#continuing to struggle with stuff

updateR() # will check for new R updates through the installr package
packageStatus() # checks whether or not there are any packages that need to be updated
update.packages(checkBuilt = TRUE) # will update all packages
packageVersion("MuMIn") # one method for checking version of a package
getNamespaceVersion("ggplot2") # alternate method for checking version of a package
citation("usethis") # provides the citation for a package
sessionInfo() # shows R version, packages, & other useful stuff (useful for stats sections of papers)
version # will provide current version of R only (sessionInfo is more useful for papers)

# Data_import -------------------------------------------------------------
data <- read_csv("data.csv", col_types = cols(Sample_type = col_factor(levels = c(
  "Factor_1", "Factor_2", "Factor_3", "Factor_4", "Factor_5",
))))

summary(Avg_CT)
glimpse(Avg_CT)
head(Avg_CT)

# Summary_statistics ------------------------------------------------------
Summary %>%
  group_by(Sample_type) %>%
  select(c("Factor")) %>%
  summarise_all(list(
    "sample_size" = ~ sum(!is.na(.)), # counts only cells with data (i.e., sample size)
    "mean" = ~ mean(., na.rm = TRUE),
    "sd" = ~ sd(., na.rm = TRUE),
    "se" = ~ sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
    "median" = ~ median(., na.rm = TRUE),
    "variance" = ~ var(., na.rm = TRUE),
    "min" = ~ min(., na.rm = TRUE),
    "max" = ~ max(., na.rm = TRUE),
    "5th" = ~ quantile(., .05, na.rm = TRUE),
    "25th" = ~ quantile(., .25, na.rm = TRUE),
    "50th" = ~ quantile(., 0.5, na.rm = TRUE),
    "75th" = ~ quantile(., 0.75, na.rm = TRUE),
    "95th" = ~ quantile(., .95, na.rm = TRUE)
  )) %>% 
  mutate(lower.95.ci = mean - qt(1 - (0.05 / 2), sample_size - 1) * se,
         upper.95.ci = mean + qt(1 - (0.05 / 2), sample_size - 1) * se) %>% 
  write.csv(., file = "Avg_CT_statistics.csv")

# ANOVA -------------------------------------------------------------------
ANOVA <- aov(Response ~ Sample_type, data = data)
summary(ANOVA)
Anova(ANOVA)
anova(ANOVA)
write.csv(Anova(ANOVA), 'ANOVA.csv') #save Anova results as .csv file

plot(ANOVA)

# check residuals for CTMax_ANOVA - run both lines of code together
par(mfrow = c(2, 2))
plot(lm(ANOVA))

# Diagnostic plots through car package
avPlots(ANOVA)
qqPlot(ANOVA, id.n = 3)
outlierTest(ANOVA)
influenceIndexPlot(ANOVA)
influencePlot(ANOVA)

# run these 4 lines of code together, also from car package
par(mfrow = c(2, 2))
crPlots(ANOVA)
influencePlot(ANOVA)
residualPlot(ANOVA)
hist(resid(ANOVA))

## Post hoc tests -----------------------------------------------------------
# multiple comparisons (i.e., post hoc test) Base R
TukeyHSD(ANOVA)

# multiple comparisons (i.e., post hoc test) using emmeans
CTMax_Emmeans <- emmeans(ANOVA, "Sample_type")
pairs(CTMax_Emmeans)
plot(CTMax_Emmeans, comparisons = TRUE)
pwpp(CTMax_Emmeans)
pwpm(CTMax_Emmeans)
emmeans(CTMax_Emmeans, list(pairwise ~ Sample_type), adjust = "tukey")
multcomp::cld(CTMax_Emmeans, alpha = 0.05, Letters = LETTERS) # display letters thru multcomp

# Plot --------------------------------------------------------------------
ggplot(data, aes(x = Sample_type, y = Response)) +
  geom_boxplot() +
  geom_jitter(
    alpha = 0.4,
    position = position_jitter(width = 0.15)
  ) +
  stat_summary(
    fun = mean, geom = "point", shape = 5, size = 1.25, stroke = 2, na.rm = TRUE,
    color = "tomato", position = position_dodge(width = 0.75), show.legend = FALSE
  ) +
  annotate("text", x = 1, y = 9, label = "a", fontface = "bold", colour = "black", size = 6) +
  annotate("text", x = 2, y = 12, label = "b", fontface = "bold", colour = "black", size = 6) +
ggsave("figure.jpeg", dpi = 1200)

# Upload to Github --------------------------------------------------------
library(usethis)
library(gitcreds)
use_git()
