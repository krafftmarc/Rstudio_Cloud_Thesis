install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("lmerTest")
library(lmerTest)
install.packages("emmeans")
library(emmeans)
install.packages("afex")
library(afex)
install.packages("ez")
library(ez)
JD_CH_midday <- CH_Midday$Julian.Date
CH_Midday.lmer <- lmer(PSI_CH_midday~ID_CH_midday*avgT_CH_midday + (1 | JD_CH_midday), data = midday)
CH_Midday.lmer
CH_midday.lmer.Anova <- car::Anova(CH_Midday.lmer, type = 3)
plot(CH_midday.lmer.Anova)
CH_midday.lmer.Anova
CH_midday.lsm <- emmeans::emmeans(CH_Midday.model, ID_CH_midday~PSI_CH_midday, adjust="fdr")
CH_midday.lmer.Anova
avgT_CH_midday <- CH_Midday$Avg.Air.T..F.
water_T <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_PSI_2022_Cleaned_W_ETo_T.csv")
midday <- water_T %>% filter(Time ==1300) %>% filter(!is.na(PSI))
CH_Midday <- midday %>% filter(Variety == "CH")
CS_Midday <- midday %>% filter(Variety == "CS")
ID_CH_midday <- CH_Midday$ID
ID_CS_midday <- CS_Midday$ID
PSI_CH_midday <- CH_Midday$PSI
PSI_CS_midday <- CS_Midday$PSI
maxT_CH_midday <- CH_Midday$Max.Air.T..F.
maxT_CS_midday <- CS_Midday$Max.Air.T..F.
CH_Midday.model <- lm(ID_CH_midday~PSI_CH_midday*maxT_CH_midday, data = midday)
CH_Midday.ANOVA <- car::Anova(CH_Midday.agg, type = 3)
CH_Midday.ANOVA
CH_Midday.agg <- CH_Midday %>% group_by(ID, Max.Air.T..F., Julian.Date) %>% summarise(PSI=mean(PSI))
head(CH_Midday.agg)
CH_Midday.agg
CH_middady.ez <- ez::ezANOVA(data = CH_Midday.agg)
jd_agg <- CH_Midday.agg$Julian.Date
ID_agg <- CH_Midday.agg$ID
maxT_agg <- CH_Midday.agg$Max.Air.T..F.
PSI_agg <- CH_Midday.agg$PSI
with(CH_Midday.agg, interaction.plot(jd_agg, PSI_agg, ID_agg))
