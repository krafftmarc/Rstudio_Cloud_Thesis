library(tidyverse)
clean <- read_csv("Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_2022_Cleaned.csv")
## surveys_avg_weight <- surveys %>% 
## filter(!is.na(weight)) %>% 
 ## group_by(species_id, sex) %>% 
##  mutate(avg_weight = mean(weight)) %>% 
##  select(species_id, sex, weight, avg_weight)
predawn <- clean %>% filter(Time < 1300) %>% filter(!is.na(PSI)) 
predawn
jd <- clean$`Julian Date`
midday_leaf_base <- clean %>% filter(Time > 1000 & pot_type == "Leaf") %>% filter(!is.na(PSI)) 
midday_leaf_base
var_mlb <- midday_leaf_base$Variety
id_mlb <- as.factor(midday_leaf_base$ID)
jd_mlb <- as.factor(midday_leaf_base$`Julian Date`)
psi_mlb <- midday_leaf_base$PSI
mlb_df <- data.frame(id_mlb,var_mlb,jd_mlb,psi_mlb)
mlb_df
## aggregate(df$points, by=list(df$team, df$position), FUN=mean)
mlb_df_agg <- aggregate(psi_mlb, by=list(id_mlb,jd_mlb,var_mlb), FUN=mean)
plot(mlb_df_agg)
mlb_aov <- aov(psi_mlb~id_mlb+jd_mlb+var_mlb, data = mlb_df)
mlb_aov
mlb_aov.tukey <- TukeyHSD(mlb_aov)
plot(mlb_aov.tukey)
mlb_model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)
install.packages("multcomp")
install.packages("emmeans")
install.packages("multcompView")
library(emmeans)
library(multcomp)
library(multcompView)

mlb_model <- lm(psi_mlb ~ id_mlb, data = mlb_df)
mlb_model
mlb_model_means <- emmeans(object = mlb_model, specs = var_mlb)



mlb_tukey.cld <- cld(mlb_aov.tukey)

midday_stem <- clean %>% filter(Time > 1000 & pot_type == "Stem") %>% filter(!is.na(PSI))
midday_stem
ID_pd <- predawn$ID
PSI_pd <- predawn$PSI
df_pd <- data.frame(ID_pd, PSI_pd)
df_pd
ggplot(df_pd, aes(x=ID_pd, y=PSI_pd)) + geom_point() + geom_smooth(method = lm)
## Baseline separation
jd_pd <- predawn$`Julian Date`
jd_pd
predawn_base <- clean %>% filter(Time < 1300) %>% filter(!is.na(PSI)) %>% filter(jd_pd < 224)
ID_pd_base <- predawn_base$ID
PSI_pd_base <- predawn_base$PSI
df_pd_base <- data.frame(ID_pd_base, PSI_pd_base)
df_pd_base
ggplot(df_pd_base, aes(x=ID_pd_base, y=PSI_pd_base)) + geom_point() + geom_smooth(method = lm)
jd_mdl_base <- midday_leaf$`Julian Date` %>% filter(!is.na(PSI))
jd_mdl_base

midday_leaf <- clean %>% filter(Time > 1000 & pot_type == "Leaf") %>% filter(!is.na(PSI)) %>% filter(jd_mdl_base < 224)
ID_mdl_base <- midday_leaf$ID
PSI_mdl_base <- midday_leaf$PSI
df_mdl_base <- data.frame(jd_mdl_base, PSI_mdl_base)
ggplot(df_mdl_base, aes(x=jd_mdl_base, y=PSI_mdl_base)) + geom_point() + geom_smooth(method = lm)
