library(tidyverse)
clean <- read_csv("Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_PSI_2022_Cleaned.csv")
## surveys_avg_weight <- surveys %>% 
## filter(!is.na(weight)) %>% 
## group_by(species_id, sex) %>% 
##  mutate(avg_weight = mean(weight)) %>% 
##  select(species_id, sex, weight, avg_weight)
## mds_p + labs(title = "Midday Baseline Stem Water Potential", x = "Julian Days", y = "Stem Water Potential (Pascals)")
## pd_p + labs(y = "Stem Water Potential (Pascals)" , x = "Julian Days")
jd <- clean$`Julian Date`
predawn_base <- clean %>% filter(jd <224) %>% filter(Time == 500) %>% filter(!is.na(PSI)) 
predawn_base 
jd_pd_base <- predawn_base$`Julian Date`
psi_pd_base <- predawn_base$PSI
df_pd_base <- data.frame(jd_pd_base, psi_pd_base)
pd_p <- ggplot(df_pd_base, aes(x=jd_pd_base, y=psi_pd_base)) + geom_point() + geom_smooth(method = lm)
pd_p + labs(y = "Stem Water Potential (Pascals)" , x = "Julian Days")
midday_leaf_base <- clean %>% filter(jd < 224) %>% filter(Time == 1300 & pot_type == "Leaf") %>% filter(!is.na(PSI))
midday_stem_base <- clean %>% filter(jd < 224) %>% filter(Time == 1300 & pot_type == "Stem") %>% filter(!is.na(PSI))
jd_mdl_base <-midday_leaf_base$`Julian Date`
psi_mdl_base <- midday_leaf_base$PSI
df_mdl_base <- data.frame(jd_mdl_base, psi_mdl_base)
jd_mds_base <- midday_stem_base$`Julian Date`
psi_mds_base <- midday_stem_base$PSI
df_mds_base <- data.frame(jd_mds_base, psi_mds_base)
df_mdl_base <- data.frame(jd_mdl_base, psi_mdl_base)
mdl_p <- ggplot(df_mdl_base, aes(x=jd_mdl_base, y=psi_mdl_base)) + geom_point() + geom_smooth(method = lm)
mdl_p + labs(y = "Leaf Water Potential (Pascals)", x = "Julian Days")
mds_p <- ggplot(df_mds_base, aes(x=jd_mds_base, y=psi_mds_base)) + geom_point() + geom_smooth(method = lm)
mds_p + labs(y = "Stem Water Potential (Pascals)", x = "Julian Days")
