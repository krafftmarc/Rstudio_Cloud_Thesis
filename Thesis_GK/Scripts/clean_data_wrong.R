clean <- read_csv("Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_2022_Cleaned.csv")
clean
as.factor(clean$Block_ID)
as.factor(clean$Row_ID)
as.factor(clean$Vine_ID)
head(predawn)
jd_pd <- predawn$`Julian Date`
psi_pd <- predawn$PSI
df_pd <- data.frame(jd_pd, psi_pd)
df_pd
plot(df_pd)
ggplot(df_pd, aes(x=jd_pd, y=psi_pd)) + geom_point() + geom_smooth(method = lm)
df_predawn
predawn
predawnplot <- plot(predawn$`Julian Date`,predawn$PSI)
jd_mds <- midday_stem$`Julian Date`
psi_mds <- midday_stem$PSI
df_mds <- data.frame(jd_mds, psi_mds)
df_mds
ggplot(df_mds, aes(x=jd_mds, y=psi_mds)) + geom_point() + geom_smooth(method = lm)
??trendline
ggplot(data = predawn, aes(x=clean$`Julian Date`,y=clean$PSI)) 
x1 <- as.factor(clean$Block_ID)
x2 <- as.factor(clean$Row_ID)
x3 <- as.factor(clean$Vine_ID)
df <- data.frame(x1, x2, x3)
df
df_ID <- tibble::rowid_to_column(df,"ID")
df_ID
as.table(df_ID)
clean_with_ID <- merge(df_ID,clean)
clean_with_ID
## wrong format 
## surveys %>% filter(weight > 30 & weight < 60)
predawn <- clean %>% filter(Time < 1300)
predawn
clean$pot_type


midday_leaf <- clean %>% filter(Time > 1000 & pot_type == "Leaf")
midday_leaf
midday_stem <- clean %>% filter(Time > 1000 & pot_type == "Stem")
midday_stem
## surveys %>% filter(is.na(weight)) %>% 
 ## group_by(plot_id) %>%
vines_ID_pd <- predawn %>% group_by(Block_ID, Row_ID, Vine_ID)
vines_ID_pd
## ggplot(data = <DATA>), mapping = aes(<MAPPINGS>) +
## <GEOM_FUNCTION>()
ggplot(data = vines_ID_pd, aes(group_by("Block_ID", "Row_ID", "Vine_ID"), "PSI"))
