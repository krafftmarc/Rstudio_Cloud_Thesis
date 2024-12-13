library(tidyverse)
psi <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_PSI_2022_Cleaned_W_ETo_T.csv")
psi$`Julian Date` <- as.character.Date(psi$`Julian Date`)
psi_tx <- psi %>% filter(psi$Time == 500 & Variety == "CS" & pot_type == "Stem") %>% filter(!is.na(Tx)) 
psi_tx$Tx <- as.character(psi_tx$Tx) 
Block <- psi_tx$Block_ID
Row <- psi_tx$Row_ID
Vine <- psi_tx$Vine_ID
psi.tx <- psi_tx$PSI
JD <- psi_tx$`Julian Date`
Tx <- psi_tx$Tx
df <- tibble(Block, Row, Vine, JD, Tx, psi.tx)
df = df %>% unite(BRV, Block, Row, Vine, sep = "_", remove = FALSE)
df
df <- df %>% group_by(JD)
df$JD
df_plot <-ggplot(df, aes(x = JD, y = psi.tx, group = Tx, color = Tx)) + geom_bin_2d() + geom_smooth(method = lm)
df_plot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))
write_csv(df,"~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/tyree_psi_tx_stem_pd.csv")
library(car)
library(lme4)
?lmer
psi_tx.lmer <- lmer(psi.tx ~ Tx + (1 | JD), data = df)
summary(psi_tx.lmer)

psi_tx.lm <- lm(Tx~psi.tx, data = df)
psi_tx.lm                  
psi_tx.car <- car::Anova(psi_tx.lmer, type = 3)
summary(psi_tx.car)
psi_tx.car
