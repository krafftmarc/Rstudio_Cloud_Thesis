library(tidyverse)
licor <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree LiCor Data 2022/2022-08-30-1319_clean.csv")
licor
#heatwave2.meta <- heatwave1[1,]
#heatwave1.meta
licor.clean <- licor[-c(1),]
licor.clean
#heatwave2.clean$date <- ymd_hms(heatwave2.clean$date)
#colnames(heatwave1.clean) <- heatwave1.meta[1,]
#heatwave1.clean$date <- ymd_hms(heatwave1.clean$date)
jd_clean <- yday(licor.clean$date)
jd_clean
licor.clean <- cbind(licor.clean, jd_clean)
licor.clean$jd_clean
#licor.clean <- heatwave2.clean %>% filter(heatwave2.clean$jd_clean == 249)
gs <- licor.clean$gsw
# colnames(my_dataframe)[2] ="c2"
#colnames(heatwave2.clean)[1] = "Time_id"
licor.clean$gsw <- as.numeric(licor.clean$gsw)
licor.clean$row <- as.character(licor.clean$row)
licor.clean$vine <- as.character(licor.clean$vine)
Row <- licor.clean$row
Vine <- licor.clean$vine
Block <- licor.clean$block
#Time_id <- heatwave2.clean$Time_id
df <- tibble(Block, Row, Vine, gs)
df = df %>% unite(BRV, Block, Row, Vine, sep = "_", remove = FALSE)
df
psi <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_2022_Cleaned.csv")
psi_242 <- psi %>% filter(psi$Julian.Date == 242) %>% filter(pot_type == "Stem") %>% filter(!is.na(PSI)) %>% filter(Time == 1300)
psi_242
write_csv(psi_242, "psi_242")
Block_psi <- psi_242$Block_ID
Row_psi <- psi_242$Row_ID
Vine_psi <- psi_242$Vine_ID
Time_psi <- psi_242$Time_id
psi_stem <- psi_242$PSI
df_psi <- tibble(Block_psi, Row_psi, Vine_psi, psi_stem)
df_psi = df_psi %>% unite(BRV, Block_psi, Row_psi, Vine_psi, sep = "_", remove = FALSE) %>% mut(df$BRV)
df_psi
df
#merge(matA,matB,by="col1",all=TRUE)

df_comb <- merge(df, df_psi, by="BRV", all = TRUE)
#df_comb <- df_comb %>% filter(!is.na(df_comb))
df_comb
df_plot <-ggplot(df_comb, aes(x = BRV, y = psi_stem, group = Species, Block, color = Species)) + geom_line() + geom_smooth()
df_plot
df_plot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))
write_csv(df_comb,"gs_psiStem_830")
?write_csv
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