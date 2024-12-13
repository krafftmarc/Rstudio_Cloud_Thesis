library(tidyverse)
licor <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree LiCor Data 2022/tyree_9622_hw.csv")
licor1PM <- licor %>% filter(Time_id == "1PM")
licor4PM <- licor %>% filter(Time_id == "4PM")
licor_day <- merge(licor1PM, licor4PM, by = "Time_id")
?merge
#heatwave2.meta <- heatwave1[1,]
#heatwave1.meta
licor.clean <- licor[-c(1),]
licor.clean
#heatwave2.clean$date <- ymd_hms(heatwave2.clean$date)
#colnames(heatwave1.clean) <- heatwave1.meta[1,]
#heatwave1.clean$date <- ymd_hms(heatwave1.clean$date)
#jd_clean <- yday(licor.clean$date)
#jd_clean
#licor.clean <- cbind(licor.clean, jd_clean)
#licor.clean$jd_clean
#licor.clean <- heatwave2.clean %>% filter(heatwave2.clean$jd_clean == 249)
E <- as.numeric(licor$E) 
A <- as.numeric(licor$A)
wue <- A/E 
stress <- licor$Stress
# colnames(my_dataframe)[2] ="c2"
#colnames(heatwave2.clean)[1] = "Time_id"
#licor.clean$gsw <- as.numeric(licor.clean$gsw)
licor$row <- as.character(licor$row)
licor$vine <- as.character(licor$vine)
Row <- licor$row
Vine <- licor$vine
Block <- licor$block
Tx <- licor$Tx
Time_id <- licor$Time_id
df <- tibble(Block, Row, Vine, Time_id, Tx, stress, wue)
df = df %>% unite(BRV, Block, Row, Vine, sep = "_", remove = FALSE)
df
library(car)
library(lme4)
?lmer

df.lmer <- lmer(wue ~ Time_id + (1 | Tx), data = df)
summary(df.lmer)
help('isSingular')
df1PM.lm <- lm(Tx1PM~df1PM, data = df1PM)
psi_tx.lm                  
df.car <- car::Anova(df.lmer, type = 3)
summary(df.car)
df.car

write_csv(df, "Tyree_9622_TxXStress.csv")
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
df_plot <-ggplot(df1PM, aes(x = BRV, y = wue1PM, group = Tx1PM, color = stress1PM)) + geom_line() + geom_smooth()
df_plot
df_plot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))
3write_csv(df_comb,"gs_psiStem_830")
?write_csv
library(car)
library(lme4)
?lmer

df1PM.lmer <- lmer(wue1PM ~ Tx1PM + (1 | Tx1PM), data = df1PM)
summary(df1PM.lmer)
help('isSingular')
df1PM.lm <- lm(Tx1PM~df1PM, data = df1PM)
psi_tx.lm                  
df1PM.car <- car::Anova(df1PM.lmer, type = 3)
summary(df1PM.car)
df1PM.car
