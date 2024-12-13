library(tidyverse)
heatwave2 <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree LiCor Data 2022/NSCs_2nd-heatwave-September-6-2022-compiled.xlsx")
heatwave2
#heatwave2.meta <- heatwave1[1,]
#heatwave1.meta
heatwave2.clean <- heatwave2[-c(1),]
heatwave2.clean
heatwave2.clean$date <- ymd_hms(heatwave2.clean$date)
#colnames(heatwave1.clean) <- heatwave1.meta[1,]
#heatwave1.clean$date <- ymd_hms(heatwave1.clean$date)
jd_clean <- yday(heatwave2.clean$date)
jd_clean
heatwave2.clean <- cbind(heatwave2.clean, jd_clean)
heatwave2.clean$jd_clean
heatwave2.clean <- heatwave2.clean %>% filter(heatwave2.clean$jd_clean == 249)
gs <- heatwave2.clean$gsw
# colnames(my_dataframe)[2] ="c2"
colnames(heatwave2.clean)[1] = "Time_id"
heatwave2.clean$gsw <- as.numeric(heatwave2.clean$gsw)
heatwave2.clean$row <- as.character(heatwave2.clean$row)
heatwave2.clean$vine <- as.character(heatwave2.clean$vine)
Row <- heatwave2.clean$row
Vine <- heatwave2.clean$vine
Block <- heatwave2.clean$Block
Time_id <- heatwave2.clean$Time_id
df <- tibble(Block, Row, Vine,Time_id, gs)
df = df %>% unite(BRV, Block, Row, Vine, sep = "_", remove = FALSE)
df
psi <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree Water Potentials 2022/Tyree_2022_Cleaned.csv")
psi_249 <- psi %>% filter(psi$Julian.Date == 249) %>% filter(pot_type == "Stem") %>% filter(!is.na(PSI))
psi_249
Block_psi <- psi_249$Block_ID
Row_psi <- psi_249$Row_ID
Vine_psi <- psi_249$Vine_ID
Time_psi <- psi_249$Time_id
psi_stem <- psi_249$PSI
df_psi <- tibble(Block_psi, Row_psi, Vine_psi, Time_psi, psi_stem)
df_psi = df_psi %>% unite(BRV, Block_psi, Row_psi, Vine_psi, sep = "_", remove = FALSE)
df_psi
df
