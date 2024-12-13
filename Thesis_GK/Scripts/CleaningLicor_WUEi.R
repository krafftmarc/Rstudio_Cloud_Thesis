library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
licor_71922 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree LiCor Data 2022/Tyree_2022_/2022-07-19-survey.csv")
head(licor_71922)
licor_71922.meta <- licor_71922[1,]
licor_71922.meta
licor_71922.clean <- licor_71922[-c(1),]
names(licor_71922.clean)
licor_71922.clean$A <- as.numeric(licor_71922.clean$A)
licor_71922.clean$gsw <- as.numeric(licor_71922.clean$gsw)
WUEi <- licor_71922.clean$A/licor_71922.clean$gsw
licor_71922.clean$row <- as.character(licor_71922.clean$row)
licor_71922.clean$vine <- as.character(licor_71922.clean$vine)
Row <- licor_71922.clean$row
Vine <- licor_71922.clean$vine
Block <- licor_71922.clean$block
Species <- licor_71922.clean$species
df <- tibble(Block, Row, Vine,Species, WUEi)
df = df %>% unite(BRV, Block, Row, Vine, sep = "_", remove = FALSE)
df
# plotting ggplot(DFlng, aes(x = name, y = value, group = MY, color = MY)) + geom_line()
df_plot <-ggplot(df, aes(x = BRV, y = WUEi, group = Species, Block, color = Species)) + geom_line() + geom_smooth()
df_plot
df_plot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3))
