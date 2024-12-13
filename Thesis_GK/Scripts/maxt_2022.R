library(tidyverse)
cimis_2022_season <- read_csv("Data/Tyree_2022_ReadOnly/CIMIS_2022/cimis_2022_season.csv")
cimis_2022_season
Julian_Date <- cimis_2022_season$Jul
Julian_Date
Max_Temp <- cimis_2022_season$`Max Air Temp (F)`
maxt_Season_2022_base <- data.frame(Julian_Date, Max_Temp)
maxt_Season_2022_base
p_cimis_2022 <- ggplot(maxt_Season_2022_base, aes(x=091:273, y=Max_Temp)) + geom_point() + geom_smooth(method = lm)
p_cimis_2022
p_cimis_2022 + scale_x_continuous(name = "Julian Date", limits = c(91,273)) + scale_y_continuous(name = "Max Temp (F)", limits = c(50,120)) 
