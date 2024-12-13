library(tidyverse)
irrigation_2022 <- read_csv("Data/Tyree_2022_ReadOnly/Irrigation/tyree_irrigation_2022.csv")
vol_vine <- irrigation_2022$gal_vine
maxt_irr_2022_base <- data.frame(maxt_irr_cimis, vol_vine)
maxt_irr_2022_base
ggplot(maxt_irr_2022_base, aes(x=maxt_irr_cimis, y=vol_vine)) + geom_point() + geom_smooth(method = lm)
vol_block <- irrigation_2022$gal_block
jd_irr <- irrigation_2022$jul
jd_irr
irr_maxt clean %>% filter(jd <224) %>% filter(Time == 500) %>% filter(!is.na(PSI))
jd_vine_2022_base <- data.frame(jd_irr, vol_vine)
jd_block_2022_base <- data.frame(jd_irr, vol_block)
ggplot(jd_vine_2022_base, aes(x=jd_irr, y=vol_vine)) + geom_point() + geom_smooth(method = lm)
ggplot(jd_block_2022_base, aes(x=jd_irr, y=vol_block)) + geom_point() + geom_smooth(method = lm)
