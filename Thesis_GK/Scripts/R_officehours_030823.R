library(tidyverse)
rh_b2r2 <- read_csv("Data/Tyree_2022_ReadOnly/Tyree_Sensor_Data_2022/Tyree_Decoded_Output/sensor_data_b2r2.csv")
ts <- rh_b2r2$TOA5
rh <- rh_b2r2$...14
rh1 <- rh_b2r2$...16
ts_rh_base_2022 <- data.frame(ts, rh)
ggplot(ts_rh_base_2022, aes(x=ts, y=rh)) + geom_point() + geom_smooth(method = lm)
ts_rh_base_2022
head(ts_rh_base_2022)
#library(dplyr)
#library(lubridate)
#dat2 <- dat %>% mutate(Date = dmy(Date)) %>% mutate(WeekDay = wday(Date)) %>% group_by(WeekDay) %>% summarize(AvgTemp = mean(AvgTemp)) %>% ungroup()
library(dplyr)
library(lubridate)
#df$Timestamp <- as.POSIXlt(df$Timestamp, format = "%m/%d/%Y %H:%M")
#aggregate(list(avgdata = df$data), list(hourofday = cut(df$Timestamp, "2 hour"), category = df$category), FUN = function(x) c(data_avg = mean(x), data_sd = length(x)))
ts_timestamp <- as.POSIXlt(ts, format = "%m/%d/%Y %H:%M")
ts_timestamp
ts_rh_avg <- aggregate(list(avgdata = rh), list(hourofday = cut(ts_timestamp, "6 hour"), category = rh), FUN = function(x) c(data_avg = mean(x), data_sd = length(x)))
warnings()
ts_rh_avg
ggplot(ts_rh_avg, aes(x=hourofday, y=category))
df_ts_rh <- data.frame(ts_timestamp, rh)
df_ts_rh
library(tidyverse)
#predawn_base <- clean %>% filter(jd <224) %>% filter(Time < 1300) %>% filter(!is.na(PSI)) 
df_ts_rh_clean <- df_ts_rh %>% filter(!is.na(ts_timestamp))
df_ts_rh_clean
#df %>% mutate(Timestamp = mdy_hm(Timestamp),                   # update to a datetime variable (if needed)
#TimeDiff = difftime(Timestamp, min(Timestamp), units = "hours"),  # get the distance from the first timestamp of the dataset (in hours)
       TimeGroup = as.numeric(TimeDiff) %/% 2) %>%      # create a grouping variable based on the distance
  group_by(TimeGroup, Category) %>%                       # for each group and category
  summarise(Category_MinTime = min(Timestamp),            # get the first time stamp for this category in this group
            data_avg = mean(data),                        # get average
            data_sd = sd(data),                           # get sd
            NumObs = n()) %>%                             # get number of observations (might be useful)
  mutate(TimeGroup_MinTime = min(Category_MinTime)) %>%   # get first time stamp of that time group
  ungroup() %>%                                           # forget the grouping
  select(TimeGroup, TimeGroup_MinTime, everything())      # re arrange columns
df %>% mutate(Timestamp = mdy_hm(Timestamp), TimeDiff = difftime(Timestamp, min(Timestamp), units = "hours"), TimeGroup = as.numeric(TimeDiff) %/% 2) %>% group_by(TimeGroup, Category) %>% summarise(Category_MinTime = min(Timestamp),  data_avg = mean(data), data_sd = sd(data),  NumObs = n()) %>%  mutate(TimeGroup_MinTime = min(Category_MinTime)) %>% ungroup() %>%  select(TimeGroup, TimeGroup_MinTime, everything()) 
head(clean)
head(rh_b2r2)
prime_chem <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Data/Tyree_2022_ReadOnly/Tyree_2022_PrimaryChem/TyreePrimaryChem_Cleaned.csv")
head(prime_chem)
ts_timestamp
library(lubridate)
## lubridate transform date
head(rh_b2r2$`CPU:Base-code.CR1X`)
# Extract Rows from data
metadata <- rh_b2r2[1:3,]
rh_b2r2_cleanish <- rh_b2r2[-c(1:3),]
colnames(rh_b2r2_cleanish) <- metadata[1,]
rh_b2r2_cleanish$TIMESTAMP1 <- mdy_hm(rh_b2r2_cleanish$TIMESTAMP)
## lubridate change to Jul Date
jd_rh_b2r2 <- yday(rh_b2r2_cleanish$TIMESTAMP1)

#grouping large datasets by every N rows
rh_b2r2_cleanish.gr <- rh_b2r2_cleanish %>% group_by(grp = rep(row_number(), length.out = n(), each = 500))
rh_b2r2_cleanish.sum <- rh_b2r2_cleanish.gr %>% summarise_all(list(min, max))
jd_rh_b2r2.sum <- yday(rh_b2r2_cleanish.sum$TIMESTAMP1_fn1)


# concatenate all samples into unique data frame 
?group_by
finaldf = pd.concat(samples)
# assign every one hundred measurement to a group the group_by(group)
# df$group = c(rep(“A”, 100), rep(B, “100”)…)
groups = ["A", "B", "C" ...]
sample_sizes = [100]


