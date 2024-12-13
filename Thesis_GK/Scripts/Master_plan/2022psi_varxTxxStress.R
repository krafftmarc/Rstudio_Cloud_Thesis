# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)  # For regression summaries
library(readxl)  # For read_xlsx
library(tidyr)  # For unnesting

# Load the data
data <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/R/R/Version Control/Heat_Water_Acclimation_Masters_Thesis/Figures/MasterPlan/Tyree_PSI_2022_Cleaned.xlsx")

# Data transformation
data <- data %>%
  mutate(Date = make_date(Year, Month, Day),
         PSI = as.numeric(PSI),
         Tx = as.character(Tx),
         Unique_ID = paste(Block_ID, Row_ID, Vine, sep = "_")) %>%
  filter(!is.na(Date), !is.na(PSI))

# Filter data for specific Time, e.g., 1300
data_1300 <- data %>%
  filter(Time == 1300)

# Perform linear regression by Treatment (Tx), Stress groups, Variety, and the new Unique_ID
model_summaries_1300 <- data_1300 %>%
  group_by(Tx, Stress, Variety, Unique_ID) %>%
  summarize(
    model = list(lm(PSI ~ Date, data = .)),
    .groups = 'drop'
  ) %>%
  mutate(
    glance_data = map(model, broom::glance),
    tidy_data = map(model, broom::tidy)
  ) %>%
  select(-model) %>%
  unnest(c(glance_data, tidy_data), names_sep = "_") %>%
  distinct(Tx, Stress, Variety, Unique_ID, .keep_all = TRUE)

# Merge the model summaries back with the filtered data
data_1300 <- left_join(data_1300, model_summaries_1300, by = c("Tx", "Stress", "Variety", "Unique_ID"))

# Prepare the plot
p <- ggplot(data_1300, aes(x = Date, y = PSI, color = Stress)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(Tx ~ Variety, scales = "free_y", space = "free") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 8)
  ) +
  labs(title = "PSI by Date across Treatments, Varieties, and Stress Conditions",
       x = "Date",
       y = "PSI",
       caption = "Treatments and stress conditions are overlaid within each variety panel with R² and p-values.")

# Print the plot
print(p)
