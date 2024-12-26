# Load required libraries
library(tidyverse)      # Data manipulation and visualization
library(readxl)         # Read Excel files
library(lubridate)      # Date handling
library(nlme)           # Mixed effects models
library(car)            # Regression diagnostics
library(ggplot2)        # Plotting
library(viridis)        # Color palettes
library(gridExtra)      # Arranging plots
library(grid)           # Low-level graphics
library(corrplot)       # Correlation plots
library(gtable)         # For table manipulation

# Create output directories
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# Define all plotting functions first
create_chemistry_plot <- function(data, year) {
  ggplot(data, aes(x = Location_ID, y = Value, color = Treatment)) +
    geom_point(size = 3, alpha = 0.7, na.rm = TRUE) +
    facet_grid(Parameter ~ Date, scales = "free_y") +
    theme_bw() +
    labs(title = paste(year, "Chemistry by Location and Date"),
         x = "Block-Row-Vine",
         y = "Value") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      strip.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_color_viridis(discrete = TRUE) +
    guides(color = guide_legend(title = "Treatment"))
}

create_treatment_plot <- function(summary_data) {
  ggplot(summary_data, 
         aes(x = Treatment, y = mean, fill = Parameter)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(0.9), width = 0.25) +
    facet_wrap(~Year, scales = "free_y") +
    theme_bw() +
    labs(title = "Treatment Summary by Year",
         y = "Value (mean ± SD)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_temporal_plot <- function(data) {
  ggplot(data, 
         aes(x = Date, y = Value, color = Treatment)) +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~Parameter, scales = "free_y") +
    theme_bw() +
    labs(title = "Temporal Trends in Chemistry Parameters",
         y = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_distribution_plot <- function(data) {
  ggplot(data,
         aes(x = Treatment, y = Value, fill = as.factor(Year))) +
    geom_boxplot() +
    facet_wrap(~Parameter, scales = "free_y") +
    theme_bw() +
    labs(title = "Parameter Distributions by Treatment and Year",
         fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_correlation_plot <- function(data, year) {
  corr_matrix <- cor(data, use = "complete.obs")
  png(paste0("figures/correlations_", year, ".png"), width = 800, height = 800)
  corrplot(corr_matrix, method = "color", 
           addCoef.col = "black", 
           title = paste(year, "Parameter Correlations"),
           tl.col = "black")
  dev.off()
}

# Parse chemical ID function
parse_chem_id <- function(id) {
  # Handle empty/NA values
  if(is.na(id) || nchar(trimws(id)) == 0) {
    return(list(
      block = NA_character_,
      row_pair = NA_character_,
      vine_group = NA_real_,
      full_id = NA_character_
    ))
  }
  
  # Parse ID format like "AR23-1":
  # A = block
  # R23 = row pair
  # 1 = vine group
  block <- substr(id, 1, 1)        # Get "A" or "B"
  row_str <- substr(id, 3, 4)      # Get "23", "67", or "10"
  
  # Handle special case where "10" should be "1011"
  row_pair <- case_when(
    row_str == "10" ~ "1011",
    !is.na(row_str) ~ row_str,
    TRUE ~ NA_character_
  )
  
  # Get vine group number
  vine_group <- suppressWarnings({
    last_char <- substr(id, nchar(id), nchar(id))
    if(grepl("^\\d+$", last_char)) as.numeric(last_char) else NA_real_
  })
  
  return(list(block = block,
              row_pair = row_pair,
              vine_group = vine_group,
              full_id = id))
}

# Process 2022 data
chem_2022 <- TyreePrimary_Chemistry %>%
  mutate(
    # Clean up character columns
    across(where(is.character), trimws),
    
    # Standardize variety names
    Variety = case_when(
      toupper(Variety) == "CH" ~ "Chardonnay",
      toupper(Variety) == "CS" ~ "Cabernet Sauvignon",
      TRUE ~ Variety
    ),
    
    # Create proper date
    Date = make_date(Year, Month, Day),
    
    # Group rows into pairs
    Row_Pair = case_when(
      Row %in% c(2,3) ~ "23",
      Row %in% c(6,7) ~ "67",
      Row %in% c(10,11) ~ "1011",
      TRUE ~ NA_character_
    ),
    
    # Define treatments
    Treatment = case_when(
      is.na(Irrigation) | Irrigation %in% c("NaN", "NA", "") ~ "Pre-Treatment",
      TRUE ~ paste(Block, Irrigation)
    ),
    
    Year = 2022,
    TA = suppressWarnings(as.numeric(`TA g/L`))
  ) %>%
  filter(!is.na(Block))

# Clean 2022 Location IDs
chem_2022_clean <- chem_2022 %>%
  mutate(
    # Create Location_ID
    Location_ID = case_when(
      !is.na(Row_Pair) ~ str_trim(paste(Block, Row_Pair, Vine)),
      TRUE ~ str_trim(paste(Block, "NA", Vine))
    )
  ) %>%
  mutate(
    # Clean up Location_ID
    Location_ID = str_replace_all(Location_ID, "(NaN|NA)", ""),
    Location_ID = str_trim(Location_ID)
  ) %>%
  # Select final columns
  select(Date, Year, Treatment, Block, Row, Row_Pair, Vine, Location_ID,
         Irrigation, Variety, Brix, pH, TA, `Malic Acid mg/L`)

# Process 2023 data
chem_2023 <- Tyree_Primary_Chem %>%
  mutate(
    # Handle Brix values separately
    `Brix-Aug-10` = suppressWarnings(as.numeric(`Brix-Aug-10`)),
    `Brix-Aug-24` = suppressWarnings(as.numeric(`Brix-Aug-24`)),
    pH = suppressWarnings(as.numeric(pH)),
    TA = suppressWarnings(as.numeric(`Raw Value TA`)) * 2.5,
    
    # Parse location IDs
    parsed_id = map(ID, parse_chem_id),
    Block = map_chr(parsed_id, "block"),
    Row_Pair = map_chr(parsed_id, "row_pair"),
    Vine_Group = map_dbl(parsed_id, "vine_group"),
    
    Variety = "Cabernet Sauvignon",
    Location_ID = str_trim(paste(Block, Row_Pair, Vine_Group))
  ) %>%
  filter(!is.na(Block))

# Clean 2023 data and create date-specific rows
chem_2023_clean <- chem_2023 %>%
  bind_rows(
    mutate(., Date = as.Date("2023-08-10"), Brix = `Brix-Aug-10`),
    mutate(., Date = as.Date("2023-08-24"), Brix = `Brix-Aug-24`)
  ) %>%
  filter(!is.na(Brix)) %>%
  mutate(
    Year = 2023,
    Treatment = paste(Block, "2L")
  ) %>%
  select(Date, Year, Treatment, Block, Row_Pair, Vine_Group, Location_ID,
         Variety, Brix, pH, TA)

# Convert data to long format for plotting
chem_2022_long <- chem_2022_clean %>%
  pivot_longer(
    cols = c(Brix, pH, TA),
    names_to = "Parameter",
    values_to = "Value"
  )

chem_2023_long <- chem_2023_clean %>%
  pivot_longer(
    cols = c(Brix, pH, TA),
    names_to = "Parameter",
    values_to = "Value"
  )

# Create location summary first
location_summary <- bind_rows(
  chem_2022_long %>%
    group_by(Year, Location_ID, Parameter, Treatment) %>%
    summarise(
      n = n(),
      mean = mean(Value, na.rm = TRUE),
      sd = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ),
  chem_2023_long %>%
    group_by(Year, Location_ID, Parameter, Treatment) %>%
    summarise(
      n = n(),
      mean = mean(Value, na.rm = TRUE),
      sd = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
)

# Create treatment summary
treatment_summary <- bind_rows(
  chem_2022_long %>%
    group_by(Treatment, Parameter) %>%
    summarise(
      n = n(),
      mean = round(mean(Value, na.rm = TRUE), 2),
      sd = round(sd(Value, na.rm = TRUE), 2),
      min = round(min(Value, na.rm = TRUE), 2),
      max = round(max(Value, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    mutate(Year = 2022),
  chem_2023_long %>%
    group_by(Treatment, Parameter) %>%
    summarise(
      n = n(),
      mean = round(mean(Value, na.rm = TRUE), 2),
      sd = round(sd(Value, na.rm = TRUE), 2),
      min = round(min(Value, na.rm = TRUE), 2),
      max = round(max(Value, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    mutate(Year = 2023)
)

# Create and save all plots
plot_2022 <- create_chemistry_plot(chem_2022_long, "2022")
plot_2023 <- create_chemistry_plot(chem_2023_long, "2023")
treatment_plot <- create_treatment_plot(treatment_summary)
temporal_plot <- create_temporal_plot(bind_rows(chem_2022_long, chem_2023_long))
param_dist_plot <- create_distribution_plot(bind_rows(chem_2022_long, chem_2023_long))

# Save all plots
ggsave("figures/chemistry_2022.png", plot_2022, width = 15, height = 10, dpi = 300)
ggsave("figures/chemistry_2023.png", plot_2023, width = 15, height = 10, dpi = 300)
ggsave("figures/treatment_summary.png", treatment_plot, width = 12, height = 8, dpi = 300)
ggsave("figures/temporal_trends.png", temporal_plot, width = 12, height = 8, dpi = 300)
ggsave("figures/parameter_distributions.png", param_dist_plot, width = 12, height = 8, dpi = 300)

# Generate correlation plots
create_correlation_plot(
  chem_2022_clean %>% select(Brix, pH, TA, `Malic Acid mg/L`), 
  2022
)
create_correlation_plot(
  chem_2023_clean %>% select(Brix, pH, TA), 
  2023
)

# Save summary data
write_csv(location_summary, "tables/chemistry_summary.csv")
write_csv(chem_2022_clean, "tables/chemistry_2022_clean.csv")
write_csv(chem_2023_clean, "tables/chemistry_2023_clean.csv")
write_csv(treatment_summary, "tables/treatment_summary.csv")

# Print summary of generated files
cat("\nGenerated files:\n")
cat("\nFigures:\n")
list.files("figures", pattern = ".png$")
cat("\nTables:\n")
list.files("tables", pattern = ".csv$")
warnings()
