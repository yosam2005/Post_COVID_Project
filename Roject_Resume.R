
# ================================
# Post-COVID Project: WBD + IMF Panel Dataset
# ================================

# -------------------------------
# Section 1: Setup
# -------------------------------
folder <- "C:/Post_COVID_project/WBD_IMF.data/"

# Load libraries
library(dplyr)
library(tidyr)
library(plm)
library(car)
library(countrycode)
library(ggplot2)

# -------------------------------
# Section 2: Load WBD CSVs
# -------------------------------
wbd_files <- c("API_FP.CPI.csv", "API_NE.EXP.csv", "API_NY.GDP.csv", "API_SH.XPD.csv", "API_SL.UEM.csv")

wbd_data <- lapply(wbd_files, function(file) {
  read.csv(paste0(folder, file), skip = 4)
})
names(wbd_data) <- gsub(".csv", "", wbd_files)

API_FP.CPI   <- wbd_data[["API_FP.CPI"]]
API_NE.EXP   <- wbd_data[["API_NE.EXP"]]
API_NY.GDP   <- wbd_data[["API_NY.GDP"]]
API_SH.XPD   <- wbd_data[["API_SH.XPD"]]
API_SL.UEM   <- wbd_data[["API_SL.UEM"]]

# -------------------------------
# Section 3: Clean Year Columns
# -------------------------------
rename_years <- function(df) {
  names(df)[5:ncol(df)] <- gsub("^X", "", names(df)[5:ncol(df)])
  return(df)
}
wbd_data <- lapply(wbd_data, rename_years)

API_FP.CPI   <- wbd_data[["API_FP.CPI"]]
API_NE.EXP   <- wbd_data[["API_NE.EXP"]]
API_NY.GDP   <- wbd_data[["API_NY.GDP"]]
API_SH.XPD   <- wbd_data[["API_SH.XPD"]]
API_SL.UEM   <- wbd_data[["API_SL.UEM"]]

# -------------------------------
# Section 4: Load and Clean IMF Data
# -------------------------------
IMF_DATA <- read.csv(paste0(folder, "IMF_DATA.2025.csv"), header = TRUE)
year_cols <- grep("^X[0-9]{4}$", names(IMF_DATA), value = TRUE)
IMF_clean <- IMF_DATA[, c("COUNTRY", "INDICATOR", "FREQUENCY", "SCALE", year_cols)]
names(IMF_clean)[5:ncol(IMF_clean)] <- gsub("^X", "", names(IMF_clean)[5:ncol(IMF_clean)])

rm(IMF_DATA, wbd_files)  # clean environment

# -------------------------------
# Section 5: Reshape Datasets to Long Format (Robust)
# -------------------------------
reshape_long <- function(df, value_name){
  # Select columns with 4-digit years, optionally starting with X
  year_cols <- grep("^(X)?\\d{4}$", names(df), value = TRUE)
  
  df_long <- df %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "Year",
      values_to = value_name
    ) %>%
    mutate(Year = as.numeric(gsub("X", "", Year)))  # remove X if present
  return(df_long)
}

CPI_long     <- reshape_long(API_FP.CPI, "CPI")
Exports_long <- reshape_long(API_NE.EXP, "Exports")
GDP_long     <- reshape_long(API_NY.GDP, "GDP")
GovExp_long  <- reshape_long(API_SH.XPD, "GovExp")
Unemp_long   <- reshape_long(API_SL.UEM, "Unemployment")
IMF_long     <- reshape_long(IMF_clean, "IMF_Value")

# -------------------------------
# Section 6: Merge WBD Datasets into Master Data
# -------------------------------
master_data <- CPI_long %>%
  select(Country.Name, Country.Code, Year, CPI) %>%
  left_join(Exports_long %>% select(Country.Code, Year, Exports), by = c("Country.Code","Year")) %>%
  left_join(GDP_long %>% select(Country.Code, Year, GDP), by = c("Country.Code","Year")) %>%
  left_join(GovExp_long %>% select(Country.Code, Year, GovExp), by = c("Country.Code","Year")) %>%
  left_join(Unemp_long %>% select(Country.Code, Year, Unemployment), by = c("Country.Code","Year"))

# -------------------------------
# Section 7: Merge IMF Data
# -------------------------------
master_data <- master_data %>%
  left_join(IMF_long %>% select(COUNTRY, Year, IMF_Value),
            by = c("Country.Name" = "COUNTRY", "Year"))

# -------------------------------
# Section 8: Filter Years & Check Data
# -------------------------------
master_data <- master_data %>% filter(Year >= 2010 & Year <= 2024)
sapply(master_data, function(x) sum(is.na(x)))  # check missing values

# -------------------------------
# Section 9: Fill Missing Values
# -------------------------------
vars_to_fill <- c("CPI","Exports","GDP","GovExp","Unemployment","IMF_Value")
master_data_clean <- master_data %>%
  group_by(Country.Code) %>%
  arrange(Year) %>%
  fill(all_of(vars_to_fill), .direction = "downup") %>%
  ungroup()

# -------------------------------
# Section 10: Final Panel Dataset
# -------------------------------
master_data_unique <- master_data_clean %>%
  group_by(Country.Code, Year) %>%
  summarise(
    CPI = mean(CPI, na.rm = TRUE),
    Exports = mean(Exports, na.rm = TRUE),
    GDP = mean(GDP, na.rm = TRUE),
    GovExp = mean(GovExp, na.rm = TRUE),
    Unemployment = mean(Unemployment, na.rm = TRUE),
    IMF_Value = mean(IMF_Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    GDP_log = log(GDP + 100),
    Exports_log = log(Exports + 1),
    GovExp_log = log(GovExp + 1)
  ) %>%
  drop_na()

write.csv(master_data_unique, "master_data_panel_clean.csv", row.names = FALSE)

# -------------------------------
# Section 11: Quick Checks & Key Variable Plots
# -------------------------------

library(ggplot2)
library(dplyr)
library(countrycode)

# 1. Add country names, handle WLD
master_data_panel_named <- master_data_unique %>%
  mutate(Country.Name = countrycode(Country.Code, origin = "iso3c", destination = "country.name")) %>%
  mutate(Country.Name = ifelse(Country.Code == "WLD", "World Aggregate", Country.Name))

# 2. Quick checks
# Number of unique countries
num_countries <- master_data_panel_named %>% summarise(n_countries = n_distinct(Country.Code))
print(num_countries)

# Years available per country
years_per_country <- master_data_panel_named %>%
  group_by(Country.Name, Country.Code) %>%
  summarise(years_available = n_distinct(Year), .groups = "drop") %>%
  arrange(desc(years_available))
print(years_per_country, n = nrow(years_per_country))

# 3. Function to plot variables
plot_variable <- function(data, variable, y_label, top_n = 10) {
  
  # Select top countries by average GDP
  top_countries <- data %>%
    group_by(Country.Name) %>%
    summarise(avg_GDP = mean(GDP, na.rm = TRUE)) %>%
    arrange(desc(avg_GDP)) %>%
    slice(1:top_n) %>%
    pull(Country.Name)
  
  plot_data <- data %>% filter(Country.Name %in% top_countries)
  
  ggplot(plot_data, aes_string(x = "Year", y = variable, color = "Country.Name")) +
    geom_line(size = 1, alpha = 0.8) +
    labs(
      title = paste0(y_label, " Trends by Country, 2010–2024"),
      subtitle = "Top 10 countries by average GDP",
      x = "Year",
      y = y_label,
      color = "Country"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic")
    )
}

# 4. Plot all key variables
cpi_plot <- plot_variable(master_data_panel_named, "CPI", "Consumer Price Index (CPI)")
gdp_plot <- plot_variable(master_data_panel_named, "GDP", "Gross Domestic Product (GDP)")
exports_plot <- plot_variable(master_data_panel_named, "Exports", "Exports")
govexp_plot <- plot_variable(master_data_panel_named, "GovExp", "Government Expenditure")
unemp_plot <- plot_variable(master_data_panel_named, "Unemployment", "Unemployment Rate")
imf_plot <- plot_variable(master_data_panel_named, "IMF_Value", "IMF Value Indicator")

# 5. Print plots
print(cpi_plot)
print(gdp_plot)
print(exports_plot)
print(govexp_plot)
print(unemp_plot)
print(imf_plot)


# -------------------------------
# Section 12: Basic Panel Regression (Fixed & Safe)
# -------------------------------

library(plm)
library(car)

# 1. Check multicollinearity (VIF)
vif_model <- lm(CPI ~ GDP + Exports + GovExp + Unemployment + IMF_Value, data = master_data_panel_named)
vif_values <- vif(vif_model)
print(vif_values)
# All VIFs < 5 → no serious multicollinearity

# 2. Remove or adjust variables causing singularity
# If singularity persists, remove IMF_Value (or another variable with very low within-country variation)
panel_data_adjusted <- master_data_panel_named %>%
  select(Country.Code, Year, CPI, GDP, Exports, GovExp, Unemployment)

# 3. Fixed-effects panel regression
panel_model <- plm(
  CPI ~ GDP + Exports + GovExp + Unemployment,
  data = panel_data_adjusted,
  index = c("Country.Code", "Year"),
  model = "within"
)

# 4. Summary
summary(panel_model)

# 5. Optional: Check R-squared and significance
fixef_summary <- summary(panel_model)
fixef_summary$r.squared
fixef_summary$coefficients

