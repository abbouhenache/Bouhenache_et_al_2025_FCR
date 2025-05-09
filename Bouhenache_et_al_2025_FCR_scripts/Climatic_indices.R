#### Script for climatic indices treatment & visualization Bouhenache at al. 2025 FCR ####

### Script by: Abderrahim BOUHENACHE

# Detach all non-base packages in one-liner
invisible(lapply(setdiff(loadedNamespaces(), c("base", "stats", "utils", "methods", "datasets", "graphics", "grDevices")),
                 function(pkg) try(detach(paste("package", pkg, sep = ":"), unload = TRUE, character.only = TRUE), silent = TRUE)))

# Clear all objects from the working environment
rm(list = ls())

# Clear the console
cat("\014") 

# Clear all the graphics
if (!is.null(dev.list())) dev.off()

#### Call necessary packages and set directories ####

#### Import and process the data ####
## Call necessary packages 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, here, SPEI)

# Create a path for saving figures
vis_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Figures")

# Create a path for saving figures
tables_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Tables")

## change system locale settings to English for x axis labels
Sys.setlocale("LC_ALL", "English")



#### Import and prepare data ####

# Import data for experimental years
Experiment_climate_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Exp_years_climate_dataset")

# Import data for historical period
Historical_climate_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Historical_climate_dataset")

## data preparation
Experiment_climate_data <- Experiment_climate_data %>%
  # Remove header description row
  slice(-1) %>%
  # Remove unwanted columns
  select(-Field, -idPoint, -GPS_coordinates) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(across(
    .cols = -c(date), 
    .fns = ~ round(as.numeric(.), 1)
  ))


Historical_climate_data <- Historical_climate_data %>%
  # Remove header description row
  slice(-1) %>%
  # Remove unwanted columns
  select(-Field, -idPoint, -GPS_coordinates) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(across(
    .cols = -c(date), 
    .fns = ~ round(as.numeric(.), 1)
  ))


#### Calculate long term annual rainfall based on year from 15/05 of n year to 15/05 of n+1 ####
## This annual mean is calculated instead of annual calendar years because
## crop season span over two calendar year in SSA and because the average harvest year is 
## 15/05 in the study area

annual_rainfall_data <- Historical_climate_data %>% 
  mutate(Crop_year = if_else(date >= make_date(Year, 5, 15), Year, Year - 1)) %>%
  filter(Crop_year != "2008" & Crop_year != "2022") %>%
  group_by(Crop_year) %>%
  summarise(RAIN_mm = sum(RAIN_mm, na.rm = TRUE))

mean_annual_rainfall <- mean(annual_rainfall_data$RAIN_mm)

print(mean_annual_rainfall)


#### filter only cropping seasons data (between 15/11 and 15/05) ####
### on historical data 

Historical_seasonal_data <- Historical_climate_data %>%
  filter((Year > 2009 | (Year == 2009 & Month >= 6)) &  # Keep data from June onwards for 2009
           ((Month == 11 & Day >= 15) |
              (Month == 12) |  
              (Month %in% 1:4) |  
              (Month == 5 & Day <= 15))) %>%
  ## add Crop_season variable
  mutate(Crop_season = case_when(
    Month >= 11 ~ paste0(Year, "-", Year + 1),  # If month is November or December, season starts from current year
    Month <= 5  ~ paste0(Year - 1, "-", Year)   # If month is January to May, it belongs to the previous year's season
  )) %>% 
  
  droplevels() %>%
  
  ## add periods of the cycle 
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  ) %>%
  
  relocate(Period, .after = DOY) %>%
  relocate(Crop_season, .before = date)

Summary_hist_seasonal_data <- Historical_seasonal_data %>% 
  group_by(Crop_season) %>%
  summarise(RAIN_mm = sum(RAIN_mm, na.rm = TRUE),
            TMAX = mean(TMAX, na.rm = TRUE),
            TMIN = mean(TMIN, na.rm = TRUE))

summary(Historical_seasonal_data)



### Experimental data 
### Edit UZAIP data
Experiment_seasonal_data <- Experiment_climate_data %>%
  mutate(
    date = as.Date(date),
    across(-date, as.numeric),
    DOY = yday(date) # Create DOY here
  ) %>%
  rename(
    RAIN_mm = rain,
    TMEAN = tmean,
    TMIN = tmin,
    TMAX = tmax,
    RELHUM = rhum,
    WIND_ms = wind,
    Month = Nmonth,
    Year = year,
    Day = NdayM
  ) %>%
  select(date, Day, Month, Year, DOY, RELHUM, TMAX, TMIN, TMEAN, RAIN_mm, WIND_ms) %>%
  
  ## Keep only data of cropping seasons 
  filter(((Month == 11 & Day >= 15) |
            (Month == 12) |  
            (Month %in% 1:4) |  
            (Month == 5 & Day <= 15))) %>%
  ## add Crop_season variable
  mutate(Crop_season = case_when(
    Month >= 11 ~ paste0(Year, "-", Year + 1),  # If month is November or December, season starts from current year
    Month <= 5  ~ paste0(Year - 1, "-", Year)   # If month is January to May, it belongs to the previous year's season
  )) %>% 
  
  ## add periods of the cycle 
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  ) %>%
  filter(date != "2024-11-15") %>%
  
  relocate(Period, .after = DOY) %>%
  relocate(Crop_season, .before = date)

#### Candidate climatic indices ####
#### Indices per cropping season ####

## calculate percentiles for temperature and rainfall

Overall_season_qvalues <- Historical_seasonal_data %>%
  summarise(
    TX = mean(TMAX),
    TN = mean(TMIN),
    TMEAN = mean(TMEAN),
    TXx = max(TMAX),
    TXn = min(TMAX),
    TNx = max(TMIN),
    TNn = min(TMIN),
    TX95 = quantile(TMAX, 0.95, na.rm = TRUE),
    TX99 = quantile(TMAX, 0.99, na.rm = TRUE),
    TN95 = quantile(TMIN, 0.95, na.rm = TRUE),
    TN99 = quantile(TMIN, 0.99, na.rm = TRUE),
    R95 = quantile(RAIN_mm, 0.95, na.rm = TRUE),
    R99 = quantile(RAIN_mm, 0.99, na.rm = TRUE)
  )

Season_indices <- Historical_seasonal_data %>% 
  group_by(Crop_season) %>%
  summarise(
    ## temperature indices
    TX = mean(TMAX),
    TN = mean(TMIN),
    TX95p = sum(TMAX > Overall_season_qvalues$TX95, na.rm = TRUE)/n()*100,
    TX99p = sum(TMAX > Overall_season_qvalues$TX99, na.rm = TRUE)/n()*100,
    TN95p = sum(TMIN > Overall_season_qvalues$TN95, na.rm = TRUE)/n()*100,
    TN99p = sum(TMIN > Overall_season_qvalues$TN99, na.rm = TRUE)/n()*100,
    EHD_I30 = sum(ifelse(TMAX > 30, TMAX - 30, 0), na.rm = TRUE),
    
    ## rainfall 
    RX1day = max(RAIN_mm),
    
    RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    Rnn95p = sum(RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    Rnn99p = sum(RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RAIN_mm[RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RAIN_mm[RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
  ) %>%
  left_join(
    Historical_seasonal_data %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 25/02 (DOY 56) to 15/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(Prec_R1 =sum(RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  )



## Expe crop seasons indices 
Exp_season_indices <- Experiment_seasonal_data %>% 
  group_by(Crop_season) %>%
  summarise(
    ## temperature indices
    TX = mean(TMAX),
    TN = mean(TMIN),
    TX95p = sum(TMAX > Overall_season_qvalues$TX95, na.rm = TRUE)/n()*100,
    TX99p = sum(TMAX > Overall_season_qvalues$TX99, na.rm = TRUE)/n()*100,
    TN95p = sum(TMIN > Overall_season_qvalues$TN95, na.rm = TRUE)/n()*100,
    TN99p = sum(TMIN > Overall_season_qvalues$TN99, na.rm = TRUE)/n()*100,
    EHD_I30 = sum(ifelse(TMAX > 30, TMAX - 30, 0), na.rm = TRUE),
    
    ## rainfall 
    RX1day = max(RAIN_mm),
    
    RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    Rnn95p = sum(RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    Rnn99p = sum(RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RAIN_mm[RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RAIN_mm[RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
  ) %>%
  left_join(
    Experiment_seasonal_data %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 25/02 (DOY 56) to 15/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(Prec_R1 =sum(RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  )


#### Plot exploratory temperature indices ####

# Reshape data into long format for plotting
Season_indices_long <- Season_indices %>%
  pivot_longer(cols = -Crop_season, names_to = "Variable", values_to = "Value")

# Reshape data into long format for plotting
Exp_season_indices_long <- Exp_season_indices %>%
  pivot_longer(cols = -Crop_season, names_to = "Variable", values_to = "Value")

# Separate temperature and rainfall variables
temperature_vars <- c("TX", "TX95p", "TX99p", "TN", "TN95p", "TN99p")


Season_indices_temp <- Season_indices_long %>% filter(Variable %in% temperature_vars)

# Filter for temperature variables (Experimental)
Exp_indices_temp <- Exp_season_indices_long %>% filter(Variable %in% temperature_vars)

# Plot temperature variables with experimental seasons overlaid
Seasonal_temp_selected_indices_plot <- ggplot(Season_indices_temp, aes(y = Value)) +
  geom_boxplot(width = 0.5) +  # Boxplot for historical data
  stat_summary(aes(x = 0), fun.y=mean, geom="point", shape=23, size=1.6, color="grey10", fill="grey10") +
  geom_point(data = Exp_indices_temp, aes(x = 0, y = Value, color = Crop_season), 
             size = 2.7) +  # Points for experimental seasons
  theme_bw() +
  labs(y = "Value", title = "Temperature indices", color = "Experiment seasons") +
  scale_color_manual(values = c("2022-2023" = "#fc8d62", "2023-2024" = "#66c2a5"),
                     labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Custom labels for Crop_season
  
  xlim(-0.5, 0.5) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(Seasonal_temp_selected_indices_plot, file=paste0("./Seasonal_temp_selected_indices_plot.png"), path =vis_path, width = 13, height = 10, units = "cm", dpi = 600)


#### Explore GDD difference between the experimental seasons ####

#### Candidate crop cycle indices ####

sowing_date_2023 <- as.Date("2022-11-25")
sowing_date_2024 <- as.Date("2023-12-12")

emergence_date_2023 <- as.Date("2022-12-07")
emergence_date_2024 <- as.Date("2023-12-20")

harvest_date_2023 <- as.Date("2023-05-05")
harvest_date_2024 <- as.Date("2024-04-26")

GDD_data <- Experiment_seasonal_data %>%
  filter(
    (Crop_season == "2022-2023" & date >= sowing_date_2023 & date <= harvest_date_2023) |
      (Crop_season == "2023-2024" & date >= sowing_date_2024 & date <= harvest_date_2024)
  ) %>% 
  
  ## create a DAS vareiable 
  mutate(
    # Days After Sowing (DAS)
    DAS = case_when(
      Crop_season == "2022-2023" ~ as.numeric(date - sowing_date_2023),
      Crop_season == "2023-2024" ~ as.numeric(date - sowing_date_2024),
      TRUE ~ NA_real_  # Default to NA (shouldn't occur)
    )) %>%
  
  mutate(
    DD_s = pmax(TMEAN - 8, 0),  # Calculate daily GDD from sowing with base temperature of 8°C
    # GDD from emergence
    DD_e = case_when(
      (Crop_season == "2022-2023" & date >= emergence_date_2023) ~ pmax(TMEAN - 8, 0),
      (Crop_season == "2023-2024" & date >= emergence_date_2024) ~ pmax(TMEAN - 8, 0),
      TRUE ~ 0)  # Set to 0 before emergence
  ) %>%
  group_by(Crop_season) %>%
  mutate(GDD_s = cumsum(DD_s),
         GDD_e = cumsum(DD_e))  # Compute cumulative GDD from sowing per crop season

## Edit cropping season labels for plot

### plot GDD_s and GDD_e on the same plot using DAS
GDD_line_plot <- ggplot(GDD_data, aes(x = DAS, color = Crop_season)) +
  geom_line(aes(y = GDD_s, linetype = "GDD_s"), linewidth = 0.6) +  # Dashed line for GDD_s
  geom_line(aes(y = GDD_e, linetype = "GDD_e"), linewidth = 0.6) +  # Solid line for GDD_e
  scale_linetype_manual(values = c("GDD_s" = "dashed", "GDD_e" = "solid"),
                        labels = c("GDD_s" = "GDD_sowing", "GDD_e" = "GDD_emergence")) + 
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.05)),
                     breaks = seq(0, 1800, by = 250)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0, 160, by = 20)) +
  scale_color_manual(values = c("2022-2023" = "#66c2a5", "2023-2024" = "#fc8d62"),
                     labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Custom labels for Crop_season
  theme_bw() +
  labs(
    y = "Cumulative GDD (°C day)",
    color = "",
    linetype = ""
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90"),
    axis.title.y = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 12),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 12), 
    legend.position = "bottom"
  )


ggsave(GDD_line_plot, file=paste0("./comparative_GDD_line_plot.png"), path =vis_path, width = 19, height = 10, units = "cm", dpi = 600)



#### Indices per periods of the cropping season ####

Periods_qvalues <- Historical_seasonal_data %>%
  group_by(Period) %>%
  summarise(
    R95 = quantile(RAIN_mm, 0.95, na.rm = TRUE),
    R99 = quantile(RAIN_mm, 0.99, na.rm = TRUE)
  )


# Calculate Seasonal periods indices
Season_periods_indices <- Historical_seasonal_data %>%
  left_join(Periods_qvalues, by = "Period") %>%
  group_by(Crop_season, Period) %>%
  summarise(
        ## rainfall
    RX1day = max(RAIN_mm, na.rm = TRUE),
    RX5day = {
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      max(rolling_5day, na.rm = TRUE)
    },
    Rnn95p = sum(RAIN_mm > R95, na.rm = TRUE),
    Rnn99p = sum(RAIN_mm > R99, na.rm = TRUE),
    R95p = sum(RAIN_mm[RAIN_mm > R95], na.rm = TRUE),
    R99p = sum(RAIN_mm[RAIN_mm > R99], na.rm = TRUE),
    PRCPTOT = sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE),
    CDD = {
      dry_days <- RAIN_mm < 1
      rle_values <- rle(dry_days)
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      max_consecutive_dry_days
    },
    SDII = {
      wet_days <- RAIN_mm >= 1
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      if (total_wet_days > 0) {
        PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
  )




### Calculate experimental seasonal periods indices
Exp_season_periods_indices <- Experiment_seasonal_data %>%
  left_join(Periods_qvalues, by = "Period") %>%
  group_by(Crop_season, Period) %>%
  summarise(
    ## rainfall
    RX1day = max(RAIN_mm, na.rm = TRUE),
    RX5day = {
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      max(rolling_5day, na.rm = TRUE)
    },
    Rnn95p = sum(RAIN_mm > R95, na.rm = TRUE),
    Rnn99p = sum(RAIN_mm > R99, na.rm = TRUE),
    R95p = sum(RAIN_mm[RAIN_mm > R95], na.rm = TRUE),
    R99p = sum(RAIN_mm[RAIN_mm > R99], na.rm = TRUE),
    PRCPTOT = sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE),
    CDD = {
      dry_days <- RAIN_mm < 1
      rle_values <- rle(dry_days)
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      max_consecutive_dry_days
    },
    SDII = {
      wet_days <- RAIN_mm >= 1
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      if (total_wet_days > 0) {
        PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
  )




#### Perform calculation of precipitation indices for all rainfall treatments ####
## add heavy rainfall events
Exp_data_rain_treats <- Experiment_seasonal_data %>%
  select(-c(RELHUM, TMAX, TMIN, TMEAN, WIND_ms)) %>%
  mutate(RR_RAIN_mm = case_when(date < "2022-11-25" ~ RAIN_mm,
                                date >= "2022-11-25" ~ RAIN_mm*0.7),
         HR_RAIN_mm = case_when(date %in% c("2023-03-07", "2023-03-16",
                                            "2024-01-23", "2024-02-03") ~ 100,
                                TRUE ~ RAIN_mm),
         PR_RAIN_mm = case_when(date %in% c("2023-03-07", "2023-03-16") ~ 35,
                                date %in% c("2024-01-31") ~ 25.8,
                                date %in% c("2024-02-20") ~ 25,
                                date %in% c("2024-03-08") ~ 38,
                                TRUE ~ RAIN_mm)) %>%
  
  # Calculate cumuls 
  group_by(Crop_season) %>%
  mutate(cumul_AR_RAIN_mm = cumsum(RAIN_mm),
         cumul_RR_RAIN_mm = cumsum(RR_RAIN_mm),
         cumul_HR_RAIN_mm = cumsum(HR_RAIN_mm),
         cumul_PR_RAIN_mm = cumsum(PR_RAIN_mm))

### perform indices calculation for precipitation at level of cropping season

Rain_exp_season_indices <- Exp_data_rain_treats %>% 
  group_by(Crop_season) %>%
  summarise(
    ## AR
    AR_RX1day = max(RAIN_mm),
    
    AR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    AR_Rnn95p = sum(RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    AR_Rnn99p = sum(RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    AR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RAIN_mm[RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    AR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RAIN_mm[RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    AR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    AR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    AR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        AR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    ## RR
    RR_RX1day = max(RR_RAIN_mm),
    
    RR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    RR_Rnn95p = sum(RR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    RR_Rnn99p = sum(RR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    RR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RR_RAIN_mm[RR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    RR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RR_RAIN_mm[RR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    RR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RR_RAIN_mm[RR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    RR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    RR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        RR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    
    ## HR
    HR_RX1day = max(HR_RAIN_mm),
    
    HR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(HR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    HR_Rnn95p = sum(HR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    HR_Rnn99p = sum(HR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    HR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(HR_RAIN_mm[HR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    HR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(HR_RAIN_mm[HR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    HR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(HR_RAIN_mm[HR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    HR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- HR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    HR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- HR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        HR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    ## PR
    PR_RX1day = max(PR_RAIN_mm),
    
    PR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(PR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    PR_Rnn95p = sum(PR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    PR_Rnn99p = sum(PR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    PR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(PR_RAIN_mm[PR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    PR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(PR_RAIN_mm[PR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    PR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(PR_RAIN_mm[PR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    PR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- PR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    PR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- PR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        PR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
    
    
  ) %>%
  left_join(
    Exp_data_rain_treats %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 25/02 (DOY 56) to 15/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(AR_Prec_R1 =sum(RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  ) %>%
  left_join(
    Exp_data_rain_treats %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 24/02 (DOY 55) to 16/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(RR_Prec_R1 =sum(RR_RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  ) %>%
  left_join(
    Exp_data_rain_treats %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 25/02 (DOY 56) to 15/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(HR_Prec_R1 =sum(HR_RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  ) %>%
  left_join(
    Exp_data_rain_treats %>% 
      filter(DOY >= 55 & DOY <= 75) %>%  # 25/02 (DOY 56) to 15/03 (DOY 74)
      group_by(Crop_season) %>%
      summarise(PR_Prec_R1 =sum(PR_RAIN_mm, na.rm = TRUE)), 
    by = "Crop_season"
  ) 




## Select only indices of interest

Rain_exp_season_select_indices <- Rain_exp_season_indices %>%
  select(Crop_season, matches("_(CDD|PRCPTOT|R95p|R99p|Rnn95p|Rnn99p|RX1day|RX5day|SDII|Prec_R1)$")) %>%
  pivot_longer(
    cols = matches("_(CDD|PRCPTOT|R95p|R99p|Rnn95p|Rnn99p|RX1day|RX5day|SDII|Prec_R1)$"),  # Select relevant columns
    names_to = c("Rain_Treat", "Variable"),  # New column names
    names_pattern = "^(AR|RR|HR|PR)_(.*)$",  # Extract Rain_Treat and Variable correctly
    values_to = "Value"  # Set values column name
  ) %>%
  arrange(Crop_season, Rain_Treat)

# create a vector for rainfall variables
selected_rainfall_vars <- c("CDD","PRCPTOT","R95p","R99p","Rnn95p","Rnn99p","RX1day","RX5day","SDII","Prec_R1")

# Filter for rainfall variables
Season_selected_indices_rain <- Season_indices_long %>% filter(Variable %in% selected_rainfall_vars)

## rearrange levels of variable

Season_selected_indices_rain$Variable <- factor(
  Season_selected_indices_rain$Variable,
  levels = selected_rainfall_vars
)


Rain_exp_season_select_indices$Variable <- factor(Rain_exp_season_select_indices$Variable, levels = selected_rainfall_vars)
Rain_exp_season_select_indices$Rain_Treat <- factor(Rain_exp_season_select_indices$Rain_Treat, levels = c("AR", "RR", "HR", "PR"), labels = c("AR", "RR", "HR", "PR-N"))

# Plot rainfall variables
rain_treats_selected_indices_plot <- ggplot(Season_selected_indices_rain, aes(y = Value)) +
  geom_boxplot(width = 0.75) +  # Boxplot for historical data
  stat_summary(aes(x = 0), fun.y=mean, geom="point", shape=23, size=1.6, color="grey10", fill="grey10") +
  geom_point(data = Rain_exp_season_select_indices, aes(y = Value, x = 0, color = Crop_season, shape = Rain_Treat), 
             size = 3) +  # Points for experimental seasons
  theme_bw() +
  labs(y = "Value", title = "Rainfall indices", color = "Experiment seasons", shape = "Rainfall treatment") +
  xlim(-0.5, 0.5) +
  scale_color_manual(values = c("2022-2023" = "#fc8d62", "2023-2024" = "#66c2a5"),
                     labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Custom labels for Crop_season
  facet_wrap(~ Variable, scales = "free_y", ncol = 5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 12),   # Increase y-axis tick text size
    strip.text = element_text(size = 12.5),     # Increase facet label text size
    legend.title = element_text(size = 13),   # Increase legend title text size
    legend.text = element_text(size = 12.5),    # Increase legend item text size
    legend.position = "right")

ggsave(rain_treats_selected_indices_plot, file=paste0("./Seasonal_rain_treats_selected_indices_plot.png"), path =vis_path, width = 22, height = 12, units = "cm", dpi = 600)



#### Perform calculations per cropping season and crop growth periods

Rain_exp_season_periods_indices <- Exp_data_rain_treats %>% 
  group_by(Crop_season, Period) %>%
  summarise(
    ## AR
    AR_RX1day = max(RAIN_mm),
    
    AR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    AR_Rnn95p = sum(RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    AR_Rnn99p = sum(RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    AR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RAIN_mm[RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    AR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RAIN_mm[RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    AR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RAIN_mm[RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    AR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    AR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        AR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    ## RR
    RR_RX1day = max(RR_RAIN_mm),
    
    RR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(RR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    RR_Rnn95p = sum(RR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    RR_Rnn99p = sum(RR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    RR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(RR_RAIN_mm[RR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    RR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(RR_RAIN_mm[RR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    RR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(RR_RAIN_mm[RR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    RR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- RR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    RR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- RR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        RR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    
    ## HR
    HR_RX1day = max(HR_RAIN_mm),
    
    HR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(HR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    HR_Rnn95p = sum(HR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    HR_Rnn99p = sum(HR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    HR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(HR_RAIN_mm[HR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    HR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(HR_RAIN_mm[HR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    HR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(HR_RAIN_mm[HR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    HR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- HR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    HR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- HR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        HR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    },
    
    ## PR
    PR_RX1day = max(PR_RAIN_mm),
    
    PR_RX5day = {
      # Create a rolling sum of 5-day precipitation
      rolling_5day <- zoo::rollsum(PR_RAIN_mm, 5, align = "right", fill = NA)
      
      # Find the maximum 5-day precipitation sum in the season
      max(rolling_5day, na.rm = TRUE)
    },
    
    PR_Rnn95p = sum(PR_RAIN_mm > Overall_season_qvalues$R95, na.rm = TRUE),
    PR_Rnn99p = sum(PR_RAIN_mm > Overall_season_qvalues$R99, na.rm = TRUE),
    
    PR_R95p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R95_threshold <- Overall_season_qvalues$R95
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R95 <- sum(PR_RAIN_mm[PR_RAIN_mm > R95_threshold], na.rm = TRUE)
      
      total_precip_above_R95
    },
    
    PR_R99p = {
      # Calculate the 95th percentile of daily precipitation for the season
      R99_threshold <- Overall_season_qvalues$R99
      
      # Sum the precipitation for days when daily precipitation exceeds the R95 threshold
      total_precip_above_R99 <- sum(PR_RAIN_mm[PR_RAIN_mm > R99_threshold], na.rm = TRUE)
      
      total_precip_above_R99
    },
    
    PR_PRCPTOT = {
      # Sum the precipitation on days where RR >= 1mm (wet days)
      total_precip_wet_days <- sum(PR_RAIN_mm[PR_RAIN_mm >= 1], na.rm = TRUE)
      
      total_precip_wet_days
    },
    
    PR_CDD = {
      # Identify dry days (RR < 1mm)
      dry_days <- PR_RAIN_mm < 1
      
      # Calculate the length of consecutive dry days
      rle_values <- rle(dry_days)
      
      # Get the maximum length of consecutive dry days
      max_consecutive_dry_days <- if(length(rle_values$lengths[rle_values$values == TRUE]) > 0) {
        max(rle_values$lengths[rle_values$values == TRUE], na.rm = TRUE)
      } else {
        0
      }
      
      max_consecutive_dry_days
    },
    
    PR_SDII = {
      # Identify wet days (RR >= 1mm)
      wet_days <- PR_RAIN_mm >= 1
      
      # Count the number of wet days
      total_wet_days <- sum(wet_days, na.rm = TRUE)
      
      # Compute SDII only if there are wet days (avoid division by zero)
      if (total_wet_days > 0) {
        PR_PRCPTOT / total_wet_days
      } else {
        NA
      }
    }
    
    
  ) 


### select variables of interest and transform data to long format 

Rain_exp_season_periods_select_indices <- Rain_exp_season_periods_indices %>%
  select(Crop_season, Period, matches("_(CDD|PRCPTOT|R95p|R99p|Rnn95p|Rnn99p|RX1day|RX5day|SDII|Prec_R1)$")) %>%
  pivot_longer(
    cols = -c(Crop_season, Period),  # Select relevant columns
    names_to = c("Rain_Treat", "Variable"),  # New column names
    names_pattern = "^(AR|RR|HR|PR)_(.*)$",  # Extract Rain_Treat and Variable correctly
    values_to = "Value"  # Set values column name
  ) %>%
  arrange(Crop_season, Period, Rain_Treat)


Rain_exp_season_periods_select_indices$Variable <- factor(Rain_exp_season_periods_select_indices$Variable, levels = selected_rainfall_vars)
Rain_exp_season_periods_select_indices$Rain_Treat <- factor(Rain_exp_season_periods_select_indices$Rain_Treat, levels = c("AR", "RR", "HR", "PR"),labels = c("AR", "RR", "HR", "PR-N"))

# Reshape data into long format for plotting
Season_periods_indices_long <- Season_periods_indices %>%
  pivot_longer(cols = -c(Crop_season, Period), names_to = "Variable", values_to = "Value")

# Reshape data into long format for plotting
Exp_season_periods_indices_long <- Exp_season_periods_indices %>%
  pivot_longer(cols = -c(Crop_season, Period), names_to = "Variable", values_to = "Value")


# Filter for rainfall variables
Season_periods_selected_indices_rain <- Season_periods_indices_long %>% filter(Variable %in% selected_rainfall_vars)

# Filter for rainfall variables (Experimental)
Exp_periods_selected_indices_rain <- Exp_season_periods_indices_long %>% filter(Variable %in% selected_rainfall_vars)


#### Plot of period specific CDD for supplementary of article 1 #### 

rain_treats_periods_CDD_plot <- ggplot(subset(Season_periods_selected_indices_rain %>% filter(Variable == "CDD")), aes(x = Period, y = Value)) +
  geom_boxplot(width = 0.6) +  # Boxplot for historical data
  stat_summary(fun.y=mean, geom="point", shape=23, size=1.4, color="grey10", fill="grey10") +
  geom_point(data = subset(Rain_exp_season_periods_select_indices %>% filter(Variable == "CDD")), aes(y = Value, x = Period, color = Crop_season, shape = Rain_Treat), 
             size = 2.5) +  # Points for experimental seasons
  theme_bw() +
  labs(y = "CDD (days)", x = "Phase", color = "Experiment seasons", shape = "Rain treatments") +
  
  scale_x_discrete(labels = c(
    "P0" = "P_S-E",
    "P1" = "P_E-V6",
    "P2" = "P_V6-V10",
    "P3" = "P_V10-R1",
    "P4" = "P_R1-R6"
  )) +
  
  scale_color_manual(values = c("2022-2023" = "#fc8d62", "2023-2024" = "#66c2a5"),
                     labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Custom labels for Crop_season
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90"),
    axis.title.y = element_text(size = 13.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 13),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 13.5),  # Increase y-axis title text size
    axis.text.x = element_text(size = 13, angle = 45, vjust = 0.5),   # Increase y-axis tick text size
    strip.text = element_text(size = 13.5),     # Increase facet label text size
    legend.title = element_text(size = 14),   # Increase legend title text size
    legend.text = element_text(size = 13.5),    # Increase legend item text size
    legend.position = "right")

ggsave(rain_treats_periods_CDD_plot, file=paste0("./rain_treats_periods_CDD_plot.png"), path =vis_path, width = 15, height = 10, units = "cm", dpi = 600)





# Step 1: Aggregate the precipitation by Year and Month
combined_data <- bind_rows(Historical_seasonal_data, Experiment_seasonal_data)

## Add periods for combined data
combined_data <- combined_data %>%
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  )


data_period <- combined_data %>%
  group_by(Crop_season, Period) %>%
  summarise(Rain_mm_period = sum(RAIN_mm, na.rm = TRUE)) %>%
  ungroup()

summary(data_period)

# Step 2: Ensure there are no missing or zero precipitation values (important for SPI calculation)
data_period <- data_period %>%
  filter(!is.na(Rain_mm_period) & Rain_mm_period > 0)

# Create a continuous time index
data_period <- data_period %>%
  arrange(Crop_season, Period) %>%
  mutate(Time_Index = row_number()) # Create a sequential index

# Convert to time series object
rain_ts <- ts(data_period$Rain_mm_period, start = 1, frequency = 5)  # 5 periods per season

# Step 3: Compute SPI using this time series
spi_values <- spi(rain_ts, scale = 1) 

data_period <- data_period %>%
  mutate(SPI = spi_values$fitted)

# View results
print(data_period)


# Reorder Period for better visualization
data_period <- data_period %>%
  mutate(Period = factor(Period, levels = c("P_S-E", "P_E-V6", "P_V6-V10", "P_V10-R1", "P_R1-R6")))

# Calculate means for Rainfall and SPI over all Crop_seasons
mean_rainfall <- mean(data_period$Rain_mm_period, na.rm = TRUE)
mean_spi <- mean(data_period$SPI, na.rm = TRUE)


# Create the plot with two geom_line for Rain and SPI, including dashed lines for means
SPI_check_plot <- ggplot(data_period) +
  geom_line(aes(x = Crop_season, y = Rain_mm_period, group = Period, color = "Rainfall (mm)"), size = 0.8) +
  geom_line(aes(x = Crop_season, y = SPI * 30, group = Period, color = "SPI"), size = 0.8) +  # Scale SPI for better visibility
  geom_hline(yintercept = mean_rainfall, linetype = "dashed", color = "blue", size = 0.5) +  # Mean for Rainfall
  geom_hline(yintercept = mean_spi * 30, linetype = "dashed", color = "red", size = 0.5) +  # Mean for SPI (scaled)
  facet_wrap(~ Period, scales = "free_y", ncol = 1) +  # Facet by Period vertically
  theme_bw() +
  labs(title = "Rainfall and SPI over Crop Seasons by Phase",
       x = "Crop Season",
       y = "Rainfall (mm)",
       color = "Legend") +
  scale_color_manual(values = c("Rainfall (mm)" = "blue", "SPI" = "red")) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 10, name = "SPI (-)")  # Secondary axis for SPI
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.5, angle = 45),
        legend.position = "top")


print(SPI_check_plot)



SPI_AR_period <- data_period %>%
  select(-Time_Index) %>%
  filter(Crop_season %in% c("2022-2023", "2023-2024")) %>%
  mutate(Rain_Treat = "AR")


### SPI for RR ####

Exp_RR_data <- Exp_data_rain_treats %>%
  select(c(Crop_season:Period, RR_RAIN_mm)) %>%
  mutate(RAIN_mm = RR_RAIN_mm) %>%
  select(-RR_RAIN_mm)

# Step 1: Aggregate the precipitation by Year and Month
combined_data <- bind_rows(Historical_seasonal_data, Exp_RR_data)

## Add periods for combined data
combined_data <- combined_data %>%
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  )


data_period <- combined_data %>%
  group_by(Crop_season, Period) %>%
  summarise(Rain_mm_period = sum(RAIN_mm, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Ensure there are no missing or zero precipitation values (important for SPI calculation)
data_period <- data_period %>%
  filter(!is.na(Rain_mm_period) & Rain_mm_period > 0)

# Create a continuous time index
data_period <- data_period %>%
  arrange(Crop_season, Period) %>%
  mutate(Time_Index = row_number()) # Create a sequential index

# Convert to time series object
rain_ts <- ts(data_period$Rain_mm_period, start = 1, frequency = 5)  # 5 periods per season

# Step 3: Compute SPI using this time series
spi_values <- spi(rain_ts, scale = 1) 

data_period <- data_period %>%
  mutate(SPI = spi_values$fitted)

# View results
print(data_period)

SPI_RR_period <- data_period %>%
  select(-Time_Index) %>%
  filter(Crop_season %in% c("2022-2023", "2023-2024")) %>%
  mutate(Rain_Treat = "RR")



### SPI for HR ####

Exp_HR_data <- Exp_data_rain_treats %>%
  select(c(Crop_season:Period, HR_RAIN_mm)) %>%
  mutate(RAIN_mm = HR_RAIN_mm) %>%
  select(-HR_RAIN_mm)

# Step 1: Aggregate the precipitation by Year and Month
combined_data <- bind_rows(Historical_seasonal_data, Exp_HR_data)


## Add periods for combined data
combined_data <- combined_data %>%
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  )


data_period <- combined_data %>%
  group_by(Crop_season, Period) %>%
  summarise(Rain_mm_period = sum(RAIN_mm, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Ensure there are no missing or zero precipitation values (important for SPI calculation)
data_period <- data_period %>%
  filter(!is.na(Rain_mm_period) & Rain_mm_period > 0)

# Create a continuous time index
data_period <- data_period %>%
  arrange(Crop_season, Period) %>%
  mutate(Time_Index = row_number()) # Create a sequential index

# Convert to time series object
rain_ts <- ts(data_period$Rain_mm_period, start = 1, frequency = 5)  # 5 periods per season

# Step 3: Compute SPI using this time series
spi_values <- spi(rain_ts, scale = 1) 

data_period <- data_period %>%
  mutate(SPI = spi_values$fitted)

# View results
print(data_period)

SPI_HR_period <- data_period %>%
  select(-Time_Index) %>%
  filter(Crop_season %in% c("2022-2023", "2023-2024")) %>%
  mutate(Rain_Treat = "HR")




### SPI for PR ####

Exp_PR_data <- Exp_data_rain_treats %>%
  select(c(Crop_season:Period, PR_RAIN_mm)) %>%
  mutate(RAIN_mm = PR_RAIN_mm) %>%
  select(-PR_RAIN_mm)

# Step 1: Aggregate the precipitation by Year and Month
combined_data <- bind_rows(Historical_seasonal_data, Exp_PR_data)

## Add periods for combined data
combined_data <- combined_data %>%
  mutate(
    Period = as.factor(case_when(
      Month == 11 & Day >= 15 ~ "P_S-E",
      Month == 12 & Day <= 20 ~ "P_S-E",
      Month == 12 & Day >= 21 ~ "P_E-V6",
      Month == 1 & Day <= 21 ~ "P_E-V6",
      Month == 1 & Day >= 22 ~ "P_V6-V10",
      Month == 2 & Day <= 6 ~ "P_V6-V10",
      Month == 2 & Day >= 7 ~ "P_V10-R1",
      Month == 3 & Day <= 5 ~ "P_V10-R1",
      Month == 3 & Day >= 6 ~ "P_R1-R6",
      Month %in% 4:5 ~ "P_R1-R6",
      TRUE ~ NA_character_
    ))
  )


data_period <- combined_data %>%
  group_by(Crop_season, Period) %>%
  summarise(Rain_mm_period = sum(RAIN_mm, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Ensure there are no missing or zero precipitation values (important for SPI calculation)
data_period <- data_period %>%
  filter(!is.na(Rain_mm_period) & Rain_mm_period > 0)

# Create a continuous time index
data_period <- data_period %>%
  arrange(Crop_season, Period) %>%
  mutate(Time_Index = row_number()) # Create a sequential index

# Convert to time series object
rain_ts <- ts(data_period$Rain_mm_period, start = 1, frequency = 5)  # 5 periods per season

# Step 3: Compute SPI using this time series
spi_values <- spi(rain_ts, scale = 1) 

data_period <- data_period %>%
  mutate(SPI = spi_values$fitted)

# View results
print(data_period)

SPI_PR_period <- data_period %>%
  select(-Time_Index) %>%
  filter(Crop_season %in% c("2022-2023", "2023-2024")) %>%
  mutate(Rain_Treat = "PR")


### merge all data
SPI_period_data <- rbind(SPI_AR_period, SPI_RR_period, SPI_HR_period, SPI_PR_period) %>%
  mutate(Rain_Treat =  as.factor(Rain_Treat)) %>%
  relocate(Rain_Treat, .after = Period) %>%
  mutate(Period  = factor(Period, 
                            levels = c("P_S-E", "P_E-V6", "P_V6-V10", "P_V10-R1" , "P_R1-R6"), 
                            labels = c("P_S-E", "P_E-V6", "P_V6-V10", "P_V10-R1" , "P_R1-R6"))) %>%
  mutate(Rain_Treat = factor(Rain_Treat, levels = c("RR", "AR", "HR", "PR"))) 


# Categorize SPI values
SPI_period_data <- SPI_period_data %>%
  mutate(SPI_class = case_when(
    SPI > 2.0  ~ "Extremely wet",
    SPI > 1.5 & SPI <= 2.0  ~ "Very wet",
    SPI > 1.0 & SPI <= 1.5  ~ "Moderately wet",
    SPI > 0   & SPI <= 1.0  ~ "Mild wet",
    #SPI == 0  ~ "Neutral",
    SPI > -1.0 & SPI < 0  ~ "Mild dry",
    SPI > -1.5 & SPI <= -1.0  ~ "Moderately dry",
    SPI > -2.0 & SPI <= -1.5  ~ "Very dry",
    SPI <= -2.0  ~ "Extremely dry"
  ))


## define a color palette

spi_colors <- c(
  "Extremely wet" = "#08306b",  # Dark blue
  "Very wet" = "#2171b5",       # Blue
  "Moderately wet" = "#6baed6", # Light blue
  "Mild wet" = "#c6dbef",            # Very light blue
  #"Neutral" = "gray70",         # Neutral gray
  "Mild dry" = "#fdcc8a",            # Very light orange
  "Moderately dry" = "#fc8d59", # Light orange
  "Very dry" = "#e34a33",       # Red-orange
  "Extremely dry" = "#b30000"   # Dark red
)


SPI_period_data$SPI_class <- factor(SPI_period_data$SPI_class, levels = names(spi_colors))

SPI_period_plot <- ggplot(SPI_period_data, aes(x = Period, y = SPI, fill = SPI_class)) +
  geom_col(width = 0.85) +  # Bar plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.35) +  # Reference line at 0
  facet_grid(Rain_Treat ~ Crop_season, labeller = labeller(Crop_season = c("2022-2023" = "2022-23", "2023-2024" = "2023-24"),
                                                           Rain_Treat = c("RR" = "RR", "AR" = "AR", "HR" = "HR", "PR" = "PR-N"))) +  # Facet grid: Year (columns), Rain_Treat (rows)
  scale_fill_manual(values = spi_colors) +  # Apply custom colors
  theme_bw() +
  labs(x = "Phase", y = "SPI (-)", fill = "SPI class") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90", linewidth = 0.35),
    axis.title.y = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 12),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12.5),
    legend.text = element_text(size = 12)) 


ggsave(SPI_period_plot, file=paste0("./SPI_period_plot.png"), path =vis_path, width = 17, height = 12, units = "cm", dpi = 600)



#### Prepare a summary data frame for all climatic variables ####

### With periodic SPI values ###

### data frame for overall season indices ###
Rain_exp_season_indices_wide <- Rain_exp_season_indices %>%
  pivot_longer(
    cols = -Crop_season,  # Keep Crop_season as it is
    names_to = c("Rain_Treat", "Variable"),  # Create two new columns
    names_sep = "_",  # Split names based on the underscore
    values_to = "Value"  # Column for values
  ) %>%
  pivot_wider(
    names_from = Variable,  # Use the 'Variable' column for column names
    values_from = Value     # Use the 'Value' column for the data
  ) %>%
  rename(Prec_R1 = Prec)



Exp_season_temp_indices_wide <- Exp_season_indices_long %>% filter(Variable %in% temperature_vars | Variable == "EHD_I30") %>%
  # Expand for all Rain_Treat levels (AR, RR, HR, PR)
  crossing(Rain_Treat = c("AR", "RR", "HR", "PR")) %>%
  # Pivot back to wide format, keeping 'Crop_season' and 'Rain_Treat'
  pivot_wider(
    names_from = Variable,
    values_from = Value
  )



SPI_period_data_wide <- SPI_period_data %>%
  select(Crop_season, Rain_Treat, Period, SPI) %>%
  pivot_wider(
    names_from = Period,  # Use the modified Period as the new column names
    values_from = SPI     # Spread the SPI values
  ) %>%
  rename_with(~ paste0(.x, "_SPI"), -c(Crop_season, Rain_Treat))  # Add _SPI suffix to the new columns


Summary_df_climatic_indices_SPI_period <- Rain_exp_season_indices_wide %>%
  left_join(SPI_period_data_wide %>% select(Crop_season, Rain_Treat, `P_S-E_SPI`, `P_E-V6_SPI`, `P_V6-V10_SPI`, `P_V10-R1_SPI`, `P_R1-R6_SPI`), 
            by = c("Crop_season", "Rain_Treat")) %>%
  left_join(Exp_season_temp_indices_wide, 
            by = c("Crop_season", "Rain_Treat"))


# Save the dataframe to an excel file
write_xlsx(Summary_df_climatic_indices_SPI_period, path = paste0(tables_path, "./Summary_df_climatic_indices_SPI_period_UZAIP.xlsx"))

### data frame for periode indices ###
Rain_exp_season_SPI_periods_indices_wide <- Rain_exp_season_periods_indices %>%
  pivot_longer(
    cols = -c(Crop_season, Period),  # Select relevant columns
    names_to = c("Rain_Treat", "Variable"),  # New column names
    names_pattern = "^(AR|RR|HR|PR)_(.*)$",  # Extract Rain_Treat and Variable correctly
    values_to = "Value"  # Set values column name
  ) %>%
  arrange(Crop_season, Period, Rain_Treat) %>%
  mutate(New_Var = paste0(Period, "_", Variable)) %>%
  select(-Period, -Variable) %>%
  pivot_wider(names_from = New_Var, values_from = Value) %>%
  ungroup %>%
  left_join(Rain_exp_season_indices_wide %>% select(Crop_season, Rain_Treat, Prec_R1), 
            by = c("Crop_season", "Rain_Treat")) %>%
  left_join(SPI_period_data_wide %>% select(Crop_season, Rain_Treat, `P_S-E_SPI`, `P_E-V6_SPI`, `P_V6-V10_SPI`, `P_V10-R1_SPI`, `P_R1-R6_SPI`), 
            by = c("Crop_season", "Rain_Treat"))



Summary_SPI_periods_df_climatic_indices <- Rain_exp_season_SPI_periods_indices_wide 


# Save the dataframe to a CSV file
write_xlsx(Summary_SPI_periods_df_climatic_indices, path = paste0(tables_path, "./Summary_SPI_periods_df_climatic_indices_UZAIP.xlsx"))






































