#### Script for weather data visualization Bouhenache at al. 2025 FCR ####

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

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, patchwork, here)

# Create the output folder path and ensure it exists
vis_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Figures")

## change system locale settings to English for x axis labels
Sys.setlocale("LC_ALL", "English")

#### Import and prepare data ####

# Import data with relative path
weather_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Seasonal_weather_dataset")

## data preparation

weather_data <- weather_data %>%
  #remove header description row
  slice(-1) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         Crop_season = as.factor(Crop_season)) %>%
  mutate(across(
    .cols = -c(date, Crop_season, Events), 
    .fns = ~ round(as.numeric(.), 1)
  ))



#### Summary stats ####
summary_stats <- weather_data %>%
  group_by(Crop_season) %>%
  summarise(
    # Numeric variables with summary stats
    Cumulative_seasonal_ambient_rainfall = max(Cum_season_rain_mm, na.rm = TRUE),
    Cumulative_seasonal_reduced_rainfall = max(Cum_season_reduc_rain_mm, na.rm = TRUE),
    Cumulative_seasonal_heavy_rainfall = max(Cum_season_heavy_rain_mm, na.rm = TRUE),
    Cumulative_seasonal_potential_rainfall = max(Cum_season_potential_rain_mm, na.rm = TRUE),
    Average_mean_temperature = mean(Air_mean_temperature_C, na.rm = TRUE),
    Average_min_temperature = mean(Air_min_temperature_C, na.rm = TRUE),
    Average_max_temperature = mean(Air_max_temperature_C, na.rm = TRUE),
    Min_temperature = min(Air_min_temperature_C, na.rm = TRUE),
    Max_temperature = max(Air_max_temperature_C, na.rm = TRUE))

## maximum temperature between mid-February and mid-April in 2023-24
avg_TMAX <- weather_data %>%
  filter(Crop_season == "2023-24", date >= as.Date("2024-02-15"), date <= as.Date("2024-04-15")) %>%
  summarise(mean_TMAX = mean(Air_max_temperature_C, na.rm = TRUE))

print(avg_TMAX)

## number of days exceeding 30 °C between mid-February and mid-April in 2023-24
days_above_30 <- weather_data %>%
  filter(Crop_season == "2023-24", date >= as.Date("2024-02-15"), date <= as.Date("2024-04-15"), Air_max_temperature_C > 30) %>%
  summarise(n_days = n())

print(days_above_30)




#### Plot visualization ####

## Create custom breaks
custom_breaks <- as.Date(c(
  "2022-11-15", "2022-11-30", "2022-12-15", "2022-12-30", "2023-01-15", "2023-01-30", 
  "2023-02-15", "2023-02-28", "2023-03-15", "2023-03-30", "2023-04-15", "2023-04-30",
  "2023-05-15", 
  "2023-11-15", "2023-11-30", "2023-12-15", "2023-12-30", "2024-01-15", "2024-01-30", 
  "2024-02-15", "2024-02-29", "2024-03-15", "2024-03-30", "2024-04-15", "2024-04-30",
  "2024-05-15"
))

# Create an empty list to store plots
plot_list <- list()

## create a vector for unique crop season
Uniq_CS_season <- unique(weather_data$Crop_season)

for (i in Uniq_CS_season) {
  seasonal_daily_weath_plot = ggplot(data = subset(weather_data, Crop_season == i), aes(x = date)) + 
    geom_bar(aes(y = Rain_mm, fill = "Daily ambient rainfall"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = Heavy_rain_events_mm, fill = "Heavy rainfall events"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = Potential_rain_rates_mm, fill = "Potential irrigation rates"), stat = "identity", position = "dodge") +
    geom_line(aes(y = Air_min_temperature_C, color = "Min air temperature"), size = 0.45) +
    geom_line(aes(y = Air_max_temperature_C, color = "Max air temperature"), size = 0.45) +
    geom_line(aes(y = Cum_season_rain_mm/10, color = "Cumulative ambient rainfall"), size = 0.45) +
    geom_line(aes(y = Cum_season_reduc_rain_mm/10, color = "Cumulative reduced rainfall"), size = 0.45, linetype = "longdash") +
    geom_line(aes(y = Cum_season_heavy_rain_mm/10, color = "Cumulative heavy rainfall"), size = 0.45, linetype = "longdash") +
    geom_line(aes(y = Cum_season_potential_rain_mm/10, color = "Cumulative potential rainfall"), size = 0.45, linetype = "longdash") +
    geom_segment(data = subset(weather_data, Crop_season == i & !is.na(Events)), aes(x = date, y = 0, xend = date, yend = 103), color = "grey20", linetype = "dashed", size = 0.35) +   # Add vertical lines
    geom_text(data = subset(weather_data, Crop_season == i & !is.na(Events) & Events != "E"),
              aes(x = date, y = 103, label = Events), vjust = -0.5, hjust = 0.5, size = 3.3, color = "grey20") +  
    geom_text(data = subset(weather_data, Crop_season == i & !is.na(Events) & Events == "E"),
              aes(x = date, y = 103, label = Events), vjust = -0.5, hjust = 0, size = 3.3, color = "grey20") +  # Add labels to vertical lines# Add labels to vertical lines
    facet_grid(~ year, 
               scales = "free_x",
               space = "free_x",
               switch = "x")+
    theme_bw() +
    
    ## axis y
    scale_y_continuous(
      # Features of the first axis
      name = "Daily rainfall (mm) and air temperature (°C)",
      breaks=c(seq(0,110,10)),
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(trans = ~.*10, breaks = seq(0, 1100, by = 100), name = "Cumulative rainfall (mm)"),
      limits = c(0, 110),
      expand = c(0, 0)) +
    
    ## axis x
    
    scale_x_date(name = "Date" ,breaks = custom_breaks, date_labels = "%b %d", expand = c(0, 0)) +
    
    
    ## theme
    theme(panel.spacing.x = unit(0, units = "cm"), # removes space between panels
          strip.placement = "outside", # moves the states down
          strip.text = element_text(size = 11.5),
          ## border
          #bornes et grille
          ## border
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          ## y right axis aesthetics
          axis.line.y.right = element_line(color = "black"), 
          axis.ticks.y.right = element_line(color = "black"),
          axis.text.y.right = element_text(color = "black", size = 10.5), 
          axis.title.y.right = element_text(color = "black", size = 11.5),
          ## y left axis aesthetics
          axis.line.y.left = element_line(color = "black"), 
          axis.ticks.y.left = element_line(color = "black"),
          axis.text.y.left = element_text(color = "black", size = 10.5), 
          axis.title.y.left = element_text(color = "black", size = 11.5),
          ## x axis aesthetics
          axis.line.x = element_line(color = "black"), 
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 10, color = "black"),
          axis.title.x = element_text(size = 11.5),
          ## facet borders
          panel.border = element_blank(),
          
          ## Legend settings
          legend.key.size = unit(0.55, "cm"),  # Adjust legend item size
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10.5),
          # legend position
          legend.position = "bottom",
          plot.margin = margin(t = 30)) +
    
    scale_fill_manual(values = c(
      "Daily ambient rainfall" = 'blue',
      "Heavy rainfall events" = 'orange',
      "Potential irrigation rates" = 'black')) +
    
    scale_color_manual(values = c(
      "Min air temperature" = 'green',
      "Max air temperature" = 'red',
      "Cumulative ambient rainfall" = "blue",
      "Cumulative reduced rainfall" = "grey",
      "Cumulative heavy rainfall" = "orange",
      "Cumulative potential rainfall" = "black")) +
    
    guides(fill = guide_legend(title = "Legend", nrow = 3, order = 1),
           color = guide_legend(title = NULL, nrow = 3, order = 2)) 
  
  # Store the plot in a list
  plot_list[[i]] <- seasonal_daily_weath_plot
}


# Combine the plots side by side
final_plot <- wrap_plots(plot_list) + plot_layout(ncol = 1, guides = "collect") & 
  theme(legend.position = "bottom") 


ggsave(final_plot, file = paste0(vis_path, "/daily_weather_plot.png"), width = 18.5, height = 23, units = "cm", dpi = 600)



