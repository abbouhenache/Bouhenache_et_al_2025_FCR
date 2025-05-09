#### Script for summary tables and plot visualizations Bouhenache at al. 2025 FCR ####

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
pacman::p_load(tidyverse, readxl, readr, here, openxlsx, ggbreak, ggh4x,
               patchwork, cowplot, grid)


# Create a path for saving figures
vis_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Figures")

# Create a path for saving figures
tables_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Tables")

## change system locale settings to English for x axis lab
Sys.setlocale("LC_ALL", "English")


#### Import and prepare data ####

# TAGB data 
AGB_raw_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "TAGB_N_data")[-1, ]


AGB_data <- AGB_raw_data %>%
  # drop non necessary variable of dry weight percentage
  select(c(Crop_season:Nitrogen, AGB_DM_t_ha, Harvest_index, AGB_N_uptake_kg_ha, N_harvest_index, Tot_N_AGB, Leaves_DM_t_ha)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate_at(vars(AGB_DM_t_ha, Harvest_index, AGB_N_uptake_kg_ha, N_harvest_index, Tot_N_AGB, Leaves_DM_t_ha), as.numeric) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Sampling_stage = factor(Sampling_stage, levels = c("V6", "V10", "R1", "R6"))) %>%
  mutate(Rain_Treat = factor(Rain_Treat, levels = c("RR", "AR", "HR", "PR"))) %>%
  filter(!Rain_Treat =="PR") %>%
  droplevels()


# Grain yield data 
GY_raw_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Grain_yield_data")[-1, ]

GY_data <- GY_raw_data %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(across(Numb_ears_plant:G_yield_t_ha_moist_standard, ~ round(as.numeric(.), 2))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Sampling_stage = factor(Sampling_stage, levels = c("V6", "V10", "R1", "R6"))) %>%
  mutate(Rain_Treat = factor(Rain_Treat, levels = c("RR", "AR", "HR", "PR"))) %>%
  filter(!Rain_Treat =="PR") %>%
  droplevels()


## create a function for standar error
se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(n())
}


#### Summary tables ####

### AGB variables ####

### Rain_treat
Rain_summary_AGB <- AGB_data %>%
  group_by(Crop_season, Sampling_stage, Rain_Treat) %>%
  summarise(across(c("AGB_DM_t_ha":"N_harvest_index"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))



### Mulch

Mulch__summary_AGB <- AGB_data %>%
  group_by(Crop_season, Sampling_stage, Mulch) %>%
  summarise(across(c("AGB_DM_t_ha":"N_harvest_index"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))



### Nitrogen
Nitrogen_summary_AGB <- AGB_data %>%
  group_by(Crop_season, Sampling_stage, Nitrogen) %>%
  summarise(across(c("AGB_DM_t_ha":"N_harvest_index"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))


### grain yield variables ####

### Rain_treat
Rain_summary_GY <- GY_data %>%
  group_by(Crop_season, Sampling_stage, Rain_Treat) %>%
  summarise(across(c("Numb_ears_plant":"G_yield_t_ha_moist_standard"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))


### Mulch
Mulch_summary_GY <- GY_data %>%
  group_by(Crop_season, Sampling_stage, Mulch) %>%
  summarise(across(c("Numb_ears_plant":"G_yield_t_ha_moist_standard"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))


### Nitrogen
Nitrogen_summary_GY <- GY_data %>%
  group_by(Crop_season, Sampling_stage, Nitrogen) %>%
  summarise(across(c("Numb_ears_plant":"G_yield_t_ha_moist_standard"), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~se(.)),
                   .names = "{.col}_{fn}"),
            .groups = "drop")  %>%
  
  # round all numeric variable to 2 digits
  mutate(across(where(is.numeric), round, 2))



## save tables in an excel file 
summary_dfs <- ls(pattern = "_summary_")

#filter only data frames
summary_dfs <- Filter(function(x) is.data.frame(get(x)), summary_dfs)

# Categorize based on naming pattern
Rain_dfs  <- summary_dfs[grepl("Rain_", summary_dfs)]
Mulch_dfs <- summary_dfs[grepl("Mulch_", summary_dfs)]
Nitrogen_dfs <- summary_dfs[grepl("Nitrogen_", summary_dfs)]

# Combine in desired order
ordered_dfs <- c(Rain_dfs, Mulch_dfs, Nitrogen_dfs)

# Create workbook
wb <- createWorkbook()

# Add sheets in the ordered list
for (df_name in ordered_dfs) {
  addWorksheet(wb, sheetName = substr(df_name, 1, 31))  # Excel limit
  writeData(wb, sheet = df_name, x = get(df_name))
}

#save workbook
saveWorkbook(wb, file = file.path(tables_path, "Summary_tables.xlsx"), overwrite = TRUE)




#### Plot visualization ####

## Import all tukey tables 

# Path to the Excel file
file_path <- here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx")

# Get sheet names
sheet_names <- excel_sheets(file_path)

# Filter only those starting with "Tukey_"
tukey_sheets <- sheet_names[startsWith(sheet_names, "Tukey_")]

# Read and store each sheet into a named list
tukey_list <- map(tukey_sheets, ~ read_xlsx(file_path, sheet = .x)) %>%
  set_names(tukey_sheets)


### Harvest stage ####

### Rain x N interactions ###

## AGB ##

# extract PR-N value

PM_TAGB_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024", Sampling_stage == "R6") %>%
  pull(AGB_DM_t_ha) %>%
  as.numeric()

AGB_data_HVST <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(AGB_DM_t_ha_mean = mean(AGB_DM_t_ha),
            AGB_DM_t_ha_se = se(AGB_DM_t_ha)) 
  

# Extract and prepare the Tukey groups
Tukey_df_AGB_HVST <- tukey_list$Tukey_AGB_HVST_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(AGB_data_HVST$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(AGB_data_HVST$Nitrogen))
  )

# Join Tukey group labels
AGB_data_HVST <- AGB_data_HVST %>%
  left_join(Tukey_df_AGB_HVST, by = c("Rain_Treat", "Nitrogen"))



TAGB_bar_plot <- ggplot(AGB_data_HVST, aes(x = Rain_Treat, y = AGB_DM_t_ha_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = AGB_DM_t_ha_mean - AGB_DM_t_ha_se, 
                    ymax = AGB_DM_t_ha_mean + AGB_DM_t_ha_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_TAGB_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = AGB_DM_t_ha_mean + AGB_DM_t_ha_se + 0.5),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2),
                     expand = c(0,0),
                     name = expression(paste("TAGB (t DM ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(6.2, 11), expand = FALSE) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, hjust = 0.52),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(TAGB_bar_plot)


## N accumulation AGB ##

PM_N_upt_TAGB_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024", Sampling_stage == "R6") %>%
  pull(AGB_N_uptake_kg_ha) %>%
  as.numeric()

AGB_N_upt_data_Rain_N_HVST <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(AGB_N_uptake_kg_ha_mean = mean(AGB_N_uptake_kg_ha),
            AGB_N_uptake_kg_ha_se = se(AGB_N_uptake_kg_ha))

# Extract and prepare the Tukey groups
Tukey_df_AGB_N_upt_Rain_N_HVST <- tukey_list$Tukey_AGB_N_upt_HVST_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(AGB_N_upt_data_Rain_N_HVST$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(AGB_N_upt_data_Rain_N_HVST$Nitrogen))
  )

# Join Tukey group labels
AGB_N_upt_data_Rain_N_HVST <- AGB_N_upt_data_Rain_N_HVST %>%
  left_join(Tukey_df_AGB_N_upt_Rain_N_HVST, by = c("Rain_Treat", "Nitrogen"))


N_TAGB_Rain_N_bar_plot <- ggplot(AGB_N_upt_data_Rain_N_HVST, aes(x = Rain_Treat, y = AGB_N_uptake_kg_ha_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = AGB_N_uptake_kg_ha_mean - AGB_N_uptake_kg_ha_se, 
                    ymax = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_N_upt_TAGB_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se + 3),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10),
                     expand = c(0,0),
                     name = expression(paste("N accumulated in TAGB (kg N ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(55, 140), expand = FALSE) +
  
  guides(y = guide_axis_truncated(
    trunc_lower = c(-Inf, 150),
    trunc_upper = c(140, Inf)
  )) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, hjust = 0.52),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(N_TAGB_Rain_N_bar_plot)


## Harvest index ##

## extract PM value for HI
PM_HI_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  pull(Harvest_index) %>%
  as.numeric()


HI_data <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(Harvest_index_mean = mean(Harvest_index),
            Harvest_index_se = se(Harvest_index))


# Extract and prepare the Tukey groups
Tukey_df_HI <- tukey_list$Tukey_HI_HVST_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(HI_data$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(HI_data$Nitrogen))
  )

# Join Tukey group labels
HI_data <- HI_data %>%
  left_join(Tukey_df_HI, by = c("Rain_Treat", "Nitrogen"))



HI_bar_plot <- ggplot(HI_data, aes(x = Rain_Treat, y = Harvest_index_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = Harvest_index_mean - Harvest_index_se, 
                    ymax = Harvest_index_mean + Harvest_index_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_HI_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = Harvest_index_mean + Harvest_index_se + 0.02),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.1),
                     expand = c(0,0),
                     name = "Harvest index (-)") +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13, margin = margin(0,15,0,0)),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, hjust = 0.48, margin = margin(11,0,0,0)),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13)) 

print(HI_bar_plot)


## Grain yield ##

## extract PM value for HI
PM_GY_24 <- GY_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  pull(G_yield_t_ha_moist_standard) %>%
  as.numeric()


GY_Rain_N_data <- GY_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(G_yield_t_ha_moist_standard_mean = mean(G_yield_t_ha_moist_standard),
            G_yield_t_ha_moist_standard_se = se(G_yield_t_ha_moist_standard))


# Extract and prepare the Tukey groups
Tukey_df_GY_Rain_N <- tukey_list$Tukey_GY_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(GY_Rain_N_data$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(GY_Rain_N_data$Nitrogen))
  )

# Join Tukey group labels
GY_Rain_N_data <- GY_Rain_N_data %>%
  left_join(Tukey_df_GY_Rain_N, by = c("Rain_Treat", "Nitrogen"))




GY_Rain_N_bar_plot <- ggplot(GY_Rain_N_data, aes(x = Rain_Treat, y = G_yield_t_ha_moist_standard_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = G_yield_t_ha_moist_standard_mean - G_yield_t_ha_moist_standard_se, 
                    ymax = G_yield_t_ha_moist_standard_mean + G_yield_t_ha_moist_standard_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_GY_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = G_yield_t_ha_moist_standard_mean + G_yield_t_ha_moist_standard_se + 0.1),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5),
                     expand = c(0,0),
                     name = expression(paste("Grain yield (t ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(1.85, 4.5), expand = FALSE) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, hjust = 0.52),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(GY_Rain_N_bar_plot)


### Rain x M interactions ### 

## N accumulation AGB ##

AGB_N_upt_data_Rain_M_HVST <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Mulch) %>%
  summarise(AGB_N_uptake_kg_ha_mean = mean(AGB_N_uptake_kg_ha),
            AGB_N_uptake_kg_ha_se = se(AGB_N_uptake_kg_ha))

# Extract and prepare the Tukey groups
Tukey_df_AGB_N_upt_Rain_M_HVST <- tukey_list$Tukey_AGB_N_upt_HVST_Rain_M_24 %>%
  select(Rain_Treat, Mulch, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(AGB_N_upt_data_Rain_M_HVST$Rain_Treat)),
    Mulch = factor(Mulch, levels = levels(AGB_N_upt_data_Rain_M_HVST$Mulch))
  )

# Join Tukey group labels
AGB_N_upt_data_Rain_M_HVST <- AGB_N_upt_data_Rain_M_HVST %>%
  left_join(Tukey_df_AGB_N_upt_Rain_M_HVST, by = c("Rain_Treat", "Mulch"))


N_TAGB_Rain_M_bar_plot <- ggplot(AGB_N_upt_data_Rain_M_HVST, aes(x = Rain_Treat, y = AGB_N_uptake_kg_ha_mean, fill = Mulch)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = AGB_N_uptake_kg_ha_mean - AGB_N_uptake_kg_ha_se, 
                    ymax = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_N_upt_TAGB_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se + 3),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10),
                     expand = c(0,0),
                     name = expression(paste("N accumulation in TAGB (kg N ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(46, 140), expand = FALSE) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, hjust = 0.52),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(N_TAGB_Rain_M_bar_plot)


## Grain yield ##

GY_Rain_M_data <- GY_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R6") %>%
  group_by(Crop_season, Rain_Treat, Mulch) %>%
  summarise(G_yield_t_ha_moist_standard_mean = mean(G_yield_t_ha_moist_standard),
            G_yield_t_ha_moist_standard_se = se(G_yield_t_ha_moist_standard))


# Extract and prepare the Tukey groups
Tukey_df_GY_Rain_M <- tukey_list$Tukey_GY_Rain_M_24 %>%
  select(Rain_Treat, Mulch, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(GY_Rain_M_data$Rain_Treat)),
    Mulch = factor(Mulch, levels = levels(GY_Rain_M_data$Mulch))
  )

# Join Tukey group labels
GY_Rain_M_data <- GY_Rain_M_data %>%
  left_join(Tukey_df_GY_Rain_M, by = c("Rain_Treat", "Mulch"))


GY_Rain_M_bar_plot <- ggplot(GY_Rain_M_data, aes(x = Rain_Treat, y = G_yield_t_ha_moist_standard_mean, fill = Mulch)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = G_yield_t_ha_moist_standard_mean - G_yield_t_ha_moist_standard_se, 
                    ymax = G_yield_t_ha_moist_standard_mean + G_yield_t_ha_moist_standard_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_GY_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = G_yield_t_ha_moist_standard_mean + G_yield_t_ha_moist_standard_se + 0.1),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5),
                     expand = c(0,0),
                     name = expression(paste("Grain yield (t ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(1.8, 4.5), expand = FALSE) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(GY_Rain_M_bar_plot)


### combine all plots in one ###

# Remove legends from individual plots
TAGB_bar_plot_nolegend <- TAGB_bar_plot + theme(legend.position = "none")
N_TAGB_Rain_N_bar_plot_nolegend <- N_TAGB_Rain_N_bar_plot + theme(legend.position = "none")
N_TAGB_Rain_M_bar_plot_nolegend <- N_TAGB_Rain_M_bar_plot + theme(legend.position = "none")


# Remove legends from individual plots
GY_Rain_N_bar_plot_nolegend <- GY_Rain_N_bar_plot + theme(legend.position = "none")
HI_bar_plot_nolegend <- HI_bar_plot + theme(legend.position = "none")
GY_Rain_M_bar_plot_nolegend <- GY_Rain_M_bar_plot + theme(legend.position = "none")

# Extract legend from one of the original plots (before removing legend)
legend_1 <- ggpubr::get_legend(GY_Rain_N_bar_plot + theme(legend.position = "top"))
legend_2 <- ggpubr::get_legend(GY_Rain_M_bar_plot + theme(legend.position = "top"))


# Define layout manually
layout <- c(
  area(t = 2, l = 1, b = 59, r = 4),  
  area(t = 1, l = 5, b = 60, r = 12)  
)

# Combine plots using the custom layout
N_up_plot <- (N_TAGB_Rain_N_bar_plot_nolegend + N_TAGB_Rain_M_bar_plot_nolegend) 

final_plot <- (TAGB_bar_plot_nolegend + N_up_plot) +
  plot_layout(design = layout) 

final_plot <- ggdraw() + 
  draw_plot(final_plot, x = 0, y = 0, width = 1, height = 1) +  # Adjust plot width
  theme(plot.background = element_rect(fill = "white", color = NA))  # Set background to white

final_plot_labeled_1 <- ggdraw(final_plot) +
  draw_plot_label(label = c("(a)", "(b)", "(c)"), 
                  x = c(0.17, 0.51, 0.82),  # Adjust x-positions
                  y = c(1.01, 1.01, 1.01),  # Adjust y-positions
                  size = 14, fontface = "bold")


# Define layout manually
layout <- c(
  area(t = 1, l = 1, b = 60, r = 8),  
  area(t = 2, l = 9, b = 59, r = 12)  
)

# Combine plots using the custom layout
Rain_N_plot_2 <- (GY_Rain_N_bar_plot_nolegend + HI_bar_plot_nolegend) 

final_plot_2 <- (Rain_N_plot_2 + GY_Rain_M_bar_plot_nolegend ) +
  plot_layout(design = layout) 

final_plot_2 <- ggdraw() + 
  draw_plot(final_plot_2, x = 0, y = 0.08, width = 1, height = 0.9) +  # Adjust plot width
  draw_plot(legend_1, x = 0.25, y = 0, width = 0.2, height = 0.1) +  # Reduced width for legend
  draw_plot(legend_2, x = 0.75, y = 0, width = 0.2, height = 0.1) +  # Reduced width for legend
  theme(plot.background = element_rect(fill = "white", color = NA))  # Set background to white


final_plot_labeled_2 <- ggdraw(final_plot_2) +
  draw_plot_label(label = c("(d)", "(e)", "(f)"), 
                  x = c(0.17, 0.51, 0.82),  # Adjust x-positions
                  y = c(1.01, 1.01, 1.01),  # Adjust y-positions
                  size = 14, fontface = "bold")

## Combine the 6 plots for the article

Final_combined_plot_HVST <-  final_plot_labeled_1 / final_plot_labeled_2 + 
  plot_layout(heights = c(1, 1.1))  

# Save the final plot
ggsave(Final_combined_plot_HVST, file = paste0("./Rain_N_interaction_effects_HVST_plots.png"),
       path = vis_path, width = 25, height = 18, units = "cm", dpi = 600)
















### Flowering stage ####

### Rain x N interactions ###

## AGB ##

# extract PR-N value

PM_TAGB_FLW_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024", Sampling_stage == "R1") %>%
  pull(AGB_DM_t_ha) %>%
  as.numeric()

AGB_data_FLW <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R1") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(AGB_DM_t_ha_mean = mean(AGB_DM_t_ha),
            AGB_DM_t_ha_se = se(AGB_DM_t_ha)) 



TAGB_Rain_N_FLW_bar_plot <- ggplot(AGB_data_FLW, aes(x = Rain_Treat, y = AGB_DM_t_ha_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = AGB_DM_t_ha_mean - AGB_DM_t_ha_se, 
                    ymax = AGB_DM_t_ha_mean + AGB_DM_t_ha_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_TAGB_FLW_24, linetype = "dashed", color = "black") +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 15, by = 2.5),
                     expand = c(0,0),
                     name = expression(paste("TAGB (t DM ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(7.5, 14.5), expand = FALSE) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 

print(TAGB_Rain_N_FLW_bar_plot)


## N accumulation AGB ##

PM_N_upt_TAGB_FLW_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024", Sampling_stage == "R1") %>%
  pull(AGB_N_uptake_kg_ha) %>%
  as.numeric()

AGB_N_upt_data_Rain_N_FLW <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R1") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(AGB_N_uptake_kg_ha_mean = mean(AGB_N_uptake_kg_ha),
            AGB_N_uptake_kg_ha_se = se(AGB_N_uptake_kg_ha))

# Extract and prepare the Tukey groups
Tukey_df_AGB_N_upt_Rain_N_FLW <- tukey_list$Tukey_AGB_N_upt_FLW_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(AGB_N_upt_data_Rain_N_FLW$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(AGB_N_upt_data_Rain_N_FLW$Nitrogen))
  )

# Join Tukey group labels
AGB_N_upt_data_Rain_N_FLW <- AGB_N_upt_data_Rain_N_FLW %>%
  left_join(Tukey_df_AGB_N_upt_Rain_N_FLW, by = c("Rain_Treat", "Nitrogen"))


N_TAGB_Rain_N_FLW_bar_plot <- ggplot(AGB_N_upt_data_Rain_N_FLW, aes(x = Rain_Treat, y = AGB_N_uptake_kg_ha_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = AGB_N_uptake_kg_ha_mean - AGB_N_uptake_kg_ha_se, 
                    ymax = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_N_upt_TAGB_FLW_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = AGB_N_uptake_kg_ha_mean + AGB_N_uptake_kg_ha_se + 4),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 25),
                     expand = c(0,0),
                     name = expression(paste("N accumulated in TAGB (kg N ", ha^{-1}, ")"))) +
  
  # Apply y-axis break
  scale_y_break(c(80, 225), expand = FALSE) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 


print(N_TAGB_Rain_N_FLW_bar_plot)



## N concentration AGB ##

PM_N_conc_TAGB_FLW_24 <- AGB_raw_data %>%
  filter(Rain_Treat == "PR", Crop_season == "2023-2024", Sampling_stage == "R1") %>%
  pull(Tot_N_AGB) %>%
  as.numeric()

AGB_N_conc_data_Rain_N_FLW <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage == "R1") %>%
  group_by(Crop_season, Rain_Treat, Nitrogen) %>%
  summarise(Tot_N_AGB_mean = mean(Tot_N_AGB),
            Tot_N_AGB_se = se(Tot_N_AGB))

# Extract and prepare the Tukey groups
Tukey_df_AGB_N_conc_Rain_N_FLW <- tukey_list$Tukey_AGB_N_conc_FLW_Rain_N_24 %>%
  select(Rain_Treat, Nitrogen, Tukey = Group_label) %>%
  mutate(
    Rain_Treat = factor(Rain_Treat, levels = levels(AGB_N_conc_data_Rain_N_FLW$Rain_Treat)),
    Nitrogen = factor(Nitrogen, levels = levels(AGB_N_conc_data_Rain_N_FLW$Nitrogen))
  )

# Join Tukey group labels
AGB_N_conc_data_Rain_N_FLW <- AGB_N_conc_data_Rain_N_FLW %>%
  left_join(Tukey_df_AGB_N_conc_Rain_N_FLW, by = c("Rain_Treat", "Nitrogen"))


N_conc_TAGB_Rain_N_FLW_bar_plot <- ggplot(AGB_N_conc_data_Rain_N_FLW, aes(x = Rain_Treat, y = Tot_N_AGB_mean, fill = Nitrogen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.64) +  # Points with different shapes/colors for Mulch
  geom_errorbar(aes(ymin = Tot_N_AGB_mean - Tot_N_AGB_se, 
                    ymax = Tot_N_AGB_mean + Tot_N_AGB_se), 
                position = position_dodge(width = 0.7),
                width = 0.25) +  # Error bars
  
  geom_hline(yintercept = PM_N_conc_TAGB_FLW_24, linetype = "dashed", color = "black") +
  
  # Add Tukey annotation above bars
  geom_text(aes(label = Tukey, 
                y = Tot_N_AGB_mean + Tot_N_AGB_se + 0.6),  # Position slightly above error bars
            position = position_dodge(width = 0.7),
            size = 4.8) +
  
  #theme
  theme_bw() +
  
  # y axis
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 15, by = 3),
                     expand = c(0,0),
                     name = expression(paste("N concentration in TAGB (g ", kg^{-1}, ")"))) +
  
  scale_fill_manual(values = c("N0" = "#E69F00", "N80" = "#117733")) +
  
  labs(x = "Rainfall Treatment") +
  
  # theme elements
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13, margin = margin(t = 10)),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        
        axis.ticks.y.right = element_blank(),  # Remove right-side y-axis ticks
        axis.text.y.right = element_blank()) 


print(N_conc_TAGB_Rain_N_FLW_bar_plot)



### combine the plots ###

# Remove legends from individual plots
TAGB_Rain_N_FLW_bar_plot_nolegend <- TAGB_Rain_N_FLW_bar_plot + theme(legend.position = "none")
N_conc_TAGB_Rain_N_FLW_bar_plot_nolegend <- N_conc_TAGB_Rain_N_FLW_bar_plot + theme(legend.position = "none")
N_TAGB_Rain_N_FLW_bar_plot_nolegend <- N_TAGB_Rain_N_FLW_bar_plot + theme(legend.position = "none")

# Extract legend from one of the original plots (before removing legend)
legend <- ggpubr::get_legend(TAGB_Rain_N_FLW_bar_plot + theme(legend.position = "top"))

# Define layout manually
layout_1 <- c(
  area(t = 2, l = 1, b = 60, r = 29),  
  area(t = 1, l = 30, b = 60, r = 60)  
)

# Combine plots using the custom layout
N_plot <- (N_conc_TAGB_Rain_N_FLW_bar_plot_nolegend + N_TAGB_Rain_N_FLW_bar_plot_nolegend) +
  plot_layout(design = layout_1) 

# Define layout manually
layout_2 <- c(
  area(t = 4, l = 1, b = 238, r = 4),  
  area(t = 1, l = 5, b = 240, r = 12)  
)

final_plot <- (TAGB_Rain_N_FLW_bar_plot_nolegend + N_plot) +
  plot_layout(design = layout_2) 

final_plot <- ggdraw() + 
  draw_plot(final_plot, x = 0, y = 0.08, width = 1, height = 0.9) +  # Adjust plot width
  draw_plot(legend, x = 0, y = 0, width = 1, height = 0.1) +  # Reduced width for legend
  theme(plot.background = element_rect(fill = "white", color = NA))  # Set background to white


final_plot_labeled <- ggdraw(final_plot) +
  draw_plot_label(label = c("(a)", "(b)", "(c)"), 
                  x = c(0.01, 0.34, 0.66),  # Adjust x-positions
                  y = c(1.01, 1.01, 1.01),  # Adjust y-positions
                  size = 14, fontface = "bold")

# Save the final plot
ggsave(final_plot_labeled, file = paste0("./Rain_N_interaction_effects_FLW_plots.png"),
       path = vis_path, width = 28, height = 10, units = "cm", dpi = 600)




#### Biomass loss between flowering and harvest in 2023-24 ####

#### Plot for biomass loss between flowering and harvest ####

TAGB_loss_24 <- AGB_data %>%
  filter(Crop_season == "2023-2024" & Sampling_stage %in% c("R1","R6")) %>%
  select(Crop_season, Sampling_stage, Replicate, Rain_Treat, Mulch, Nitrogen, Leaves_DM_t_ha, AGB_DM_t_ha) %>%
  pivot_wider(names_from = Sampling_stage, values_from = c(Leaves_DM_t_ha, AGB_DM_t_ha)) %>%
  mutate(
    Leaves_DM_loss = Leaves_DM_t_ha_R6 - Leaves_DM_t_ha_R1,
    AGB_DM_loss = AGB_DM_t_ha_R6 - AGB_DM_t_ha_R1
  ) %>%
  select(Crop_season:Nitrogen, Leaves_DM_loss, AGB_DM_loss) %>% # Keep relevant variables 
  group_by(Crop_season, Rain_Treat) %>%
  summarise(Leaves_DM_loss_mean = mean(Leaves_DM_loss),
            Leaves_DM_loss_se = se(Leaves_DM_loss),
            AGB_DM_loss_mean = mean(AGB_DM_loss),
            AGB_DM_loss_se = se(AGB_DM_loss))



TAGB_loss_24_long <- TAGB_loss_24 %>%
  pivot_longer(cols = c(AGB_DM_loss_mean, Leaves_DM_loss_mean),
               names_to = "Variable",
               values_to = "Mean_Loss") %>%
  pivot_longer(cols = c(AGB_DM_loss_se, Leaves_DM_loss_se),
               names_to = "SE_Variable",
               values_to = "SE") %>%
  filter((Variable == "AGB_DM_loss_mean" & SE_Variable == "AGB_DM_loss_se") |
           (Variable == "Leaves_DM_loss_mean" & SE_Variable == "Leaves_DM_loss_se"))


# Plot with error bars
TAGB_loss_2024 <- ggplot(TAGB_loss_24_long, aes(x = Rain_Treat, y = Mean_Loss, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +  # Side-by-side bars
  geom_errorbar(aes(ymin = Mean_Loss - SE, ymax = Mean_Loss + SE),
                position = position_dodge(0.7), width = 0.2) +  # Error bars
  scale_fill_manual(values = c("AGB_DM_loss_mean" = "green4", "Leaves_DM_loss_mean" = "yellow3"),
                    labels = c("TAGB loss", "Leaves loss")) +  # Custom colors
  labs(x = "Rainfall Treatment",fill = "") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0)), 
                     name = expression(paste("Biomass loss (t DM ", ha^{-1}, ")"))) +  # Ensures space at top
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=13),
        legend.position = "top")


# Save the final plot
ggsave(TAGB_loss_2024, file = paste0("./TAGB_loss_2024.png"),
       path = vis_path, width = 12, height = 9, units = "cm", dpi = 600)



