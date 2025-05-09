#### Script for random forest and partial dependence plots Bouhenache at al. 2025 FCR ####

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
pacman::p_load(here, readxl, tidyverse, lubridate, randomForest, pdp, patchwork, 
               ggcorrplot, cowplot, car, scales)


# Create a path for saving figures
vis_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Figures")

# Create a path for saving figures
tables_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Tables")

## change system locale settings to English for x axis labels
Sys.setlocale("LC_ALL", "English")


#### Import and prepare data ####

## climatic indices data

Climatic_indices_SPI_period <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Seasonal_climatic_indices")

Climatic_periods_indices_SPI_period <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Periodic_climatic_indices")


## TAGB and grain yield data 

# TAGB data 
AGB_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "TAGB_N_data")[-1, ]

AGB_data <- AGB_data %>%
  # drop non necessary variable of dry weight percentage
  select(c(Crop_season:Nitrogen, AGB_DM_t_ha)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate_at(vars(AGB_DM_t_ha), as.numeric) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels() 

AGB_Bmean_hvst_data <- AGB_data %>%
  filter(Sampling_stage == "R6") %>%
  select(Crop_season, Replicate, Rain_Treat, Mulch, Nitrogen, 
         AGB_DM_t_ha) %>%
  group_by(Crop_season, Rain_Treat, Mulch, Nitrogen) %>%
  summarize(AGB_DM_t_ha = mean(AGB_DM_t_ha, na.rm = TRUE)) %>%
  
  mutate(
    Nitrogen = as.numeric(dplyr::recode(Nitrogen, 
                                        "N0" = 0, 
                                        "N80" = 80, 
                                        "N160" = 160)),
    Mulch = as.numeric(dplyr::recode(Mulch, 
                                     "M0" = 0, 
                                     "M6" = 6))
  )


# grain yield data 

GY_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Grain_yield_data")[-1, ]

GY_Bmean_hvst_data <- GY_data %>%
  mutate_at(vars(c(G_yield_t_ha_moist_standard)), as.numeric) %>%
  mutate_at(vars(c(Crop_season:Mulch)), as.factor) %>%
  select(Crop_season, Replicate, Rain_Treat, Mulch, Nitrogen,
         G_yield_t_ha_moist_standard) %>%
  group_by(Crop_season, Rain_Treat, Mulch, Nitrogen) %>%
  summarize(G_yield_t_ha_moist_standard = mean(G_yield_t_ha_moist_standard, na.rm = TRUE)) %>%
  
  mutate(
    Nitrogen = as.numeric(dplyr::recode(Nitrogen, 
                                        "N0" = 0, 
                                        "N80" = 80, 
                                        "N160" = 160)),
    Mulch = as.numeric(dplyr::recode(Mulch, 
                                     "M0" = 0, 
                                     "M6" = 6))
  )


## Joint Grain yield data at standard commercial moisture to AGB data
AGB_Bmean_hvst_data <- AGB_Bmean_hvst_data %>%
  left_join(GY_Bmean_hvst_data, by = c("Crop_season", "Rain_Treat", "Mulch", "Nitrogen"))

### Prepare inputs data

Input_data <- AGB_Bmean_hvst_data %>%
  left_join(Climatic_indices_SPI_period, by = c("Crop_season", "Rain_Treat")) %>%
  relocate(Mulch, .after = G_yield_t_ha_moist_standard) %>%
  relocate(Nitrogen, .after = Mulch) %>%
  ungroup


Input_periods_data <- AGB_Bmean_hvst_data %>%
  left_join(Climatic_periods_indices_SPI_period, by = c("Crop_season", "Rain_Treat")) %>%
  relocate(Mulch, .after = G_yield_t_ha_moist_standard) %>%
  relocate(Nitrogen, .after = Mulch) %>%
  ungroup



#### Multicollinearity test to remove redundant variables ####

Corr_data_season <- Input_data %>%
  select(-c(`P_S-E_SPI`, TN:TX99p))

## Add periodic CDD 
Corr_data_season <- Corr_data_season %>%
  left_join(Input_periods_data %>% select(Crop_season, Rain_Treat, Mulch, Nitrogen, `P_V6-V10_CDD`, `P_V10-R1_CDD`), 
            by = c("Crop_season", "Rain_Treat", "Nitrogen", "Mulch")) 


## select only rain variables at season level
rain_variables <- Corr_data_season %>% 
  select(-c(Crop_season:Nitrogen)) %>%
  select(RX1day, RX5day, R99p, Rnn99p, SDII, PRCPTOT, Prec_R1) 


cor_matrix <- cor(rain_variables, use = "complete.obs", method = "pearson")
rownames(cor_matrix) <- gsub("flw", "R1", rownames(cor_matrix))  
colnames(cor_matrix) <- gsub("flw", "R1", colnames(cor_matrix))
# Create correlation matrix plot
Pearson_plot <- ggcorrplot(cor_matrix, 
                           method = "square",     # Use "square" style
                           type = "lower",        # Show only lower triangle
                           lab = TRUE,            # Display correlation values
                           lab_size = 3,          # Adjust label size
                           colors = c("blue", "white", "red"), # Color scale
                           title = "Pearson Correlation Matrix",
                           ggtheme = theme_bw())

# Print the plot
print(Pearson_plot)

## We can remove RX1day, R99p and SDII
ggsave(Pearson_plot, file=paste0("./Season_indices_pearson_plot.png"), path =vis_path, width = 11, height = 10, units = "cm", dpi = 600)

## select only rain variables for P_V6-V10  
rain_variables <- Corr_data_season %>% 
  select(-c(Crop_season:Nitrogen)) %>%
  select(`P_V6-V10_SPI`, `P_V6-V10_CDD`) 


cor_matrix <- cor(rain_variables, use = "complete.obs", method = "pearson")

# Create correlation matrix plot
Pearson_plot <- ggcorrplot(cor_matrix, 
                           method = "square",     # Use "square" style
                           type = "lower",        # Show only lower triangle
                           lab = TRUE,            # Display correlation values
                           lab_size = 3,          # Adjust label size
                           colors = c("blue", "white", "red"), # Color scale
                           title = "Pearson Correlation Matrix",
                           ggtheme = theme_minimal())

# Print the plot
print(Pearson_plot)

## we can keep both SPI and CDD


## select only rain variables for P3 
rain_variables <- Corr_data_season %>% 
  select(-c(Crop_season:Nitrogen)) %>%
  select(`P_V10-R1_SPI`, `P_V10-R1_CDD`) 


cor_matrix <- cor(rain_variables, use = "complete.obs", method = "pearson")

# Create correlation matrix plot
Pearson_plot <- ggcorrplot(cor_matrix, 
                           method = "square",     # Use "square" style
                           type = "lower",        # Show only lower triangle
                           lab = TRUE,            # Display correlation values
                           lab_size = 3,          # Adjust label size
                           colors = c("blue", "white", "red"), # Color scale
                           title = "Pearson Correlation Matrix",
                           ggtheme = theme_minimal())

# Print the plot
print(Pearson_plot)

### We can keep only SPI




#### 1. Fit the random forest model ####

#### Rename variables for modeling 
names(Corr_data_season) <- gsub("-", "_", names(Corr_data_season))



#### TAGB ####

set.seed(123)  # Ensures reproducibility

# Function to run a Random Forest model and extract metrics
run_TAGB_rf <- function() {
  rf_TAGB_model <- randomForest(AGB_DM_t_ha ~ Mulch + Nitrogen + RX5day + Rnn99p +
                                  PRCPTOT + Prec_R1 + P_V6_V10_CDD + P_V6_V10_SPI + P_V10_R1_SPI + P_R1_R6_SPI + EHD_I30,
                                data = Corr_data_season,
                                ntree = 1000,
                                importance = TRUE)
  
  # Predictions
  predictions <- predict(rf_TAGB_model, Corr_data_season)
  observed <- Corr_data_season$AGB_DM_t_ha
  
  # Model Performance Metrics
  r2 <- rf_TAGB_model$rsq[length(rf_TAGB_model$rsq)]  # Last R² value
  rmse <- sqrt(mean((predictions - observed)^2))  # RMSE
  rrmse <- (rmse / mean(observed)) * 100  # Relative RMSE
  
  # EF (Nash-Sutcliffe Efficiency)
  ef <- 1 - (sum((observed - predictions)^2) / sum((observed - mean(observed))^2))
  
  ##Store Performance Metrics Separately
  TAGB_performance_df <- tibble(R2 = r2, RMSE = rmse, rRMSE = rrmse, EF = ef)
  
  # Extract Variable Importance
  TAGB_importance_df <- as.data.frame(importance(rf_TAGB_model)) %>%
    rownames_to_column(var = "Variable")
  
  # Store predictions in a separate dataframe
  TAGB_predictions_df <- data.frame(
    Crop_season = Corr_data_season$Crop_season,
    Rain_Treat = Corr_data_season$Rain_Treat,
    Nitrogen = Corr_data_season$Nitrogen,
    Mulch = Corr_data_season$Mulch,
    Observed = observed,
    Predicted = predictions,
    Run_ID = sample(1:1e6, 1)  # Unique ID per run
  )
  
  return(list(importance = TAGB_importance_df, 
              performance = TAGB_performance_df, 
              predictions = TAGB_predictions_df))
}

# Run the Random Forest model 100 times
rf_TAGB_results <- replicate(100, run_TAGB_rf(), simplify = FALSE)

# Extract the importance data from all runs
rf_TAGB_importance_list <- lapply(rf_TAGB_results, `[[`, "importance")

# Combine in chunks
rf_TAGB_importance_df <- bind_rows(rf_TAGB_importance_list[1:50]) %>% 
  bind_rows(bind_rows(rf_TAGB_importance_list[51:100])) 

# Extract the performance metrics from all runs
rf_TAGB_performance_list <- lapply(rf_TAGB_results, `[[`, "performance")

# Combine in chunks
rf_TAGB_performance_df <- bind_rows(rf_TAGB_performance_list[1:50]) %>% 
  bind_rows(bind_rows(rf_TAGB_performance_list[51:100]))

# Extract predictions (multiple rows per run)
rf_TAGB_predictions_list <- lapply(rf_TAGB_results, `[[`, "predictions")

# Combine in chunks
rf_TAGB_predictions_df <- bind_rows(rf_TAGB_predictions_list[1:50]) %>% 
  bind_rows(bind_rows(rf_TAGB_predictions_list[51:100]))

rownames(rf_TAGB_predictions_df) <- NULL




# Summarize importance metrics across 100 runs
TAGB_importance <- rf_TAGB_importance_df %>%
  group_by(Variable) %>%
  summarise(
    perc_Inc_MSE = mean(`%IncMSE`, na.rm = TRUE),
    SD_perc_Inc_MSE = sd(`%IncMSE`, na.rm = TRUE),
    Gini_index = mean(IncNodePurity, na.rm = TRUE),
    SD_Gini_index = sd(IncNodePurity, na.rm = TRUE)
  ) %>%
  mutate(
    Total = sum(perc_Inc_MSE, na.rm = TRUE),  # Compute total for normalization
    perc_Inc_MSE = (perc_Inc_MSE / Total) * 100,
    SD_perc_Inc_MSE = (SD_perc_Inc_MSE / Total) * 100
  ) %>%
  select(-Total) %>%
  arrange(desc(perc_Inc_MSE))


# Summarize model performance (including EF)
TAGB_model_performance <- rf_TAGB_performance_df %>%
  summarise(
    R2 = mean(R2, na.rm = TRUE),
    SD_R2 = sd(R2, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    rRMSE = mean(rRMSE, na.rm = TRUE),
    SD_rRMSE = sd(rRMSE, na.rm = TRUE),
    EF = mean(EF, na.rm = TRUE),
    SD_EF = sd(EF, na.rm = TRUE)
  )


rf_TAGB_predictions_summary <- rf_TAGB_predictions_df %>%
  group_by(Crop_season, Rain_Treat, Mulch, Nitrogen, Observed) %>%
  summarise(Predicted = mean(Predicted, na.rm = TRUE)) %>%
  ungroup

## rename variables accordignly to new period names
TAGB_importance <- TAGB_importance %>%
  mutate(Variable = case_when(
    Variable == "P_V6_V10_SPI"  ~ "SPI_V6-V10",
    Variable == "P_V10_R1_SPI"  ~ "SPI_V10-R1",
    Variable == "P_R1_R6_SPI"  ~ "SPI_R1-R6",
    Variable == "P_V6_V10_CDD"  ~ "CDD_V6-V10",
    TRUE ~ Variable  # Keep others unchanged
  ))


# Define color mapping for specific variables
color_mapping <- c("Nitrogen" = "#117733", 
                   "Mulch" = "#F8766D", 
                   "EHD_I30" = "darkorange",
                   "RX5day" = "blue", "Rnn99p" = "blue", 
                   "PRCPTOT" = "blue", 
                   "Prec_R1" = "blue", "CDD_V6-V10" = "blue", 
                   "SPI_V6-V10" = "blue", "SPI_V10-R1" = "blue", 
                   "SPI_R1-R6" = "blue")



## Importance plot

# Plot the feature importance with color mapping
TAGB_var_importance_plot <- ggplot(TAGB_importance, aes(x = reorder(Variable, perc_Inc_MSE), 
                                                        y = perc_Inc_MSE, 
                                                        fill = Variable)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = color_mapping) +
  coord_flip() +
  labs(title = "Relative importance for TAGB",
       x = "Explanatory variables",
       y = "% MSE increase") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "grey10", linewidth = 0.1) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "grey10", linewidth = 0.4), 
    axis.ticks.x = element_line(color = "grey10", linewidth = 0.4), 
    axis.title.y = element_text(size = 14),  # Increase y-axis title text size
    axis.text.y = element_text(size = 13.5),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 14),  # Increase y-axis title text size
    axis.text.x = element_text(size = 13.5),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),  # White inside
    plot.background = element_rect(fill = "white", color = NA) ) + # Hides legend if not
  
  ## Add annotation for explained variance
  annotate("text", 
           x = 1.5, y = 5.5, 
           label = paste0("Variance explained = ", round(TAGB_model_performance$R2 * 100, 0), "%"), 
           hjust = 0, size = 5, color = "black")

ggsave(TAGB_var_importance_plot, file=paste0("./RF_var_importance_TAGB_plot.png"), path =vis_path, width = 15, height = 13, units = "cm", dpi = 600)


## Sim. vs Obs. plot

rf_TAGB_predictions_summary$Rain_Treat <- factor(rf_TAGB_predictions_summary$Rain_Treat, levels = c("AR", "RR", "HR", "PR"))


# Create text annotation for performance metrics
TAGB_metrics_text <- paste0(
  "R² = ", round(TAGB_model_performance$R2, 2), "\n",
  "EF = ", round(TAGB_model_performance$EF, 2), "\n",
  "RMSE = ", round(TAGB_model_performance$RMSE, 2), "\n",
  "rRMSE = ", round(TAGB_model_performance$rRMSE, 2), "%"
)


# Scatter plot with 1:1 line
TAGB_plot <- ggplot(rf_TAGB_predictions_summary, aes(x = Observed, y = Predicted, color = Crop_season, shape = Rain_Treat)) +
  geom_point(size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
  scale_x_continuous(expand = c(0, 0), limits = c(0, 14), breaks = seq(0, 14, by = 3), name = expression(paste("Observed values (t DM ", ha^{-1}, ")"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14), breaks = seq(0, 14, by = 3), name = expression(paste("Predicted values (t DM ", ha^{-1}, ")"))) +
  scale_color_discrete(labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Change labels only
  scale_shape_discrete(labels = c("RR" = "RR", "AR" = "AR", "HR" = "HR", "PR" = "PR-N")) +  # Change PR to PR-N
  labs(
    title = "TAGB",
    color = "Cropping season",
    shape = "Rainfall treatment") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 12.5),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.x = element_text(size = 12.5),
    legend.title = element_text(size = 12.5),
    legend.text = element_text(size = 12), 
    legend.position = "right") +
  annotate("text", x = 1.018, 
           y = 11.846, 
           label = TAGB_metrics_text, hjust = 0, size = 4.2, color = "black")

print(TAGB_plot)






#### Grain yield ####

set.seed(123)  # Ensures reproducibility

# Function to run a Random Forest model and extract metrics
run_GY_rf <- function() {
  rf_GY_model <- randomForest(G_yield_t_ha_moist_standard ~ Mulch + Nitrogen + RX5day + Rnn99p +
                                PRCPTOT + Prec_R1 + P_V6_V10_CDD + P_V6_V10_SPI + P_V10_R1_SPI + P_R1_R6_SPI + EHD_I30,
                              data = Corr_data_season,
                              ntree = 1000,
                              importance = TRUE)
  
  # Predictions
  predictions <- predict(rf_GY_model, Corr_data_season)
  observed <- Corr_data_season$G_yield_t_ha_moist_standard
  
  # Model Performance Metrics
  r2 <- rf_GY_model$rsq[length(rf_GY_model$rsq)]  # Last R² value
  rmse <- sqrt(mean((predictions - observed)^2))  # RMSE
  rrmse <- (rmse / mean(observed)) * 100  # Relative RMSE
  
  # EF (Nash-Sutcliffe Efficiency)
  ef <- 1 - (sum((observed - predictions)^2) / sum((observed - mean(observed))^2))
  
  ##Store Performance Metrics Separately
  GY_performance_df <- tibble(R2 = r2, RMSE = rmse, rRMSE = rrmse, EF = ef)
  
  # Extract Variable Importance
  GY_importance_df <- as.data.frame(importance(rf_GY_model)) %>%
    rownames_to_column(var = "Variable")
  
  # Store predictions in a separate dataframe
  GY_predictions_df <- data.frame(
    Crop_season = Corr_data_season$Crop_season,
    Rain_Treat = Corr_data_season$Rain_Treat,
    Nitrogen = Corr_data_season$Nitrogen,
    Mulch = Corr_data_season$Mulch,
    Observed = observed,
    Predicted = predictions,
    Run_ID = sample(1:1e6, 1)  # Unique ID per run
  )
  
  return(list(importance = GY_importance_df, 
              performance = GY_performance_df, 
              predictions = GY_predictions_df))
}

# Run the Random Forest model 100 times
rf_GY_results <- replicate(100, run_GY_rf(), simplify = FALSE)

# Extract the importance data from all runs
rf_GY_importance_list <- lapply(rf_GY_results, `[[`, "importance")

# Combine in chunks
rf_GY_importance_df <- bind_rows(rf_GY_importance_list[1:50]) %>% 
  bind_rows(bind_rows(rf_GY_importance_list[51:100])) 

# Extract the performance metrics from all runs
rf_GY_performance_list <- lapply(rf_GY_results, `[[`, "performance")

# Combine in chunks
rf_GY_performance_df <- bind_rows(rf_GY_performance_list[1:50]) %>% 
  bind_rows(bind_rows(rf_GY_performance_list[51:100]))

# Extract predictions (multiple rows per run)
rf_GY_predictions_list <- lapply(rf_GY_results, `[[`, "predictions")

# Combine in chunks
rf_GY_predictions_df <- bind_rows(rf_GY_predictions_list[1:50]) %>% 
  bind_rows(bind_rows(rf_GY_predictions_list[51:100]))

rownames(rf_GY_predictions_df) <- NULL


# Summarize importance metrics across 100 runs
GY_importance <- rf_GY_importance_df %>%
  group_by(Variable) %>%
  summarise(
    perc_Inc_MSE = mean(`%IncMSE`, na.rm = TRUE),
    SD_perc_Inc_MSE = sd(`%IncMSE`, na.rm = TRUE),
    Gini_index = mean(IncNodePurity, na.rm = TRUE),
    SD_Gini_index = sd(IncNodePurity, na.rm = TRUE)
  ) %>%
  mutate(
    Total = sum(perc_Inc_MSE, na.rm = TRUE),  # Compute total for normalization
    perc_Inc_MSE = (perc_Inc_MSE / Total) * 100,
    SD_perc_Inc_MSE = (SD_perc_Inc_MSE / Total) * 100
  ) %>%
  select(-Total) %>%
  arrange(desc(perc_Inc_MSE))


# Summarize model performance (including EF)
GY_model_performance <- rf_GY_performance_df %>%
  summarise(
    R2 = mean(R2, na.rm = TRUE),
    SD_R2 = sd(R2, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    rRMSE = mean(rRMSE, na.rm = TRUE),
    SD_rRMSE = sd(rRMSE, na.rm = TRUE),
    EF = mean(EF, na.rm = TRUE),
    SD_EF = sd(EF, na.rm = TRUE)
  )


rf_GY_predictions_summary <- rf_GY_predictions_df %>%
  group_by(Crop_season, Rain_Treat, Mulch, Nitrogen, Observed) %>%
  summarise(Predicted = mean(Predicted, na.rm = TRUE)) %>%
  ungroup

## rename variables accordignly to new period names
GY_importance <- GY_importance %>%
  mutate(Variable = case_when(
    Variable == "P_V6_V10_SPI"  ~ "SPI_V6-V10",
    Variable == "P_V10_R1_SPI"  ~ "SPI_V10-R1",
    Variable == "P_R1_R6_SPI"  ~ "SPI_R1-R6",
    Variable == "P_V6_V10_CDD"  ~ "CDD_V6-V10",
    TRUE ~ Variable  # Keep others unchanged
  ))


## Importance plot

# Plot the feature importance with color mapping
GY_var_importance_plot <- ggplot(GY_importance, aes(x = reorder(Variable, perc_Inc_MSE), 
                                                    y = perc_Inc_MSE, 
                                                    fill = Variable)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = color_mapping) +
  coord_flip() +
  labs(title = "Relative importance for grain yield",
       x = "Explanatory variables",
       y = "% MSE increase") +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "grey10", linewidth = 0.1) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "grey10", linewidth = 0.4), 
    axis.ticks.x = element_line(color = "grey10", linewidth = 0.4), 
    axis.title.y = element_text(size = 14),  # Increase y-axis title text size
    axis.text.y = element_text(size = 13.5),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 14),  # Increase y-axis title text size
    axis.text.x = element_text(size = 13.5),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),  # White inside
    plot.background = element_rect(fill = "white", color = NA) ) +  # Hides legend if not 
  
  ## Add annotation for explained variance
  annotate("text", 
           x = 1.5, y = 5.422, 
           label = paste0("Variance explained = ", round(GY_model_performance$R2 * 100, 0), "%"), 
           hjust = 0, size = 5, color = "black")

ggsave(GY_var_importance_plot, file=paste0("./RF_var_importance_GY_plot.png"), path =vis_path, width = 15, height = 13, units = "cm", dpi = 600)


## Sim. vs Obs. plot

rf_GY_predictions_summary$Rain_Treat <- factor(rf_GY_predictions_summary$Rain_Treat, levels = c("AR", "RR", "HR", "PR"))


# Create text annotation for performance metrics
GY_metrics_text <- paste0(
  "R² = ", round(GY_model_performance$R2, 2), "\n",
  "EF = ", round(GY_model_performance$EF, 2), "\n",
  "RMSE = ", round(GY_model_performance$RMSE, 2), "\n",
  "rRMSE = ", round(GY_model_performance$rRMSE, 2), "%"
)


# Scatter plot with 1:1 line
GY_plot <- ggplot(rf_GY_predictions_summary, aes(x = Observed, y = Predicted, color = Crop_season, shape = Rain_Treat)) +
  geom_point(size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
  scale_x_continuous(expand = c(0, 0), limits = c(0, 6.5), breaks = seq(0, 6, by = 1), name = expression(paste("Observed values (t ", ha^{-1}, ")"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.5), breaks = seq(0, 6, by = 1), name = expression(paste("Predicted values (t ", ha^{-1}, ")"))) +
  scale_color_discrete(labels = c("2022-2023" = "2022-23", "2023-2024" = "2023-24")) +  # Change labels only
  scale_shape_discrete(labels = c("RR" = "RR", "AR" = "AR", "HR" = "HR", "PR" = "PR-N")) +  # Change PR to PR-N
  labs(
    title = "Grain yield",
    color = "Cropping season",
    shape = "Rainfall treatment") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.y = element_text(size = 12.5),   # Increase y-axis tick text size
    axis.title.x = element_text(size = 12.5),  # Increase y-axis title text size
    axis.text.x = element_text(size = 12.5),
    legend.title = element_text(size = 12.5),
    legend.text = element_text(size = 12), 
    legend.position = "right") +
  annotate("text", x = 0.4, 
           y = 5.5, 
           label = GY_metrics_text, hjust = 0, size = 4.2, color = "black")

print(GY_plot)



## Combine the two plots for both grain yield and TAGB ###

var_importance_combined_plot <- TAGB_var_importance_plot | GY_var_importance_plot 

ggsave(var_importance_combined_plot, file = paste0("./RF_var_importance_combined_plot.png"), 
       path = vis_path, width = 28, height = 13, units = "cm", dpi = 600)


predicted_vs_simulated_combined_plot <- (TAGB_plot | GY_plot) + 
  plot_layout(guides = "collect")

print(predicted_vs_simulated_combined_plot)



### 2. Partial Dependence Plot (PDP) ####
## This explains the marginal effect of a predictor on the response while keeping 
## other predictors as constant.


# List of all predictors in the model
predictors <- c( "Mulch", "Nitrogen", "RX5day", "Rnn99p", 
                "PRCPTOT", "Prec_R1", "P_V6_V10_CDD", "P_V6_V10_SPI", "P_V10_R1_SPI", 
                "P_R1_R6_SPI", "EHD_I30")

# Define a lookup table for renaming predictors in the plots with expressions for units
predictor_labels <- list(
  "SPI_V6-V10" = "SPI_V6-V10 (-)",
  "SPI_V10-R1" = "SPI_V10-R1 (-)",
  "SPI_R1-R6" = "SPI_R1-R6 (-)",
  "Prec_R1" = "Prec_R1 (mm)",
  "CDD_V6-V10" = "CDD_V6-V10 (day)",
  "Mulch" = expression(paste("Mulch (t DM ", ha^{-1}, yr^{-1},  ")")),
  "Nitrogen" = expression(paste("Nitrogen (kg N ", ha^{-1}, yr^{-1}, ")")),
  "RX5day" = "RX5day (mm)",
  "Rnn99p" = "Rnn99p (day)",
  "PRCPTOT" = "PRCPTOT (mm)",
  "EHD_I30" = "EHD_I30 (°C)"
)

### TAGB 
set.seed(123)  # Ensures reproducibility

# Function to run a Random Forest model and extract metrics
run_TAGB_rf <- function() {
  # Train the random forest model
  rf_TAGB_model <- randomForest(AGB_DM_t_ha ~ Mulch + Nitrogen + RX5day + Rnn99p +
                                PRCPTOT + Prec_R1 + P_V6_V10_CDD + P_V6_V10_SPI + P_V10_R1_SPI + P_R1_R6_SPI + EHD_I30,,
                              data = Corr_data_season,
                              ntree = 1000,
                              importance = TRUE)
  
  # Return the trained model
  return(rf_TAGB_model)
}

# Call the function to get the model
rf_TAGB_model <- run_TAGB_rf()





### Grain yield model 
set.seed(123)  # Ensures reproducibility

# Function to run a Random Forest model and extract metrics
run_GY_rf <- function() {
  # Train the random forest model
  rf_GY_model <- randomForest(G_yield_t_ha_moist_standard ~ Mulch + Nitrogen + RX5day + Rnn99p +
                                PRCPTOT + Prec_R1 + P_V6_V10_CDD + P_V6_V10_SPI + P_V10_R1_SPI + P_R1_R6_SPI + EHD_I30,,
                              data = Corr_data_season,
                              ntree = 1000,
                              importance = TRUE)
  
  # Return the trained model
  return(rf_GY_model)
}

# Call the function to get the model
rf_GY_model <- run_GY_rf()


### Plot

# Create an empty list to store PDP plots
GY_TAGB_pdp_plots <- list()

# Initialize an empty list to store the dataframes
PDP_df_list <- list()

# Initialize an empty list to store the scale factors
scale_factors <- list()

# Loop through each predictor and compute the PDP for both variables
for (predictor in predictors) {
  # Compute PDP for GY
  GY_pdp <- partial(rf_GY_model, pred.var = predictor, 
                    train = Corr_data_season, 
                    response = "G_yield_t_ha_moist_standard")  
  GY_pdp_df <- as.data.frame(GY_pdp)
  colnames(GY_pdp_df) <- c("Predictor_Value", "GY_yield")
  
  # Compute PDP for TAGB
  TAGB_pdp <- partial(rf_TAGB_model, pred.var = predictor, 
                      train = Corr_data_season, 
                      response = "AGB_DM_t_ha")  
  TAGB_pdp_df <- as.data.frame(TAGB_pdp)
  colnames(TAGB_pdp_df) <- c("Predictor_Value", "TAGB")
  
  # Merge both PDP data by Predictor_Value
  PDP_df <- left_join(GY_pdp_df, TAGB_pdp_df, by = "Predictor_Value")
  
  # Find the Predictor_Value corresponding to the max GY_yield
  max_GY_value <- max(PDP_df$GY_yield, na.rm = TRUE)
  max_GY_predictor <- PDP_df$Predictor_Value[PDP_df$GY_yield == max_GY_value]
  
  # Get the corresponding TAGB value at the same Predictor_Value
  max_TAGB_value <- PDP_df$TAGB[PDP_df$Predictor_Value == max_GY_predictor]
  
  # Compute the proportional scaling factor using the max GY_yield and the corresponding TAGB value
  scale_factor <- max_TAGB_value / max_GY_value
  
  # Store the scale factor in the list
  scale_factors[[predictor]] <- scale_factor
  
  # Apply scaling to TAGB
  PDP_df$TAGB_Scaled <- PDP_df$TAGB * scale_factor
  
  # Store the dataframe with TAGB_Scaled in the list
  PDP_df_list[[predictor]] <- PDP_df
  
  # Get the custom label
  predictor_label <- ifelse(predictor %in% names(predictor_labels), predictor_labels[[predictor]], predictor)
  
  legend_levels <- c("TAGB", "Grain yield")
  
  # Create ggplot with dual y-axis
  p <- ggplot(PDP_df, aes(x = Predictor_Value)) +
    geom_line(aes(y = TAGB, color = factor("TAGB", levels = c("TAGB", "Grain yield"))), size = 0.7) +
    geom_line(aes(y = GY_yield * scale_factor, color = factor("Grain yield", levels = c("TAGB", "Grain yield"))), size = 0.7) +
    scale_color_manual(
      values = c("TAGB" = "blue", "Grain yield" = "brown3"),
      name = NULL
    ) +  
    labs(
      x = predictor_label,
      y = expression(paste("Grain yield (t ", ha^{-1}, ")")), 
      color = ""
    ) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.01),
      name = expression(paste("TAGB (t DM ", ha^{-1}, ")")),
      sec.axis = sec_axis(
        trans = ~ . / scale_factor,  # Apply inverse scaling to map back to GY
        name = expression(paste("Grain yield (t ", ha^{-1}, ")")),
        labels = scales::number_format(accuracy = 0.01)
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 13, color = "blue"),
      axis.text.y = element_text(size = 12, color = "blue"),
      axis.title.y.right = element_text(size = 13, color = "brown3"),
      axis.text.y.right = element_text(size = 12, color = "brown3"),
      legend.key.size = unit(1.5, "cm"),  # Increase legend key size
      legend.text = element_text(size = 13.5),
      legend.position = "right"
    )
  
  # Store the plot in the list
  GY_TAGB_pdp_plots[[predictor]] <- p
}




# Extract the legend from the first plot
legend <- get_legend(GY_TAGB_pdp_plots[[1]] + theme(legend.position = "right"))

# Create a list of plots without the legend
GY_TAGB_pdp_plots_no_legend <- lapply(GY_TAGB_pdp_plots, function(p) p + theme(legend.position = "none"))

# Step 3: Add the legend as a last element in the list of plots
GY_TAGB_pdp_plots_with_legend <- c(GY_TAGB_pdp_plots_no_legend, list(legend))

# Combine the plots into one grid (without the legend)
final_plot <- plot_grid(
  plotlist = GY_TAGB_pdp_plots_with_legend, 
  ncol = 3, 
  nrow = 4,
  scale = 0.9
) + theme(plot.background = element_rect(fill = "white", color = NA))

# Save the final plot
ggsave(file = paste0(vis_path, "/GY_TAGB_pdp_plot.png"), 
       plot = final_plot, width = 29, height = 22, units = "cm", dpi = 600)










