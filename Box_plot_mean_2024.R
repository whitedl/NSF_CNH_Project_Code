library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

setwd("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/")

dir()

# make a list to fill
final_df <- list()
# the county abbreviations
cnty <- c("ALB","BLD","CHS","DGL","GVL","LDN","LEB","MES","SAC","SON","WAS","YRK")

#print( paste("You have choosen the following file name: ", fileName)) 

# See NCED_CUCED_2021_update_2024.gdb as the master database. Updated ALB and YRK with removal of records.
# HMI_v2_2024_update_1.gdb corrected HMI calculation errors for 6 CEs. 1 in LDN and 5 in SON.
gdb <- "~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/HMI_v2_2024_update_1.gdb"

st_layers(gdb)

# loop to get data
for (i  in 1:12) {
  cname <- cnty[i]
  # get data
  # Clemson CE data
  # Reasons Table
  c_layer <- paste0("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Reasons_CSV/",cname, "_CE_data_with_Reason_Variables.csv")
  c_ce <- read.csv(c_layer, colClasses=c("projectCID"="character","YearCEEnd"="integer", "CE_InstNum" = "character", "SprsededBy" = "character", "Supersedes" = "character"))
  #Subset A
  c_ce <- subset(c_ce, YearCEEnd > "2009")
  c_ce$COUNTY_NAME <- cname 
  c_gis_layer <- paste0(cname, "_CE_HMI_mean")
  c_gis_ce <- st_read(dsn = gdb, layer = c_gis_layer, stringsAsFactors = F, quiet = TRUE)
  c_gis_ce$YearCEEnd <- as.numeric(c_gis_ce$YearCEEnd) 
  c_gis_ce_sub <- subset(c_gis_ce, YearCEEnd > "2009")
  #Drop Geometry and results in a DF
  st_geometry(c_gis_ce_sub) <- NULL
  c_gis_ce_subset <- c_gis_ce_sub %>% dplyr::select(CE_InstNum, projectCID, HMI_mean, CEArea_Ac, NearCECty)
  df_list <- left_join(c_ce, c_gis_ce_subset, by = "projectCID")
  
  #print( paste("You have choosen the following file name: ", c_layer, "You have choosen the following Feature Class:", c_gis_layer)) 
  
  # Dynamically create the dataframe name
  cnty_name <- paste0("cnty_", cname)
  
  # Assign the list to the dynamically created dataframe name
  assign(cnty_name, df_list)
  
  final_df <- bind_rows(final_df, df_list)
}

write.csv(final_df, "~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/all_counties_reasons_HMI.csv")

final_df_HMI <- final_df %>% dplyr::select(projectCID, HMI_mean, COUNTY_NAME, CEArea_Ac.x)

final_df_HMI$HMI_mean_round <- as.numeric(as.character(final_df_HMI$HMI_mean))

#find HMI mean
cnty_HMI_mean <- final_df_HMI  %>%
  group_by(COUNTY_NAME, na.rm = TRUE) %>%
  summarise_at(vars(HMI_mean_round), list(name = mean), na.rm = TRUE)

write.csv(cnty_HMI_mean, "~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/All_Counties_HMI_Mean.csv", row.names = FALSE)

# BoxPlot
HMI_box_plot <-  ggplot(data=final_df_HMI, mapping=aes(x=COUNTY_NAME, y=HMI_mean_round))+geom_boxplot()+xlab("County") +
  ylab("Human Modified Index Mean")
HMI_box_plot

HMI_box_plot <- ggplot(data = final_df_HMI, mapping = aes(x = COUNTY_NAME, y = HMI_mean_round)) +
  geom_boxplot() +
  xlab("County") +
  ylab("Human Modified Index Mean") +
  scale_x_discrete(limits = c("SAC","SON","BLD", "MES","DGL", "WAS", "LEB", "YRK", "CHS", "GVL", "ALB", "LDN"), 
                   labels = c("W-CA-SAC", "W-CA-SON", "W-CO-BLD", "W-CO-MES", "MW-MN-DGL", "MW-MN-WAS", "NE-PA-LEB", "NE-PA-York", 
                              "SE-SC-CHS", "SE-SC-GVL", "SE-VA-ALB", "SE-VA-LDN")) + ylab("Human Modified Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_df_HMI$REGION <- ifelse(final_df_HMI$COUNTY_NAME %in% c("SAC", "SON", "BLD", "MES"), "West",
                              ifelse(final_df_HMI$COUNTY_NAME %in% c("DGL", "WAS"), "Midwest",
                                     ifelse(final_df_HMI$COUNTY_NAME %in% c("LEB", "YRK"), "Northeast", "Southeast")))

# Define the county order by region
county_order <- c("SAC", "SON", "BLD", "MES", "DGL", "WAS", 
                  "LEB", "YRK", "CHS", "GVL", "ALB", "LDN")

# Define custom labels for counties
custom_labels <- c("SAC" = "W-CA-SAC", "SON" = "W-CA-SON", "BLD" = "W-CO-BLD", 
                   "MES" = "W-CO-MES", "DGL" = "MW-MN-DGL", "WAS" = "MW-MN-WAS", 
                   "LEB" = "NE-PA-LEB", "YRK" = "NE-PA-York", "CHS" = "SE-SC-CHS", 
                   "GVL" = "SE-SC-GVL", "ALB" = "SE-VA-ALB", "LDN" = "SE-VA-LDN")


##################### Color Box Plot ###########################

HMI_box_plot <- ggplot(data = final_df_HMI, mapping = aes(x = COUNTY_NAME, y = HMI_mean_round, fill = REGION)) +
  geom_boxplot() +
  xlab("County") +
  ylab("Human Modified Index Mean") +
  scale_x_discrete(
    limits = c("SAC", "SON", "BLD", "MES", "DGL", "WAS", "LEB", "YRK", "CHS", "GVL", "ALB", "LDN"), 
    labels = c("W-CA-SAC", "W-CA-SON", "W-CO-BLD", "W-CO-MES", "MW-MN-DGL", "MW-MN-WAS", "NE-PA-LEB", "NE-PA-York", 
               "SE-SC-CHS", "SE-SC-GVL", "SE-VA-ALB", "SE-VA-LDN")
  ) +
  scale_fill_manual(
    values = c("West" = scales::alpha("blue", 0.6), 
               "Midwest" = scales::alpha("green", 0.6), 
               "Northeast" = scales::alpha("purple", 0.6), 
               "Southeast" = scales::alpha("orange", 0.6))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Human Modified Index by County and Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
        axis.title.x = element_text(size = 12),  # Increase size of x-axis label
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +  # Increase size of y-axis label
  theme(legend.position = "none")  # Removes the legend
HMI_box_plot

ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_box_plot_region.png")

HMI_violin_plot <- ggplot(data = final_df_HMI, mapping = aes(x = COUNTY_NAME, y = HMI_mean_round, fill = REGION)) +
  geom_violin(trim = FALSE) +
  xlab("County") +
  ylab("Human Modified Index Mean") +
  scale_x_discrete(
    limits = c("SAC", "SON", "BLD", "MES", "DGL", "WAS", "LEB", "YRK", "CHS", "GVL", "ALB", "LDN"), 
    labels = c("W-CA-SAC", "W-CA-SON", "W-CO-BLD", "W-CO-MES", "MW-MN-DGL", "MW-MN-WAS", "NE-PA-LEB", "NE-PA-York", 
               "SE-SC-CHS", "SE-SC-GVL", "SE-VA-ALB", "SE-VA-LDN")
  ) +
  scale_fill_manual(
    values = c("West" = scales::alpha("blue", 0.6), 
               "Midwest" = scales::alpha("green", 0.6), 
               "Northeast" = scales::alpha("purple", 0.6), 
               "Southeast" = scales::alpha("orange", 0.6))
  ) +
  geom_point(aes(y = weighted_median), color = "red", size = 3, shape = 17) +  # Add median point +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Human Modified Index by County and Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
        axis.title.x = element_text(size = 12),  # Increase size of x-axis label
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +  # Increase size of y-axis label
  theme(legend.position = "none")  # Removes the legend
HMI_violin_plot

ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_violin_plot_region.png")


############################ Box Plot Weighted by CE Acres:  Update 12-19-2023 File: beta_regression_by_county.R #############################################


HMI_box_plot_weighted <-  ggplot2::ggplot(data=final_df_HMI, aes(x = COUNTY_NAME, y = HMI_mean_round, weight = CEArea_Ac.x)) +
  geom_boxplot() +
  labs(x = "County", y = "HMI", title = "Box Plot of HMI by County (Weighted by CE Acres)")
HMI_box_plot_weighted
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_box_plot_weighted.png")

HMI_box_plot_weighted <-  ggplot2::ggplot(data=final_df_HMI, aes(x = COUNTY_NAME, y = HMI_mean_round, weight = CEArea_Ac.x)) +
  geom_violin() +
  labs(x = "County", y = "HMI", title = "Box Plot of HMI by County (Weighted by CE Acres)")
HMI_box_plot_weighted
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_box_plot_weighted.png")

##################### Color Box Plot Weighted ###########################

HMI_box_plot_weighted <- ggplot2::ggplot(data = final_df_HMI, 
                                         aes(x = COUNTY_NAME, y = HMI_mean_round, weight = CEArea_Ac.x, fill = REGION)) +
  geom_boxplot() +
  xlab("County") +
  ylab("Human Modified Index Weighted by CE Acres") +  # Correctly call ylab()
  scale_x_discrete(
    limits = c("SAC", "SON", "BLD", "MES", "DGL", "WAS", "LEB", "YRK", "CHS", "GVL", "ALB", "LDN"),
    labels = c("W-CA-SAC", "W-CA-SON", "W-CO-BLD", "W-CO-MES", "MW-MN-DGL", "MW-MN-WAS", "NE-PA-LEB", "NE-PA-York", 
               "SE-SC-CHS", "SE-SC-GVL", "SE-VA-ALB", "SE-VA-LDN")
  ) +
  scale_fill_manual(
    values = c("West" = scales::alpha("blue", 0.6), 
               "Midwest" = scales::alpha("green", 0.6), 
               "Northeast" = scales::alpha("purple", 0.6), 
               "Southeast" = scales::alpha("orange", 0.6))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
        axis.title.x = element_text(size = 12),  # Increase size of x-axis label
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +  # Increase size of y-axis label
  labs(title = "Human Modified Index by County and Region") +
  theme(legend.position = "none")  # Removes the legend
HMI_box_plot_weighted
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_box_plot_weighted_region.png")

HMI_violin_plot_weighted <- ggplot2::ggplot(data = final_df_HMI, 
                                         aes(x = COUNTY_NAME, y = HMI_mean_round, weight = CEArea_Ac.x, fill = REGION)) +
  geom_violin() +
  xlab("County") +
  ylab("Human Modified Index Weighted by CE Acres") +  # Correctly call ylab()
  scale_x_discrete(
    limits = c("SAC", "SON", "BLD", "MES", "DGL", "WAS", "LEB", "YRK", "CHS", "GVL", "ALB", "LDN"),
    labels = c("W-CA-SAC", "W-CA-SON", "W-CO-BLD", "W-CO-MES", "MW-MN-DGL", "MW-MN-WAS", "NE-PA-LEB", "NE-PA-York", 
               "SE-SC-CHS", "SE-SC-GVL", "SE-VA-ALB", "SE-VA-LDN")
  ) +
  scale_fill_manual(
    values = c("West" = scales::alpha("blue", 0.6), 
               "Midwest" = scales::alpha("green", 0.6), 
               "Northeast" = scales::alpha("purple", 0.6), 
               "Southeast" = scales::alpha("orange", 0.6))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
        axis.title.x = element_text(size = 12),  # Increase size of x-axis label
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +  # Increase size of y-axis label
  labs(title = "Human Modified Index by County and Region") +
  theme(legend.position = "none")  # Removes the legend
HMI_violin_plot_weighted
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_violin_plot_weighted_region.png")


########################### Facet Binned Charts ################################


# Update COUNTY_NAME as a factor with the specified order and apply custom labels
final_df_HMI_binned <- final_df_HMI %>%
  mutate(
    COUNTY_NAME = factor(COUNTY_NAME, levels = county_order),
    COUNTY_NAME = recode(COUNTY_NAME, !!!custom_labels)  # Apply custom labels for counties
  )

# Create the binned data and order counties based on custom labels
binned_data <- final_df_HMI_binned %>%
  mutate(
    CEArea_bin = cut(
      CEArea_Ac.x,
      breaks = seq(min(CEArea_Ac.x, na.rm = TRUE), max(CEArea_Ac.x, na.rm = TRUE), length.out = 5),
      include.lowest = TRUE
    )
  ) %>%
  group_by(COUNTY_NAME, CEArea_bin) %>%
  summarise(mean_HMI = mean(HMI_mean_round, na.rm = TRUE), .groups = "drop")

# Create the bar chart with transparency and custom labels
binned_chart <- ggplot(binned_data, aes(x = CEArea_bin, y = mean_HMI)) +
  geom_bar(stat = "identity", alpha = 0.5) +  # Apply transparency
  facet_wrap(~ COUNTY_NAME, scales = "free_x", labeller = labeller(COUNTY_NAME = custom_labels)) +  # Apply custom labels to facets
  xlab("CE Area (binned)") +
  ylab("Mean HMI") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10), # Adjust size of facet labels
    legend.position = "none"  # Remove legend if present
  )

# Save the plot
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_binned.png", 
       width = 12, height = 8, dpi = 300)

# Display plot
binned_chart

############################### Facet Percentiles #################################

######### Custom Labels ############# up top #################

# Update COUNTY_NAME as a factor with the specified order and apply custom labels
final_df_HMI_binned <- final_df_HMI %>%
  mutate(
    COUNTY_NAME = factor(COUNTY_NAME, levels = county_order),
    COUNTY_NAME = recode(COUNTY_NAME, !!!custom_labels)  # Apply custom labels for counties
  )

# Calculate the percentiles (25%, 50%, 75%)
percentiles <- quantile(final_df_HMI_binned$CEArea_Ac.x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Create the binned data and order counties based on custom labels
binned_data <- final_df_HMI_binned %>%
  mutate(
    CEArea_bin = cut(
      CEArea_Ac.x,
      breaks = c(-Inf, percentiles, Inf),  # Set bins based on percentiles
      labels = c("Q1 (25%)", "Q2 (50%)", "Q3 (75%)", "Q4 (100%)"),  # Label the bins
      include.lowest = TRUE
    )
  ) %>%
  group_by(COUNTY_NAME, CEArea_bin, REGION) %>%
  summarise(mean_HMI = mean(HMI_mean_round, na.rm = TRUE), .groups = "drop")

# Create the bar chart with transparency and custom labels, using REGION for color
binned_chart <- ggplot(binned_data, aes(x = CEArea_bin, y = mean_HMI, fill = REGION)) +
  geom_bar(stat = "identity", alpha = 0.5) +  # Apply transparency
  facet_wrap(~ COUNTY_NAME, scales = "free_x", labeller = labeller(COUNTY_NAME = custom_labels)) +  # Apply custom labels to facets
  xlab("CE Area (binned)") +
  ylab("Mean HMI") +
  theme_minimal() +
  scale_fill_manual(
    values = c("West" = scales::alpha("blue", 0.6), 
               "Midwest" = scales::alpha("green", 0.6), 
               "Northeast" = scales::alpha("purple", 0.6), 
               "Southeast" = scales::alpha("orange", 0.6))
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10), # Adjust size of facet labels
    legend.position = "none"  # Remove legend if present
  )

# Display plot
binned_chart

# Save the plot
ggsave("~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI_2024/Results_box_plot/HMI_mean_percentiles.png")


############################## HMI,Size and Distance to Urban boundary ##################################

final_df$NearCECty.y <- as.numeric(as.character(final_df$NearCECty.y))
final_df$CEArea_Ac.y <- as.numeric(as.character(final_df$CEArea_Ac.y))
final_df$HMI_mean <- as.numeric(as.character(final_df$HMI_mean))

# Check distributions
library(ggplot2)
ggplot(final_df, aes(x = NearCECty.y)) + geom_histogram(bins = 30)
ggplot(final_df, aes(x = log10(CEArea_Ac.y), y = HMI_mean)) + geom_point() + geom_smooth(method = "lm")
ggplot(final_df, aes(x = log10(NearCECty.y), y = HMI_mean)) + geom_point() + geom_smooth(method = "lm")

# Correlation matrix
install.packages("corrplot")
library(corrplot)

final_df$NearCECty.y_log <- log10(final_df$NearCECty.y)
final_df$CEArea_Ac.y_log <- log10(final_df$CEArea_Ac.y)
cor_matrix <- cor(final_df[, c("NearCECty.y_log", "CEArea_Ac.y_log", "HMI_mean")], use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# Regression analysis
model <- lm(CEArea_Ac.y ~ NearCECty.y, data = final_df)
summary(model)

# Visualize interaction effects
install.packages('interactions')
library(interactions)
interact_plot(model, pred = NearCECty.y, modx = CEArea_Ac.y, plot.points = TRUE)

###########

# Define the list of counties
cnty <- c("ALB", "BLD", "CHS", "DGL", "GVL", "LDN", "LEB", "MES", "SAC", "SON", "WAS", "YRK")

# Initialize lists to store results
cor_results <- list()
regression_results <- list()
plots <- list()

# Loop through each county
for (county in cnty) {
  # Subset data for the current county
  county_data <- subset(final_df, cnty == county)
  
  # Ensure columns are numeric
  county_data$NearCECty.y <- as.numeric(as.character(county_data$NearCECty.y))
  county_data$CEArea_Ac.y <- as.numeric(as.character(county_data$CEArea_Ac.y))
  county_data$HMI_mean <- as.numeric(as.character(county_data$HMI_mean))
  
  # Apply transformations (log + 1 to handle zeros)
  county_data$log_NearCECty <- log1p(county_data$NearCECty.y)
  county_data$log_CEArea_Ac <- log1p(county_data$CEArea_Ac.y)
  
  # Check data sufficiency
  if (nrow(county_data) > 2) {
    # Check distributions (log-transformed)
    histograms <- list(
      log_NearCECty = hist(county_data$log_NearCECty, main = paste("Log Distribution of NearCECty for", county), 
                           xlab = "log(NearCECty)", col = "lightblue"),
      log_CEArea_Ac = hist(county_data$log_CEArea_Ac, main = paste("Log Distribution of CEArea_Ac for", county), 
                           xlab = "log(CEArea_Ac)", col = "lightgreen"),
      HMI_mean = hist(county_data$HMI_mean, main = paste("Distribution of HMI_mean for", county), 
                      xlab = "HMI_mean", col = "lightpink")
    )
    plots[[county]] <- histograms
    
    # Correlation matrix (log-transformed variables)
    cor_matrix <- cor(
      county_data[, c("log_NearCECty", "log_CEArea_Ac", "HMI_mean")],
      use = "complete.obs"
    )
    cor_results[[county]] <- cor_matrix
    
    # Regression analysis: HMI_mean ~ log-transformed variables
    model <- lm(HMI_mean ~ log_NearCECty + log_CEArea_Ac, data = county_data)
    regression_results[[county]] <- summary(model)
    
    # Interaction plot: Log-transformed variables
    interaction_plot <- plot(
      county_data$log_NearCECty, 
      county_data$HMI_mean,
      xlab = "Log(Distance to Urban Boundary)",
      ylab = "HMI Mean",
      main = paste("Interaction for", county),
      col = county_data$log_CEArea_Ac,
      pch = 19
    )
    legend("topright", legend = paste("Size:", county_data$log_CEArea_Ac), 
           col = unique(county_data$log_CEArea_Ac), pch = 19)
    plots[[paste(county, "interaction", sep = "_")]] <- interaction_plot
  } else {
    # Handle insufficient data
    cor_results[[county]] <- "Insufficient data for correlation"
    regression_results[[county]] <- "Insufficient data for regression"
    plots[[county]] <- "Insufficient data for plots"
  }
}

# Print results for verification
cor_results
regression_results

###############################

# Define the list of counties
cnty <- c("ALB", "BLD", "CHS", "DGL", "GVL", "LDN", "LEB", "MES", "SAC", "SON", "WAS", "YRK")

# Initialize lists to store results
cor_results <- list()
regression_results <- list()
plots <- list()

# Loop through each county
for (county in cnty) {
  # Subset data for the current county
  county_data <- subset(final_df, COUNTY_NAME == county)
  
  # Ensure columns are numeric
  county_data$NearCECty.y <- as.numeric(as.character(county_data$NearCECty.y))
  county_data$CEArea_Ac.y <- as.numeric(as.character(county_data$CEArea_Ac.y))
  county_data$HMI_mean <- as.numeric(as.character(county_data$HMI_mean))
  
  # Separate into two groups:
  # Group 1: NearCECty.y == 0
  group_zero <- subset(county_data, NearCECty.y == 0)
  
  # Group 2: NearCECty.y > 0
  group_non_zero <- subset(county_data, NearCECty.y > 0)
  
  # Initialize a list to store results for this county
  county_results <- list()
  
  # Check data sufficiency and perform analysis for group_zero
  if (nrow(group_zero) > 2) {
    # Apply transformations (log + 1 to handle zeros)
    group_zero$log_NearCECty <- log1p(group_zero$NearCECty.y)
    group_zero$log_CEArea_Ac <- log1p(group_zero$CEArea_Ac.y)
    
    # Correlation matrix for group_zero
    cor_matrix_zero <- cor(
      group_zero[, c("log_NearCECty", "log_CEArea_Ac", "HMI_mean")],
      use = "complete.obs"
    )
    county_results$cor_zero <- cor_matrix_zero
    
    # Regression analysis: HMI_mean ~ log-transformed variables (group_zero)
    model_zero <- lm(HMI_mean ~ log_NearCECty + log_CEArea_Ac, data = group_zero)
    county_results$reg_zero <- summary(model_zero)
    
    # Store plots for group_zero
    plots[[paste(county, "zero", sep = "_")]] <- ggplot(group_zero, aes(x = log_NearCECty, y = HMI_mean)) +
      geom_point() + 
      geom_smooth(method = "lm") +
      ggtitle(paste("Regression for", county, "NearCECty.y == 0"))
  } else {
    county_results$cor_zero <- "Insufficient data for correlation (NearCECty.y == 0)"
    county_results$reg_zero <- "Insufficient data for regression (NearCECty.y == 0)"
    plots[[paste(county, "zero", sep = "_")]] <- "Insufficient data for plots (NearCECty.y == 0)"
  }
  
  # Check data sufficiency and perform analysis for group_non_zero
  if (nrow(group_non_zero) > 2) {
    # Apply transformations (log + 1 to handle zeros)
    group_non_zero$log_NearCECty <- log1p(group_non_zero$NearCECty.y)
    group_non_zero$log_CEArea_Ac <- log1p(group_non_zero$CEArea_Ac.y)
    
    # Correlation matrix for group_non_zero
    cor_matrix_non_zero <- cor(
      group_non_zero[, c("log_NearCECty", "log_CEArea_Ac", "HMI_mean")],
      use = "complete.obs"
    )
    county_results$cor_non_zero <- cor_matrix_non_zero
    
    # Regression analysis: HMI_mean ~ log-transformed variables (group_non_zero)
    model_non_zero <- lm(HMI_mean ~ log_NearCECty + log_CEArea_Ac, data = group_non_zero)
    county_results$reg_non_zero <- summary(model_non_zero)
    
    # Store plots for group_non_zero
    plots[[paste(county, "non_zero", sep = "_")]] <- ggplot(group_non_zero, aes(x = log_NearCECty, y = HMI_mean)) +
      geom_point() + 
      geom_smooth(method = "lm") +
      ggtitle(paste("Regression for", county, "NearCECty.y > 0"))
  } else {
    county_results$cor_non_zero <- "Insufficient data for correlation (NearCECty.y > 0)"
    county_results$reg_non_zero <- "Insufficient data for regression (NearCECty.y > 0)"
    plots[[paste(county, "non_zero", sep = "_")]] <- "Insufficient data for plots (NearCECty.y > 0)"
  }
  
  # Store results for this county
  cor_results[[county]] <- county_results$cor_zero
  regression_results[[county]] <- county_results$reg_zero
}

# Print results for verification
cor_results
regression_results



############
# Define the list of counties
cnty <- c("ALB", "BLD", "CHS", "DGL", "GVL", "LDN", "LEB", "MES", "SAC", "SON", "WAS", "YRK")

# Initialize lists to store results
cor_results <- list()
regression_results <- list()
plots <- list()

# Loop through each county
for (county in cnty) {
  # Subset data for the current county
  county_data <- subset(final_df, COUNTY_NAME == county)
  
  # Ensure columns are numeric
  county_data$NearCECty.y <- as.numeric(as.character(county_data$NearCECty.y))
  county_data$CEArea_Ac.y <- as.numeric(as.character(county_data$CEArea_Ac.y))
  county_data$HMI_mean <- as.numeric(as.character(county_data$HMI_mean))
  
  # Separate into two groups:
  # Group 1: NearCECty.y == 0
  group_zero <- subset(county_data, NearCECty.y == 0)
  
  # Group 2: NearCECty.y > 0 (all positive values)
  group_non_zero <- subset(county_data, NearCECty.y > 0)
  
  # Initialize a list to store results for this county
  county_results <- list()
  
  # Check data sufficiency and perform analysis for group_zero
  if (nrow(group_zero) > 2) {
    # Apply transformations (log + 1 to handle zeros)
    group_zero$log_NearCECty <- log1p(group_zero$NearCECty.y)
    group_zero$log_CEArea_Ac <- log1p(group_zero$CEArea_Ac.y)
    
    # Correlation matrix for group_zero
    cor_matrix_zero <- cor(
      group_zero[, c("log_NearCECty", "log_CEArea_Ac", "HMI_mean")],
      use = "complete.obs"
    )
    county_results$cor_zero <- cor_matrix_zero
    
    # Regression analysis: HMI_mean ~ log-transformed variables (group_zero)
    model_zero <- lm(HMI_mean ~ log_NearCECty + log_CEArea_Ac, data = group_zero)
    county_results$reg_zero <- summary(model_zero)
    
    # Store plots for group_zero
    plots[[paste(county, "zero", sep = "_")]] <- ggplot(group_zero, aes(x = log_NearCECty, y = HMI_mean)) +
      geom_point() + 
      geom_smooth(method = "lm") +
      ggtitle(paste("Regression for", county, "NearCECty.y == 0"))
  } else {
    county_results$cor_zero <- "Insufficient data for correlation (NearCECty.y == 0)"
    county_results$reg_zero <- "Insufficient data for regression (NearCECty.y == 0)"
    plots[[paste(county, "zero", sep = "_")]] <- "Insufficient data for plots (NearCECty.y == 0)"
  }
  
  # Check data sufficiency and perform analysis for group_non_zero
  if (nrow(group_non_zero) > 2) {
    # Apply transformations (log + 1 to handle zeros)
    group_non_zero$log_NearCECty <- log1p(group_non_zero$NearCECty.y)
    group_non_zero$log_CEArea_Ac <- log1p(group_non_zero$CEArea_Ac.y)
    
    # Correlation matrix for group_non_zero
    cor_matrix_non_zero <- cor(
      group_non_zero[, c("log_NearCECty", "log_CEArea_Ac", "HMI_mean")],
      use = "complete.obs"
    )
    county_results$cor_non_zero <- cor_matrix_non_zero
    
    # Regression analysis: HMI_mean ~ log-transformed variables (group_non_zero)
    model_non_zero <- lm(HMI_mean ~ log_NearCECty + log_CEArea_Ac, data = group_non_zero)
    county_results$reg_non_zero <- summary(model_non_zero)
    
    # Store plots for group_non_zero
    plots[[paste(county, "non_zero", sep = "_")]] <- ggplot(group_non_zero, aes(x = log_NearCECty, y = HMI_mean)) +
      geom_point() + 
      geom_smooth(method = "lm") +
      ggtitle(paste("Regression for", county, "NearCECty.y > 0"))
  } else {
    county_results$cor_non_zero <- "Insufficient data for correlation (NearCECty.y > 0)"
    county_results$reg_non_zero <- "Insufficient data for regression (NearCECty.y > 0)"
    plots[[paste(county, "non_zero", sep = "_")]] <- "Insufficient data for plots (NearCECty.y > 0)"
  }
  
  # Store results for both groups separately
  cor_results[[paste(county, "zero", sep = "_")]] <- county_results$cor_zero
  cor_results[[paste(county, "non_zero", sep = "_")]] <- county_results$cor_non_zero
  regression_results[[paste(county, "zero", sep = "_")]] <- county_results$reg_zero
  regression_results[[paste(county, "non_zero", sep = "_")]] <- county_results$reg_non_zero
}

# Print results for verification
cor_results
regression_results

##############

# Initialize a list to store the correlation matrix results
cor_matrix_table <- data.frame(
  County = character(),
  Group = character(),
  Var1 = character(),
  Var2 = character(),
  Correlation = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each county and its results
for (county in cnty) {
  # For group zero (NearCECty.y == 0)
  if (is.matrix(cor_results[[paste(county, "zero", sep = "_")]])) {
    cor_matrix_zero <- cor_results[[paste(county, "zero", sep = "_")]]
    
    # Store correlation results as a data frame
    cor_matrix_zero_df <- as.data.frame(as.table(cor_matrix_zero))
    cor_matrix_zero_df$County <- county
    cor_matrix_zero_df$Group <- "Zero"
    
    # Append to the final table
    cor_matrix_table <- rbind(cor_matrix_table, cor_matrix_zero_df)
  }
  
  # For group non-zero (NearCECty.y > 0)
  if (is.matrix(cor_results[[paste(county, "non_zero", sep = "_")]])) {
    cor_matrix_non_zero <- cor_results[[paste(county, "non_zero", sep = "_")]]
    
    # Store correlation results as a data frame
    cor_matrix_non_zero_df <- as.data.frame(as.table(cor_matrix_non_zero))
    cor_matrix_non_zero_df$County <- county
    cor_matrix_non_zero_df$Group <- "Non Zero"
    
    # Append to the final table
    cor_matrix_table <- rbind(cor_matrix_table, cor_matrix_non_zero_df)
  }
}

# Print the correlation matrix table
cor_matrix_table


#####################

library(ggplot2)

# Initialize a list to store the plots
regression_plots <- list()

# Loop through each county and generate regression plots
for (county in cnty) {
  # For group zero (NearCECty.y == 0)
  county_data_zero <- subset(final_df, COUNTY_NAME == county & NearCECty.y == 0)
  
  if (nrow(county_data_zero) > 0) {
    # Linear regression model for NearCECty.y == 0 group
    model_zero <- lm(HMI_mean ~ NearCECty.y + CEArea_Ac.y, data = county_data_zero)
    
    # Plot the regression line
    plot_zero <- ggplot(county_data_zero, aes(x = NearCECty.y, y = HMI_mean)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "blue") +
      ggtitle(paste("Regression for NearCECty.y == 0 in", county)) +
      xlab("NearCECty.y") +
      ylab("HMI_mean") +
      theme_minimal()
    
    regression_plots[[paste(county, "zero", sep = "_")]] <- plot_zero
  }
  
  # For group non-zero (NearCECty.y > 0)
  county_data_non_zero <- subset(final_df, COUNTY_NAME == county & NearCECty.y > 0)
  
  if (nrow(county_data_non_zero) > 0) {
    # Linear regression model for NearCECty.y > 0 group
    model_non_zero <- lm(HMI_mean ~ NearCECty.y + CEArea_Ac.y, data = county_data_non_zero)
    
    # Plot the regression line
    plot_non_zero <- ggplot(county_data_non_zero, aes(x = NearCECty.y, y = HMI_mean)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "red") +
      ggtitle(paste("Regression for NearCECty.y > 0 in", county)) +
      xlab("NearCECty.y") +
      ylab("HMI_mean") +
      theme_minimal()
    
    regression_plots[[paste(county, "non_zero", sep = "_")]] <- plot_non_zero
  }
}

# Check if plots are stored
regression_plots

# Optionally, print a plot to ensure it's working
if (length(regression_plots) > 0) {
  print(regression_plots[[1]]) # Print the first plot in the list
}


 