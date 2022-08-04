library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)


#setwd("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1")

#setwd("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1")

setwd("/Users/davidwhite/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/")

# where the data are as feature classes
#gdb <- "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/HMI_v1.gdb"

# where the data are as feature classes
#gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/HMI_v1.gdb"

dir()


# make a list to fill
final_df <- list()
# the county abbreviations
cnty <- c("ALB","BLD","CHS","DGL","GVL","LDN","LEB","MES","SAC","SON","WAS","YRK")

#print( paste("You have choosen the following file name: ", fileName)) 

gdb <- "~/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/HMI_v1/HMI_v1.gdb"

st_layers(gdb)

# loop to get data
for (i  in 1:12) {
  cname <- cnty[i]
  # get data
  
  # Clemson CE data
  c_layer <- paste0(cname, "_CE_data_with_Reason_Variables.csv")
  c_ce <- read.csv(c_layer, colClasses=c("CE_InstNum"="character","SprsededBy"="character","Supersedes"="character"))
  c_ce$COUNTY_NAME <- cname  
  c_gis_layer <- paste0(cname, "_CE_HMI_mean")
  c_gis_ce <- st_read(dsn = gdb, layer = c_gis_layer, stringsAsFactors = F, quiet = TRUE)
  
  #Drop Geometry and results in a DF
  st_geometry(c_gis_ce) <- NULL
  c_gis_ce_subset <- c_gis_ce %>% dplyr::select(CE_InstNum, projectCID, HMI_mean, perim_area_ratio, shape_index, frac_dim)
  df_list <- left_join(c_ce, c_gis_ce_subset, by = "projectCID")
  
  
  #print( paste("You have choosen the following file name: ", c_layer, "You have choosen the following Feature Class:", c_gis_layer)) 




  final_df <- bind_rows(final_df, df_list)
}



write.csv(final_df, "/Users/davidwhite/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/all_counties_reasons_HMI.csv")

final_df_HMI <- final_df %>% dplyr::select(projectCID, HMI_mean, COUNTY_NAME)

final_df_HMI$HMI_mean_round <- as.numeric(as.character(final_df_HMI$HMI_mean))

#find HMI mean
final_df_HMI  %>%
  group_by(COUNTY_NAME, na.rm = TRUE) %>%
  summarise_at(vars(HMI_mean_round), list(name = mean), na.rm = TRUE)

# BoxPlot
HMI_box_plot <-  ggplot(data=final_df_HMI, mapping=aes(x=COUNTY_NAME, y=HMI_mean_round))+geom_boxplot()+xlab("County") +
  ylab("Human Modified Index Mean")
HMI_box_plot
ggsave("/Users/davidwhite/Box Sync/Default Sync Folder/Projects/NSF_CNH/HMI/results/HMI_mean_box_plot.png")




