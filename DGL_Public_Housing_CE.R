#David L. White
#Date: 05/29/2019
#Purpose: Processes ESRI Shapefiles and tabular data sets to generate a table of summarized public housing metrics and conservation easement sptial proximity. 


library(readr)
library(dplyr)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)
library(units)
#library(shapefiles)
#library(raster) prevents DPLYR select from working.
#library(maptools)



#----------------------- MAX Section 8 Housing Data by YEAR and Generate a DF by Census Tract ID --------------------------#
# Data Directory: C:\Users\whitedl\Box Sync\Default Sync Folder\Projects\NSF_CNH\HUD_Analysis\Section8\DGL_AHD.csv

DGL_AHD <- read.csv("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/Tables_In/DGL_HUD.csv", stringsAsFactors=FALSE)
DGL_AHD_MAX <- DGL_AHD %>% group_by(GIS_JOIN) %>% filter(YEAR == max(YEAR)) 
DGL_AHD_Edit <- DGL_AHD_MAX %>% drop_na(YEAR) %>% select(Tract_Num = GIS_JOIN,YEAR,Sec_8_Reported = NUMBER_REPORTED)
#Summarize the number of section 8 units as check compare to later.

sum(DGL_AHD_Edit$Sec_8_Reported,na.rm=T)

#------------------------------------ END -----------------------------------#


#-------------------------- Read in the County Census Tract Polygon File --------------------------------------#
DGL_Census_Poly <- sf:::st_read(dsn = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "DGL_Census_Tract")
DGL_Census_Poly$Tract_Num <- as.numeric(as.character(DGL_Census_Poly$GISJOIN2))
str(DGL_Census_Poly)
plot(DGL_Census_Poly)
summary(DGL_Census_Poly)
attributes(DGL_Census_Poly)

#Removes Geometry - If needed
#DGL_Shape_table <- DGL_shape
#st_geometry(DGL_Shape_table) <- NULL

#------------------------------------ END -----------------------------------#

#----------------------------- Merge Section 8 Tabular Data and County Census Tract data via Tract Number ----------------------------#

# Uses SP package to merge --- A bit scary, but you do not have to define the join attribute ------ Assume that since Tract_Num is common between the two data sets that is being used -----
DGL_Census_AHD <- sp:::merge(DGL_Census_Poly, DGL_AHD_Edit)

# Specifies parameters for shapefile export 
#DGL_out_shape  = tempfile(pattern = "DGL_AHD_", tmpdir = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf:::st_write(DGL_Census_AHD, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/DGL_Census_AHD.shp", delete_layer=TRUE)
#Plot to Check 
plot(DGL_Census_AHD[c("Sec_8_Reported")])
sum(DGL_Census_AHD$Sec_8_Reported,na.rm=T)


#----------------------------------- End ------------------------------------#

# ---------------------- Geodatabase Feature Layer Import Resulted in the Wrong Data Type ----------------------#
#gdb <- "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
#DGL_CE_poly <- st_read(gdb, layer = "DGL_CE_data")
#DGL_CE_poly <- st_transform(DGL_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#ggplot() + geom_sf(data = DGL_CE_poly)
#DGL_CE_poly <- st_cast(DGL_CE_poly, "MULTIPOLYGON")

# --------------------- Use of Shapefile Results in Correct Data Type "Multipolygon" ------------------------#
DGL_CE_poly <- st_read("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In/DGL_CE_data.shp")
DGL_CE_poly <- st_transform(DGL_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
DGL_CE_poly_select <- DGL_CE_poly %>% select(geometry)
ggplot() + geom_sf(data = DGL_CE_poly)
str(DGL_CE_poly)
st_crs(DGL_CE_poly)
class(DGL_CE_poly)

#----------------------------------- End ------------------------------------#


# ------------------------- SF Intersects Builds a list of polygons that have CEs --------------------------#

DGL_Census_AHD <- st_transform(DGL_Census_AHD, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
DGL_Census_AHD_list <- sf:::st_intersects(DGL_Census_AHD, DGL_CE_poly_select, sparse = TRUE)
#Convert the list to a DF
DGL_AHD_CE_spatial_join_df <- as.data.frame(DGL_Census_AHD_list)

# --------------------------- Creates a vector based on row numbers from intersect performed in prior step -----------------------#
poly_list <- DGL_AHD_CE_spatial_join_df %>% distinct(DGL_AHD_CE_spatial_join_df$row.id, row.id = DGL_AHD_CE_spatial_join_df$row.id)
#Generate a new list of only row.id. Yes I know there are easier ways to do this. :-)
poly_list <- as.vector(poly_list$row.id) 

# ---------------------------- Adds a CE_Present column and Row.ID column that matches poly_list ---------------------#
DGL_Census_AHD <- mutate(DGL_Census_AHD, CE_Present = 0, row.id = as.numeric(rownames(DGL_Census_AHD)))

# --------------------------- DPLYR to populate a binary column in the original spatial join. Are CEs present... yes = 1 & no = 0 ------#
DGL_Census_AHD_FINAL <- DGL_Census_AHD %>% mutate(CE_Present = replace(CE_Present,row.id %in% poly_list, 1))
ggplot() + geom_sf(data = DGL_Census_AHD_FINAL)

#Export result to a shapefile
#DGL_AHD_Tax_spatial_join_FINAL_shp  = tempfile(pattern = "DGL_AHD_Tax_spatial_join_FINAL_", tmpdir = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(DGL_Census_AHD_FINAL, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/DGL_AHD_Tax_spatial_join_FINAL.shp",  delete_layer=TRUE)

#Export data frame only
DGL_Census_AHD_FINAL_table <- dplyr::select(as.data.frame(DGL_Census_AHD_FINAL), -geometry, -row.id)
write.csv(DGL_Census_AHD_FINAL_table, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out/DGL_AHD_Tax_spatial_join_FINAL_table.csv" )

#Drop geometry
st_geometry(DGL_Census_AHD_FINAL) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(DGL_AHD_Edit$Sec_8_Reported,na.rm=T)
sum(DGL_Census_AHD$Sec_8_Reported,na.rm=T)
sum(DGL_Census_AHD_FINAL$Sec_8_Reported,na.rm=T)


#----------------------------------- End Of Script ------------------------------------#
