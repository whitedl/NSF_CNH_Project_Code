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
# Data Directory: C:\Users\admin\Box Sync\Default Sync Folder\Projects\NSF_CNH\HUD_Analysis\Section8\MES_AHD.csv

MES_AHD <- read.csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/Tables_In/MES_HUD.csv", stringsAsFactors=FALSE)
MES_AHD_MAX <- MES_AHD %>% group_by(GIS_JOIN) %>% filter(YEAR == max(YEAR)) 
MES_AHD_Edit <- MES_AHD_MAX  %>% select(Tract_Num = GIS_JOIN,YEAR,Sec_8_Reported = NUMBER_REPORTED)
#Summarize the number of section 8 units as check compare to later.
sum(MES_AHD_Edit$Sec_8_Reported,na.rm=T)

#------------------------------------ END -----------------------------------#


#-------------------------- Read in the County Census Tract Polygon File --------------------------------------#
MES_Census_Poly <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "MES_Census_Tract")
MES_Census_Poly$Tract_Num <- as.numeric(as.character(MES_Census_Poly$GISJOIN2))
str(MES_Census_Poly)
plot(MES_Census_Poly)
summary(MES_Census_Poly)
attributes(MES_Census_Poly)

#Removes Geometry - If needed
#MES_Shape_table <- MES_shape
#st_geometry(MES_Shape_table) <- NULL

#------------------------------------ END -----------------------------------#

#----------------------------- Merge Section 8 Tabular Data and County Census Tract data via Tract Number ----------------------------#

# Uses SP package to merge --- A bit scary, but you do not have to define the join attribute ------ Assume that since Tract_Num is common between the two data sets that is being used -----
MES_Census_AHD <- sp:::merge(MES_Census_Poly, MES_AHD_Edit)

# Specifies parameters for shapefile export 
#MES_out_shape  = tempfile(pattern = "MES_AHD_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf:::st_write(MES_Census_AHD, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/MES_Census_AHD.shp", delete_layer=TRUE)
#Plot to Check 
plot(MES_Census_AHD[c("Sec_8_Reported")])
sum(MES_Census_AHD$Sec_8_Reported,na.rm=T)


#----------------------------------- End ------------------------------------#

#------------------------------ Summarize and Merge Geocoded Tax Credit with Section 8 and add back to County Census Tract Poly ----------------------------------#

#Open Geocoded Tax Credit
MES_TAX_point <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "MES_Tax_Credit")

#Checks to see if there are any duplicates in USER_HUD_ID_NUMBER, if so, need to examine 
#further as there may be multiple years that need to be addressed. 
MES_TAX_point %>% filter(duplicated(.[["USER_HUD_I"]]))

#Summarize the number of units as check for later.
summarise(MES_TAX_point, sum(USER_Low_I))

#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
MES_AHD_Tax <- sf:::st_join(MES_Census_AHD,MES_TAX_point)

#Export result to a shapefile
#MES_out_shape_TAX  = tempfile(pattern = "MES_AHD_TAX_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(MES_AHD_Tax, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/MES_AHD_TAX.shp", delete_layer=TRUE)
#Plot to Check 
plot(MES_AHD_Tax[c("Sec_8_Reported")])
plot(MES_AHD_Tax[c("USER_Numbe")])

#The Spatial Join results in multiple points in a census tract. Need to summarize by census tract
MES_AHD_Tax_Sum <- MES_AHD_Tax %>% group_by(Tract_Num) %>% summarise(Tax_Units_reported = sum(USER_Low_I))
MES_AHD_Tax_Edit <- MES_AHD_Tax  %>% select(Tract_Num,Sec_8_Reported) %>% distinct(Tract_Num, .keep_all = TRUE)

#Generate tabular data. Drops geometry
st_geometry(MES_AHD_Tax_Sum) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(MES_AHD_Tax_Sum$Tax_Units_reported,na.rm=T)

#Generate tabular data. Drops geometry
st_geometry(MES_AHD_Tax_Edit) <- NULL

#Run a join
MES_AHD_Tax_tabular_join <- inner_join(MES_AHD_Tax_Edit,MES_AHD_Tax_Sum, by="Tract_Num")
#Summarize the number of units as check compare to earlier sum.
sum(MES_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(MES_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)


#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
MES_AHD_Tax_spatial_join <- sp:::merge(MES_Census_Poly,MES_AHD_Tax_tabular_join)
MES_AHD_Tax_spatial_join <- st_transform(MES_AHD_Tax_spatial_join, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# <- st_cast(MES_AHD_Tax_spatial_join, "MULTIPOLYGON")
str(MES_AHD_Tax_spatial_join)
st_crs(MES_AHD_Tax_spatial_join)
class(MES_AHD_Tax_spatial_join)

plot(MES_AHD_Tax_spatial_join[c("Sec_8_Reported")])
plot(MES_AHD_Tax_spatial_join[c("Tax_Units_reported")])
ggplot() + geom_sf(data = MES_AHD_Tax_spatial_join)

#----------------------------------- End ------------------------------------#

#------------------------------ Merge Geocoded Tax Credit & Section 8 with CE ----------------------------------#

# ---------------------- Geodatabase Feature Layer Import Resulted in the Wrong Data Type ----------------------#
#gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
#MES_CE_poly <- st_read(gdb, layer = "MES_CE_data")
#MES_CE_poly <- st_transform(MES_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#ggplot() + geom_sf(data = MES_CE_poly)
#MES_CE_poly <- st_cast(MES_CE_poly, "MULTIPOLYGON")

# --------------------- Use of Shapefile Results in Correct Data Type "Multipolygon" ------------------------#
MES_CE_poly <- st_read("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In/MES_CE_data.shp")
MES_CE_poly <- st_transform(MES_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
MES_CE_poly_select <- MES_CE_poly %>% select(geometry)
ggplot() + geom_sf(data = MES_CE_poly)
str(MES_CE_poly)
st_crs(MES_CE_poly)
class(MES_CE_poly)

#----------------------------------- End ------------------------------------#


# ------------------------- SF Intersects Builds a list of polygons that have CEs --------------------------#
MES_AHD_Tax_CE_spatial_join_list <- sf:::st_intersects(MES_AHD_Tax_spatial_join,MES_CE_poly_select, sparse = TRUE)
#Convert the list to a DF
MES_AHD_Tax_CE_spatial_join_df <- as.data.frame(MES_AHD_Tax_CE_spatial_join_list)

# --------------------------- Creates a vector based on row numbers from intersect performed in prior step -----------------------#
poly_list <- MES_AHD_Tax_CE_spatial_join_df %>% distinct(MES_AHD_Tax_CE_spatial_join_df$row.id, row.id = MES_AHD_Tax_CE_spatial_join_df$row.id)
#Generate a new list of only row.id. Yes I know there are easier ways to do this. :-)
poly_list <- as.vector(poly_list$row.id) 

# ---------------------------- Adds a CE_Present column and Row.ID column that matches poly_list ---------------------#
MES_AHD_Tax_spatial_join <- mutate(MES_AHD_Tax_spatial_join, CE_Present = 0, row.id = as.numeric(rownames(MES_AHD_Tax_spatial_join)))

# --------------------------- DPLYR to populate a binary column in the original spatial join. Are CEs present... yes = 1 & no = 0 ------#
MES_AHD_Tax_spatial_join_FINAL <- MES_AHD_Tax_spatial_join %>% mutate(CE_Present = replace(CE_Present,row.id %in% poly_list, 1))
ggplot() + geom_sf(data = MES_AHD_Tax_spatial_join_FINAL)

#Export result to a shapefile
#MES_AHD_Tax_spatial_join_FINAL_shp  = tempfile(pattern = "MES_AHD_Tax_spatial_join_FINAL_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(MES_AHD_Tax_spatial_join_FINAL, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/MES_AHD_Tax_spatial_join_FINAL.shp",  delete_layer=TRUE)

#Export data frame only
MES_AHD_Tax_spatial_join_FINAL_table <- dplyr::select(as.data.frame(MES_AHD_Tax_spatial_join_FINAL), -geometry, -row.id)
write.csv(MES_AHD_Tax_spatial_join_FINAL_table, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out/MES_AHD_Tax_spatial_join_FINAL_table.csv" )

#Drop geometry
st_geometry(MES_AHD_Tax_spatial_join_FINAL) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(MES_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(MES_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)

#----------------------------------- End Of Script ------------------------------------#
