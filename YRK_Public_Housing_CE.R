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
# Data Directory: C:\Users\admin\Box Sync\Default Sync Folder\Projects\NSF_CNH\HUD_Analysis\Section8\YRK_AHD.csv

YRK_AHD <- read.csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/Tables_In/YRK_HUD.csv", stringsAsFactors=FALSE)
YRK_AHD_MAX <- YRK_AHD %>% group_by(GIS_JOIN) %>% filter(YEAR == max(YEAR)) 
YRK_AHD_Edit <- YRK_AHD_MAX  %>% select(Tract_Num = GIS_JOIN,YEAR,Sec_8_Reported = NUMBER_REPORTED)
#Summarize the number of section 8 units as check compare to later.
sum(YRK_AHD_Edit$Sec_8_Reported,na.rm=T)

#------------------------------------ END -----------------------------------#


#-------------------------- Read in the County Census Tract Polygon File --------------------------------------#
YRK_Census_Poly <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "YRK_Census_Tract")
YRK_Census_Poly$Tract_Num <- as.numeric(as.character(YRK_Census_Poly$GISJOIN2))
str(YRK_Census_Poly)
plot(YRK_Census_Poly)
summary(YRK_Census_Poly)
attributes(YRK_Census_Poly)

#Removes Geometry - If needed
#YRK_Shape_table <- YRK_shape
#st_geometry(YRK_Shape_table) <- NULL

#------------------------------------ END -----------------------------------#

#----------------------------- Merge Section 8 Tabular Data and County Census Tract data via Tract Number ----------------------------#

# Uses SP package to merge --- A bit scary, but you do not have to define the join attribute ------ Assume that since Tract_Num is common between the two data sets that is being used -----
YRK_Census_AHD <- sp:::merge(YRK_Census_Poly, YRK_AHD_Edit)

# Specifies parameters for shapefile export 
#YRK_out_shape  = tempfile(pattern = "YRK_AHD_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf:::st_write(YRK_Census_AHD, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/YRK_Census_AHD.shp", delete_layer=TRUE)
#Plot to Check 
plot(YRK_Census_AHD[c("Sec_8_Reported")])
sum(YRK_Census_AHD$Sec_8_Reported,na.rm=T)


#----------------------------------- End ------------------------------------#

#------------------------------ Summarize and Merge Geocoded Tax Credit with Section 8 and add back to County Census Tract Poly ----------------------------------#

#Open Geocoded Tax Credit
YRK_TAX_point <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "YRK_Tax_Credit")

#Checks to see if there are any duplicates in USER_HUD_ID_NUMBER, if so, need to examine 
#further as there may be multiple years that need to be addressed. 
YRK_TAX_point %>% filter(duplicated(.[["USER_HUD_I"]]))

#Summarize the number of units as check for later.
summarise(YRK_TAX_point, sum(USER_Low_I))

#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
YRK_AHD_Tax <- sf:::st_join(YRK_Census_AHD,YRK_TAX_point)

#Export result to a shapefile
#YRK_out_shape_TAX  = tempfile(pattern = "YRK_AHD_TAX_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(YRK_AHD_Tax, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/YRK_AHD_TAX.shp", delete_layer=TRUE)
#Plot to Check 
plot(YRK_AHD_Tax[c("Sec_8_Reported")])
plot(YRK_AHD_Tax[c("USER_Numbe")])

#The Spatial Join results in multiple points in a census tract. Need to summarize by census tract
YRK_AHD_Tax_Sum <- YRK_AHD_Tax %>% group_by(Tract_Num) %>% summarise(Tax_Units_reported = sum(USER_Low_I))
YRK_AHD_Tax_Edit <- YRK_AHD_Tax  %>% select(Tract_Num,Sec_8_Reported) %>% distinct(Tract_Num, .keep_all = TRUE)

#Generate tabular data. Drops geometry
st_geometry(YRK_AHD_Tax_Sum) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(YRK_AHD_Tax_Sum$Tax_Units_reported,na.rm=T)

#Generate tabular data. Drops geometry
st_geometry(YRK_AHD_Tax_Edit) <- NULL

#Run a join
YRK_AHD_Tax_tabular_join <- inner_join(YRK_AHD_Tax_Edit,YRK_AHD_Tax_Sum, by="Tract_Num")
#Summarize the number of units as check compare to earlier sum.
sum(YRK_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(YRK_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)


#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
YRK_AHD_Tax_spatial_join <- sp:::merge(YRK_Census_Poly,YRK_AHD_Tax_tabular_join)
YRK_AHD_Tax_spatial_join <- st_transform(YRK_AHD_Tax_spatial_join, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# <- st_cast(YRK_AHD_Tax_spatial_join, "MULTIPOLYGON")
str(YRK_AHD_Tax_spatial_join)
st_crs(YRK_AHD_Tax_spatial_join)
class(YRK_AHD_Tax_spatial_join)

plot(YRK_AHD_Tax_spatial_join[c("Sec_8_Reported")])
plot(YRK_AHD_Tax_spatial_join[c("Tax_Units_reported")])
ggplot() + geom_sf(data = YRK_AHD_Tax_spatial_join)

#----------------------------------- End ------------------------------------#

#------------------------------ Merge Geocoded Tax Credit & Section 8 with CE ----------------------------------#

# ---------------------- Geodatabase Feature Layer Import Resulted in the Wrong Data Type ----------------------#
#gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
#YRK_CE_poly <- st_read(gdb, layer = "YRK_CE_data")
#YRK_CE_poly <- st_transform(YRK_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#ggplot() + geom_sf(data = YRK_CE_poly)
#YRK_CE_poly <- st_cast(YRK_CE_poly, "MULTIPOLYGON")

# --------------------- Use of Shapefile Results in Correct Data Type "Multipolygon" ------------------------#
YRK_CE_poly <- st_read("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In/YRK_CE_data.shp")
YRK_CE_poly <- st_transform(YRK_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
YRK_CE_poly_select <- YRK_CE_poly %>% select(geometry)
ggplot() + geom_sf(data = YRK_CE_poly)
str(YRK_CE_poly)
st_crs(YRK_CE_poly)
class(YRK_CE_poly)

#----------------------------------- End ------------------------------------#


# ------------------------- SF Intersects Builds a list of polygons that have CEs --------------------------#
YRK_AHD_Tax_CE_spatial_join_list <- sf:::st_intersects(YRK_AHD_Tax_spatial_join,YRK_CE_poly_select, sparse = TRUE)
#Convert the list to a DF
YRK_AHD_Tax_CE_spatial_join_df <- as.data.frame(YRK_AHD_Tax_CE_spatial_join_list)

# --------------------------- Creates a vector based on row numbers from intersect performed in prior step -----------------------#
poly_list <- YRK_AHD_Tax_CE_spatial_join_df %>% distinct(YRK_AHD_Tax_CE_spatial_join_df$row.id, row.id = YRK_AHD_Tax_CE_spatial_join_df$row.id)
#Generate a new list of only row.id. Yes I know there are easier ways to do this. :-)
poly_list <- as.vector(poly_list$row.id) 

# ---------------------------- Adds a CE_Present column and Row.ID column that matches poly_list ---------------------#
YRK_AHD_Tax_spatial_join <- mutate(YRK_AHD_Tax_spatial_join, CE_Present = 0, row.id = as.numeric(rownames(YRK_AHD_Tax_spatial_join)))

# --------------------------- DPLYR to populate a binary column in the original spatial join. Are CEs present... yes = 1 & no = 0 ------#
YRK_AHD_Tax_spatial_join_FINAL <- YRK_AHD_Tax_spatial_join %>% mutate(CE_Present = replace(CE_Present,row.id %in% poly_list, 1))
ggplot() + geom_sf(data = YRK_AHD_Tax_spatial_join_FINAL)

#Export result to a shapefile
#YRK_AHD_Tax_spatial_join_FINAL_shp  = tempfile(pattern = "YRK_AHD_Tax_spatial_join_FINAL_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(YRK_AHD_Tax_spatial_join_FINAL, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/YRK_AHD_Tax_spatial_join_FINAL.shp",  delete_layer=TRUE)

#Export data frame only
YRK_AHD_Tax_spatial_join_FINAL_table <- dplyr::select(as.data.frame(YRK_AHD_Tax_spatial_join_FINAL), -geometry, -row.id)
write.csv(YRK_AHD_Tax_spatial_join_FINAL_table, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out/YRK_AHD_Tax_spatial_join_FINAL_table.csv" )

#Drop geometry
st_geometry(YRK_AHD_Tax_spatial_join_FINAL) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(YRK_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(YRK_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)

#----------------------------------- End Of Script ------------------------------------#
