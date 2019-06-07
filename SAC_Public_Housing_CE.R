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
# Data Directory: C:\Users\admin\Box Sync\Default Sync Folder\Projects\NSF_CNH\HUD_Analysis\Section8\SAC_AHD.csv

SAC_AHD <- read_csv("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/Tables_IN/SAC_AHD.csv")
SAC_AHD_MAX <- SAC_AHD %>% group_by(CODE) %>% filter(YEAR == max(YEAR)) 
SAC_AHD_Edit <- SAC_AHD_MAX  %>% select(Tract_Num = CODE,YEAR,Sec_8_Reported = NUMBER_REPORTED)
#Summarize the number of section 8 units as check compare to later.
colSums(SAC_AHD_Edit,na.rm=T)

#------------------------------------ END -----------------------------------#


#-------------------------- Read in the County Census Tract Polygon File --------------------------------------#
SAC_Census_Poly <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "Sacramento")
str(SAC_Census_Poly)
plot(SAC_Census_Poly)
summary(SAC_Census_Poly)
attributes(SAC_Census_Poly)

#Removes Geometry - If needed
#SAC_Shape_table <- SAC_shape
#st_geometry(SAC_Shape_table) <- NULL

#------------------------------------ END -----------------------------------#

#----------------------------- Merge Section 8 Tabular Data and County Census Tract data via Tract Number ----------------------------#

# Uses SP package to merge --- A bit scary, but you do not have to define the join attribute ------ Assume that since Tract_Num is common between the two data sets that is being used -----
SAC_Census_AHD <- sp:::merge(SAC_Census_Poly, SAC_AHD_Edit)
# Specifies parameters for shapefile export 
#SAC_out_shape  = tempfile(pattern = "SAC_AHD_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf:::st_write(SAC_Census_AHD, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/SAC_Census_AHD.shp")
#Plot to Check 
plot(SAC_Census_AHD[c("Sec_8_Reported")])



#----------------------------------- End ------------------------------------#

#------------------------------ Summarize and Merge Geocoded Tax Credit with Section 8 and add back to County Census Tract Poly ----------------------------------#

#Open Geocoded Tax Credit
gdb <- "HUD_working.gdb"
SAC_TAX_point <- sf:::st_read(dsn = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/HUD_working.gdb", layer = "SAC_Tax_Credit")

#Checks to see if there are any duplicates in USER_HUD_ID_NUMBER, if so, need to examine 
#further as there may be multiple years that need to be addressed. 
SAC_TAX_point %>% filter(duplicated(.[["USER_HUD_ID_Number"]]))

#Summarize the number of units as check for later.
summarise(SAC_TAX_point, sum(USER_Low_Income_Units))

#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
SAC_AHD_Tax <- sf:::st_join(SAC_Census_AHD,SAC_TAX_point)

#Export result to a shapefile
#SAC_out_shape_TAX  = tempfile(pattern = "SAC_AHD_TAX_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(SAC_AHD_Tax, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/SAC_AHD_TAX.shp")
#Plot to Check 
plot(SAC_AHD_Tax[c("Sec_8_Reported")])
plot(SAC_AHD_Tax[c("USER_Number_Units")])

#The Spatial Join results in multiple points in a census tract. Need to summarize by census tract
SAC_AHD_Tax_Sum <- SAC_AHD_Tax %>% group_by(Tract_Num) %>% summarise(Tax_Units_reported = sum(USER_Low_Income_Units))
SAC_AHD_Tax_Edit <- SAC_AHD_Tax  %>% select(Tract_Num,Sec_8_Reported) %>% distinct(Tract_Num, .keep_all = TRUE)

#Generate tabular data. Drops geometry
st_geometry(SAC_AHD_Tax_Sum) <- NULL
#Summarize the number of units as check compare to earlier sum.
colSums(SAC_AHD_Tax_Sum,na.rm=T)

#Generate tabular data. Drops geometry
st_geometry(SAC_AHD_Tax_Edit) <- NULL

#Run a join
SAC_AHD_Tax_tabular_join <- inner_join(SAC_AHD_Tax_Edit,SAC_AHD_Tax_Sum, by="Tract_Num")
#Summarize the number of units as check compare to earlier sum.
colSums(SAC_AHD_Tax_tabular_join,na.rm=T)



#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
SAC_AHD_Tax_spatial_join <- sp:::merge(SAC_Census_Poly,SAC_AHD_Tax_tabular_join)
SAC_AHD_Tax_spatial_join <- st_transform(SAC_AHD_Tax_spatial_join, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# <- st_cast(SAC_AHD_Tax_spatial_join, "MULTIPOLYGON")
str(SAC_AHD_Tax_spatial_join)
st_crs(SAC_AHD_Tax_spatial_join)
class(SAC_AHD_Tax_spatial_join)

plot(SAC_AHD_Tax_spatial_join[c("Sec_8_Reported")])
plot(SAC_AHD_Tax_spatial_join[c("Tax_Units_reported")])
ggplot() + geom_sf(data = SAC_AHD_Tax_spatial_join)

#----------------------------------- End ------------------------------------#

#------------------------------ Merge Geocoded Tax Credit & Section 8 with CE ----------------------------------#

# ---------------------- Geodatabase Feature Layer Import Resulted in the Wrong Data Type ----------------------#
#gdb <- "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
#SAC_CE_poly <- st_read(gdb, layer = "SAC_CE_data")
#SAC_CE_poly <- st_transform(SAC_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#ggplot() + geom_sf(data = SAC_CE_poly)
#SAC_CE_poly <- st_cast(SAC_CE_poly, "MULTIPOLYGON")

# --------------------- Use of Shapefile Results in Correct Data Type "Multipolygon" ------------------------#
SAC_CE_poly <- st_read("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In/SAC_CE_data.shp")
SAC_CE_poly <- st_transform(SAC_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
SAC_CE_poly_select <- SAC_CE_poly %>% select(geometry)
ggplot() + geom_sf(data = SAC_CE_poly)
str(SAC_CE_poly)
st_crs(SAC_CE_poly)
class(SAC_CE_poly)

#----------------------------------- End ------------------------------------#


# ------------------------- SF Intersects Builds a list of polygons that have CEs --------------------------#
SAC_AHD_Tax_CE_spatial_join_list <- sf:::st_intersects(SAC_AHD_Tax_spatial_join,SAC_CE_poly_select, sparse = TRUE)
#Convert the list to a DF
SAC_AHD_Tax_CE_spatial_join_df <- as.data.frame(SAC_AHD_Tax_CE_spatial_join_list)

# --------------------------- Creates a vector based on row numbers from intersect performed in prior step -----------------------#
poly_list <- SAC_AHD_Tax_CE_spatial_join_df %>% distinct(SAC_AHD_Tax_CE_spatial_join_df$row.id, row.id = SAC_AHD_Tax_CE_spatial_join_df$row.id)
#Generate a new list of only row.id. Yes I know there are easier ways to do this. :-)
poly_list <- as.vector(poly_list$row.id) 

# ---------------------------- Adds a CE_Present column and Row.ID column that matches poly_list ---------------------#
SAC_AHD_Tax_spatial_join <- mutate(SAC_AHD_Tax_spatial_join, CE_Present = 0, row.id = as.numeric(rownames(SAC_AHD_Tax_spatial_join)))

# --------------------------- DPLYR to populate a binary column in the original spatial join. Are CEs present... yes = 1 & no = 0 ------#
SAC_AHD_Tax_spatial_join_FINAL <- SAC_AHD_Tax_spatial_join %>% mutate(CE_Present = replace(CE_Present,row.id %in% poly_list, 1))
ggplot() + geom_sf(data = SAC_AHD_Tax_spatial_join_FINAL)

#Export result to a shapefile
#SAC_AHD_Tax_spatial_join_FINAL_shp  = tempfile(pattern = "SAC_AHD_Tax_spatial_join_FINAL_", tmpdir = "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(SAC_AHD_Tax_spatial_join_FINAL, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/SAC_AHD_Tax_spatial_join_FINAL.shp")

#Export data frame only
SAC_AHD_Tax_spatial_join_FINAL_table <- dplyr::select(as.data.frame(SAC_AHD_Tax_spatial_join_FINAL), -geometry, -row.id)
write.csv(SAC_AHD_Tax_spatial_join_FINAL_table, "C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out/SAC_AHD_Tax_spatial_join_FINAL_table.csv" )

#Drop geometry
st_geometry(SAC_AHD_Tax_spatial_join_FINAL) <- NULL
#Summarize the number of units as check compare to earlier sum.
colSums(SAC_AHD_Tax_tabular_join,na.rm=T)
 
#----------------------------------- End Of Script ------------------------------------#
