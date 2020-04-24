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
# Data Directory: C:\Users\whitedl\Box Sync\Default Sync Folder\Projects\NSF_CNH\HUD_Analysis\Section8\CHS_AHD.csv

CHS_AHD <- read.csv("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/Tables_In/CHS_HUD.csv", stringsAsFactors=FALSE)
CHS_AHD_MAX <- CHS_AHD %>% group_by(GIS_JOIN) %>% filter(YEAR == max(YEAR)) 
CHS_AHD_Edit <- CHS_AHD_MAX  %>% select(Tract_Num = GIS_JOIN,YEAR,Sec_8_Reported = NUMBER_REPORTED)
#Summarize the number of section 8 units as check compare to later.
sum(CHS_AHD_Edit$Sec_8_Reported,na.rm=T)

#------------------------------------ END -----------------------------------#


#-------------------------- Read in the County Census Tract Polygon File --------------------------------------#
CHS_Census_Poly <- sf:::st_read(dsn = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "CHS_Census_Tract")
CHS_Census_Poly$Tract_Num <- as.numeric(as.character(CHS_Census_Poly$GISJOIN2))
str(CHS_Census_Poly)
plot(CHS_Census_Poly)
summary(CHS_Census_Poly)
attributes(CHS_Census_Poly)

#Removes Geometry - If needed
#CHS_Shape_table <- CHS_shape
#st_geometry(CHS_Shape_table) <- NULL

#------------------------------------ END -----------------------------------#

#----------------------------- Merge Section 8 Tabular Data and County Census Tract data via Tract Number ----------------------------#

# Uses SP package to merge --- A bit scary, but you do not have to define the join attribute ------ Assume that since Tract_Num is common between the two data sets that is being used -----
CHS_Census_AHD <- sp:::merge(CHS_Census_Poly, CHS_AHD_Edit)

# Specifies parameters for shapefile export 
#CHS_out_shape  = tempfile(pattern = "CHS_AHD_", tmpdir = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf:::st_write(CHS_Census_AHD, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/CHS_Census_AHD.shp", delete_layer=TRUE)
#Plot to Check 
#plot(DGL_Census_AHD[c("Sec_8_Reported")])
ggplot(CHS_Census_AHD) + geom_sf(aes(fill = Sec_8_Reported), color = "black") +
scale_fill_viridis_c(direction = -1, option = "inferno", alpha=.55)

sum(CHS_Census_AHD$Sec_8_Reported,na.rm=T)


#----------------------------------- End ------------------------------------#

#------------------------------ Summarize and Merge Geocoded Tax Credit with Section 8 and add back to County Census Tract Poly ----------------------------------#

#Open Geocoded Tax Credit
CHS_TAX_point <- sf:::st_read(dsn = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In", layer = "CHS_Tax_Credit")

#Checks to see if there are any duplicates in USER_HUD_ID_NUMBER, if so, need to examine 
#further as there may be multiple years that need to be addressed. 
CHS_TAX_point %>% filter(duplicated(.[["USER_HUD_I"]]))

#Summarize the number of units as check for later.
summarise(CHS_TAX_point, sum(USER_Low_I))

#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
CHS_AHD_Tax <- sf:::st_join(CHS_Census_AHD,CHS_TAX_point)

#Export result to a shapefile
#CHS_out_shape_TAX  = tempfile(pattern = "CHS_AHD_TAX_", tmpdir = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(CHS_AHD_Tax, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/CHS_AHD_TAX.shp", delete_layer=TRUE)
#Plot to Check 
plot(CHS_AHD_Tax[c("Sec_8_Reported")])
plot(CHS_AHD_Tax[c("USER_Numbe")])

#The Spatial Join can result in multiple LIHTC points in a census tract. Need to summarize by census tract
CHS_AHD_Tax_Sum <- CHS_AHD_Tax %>% group_by(Tract_Num) %>% summarise(Tax_Units_reported = sum(USER_Low_I)) %>% replace(is.na(.), 0)
CHS_AHD_Tax_Edit <- CHS_AHD_Tax  %>% select(Tract_Num,Sec_8_Reported) %>% distinct(Tract_Num, .keep_all = TRUE)

#Generate tabular data. Drops geometry
st_geometry(CHS_AHD_Tax_Sum) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(CHS_AHD_Tax_Sum$Tax_Units_reported,na.rm=T)

#Generate tabular data. Drops geometry
st_geometry(CHS_AHD_Tax_Edit) <- NULL

#Run a join
CHS_AHD_Tax_tabular_join <- inner_join(CHS_AHD_Tax_Edit,CHS_AHD_Tax_Sum, by="Tract_Num")
#Summarize the number of units as check compare to earlier sum.
sum(CHS_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(CHS_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)


#Spatial Join between Section 8 (Census Tract) with Geocoded Tax Credit (Address - Point). Runs an Intersect
CHS_AHD_Tax_spatial_join <- sp:::merge(CHS_Census_Poly,CHS_AHD_Tax_tabular_join)
CHS_AHD_Tax_spatial_join <- st_transform(CHS_AHD_Tax_spatial_join, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
# <- st_cast(CHS_AHD_Tax_spatial_join, "MULTIPOLYGON")
str(CHS_AHD_Tax_spatial_join)
st_crs(CHS_AHD_Tax_spatial_join)
class(CHS_AHD_Tax_spatial_join)


plot(CHS_AHD_Tax_spatial_join[c("Sec_8_Reported")])
plot(CHS_AHD_Tax_spatial_join[c("Tax_Units_reported")])
ggplot() + geom_sf(data = CHS_AHD_Tax_spatial_join)

#----------------------------------- End ------------------------------------#

#------------------------------ Merge Geocoded Tax Credit & Section 8 with CE ----------------------------------#

# ---------------------- Geodatabase Feature Layer Import Resulted in the Wrong Data Type ----------------------#
#gdb <- "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/Papers/NCED/NCED_CUCED_2018.gdb"
#CHS_CE_poly <- st_read(gdb, layer = "CHS_CE_data")
#CHS_CE_poly <- st_transform(CHS_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#ggplot() + geom_sf(data = CHS_CE_poly)
#CHS_CE_poly <- st_cast(CHS_CE_poly, "MULTIPOLYGON")

# --------------------- Use of Shapefile Results in Correct Data Type "Multipolygon" ------------------------#
CHS_CE_poly <- st_read("C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_In/CHS_CE_data.shp")
CHS_CE_poly <- st_transform(CHS_CE_poly, "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
CHS_CE_poly_select <- CHS_CE_poly %>% select(geometry)
ggplot() + geom_sf(data = CHS_CE_poly)
str(CHS_CE_poly)
st_crs(CHS_CE_poly)
class(CHS_CE_poly)

#----------------------------------- End ------------------------------------#


# ------------------------- SF Intersects Builds a list of polygons that have CEs --------------------------#
CHS_AHD_Tax_CE_spatial_join_list <- sf:::st_intersects(CHS_AHD_Tax_spatial_join,CHS_CE_poly_select, sparse = TRUE)
#Convert the list to a DF
CHS_AHD_Tax_CE_spatial_join_df <- as.data.frame(CHS_AHD_Tax_CE_spatial_join_list)

# --------------------------- Creates a vector based on row numbers from intersect performed in prior step -----------------------#
poly_list <- CHS_AHD_Tax_CE_spatial_join_df %>% distinct(CHS_AHD_Tax_CE_spatial_join_df$row.id, row.id = CHS_AHD_Tax_CE_spatial_join_df$row.id)
#Generate a new list of only row.id. Yes I know there are easier ways to do this. :-)
poly_list <- as.vector(poly_list$row.id) 

# ---------------------------- Adds a CE_Present column and Row.ID column that matches poly_list ---------------------#
CHS_AHD_Tax_spatial_join <- mutate(CHS_AHD_Tax_spatial_join, CE_Present = 0, row.id = as.numeric(rownames(CHS_AHD_Tax_spatial_join)))

# --------------------------- DPLYR to populate a binary column in the original spatial join. Are CEs present... yes = 1 & no = 0 ------#
CHS_AHD_Tax_spatial_join_FINAL <- CHS_AHD_Tax_spatial_join %>% mutate(CE_Present = replace(CE_Present,row.id %in% poly_list, 1))
ggplot() + geom_sf(data = CHS_AHD_Tax_spatial_join_FINAL)

#Export result to a shapefile
#CHS_AHD_Tax_spatial_join_FINAL_shp  = tempfile(pattern = "CHS_AHD_Tax_spatial_join_FINAL_", tmpdir = "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/CensusTract_2000", fileext = ".shp")
# Exports to a Shapefile
sf::st_write(CHS_AHD_Tax_spatial_join_FINAL, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/ShapeFiles_Out/CHS_AHD_Tax_spatial_join_FINAL.shp",  delete_layer=TRUE)

#Export data frame only
CHS_AHD_Tax_spatial_join_FINAL_table <- dplyr::select(as.data.frame(CHS_AHD_Tax_spatial_join_FINAL), -geometry, -row.id)
write.csv(CHS_AHD_Tax_spatial_join_FINAL_table, "C:/Users/whitedl/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out/CHS_AHD_Tax_spatial_join_FINAL_table.csv" )

#Drop geometry
st_geometry(CHS_AHD_Tax_spatial_join_FINAL) <- NULL
#Summarize the number of units as check compare to earlier sum.
sum(CHS_AHD_Tax_tabular_join$Tax_Units_reported,na.rm=T)
sum(CHS_AHD_Tax_tabular_join$Sec_8_Reported,na.rm=T)

#----------------------------------- End Of Script ------------------------------------#
