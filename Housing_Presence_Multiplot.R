# Date: 2019-06-25
# D White
# Description: Build dataset with common purpose, holder, and area

library(tidyverse)
library(sf)
library(dplyr)
# build a dataset of all data ---------------------------------------------

setwd("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalTables_Out")

# the county abbreviations
cnty <- c("ALB","BLD","CHS","DGL","GVL","LDN","LEB","MES","SAC","SON","WAS","YRK")

df_list <- list()

for (i in 1:12) {
  cuce <- read.csv(paste0(cnty[i], "_AHD_Tax_spatial_join_FINAL_table.csv"), stringsAsFactors = F)[,9:11]
  cuce <- mutate(cuce, County_ID = cnty[i])
  df_list[[i]] <- cuce
}

# combine all of the county data
alldata <- bind_rows(df_list) %>% select(Sec_8_Reported, Tax_Units_reported, CE_Present, County_ID)
alldata$CE_Present <- as.character(alldata$CE_Present)

alldata <- alldata %>% mutate(CE_Present = recode(CE_Present, '0' = 'No CE', '1' = 'CE'))


#### Count of CE's ####

ggplot(all_dat_group) + geom_bar(aes(x=CE_Present,y=Sec_8_Reported, fill=CE_Present), stat="identity", width = .5, show.legend = FALSE) + coord_flip() +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~County_ID) + 
  labs(title = "Section 8 Units Reported by Census Tract", x='', y='Counts') +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle=-45, hjust=.15, vjust=.15 ))

ggsave("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalFigures_Out/Section_8_Units_by_CNTY.pdf", width = 10, height = 8)


ggplot(all_dat_group) + geom_bar(aes(x=CE_Present,y=Tax_Units_reported, fill=CE_Present), stat="identity", width = .5, show.legend = FALSE) + coord_flip() +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~County_ID) + 
  labs(title = "Tax Credit Units Reported by Census Tract", x='', y='Counts') +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle=-45, hjust=.15, vjust=.15 ))

ggsave("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalFigures_Out/Tax_Credit_Units_by_CNTY.pdf", width = 10, height = 8)

all_dat_group <- group_by(alldata,Tax_Units_reported,Sec_8_Reported) 

test <- melt(alldata[,c('Sec_8_Reported', 'Tax_Units_reported', 'CE_Present', 'County_ID')])

ggplot(test) + geom_bar(aes(x=CE_Present,y=value, fill=variable), stat="identity", width = .5, show.legend = TRUE) + coord_flip() +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~County_ID) + 
  labs(title = "Housing Units Reported by Census Tract", x='', y='Counts') +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle=-45, hjust=.15, vjust=.15 ), legend.position="bottom", legend.title = element_blank())

ggsave("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalFigures_Out/All_Units_Stacked_by_CNTY.pdf", width = 10, height = 8)

ggplot(test) + geom_bar(aes(x=CE_Present,y=value, fill=variable), stat="identity", position="dodge",  width = .5, show.legend = TRUE) + coord_flip() +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~County_ID) + 
  labs(title = "Housing Units Reported by Census Tract", x='', y='Counts') +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(size=8, angle=-45, hjust=.15, vjust=.15 ), legend.position="bottom", legend.title = element_blank())

ggsave("C:/Users/admin/Box Sync/Default Sync Folder/Projects/NSF_CNH/HUD_Analysis/FinalFigures_Out/All_Units_Grouped_by_CNTY.pdf", width = 10, height = 8)
