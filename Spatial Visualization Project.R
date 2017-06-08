#.......................
# Durham County Mortality Data
# Jessica Needleman
#.......................

# Working directory 
setwd("~/Desktop/Data+/Project Data")

# Libraries used  
library(rgdal)
library(sp)
library(rgeos)
library(varhandle)
library(tidyverse)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(reshape2)
library(data.table)


#### HEALTH DATA FILES

# Open Deaths_2004_to_2014.zip 
deaths_zip <- readOGR(dsn = "Deaths_2004_to_2014", layer = "Deaths2004to2014", stringsAsFactors = F)
deaths_unordered <- as.data.frame(deaths_zip)
deaths_total <- deaths_unordered[order(deaths_unordered$GeoIDBlkGr),]

# Pull out Durham County data from deaths_total 
nc_county_fips <- read.csv("county fips table.csv")
fips_durham <- nc_county_fips$FIPS[nc_county_fips$County == "Durham"]
deaths_durham <- deaths_total[which(unfactor(deaths_total$GeoIDBlkGr) >= fips_durham *10000000 & unfactor(deaths_total$GeoIDBlkGr) < (fips_durham + 1) *10000000),]
deaths_durham <- unfactor(deaths_durham)

# Map of block groups in Durham
block_groups <- readOGR(dsn = "Durham_Block_Groups", layer = "DurhamBlockGroups", stringsAsFactors = F)
plot(block_groups)


#### POPULATION BY AGE DATA

# Function to create GEOIDs
row_to_geo <- function(ex){
  
  if (nchar(ex)==0){
    return(NULL)
  }
  
  # Split up different parts of geocode mentioned
  bits <- unlist(strsplit(ex, ', '))
  bits <- do.call(rbind, strsplit(bits, ';'))
  
  # Remove unnecessary characters, left with census tract number
  census_trct = gsub("[A-z]","", gsub("\\s","",bits[2]))
  
  # If census tract is not a decimal
  if (!grepl("\\.", census_trct)){
    two = census_trct
    three = "00"
  }else{
    # If census tract is a decimal 
    x = str_locate(census_trct, "\\.")
    two = substr(census_trct, 1, x-1)
    three = substr(census_trct, x+1, nchar(census_trct))
  }
  
  # Add leading zeros
  num0 = 4-nchar(two)
  i = 0
  while (i < num0) {
    two = paste0("0", two)
    i = i+1
  }
  
  # Combine all elements to create full GEOID
  ex = paste0(fips_durham, two, three, gsub("\\D","",bits[1]))
}

# Retrieve data
age_data <- as.data.frame(read.csv("Population_By_Age.csv"))

# Because file format is awful
age_data[2] <- NULL; age_data[2] <- NULL
age_data[3] <- NULL; age_data[3] <- NULL

# Transpose function
adf = t(age_data)

# Descriptions to GEOIDs
i = 2
while (i < 155) {
  adf[i,4] <- row_to_geo(adf[i,4])
  i = i+1
}

# Data frame to csv file
options(scipen = 999)
write.csv(file="Age Population Data by Block Group.csv", x=adf)
age_population <- read.csv("Age Population Data by Block Group.csv")
age_population <- unfactor(age_population)

#### REFORMAT AGE POPULATION FILE (specific to this file)

# Removing unnecessary rows and columns 
age_population <- age_population[c(2:154), c(5:54)]

#Renaming remaining columns
setnames(age_population, 
         old=c("V4", "V5", "V6", "V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21",
               "V22","V23","V24","V25","V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39",
               "V40","V41","V42","V43","V44","V45","V46","V47","V48","V49","V50","V51","V52","V53"), 
         new=c("GeoIDBlkGr","Total","Males","M_Under5","M_5to9","M_10to14","M_15to17","M_18to19","M_20","M_21","M_22to24",
               "M_25to29","M_30to34","M_35to39","M_40to44","M_45to49","M_50to54","M_55to59","M_60to61","M_62to64","M_65to66",
               "M_67to69","M_70to74","M_75to79","M_80to84","M_85andOver","Females","F_Under5","F_5to9","F_10to14","F_15to17",
               "F_18to19","F_20","F_21","F_22to24","F_25to29","F_30to34","F_35to39","F_40to44","F_45to49","F_50to54","F_55to59",
               "F_60to61","F_62to64","F_65to66","F_67to69","F_70to74","F_75to79","F_80to84","F_85andOver"))

#### MORTALITY RATES
block_groups_population <- merge(deaths_durham, age_population, by = "GeoIDBlkGr")
x <- block_groups_population %>% group_by(GeoIDBlkGr) %>% summarize(rate = sum(SEX == "F" & SEX == "M")/as.numeric(block_groups_population$Total)) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data <- merge(block_groups, x, by="GeoIDBlkGr")
spplot(block_groups_data, "rate", col.regions = brewer.pal(9, "Reds"), cuts = 8)


#### SUICIDE

## Males and females side by side 

# Males
x <- deaths_durham %>% group_by(GeoIDBlkGr)  %>% summarize(cause = sum(CauseOfDea == "Suicide" & SEX == "M")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data_males <- merge(block_groups, x, by="GEOID10")
plot1 <- spplot(block_groups_data_males, "cause", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, # Color of the graph
                at = seq(0, 6, by = 1), # Scale of the color gradient 
                sub = "Males") # Title of the graph, found at the bottom of the graph

# Females
y <- deaths_durham %>% group_by(GeoIDBlkGr)  %>% summarize(cause = sum(CauseOfDea == "Suicide" & SEX == "F")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data_females <- merge(block_groups, y, by="GEOID10")
plot2 <- spplot(block_groups_data_females, "cause", 
                col.regions = brewer.pal(9, "Reds"), 
                cuts = 8, at = seq(0, 6, by = 1),
                sub = "Females")

# Plot
grid.arrange(plot1, plot2, nrow = 1, top = "Mortality Due to Suicide in Durham County, 2004 to 2014")



# Function to create GEOIDs
row_to_geo <- function(ex){
  
  if (nchar(ex)==0){
    return(NULL)
  }
  
  # Split up different parts of geocode mentioned
  bits <- unlist(strsplit(ex, ', '))
  bits <- do.call(rbind, strsplit(bits, ';'))
  
  # Remove unnecessary characters, left with census tract number
  census_trct = gsub("[A-z]","", gsub("\\s","",bits[2]))
  
  # If census tract is not a decimal
  if (!grepl("\\.", census_trct)){
    two = census_trct
    three = "00"
  }else{
    # If census tract is a decimal 
    x = str_locate(census_trct, "\\.")
    two = substr(census_trct, 1, x-1)
    three = substr(census_trct, x+1, nchar(census_trct))
  }
  
  # Add leading zeros
  num0 = 4-nchar(two)
  i = 0
  while (i < num0) {
    two = paste0("0", two)
    i = i+1
  }
  
  # Combine all elements to create full GEOID
  ex = paste0(fips_durham, two, three, gsub("\\D","",bits[1]))
}

# Retrieve data
age_data <- as.data.frame(read.csv("Population_By_Age.csv"))

# Because file format is awful
age_data[2] <- NULL; age_data[2] <- NULL
age_data[3] <- NULL; age_data[3] <- NULL

# Transpose function
adf = t(age_data)

# Descriptions to GEOIDs
i = 2
while (i < 155) {
  adf[i,4] <- row_to_geo(adf[i,4])
  i = i+1
}
library(stringr)
library(magrittr)
library(reshape2)
library(data.table)
