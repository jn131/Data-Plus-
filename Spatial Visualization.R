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
library(gridExtra)
library(grid)
library(scales)
library(plyr) # plyr has to run before dplyr 
library(dplyr)
library(magrittr)
library(reshape2)
library(data.table)

#### MORTALITY DATA 

# Open Deaths_2004_to_2014.zip 
deaths_zip <- readOGR(dsn = "Deaths_2004_to_2014", layer = "Deaths2004to2014")
deaths_unordered <- as.data.frame(deaths_zip)
deaths_total <- deaths_unordered[order(deaths_unordered$GeoIDBlkGr),]

# Pull out Durham County data from deaths_total 
nc_county_fips <- read.csv("county fips table.csv")
fips_durham <- nc_county_fips$FIPS[nc_county_fips$County == "Durham"]
deaths_durham <- deaths_total[which(unfactor(deaths_total$GeoIDBlkGr) >= fips_durham *10000000 & unfactor(deaths_total$GeoIDBlkGr) < (fips_durham + 1) *10000000),]
deaths_durham <- unfactor(deaths_durham)


#### SPATIAL DATA 

# Map of block groups in Durham
block_groups <- readOGR(dsn = "Durham_Block_Groups", layer = "DurhamBlockGroups", stringsAsFactors = F)
plot(block_groups)


#### POPULATION DATA 

## Open population by age file 
population_total <- read.csv("Population_by_Age.csv", stringsAsFactors = F)

## Reformat file

# Create GeoIDBlkGr column
x <- population_total$GISJOIN
population_total$GISJOIN <- paste(substr(x, 2, 3), substr(x, 5, 7), substr(x, 9, nchar(x)), sep='')

population_total <- population_total[, c(1, 41:106)]

# Rename remaining columns
setnames(population_total, 
         old=c("GISJOIN", "H76001", "H76002","H76003","H76004","H76005","H76006","H76007","H76008","H76009","H76010","H76011",
               "H76012","H76013","H76014","H76015","H76016","H76017","H76018","H76019","H76020","H76021","H76022","H76023",
               "H76024","H76025","H76026","H76027","H76028","H76029","H76030","H76031","H76032","H76033","H76034","H76035",
               "H76036","H76037","H76038","H76039","H76040","H76041","H76042","H76043","H76044","H76045","H76046","H76047",
               "H76048","H76049"),
         new=c("GeoIDBlkGr","Total","Males","M_Under5","M_5to9","M_10to14","M_15to17","M_18to19","M_20","M_21","M_22to24",
               "M_25to29","M_30to34","M_35to39","M_40to44","M_45to49","M_50to54","M_55to59","M_60to61","M_62to64","M_65to66",
               "M_67to69","M_70to74","M_75to79","M_80to84","M_85andOver","Females","F_Under5","F_5to9","F_10to14","F_15to17",
               "F_18to19","F_20","F_21","F_22to24","F_25to29","F_30to34","F_35to39","F_40to44","F_45to49","F_50to54","F_55to59",
               "F_60to61","F_62to64","F_65to66","F_67to69","F_70to74","F_75to79","F_80to84","F_85andOver"))

## Extract Durham County data 
population_durham <- population_total[which(unfactor(as.matrix(population_total$GeoIDBlkGr)) >= fips_durham *10000000 & unfactor(as.matrix(population_total$GeoIDBlkGr)) < (fips_durham + 1) *10000000),]


#### COUNT PLOTS

## Plot of a specific cause of death 

# Plot of cancer deaths 
x = deaths_durham %>% group_by(GeoIDBlkGr) %>% summarise(count_cause = sum(CauseOfDea == "Homicide")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data <- merge(block_groups, x, by="GEOID10")
spplot(block_groups_data, "count_cause", col.regions = brewer.pal(9, "Reds"), cuts = 8)

## Two plots side by side

# Plot 1 - male suicides 
x <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(cause = sum(CauseOfDea == "Suicide" & SEX == "M")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data_males <- merge(block_groups, x, by="GEOID10")
plot1 <- spplot(block_groups_data_males, "cause",
                col.regions = brewer.pal(9, "Reds"), cuts = 8, # Color of the graph
                at = seq(0, 6, by = 1), low = "snow", # Scale of the color gradient
                main = list(label = "Males", cex = 1.5)) # Title of the graph

# Plot 2 - female suicides 
y <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(cause = sum(CauseOfDea == "Suicide" & SEX == "F")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data_females <- merge(block_groups, y, by="GEOID10")
plot2 <- spplot(block_groups_data_females, "cause",
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                at = seq(0, 6, by = 1),
                main = list(label = "Females", cex = 1.5))

# Plot together
grid.arrange(plot1, plot2, nrow = 1, 
             top = textGrob("Number of Mortalities Due to Suicide in Durham County, 2004 to 2014", gp = gpar(fontsize = 20, fontface = "bold")))

#### RATE PLOTS  

## Plot of age-adjusted mortality rate for people aged 50 to 64 
x <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate = sum(SEX == "M" | SEX == "F")/sum(population_durham$Total) * 100000) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data <- merge(block_groups, x, by="GEOID10")
spplot(block_groups_data, "rate", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Mortality Rates by Block Group, Per 100,000 People", cex = 2))

x <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate = sum(SEX == "M" | SEX == "F")) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data <- merge(block_groups, x, by="GEOID10")
spplot(block_groups_data, "rate", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Mortality Rates by Block Group, Per 100,000 People", cex = 2))

age_distr <- (sum(population_total$M_50to54) + sum(population_total$F_50to54) + sum(population_total$M_55to59) + sum(population_total$F_55to59) + 
              sum(population_total$M_60to61) + sum(population_total$F_60to61) + sum(population_total$M_62to64) + sum(population_total$F_62to64))/sum(population_total$Total)
x <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate = sum(AGE >= "50" & AGE <= "64")/sum(population_durham$M_50to54 + population_durham$F_50to54 + 
                                                          population_durham$M_55to59 + population_durham$F_55to59 + population_durham$M_60to61 + population_durham$F_60to61 + 
                                                          population_durham$M_62to64 + population_durham$F_62to64) * 1000 * age_distr) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_data <- merge(block_groups, x, by="GEOID10")
spplot(block_groups_data, "rate", 
       col.regions = brewer.pal(9, "Reds"), cuts = 8, 
       at = seq(0, 0.14, by = 0.02),
       main = list(label = "Age-Adjusted Mortality Rates per 100,000 People, Ages 50 to 64 ", cex = 1.5))


#### Use to check values by block group 
stupid <- summary(as.factor(deaths_durham$GeoIDBlkGr == "370630017081" & deaths_durham$AGE >="50" & deaths_durham$AGE <= "59"))
stupid

## All age-adjusted mortalities for cause of disease = cancer 

# Plot 1: 9 and Under 
age_distr_0to9 <- (sum(population_total$M_Under5) + sum(population_total$F_Under5) + sum(population_total$M_5to9) + sum(population_total$F_5to9))/sum(population_total$Total)
data_0to9 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_0to9 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "0" & AGE <= "9")
                                                                  /sum(population_durham$M_Under5 + population_durham$F_Under5 + 
                                                                  population_durham$M_5to9 + population_durham$F_5to9) * 1000 * age_distr_0to9) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_0to9 <- merge(block_groups, data_0to9, by="GEOID10")
plot1 <- spplot(block_groups_0to9, "rate_0to9", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.25, by = 0.05),
                main = list(label = "Ages 9 and Under", cex = 1)) 

# Plot 2: 10-19 
age_distr_10to19 <- (sum(population_total$M_10to14) + sum(population_total$F_10to14) + sum(population_total$M_15to17) + sum(population_total$F_15to17) + sum(population_total$M_18to19) + sum(population_total$F_18to19))/sum(population_total$Total)
data_10to19 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_10to19 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "10" & AGE <= "19")
                                                                    /sum(population_durham$M_10to14 + population_durham$F_10to14 + population_durham$M_15to17 + population_durham$F_15to17 + 
                                                                    population_durham$M_18to19 + population_durham$F_18to19) * 1000 * age_distr_10to19) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_10to19 <- merge(block_groups, data_10to19, by="GEOID10")
plot2 <- spplot(block_groups_10to19, "rate_10to19", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.12, by = 0.02),
                main = list(label = "Ages 10 to 19", cex = 1))

# Plot 3: 20-29 
age_distr_20to29 <- (sum(population_total$M_20) + sum(population_total$F_20) + sum(population_total$M_21) + sum(population_total$F_21) + sum(population_total$M_22to24) + sum(population_total$F_22to24) + sum(population_total$M_25to29) + sum(population_total$F_25to29))/sum(population_total$Total)
data_20to29 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_20to29 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "20" & AGE <= "29")
                                                                    /sum(population_durham$M_20 + population_durham$F_20 + population_durham$M_21 + population_durham$F_21 + 
                                                                    population_durham$M_22to24 + population_durham$F_22to24 + population_durham$M_25to29 + population_durham$F_25to29) * 1000 * age_distr_20to29) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_20to29 <- merge(block_groups, data_20to29, by="GEOID10")
plot3 <- spplot(block_groups_20to29, "rate_20to29", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.27, by = 0.03),
                main = list(label = "Ages 20 to 29", cex = 1))

# Plot 4: 30-39
age_distr_30to39 <- (sum(population_total$M_30to34) + sum(population_total$F_30to34) + sum(population_total$M_35to39) + sum(population_total$F_35to39))/sum(population_total$Total)
data_30to39 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_30to39 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "30" & AGE <= "39")
                                                                    /sum(population_durham$M_30to34 + population_durham$F_30to34 + 
                                                                    population_durham$M_35to39 + population_durham$F_35to39) * 1000 * age_distr_30to39) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_30to39 <- merge(block_groups, data_30to39, by="GEOID10")
plot4 <- spplot(block_groups_30to39, "rate_30to39", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
               # at = seq(0, 0.27, by = 0.03),
                main = list(label = "Ages 30 to 39", cex = 1))

# Plot 5: 40-49
age_distr_40to49 <- (sum(population_total$M_40to44) + sum(population_total$F_40to44) + sum(population_total$M_45to49) + sum(population_total$F_45to49))/sum(population_total$Total)
data_40to49 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_40to49 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "40" & AGE <= "49")
                                                                    /sum(population_durham$M_40to44 + population_durham$F_40to44 + 
                                                                    population_durham$M_45to49 + population_durham$F_45to49) * 1000 * age_distr_40to49) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_40to49 <- merge(block_groups, data_40to49, by="GEOID10")
plot5 <- spplot(block_groups_40to49, "rate_40to49", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.27, by = 0.03),
                main = list(label = "Ages 40 to 49", cex = 1))

# Plot 6: 50-59
age_distr_50to59 <- (sum(population_total$M_50to54) + sum(population_total$F_50to54) + sum(population_total$M_55to59) + sum(population_total$F_55to59))/sum(population_total$Total)
age_distr_50to59
data_50to59 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_50to59 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "50" & AGE <= "59")
                                                                    /sum(population_durham$M_50to54 + population_durham$F_50to54 + 
                                                                    population_durham$M_55to59 + population_durham$F_55to59) * 1000 * age_distr_50to59) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_50to59 <- merge(block_groups, data_50to59, by="GEOID10")
plot6 <- spplot(block_groups_50to59, "rate_50to59", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.27, by = 0.03),
                main = list(label = "Ages 50 to 59", cex = 2))
plot6
# Plot 7: 60-69
age_distr_60to69 <- (sum(population_total$M_60to61) + sum(population_total$F_60to61) + sum(population_total$M_62to64) + sum(population_total$F_62to64) + 
                     sum(population_total$M_65to66) + sum(population_total$F_65to66) + sum(population_total$M_67to69) + sum(population_total$F_67to69))/sum(population_total$Total)
data_60to69 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_60to69 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "60" & AGE <= "69")/sum(population_durham$M_60to61 + population_durham$F_60to61 + 
                                                                    population_durham$M_62to64 + population_durham$F_62to64 + population_durham$M_65to66 + population_durham$F_65to66 + 
                                                                    population_durham$M_67to69 + population_durham$F_67to69) * 1000 * age_distr_60to69) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_60to69 <- merge(block_groups, data_60to69, by="GEOID10")
plot7 <- spplot(block_groups_60to69, "rate_60to69", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.25, by = 0.05),
                main = list(label = "Ages 60 to 69", cex = 1))
plot7
# Plot 8: 70-79 
age_distr_70to79 <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79))/sum(population_total$Total)
data_70to79 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_70to79 = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "70" & AGE <= "79")
                                                                    /sum(population_durham$M_70to74 + population_durham$F_70to74 + 
                                                                    population_durham$M_75to79 + population_durham$F_75to79) * 1000 * age_distr_70to79) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_70to79 <- merge(block_groups, data_70to79, by="GEOID10")
plot8 <- spplot(block_groups_70to79, "rate_70to79", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.7, by = 0.1),
                main = list(label = "Ages 70 to 79", cex = 1))


# Plot 9: 80 and Over
age_distr_80Plus <- (sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
data_80Plus <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_80Plus = sum(CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer" & AGE >= "80")
                                                                    /sum(population_durham$M_80to84 + population_durham$F_80to84 + 
                                                                    population_durham$M_85andOver + population_durham$F_85andOver) * 1000 * age_distr_80Plus) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_80Plus <- merge(block_groups, data_80Plus, by="GEOID10")
plot9 <- spplot(block_groups_80Plus, "rate_80Plus", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
               # at = seq(0, .9, by = 0.1),
                main = list(label = "Ages 80 and Over", cex = 1))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, nrow = 3, 
             top = textGrob("Age-Adjusted Mortality Rates due to Cancer Per 1,000 People", gp = gpar(fontsize = 20, fontface = "bold")))


######### THINGS YOU DON'T NEED 
## Graph of age-adjusted mortalities for different causes of death, 70 and Over 

# Plot 10: Heart Disease 
age_distr_70Plus <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79) + 
                       sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
data_70Plus_heartdisease <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_70Plus_heartdisease = sum(CauseOfDea == "Heart Disease" & AGE >= "70")/sum(population_durham$M_70to74 + population_durham$F_70to74 + 
                                                                                                                     population_durham$M_75to79 + population_durham$F_75to79 + population_durham$M_80to84 + population_durham$F_80to84 + 
                                                                                                                     population_durham$M_85andOver + population_durham$F_85andOver) * 100000 * age_distr_70Plus) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_70Plus_heartdisease <- merge(block_groups, data_70Plus_heartdisease, by="GEOID10")
plot10 <- spplot(block_groups_70Plus_heartdisease, "rate_70Plus_heartdisease", 
                col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                #at = seq(0, 0.25, by = 0.05),
                main = list(label = "Heart Disease", cex = 1.5))


# Plot 11: Cancer 
age_distr_70Plus <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79) + 
                       sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
data_70Plus_cancer <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_70Plus_cancer = sum(CauseOfDea == "Cancer" & AGE >= "70")/sum(population_durham$M_70to74 + population_durham$F_70to74 + 
                                                                                                                                                                   population_durham$M_75to79 + population_durham$F_75to79 + population_durham$M_80to84 + population_durham$F_80to84 + 
                                                                                                                                                                   population_durham$M_85andOver + population_durham$F_85andOver) * 100000 * age_distr_70Plus) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_70Plus_cancer <- merge(block_groups, data_70Plus_cancer, by="GEOID10")
plot11 <- spplot(block_groups_70Plus_cancer, "rate_70Plus_cancer", 
                 col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                 #at = seq(0, 0.25, by = 0.05),
                 main = list(label = "Cancer", cex = 1.5))

# Plot 12: Dementia  
age_distr_70Plus <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79) + 
                       sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
data_70Plus_dementia <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_70Plus_dementia = sum(CauseOfDea == "Dementia and Other Mental and Behavior Disorders" & AGE >= "70")/sum(population_durham$M_70to74 + population_durham$F_70to74 + 
                                                                                                                                                                   population_durham$M_75to79 + population_durham$F_75to79 + population_durham$M_80to84 + population_durham$F_80to84 + 
                                                                                                                                                                   population_durham$M_85andOver + population_durham$F_85andOver) * 100000 * age_distr_70Plus) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_70Plus_dementia <- merge(block_groups, data_70Plus_dementia, by="GEOID10")
plot12 <- spplot(block_groups_70Plus_dementia, "rate_70Plus_dementia", 
                 col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                 #at = seq(0, 0.25, by = 0.05),
                 main = list(label = "Dementia and Other Mental and Behavior Disorders", cex = 1.5))

# Plot 13: Alzheimer's 
age_distr_70Plus <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79) + 
                       sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
data_70Plus_alzheimers <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(rate_70Plus_alzheimers = sum(CauseOfDea == "Alzheimer's Disease" & AGE >= "70")/sum(population_durham$M_70to74 + population_durham$F_70to74 + 
                                                                                                                                                                   population_durham$M_75to79 + population_durham$F_75to79 + population_durham$M_80to84 + population_durham$F_80to84 + 
                                                                                                                                                                   population_durham$M_85andOver + population_durham$F_85andOver) * 100000 * age_distr_70Plus) %>% rename(GEOID10 = GeoIDBlkGr)
block_groups_70Plus_alzheimers <- merge(block_groups, data_70Plus_alzheimers, by="GEOID10")
plot13 <- spplot(block_groups_70Plus_alzheimers, "rate_70Plus_alzheimers", 
                 col.regions = brewer.pal(9, "Reds"), cuts = 8, 
                 #at = seq(0, 0.25, by = 0.05),
                 main = list(label = "Alzheimer's Disease", cex = 1.5))

