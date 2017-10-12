#.......................
# Creating Datasets
# Jessica Needleman
#.......................

#### Set Working Directory
setwd("~/Desktop/College/Data+/Project Data")

#### Set County
county <- "Durham"
county_dsn <- "Durham_Block_Groups"
county_layer <- "DurhamBlockGroups"

## Download Libraries 
library(rgdal) # readOGR 
library(sp) # spplot, merge 
library(RColorBrewer) # brewer.pal
library(gridExtra) # grid.arrange
library(grid) # textGrob
library(plyr) # ddply
library(dplyr) # group_by, summarize, rename
library(data.table) # setnames


## Mortality Data 

# deaths_total: Mortality Data for All of North Carolina
deaths_zip <- readOGR(dsn = "Deaths2004to2014", layer = "Deaths2004to2014", stringsAsFactors = F) 
deaths_total <- as.data.frame(deaths_zip) 


# deaths_county: Specific County Data Pulled Out of deaths_total 
nc_fips <- read.csv("county fips table.csv") 
fips_county <- nc_fips$FIPS[nc_fips$County == county] 
options(scipen=999)
lowerbound <- fips_county * 10000000
upperbound <- (fips_county + 1) * 10000000
deaths_county <- deaths_total[which(as.matrix(deaths_total$GeoIDBlkGr) >= lowerbound & as.matrix(deaths_total$GeoIDBlkGr) < upperbound),]
deaths_county$AGE <- lapply(deaths_county$AGE, function(y) sub('^0+(?=[0-9])', '', y, perl=TRUE))
deaths_county$AGE <- as.numeric(deaths_county$AGE)
deaths_county$AGE[is.na(deaths_county$AGE)] <- 100


## Population Data

# Open File: population_total
population_total <- read.csv("North_Carolina_Population.csv", stringsAsFactors = F) 

# Reformat File 
x <- population_total$GISJOIN
population_total$GISJOIN <- paste(substr(x, 2, 3), substr(x, 5, 7), substr(x, 9, nchar(x)), sep='') 
population_total <- population_total[, c(1, 42:106)] 
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

# Additional Variables:
# H7Z002 = Not Hispanic or Latino
# H7Z003 = Not Hispanic or Latino: White alone
# H7Z004 = Not Hispanic or Latino: Black or African American alone
# H7Z005 = Not Hispanic or Latino: American Indian and Alaska Native alone
# H7Z006 = Not Hispanic or Latino: Asian alone
# H7Z007 = Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
# H7Z008 = Not Hispanic or Latino: Some Other Race alone
# H7Z009 = Not Hispanic or Latino: Two or More Races
# H7Z010 = Hispanic or Latino
# H7Z011 = Hispanic or Latino: White alone
# H7Z012 = Hispanic or Latino: Black or African American alone
# H7Z013 = Hispanic or Latino: American Indian and Alaska Native alone
# H7Z014 = Hispanic or Latino: Asian alone
# H7Z015 = Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
# H7Z016 = Hispanic or Latino: Some Other Race alone
# H7Z017 = Hispanic or Latino: Two or More Races

# Extract County Population Data: population_county 
population_county <- population_total[which(as.matrix(population_total$GeoIDBlkGr) >= lowerbound & as.matrix(population_total$GeoIDBlkGr) < upperbound),]


## Block Group Data 
block_groups <- readOGR(dsn = county_dsn, layer = county_layer, stringsAsFactors = F)


## Age-Distribution

# age_distr
max_age <- max(as.numeric(deaths_county$AGE)) 
age_names <- c("000-004","005-009","010-019","020-029","030-039","040-049","050-059","060-069","070-079","080-084", paste("085-", max_age, sep = ""))
agedistr.f <- function(x) sum(colSums(population_total[,x]))/(colSums(population_total[18]))
age_distr <- cbind(as.data.frame(matrix(agedistr.f(c(20,44)), agedistr.f(c(21,45)), nrow = 1, ncol = 2)), agedistr.f(c(22:24,46:48)), 
                   agedistr.f(c(25:28,49:52)), agedistr.f(c(29:30,53:54)), agedistr.f(c(31:32,55:56)), agedistr.f(c(33:34,57:58)), 
                   agedistr.f(c(35:38,59:62)), agedistr.f(c(39:40,63:64)), agedistr.f(c(41,65)), agedistr.f(c(42,66)))
setnames(age_distr, old = colnames(age_distr), new = age_names)


## Age-Adjusted Datasets

sumcol.f <- function(x,y) apply(population_county[,c(x:y)], 1, sum)

# pop_ageadj_females 
pop_ageadj_females <- cbind(population_county[,c(1, 43:45)], sumcol.f(46,48), sumcol.f(49,52), sumcol.f(53,54), sumcol.f(55,56), sumcol.f(57,58),
                            sumcol.f(59,62), sumcol.f(63,64), population_county[,65:66])
setnames(pop_ageadj_females, old = colnames(pop_ageadj_females), new = c("GEOID10", "Total", age_names))
pop_ageadj_females[pop_ageadj_females == 0] <- NA

# pop_ageadj_males 
pop_ageadj_males <- cbind(population_county[,c(1, 19:21)], sumcol.f(22,24), sumcol.f(25,28), sumcol.f(29,30), sumcol.f(31,32), sumcol.f(33,34),
                          sumcol.f(35,38), sumcol.f(39,40), population_county[,41:42])
setnames(pop_ageadj_males, old = colnames(pop_ageadj_males), new = c("GEOID10", "Total", age_names))
pop_ageadj_males[pop_ageadj_males == 0] <- NA

# pop_ageadj_total
pop_ageadj_total <- plyr::ddply(cbind(names=c(rownames(pop_ageadj_males), rownames(pop_ageadj_females)), rbind.fill(list(pop_ageadj_males, pop_ageadj_females))), 
                                .(GEOID10), function(x) colSums(x[,3:14], na.rm = TRUE))
pop_ageadj_total[pop_ageadj_total == 0] <- NA


