#.......................
# Basic Maps
# Jessica Needleman
#.......................


## Block Group Plot = Durham County

block_groups <- readOGR(dsn = "Durham_Block_Groups", layer = "DurhamBlockGroups", stringsAsFactors = F)
plot(block_groups) 


## Mortality Count Maps 
# Specifications: YEAR, AGE, SEX, HISPANIC, RACE, ACME, MENT1, MENT2, MENT3, MENT4, MENT5, CauseOfDea

count_blckgrp <- deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(count = sum(n())) %>% dplyr::rename(GEOID10 = GeoIDBlkGr) 
merged_data <- merge(block_groups, count_blckgrp, by="GEOID10")  
spplot(merged_data, "count", 
       col.regions = brewer.pal(9, "Blues"), cuts = 8, 
       at = seq(0, 800, by = 100), # Optional Scale
       main = list(label = "Total Mortality Counts", cex = 2)) 

## Mortality Rate Maps

count_blckgrp <- deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(count = sum(n()) * 1000/11)
rate_blkgrp <- merge(count_blckgrp, population_county, by = "GeoIDBlkGr") %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(rate = sum(count/Total)) %>% dplyr::rename(GEOID10 = GeoIDBlkGr) 
rate_blkgrp[153,2] = 0 # Sets block group 370639801001 to zero

merged_data <- merge(block_groups, rate_blkgrp, by="GEOID10") 
spplot(merged_data, "rate",
       col.regions = brewer.pal(9, "Blues"), cuts= 8,
       at = seq(0, 40, by = 5), # Optional Scale
       main=list(label = "Mortality Rates by Block Group, Per 1,000 People", cex = 2)) 


## Age-Adjusted Mortality Rate Maps: Ex. Age-Adjusted Cancer Mortality Rates 

cancer <- c("Cancer","Breast Cancer","Colon, Rectum and Anus Cancer","Prostate Cancer","Trachea, Bronchus and Lung Cancer")
age.f <- function(y) sub('^0+(?=[0-9])', '', y, perl=TRUE)
setname.f <- function(x) setnames(x, old = colnames(x), new = c("GEOID10","count","population"))
ageadj_pop.f <- function(pop, y, z) {
  ageadj_rate = setname.f(cbind(deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(
    count = sum(CauseOfDea %in% c(cancer) & 
                  AGE >= as.numeric(age.f(substr(colnames(pop[y]), 1, 3))) & AGE <= as.numeric(age.f(substr(colnames(pop[y]), 5, 7)))) * 1000/11  * age_distr[1, y-2]) %>% rename(GEOID10 = GeoIDBlkGr), 
    pop[,paste(substr(colnames(pop[y]), 1, 3), "-",substr(colnames(pop[y]), 5, 7), sep = "")])) %>% dplyr::group_by(GEOID10) %>% dplyr::summarize(rate = sum(count/population))
  setnames(ageadj_rate, old = "rate", new = paste(z, sep = ""))
}
ageadj.f <- function(y, z) ageadj_pop.f(pop_ageadj_total, y, z)
ageadj <- plyr::join_all(list(a <- ageadj.f(3,"a"), ageadj.f(4,"b"), ageadj.f(5,"c"), ageadj.f(6,"d"), ageadj.f(7,"e"), ageadj.f(8,"f"), ageadj.f(9,"g"), ageadj.f(10,"h"), ageadj.f(11,"i"), ageadj.f(12,"j"), ageadj.f(13,"k")), by = "GEOID10")
ageadj[is.na(ageadj)] <- 0
total_ageadj <- ageadj %>% dplyr::group_by(GEOID10) %>% summarize(total_rate = sum(sum(a, b, c, d, e, f, g, h, i, j, k)))
merged_data <- merge(block_groups, total_ageadj, by="GEOID10") 

spplot(merged_data, "total_rate", 
       col.regions = brewer.pal(9, "Blues"), cuts= 8, 
       #at = seq(0, 16, by = 2),# Optional Scale 
       main = list(label = "Age-Adjusted Cancer Mortality Rates, Per 1,000 People", cex = 2)) 


## Multiple Plots 

# Plot 1: Male Suicides 
count_blckgrp1 <- deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(count1 = sum(CauseOfDea == "Suicide" & SEX == "M")) %>% dplyr::rename(GEOID10 = GeoIDBlkGr) 
merged_data1 <- merge(block_groups, count_blckgrp1, by="GEOID10")  
plot1 <- spplot(merged_data1, "count1", 
                col.regions = brewer.pal(9, "Blues"), cuts = 8, 
                at = seq(0, 6, by = 1), # Standardizes Scale
                main = list(label = "Males", cex = 2)) 

# Plot 2: Female Suicides 
count_blckgrp2 <- deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(count2 = sum(CauseOfDea == "Suicide" & SEX == "F")) %>% dplyr::rename(GEOID10 = GeoIDBlkGr) 
merged_data2 <- merge(block_groups, count_blckgrp2, by="GEOID10")  
plot2 <- spplot(merged_data2, "count2", 
                col.regions = brewer.pal(9, "Blues"), cuts = 8, 
                at = seq(0, 6, by = 1), # Standardizes Scale
                main = list(label = "Females", cex = 2)) 

# Plots 1 and 2 Side-By-Side
grid.arrange(plot1, plot2, nrow = 1, 
             top = textGrob("Suicide Counts in Males and Females", gp = gpar(fontsize = 20, fontface = "bold"))) 

