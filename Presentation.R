
# Mortality Rates by Block Group
mortality = deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_mort = sum(SEX == "M" | SEX == "F")*1000/11) 
data_mortality = merge(mortality, population_durham)
data_mortality$rate_mortality = data_mortality$count_mort/data_mortality$Total
final_mortality = data_mortality %>% group_by(GeoIDBlkGr) %>% summarise(rate_mortality = sum(rate_mortality)) %>% rename(GEOID10 = GeoIDBlkGr)
final_mortality[153,2]=0 # Sets block group 980100 to zero 

block_groups_data <- merge(block_groups, final_mortality, by="GEOID10")
spplot(block_groups_data, "rate_mortality", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Mortality Rates by Block Group, Per 1,000 People", cex = 2))


# Age-Adjusted Mortality Rates for Ages 50 to 59
age_distr_50to59 <- (sum(population_total$M_50to54) + sum(population_total$F_50to54) + sum(population_total$M_55to59) + sum(population_total$F_55to59))/sum(population_total$Total)
mort_50to59 = deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_mort50to59 = sum((SEX == "M" | SEX == "F") & AGE >= 50 & AGE <= 59 )*1000/11 * age_distr_50to59) 
data_mort_50to59 = merge(mort_50to59, population_durham)
data_mort_50to59$rate_mortality50to59 = data_mort_50to59$count_mort50to59/(data_mort_50to59$M_50to54 + data_mort_50to59$F_50to54 + data_mort_50to59$M_55to59 + data_mort_50to59$F_55to59)
final_mort_50to59 = data_mort_50to59 %>% group_by(GeoIDBlkGr) %>% summarise(rate_mortality50to59 = sum(rate_mortality50to59)) %>% rename(GEOID10 = GeoIDBlkGr)
final_mort_50to59[153,2]=0 
final_mort_50to59[45,2]=0

block_groups_data <- merge(block_groups, final_mort_50to59, by="GEOID10")
spplot(block_groups_data, "rate_mortality50to59", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Age-Adjusted Mortality Rates for Ages 50 to 59", cex = 2))

# DOES CODE WORK? YES. 
summary(as.factor(deaths_durham$GeoIDBlkGr == "370630015011" & (deaths_durham$CauseOfDea == "Cancer" | deaths_durham$CauseOfDea == "Breast Cancer" | deaths_durham$CauseOfDea == "Colon, Rectum and Anus Cancer" | deaths_durham$CauseOfDea == "Prostate Cancer" | deaths_durham$CauseOfDea == "Trachea, Bronchus and Lung Cancer"))) # & deaths_durham$AGE >= 50 & deaths_durham$AGE <= 59))
summary(as.factor(population_durham$M_50to54[which(population_durham$GeoIDBlkGr == "370630015011")]))
summary(as.factor(population_durham$F_50to54[which(population_durham$GeoIDBlkGr == "370630015011")]))
summary(as.factor(population_durham$M_55to59[which(population_durham$GeoIDBlkGr == "370630015011")]))
summary(as.factor(population_durham$F_55to59[which(population_durham$GeoIDBlkGr == "370630015011")]))
summary(as.factor(population_durham$M_18to19[which(population_durham$GeoIDBlkGr == "370630015011")]))
summary(as.factor(population_durham$F_18to19[which(population_durham$GeoIDBlkGr == "370630015011")]))

1/(9 + 7 + 9 + 6 + 20 + 13) * 1000/11 * age_distr_10to19
summary(as.factor(deaths_durham$CauseOfDea[which(deaths_durham$AGE >= 0 & deaths_durham$AGE <= 9)]))


## Age-Adjusted Mortality Rates due to Cancer 

# Plot 1: 9 and Under 
age_distr_0to9 <- (sum(population_total$M_Under5) + sum(population_total$F_Under5) + sum(population_total$M_5to9) + sum(population_total$F_5to9))/sum(population_total$Total)
num_0to9 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_0to9 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE <= 9 ) * 1000/11 * age_distr_0to9) 
data_0to9 = merge(num_0to9, population_durham)
data_0to9$rate_0to9 = data_0to9$count_0to9/(data_0to9$M_Under5 + data_0to9$F_Under5 + data_0to9$M_5to9 + data_0to9$F_5to9)
final_0to9 = data_0to9 %>% group_by(GeoIDBlkGr) %>% summarize(rate_0to9 = sum(rate_0to9)) %>% rename(GEOID10 = GeoIDBlkGr)
final_0to9[153,2]=0 

block_groups_data <- merge(block_groups, final_0to9, by="GEOID10")
plot1 <- spplot(block_groups_data, "rate_0to9", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Ages 9 and Under", cex = 1))

# Plot 2: 10-19 
age_distr_10to19 <- (sum(population_total$M_10to14) + sum(population_total$F_10to14) + sum(population_total$M_15to17) + sum(population_total$F_15to17) + sum(population_total$M_18to19) + sum(population_total$F_18to19))/sum(population_total$Total)
num_10to19 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_10to19 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 10 & AGE <= 19 ) * 1000/11 * age_distr_10to19) 
data_10to19 = merge(num_10to19, population_durham)
data_10to19$rate_10to19 = data_10to19$count_10to19/(data_10to19$M_10to14 + data_10to19$F_10to14 + data_10to19$M_15to17 + data_10to19$F_15to17 + data_10to19$M_18to19 + data_10to19$F_18to19)
final_10to19 = data_10to19 %>% group_by(GeoIDBlkGr) %>% summarize(rate_10to19 = sum(rate_10to19)) %>% rename(GEOID10 = GeoIDBlkGr)
final_10to19[153,2]=0 
final_10to19[123,2]=0 
final_10to19[45,2]=0

block_groups_data <- merge(block_groups, final_10to19, by="GEOID10")
plot2 <- spplot(block_groups_data, "rate_10to19", 
       col.regions = brewer.pal(9, "Reds"), cuts= 8, 
       main=list(label = "Ages 10 to 19", cex = 1))

# Plot 3: 20-29 
age_distr_20to29 <- (sum(population_total$M_20) + sum(population_total$F_20) + sum(population_total$M_21) + sum(population_total$F_21) + sum(population_total$M_22to24) + sum(population_total$F_22to24) + sum(population_total$M_25to29) + sum(population_total$F_25to29))/sum(population_total$Total)
num_20to29 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_20to29 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 20 & AGE <= 29 ) * 1000/11 * age_distr_20to29) 
data_20to29 = merge(num_20to29, population_durham)
data_20to29$rate_20to29 = data_20to29$count_20to29/(data_20to29$M_20 + data_20to29$F_20 + data_20to29$M_21 + data_20to29$F_21 + data_20to29$M_22to24 + data_20to29$F_22to24 + data_20to29$M_25to29 + data_20to29$F_25to29)
final_20to29 = data_20to29 %>% group_by(GeoIDBlkGr) %>% summarize(rate_20to29 = sum(rate_20to29)) %>% rename(GEOID10 = GeoIDBlkGr)
final_20to29[153,2]=0 
final_20to29[123,2]=0
final_20to29[45,2]=0

block_groups_data <- merge(block_groups, final_20to29, by="GEOID10")
plot3 <- spplot(block_groups_data, "rate_20to29", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 20 to 29", cex = 1))

# Plot 4: 30-39
age_distr_30to39 <- (sum(population_total$M_30to34) + sum(population_total$F_30to34) + sum(population_total$M_35to39) + sum(population_total$F_35to39))/sum(population_total$Total)
num_30to39 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_30to39 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 30 & AGE <= 39 ) * 1000/11 * age_distr_30to39) 
data_30to39 = merge(num_30to39, population_durham)
data_30to39$rate_30to39 = data_30to39$count_30to39/(data_30to39$M_30to34 + data_30to39$F_30to34 + data_30to39$M_35to39 + data_30to39$F_35to39)
final_30to39 = data_30to39 %>% group_by(GeoIDBlkGr) %>% summarize(rate_30to39 = sum(rate_30to39)) %>% rename(GEOID10 = GeoIDBlkGr)
final_30to39[153,2]=0 
final_30to39[123,2]=0
final_30to39[45,2]=0

block_groups_data <- merge(block_groups, final_30to39, by="GEOID10")
plot4 <- spplot(block_groups_data, "rate_30to39", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 30 to 39", cex = 1))

# Plot 5: 40-49
age_distr_40to49 <- (sum(population_total$M_40to44) + sum(population_total$F_40to44) + sum(population_total$M_45to49) + sum(population_total$F_45to49))/sum(population_total$Total)
num_40to49 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_40to49 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 40 & AGE <= 49 ) * 1000/11 * age_distr_40to49) 
data_40to49 = merge(num_40to49, population_durham)
data_40to49$rate_40to49 = data_40to49$count_40to49/(data_40to49$M_40to44 + data_40to49$F_40to44 + data_40to49$M_45to49 + data_40to49$F_45to49)
final_40to49 = data_40to49 %>% group_by(GeoIDBlkGr) %>% summarize(rate_40to49 = sum(rate_40to49)) %>% rename(GEOID10 = GeoIDBlkGr)
final_40to49[153,2]=0
final_40to49[123,2]=0
final_40to49[45,2]=0

block_groups_data <- merge(block_groups, final_40to49, by="GEOID10")
plot5 <- spplot(block_groups_data, "rate_40to49", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 40 to 49", cex = 1))

# Plot 6: 50-59
age_distr_50to59 <- (sum(population_total$M_50to54) + sum(population_total$F_50to54) + sum(population_total$M_55to59) + sum(population_total$F_55to59))/sum(population_total$Total)
num_50to59 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_50to59 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 50 & AGE <= 59 ) * 1000/11 * age_distr_50to59) 
data_50to59 = merge(num_50to59, population_durham)
data_50to59$rate_50to59 = data_50to59$count_50to59/(data_50to59$M_50to54 + data_50to59$F_50to54 + data_50to59$M_55to59 + data_50to59$F_55to59)
final_50to59 = data_50to59 %>% group_by(GeoIDBlkGr) %>% summarize(rate_50to59 = sum(rate_50to59)) %>% rename(GEOID10 = GeoIDBlkGr)
final_50to59[153,2]=0 
final_50to59[123,2]=0
final_50to59[45,2]=0

block_groups_data <- merge(block_groups, final_50to59, by="GEOID10")
plot6 <- spplot(block_groups_data, "rate_50to59", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 50 to 59", cex = 1))

# Plot 7: 60-69
age_distr_60to69 <- (sum(population_total$M_60to61) + sum(population_total$F_60to61) + sum(population_total$M_62to64) + sum(population_total$F_62to64) + 
                       sum(population_total$M_65to66) + sum(population_total$F_65to66) + sum(population_total$M_67to69) + sum(population_total$F_67to69))/sum(population_total$Total)
num_60to69 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_60to69 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 60 & AGE <= 69 ) * 1000/11 * age_distr_60to69) 
data_60to69 = merge(num_60to69, population_durham)
data_60to69$rate_60to69 = data_60to69$count_60to69/(data_60to69$M_60to61 + data_60to69$F_60to61 + data_60to69$M_62to64 + data_60to69$F_62to64 + data_60to69$M_65to66 + data_60to69$F_65to66 + data_60to69$M_67to69 + data_60to69$F_67to69)
final_60to69 = data_60to69 %>% group_by(GeoIDBlkGr) %>% summarize(rate_60to69 = sum(rate_60to69)) %>% rename(GEOID10 = GeoIDBlkGr)
final_60to69[153,2]=0 
final_60to69[123,2]=0
final_60to69[45,2]=0

block_groups_data <- merge(block_groups, final_60to69, by="GEOID10")
plot7 <- spplot(block_groups_data, "rate_60to69", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 60 to 69", cex = 1))

# Plot 8: 70-79 
age_distr_70to79 <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79))/sum(population_total$Total)
num_70to79 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_70to79 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 70 & AGE <= 79 ) * 1000/11 * age_distr_70to79) 
data_70to79 = merge(num_70to79, population_durham)
data_70to79$rate_70to79 = data_70to79$count_70to79/(data_70to79$M_70to74 + data_70to79$F_70to74 + data_70to79$M_75to79 + data_70to79$F_75to79)
final_70to79 = data_70to79 %>% group_by(GeoIDBlkGr) %>% summarize(rate_70to79 = sum(rate_70to79)) %>% rename(GEOID10 = GeoIDBlkGr)
final_70to79[153,2]=0 
final_70to79[123,2]=0
final_70to79[45,2]=0

block_groups_data <- merge(block_groups, final_70to79, by="GEOID10")
plot8 <- spplot(block_groups_data, "rate_70to79", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 70 to 79", cex = 1))


# Plot 9: 80 and Over
age_distr_80andOver <- (sum(population_total$M_80to84) + sum(population_total$F_80to84) + sum(population_total$M_85andOver) + sum(population_total$F_85andOver))/sum(population_total$Total)
num_80andOver <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_80andOver = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 80) * 1000/11 * age_distr_80andOver) 
data_80andOver = merge(num_80andOver, population_durham)
data_80andOver$rate_80andOver = data_80andOver$count_80andOver/(data_80andOver$M_80to84 + data_80andOver$F_80to84 + data_80andOver$M_85andOver + data_80andOver$F_85andOver)
final_80andOver = data_80andOver %>% group_by(GeoIDBlkGr) %>% summarize(rate_80andOver = sum(rate_80andOver)) %>% rename(GEOID10 = GeoIDBlkGr)
final_80andOver[153,2]=0 
final_80andOver[123,2]=0
final_80andOver[45,2]=0

block_groups_data <- merge(block_groups, final_80andOver, by="GEOID10")
plot9 <- spplot(block_groups_data, "rate_80andOver", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                main=list(label = "Ages 80 and Over", cex = 1))
# Plot all 
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, nrow = 3, 
             top = textGrob("Age-Adjusted Mortality Rates due to Cancer Per 1,000 People", gp = gpar(fontsize = 20, fontface = "bold")))


# Age-Adjusted Mortality Rates due to Cancer for Ages 50 and Over

# Plot 1: 40-49
age_distr_40to49 <- (sum(population_total$M_40to44) + sum(population_total$F_40to44) + sum(population_total$M_45to49) + sum(population_total$F_45to49))/sum(population_total$Total)
num_40to49 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_40to49 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 40 & AGE <= 49 ) * 1000/11 * age_distr_40to49) 
data_40to49 = merge(num_40to49, population_durham)
data_40to49$rate_40to49 = data_40to49$count_40to49/(data_40to49$M_40to44 + data_40to49$F_40to44 + data_40to49$M_45to49 + data_40to49$F_45to49)
final_40to49 = data_40to49 %>% group_by(GeoIDBlkGr) %>% summarize(rate_40to49 = sum(rate_40to49)) %>% rename(GEOID10 = GeoIDBlkGr)
final_40to49[153,2]=0
final_40to49[123,2]=0
final_40to49[45,2]=0

block_groups_data <- merge(block_groups, final_40to49, by="GEOID10")
plot10 <- spplot(block_groups_data, "rate_40to49", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                at = seq(0, 1.6, by = .2), 
                main=list(label = "Ages 40 to 49", cex = 1))

# Plot 2: 50-59
age_distr_50to59 <- (sum(population_total$M_50to54) + sum(population_total$F_50to54) + sum(population_total$M_55to59) + sum(population_total$F_55to59))/sum(population_total$Total)
num_50to59 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_50to59 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 50 & AGE <= 59 ) * 1000/11 * age_distr_50to59) 
data_50to59 = merge(num_50to59, population_durham)
data_50to59$rate_50to59 = data_50to59$count_50to59/(data_50to59$M_50to54 + data_50to59$F_50to54 + data_50to59$M_55to59 + data_50to59$F_55to59)
final_50to59 = data_50to59 %>% group_by(GeoIDBlkGr) %>% summarize(rate_50to59 = sum(rate_50to59)) %>% rename(GEOID10 = GeoIDBlkGr)
final_50to59[153,2]=0 
final_50to59[123,2]=0
final_50to59[45,2]=0

block_groups_data <- merge(block_groups, final_50to59, by="GEOID10")
plot11 <- spplot(block_groups_data, "rate_50to59", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                at = seq(0, 1.6, by = .2), 
                main=list(label = "Ages 50 to 59", cex = 1))

# Plot 3: 60-69
age_distr_60to69 <- (sum(population_total$M_60to61) + sum(population_total$F_60to61) + sum(population_total$M_62to64) + sum(population_total$F_62to64) + 
                       sum(population_total$M_65to66) + sum(population_total$F_65to66) + sum(population_total$M_67to69) + sum(population_total$F_67to69))/sum(population_total$Total)
num_60to69 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_60to69 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 60 & AGE <= 69 ) * 1000/11 * age_distr_60to69) 
data_60to69 = merge(num_60to69, population_durham)
data_60to69$rate_60to69 = data_60to69$count_60to69/(data_60to69$M_60to61 + data_60to69$F_60to61 + data_60to69$M_62to64 + data_60to69$F_62to64 + data_60to69$M_65to66 + data_60to69$F_65to66 + data_60to69$M_67to69 + data_60to69$F_67to69)
final_60to69 = data_60to69 %>% group_by(GeoIDBlkGr) %>% summarize(rate_60to69 = sum(rate_60to69)) %>% rename(GEOID10 = GeoIDBlkGr)
final_60to69[153,2]=0 
final_60to69[123,2]=0
final_60to69[45,2]=0

block_groups_data <- merge(block_groups, final_60to69, by="GEOID10")
plot12 <- spplot(block_groups_data, "rate_60to69", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                at = seq(0, 1.6, by = .2), 
                main=list(label = "Ages 60 to 69", cex = 1))

# Plot 4: 70-79 
age_distr_70to79 <- (sum(population_total$M_70to74) + sum(population_total$F_70to74) + sum(population_total$M_75to79) + sum(population_total$F_75to79))/sum(population_total$Total)
num_70to79 <- deaths_durham %>% group_by(GeoIDBlkGr) %>% summarize(count_70to79 = sum((CauseOfDea == "Cancer" | CauseOfDea == "Breast Cancer" | CauseOfDea == "Colon, Rectum and Anus Cancer" | CauseOfDea == "Prostate Cancer" | CauseOfDea == "Trachea, Bronchus and Lung Cancer") & AGE >= 70 & AGE <= 79 ) * 1000/11 * age_distr_70to79) 
data_70to79 = merge(num_70to79, population_durham)
data_70to79$rate_70to79 = data_70to79$count_70to79/(data_70to79$M_70to74 + data_70to79$F_70to74 + data_70to79$M_75to79 + data_70to79$F_75to79)
final_70to79 = data_70to79 %>% group_by(GeoIDBlkGr) %>% summarize(rate_70to79 = sum(rate_70to79)) %>% rename(GEOID10 = GeoIDBlkGr)
final_70to79[153,2]=0 
final_70to79[123,2]=0
final_70to79[45,2]=0

block_groups_data <- merge(block_groups, final_70to79, by="GEOID10")
plot13 <- spplot(block_groups_data, "rate_70to79", 
                col.regions = brewer.pal(9, "Reds"), cuts= 8, 
                at = seq(0, 1.6, by = .2), 
                main=list(label = "Ages 70 to 79", cex = 1))

# Plot all 
grid.arrange(plot10, plot11, plot12, plot13, nrow = 2, 
             top = textGrob("Age-Adjusted Mortality Rates due to Cancer for Ages 40 to 79", gp = gpar(fontsize = 20, fontface = "bold")))

