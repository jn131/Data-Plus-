#.......................
# Shiny Datasets
# Jessica Needleman
#.......................


## df
df <- deaths_county %>% dplyr::rename(GEOID10 = GeoIDBlkGr)

# race_shiny
race_original <- c("African American","Chinese","Filipino","Hawaiian","Japanese","Native American","Other","Other Asian","Unknown","White")
race_new <- c("African American","Asian","Asian","Native Hawaiian","Asian","Native American","Other","Asian","Other","White")
df$race_shiny <- race_new[match(df$RACE, race_original)]

# age_shiny
age_original <- c(0:112)
age_new <- c(rep("0-4", 5), rep("5-9", 5), rep("10-14", 5), rep("15-19", 5), rep("20-24", 5), rep("25-29", 5), rep("30-34", 5), 
             rep("35-39", 5), rep("40-44", 5), rep("45-49", 5), rep("50-54", 5), rep("55-59", 5), rep("60-64", 5), rep("65-69", 5), 
             rep("70-74", 5), rep("75-79", 5), rep("80-84", 5), rep("85 and Over", 113-85))
df$age_shiny <- age_new[match(df$AGE, age_original)]

# sex_shiny
sex_original <- c("M","F")
sex_new <- c("Male","Female")
df$sex_shiny <- sex_new[match(df$SEX, sex_original)]

# causeofdeath_shiny 
cause_original <- c("Cancer","Breast Cancer","Colon, Rectum and Anus Cancer","Prostate Cancer","Trachea, Bronchus and Lung Cancer","HIV Disease","Diabetes Mellitus","Heart Disease")
cause_new <- c(rep("Cancer", 5),"HIV","Diabetes","Heart Disease")
df$causeofdeath_shiny <- cause_new[match(df$CauseOfDea, cause_original)]

# ment_shiny
cancer.f <- function(x) {x[which(substr(x, 1, 1) == "C")]}
cancer_icd10 <- unique(c(cancer.f(df$MENT1), cancer.f(df$MENT2), cancer.f(df$MENT3), cancer.f(df$MENT4), cancer.f(df$MENT5))) 
cancer_new <- rep("Cancer", length(cancer_icd10))

diabetes.f <- function(x) {x[which(substr(x, 1, 3) %in% c("E08","E09","E10","E11","E12","E13","E14"))]}
diabetes_icd10 <- unique(c(diabetes.f(df$MENT1), diabetes.f(df$MENT2), diabetes.f(df$MENT3), diabetes.f(df$MENT4), diabetes.f(df$MENT5)))
diabetes_new <- rep("Diabetes", length(diabetes_icd10))

heart.f <- function(x) {x[which(substr(x, 1, 3) %in% c(paste("I","0",c(5:9), sep = ""), paste("I",c(20:52), sep = "")))]}
heartdisease_icd10 <- unique(c(heart.f(df$MENT1), heart.f(df$MENT2), heart.f(df$MENT3), heart.f(df$MENT4), heart.f(df$MENT5))) 
heartdisease_new <- rep("Heart Disease", length(heartdisease_icd10))

hiv.f <- function(x) {x[which(substr(x, 1, 3) %in% c(paste("B",c(20:24), sep = "")))]}
hiv_icd10 <- unique(c(hiv.f(df$MENT1), hiv.f(df$MENT2), hiv.f(df$MENT3), hiv.f(df$MENT4), hiv.f(df$MENT5))) 
hiv_new <- rep("HIV", length(hiv_icd10))

ment_original <- c(cancer_icd10, diabetes_icd10, heartdisease_icd10, hiv_icd10)
ment_new <- c(cancer_new, diabetes_new, heartdisease_new, hiv_new)
df$ment1_shiny <- ment_new[match(df$MENT1, ment_original)]
df$ment2_shiny <- ment_new[match(df$MENT2, ment_original)]
df$ment3_shiny <- ment_new[match(df$MENT3, ment_original)]
df$ment4_shiny <- ment_new[match(df$MENT4, ment_original)]
df$ment5_shiny <- ment_new[match(df$MENT5, ment_original)]


## Rate Datasets

race_colnames <- c("GEOID10","Total","White","African American","Native American","Asian","Native Hawaiian","Other")
pop.f <- function(x, y) {apply(population_county[,c(x,y)], 1, sum)}

# pop_shiny_total 
pop_shiny_total <- cbind(population_county[,c(1,18)], pop.f(3,11), pop.f(4,12), pop.f(5,13), pop.f(6,14), pop.f(7,15), pop.f(5:9, 13:17), rep(0, length(population_county$GeoIDBlkGr)), rep(0, length(population_county$Total)))
setnames(pop_shiny_total, old = colnames(pop_shiny_total), new = c(race_colnames, "Zero1","Zero2"))

# pop_shiny_nonhispanic
pop_shiny_nonhispanic <- cbind(population_county[,c(1:7)], apply(population_county[,5:9], 1, sum), rep(0, length(population_county$GeoIDBlkGr)), rep(0, length(population_county$Total)))
setnames(pop_shiny_nonhispanic, old = colnames(pop_shiny_nonhispanic), new = c(race_colnames,"Zero1","Zero2"))

# population_county_hispanic
pop_shiny_hispanic <- cbind(population_county[,c(1,10:15)],apply(population_county[,16:17], 1, sum), rep(0, length(population_county$GeoIDBlkGr)), rep(0, length(population_county$Total)))
setnames(pop_shiny_hispanic, old = colnames(pop_shiny_hispanic), new = c(race_colnames,"Zero1","Zero2"))



