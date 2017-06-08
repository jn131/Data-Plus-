
#### POPULATION DATA 

## Open population by age file 
population <- read.csv("Population_by_Age.csv", stringsAsFactors = F)

## Reformat file

# Create GeoIDBlkGr column
x <- population$GISJOIN
population$GISJOIN <- paste(substr(x, 2, 3), substr(x, 5, 7), substr(x, 9, nchar(x)), sep='')

population <- population[, c(1, 41:106)]

# Rename remaining columns
setnames(population, 
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
population_durham <- population[which(population$GeoIDBlkGr >= fips_durham *10000000 & population$GeoIDBlkGr < (fips_durham + 1) *10000000),]

