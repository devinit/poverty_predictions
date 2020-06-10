required.packages <- c("WDI","data.table", "readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions/")

projpov <- fread("output/globalproj_long_Apr20.csv")

proc.crisis <- c("Afghanistan",
                 "Bangladesh",
                 "Burundi",
                 "Congo, Democratic Republic of",
                 "Central African Republic",
                 "Cameroon",
                 "Egypt, Arab Republic of",
                 "Ethiopia",
                 "Haiti",
                 "Iraq",
                 "Jordan",
                 "Kenya",
                 "Korea, Democratic People's Republic of",
                 "Lebanon",
                 "Libya",
                 "Mali",
                 "Myanmar",
                 "Niger",
                 "Nigeria",
                 "Pakistan",
                 "West Bank and Gaza",
                 "Rwanda",
                 "Sudan",
                 "Somalia",
                 "South Sudan",
                 "Syrian Arab Republic",
                 "Chad",
                 "Turkey",
                 "Tanzania",
                 "Ukraine",
                 "Uganda",
                 "Yemen, Republic of"
)

rec.crisis <- c("Brazil",
                "Colombia",
                "Ecuador",
                "Peru",
                "Venezuela, Republica Bolivariana de",
                "Zambia"
)

overall.crisis <- c("Afghanistan",
                      "Burundi",
                      "Colombia",
                      "Congo, Democratic Republic of",
                      "India",
                      "Iraq",
                      "Nigeria",
                      "West Bank and Gaza",
                      "Philippines",
                      "Somalia",
                      "Sudan",
                      "Uganda",
                      "Myanmar",
                      "Central African Republic",
                      "Chad",
                      "Congo, Republic of",
                      "Cyprus",
                      "Djibouti",
                      "Kenya",
                      "Turkey",
                      "Yemen, Republic of",
                      "Zimbabwe",
                      "Burkina Faso",
                      "Ethiopia",
                      "Jordan",
                      "Lebanon",
                      "Mali",
                      "Mauritania",
                      "Niger",
                      "Pakistan",
                      "Syrian Arab Republic",
                      "Bangladesh",
                      "Georgia",
                      "Guatemala",
                      "Cote d'Ivoire",
                      "Haiti",
                      "Libya",
                      "South Sudan",
                      "Nauru", #No poverty data
                      "Cameroon",
                      "Senegal"
)

projpov$crisis <- "Non-crisis"
projpov[DisplayName %in% proc.crisis]$crisis <- "Protracted crisis"
projpov[DisplayName %in% rec.crisis]$crisis <- "Recurrent crisis"

crisis.pov <- projpov[ProjYear > 2011 & Level == "National", .(HeadCount = weighted.mean(value[variable == "HeadCount"], value[variable == "ReqYearPopulation"], na.rm=T), 
                           NumPoor = sum(value[variable == "NumPoor"]),
                           ReqYearPopulation = sum(value[variable == "ReqYearPopulation"]),
                           n = length(value[variable == "HeadCount"])), by=.(crisis, ProjYear, PovertyLine)]

crisis.pov[, `:=` (SharePoor = NumPoor/sum(NumPoor), SharePop = ReqYearPopulation/sum(ReqYearPopulation)), by=.(ProjYear, PovertyLine)]
