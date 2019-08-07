required.packages <- c("reshape2","ggplot2","data.table","WDI","XML","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

###FUNCTIONS

#Remap survey types
remap_cov <- function(x){
  cov_dict <- c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov <- Vectorize(remap_cov)

#Pull from Povcal API
povcal_svy <- function(pl=1.9,group.by="WB"){
  url <- "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params <- list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "SurveyYears"="all",
    "Display"="C",
    "GroupedBy"=group.by,
    "format"="csv"
  )
  param_names <- names(params)
  for(param_name in param_names){
    param <- params[[param_name]]
    url <- paste0(url,param_name,"=",param,"&")
  }
  url <- substr(url,1,nchar(url)-1)
  return(fread(url))
}

#Pull Povcal distributions
povcal_dist <- function(C0="AGO_3",Y0=2015){
  dist_url <- paste0("http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0=",C0,"&Y0=",Y0)
  dist_html <- htmlParse(dist_url, isURL=T)
  dist_root <- xmlRoot(dist_html)
  dist_txt <- xmlValue(getNodeSet(dist_root, "//pre")[[1]])
  start_point <- gregexpr(pattern="---\r\n", dist_txt)[[1]][4]
  end_point <- gregexpr(pattern="\r\n---", dist_txt)[[1]][5]
  if(start_point<end_point){
    txt_table <- substr(dist_txt,start_point+5,end_point-1)
    df <- read.table(text=txt_table, header=F, col.names=c("i","P", "L"))
  }else{
    return(NULL)
  }
  
  if((mean_start_point <- gregexpr(pattern="mean in PPP", dist_txt)[[1]][1])==-1){
    mean_start_point <- gregexpr(pattern="Mean:", dist_txt)[[1]][1]+6
    mean_end_point <- gregexpr(pattern=" overall sum", dist_txt)[[1]][1]-5
  }else{
    mean_start_point <- gregexpr(pattern="mean in PPP", dist_txt)[[1]][1]+14
    mean_end_point <- gregexpr(pattern="Poverty line in", dist_txt)[[1]][1]-11
  }
  if(mean_start_point<mean_end_point){
    mean <- substr(dist_txt,mean_start_point,mean_end_point)
  }else{
    mean <- NULL
  }
  dist <- list()
  dist <- df
  return(dist)
}

#Solve distributions for headcounts
GQsolve <- function(a,b,c,PL,mu){
  e <- -(a+b+c+1)
  m <- b^2-4*a
  n <- 2*b*e-4*c
  z <- PL/mu+b/2
  y <- 16*z^2*m-4*m^2
  x <- 16*z^2*n-4*n*m
  w <- 16*z^2*e^2-n^2
  p1 <- (-x-(sqrt(x^2-4*y*w)))/(2*y)
  p2 <- (-x+(sqrt(x^2-4*y*w)))/(2*y)
  if(p1>1 | p1<0){
    p1 <- NaN
  }
  if(p2>1 | p2<0){
    p2 <- NaN
  }
  return(data.table(P1=p1,P2=p2))
}

Betasolve <- function(a,b,c,PL,mu,init){
  p0 <- init
  N <- 10000
  tol <- 1E-15
  i <- 1
  p1 <- p0
  s <- numeric(N)
  while (i<=N) {
    f <- a*(p0^b)*((1-p0)^c)*((b/p0)-c/(1-p0))-(1-PL/mu)
    df.dx <- a*(p0^(b-2))*((1-p0)^(c-2))*((b^2)*((p0-1)^2)+b*(p0-1)*((2*c-1)*p0+1)+(c-1)*c*(p0^2))
    p1 <- (p0 - 0.01*(f/df.dx))
    s[i] <- p1
    i <- i + 1
    if (abs(p1-p0) < tol) {break}
    p0 <- p1
  }
  return(s[i-1])
}

###ANALYSIS

#Parameterise Povcal dists
if(!("all_dist.csv" %in% list.files("project_data") & "all_parameters.csv" %in% list.files("project_data"))){
  ext <- povcal_svy()
  fwrite(ext, "project_data/povcalout.csv")
  ext$svy_code <- remap_cov(ext$CoverageType)
  ext <- subset(ext,!is.na(svy_code))
  ext$C0 <- paste(ext$CountryCode,ext$svy_code,sep="_")
  data.list <- list()
  data.index <- 1
  errs <- c()
  pb <- txtProgressBar(min=1, max=nrow(ext), style=3)
  for(i in 1:nrow(ext)){
    svy <- as.character(ext[i,"C0"])
    year <- as.numeric(ext[i,"DataYear"])
    PPP <- as.numeric(ext[i,"PPP"])
    mean <- as.numeric(ext[i,"Mean"])
    msg_lbl <- paste(svy,year)
    setTxtProgressBar(pb, i, label=msg_lbl)
    dist.tmp <- tryCatch({povcal_dist(svy, year)},error=function(e){return(NULL)})
    if(!is.null(dist.tmp)){
      dist.tmp$svy <- svy
      dist.tmp$year <- year
      dist.tmp$ppp <- PPP
      dist.tmp$mean <- mean
      data.list[[i]] <- dist.tmp
    }
  }
  all_dist <- rbindlist(data.list)
  fwrite(all_dist, "project_data/all_dist.csv")
  parameters.list <- list()
  for (i in 1:length(data.list)){
    data.list[[i]]$`L(1-L)` <- data.list[[i]]$L*(1-data.list[[i]]$L)
    data.list[[i]]$`(P^2-L)` <- (data.list[[i]]$P^2-data.list[[i]]$L)
    data.list[[i]]$`L(P-1)` <- data.list[[i]]$L*(data.list[[i]]$P-1)
    data.list[[i]]$`(P-L)` <- (data.list[[i]]$P-data.list[[i]]$L)
    data.list[[i]]$`ln(P-L)` <- log(data.list[[i]]$P-data.list[[i]]$L)
    data.list[[i]]$`ln(P)` <- log(data.list[[i]]$P)
    data.list[[i]]$`ln(1-P)` <- log(1-data.list[[i]]$P)
    data.list[[i]]$`ln(P-L)`[is.infinite(data.list[[i]]$`ln(P-L)`)] <- NA
    data.list[[i]]$`ln(P)`[is.infinite(data.list[[i]]$`ln(P)`)] <- NA
    data.list[[i]]$`ln(1-P)`[is.infinite(data.list[[i]]$`ln(1-P)`)] <- NA
    GQ <- lm(`L(1-L)`~ 0 + `(P^2-L)` + `L(P-1)` + `(P-L)`,data.list[[i]],na.action=na.omit)$coefficients
    Beta <- lm(`ln(P-L)`~ `ln(P)` + `ln(1-P)`,data.list[[i]],na.action=na.omit)$coefficients
    Beta[1] <- exp(Beta[1])
    parameters.temp <- as.data.frame(rbind(GQ,Beta))
    parameters.temp$svy <- head(data.list[[i]]$svy,2)
    parameters.temp$year <- head(data.list[[i]]$year,2)
    parameters.temp$mean <- head(data.list[[i]]$mean,2)
    parameters.temp$PPP <- head(data.list[[i]]$ppp,2)
    parameters.temp$type <- c("GQ","Beta")
    colnames(parameters.temp) <- c("A","B","C","svy","year","mean","ppp","type")
    parameters.list[[i]] <- parameters.temp
  }
  all_parameters <- rbindlist(parameters.list)
  fwrite(all_parameters, "project_data/all_parameters.csv")
} else {
  all_dist <- fread("project_data/all_dist.csv")
  all_parameters <- fread("project_data/all_parameters.csv")
  ext <- fread("project_data/povcalout.csv")
}

#Manual addition of Afghanistan and Somalia
afg.som.lby.param <- data.table(A=c(0.935463769,0.5760727,0.88737242,0.666110634,1.002559972,0.613658334)
                            ,B=c(-1.476794803,0.856948964,-1.254176897,0.95205282,-1.341360921,0.934277217)
                            ,C=c(0.083512829,0.49178234,0.196100839,0.562072863,0.241518659,0.607012527)
                            ,svy=c("AFG_3","AFG_3","SOM_3","SOM_3","LBY_3","LBY_3"),
                            year=c(2016.5,2016.5,2017,2017,2008,2008),
                            mean=c(2.788346962/12*365.2424,2.788346962/12*365.2424,1.69211737786/12*365.2424,1.69211737786/12*365.2424,4.9507954/12*365.2424,4.9507954/12*365.2424),
                            ppp=c(22.8388105,22.8388105,0.674879848994,0.674879848994,0.6577641,0.6577641),
                            type=rep(c("GQ","Beta"),3)
)

all_parameters <- rbind(all_parameters, afg.som.lby.param)
all_parameters <- merge(all_parameters, unique(ext[,c(4,34)]), by.x="svy", by.y="C0",all.x=T)
all_parameters[which(svy=="AFG_3")]$CountryName <- "Afghanistan"
all_parameters[which(svy=="SOM_3")]$CountryName <- "Somalia"
all_parameters[which(svy=="LBY_3")]$CountryName <- "Libya"

#Only most recent surveys
recent_years <- all_parameters[all_parameters[, .I[which.max(year)], by=svy]$V1][,c(1,5,9)]
recent_years$year <- round(recent_years$year,0)

#Split GQ and Beta
parameters_GQ <- all_parameters[all_parameters[, .I[which.max(year)], by=svy]$V1]
parameters_Beta <- all_parameters[all_parameters[, .I[which.max(year)], by=svy]$V1+1]

#GDP per capita growth
WEOraw <- fread("http://www.imf.org/external/pubs/ft/weo/2019/01/weodata/WEOApr2019all.xls", na.strings="n/a")
WEO <- WEOraw[`WEO Subject Code`=="NGDP_RPCH"]
WEO[WEO=="--"] <- 0

{WEO$Country[which(WEO$Country=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
  WEO$Country[which(WEO$Country=="Republic of Congo")]="Congo, Republic of"
  WEO$Country[which(WEO$Country=="Côte d'Ivoire")]="Cote d'Ivoire"
  WEO$Country[which(WEO$Country=="Egypt")]="Egypt, Arab Republic of"
  WEO$Country[which(WEO$Country=="The Gambia")]="Gambia, The"
  WEO$Country[which(WEO$Country=="Islamic Republic of Iran")]="Iran, Islamic Republic of"
  WEO$Country[which(WEO$Country=="Russia")]="Russian Federation"
  WEO$Country[which(WEO$Country=="Syria")]="Syrian Arab Republic"
  WEO$Country[which(WEO$Country=="Venezuela")]="Venezuela, Republica Bolivariana de"
  WEO$Country[which(WEO$Country=="Yemen")]="Yemen, Republic of"
  WEO$Country[which(WEO$Country=="Lao P.D.R.")]="Lao People's Democratic Republic"
  WEO$Country[which(WEO$Country=="FYR Macedonia")]="Macedonia, former Yugoslav Republic of"
  WEO$Country[which(WEO$Country=="Micronesia")]="Micronesia, Federated States of"
  WEO$Country[which(WEO$Country=="São Tomé and Príncipe")]="Sao Tome and Principe"
  WEO$Country[which(WEO$Country=="Korea")]="Korea, Republic of"}

names(WEO)[which(names(WEO)=="Country")]<- "CountryName"
WEO <- merge(WEO, recent_years, by="CountryName", all=T)
WEO[is.na(WEO$year)]$year <- 2018

#Calculate average growth for years between survey and 2018, and 2018-2024
WEO$growthto18 <- as.numeric(NA)
WEO$growthforecast <- as.numeric(NA)
for(i in 1:nrow(WEO)){
  if(WEO[i]$year==2018){
    WEO[i]$growthto18 <- 0
  } else {
    cols <- as.character(seq(WEO[i]$year,2018))
    WEO[i]$growthto18 <- mean(as.numeric(WEO[i,..cols]), na.rm=T)
  }
  cols <- as.character(seq(2018,2024))
  WEO[i]$growthforecast <- mean(as.numeric(WEO[i,..cols]), na.rm=T)
}

#Forecast effective poverty lines by adjusted growth rates
forecasts <- WEO[,c("CountryName","year","svy","growthto18","growthforecast")]
forecasts$adjgrowthto18 <- forecasts$growthto18*0.87
forecasts$adjgrowthforecast <- forecasts$growthforecast*0.87
forecasts[which(CountryName=="China")]$adjgrowthto18 <- forecasts[which(CountryName=="China")]$growthto18*0.72
forecasts[which(CountryName=="India")]$adjgrowthto18 <- forecasts[which(CountryName=="India")]$growthto18*0.51
forecasts$PL2018 <- 1.9/(1+forecasts$adjgrowthto18/100)^(2018-forecasts$year)
forecasts$PL2030 <- forecasts$PL2018/(1+forecasts$adjgrowthforecast/100)^12
forecasts$PL2030plus1 <- forecasts$PL2018/(1+(forecasts$adjgrowthforecast+1)/100)^12
forecasts$PL2030minus1 <- forecasts$PL2018/(1+(forecasts$adjgrowthforecast-1)/100)^12

parameters_GQ <- merge(parameters_GQ, forecasts, by=c("svy","CountryName"))
parameters_Beta <- merge(parameters_Beta, forecasts, by=c("svy","CountryName"))

#Run distribution solutions for effective projected poverty lines
data.list <- list()
for(i in 1:nrow(parameters_GQ)){
  data.list[[i]] <- data.table(tryCatch({GQsolve(parameters_GQ[i]$A,parameters_GQ[i]$B,parameters_GQ[i]$C,parameters_GQ[i]$PL2018,parameters_GQ[i]$mean*12/365.2424)},error=function(e){return(data.table(NaN,NaN))})
                               ,tryCatch({GQsolve(parameters_GQ[i]$A,parameters_GQ[i]$B,parameters_GQ[i]$C,parameters_GQ[i]$PL2030,parameters_GQ[i]$mean*12/365.2424)},error=function(e){return(data.table(NaN,NaN))})
                               ,tryCatch({GQsolve(parameters_GQ[i]$A,parameters_GQ[i]$B,parameters_GQ[i]$C,parameters_GQ[i]$PL2030plus1,parameters_GQ[i]$mean*12/365.2424)},error=function(e){return(data.table(NaN,NaN))})
                               ,tryCatch({GQsolve(parameters_GQ[i]$A,parameters_GQ[i]$B,parameters_GQ[i]$C,parameters_GQ[i]$PL2030minus1,parameters_GQ[i]$mean*12/365.2424)},error=function(e){return(data.table(NaN,NaN))}))
}

GQhc <- rbindlist(data.list, fill=T)
GQhc <- GQhc[,c(1:8)]
names(GQhc) <- c("2018P1","2018P2","2030P1","2030P2","2030+1P1","2030+1P2","2030-1P1","2030-1P2")
parameters_GQ <- cbind(parameters_GQ, GQhc)

data.list <- list()
for(i in 1:nrow(parameters_Beta)){
  data.list[[i]] <- data.table(tryCatch({Betasolve((parameters_Beta[i]$A),parameters_Beta[i]$B,parameters_Beta[i]$C,parameters_Beta[i]$PL2018,parameters_Beta[i]$mean*12/365.2424,0.1)},error=function(e){return(NaN)})
                               ,tryCatch({Betasolve((parameters_Beta[i]$A),parameters_Beta[i]$B,parameters_Beta[i]$C,parameters_Beta[i]$PL2030,parameters_Beta[i]$mean*12/365.2424,0.1)},error=function(e){return(NaN)})
                               ,tryCatch({Betasolve((parameters_Beta[i]$A),parameters_Beta[i]$B,parameters_Beta[i]$C,parameters_Beta[i]$PL2030plus1,parameters_Beta[i]$mean*12/365.2424,0.1)},error=function(e){return(NaN)})
                               ,tryCatch({Betasolve((parameters_Beta[i]$A),parameters_Beta[i]$B,parameters_Beta[i]$C,parameters_Beta[i]$PL2030minus1,parameters_Beta[i]$mean*12/365.2424,0.1)},error=function(e){return(NaN)}))
}

Betahc <- rbindlist(data.list)
names(Betahc) <- c("Beta2018","Beta2030","Beta2030high","Beta2030low")
parameters_Beta <- cbind(parameters_Beta,Betahc)

#Output raw solutions; check GQ results against Beta and select nearest root
hc <- merge(parameters_GQ[,c(1:2,19:26)],parameters_Beta[,c(1:2,19:22)], by=c("svy","CountryName"))
hc[is.na(hc)] <- 0
hc$GQ2018 <- hc$`2018P1`
hc$GQ2030 <- hc$`2030P1`
hc$GQ2030high <- hc$`2030+1P1`
hc$GQ2030low <- hc$`2030-1P1`

for(i in 1:nrow(hc)){
  if(abs(hc[i]$`2018P1`-hc[i]$Beta2018) > abs(hc[i]$`2018P2`-hc[i]$Beta2018)){
    hc[i]$GQ2018 <- hc[i]$`2018P2`
  }
  if(abs(hc[i]$`2030P1`-hc[i]$Beta2030) > abs(hc[i]$`2030P2`-hc[i]$Beta2030)){
    hc[i]$GQ2030 <- hc[i]$`2030P2`
  }
  if(abs(hc[i]$`2030+1P1`-hc[i]$Beta2030high) > abs(hc[i]$`2030+1P2`-hc[i]$Beta2030high)){
    hc[i]$GQ2030high <- hc[i]$`2030+1P2`
  }
  if(abs(hc[i]$`2030-1P1`-hc[i]$Beta2030low) > abs(hc[i]$`2030-1P2`-hc[i]$Beta2030low)){
    hc[i]$GQ2030low <- hc[i]$`2030-1P2`
  }
}

headcountproj <- hc[,c(1,2,11:18)]

if(!("WUP_urban.xls" %in% list.files("project_data") & "WUP_rural.xls" %in% list.files("project_data"))){
  download.file("https://population.un.org/wup/Download/Files/WUP2018-F19-Urban_Population_Annual.xls", "project_data/WUP_urban.xls", mode="wb")
  download.file("https://population.un.org/wup/Download/Files/WUP2018-F20-Rural_Population_Annual.xls", "project_data/WUP_rural.xls", mode="wb")
}

wup.urb <- read_xls("project_data/WUP_urban.xls", skip=16)
wup.rur <- read_xls("project_data/WUP_rural.xls", skip=16)
wup.urb <- data.table(type="2",wup.urb[,c(2,73,85)])
wup.rur <- data.table(type="1",wup.rur[,c(2,73,85)])
wup.tot <- data.table(type="3",wup.urb[,c(2)],wup.urb[,c(3,4)]+wup.rur[,c(3,4)])
wup.all <- rbind(wup.tot,wup.urb,wup.rur)
names(wup.all)[2] <- "CountryName"

{
wup.all$CountryName[which(wup.all$CountryName=="Bolivia (Plurinational State of)")]="Bolivia"
wup.all$CountryName[which(wup.all$CountryName=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Congo")]="Congo, Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Côte d'Ivoire")]="Cote d'Ivoire"
wup.all$CountryName[which(wup.all$CountryName=="Czechia")]="Czech Republic"
wup.all$CountryName[which(wup.all$CountryName=="Egypt")]="Egypt, Arab Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Gambia")]="Gambia, The"
wup.all$CountryName[which(wup.all$CountryName=="Iran (Islamic Republic of)")]="Iran, Islamic Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Republic of Korea")]="Korea, Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Kyrgyzstan")]="Kyrgyz Republic"
wup.all$CountryName[which(wup.all$CountryName=="TFYR Macedonia")]="Macedonia, former Yugoslav Republic of"
wup.all$CountryName[which(wup.all$CountryName=="Micronesia (Fed. States of)")]="Micronesia, Federated States of"
wup.all$CountryName[which(wup.all$CountryName=="Republic of Moldova")]="Moldova"
wup.all$CountryName[which(wup.all$CountryName=="Slovakia")]="Slovak Republic"
wup.all$CountryName[which(wup.all$CountryName=="Saint Lucia")]="St. Lucia"
wup.all$CountryName[which(wup.all$CountryName=="Swaziland")]="Eswatini"
wup.all$CountryName[which(wup.all$CountryName=="United Republic of Tanzania")]="Tanzania"
wup.all$CountryName[which(wup.all$CountryName=="United States of America")]="United States"
wup.all$CountryName[which(wup.all$CountryName=="Venezuela (Bolivarian Republic of)")]="Venezuela, Republica Bolivariana de"
wup.all$CountryName[which(wup.all$CountryName=="Viet Nam")]="Vietnam"
wup.all$CountryName[which(wup.all$CountryName=="Yemen")]="Yemen, Republic of"
wup.all$CountryName[which(wup.all$CountryName=="State of Palestine")]="West Bank and Gaza"
}

afg.som.lby <- data.table(CountryCode=c("AFG","SOM","LBY"), CountryName=c("Afghanistan","Somalia","Libya"))

wup.all <- merge(wup.all, rbind(unique(ext[,c(3,4)]),afg.som.lby), by="CountryName", all.y=T)
wup.all$svy <- paste0(wup.all$CountryCode,"_",wup.all$type)

headcountproj <- merge(headcountproj, wup.all, by=c("svy","CountryName"), all.x=T)

headcountproj$poorBeta2018 <- headcountproj$`2018`*headcountproj$Beta2018
headcountproj$poorGQ2018 <- headcountproj$`2018`*headcountproj$GQ2018
headcountproj$poorBeta2030 <- headcountproj$`2030`*headcountproj$Beta2030
headcountproj$poorGQ2030 <- headcountproj$`2030`*headcountproj$GQ2030
headcountproj$poorBeta2030high <- headcountproj$`2030`*headcountproj$Beta2030high
headcountproj$poorGQ2030high <- headcountproj$`2030`*headcountproj$GQ2030high
headcountproj$poorBeta2030low <- headcountproj$`2030`*headcountproj$Beta2030low
headcountproj$poorGQ2030low <- headcountproj$`2030`*headcountproj$GQ2030low

crisis.countries <- c("Somalia",
"Central African Republic",
"West Bank and Gaza",
"Chad",
"Congo, Democratic Republic of",
"Yemen, Republic of",
"South Sudan",
"Afghanistan",
"Mali",
"Myanmar",
"Mauritania",
"Syrian Arab Republic",
"Iraq",
"Cameroon",
"Nigeria",
"Senegal",
"Libya",
"Niger",
"Ukraine",
"Sudan",
"Haiti",
"Burundi")

headcountproj$crisis <- 0
headcountproj[which(CountryName %in% crisis.countries)]$crisis <- 1

headcountproj.melt <- melt(headcountproj, id.vars=c("svy","CountryName","CountryCode","type","crisis"))
headcountproj.melt <- headcountproj.melt[variable %in% c("poorBeta2018","poorGQ2018","poorBeta2030","poorGQ2030","poorBeta2030high","poorGQ2030high","poorBeta2030low","poorGQ2030low")]

crisis.poverty <- headcountproj.melt[!(svy %in% c("BOL_2","ETH_1","COL_2","FSM_2","HND_2","URY_2")), .(total.poor=sum(value*1000, na.rm=T)), by=.(variable,crisis)]
fwrite(crisis.poverty, "output/crisis_forecast.csv")
