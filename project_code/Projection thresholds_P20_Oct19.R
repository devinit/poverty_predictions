required.packages <- c("WDI","data.table", "readxl")
lapply(required.packages, require, character.only=T)

setwd("~/git/poverty_predictions/")

povcal.ind.out <- function(RefYears=T, countries, years="all", PLs=1.9, PPPs=NULL, display="c"){
  if(length(PLs) == 1){
    p <- paste0("PovertyLine=", PLs)
  } else {
    p <- paste0("PL", seq(0, length(PLs)-1), "=", PLs, collapse = "&")
  }
  d <- paste0("display=", display)
  c <- paste0("C", seq(0, length(countries)-1), "=", countries, collapse = "&")
  if(RefYears){
    y <- paste0("RefYears=", paste0(years, collapse=","))
  } else {
    if(length(years) == 1){
      years <- rep(years, length(countries))
    }
    y <- paste0("Y", seq(0, length(years)-1), "=", years, collapse = "&")
  }
  if(!(is.null(PPPs))){
    pp <- paste0("PPP", seq(0, length(PPPs)-1), "=", PPPs, collapse = "&")
  } else {
    pp <- NULL
  }
  param <- paste0(c(y, c, p, pp, d), collapse="&")
  url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param)
  return(read.csv(url,header=T))
}

povcal.tot.out <- function(country="all",year="all",PL=1.9,display="c"){
  param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&Countries=",country,"&display=",display)
  url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param)
  return(read.csv(url,header=T))
}

projection.threshold <- function(cc="UGA", year=2020, pl=1.9){
  
  pov.lines <- pl
  
  pov <- as.data.table(povcal.tot.out(country=cc))
  pov = subset(pov,!is.na(HeadCount))
  if(year %in% pov$RequestYear){
    pov = povcal.ind.out(RefYears = T, countries = cc, years = year, PLs = pl)
    return(pov$HeadCount)
  }
  pov <- pov[pov[!is.na(HeadCount), .I[which.max(RequestYear)], by=.(CountryCode, CoverageType)]$V1]
  
  countries <- pov[, c("CountryCode", "CoverageType", "RequestYear", "PPP")]
  countries <- countries[CoverageType %in% c("N", "A")]
  
  #GDP per capita growth
  if(!file.exists("project_data/WEOOct2019all.xls")){
    download.file("http://www.imf.org/external/pubs/ft/weo/2019/02/weodata/WEOOct2019all.xls", "project_data/WEOOct2019all.xls")
  }
  WEOraw <- fread("project_data/WEOOct2019all.xls", na.strings="n/a")
  WEO <- WEOraw[`WEO Subject Code` %in% c("NGDPRPPPPCPCH", "NGDPRPPPPC")]
  
  year.cols <- as.character(seq(min(countries$RequestYear), max(as.numeric(names(WEO)), na.rm=T)))
  WEO[, (year.cols) := lapply(.SD, function(x) gsub(",", "", x)), .SDcols=(year.cols)]
  
  WEO <- WEO[, lapply(.SD, as.numeric), .SDcols=(year.cols), by=.(ISO, `WEO Subject Code`)]
  WEO[`WEO Subject Code` == "NGDPRPPPPC", (year.cols) := cbind(0,as.data.table(t(apply(.SD, 1, function(x) 100*(diff(x)/shift(x,0)))))), .SDcols=(year.cols), by=ISO]
  
  WEO[WEO=="--"] <- 0
  WEO$`WEO Subject Code` <- NULL
  
  WEO[ISO == "UVK"]$ISO <- "XKX"
  
  WEO <- merge(WEO, countries, by.x="ISO", by.y="CountryCode")
  
  #Calculate new effective PPPs for each year of growth data
  WEO[, (year.cols) := lapply(1:ncol(.SD), function(i) ifelse(names(.SD)[i] <= RequestYear, 0, .SD[[i]])), .SDcols=(year.cols)]
  WEO[ISO=="CHN", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.72)/100))))), .SDcols=(year.cols), by=ISO]
  WEO[ISO=="IND", (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.51)/100))))), .SDcols=(year.cols), by=ISO]
  WEO[!(ISO %in% c("CHN", "IND")), (year.cols) := as.data.table(t(apply(.SD, 1, function(x) cumprod(1+((x*0.87)/100))))), .SDcols=(year.cols), by=ISO]
  WEO[, (year.cols) := PPP/.SD, .SDcols=(year.cols)]
  
  proj.years <- seq(max(WEO$RequestYear), max(as.numeric(names(WEO)), na.rm=T))
  

  projpov.list <- list()
  for(i in 1:length(proj.years)){
    proj.year <- as.character(proj.years[i])
    ppp = WEO[,proj.year,with=F]
    proj <- povcal.ind.out(RefYears = T, countries = cc, years = proj.years, PLs = pl, PPPs = ppp)
    proj$ProjYear <- proj.year
    projpov.list[[i]] <- proj
  }
  
  projpov <- rbindlist(projpov.list)
  projpov <- projpov[projpov[!is.na(HeadCount), .I[which.max(RequestYear)], by=.(CountryCode, CoverageType, PovertyLine, ProjYear)]$V1]
  
  keep <- c("CountryCode","CountryName","CoverageType","PovertyLine","HeadCount", "ProjYear")
  projpov <- projpov[,..keep]

  projpov.melt <- melt(projpov, id.vars=c("CountryCode","CountryName","ProjYear","PovertyLine"), measure.vars=c("HeadCount"))
  
  sub = subset(projpov.melt,ProjYear==year)
  return(sub$value)
}

data.list = list()
data.index = 1
all_dat = as.data.table(povcal.tot.out())
all_dat = subset(all_dat,!is.na(HeadCount) & RequestYear>=2010)
all_dat = unique(all_dat[,c("CountryCode","RequestYear")])
proj_dat = expand.grid(CountryCode=unique(all_dat$CountryCode),RequestYear=c(2018:2021))
all_dat = unique(rbind(all_dat,proj_dat))
pb = txtProgressBar(min=1,max=nrow(all_dat),style=3)
for(i in 1:nrow(all_dat)){
  rm(cc, year, p20, p80)
  setTxtProgressBar(pb, i)
  row = all_dat[i,]
  cc = row$CountryCode
  year = row$RequestYear
  
  p20 = NA
  attempt = 0
  while( is.na(p20) && attempt <= 10 ) {
    Sys.sleep(1)
    attempt <- attempt + 1
    try({
      p20 = optimise(function(x){return(abs(projection.threshold(cc,year,x)-0.2))}, lower=0.01, upper=100, tol=0.001)$minimum
    })
  }
  
  p80 = NA
  attempt = 0
  while( is.na(p80) && attempt <= 10 ) {
    Sys.sleep(1)
    attempt <- attempt + 1
    try({
      p80 = optimise(function(x){return(abs(projection.threshold(cc,year,x)-0.8))}, lower=0.01, upper=100, tol=0.001)$minimum
    })
  }
  
  tmp = data.frame(CountryCode=cc,ProjYear=year,p20=p20,p80=p80)
  data.list[[data.index]] = tmp
  data.index = data.index + 1
}
close(pb)
income_dat = rbindlist(data.list)
fwrite(income_dat, "output/p20_p80_incomes_Oct19.csv")