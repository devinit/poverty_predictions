wpc_grab <- function(country="all", year=2020){
  country <- paste0(country, collapse=",")
  year <- paste0(year, collapse=",")
  isos <- c("WORLD,AFG,ALB,DZA,AGO,ARG,ARM,AUS,AUT,AZE,BHS,BHR,BGD,BRB,BLR,BEL,BLZ,BEN,BTN,BOL,BIH,BWA,BRA,BRN,BGR,BFA,BDI,KHM,CMR,CAN,CPV,CAF,TCD,CHL,CHN,COL,COM,COG,COD,CRI,CIV,HRV,CUB,CYP,CZE,DNK,DJI,DOM,ECU,EGY,SLV,GNQ,ERI,EST,ETH,FJI,FIN,FRA,GAB,GMB,GEO,DEU,GHA,GRC,GTM,GIN,GNB,GUY,HTI,HND,HKG,HUN,ISL,IND,IDN,IRN,IRQ,IRL,ISR,ITA,JAM,JPN,JOR,KAZ,KEN,KSV,KWT,KGZ,LAO,LVA,LBN,LSO,LBR,LBY,LTU,LUX,MAC,MKD,MDG,MWI,MYS,MDV,MLI,MLT,MRT,MUS,MEX,MDA,MNG,MNE,MAR,MOZ,MMR,NAM,NPL,NLD,NZL,NIC,NER,NGA,PRK,NOR,OMN,PAK,PSE,PAN,PNG,PRY,PER,PHL,POL,PRT,PRI,QAT,ROU,RUS,RWA,LCA,VCT,WSM,STP,SAU,SEN,SRB,SLE,SGP,SVK,SVN,SLB,SOM,ZAF,KOR,SSD,ESP,LKA,SDN,SUR,SWZ,SWE,CHE,TWN,TJK,TZA,THA,TLS,TGO,TON,TTO,TUN,TUR,TKM,UGA,UKR,ARE,GBR,USA,URY,UZB,VUT,VEN,VNM,YEM,ZMB,ZWE")
  #Public key API download
  if(is.null(country)|country=="all"){country <- isos}
  
  ncountries <- nchar(country)-nchar(gsub(",","",country))+1
  nyears <- nchar(year)-nchar(gsub(",","",year))+1
  
  url <- paste0("https://api.worldpoverty.io/LB0Bq1Tq3HWjL3F5ycnf2IEqxILfUStr/ages/",country, "/", year, "/[00,INF)/both/1.9?format=binary&include_escape_rates=true")
  to.read <- file(url, "rb")
  bytes <- readBin(to.read, integer(), n=1000000, size=1, signed=F)
  close.connection(to.read)
  index <- seq(0, length(bytes)-1, 1)
  
  #Decryption of binary
  byteout <- list()
  datout <- list()
  for(i in 1:length(bytes)){
    index.char.code <- as.integer(substr(as.character(index[i]),1,1))+48 #Convert to decimal 
    byteout[[i]] <- bitwXor(bytes[i],index.char.code)
  }
  byteout <- unlist(byteout)
  for(i in 1:length(byteout)){
    index.char.code <- as.integer(substr(as.character(length(byteout)-index[i]),1,1))+48
    datout[[i]] <- bitwXor(byteout[i],index.char.code)
  }
  datout <- unlist(datout)
  rawdat <- as.raw(datout)

  countrylist <- unlist(strsplit(country,","))
  yearlist <- unlist(strsplit(as.character(year),","))
  
  if("WORLD" %in% countrylist){
    locworld <- (regexpr("WORLD",country)-1)[1]/4
    worldraw <- rawdat[((locworld*21):(nyears*29))]
    countryraw <- rawdat[-((locworld*21):(nyears*29))]
    ncountries <- ncountries-1
    countrylist <- countrylist[-(locworld+1)]
    worldout <- list()
    offset <- 0
    for(i in 1:nyears){
      y <- yearlist[i]
      if(offset > 0){
        wnumpoor <- readBin(worldraw[-(1:offset)], integer(), n=1, size=4)
        wpopulation <- readBin(worldraw[-(1:offset)], double(), n=3, size=4)[3]
      } else {
        wnumpoor <- readBin(worldraw, integer(), n=1, size=4)
        wpopulation <- readBin(worldraw, double(), n=3, size=4)[3]
      }
      whcr <- wnumpoor/wpopulation
      worldout[[i]] <- (data.frame(iso3c="WORLD",year=y,headcount=whcr,numpoor=wnumpoor,population=wpopulation))
      offset <- offset + 29
    }
    worldout <- rbindlist(worldout)
  } else {
    countryraw <- rawdat
  }
  
  allout <- list()
  offset <- 0
  for(i in 1:ncountries){
    c <- countrylist[i]
    tempout <- list()
    for(j in 1:nyears){
      y <- yearlist[j]
      if(offset > 0){
        numpoor <- readBin(countryraw[-(1:offset)], integer(), n=1, size=4)
        population <- readBin(countryraw[-(1:offset)], double(), n=3, size=4)[3]
      } else {
        numpoor <- readBin(countryraw, integer(), n=1, size=4)
        population <- readBin(countryraw, double(), n=3, size=4)[3]
      }
      hcr <- numpoor/population
      tempout[[j]] <- (data.frame(iso3c=c,year=y,headcount=hcr,numpoor=numpoor,population=population))
      offset <- offset + 21
    }
    allout[[i]] <- rbindlist(tempout)
  }
  allout <- rbindlist(allout)
  if(exists("worldout")){
    allout <- rbind(worldout,allout)
  }
  
  return(allout)
}
