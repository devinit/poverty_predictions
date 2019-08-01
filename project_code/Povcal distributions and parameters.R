required.packages <- c("reshape2","ggplot2","data.table","WDI")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

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
  dist[["dist"]] <- df
  dist[["mean"]] <- as.numeric(mean)
  return(dist)
}

if(!("all_dist.csv" %in% list.files("project_data") & "all_parameters.csv" %in% list.files("project_data"))){
  ext = povcal_svy()
  ext$svy_code = remap_cov(ext$CoverageType)
  ext = subset(ext,!is.na(svy_code))
  ext$C0 = paste(ext$CountryCode,ext$svy_code,sep="_")
  data.list = list()
  data.index = 1
  errs = c()
  pb = txtProgressBar(min=1, max=nrow(ext), style=3)
  for(i in 1:nrow(ext)){
    svy = ext[i,"C0"]
    year = ext[i,"DataYear"]
    PPP = ext[i,"PPP"]
    msg_lbl = paste(svy,year)
    setTxtProgressBar(pb, i, label=msg_lbl)
    dist.tmp = tryCatch({povcal_dist(svy, year)},error=function(e){return(NULL)})
    if(!is.null(dist.tmp)){
      dist.tmp[["svy"]] = svy
      dist.tmp[["year"]] = year
      dist.tmp[["PPP"]] = PPP
      data.list[[i]] = dist.tmp
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
    parameters.temp <- as.data.frame(rbind(GQ,Beta))
    parameters.temp$svy <- head(data.list[[i]]$svy,2)
    parameters.temp$year <- head(data.list[[i]]$year,2)
    colnames(parameters.temp) <- c("A","B","C","svy","year")
    parameters.list[[i]] <- parameters.temp
  }
  all_parameters <- rbindlist(parameters.list)
  fwrite(all_parameters, "project_data/all_parameters.csv")
  } else {
  all_dist <- fread("project_data/all_dist.csv")
  all_parameters <- fread("project_data/all_parameters.csv")
}
