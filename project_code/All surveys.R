
list.of.packages <- c("data.table","XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

remap_cov = function(x){
  cov_dict = c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov = Vectorize(remap_cov)

beta.cdf <- function(a,b,c,p){
  1-c*(p^a)*(1-p)^b*((a/p)-b/(1-p))}

beta.foc <- function(a,b,c,p){
  ((p^a)*((1-p)^b))*c*((a*(1-a)/p^2)+(2*a*b/(p*(1-p)))+(b*(1-b)/(1-p)^2))
}

beta.soc <- function(a,b,c,p){
  -((p^(a)*(1-p)^(b))*c)*((a*(1-a)*(2-a))/p^3+(3*a*(1-a)*b)/(p^2*(1-p))-(3*a*b*(1-b))/(p*(1-p)^2)-(b*(1-b)*(2-b))/(1-p)^3)
}

beta.toc.0 <- function(a,b,c) {
  p0=0.001
  N=1000
  tol=1E-15
  h <- 0.0001
  i <- 1
  p1 <- p0
  s <- numeric(N)
  while (i<=N) {
    df.dx <- (beta.soc(a,b,c,p0+h)-beta.soc(a,b,c,p0))/h
    p1 <- (p0 - 0.25*(beta.soc(a,b,c,p0)/df.dx))
    s[i] <- p1
    i <- i + 1
    if (abs(p1-p0) < tol) break
    p0 <- p1
  }
  return(s[i-1])
}


mode.p.beta <- function(a,b,c){
  1/beta.foc(a,b,c,beta.toc.0(a,b,c))
}

mode.z.beta <- function(a,b,c){
  beta.cdf(a,b,c,beta.toc.0(a,b,c))
}


povcal_svy = function(pl=1.9,group.by="WB"){
  url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params = list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "SurveyYears"="all",
    "Display"="C",
    "GroupedBy"=group.by,
    "format"="csv"
  )
  param_names = names(params)
  for(param_name in param_names){
    param = params[[param_name]]
    url = paste0(url,param_name,"=",param,"&")
  }
  url = substr(url,1,nchar(url)-1)
  return(fread(url))
}

povcal_dist = function(C0="AGO_3",Y0=2015){
  dist_url = paste0("http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0=",C0,"&Y0=",Y0)
  dist_html = htmlParse(dist_url, isURL=T)
  dist_root = xmlRoot(dist_html)
  dist_txt = xmlValue(getNodeSet(dist_root, "//pre")[[1]])
  start_point = gregexpr(pattern="---\r\n", dist_txt)[[1]][4]
  end_point = gregexpr(pattern="\r\n---", dist_txt)[[1]][5]
  if(start_point<end_point){
    txt_table = substr(dist_txt,start_point+5,end_point-1)
    df = read.table(text=txt_table, header=F, col.names=c("i","P", "L"))
  }else{
    return(NULL)
  }
  
  if((mean_start_point = gregexpr(pattern="mean in PPP", dist_txt)[[1]][1])==-1){
    mean_start_point = gregexpr(pattern="Mean:", dist_txt)[[1]][1]+6
    mean_end_point = gregexpr(pattern=" overall sum", dist_txt)[[1]][1]-5
  }else{
    mean_start_point = gregexpr(pattern="mean in PPP", dist_txt)[[1]][1]+14
    mean_end_point = gregexpr(pattern="Poverty line in", dist_txt)[[1]][1]-11
  }
  if(mean_start_point<mean_end_point){
    mean=substr(dist_txt,mean_start_point,mean_end_point)
  }else{
    mean=NULL
  }
  dist <- list()
  dist[["dist"]] <- df
  dist[["mean"]] <- as.numeric(mean)
#  dist[["PPP"]] <- PPP
  return(dist)
}


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
    data.list[[i]][["dist"]]$`L(1-L)` <- data.list[[i]][["dist"]]$L*(1-data.list[[i]][["dist"]]$L)
    data.list[[i]][["dist"]]$`(P^2-L)` <- (data.list[[i]][["dist"]]$P^2-data.list[[i]][["dist"]]$L)
    data.list[[i]][["dist"]]$`L(P-1)` <- data.list[[i]][["dist"]]$L*(data.list[[i]][["dist"]]$P-1)
    data.list[[i]][["dist"]]$`(P-L)` <- (data.list[[i]][["dist"]]$P-data.list[[i]][["dist"]]$L)
    data.list[[i]][["dist"]]$`ln(P-L)` <- log(data.list[[i]][["dist"]]$P-data.list[[i]][["dist"]]$L)
    data.list[[i]][["dist"]]$`ln(P)` <- log(data.list[[i]][["dist"]]$P)
    data.list[[i]][["dist"]]$`ln(1-P)` <- log(1-data.list[[i]][["dist"]]$P)
    data.list[[i]][["dist"]]$`ln(P-L)`[is.infinite(data.list[[i]][["dist"]]$`ln(P-L)`)] <- NA
    data.list[[i]][["dist"]]$`ln(P)`[is.infinite(data.list[[i]][["dist"]]$`ln(P)`)] <- NA
    data.list[[i]][["dist"]]$`ln(1-P)`[is.infinite(data.list[[i]][["dist"]]$`ln(1-P)`)] <- NA
    data.list[[i]][["gq"]] <- lm(`L(1-L)`~ 0 + `(P^2-L)` + `L(P-1)` + `(P-L)`,data.list[[i]][["dist"]],na.action=na.omit)$coefficients
    data.list[[i]][["beta"]] <- lm(`ln(P-L)`~ `ln(P)` + `ln(1-P)`,data.list[[i]][["dist"]],na.action=na.omit)$coefficients
    mode.beta <- tryCatch({c(as.numeric(mode.p.beta(data.list[[i]][["beta"]][2],data.list[[i]][["beta"]][3],exp(data.list[[i]][["beta"]][1]))),as.numeric(mode.z.beta(data.list[[i]][["beta"]][2],data.list[[i]][["beta"]][3],exp(data.list[[i]][["beta"]][1]))))},error=function(e){return(NULL)})
    if(!is.null(mode.beta)){
      data.list[[i]][["mode.beta"]] <- mode.beta
    } else {errs = c(errs, msg_lbl)}
  }else{
    errs = c(errs, msg_lbl)
  }
  
}

mode.list <- list()
i=1
for (i in 1:length(data.list)){
  mode.list[[i]] <- transpose(as.data.frame(c(data.list[[i]][["svy"]],data.list[[i]][["year"]],data.list[[i]][["PPP"]],data.list[[i]][["mean"]],data.list[[i]][["mode.beta"]])))}

all.beta.mode <- rbindlist(mode.list,fill=T)
colnames(all.beta.mode) <- c("svy","year","PPP","mean","mode.p.mu","mode.z/mu")

all.beta.mode$year <- as.numeric(all.beta.mode$year)
all.beta.mode$PPP <- as.numeric(all.beta.mode$PPP)
all.beta.mode$mean <- as.numeric(all.beta.mode$mean)
all.beta.mode$mode.p.mu <- as.numeric(all.beta.mode$mode.p.mu)
all.beta.mode$`mode.z/mu` <- as.numeric(all.beta.mode$`mode.z/mu`)
all.beta.mode$mode.p <- all.beta.mode$mode.p.mu/(all.beta.mode$mean*12/365.25)
all.beta.mode$mode.z <- all.beta.mode$`mode.z/mu`*all.beta.mode$mean*12/365.25

close(pb)

floorfit1 <- function(start,end,upper){
  fit1 <- nls(mode.z ~ Const + A*mode.p^-B ,data=subset(all.beta.mode,year <= end & year > start & mode.z <= upper),start=list(Const=0.01,A=0.5,B=1))
  #return(ggplot(all.beta.mode, aes(y=as.numeric(mode.z), x=as.numeric(mode.p), col=as.numeric(year))) +geom_point(data=subset(all.beta.mode,as.numeric(year)>start & as.numeric(year)<=end)) +stat_function(fun=function(x) coef(fit1)[1]+coef(fit1)[2]*x^(-coef(fit1)[3])))
  return(paste0("Estimated floor: ",round(coef(fit1)[1],3)))
}

floorfit2 <- function(start,end,upper){
  fit2 <- lm(log(mode.z) ~ log(1/mode.p), subset(all.beta.mode, year>start & year<=end & mode.z <= upper), na.action = na.omit)
  return(ggplot(all.beta.mode, aes(y=log(mode.z), x=log(1/mode.p),col=year)) +geom_point(data=subset(all.beta.mode,year>start & year<=end & mode.z <= upper)) +stat_function(fun=function(x) coef(fit2)[1]+coef(fit2)[2]*x))
  #return(paste0("Estimated floor: ",round(coef(fit2)[1],3))
}
  


