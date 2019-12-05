required.packages <- c("reshape2","ggplot2","data.table","WDI","XML","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

ginicalc <- function(P,L){
  dP <- diff(P)
  dL <- diff(L)
  fL <- head(L,-1)
  dPL <- dP*dL
  dPfL <- dP*fL
  area <- sum(dPL) + sum(dPfL)
  gini <- 1-2*area
  return(gini)
}

all_parameters <- fread("project_data/all_parameters.csv")
afg.som.lby.param <- data.table(A=c(0.935463769,0.5760727,0.88737242,0.666110634,1.002559972,0.613658334)
                                ,B=c(-1.476794803,0.856948964,-1.254176897,0.95205282,-1.341360921,0.934277217)
                                ,C=c(0.083512829,0.49178234,0.196100839,0.562072863,0.241518659,0.607012527)
                                ,svy=c("AFG_3","AFG_3","SOM_3","SOM_3","LBY_3","LBY_3"),
                                year=c(2016.5,2016.5,2017,2017,2008,2008),
                                mean=c(2.788346962/12*365.2424,2.788346962/12*365.2424,1.69211737786/12*365.2424,1.69211737786/12*365.2424,4.9507954/12*365.2424,4.9507954/12*365.2424),
                                ppp=c(22.8388105,22.8388105,0.674879848994,0.674879848994,0.6577641,0.6577641),
                                type=rep(c("GQ","Beta"),3)
)

ext <- fread("project_data/povcalout.csv")

#all_parameters <- rbind(all_parameters, afg.som.lby.param)
all_parameters <- merge(all_parameters, ext[,c(4,8,21,34)], by.x=c("svy","year"), by.y=c("C0","DataYear"),all.x=T)
#all_parameters[which(svy=="AFG_3")]$CountryName <- "Afghanistan"
#all_parameters[which(svy=="SOM_3")]$CountryName <- "Somalia"
#all_parameters[which(svy=="LBY_3")]$CountryName <- "Libya"

#Only most recent surveys
recent_years <- all_parameters[all_parameters[, .I[which.max(year)], by=.(svy,type)]$V1]
parameters_GQ <- recent_years[recent_years[, .I[which.max(year)], by=svy]$V1]
parameters_Beta <- recent_years[recent_years[, .I[which.max(year)], by=svy]$V1+1]

beta.list <- list()
pb <- txtProgressBar(0,nrow(parameters_Beta),style=3)
for(i in 1:nrow(parameters_Beta)){
  svy <- parameters_Beta[i]
  mean <- svy$mean*12/365.2424
  pop <- svy$ReqYearPopulation*1000
  df <- data.table(P=seq(1/pop,1,1/pop))
  #df$p <- 1/pop
  df$z <- mean*(1-(svy$A*(df$P^svy$B)*((1-(df$P))^svy$C)*((svy$B/df$P)-(svy$C/(1-df$P)))))
  beta.list[[i]] <- df
  setTxtProgressBar(pb,i)
}
beta <- rbindlist(beta.list)
rm(beta.list)
beta <- beta[order(z)]
beta$P <- seq(1/nrow(beta),1,1/nrow(beta))

GQ.list <- list()
pb <- txtProgressBar(0,nrow(parameters_GQ),style=3)
for(i in 1:nrow(parameters_GQ)){
  svy <- parameters_GQ[i]
  mean <- svy$mean*12/365.2424
  pop <- svy$ReqYearPopulation*1000
  e <- -(svy$A+svy$B+svy$C+1)
  m <- svy$B^2-4*svy$A
  n <- 2*svy$B*e-4*svy$C
  df <- data.table(P=seq(1/pop,1,1/pop))
  #df$p <- 1/pop
  df$z <- mean*(-(svy$B/2)-((2*m*df$P+n)*((m*(df$P^2)+n*df$P+e^2))^(-0.5))/4)
  GQ.list[[i]] <- df
  setTxtProgressBar(pb,i)
}
GQ <- rbindlist(GQ.list)
rm(GQ.list)
GQ <- GQ[order(z)]
GQ$P <- seq(1/nrow(GQ),1,1/nrow(GQ))

beta[z<0]$z <- 0
GQ[z<0]$z <- 0
beta[is.na(z)]$z <- max(beta$z, na.rm=T)

beta$Z <- cumsum(beta$z)
beta$L <- beta$Z/max(beta$Z)

GQ$Z <- cumsum(GQ$z)
GQ$L <- GQ$Z/max(GQ$Z)
