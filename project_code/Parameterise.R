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
all.parameters <- rbindlist(parameters.list)