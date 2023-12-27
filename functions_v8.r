
availability <- function(reservoirInflow, percentilesFile, startDate, endDate, write.file = F, outputReturn = "An"){

  # availability at the end of the irrigation season based on observed reservoir data and historic percentiles.
  # outputReturn: "percentiles", "An", "all"
  
  # open observed flows file and subset it to according to start and end date
  inflow <- read.table(reservoirInflow, header = T)
  inflowdt <- as.Date(paste(inflow[,1], inflow[,2], inflow[,3], sep = "-"), format = "%Y-%m-%d")
 
  row1 <- which(inflowdt == startDate)
  rown <- which(inflowdt == endDate)
  inflow2 <- inflow[row1:rown,]
  inflowdt2 <- inflowdt[row1:rown]
  
  # AI: Cumulative available water, AIXX: AI on Ddts
  AIh <- inflow2[, "accu_vol"]
  AIXX <- data.frame(dates = inflowdt2, day = format(inflowdt2, "%m-%d"), AIh)

  # AIne: cumulative available water at the end of the season, for percentile (per) on Ddts
  per <- read.table(percentilesFile, header = T) # Matrix: rows -> percentile number from 0 to 100 (rownames from 1 to 101), cols -> date
  colnames(per) <- format(seq(as.Date("2000-oct-01", "%Y-%b-%d"), as.Date("2001-sep-30", "%Y-%b-%d"), 1), "%m-%d")

  pAIn <- unlist(lapply(1:nrow(AIXX), function(i) {
    if(!is.na(AIXX[i, "AIh"])){
      perdy <- per[, as.character(AIXX[i, "day"])]
      val <- which(abs(perdy - AIXX[i, "AIh"]) == min(abs(perdy - AIXX[i, "AIh"])))
	  if(length(val > 1) & length(unique(perdy[val])) == 1){val <- val[1]}
      return(c(as.character(AIXX[i, 1]), perdy[val], which(perdy == perdy[val])[1] - 1, per[val, 365]))
    }
  }))

  pAIn <- data.frame(matrix(pAIn, ncol = 4, byrow = T))
  colnames(pAIn) <- c("dates", "pAIh", "per", "AIne") # pAIh = closest percentile to AIh. 
  
  pAIn$dates <- as.Date(pAIn$dates)
    pAIn[, c("pAIh", "per", "AIne")] <- data.frame(as.numeric(pAIn$pAIh), 
                                                   as.numeric(pAIn$per), 
                                                   as.numeric(pAIn$AIne))
    
  AIne <- data.frame(AIXX, pAIn[,-1])

  if(write.file == T){
    write.table(AIne, file.path(dirname(folder), "input_files", "An.txt"), col.names = T, row.names = F, append = F)
  }

  if(outputReturn == "all"){return(AIne)}
  if(outputReturn == "An"){return(AIne[, c("dates", "AIne")])}
  if(outputReturn == "percentiles"){return(AIne[, c("dates", "per")])}
 
}

availabilityF <- function(reservoirInflow, percentilesFile, SFforecastFile, startDate, endDate, write.file = F, outputReturn = "AnF"){
  
  # availability at the end of the irrigation season based on observed reservoir data, SF forecast data and historic percentiles.
  
  # outputReturn: "percentiles" OR "AnF"
  
  # open observed flows file and subset it to according to start and end date
  inflow <- read.table(reservoirInflow, header = T)
  inflowdt <- as.Date(paste(inflow[,1], inflow[,2], inflow[,3], sep = "-"), format = "%Y-%m-%d")
  
  row1 <- which(inflowdt == startDate)
  rown <- which(inflowdt == endDate)
  inflow2 <- inflow[row1:rown,]
  inflowdt2 <- inflowdt[row1:rown]
  
  # AI: Cumulative available water, AIXX: AI on Ddts
  AIh <- inflow2[, "accu_vol"]
  AIXX <- data.frame(dates = inflowdt2, day = format(inflowdt2, "%m-%d"), AIh)
  
  # Open SF forecast values (accumulated for the corresponding lead)
  SFforecast <- read.csv(SFforecastFile, header = T)
  SFforecastdt <- as.Date(paste(SFforecast$year, SFforecast$month, "01", sep = "-"))
  SFpos <- match(AIXX$dates, SFforecastdt)
  SFpos <- SFpos[!is.na(SFpos)]
  SFforecastXX <- SFforecast[SFpos, ]
  
  # subset AIXX with dates matching SFforecastXX
  AIpos <- match(as.Date(paste(SFforecastXX$year, SFforecastXX$month, '01', sep = '-')), AIXX$dates)
  AIXX2 <- AIXX[AIpos,]
  
  # Open percentile data and subset to the dates for the first of the month
  per <- read.table(percentilesFile, header = T) # Matrix: rows -> percentile number from 0 to 100 (rownames from 1 to 101), cols -> date
  colnames(per) <- format(seq(as.Date("2000-oct-01", "%Y-%b-%d"), as.Date("2001-sep-30", "%Y-%b-%d"), 1), "%m-%d")
  perpos <- match(unique(substr(AIXX2$dates, 6, 10)), colnames(per))
  perXX <- per[, c(perpos, ncol(per))]
  
  # Accumulation at each decision step, combining known accumulated inflow, accumulated SF forecast, and percentile extrapolation 
  SFcols <- SFforecastXX[, grep("SFx[0-9]", colnames(SFforecastXX))]
  combinedAI <- data.frame(dates = AIXX2$dates, year = SFforecastXX$year, month = SFforecastXX$month, 
                           day = '01', AIh = AIXX2$AIh, SFp = SFforecastXX$SFp, 
                           ensMeanSF = SFforecastXX$SFx, SFcols)
  
  month1 <- combinedAI$month[1]
  monthsHyear <- combinedAI$month[1:12]
  
  accuAfterLead <- combinedAI$AIh + combinedAI[, grep("SF", colnames(combinedAI))]
  
  matchingPer <- function(x){
    day <- substr(combinedAI$dates[x], 6, 10)
    pcol <- match(day, colnames(perXX)) + lead
    pcol[pcol > 13] <- 13
    P <- unlist(lapply(1:ncol(accuAfterLead), function(co) {
      which(abs(perXX[, pcol] - accuAfterLead[x, co]) == min(abs(perXX[, pcol] - accuAfterLead[x, co]))) - 1}))
  }	
  
  perAfterLead <- do.call(rbind, lapply(1:nrow(accuAfterLead), matchingPer))
  colnames(perAfterLead) <- colnames(accuAfterLead)
  
  # Accumulation at the end of the season based on percentile after lead
  accuEIS <- perXX[perAfterLead + 1, ncol(perXX)]
  accuEIS <- matrix(accuEIS, ncol = ncol(perAfterLead), nrow = nrow(perAfterLead))
  colnames(accuEIS) <- colnames(perAfterLead)
  accuEIS <- data.frame(dates = combinedAI$dates[!is.na(combinedAI$ensMeanSF)], accuEIS)
  
  # dates
  accuEISpos <- match(combinedAI$dates, accuEIS$dates)
  accuEISXX <- data.frame(dates = combinedAI$dates, accuEIS[accuEISpos, -1]) 
  perAfterLeadXX <- data.frame(dates = combinedAI$dates, perAfterLead[accuEISpos, ])
  
  # save perAfterLeadXX (Required for Farmers' decision) and accuEISXX (Required for Managers' decision) to input files folder.
  if(write.file == T){
    outfile1 <- file.path(dirname(reservoirInflow), "perAfterLead.txt")
    write.table(perAfterLeadXX, outfile1, col.names = T, row.names = F)
    outfile2 <- file.path(dirname(reservoirInflow), paste0("AnF_", lead, ".txt"))
    write.table(accuEISXX, outfile2, col.names = T, row.names = F, append = F)
  }
  
  # return output
  if(outputReturn == "AnF"){return(accuEISXX)}
  if(outputReturn == "percentiles"){return(perAfterLeadXX)}
}

tot_demand <- function(LCM_prop, SCM_prop){
  # This function calculates the total demand for the planted area [in hm3]
  
  # The variable crop surface (1/3 of total crop area) can be planted with LCM, SCM and rainfed crops. Maximum area planted with LCM or SCM depends on the proportion of each type of farmer.
  
  wrt_area <- cbind(year = wrt_year[,1], wrt_year[,-1] * total_area_third * 10^-5)
  wrt_area$LCM <- wrt_area$LCM * LCM_prop
  wrt_area$SCM <- wrt_area$SCM * SCM_prop
  
  wrt_area$totalD <- wrt_area$ALF + wrt_area$PCH + wrt_area$LCM + wrt_area$SCM
  return(wrt_area)
}

crops_after_month <- function(year, month, info, blc, ensth, LCMprop){
  
  # This function returns the final proportion of crops planted after given month (March, Apr, May)
  # Information can be: 'N' (historic record), 'F' (forecast) or 'P' (perfect)
  # blc corresponds to the water balance for T1 farmers (blc_dec1), N and F are in the same file (for farmers T2 the balance is recalculated taking into account the demand since the start of the irrigation season).
  
  iyear <- which(rownames(blc) == year)
  
  # crop matrix
  crops <- matrix(0, ncol = 8, nrow = 1)
  colnames(crops) <- c("SCB", "LCM", "SCM", "SCBl", "LCB", "FAL1", "FAL2", "SCB2") #SCBl is not used
  
  crops[1, 'SCB2'] <- T2 # T2 always plant SCB
  
  if(LCMprop == 0){
    DNov <- 0
    DFeb <- 0
    DApr <- 0
  } else {
    
    ### FARMER DECISION 1&2 (before irrigation season) ####
    # Nov: Decision R1 based on Nov balance
    # (if balance is negative, R1 plant LCB)
    info_Nov <- ifelse(info == 'P', 'PI_balance', paste0(info, '_Nov'))
    DNov <- ifelse(blc[iyear, info_Nov] < 0, 0, 1)
    
    if(DNov == 0){crops[1, 'LCB'] <- T1 * R1}
    
    # Feb: Decisions R1  R2 based on Feb balance
    # if balance is negative again, R2 plant SCB. If it starts to be negative after it was positive in Nov, R1 plant SCB too
    info_Feb <- ifelse(info == 'P', 'PI_balance', paste0(info, '_Feb'))
    DFeb <- ifelse(blc[iyear, info_Feb] < 0, 0, 1)
    if(DNov == 1 & DFeb == 0) {crops[1, 'SCB'] <- T1 * R2 + T1 * R1} 
    if(DNov == 0 & DFeb == 0) {crops[1, 'SCB'] <- T1 * R2}
    
    if(month == 3){return(cbind(crops, DNov, DFeb, DApr = NA, DMay = NA))} else {
      ### FARMER DECISION 3 (Apr, no variable irrigated crops yet)
      
      # Apr: Decisions R1, R2 and R3 based on Apr balance. No maize has been planted yet and perfect information is used for Alf and Peach in all cases, therefore demand doesn't need to be recalculated
      # if balance is positive after it was positive in Nov and Feb, R1 plant LCM
      # if balance is positive after it was positive in Feb, R2 plant LCM
      # if balance is positive, R3 plant LCM
      info_Apr <- ifelse(info == 'P', 'PI_balance', paste0(info, '_Apr'))
      DApr <- ifelse(blc[iyear, info_Apr] < 0, 0, 1)
      if(DApr == 1 & DFeb == 1 & DNov == 1){
        crops[1, 'LCM'] <- T1 * (R1 + R2 + R3)}
      if(DApr == 0 & DFeb == 1 & DNov == 1){
        crops[1, 'FAL1'] <- T1 * (R1 + R2 + R3)}
      if(DApr == 1 & DFeb == 1 & DNov == 0){
        crops[1, 'LCM'] <- T1 * (R2 + R3)}
      if(DApr == 0 & DFeb == 1 & DNov == 0){
        crops[1, 'FAL1'] <- T1 * (R2 + R3)} 
      if(DApr == 0 & DFeb == 0){
        crops[1, 'FAL1'] <- T1 * (R3)} 
      if(DApr == 1 & DFeb == 0){
        crops[1, 'LCM'] <- T1 * R3
      }
    }
  }
  
  
  if(month == 4) {return(cbind(crops, DNov, DFeb, DApr, DMay = NA))} else {
    ### FARMER DECISION 4 (May, LCM already planted - requires recalculating the balance)
    
    ## Assumes that farmers can use as much water as they need as long as there is availability. If there is not, they receive the same reduction from that point on.
    
    ## 1. Water balance assuming the crop is planted in May
    ## Assume that they decide to plant in May to calculate the balance. Then depending on the water balance determine the actual decision 
    LCMpropth <- crops[,'LCM']
    SCMpropth <- T2 # prop assuming that farmers T2 plant SCM
    wrt_area_may <- tot_demand(LCMpropth, SCMpropth)
    wrt_area_may <- wrt_area_may[wrt_area_may$year > startYear,]
    write.table(wrt_area_may, file.path(folder, 'totD_May.txt'), row.names = F)
    ## availability
    if(info == 'P' | info == 'N'){Av <- AIne}
    if(info == 'F'){Av <- AIneF_Eff}
    ## comparison
    if(info == 'P'){
      DA_may <- merge(wrt_area_may, Av[Av$day=='09-30',])
      DA_may$balance <- DA_may$AnEff - DA_may$totalD
      DA_may_year <- DA_may[DA_may$year == year, ]
    }
    if(info == 'N'){
      DA_may <- merge(wrt_area_may, Av[Av$day=='05-01',])
      DA_may$balance <- DA_may$AnEff - DA_may$totalD
      DA_may_year <- DA_may[DA_may$year == year, ]
    }
    if(info == 'F'){
      Av$year <- substr(Av$dates, 1, 4)
      DA_may <- merge(wrt_area_may, Av[substr(Av$dates, 6, 7)=='05',])
      DA_may$balance <- DA_may[, ensth] - DA_may$totalD
      DA_may_year <- DA_may[DA_may$year == year, ]
    }
    
    write.table(DA_may, file.path(folder, paste0('totD_May_', info, '.txt')), row.names = F)
    
    ## 2. Determine decision based on Water balance
    ## if balance is positive, T2 plant SCM, if it is negative nothing is planted
    DMay <- ifelse(DA_may_year$balance < 0, 0 , 1)
    if(DMay == 1) {crops[1, 'SCM'] <- T2}
    
    return(cbind(crops, DNov, DFeb, DApr, DMay))
  }
}

AD_for_value <- function(information, blc, ensth){
  # information can be 'N' (historic), 'F' (forecast), 'P' (perfect)
  # the result is the proportion of the planted crop that can be supported with the available water
  
  # 1. get crop proportions after each decision point after the start of the irrigation season: March, Apr and May
  crops_March <- do.call(rbind, lapply(hyears, crops_after_month, month = 3, info = information, blc, LCMprop = T1))
  crops_Apr <- do.call(rbind, lapply(hyears, crops_after_month, month = 4, info = information, blc, LCMprop = T1))
  crops_May <- do.call(rbind, lapply(hyears, crops_after_month, month = 5, info = information, blc, ensth = ensth, LCMprop = T1))
  
  rownames(crops_March) <- hyears
  rownames(crops_Apr) <- hyears
  rownames(crops_May) <- hyears
  
  ifelse(information == 'F', outname <- paste0(information, '_', ensth), outname <- information)
  outfile <- file.path(folder, paste0('crops_May_', outname, '.txt'))
  write.table(crops_May, outfile)
  
  # Final availability
  inflow <- read.table(reservoirInflow, header = T)
  av <- inflow[inflow$month==9 & inflow$day==30,]
  av <- av[av$year %in% hyears, c('year', 'accu_vol')]
  av$avEff <- av$accu_vol*alloFactor
  
  # Final demand
  # wrt_year: aggregated demand at the end of each year (per hectare per crop type)
  wrt_year <- wrt_year[wrt_year$Group.1 %in% hyears,]
  # wrt_area: aggregated demand for the total area third, in hectares
  wrt_area_crops <- wrt_year[,-1] * total_area_third * 10^-5
  wrt_area <- data.frame(year = wrt_year$Group.1, wrt_area_crops)
  # wrt_area 2: aggregated demand for the area planted with each crop
  wrt_area <- wrt_area[wrt_area$year > startYear,] #remove 1984
  wrt_area$LCM <- wrt_area$LCM * crops_May[, 'LCM']
  wrt_area$SCM <- wrt_area$SCM * crops_May[, 'SCM']
  wrt_area$Dtot <- rowSums(wrt_area[, -1])
  
  # combined table
  AD <- data.frame(av, wrt_area[, -1])
  AD$diff <- AD$avEff - AD$Dtot
  AD$pLCB <- crops_May[, 'LCB']
  AD$pSCB <- crops_May[, 'SCB'] + crops_May[, 'SCBl'] + crops_May[, 'SCB2']
  AD$pLCM <- crops_May[, 'LCM']
  AD$pSCM <- crops_May[, 'SCM']
  
  
  # For different types of information the proportion planted is compared against the proportion that can be supported (p2LCM and p2SCM represent the proportion that can be supported). 
  # When only either LCM or SCM are planted, the crop planted takes the whole curtailment. When both are planted they get the part proportional to the planted surface (e.g. diff divided by (pLCM+pLSCM)/pLCM)). 
  
  AD$p2LCM <- rep(NA, dim(AD)[1])
  AD$p2SCM <- rep(NA, dim(AD)[1])
  
  cond1 <- AD$pLCM != 0 & AD$pSCM != 0 & AD$diff < 0
  cond2 <- AD$pLCM == 0 & AD$pSCM != 0 & AD$diff < 0
  cond3 <- AD$pLCM != 0 & AD$pSCM == 0 & AD$diff < 0
  AD$p2LCM[cond1] <- (AD$LCM[cond1] - -AD$diff[cond1] * AD$pLCM[cond1]/(AD$pLCM[cond1] + AD$pSCM[cond1])) / AD$LCM[cond1]
  AD$p2SCM[cond1] <- (AD$SCM[cond1] - -AD$diff[cond1] * AD$pSCM[cond1]/(AD$pLCM[cond1] + AD$pSCM[cond1])) / AD$SCM[cond1]
  AD$p2SCM[cond2] <- (AD$SCM[cond2] - -AD$diff[cond2])/AD$SCM[cond2]
  AD$p2LCM[cond3] <- (AD$LCM[cond3] - -AD$diff[cond3])/AD$LCM[cond3]
  
  # if the value is negative convert to 0, this means that no surface of the crop can be supported and part of the ALF and PCH can't be supported either.
  AD$p2LCM[AD$p2LCM < 0] <- 0
  AD$p2SCM[AD$p2SCM < 0] <- 0
  
  outfile <- file.path(folder, paste0('AD_', outname, '.txt'))
  write.table(AD, outfile, row.names = F)
  
  return(AD)
}

calcYield <- function(information, AD_X, ensth){
  
  ### Yield tables (in tonnes/ha)
  yieldTables <- lapply(crops_var, function(x) {
    read.table(file.path(infolder, "Cd", paste0(x, "_WR_AC.txt")), header = T)})
  
  ### Subset yield tables to match hyear
  yieldTables2 <- do.call(cbind, lapply(1:length(yieldTables), function(i){
    yieldYears <- substr(yieldTables[[i]]$HarvestCD, 7, 10)
    yearLines <- yieldYears %in% hyears
    subtable <- yieldTables[[i]][yearLines,]
    yieldCol <- subtable$Yield
  }))
  
  yield <-  cbind(hyears, yieldTables2)
  colnames(yield) <- c("year", crops_var)
  
  totalYield <- yield[, -1] * total_area_third * AD_X[, paste0('p', crops_var)]
  colnames(totalYield) <- crops_var
  totalYield[, 'LCM'] <- ifelse(!is.na(AD_X$p2LCM), totalYield[, "LCM"] * AD_X$p2LCM, totalYield[, "LCM"])
  totalYield[, 'SCM'] <- ifelse(!is.na(AD_X$p2SCM), totalYield[, "SCM"] * AD_X$p2SCM, totalYield[, "SCM"])
  totalYield <- data.frame(hyear = as.numeric(yield[, 1]), totalYield, AD_X[, c('p2LCM', 'p2SCM')])  
  
  ifelse(information == 'F', outname <- paste0(information, '_', ensth), outname <- information)
  write.table(totalYield, file.path(folder, paste0("yield_", outname, ".txt")), row.names = F)
  
  return(totalYield)
}

calcVal <- function(information, AD_X, yield_X, ensth){
  # yield_X <- get(paste0('yield_', information))
  # AD_X <- get(paste0('AD_', information))
  print(ensth)
  gainBarley <- rowSums(yield_X[, c("SCB", "LCB")] * valBarley)
  gainMaize <- rowSums(yield_X[, c("LCM", "SCM")] * valMaize)
  costBarley <- rowSums(AD_X[, c("pSCB", "pLCB")] * cosBarley * total_area_third)
  costMaize <- rowSums(AD_X[, c("pLCM", "pSCM")] * cosMaize * total_area_third)
  val <- gainBarley + gainMaize - costBarley - costMaize
  names(val) <- yield_X[, 1]
  
  ifelse(information == 'F', outname <- paste0(information, '_', ensth), outname <- information)
  write.table(val, file.path(folder, paste0("val_", outname, ".txt")))
  write.table(cbind(gainBarley, gainMaize, costBarley, costMaize), file.path(folder, paste0('gain_', outname, '.txt')), row.names = names(val))
  return(val)
}

compareAD <- function(Eff_col, AIneX, Dtot, by.year = T){
  DA <- data.frame(year = as.numeric(substr(AIneX$dates, 1, 4)), month = as.numeric(substr(AIneX$dates, 6, 7)), AIneX)
  DA$D <- Dtot$totalD[match(DA$year, Dtot$year)]
  DA$D[DA$month >= 10] <- Dtot$totalD[match(DA$year[DA$month >= 10], Dtot$year - 1)]
  DA$balance <-  round(DA[, Eff_col] - DA$D, 2)
  
  # result by year and month
  years <- (DA$year[1] + 1):DA$year[nrow(DA)]
  res <- matrix(DA$balance[1:(12*floor(nrow(DA) / 12))], ncol = 12, nrow = floor(nrow(DA) / 12), byrow = T)
  colnames(res) <- month.abb[c(10:12, 1:9)]
  rownames(res) <- years[1:floor(nrow(DA) / 12)]
  
  ifelse(by.year == T, return(res), return(DA))
}

  
