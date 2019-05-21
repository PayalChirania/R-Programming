# We need to load the following packages to use some of the functions contained within this file
library(tidyverse)
library(readxl)
library(forecast)
library(imputeTS)
library(VIM)
library(bsts)
library(readr)


prepareDataDF <- function(dataframe, startYR, endYR, startMonth, endMonth){
  # Use this function get a data frame in to 
  # format that can be used by `salesCast()` function
  # The first column should contain mastercodes
  # Each subsequent column should contain a monthly sales number
  # Important!! These must be in sequential order
  # startYR is the year of the first data in the timseries as integer.
  # endYR is the same, but year of the last data
  # startMonth and endMonth are the first and last month observed, as integers
  # For example, if our time series starts Jan 2016 and goes to June 2018, we 
  # would have startYR = 2016, endYR = 2018, startMonth = 1, endMonth = 6
  
  file <- dataframe
  
  tsList <- list()
  
  for(i in 1:nrow(file)){
    # Get the master code to use in naming list elements
    mc <- as.character(file[i,1])
    # Convert row to time series object
    ts <- ts(t(file[i,-c(1)]),
                frequency = 12)
    # Store ts object in a new list
    ts <- as.numeric(ts)
    tmp <- list(name = mc, timeseries = ts)
    # Append this list to `tsList` using master code naming convention
    tsList[[mc]] <- tmp
  }
  tsList
}
### Check Seasonality

# cleanTS is called by checkSeasonal to ensure that missing
# values are treated before performing the actual seasonality check
# These imputation are not retained, and are only done because NA's
# can cause checkSeasonal to fail.
cleanTS <- function(timeseries){
  # Simple wrapper for `tsclean` function in 
  # forecast package
  # Imputes missings values in time series and 
  # Removes outliers
  # Called when checking seasonality
  clean1 <- tsclean(timeseries, replace.missing = F)
  clean1 <- imputeTS::na.mean(clean1, option = "median")
  clean1
}


checkSeasonal <- function(preparedData){
  seasonal <- c()
  # Input should be the output of preparedData*()
  for(i in 1:length(preparedData)){
    cat(i, ": ", sep = "")
    ts <- ts(preparedData[[i]][[2]], frequency=12)
    if(sum(!is.na(ts))<2){
      seasonal[i] <- 0
    }else{
      ts <- cleanTS(ts)
      model <- ets(as.numeric(ts))
      seasonal[i] <- ifelse(model$components[[2]] == 'N', 0, 1)
    }
    cat("Done\n")
  }
  seasonal
}


### Check for Outliers and Missing Data

checkOutliers <- function(data){
  # Pass output from prepareData() to this function
  # Runs tsoutliers() function from forecast package on each product
  # Store the index of possible outliers and the reccomended replacement
  # Does this by appending list of outliers and replacements to data list
  library(forecast)
  output <- list()
  for(i in 1:length(data)){
    # Need at least two nonmissing data to check for outliers
    if(!sum(!is.na(data[[1]][[2]]))<2){
      mc = data[[i]][[1]]
      ts = data[[i]][[2]]
      ts = as.ts(ts, frequency = 12)
      
      ts_tmp <<- as.numeric(ts)
      # Uncomment the following line to use the unmodified tsoutliers() function from forecast package
      #check <- tsoutliers(data[[i]][[2]])
      check <- tsoutliers_(data[[i]][[2]])
      outlierIndices <<- check$index
      replacements <<- check$replacements
      
      tmp = list(mc = mc, ts = ts, outlierIndices = outlierIndices, replacements = replacements)
      
      output[[mc]] = tmp
    }else{
      mc = data[[i]][[1]]
      ts = data[[i]][[2]]
      outlierIndices <- numeric(0)
      replacements <- numeric(0)
      tmp = list(mc = mc, ts = ts, outlierIndices = outlierIndices, replacements = replacements)
      output[[mc]] = tmp
    }
  }
  return(output)
}

# Modified from forecast package to be more sensitive to outliers
tsoutliers_ <- function(x, iterate=2, lambda=NULL) {
  n <- length(x)
  freq <- 12
  
  # Identify and fill missing values
  missng <- is.na(x)
  nmiss <- sum(missng)
  if (nmiss > 0L) {
    xx <- na.interp(x, lambda = lambda)
  } else {
    xx <- x
  }
  
  # Check if constant
  if (is.constant(xx)) {
    return(list(index = integer(0), replacements = numeric(0)))
  }
  
  # Transform if requested
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda = lambda)
    lambda <- attr(xx, "lambda")
  }
  
  # Seasonally adjust data if necessary
  if (freq > 1 && n > 2 * freq) { 
    fit <- mstl(xx, robust=TRUE)
    # Check if seasonality is sufficient to warrant adjustment
    rem <- remainder(fit)
    detrend <- xx - trendcycle(fit)
    strength <- 1 - var(rem) / var(detrend)
    if (strength >= 0.6) {
      xx <- seasadj(fit)
    }
  }
  
  # Use super-smoother on the (seasonally adjusted) data
  tt <- 1:n
  mod <- supsmu(tt, xx)
  resid <- xx - mod$y
  
  # Make sure missing values are not interpeted as outliers
  if (nmiss > 0L) {
    resid[missng] <- NA
  }
  
  # Limits of acceptable residuals
  resid.q <- quantile(resid, prob = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + 2.5 * iqr * c(-1, 1)
  
  # Find residuals outside limits
  if ((limits[2] - limits[1]) > 1e-12) {
    outliers <- which((resid < limits[1]) | (resid > limits[2]))
  } else {
    outliers <- numeric(0)
  }
  
  # Replace all missing values including outliers
  x[outliers] <- NA
  x <- na.interp(x, lambda = lambda)
  
  # Do no more than 2 iterations regardless of the value of iterate
  if (iterate > 1) {
    tmp <- tsoutliers_(x, iterate = 1, lambda = lambda)
    if (length(tmp$index) > 0) # Found some more
    {
      outliers <- sort(c(outliers, tmp$index))
      x[outliers] <- NA
      x <- na.interp(x, lambda = lambda)
    }
  }
  
  # Return outlier indexes and replacements
  return(list(index = outliers, replacements = x[outliers]))
}

### Mark Outliers ###
# Input: the results of checkOutliers
markOutliers <- function(outlierDF){
  outliers <- outlierDF
  # Appends * to cells that are determines to contain outliers
  for(i in 1:length(outliers)){
    outliers[[i]]$ts <- as.character(outliers[[i]]$ts)
    if(length(outliers[[i]]$outlierIndices > 0)){
      for(j in 1:length(outliers[[i]]$outlierIndices)){
        cat(i," ", j, "\n")
        k <- outliers[[i]]$outlierIndices[j]
        outliers[[i]]$ts[k] <- capture.output(cat(paste(outliers[[i]]$ts[k],"out", sep = '')))
      }
    }
  }
  return(outliers)
}

# This function writes the results of checkOutliers to a CSV file
# Excel file is the original dataframe loaded into R
outlierToExcel <- function(markedOutliers, excelFile,seasonal, outputFileName = "MarkedOutliers.csv"){
  # Pass results from markOutliers() function
  # Also pass data frame created from read_excel() above
  # preparedData is the output of prepareData*()
  print("Excel details")
  print(excelFile)
  print(markedOutliers)
  print(length(markedOutliers))
  print(length(markedOutliers[[1]]$ts))

  for(i in 1:length(markedOutliers)){
    for(j in 1:length(markedOutliers[[i]]$ts)){
      if(grepl("out", markedOutliers[[i]]$ts[j])){
        excelFile[i, j+1] <- markedOutliers[[i]]$ts[j]
      }
    }
  }
  excelFile <- cbind(seasonal, excelFile)
  write_excel_csv(excelFile, outputFileName)

}

# Impute those cells determined to contain outliers
imputeOutliers <- function(outlierDF){
  outliers <- outlierDF
  # Appends  to cells that are determines to contain outliers
  for(i in 1:length(outliers)){
    outliers[[i]]$ts <- as.character(outliers[[i]]$ts)
    if(length(outliers[[i]]$outlierIndices > 0)){
      for(j in 1:length(outliers[[i]]$outlierIndices)){
        cat(i," ", j, "\n")
        k <- outliers[[i]]$outlierIndices[j]
        outliers[[i]]$ts[k] <- capture.output(cat(paste(outliers[[i]]$replacements[j],"imp", sep = '')))
      }
    }
  }
  return(outliers)
}

# Generate our CSV file of imputed values
imputeToExcel <- function(imputedOutliers, excelFile ,seasonal,outputFileName = "ImputedOutliers.csv"){
  # Pass results from markOutliers() function
  # Also pass data frame created from read_excel() above
  for(i in 1:length(imputedOutliers)){
    for(j in 1:length(imputedOutliers[[i]]$ts)){
      if(grepl("imp", imputedOutliers[[i]]$ts[j])){
        excelFile[i, j+1] <- imputedOutliers[[i]]$ts[j]
      }
    }
  }
  excelFile <- cbind(seasonal, excelFile)
  write_excel_csv(excelFile, outputFileName)
}

# Generate the clean data file by imputing missing values and outliers
imputeOutliersAndMissing <- function(data){
  # Pass results from imputeOutliers
  # Runs tsoutliers() function from forecast package on each product
  # Store the index of possible outliers and the reccomended replacement
  # Does this by appending list of outliers and replacements to data list
  output <- list()
  for(i in 1:length(data)){
    # Need at least two nonmissing data to check for outliers
    if(sum(!is.na(data[[i]][[2]]))>2){
      # Add noise if too many missing
      if(TRUE){
        mc = data[[i]][[1]]
        ts = data[[i]][[2]]
        clean1 <- sub("imp", "", ts, fixed = T)
        clean1 <- as.numeric(clean1)
        clean1 <- ts(clean1, frequency = 12)
        #clean <- na.seadec(clean, "mean")
        #clean <- na.interpolation(clean, "linear")
        #clean <- na.interpolation(clean, "spline")
        #clean <- na.mean(clean, 'mean')
        #clean <- na.kalman(clean)
        clean1 <- imputeTS::na.mean(clean1, "median")
        tmp = list(mc = mc, ts = clean1)
        output[[mc]] = tmp
      }
    }else{
      mc = data[[i]][[1]]
      ts = data[[i]][[2]]
      clean1 <- sub("imp", "", ts, fixed = T)
      tmp = list(mc = mc, ts = rep(NA,length(clean1)))
      output[[mc]] = tmp
    }
  }
  return(output)
}

cleanDataToExcel <- function(noOutlierOrMiss, excelFile, seasonal,outputFileName = "clean.csv"){
  copy <- excelFile
  print(copy)
  print(noOutlierOrMiss)
  print(length(noOutlierOrMiss))
 # print(seasonal)
  # Pass results from imputeOutliersAndMissing() function
  # Also pass data frame created from read_excel() above
  for(i in 1:length(noOutlierOrMiss)){
    copy[i,-1] <- as.vector(noOutlierOrMiss[[i]]$ts)
    
  }
  copy <- cbind(seasonal,copy)
  write_excel_csv(copy, "clean.csv")
}

cleanToDF <- function(clean1){
  datalist <- list()
  for(i in 1:length(clean1)){
    cat(i, " - ")
    name <- data.frame()
    name[1,1] <- clean1[[i]][[1]]
    tmp <- data.frame(matrix(NA, nrow = 1, ncol = length(clean1[[i]][[2]])))
    for(j in 1:length(clean1[[i]][[2]])){
      tmp[1,j] <- clean1[[i]][[2]][j]
    }
    tmp <- cbind(name,tmp)
    datalist[[i]] <- tmp
    cat("Done\n")
  }
  cat("Does do.call work?")
  out = do.call(rbind, datalist)
  cat(" - Yes\n")
  cat("Success\n")
  return(out)
}

preprocess.svd <- function(train, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself.
  # This is for noise reduction. The intuition is that characteristics
  # that are common across stores (within the same department) are probably
  # signal, while those that are unique to one store may be noise.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # n.comp - the number of components to keep in the singular value
  #         decomposition
  #
  # returns:
  #  the rank-reduced approximation of the training data
  
  # Taken from kaggle
  train[is.na(train)] <- 0
 
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)

  train
}

# For loading the clean data back in to R
loadClean <- function(filename, startYR, endYR, startMonth, endMonth){
  # Use this function get excel spreadsheet in to 
  # format that can be used by `salesCast()` function
  # The first row of the spreadsheet should only have column names
  # The first column should contain mastercodes
  # Each subsequent column should contain a monthly sales number
  # Important!! These must be in sequential order
  # filename should be a string of the filename, including extension
  # If the file is not in the working directory of the current R session
  # pass the full file path for `filename`
  # startYR is the year of the first data in the timseries as integer.
  # endYR is the same, but year of the last data
  # startMonth and endMonth are the first and last month observed, as integers
  # For example, if our time series starts Jan 2016 and goes to June 2018, we 
  # would have startYR = 2016, endYR = 2018, startMonth = 1, endMonth = 6
  file <- read_csv(filename)
  
  tsList <- list()
  
  for(i in 1:nrow(file)){
    # Get the master code to use in naming list elements
    mc <- as.character(file[i,2])
    # Convert row to time series object
    ts <- ts(t(file[i,-c(1:2)]),
             frequency = 12)
    
    # Store ts object in a new list
    tmp <- list(name = mc, timeseries = ts)
    
    # Append this list to `tsList` using master code naming convention
    tsList[[mc]] <- tmp
  }
  tsList
}

trainTS_bayes <- function(trainTestList, trainMode = 1, index = NULL, monthsToForecast = 18, mape = NULL){
  # Pass a list element created by trainTestSplitTS
  # Uses training set (first element of list) to train
  # time series models. Make monthsToForecast month predictions with each
  # of the number of models chosen
  # Outputs a list where each element is a 6 month prediction using
  # the given method
  if(sum(is.na(trainTestList[[1]]))>0){
    if(trainMode == 1){
      return(list(model = NA, forecast = rep(NA,12)))
    }else{
      return(list(model = NA, rep(NA, monthsToForecast)))
    }
  }
  if(!sd(trainTestList[[1]])>0){
    trainTestList[[1]] <- trainTestList[[1]] + abs(rnorm(length(trainTestList[[1]]),0,2))
  }
  # If trainMode = 1, train only using trainset
  # If trainMode = 0, train on train and test set
  if(trainMode == 1){
    ts = trainTestList[[1]]
    ts = abs(ts)
    ts = ts(as.numeric(ts), frequency = 12)
    
  }else{
    ts = c(trainTestList$train, trainTestList$test)
    ts = abs(ts)
    ts = ts(as.numeric(ts), frequency = 12)
    
    if(index == 1){
      return(list(model = index, 
                  forecast = as.vector(forecast(stlf(ts,
                                                     method = 'ets',
                                                     lambda = 0),
                                                h=monthsToForecast)$mean), 
                  mape = mape))
      
    }
    if(index == 2){
      ss <- AddLocalLevel(list(), ts)
      ss <- AddSeasonal(ss, ts, nseasons = 12)
      test <- bsts(ts,
                   state.specification = ss,
                   niter = 10000,
                   family = "gaussian",
                   ping = 0)
      pred <- predict(test, horizon = monthsToForecast, burn = 3000)
      return(list(model = index, forecast = as.vector(pred$median), mape = mape))
    }
    if(index == 3){
      return(list(model = index, 
                  forecast = as.vector(forecast(auto.arima(ts, 
                                                           lambda = 0,
                                                           stepwise = F,
                                                           approximation = F)
                                                ,h=monthsToForecast)$mean), 
                  mape = mape))
    
    }
    if(index == 4){
      return(list(model = index, 
                  forecast = as.vector(forecast(nnetar(ts, scale.inputs = T, lambda = 0),h=monthsToForecast)$mean), 
                  mape = mape))
    }
  }  
  stlf = as.vector(forecast(stlf(ts,
                               method = 'ets',
                              lambda = 0), h=12)$mean)
  ss <- AddLocalLevel(list(), ts)
  ss <- AddSeasonal(ss, ts, nseasons = 12)
  test <- bsts(ts,
               state.specification = ss,
               niter = 2000,
               familty = "gaussian",
               ping = 0)
  pred <- predict(test, horizon = 12)
  bayes <- as.numeric(pred$mean)
  autoarima = as.vector(forecast(auto.arima(ts, 
                                            lambda = 0,
                                            stepwise = F,
                                            approximation = F),h=12)$mean)
  nn = as.vector(forecast(nnetar(ts, scale.inputs = T, lambda = 0),h=12)$mean)
  acc <- c(accuracy(stlf, trainTestList[[2]])[5],
           accuracy(bayes,trainTestList[[2]])[5],
           accuracy(autoarima,trainTestList[[2]])[5],
           accuracy(nn,trainTestList[[2]])[5])
  
  mapeDF <<- data.frame( mapeNames=c("Stlf","Bayes","Autoarima","Neural"),
                         acc  = acc)
  
  minInd <- which.min(acc)
 

  trainTS_bayes(trainTestList, trainMode = 0, index = minInd, monthsToForecast = monthsToForecast, mape = acc[minInd])
}

trainTestListTS <- function(timeseries,clean1){
  # Should pass a clean time series (no missing values, no
  # outliers). Function splits data in train and test sets
  # by removing last 6 months of time series for validation.
  # Stores results in a list object. First element is train
  # series, second element is test time series
  train <- timeseries[-c((length(clean1[[1]]$timeseries)-5):(length(clean1[[1]]$timeseries)))]
  test <- timeseries[c((length(clean1[[1]]$timeseries)-5):(length(clean1[[1]]$timeseries)))]
  list('train' = train, 'test' = as.vector(test))
}

salesCast<- function(tsListObject, monthsToForecast = 18){
  # Takes list of the following form: each element of the list is itself a 
  # list. The first element of each nested list is a character string corresponding
  # to a MasterCode. The second element of each nested list is a timeseries object
  # corresponding to the monthly sales of that master code
  # Pass integers of number of desired forecast
  # eg. pass monthsToForecast = 18 for 18 month forecast
  # Output will be a list of similar structure -- each element of the list will be
  # itself a list. The first element of the sublist will be the character string
  # of the product's mastercode. The second element of each sublist will be the 6 month
  # sales forecast for that mastercode. If a forecast is unable to be made, this
  # second element of each nested list will be NULL. The third element will be training mape.
  # Fourth is type of model used
  
 # if(length(tsListObject) == 1){
#    singleForecast(tsListObject) 
 # }
  
  
  
  
  output <- list()
  
  # We iterate over all products in our tsListObject
  for(i in 1:length(tsListObject)){
    ### DEBUG
    cat(i, " - ", tsListObject[[i]][[1]], ":\n ")
    # The function first checks that we can make a forecast. If the number of missing
    # values is greater than 2, we can't forecast this mastercode
    if(sum(is.na(tsListObject[[i]][[2]]))>0){
      mastercode = tsListObject[[i]][[1]]
      forecast = rep(NA, monthsToForecast)
      mape = NA
      model = NA
      tmp = list(mastercode = mastercode, forecast = forecast, mape = mape, model = model)
      output[[mastercode]] <- tmp
    }else{
      cleanData <- tsListObject[[i]][[2]]
      print("Data is cleaned")
      print(cleanData)
      # Next we perform a train-test split on the newly cleaned timeseries
      trainTestList <- trainTestListTS(timeseries = cleanData,tsListObject)
      print(trainTestList)
      # Now train a number of models on the training set
      fc <- trainTS_bayes(trainTestList)
      
      
      
      
      
      
      #returns
      mastercode = tsListObject[[i]][[1]]
      forecast = fc[[2]]
      mape = as.numeric(fc[[3]])
      model = fc[[1]]
    tmp = list(mastercode = mastercode, forecast = forecast, mape = mape, model = model )
      output[[mastercode]] <- tmp
      
    }
    cat("\nDone\n")
  } 
  return(output)
}

exportsalesCast <- function(seasonal,salesCastList, filename = "salesCast", originalDF = df){
  # This functions creates a data frame from the results of salesCast
  # Each row corresponds to the forecasts of a master code
  # 1 through 6 refer to the forecast for 1 through 6 months into the future
  # Mape is the mean absolute percent error

  datalist = list()
  models <- c("stlf", "bayes", "auto.arima", "neuralnet")
  
  for(i in 1:length(salesCastList)){
    cat(i, " - ")
    tmp1 <- data.frame(MasterCode = salesCastList[[i]][[1]])
    tmp2 <- data.frame(matrix(NA, nrow = 1, ncol = length(salesCastList[[i]][[2]])))
    for(j in 1:length(salesCastList[[i]][[2]])){
      tmp2[1,j] <- salesCastList[[i]][[2]][j]
    }
    names(tmp2) <- paste("Month", 1:length(salesCastList[[1]][[2]]), sep = "")
    tmp3 <- data.frame(Model = salesCastList[[i]][[4]])
    tmp3[1,1] <- ifelse(tmp3[1,1] %in% 1:4, models[tmp3[1,1]], tmp3[1,1])
    tmp4 <- data.frame(Mape = salesCastList[[i]][[3]])
    tmp <- cbind(tmp1,tmp2, tmp3, tmp4)
    datalist[[i]] <- tmp
    cat("Done\n")
  }
  cat("Does do.call work?")
  out = do.call(rbind, datalist)
  cat(" - Yes\n")
  cat("Does cbind work?")
  print("out in exportsalescast")
  #print(out)
  #print(df)
  out <- cbind(originalDF, out[,-1])
  out <- cbind(seasonal, out)
  cat(" - Yes\n")
  cat("Generating CSV file.")
  cat(".")
  cat(".\n")
  write_excel_csv(out, paste(filename,length(salesCastList[[1]][[2]]), ".csv", sep = ""))
  cat("Success: Forecasts written to ", filename, length(salesCastList[[1]][[2]]), ".csv", sep = "")
}

singleForecast <- function(tsListObject){
  print(autoplot(forecast(auto.arima(tsListObject[[1]][[2]], stepwise = F, lambda = 0, approximation = F), h = 18)) + labs(y = "Sales"))
 # print(autoplot(forecast(stlf(tsListObject[[1]][[2]], method = 'ets' ,lambda = 0), h = 18)) + labs(y = "Sales"))
 # print(autoplot(forecast(nnetar(tsListObject[[1]][[2]], lambda = 0), h = 18)) + labs(y = "Sales"))
  ss <- AddLocalLevel(list(), tsListObject[[1]][[2]])
  ss <- AddSeasonal(ss, tsListObject[[1]][[2]], nseasons = 12)
  test <- bsts(tsListObject[[1]][[2]],
               state.specification = ss,
               niter = 10000,
               family = "gaussian",
               ping = 0)
  pred <- predict(test, horizon = 18, burn = 3000)
 # print(plot(pred, ylab = "Sales", main = "Bayesian Structural Time Series"))
}


#Graph plotting rendering through input.
outputPlotData <- function(Product){
  df <- read_csv("HistoryTS.csv",na = c("0", "NA"))
  names(df) <- c("MC", paste(seq(as.Date("2016-01-01"), as.Date("2018-7-01"), by = 'month')))
  
  
  
  # If interested in generating forecast for a single mastercode, uncomment the following 2 lines and replace with master code of interest
  df <- df[df$MC == Product,]
  View(df)
  # The following code generates a csv files of products that aren't forecastable because they do not have enough data
  # These rows are not removed from the original data table
  filterMissing <- c()
  for(i in 1:nrow(df)){
    if(sum(!is.na(df[i, ])) <= 6){
      filterMissing[i] = 0
    }else{
      filterMissing[i] = 1
    }
  }
  
  # To keep only forecastable rows
  #df <- df[filterMissing == 1,] 
  
  # Writes CSV of nonforecastable data
  noFC <- df[filterMissing == 0,] 
  write_csv(noFC, "notForecastable.csv")
  
  print("Step1")
  ### First we must prepare the data so that it can be used with our software
  data <- prepareDataDF(df, startYR =  2016, endYR = 2018, startMonth = 1, endMonth = 7)
  print("Step2")
  ### Now we check the seasonality of the time series
  seasonal <- checkSeasonal(data)
  
  print("Step3")
  print(seasonal)
  ### Now we check the data for outliers and generate a CSV file with all of these
  # outliers marked by 'out'
  checkedOutliers <- checkOutliers(data)
  print("Step4")
  markedOutliers <<- markOutliers(checkedOutliers)
  print("Step5")
  outlierToExcel(markedOutliers, df,seasonal)
  print("Step6")
  
  ### Now we replace those cells marked by 'out' with their new values
  # The algorithm uses interpolation to generate these values.
  # Imputation are marked by 'imp'
  imputedOutliers <- imputeOutliers(checkedOutliers)
  imputeToExcel(imputedOutliers, df,seasonal)
  print("step7")
  
  ### Now we deal with missing values and generate a cleaned data table that can be 
  # used in forecasting. Current method uses 'median' imputation to deal with missing values.
  # To change the method of missing value imputation, comment line 278 of the  
  # 'imputeOutliersAndMissing()` function in helper.R and uncomment one of the lines above it.
  # If changes are made, be sure to save `helper.R` and call source('helper.R') again.
  clean1 <- imputeOutliersAndMissing(imputedOutliers)
  print("step8")
  # Write to a CSV
  # Make any changes necessary to clean.csv, then save the file.
  # Do not make any structure changes to the csv, only change numerical values.
  print("Clean1")
  print(clean1)
  View(df)
  cleanDataToExcel(clean1,df,seasonal)
  
  # If everything is alright, reload it:
  clean <- loadClean("clean.csv", startYR =  2016, endYR = 2018, startMonth = 1, endMonth = 7)
  ### Generate 18 Month Forecasts
  # Then write these forecasts to a CSV
  
  Monthlyforecast <<- salesCast(clean)
 
                      
 # print(Monthlyforecast)
  exportsalesCast( seasonal,Monthlyforecast,"salesCast",df)
  
  return(clean)
}



