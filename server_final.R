# Define server logic required to draw a histogram



source('helper.R')

server <- function(input, output,session) {
  
 
  output$tsplot <- renderPlot({
   # productSales3years  <- as.numeric(df[input$radioProduct, 2:length(df)] )
    # draw the histogram with the specified number of bins
   # hist(productSales3years, col = 'darkgray', border = 'white')
    
    
    # If the following packages are not installed, uncomment and run the following line:
    # install.packages(c("tidyverse", "readxl", "forecast", "imputeTS", "VIM", "bsts"))
    
    # We also must load the helper functions from helper.R
    # This file contains function definitions for all those functions called below.
    # Please make sure that 'helper.R' is in the current working directory.
    
    
    print(input$radioProduct)
 
    df <- read_csv("HistoryTS.csv",na = c("0", "NA"))
    names(df) <- c("MC", paste(seq(as.Date("2016-01-01"), as.Date("2018-7-01"), by = 'month')))
    
    
    
    # If interested in generating forecast for a single mastercode, uncomment the following 2 lines and replace with master code of interest
    df <- df[df$MC == input$radioProduct,]
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
    markedOutliers <- markOutliers(checkedOutliers)
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
    clean <- imputeOutliersAndMissing(imputedOutliers)
    print("step8")
    # Write to a CSV
    # Make any changes necessary to clean.csv, then save the file.
    # Do not make any structure changes to the csv, only change numerical values.
    print("Clean")
    print(clean)
    View(df)
   cleanDataToExcel(clean,df,seasonal)
    
    # If everything is alright, reload it:
  clean <- loadClean("clean.csv", startYR =  2016, endYR = 2018, startMonth = 1, endMonth = 7)
    ### Generate 18 Month Forecasts
    # Then write these forecasts to a CSV
    
  
  par(mfcol = c(2, 1))
    print("step9")
   
    
   
    print("Clean")
    print(clean)
    
    print("clean[[1]][[2]]")
    print(clean[[1]][[2]])
    
    trainTestList <- trainTestListTS(clean[[1]][[2]],clean) 
    print(trainTestList)
    ts = c(trainTestList$train, trainTestList$test)
    ts = abs(ts)
    print(ts)
  #stlf = as.vector(forecast(stlf(ts,
   #                              method = 'ets',
    #                             lambda = 0), h=12)$mean)
 # print(stlf)
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

 
 
  
   
    
 
  
    
   
     forecast <- salesCast(clean)
    View(forecast)
   exportsalesCast( seasonal,forecast,"salesCast",df)
  
   
   if(input$radioModel == 'A'){
     pt2<-autoplot(forecast(auto.arima(clean[[1]][[2]], stepwise = F, lambda = 0, approximation = F), h = 18)) + labs(y = "Sales")
   }
   else if(input$radioModel == 'E'){
     pt2<-autoplot(forecast(stlf(clean[[1]][[2]], method = 'ets' ,lambda = 0), h = 18)) + labs(y = "Sales")
   }
   else if(input$radioModel == 'N'){
     pt2<-autoplot(forecast(nnetar(clean[[1]][[2]], lambda = 0), h = 18)) + labs(y = "Sales")
   }
   else{
     ss <- AddLocalLevel(list(), clean[[1]][[2]])
     ss <- AddSeasonal(ss, clean[[1]][[2]], nseasons = 12)
     test <- bsts(clean[[1]][[2]],
                  state.specification = ss,
                  niter = 10000,
                  family = "gaussian",
                  ping = 0)
     pred <- predict(test, horizon = 18, burn = 3000)
    pt2<-plot(pred, ylab = "Sales", main = "Bayesian Structural Time Series")
   }
   
   
   # Change the width of bars
   #ggplot(data=mapeData, aes(x=mapNames, y=acc)) +
    # geom_bar(stat="identity", width=0.5)
   # Change colors
   #ggplot(data=mapeData, aes(x=mapNames, y=acc)) +
    # geom_bar(stat="identity", color="blue", fill="white")
   # Minimal theme + blue fill color
   #p<-ggplot(data=mapeData, aes(x=mapNames, y=acc)) +
    # geom_bar(stat="identity", fill="steelblue")+
    # theme_minimal()
  # p
 
   mapeData <- data.frame( mapeNames=c("bayes","autoarima","nn"),
                           acc  = c(accuracy(bayes,trainTestList[[2]])[5],
                                    accuracy(autoarima,trainTestList[[2]])[5],
                                    accuracy(nn,trainTestList[[2]])[5]))
   print(head(mapeData))
   
   pt1<-ggplot(data=mapeData, aes(x=mapeNames, y=acc)) +
     geom_bar(stat="identity")
   print(pt1)
   
   ptlist<-c(pt2,pt1)
   #grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
   grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  
 
  
  #
  
   
  
  
 
}


