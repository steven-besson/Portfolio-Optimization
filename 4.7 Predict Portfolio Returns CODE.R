#1  Write the MONTHLY.HISTORY function
MONTHLY.HISTORY  <-  function(STOCKS , START , END) {
  NULL -> HISTORY
  library(quantmod)
  for (COFFEE in STOCKS) {
    getSymbols(COFFEE , from=START , to=END , auto.assign=F) -> PRICE
    monthlyReturn(PRICE[,6])  ->  RETURNS
    cbind(HISTORY,RETURNS) -> HISTORY
  }
  STOCKS -> colnames(HISTORY) 
  HISTORY
}



#2  Apply the MONTHLY.HISTORY function
MONTHLY.HISTORY( c("AAPL", "DAL", "HSY", "LLY", "SBUX","TSLA", "TGT") , 
                 "2017-8-1" , "2022-8-1" )  ->  stocks

head(stocks)



#3  Find the minimum-risk portfolio
library(fPortfolio)
as.timeSeries(stocks)  ->  STOCKS

head(STOCKS)

minriskPortfolio(STOCKS)  ->  SAFEST

SAFEST



#4  Save the stock weights
getWeights( SAFEST )  ->  SAFEST.WEIGHTS

SAFEST.WEIGHTS



#5  Save the stock symbols
colnames(STOCKS)  ->  MY.BABIES

MY.BABIES



#6  Create a custom function
GIMME.THAT <- function(SYMBOLS , WEIGHTS , RETURN , START)          {
  library(quantmod)
  0 -> PRICE
  for (i in 1:length(SYMBOLS))                   {
    getSymbols( SYMBOLS[i] , from=START , to=Sys.Date()+1 , 
                auto.assign=F ) -> ONE.PRICE
    ONE.PRICE * WEIGHTS[i] + PRICE  ->  PRICE   }
  ifelse(  lag.xts(PRICE[,6] , k=-252)  >=  (RETURN + 1)*PRICE[,6]  , 
           "YES" , "NO" )  ->  TARGET
  "GET.THAT.RETURN" -> colnames(TARGET)
  ADX( PRICE[,2:4] , n=252 )  ->  PREDICTORS
  data.frame( PREDICTORS , TARGET )  ->  ALL
  na.omit(ALL) -> ALL
  as.factor(ALL$GET.THAT.RETURN)  ->  ALL$GET.THAT.RETURN
  set.seed(99)
  sample( nrow(ALL) , 0.7*nrow(ALL) ) -> ROWS
  ALL[ROWS, ]  ->  GOUDA
  ALL[-ROWS, ]  ->  FETA
  library(C50)
  C5.0( GET.THAT.RETURN ~., data=GOUDA , trials=10) -> ORACLE
  predict(ORACLE,FETA) -> OMEN
  library(caret)
  confusionMatrix(OMEN, FETA$GET.THAT.RETURN)  -> TEST.RESULTS
  tail(PREDICTORS , 1) -> TODAY
  library(formattable)
  percent( predict(ORACLE , TODAY , type="prob") ) -> PREDICT.TODAY
  list(TEST.RESULTS , PREDICT.TODAY)                                  }



#7  Use the function
GIMME.THAT( SYMBOLS=MY.BABIES  ,  RETURN=0.15  ,
            WEIGHTS=SAFEST.WEIGHTS  ,  START="2011-1-1")
