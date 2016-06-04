#################################################################### 
#### Load packages 
#####################################################################
library(dplyr)
suppressMessages(require(quantstrat))
require(IKTrading)
require(RSQLite)

####################################################################
#### remove objects 
########################################################################

rm(list = ls(all.names = T))

if(!exists(".instrument")) .instrument <<- new.env()
if(!exists(".blotter")) .blotter <<- new.env()
if(!exists(".strategy")) .strategy <- new.env()



########################################################################
#### DEFINE VARIABLES or parameters 
########################################################################
smalltest=TRUE
initDate      = "2000-01-01"
symbol.st     = 'CYB_DAY'
portf.st      = 'bug'
acct.st       = 'colony'
strat.st      = 'bee'
initEq        = 100000
nFast         = 10
nSlow         = 30
nSd           = 1

currency('USD')
Sys.setenv(TZ="UTC")
.timespans<-c('T06:00/T10:00', 'T07:00/T11:00', 'T08:00/T12:00', 'T09:00/T13:00', 'T10:00/T14:00', 'T11:00/T15:00', 'T12:00/T16:00')
.timespan = NULL
.threshold = 0.015 #stop loss theshold improvement 0.015, initial was 0.0005
.stoploss <- 0.40/100 #optimization range
.stoptrailing <- 0.8/100 #optimization range
.takeprofit <- 2.0/100 #optimization range
stopLossPercent=0.015  #initial 0.003, optimized 0.015
trailingStopPercent=0.015 #initial 0.005, optimized 0.015
tradeSize=100

.smoothing=10:20
.StopLoss = seq(0.05, 2.4, length.out=5)/100
.StopTrailing = seq(0.05, 2.4, length.out=5)/100
.TakeProfit = seq(0.1, 4.8, length.out=5)/100
.nsamples=5
smalltest.initdate<-'2016-01-01'

########################################################################
#### GET DATA
###############################################################
# set your working directory where the data is stored
setwd("/Users/tyamada/Google Drive/CFRM551/supplemental/")
currency('USD') # initiate currency


getSymbols(Symbols = symbol.st, src = "csv")
stock(symbol.st ,currency='USD', multiplier=1) # initiate stock
initPortf(
  portf.st, 
  symbol.st, 
  initDate=initDate) # initiate portfolio

initAcct(
  acct.st, 
  portf.st, 
  initEq=initEq, 
  initDate=initDate) # initiate account

initOrders(
  portf.st, 
  initDate=initDate ) # initiate order_book

bee = strategy(strat.st) # create strategy object



addPosLimit(
  portfolio=portf.st,
  symbol=symbol.st, 
  timestamp=initDate,  
  maxpos=300, longlevels = 3) # only trade in one direction once 


bee <- add.indicator( 
  strategy  = strat.st, 
  name      = 'BBands', # TA name
  arguments = list(HLC=quote(HLC(mktdata)), 
                   n=nSlow, 
                   sd=nSd),
  label     = 'BBand')

#### SMA column
bee <- add.indicator(
  strategy  = strat.st, 
  name      = 'SMA', # TA name
  arguments = list(x=quote(Cl(mktdata)), 
                   n=nFast),
  label     = 'MA' )



bee <- add.signal(
  strategy  = strat.st,
  name      = 'sigCrossover',
  arguments = list(columns=c('MA','dn'), 
                   relationship='lt'),
  label     = 'MA.lt.dn')

#### SMA cross over upperBand
bee <- add.signal(
  strategy  = strat.st,
  name      = 'sigCrossover',
  arguments = list(columns=c('MA','up'),
                   relationship='gt'),
  label     = 'MA.gt.up')
### add signal using formula to check parallel works on sigAND
# add.signal(strat.st, name="sigAND",arguments=list(columns=c("MA.lt.dn","MA.gt.up"),cross=TRUE),label="unlikleyCross")
### add signal using formula to check parallel works
add.signal(strat.st, name="sigFormula",arguments=list(columns=c("MA.lt.dn","MA.gt.up"),formula="(MA.lt.dn==1 & MA.gt.up==1 & MA.gt.up==1)",cross=TRUE),label="unlikleyCross")
n_aroon=15
add.indicator(  strat.st,name = "aroon",arguments = list(HL = quote(merge( Hi(mktdata),Lo(mktdata)  )),n = n_aroon),label = 'ar_ind'  ) 
add.signal(strat.st, name="sigComparison",arguments=list(columns=c("Up","Dn")),relationship="gt",label="longfilter")
add.signal(strat.st, name="sigComparison",arguments=list(columns=c("Dn","Up")),relationship="gt",label="shortfilter")
add.signal(strat.st, name="sigThreshold",arguments=list(column="Up",threshold=100,relationship="eq",cross=FALSE),label="aroonU_EQ100")
add.signal(strat.st, name="sigThreshold",arguments=list(column="Dn",threshold=30,relationship="lte",cross=FALSE),label="aroonD_LTE30")
add.signal(strat.st, name="sigThreshold",arguments=list(column="Dn",threshold=100,relationship="eq",cross=FALSE),label="aroonD_EQ100")
add.signal(strat.st, name="sigThreshold",arguments=list(column="Up",threshold=30,relationship="lte",cross=FALSE),label="aroonU_LTE30")
add.signal(strat.st, name="sigFormula",arguments=list(columns=c("aroonU_EQ100","aroonD_LTE30","longfilter"),formula="(aroonU_EQ100==1 & aroonD_LTE30==1 & longfilter==1)",cross=TRUE),label="longEntry")
add.signal(strat.st, name="sigFormula",arguments=list(columns=c("aroonD_EQ100","aroonU_LTE30","shortfilter"),formula="(aroonD_EQ100==1 & aroonU_LTE30==1 & shortfilter==1)",cross=TRUE),label="shortEntry")
# stop loss with orderset
add.rule(strat.st,name='ruleSignal',arguments=list(sigcol="longEntry",sigval=TRUE,orderqty=100,ordertype='stoplimit',threhold=.threshold,orderside='long',orderset='ocolong',replace=FALSE ,osFUN= osMaxPos),type='enter',timespan = .timespan,label='LE')
add.rule(strat.st,name='ruleSignal',arguments=list(sigcol="shortEntry",sigval=TRUE,orderqty=-100,ordertype='stoplimit',threshold=.threshold,orderside='short',orderset='ocoshort',replace=FALSE,osFUN= osMaxPos),type='enter',timespan = .timespan,label='SE',replace=FALSE)
add.rule(strat.st,name='ruleSignal',arguments=list(sigcol="shortEntry",sigval=TRUE,orderqty='all',ordertype='market',orderside='long',orderset='ocolong',replace=FALSE),type='exit',timespan = .timespan,label = 'LX')
add.rule(strat.st,name='ruleSignal',arguments=list(sigcol="longEntry",sigval=TRUE,orderqty='all',ordertype='market',orderside='short',orderset='ocoshort',replace=FALSE),type='exit',timespan = .timespan,label='SX')
## #------stoploss related rules
add.rule(strat.st,name='ruleSignal',arguments = list(sigcol="longEntry",sigval=TRUE,replace=FALSE,orderside='long',ordertype='stoplimit',tmult=TRUE,threshold=quote(stopLossPercent),orderqty='all',orderset='ocolong'),type='chain',parent="LE",label='StopLossLong',enabled=FALSE)
add.rule(strat.st,name='ruleSignal',arguments = list(sigcol="shortEntry",sigval=TRUE,replace=FALSE,orderside='short',ordertype='stoplimit',tmult=TRUE,threshold=quote(stopLossPercent),orderqty='all',orderset='ocoshort'),type='chain',parent="SE",label='StopLossShort',enabled=FALSE)

###_________________________________add Distributions for STOPLOSS  _________________#######
add.distribution(strat.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossLong',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossLONG'
)

add.distribution(strat.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossShort',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossSHORT'
)

add.distribution.constraint(strat.st,
                            paramset.label = 'StopLoss',
                            distribution.label.1 = 'StopLossLONG',
                            distribution.label.2 = 'StopLossSHORT',
                            operator = '==',
                            label = 'StopLoss'
)


## #------trailing stop related rules
add.rule(strat.st,name = 'ruleSignal',arguments = list(sigcol="longEntry",sigval=TRUE,replace=FALSE,orderside='long',ordertype='stoptrailing',tmult=TRUE,threshold=quote(trailingStopPercent),orderqty='all',orderset='ocolong'),type='chain',parent="LE",label='StopTrailingLong',enabled=FALSE)
add.rule(strat.st,name = 'ruleSignal',arguments = list(sigcol="shortEntry",sigval=TRUE,replace=FALSE,orderside='short',ordertype='stoptrailing',tmult=TRUE,threshold=quote(trailingStopPercent),orderqty='all',orderset='ocoshort'),type='chain',parent="SE",label='StopTrailingShort',enabled=FALSE)
###_________________________________add Distributions for TRAILING STOP  _________________#######
add.distribution(strat.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingLong',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingLONG'
)

add.distribution(strat.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingShort',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingSHORT'
)

add.distribution.constraint(strat.st,
                            paramset.label = 'StopTrailing',
                            distribution.label.1 = 'StopTrailingLONG',
                            distribution.label.2 = 'StopTrailingSHORT',
                            operator = '==',
                            label = 'StopTrailing'
)

###_________________________________add rules for TAKE PROFIT  _________________#######

add.rule(strat.st, name = 'ruleSignal',
         arguments=list(sigcol='longEntry' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', tmult=TRUE, threshold=quote(.TakeProfit),
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='LE',
         label='TakeProfitLONG',
         enabled=FALSE
)

add.rule(strat.st, name = 'ruleSignal',
         arguments=list(sigcol='shortEntry' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='limit', tmult=TRUE, threshold=quote(.TakeProfit),
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='SE',
         label='TakeProfitSHORT',
         enabled=FALSE
)


add.distribution(strat.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitLONG',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitLONG'
)

add.distribution(strat.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitSHORT',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitSHORT'
)

add.distribution.constraint(strat.st,
                            paramset.label = 'TakeProfit',
                            distribution.label.1 = 'TakeProfitLONG',
                            distribution.label.2 = 'TakeProfitSHORT',
                            operator = '==',
                            label = 'TakeProfit'
)

# for (i in symbols) addPosLimit(portf.st,i,timestamp = initDate,maxpos = 2000,minpos = -700)

.smoothing=10:30
add.distribution(strategy = strat.st,paramset.label = 'smoothing',component.type = 'indicator',component.label = 'ar_ind',variable = list(n=.smoothing),label = 'nLOOKBACK')

           
           bee <- add.rule(
             strategy  = strat.st,
             name      = 'ruleSignal',
             arguments = list(sigcol    = 'MA.gt.up',
                              sigval    = TRUE,
                              replace    = F,
                              orderqty  = 100,
                              ordertype = 'market',  
                              # one of "market","limit","stoplimit", "stoptrailing", or "iceberg" 
                              orderside = 'long',
                              osFUN     = 'osMaxPos'), 
             
             type      = 'enter',
             label     = 'EnterLONG')
           
           bee <- add.rule(
             strategy  = strat.st,
             name      = 'ruleSignal',
             arguments = list(sigcol    = 'unlikleyCross',
                              sigval    = TRUE,
                              replace    = F,
                              orderqty  = 500,
                              ordertype = 'market',  
                              # one of "market","limit","stoplimit", "stoptrailing", or "iceberg" 
                              orderside = 'long',
                              osFUN     = 'osMaxPos'), 
             
             type      = 'enter',
             label     = 'EnterLONGBIG')
           
           
           #### exitLong when SMA cross under LowerBand
           bee <- add.rule(
             strategy  = strat.st,
             name      = 'ruleSignal',
             arguments = list(sigcol    = 'MA.lt.dn',
                              sigval    = TRUE,
                              replace    = F,
                              orderqty  = 'all',
                              ordertype = 'market',
                              orderside = 'long'),
             type      = 'exit',
             label     = 'ExitLONG')
           
           #### enterShort when SMA cross under LowerBand
           bee <- add.rule(
             strategy  = strat.st,
             name      = 'ruleSignal',
             arguments = list(sigcol     = 'MA.lt.dn',
                              sigval    = TRUE,
                              replace    = F,
                              orderqty  =  -100,
                              ordertype = 'market',
                              orderside = 'short',
                              osFUN     = 'osMaxPos'), 
             type      = 'enter',
             label     = 'EnterSHORT')
           
           #### exitShort when SMA cross over upperBand
           bee <- add.rule(
             strategy  = strat.st,
             name      = 'ruleSignal',
             arguments = list(sigcol     = 'MA.gt.up',
                              sigval     = TRUE,
                              replace    = F,
                              orderqty   = 'all',
                              ordertype  = 'market',
                              orderside  = 'short'),
             type      = 'exit',
             label     = 'ExitSHORT')
           
           
           
           
           applyStrategy(
             strat.st, 
             portf.st, 
             prefer='Open', # why prefer='Open'
             verbose=T)
           
           updatePortf(
             portf.st) #, 
           
           updateAcct(
             acct.st) # , 
           
           updateEndEq(
             Account = acct.st)#,
           
           
           
           ### User Set up pf parameter ranges to test
           .nFastList = 5:13
           .nSlowList  = 10:40
           .nSdList = 1:3
           
           # number of random samples of the parameter distribution to use for random run
           .nsamples = 10 

           
           add.distribution(strat.st,
                            paramset.label = 'SMA_BBparams',
                            component.type = 'indicator',
                            component.label = 'BBand', #this is the label given to the indicator in the strat
                            variable = list(n = .nSlowList),
                            label = 'BBandMA')
           
           add.distribution(strat.st,
                            paramset.label = 'SMA_BBparams',
                            component.type = 'indicator',
                            component.label = 'BBand', #this is the label given to the indicator in the strat
                            variable = list(sd = .nSdList),
                            label = 'BBandSD'
           )
           
           
           add.distribution(strat.st,
                            paramset.label = 'SMA_BBparams',
                            component.type = 'indicator',
                            component.label = 'MA', #this is the label given to the indicator in the strat
                            variable = list(n = .nFastList),
                            label = 'MAn'
           )
           
           
           
           add.distribution.constraint(strat.st,
                                       paramset.label = 'SMA_BBparams',
                                       distribution.label.1 = 'BBandMA',
                                       distribution.label.2 = 'MAn',
                                       operator = '>',
                                       label = 'BBandMA>MAn'
           )
           
###_______________________ Timespan paramset
           
           add.distribution(strat.st,
                            paramset.label = 'Timespan',
                            component.type = 'enter',
                            component.label = 'LE',
                            variable = list(timespan = .timespans),
                            label = 'EnterLong'
           )
           
           add.distribution(strat.st,
                            paramset.label = 'Timespan',
                            component.type = 'enter',
                            component.label = 'SE',
                            variable = list(timespan = .timespans),
                            label = 'EnterShort'
           )
           
           add.distribution(strat.st,
                            paramset.label = 'Timespan',
                            component.type = 'exit',
                            component.label = 'LX',
                            variable = list(timespan = .timespans),
                            label = 'ExitLong'
           )
           
           add.distribution(strat.st,
                            paramset.label = 'Timespan',
                            component.type = 'exit',
                            component.label = 'SX',
                            variable = list(timespan = .timespans),
                            label = 'ExitShort'
           )
           
           add.distribution.constraint(strat.st,
                                       paramset.label = 'Timespan',
                                       distribution.label.1 = 'EnterLong',
                                       distribution.label.2 = 'EnterShort',
                                       operator = '==',
                                       label = 'EnterTimespan'
           )
           
           add.distribution.constraint(strat.st,
                                       paramset.label = 'Timespan',
                                       distribution.label.1 = 'ExitLong',
                                       distribution.label.2 = 'ExitShort',
                                       operator = '==',
                                       label = 'ExitTimespan'
           )
           
           add.distribution.constraint(strat.st,
                                       paramset.label = 'Timespan',
                                       distribution.label.1 = 'EnterLong',
                                       distribution.label.2 = 'ExitShort',
                                       operator = '==',
                                       label = 'EnterExitTimespan'
           )
           
           
           
           ### parallel computing to speed up
#            
#            if( Sys.info()['sysname'] == "Windows" )
#            {
#              library(doParallel)
#              registerDoParallel(cores=detectCores())
#              # registerDoSEQ()
#            } else {
#              library(doMC)
#              registerDoMC(cores=detectCores())
#            }
#            results <- apply.paramset(strat.st, 
#                                      paramset.label='SMA_BBparams', 
#                                      portfolio.st=portf.st, 
#                                      account.st=acct.st, 
#                                      samples= 0, # take all options
#                                      #.nsamples,  only take 10 samples
#                                      verbose=TRUE)
           
                       
cl<-makeCluster(4,type='SOCK')
registerDoSNOW(cl)
##---------------------SMOOTHING parameter optimziation
           # res<-apply.paramset(strategy.st = strat.st,paramset.label = 'smoothing',portfolio.st = portf.st,account.st = acct.st,samples=0,verbose = FALSE)
##---------------------STOP TRAILING parameter optimziation
#            enable.rule(strat.st,type="chain",label = "StopTrailingLong")
#            enable.rule(strat.st,type="chain",label = "StopTrailingShort")
#            res <- apply.paramset(strat.st, paramset.label='StopTrailing', portfolio.st=portf.st, account.st=acct.st, samples=0, verbose=FALSE)
##--------------------- Original optimization for BBands
#            results <- apply.paramset(strat.st, 
#                                      paramset.label='SMA_BBparams', 
#                                      portfolio.st=portf.st, 
#                                      account.st=acct.st, 
#                                      samples= 0, # take all options
#                                      #.nsamples,  only take 10 samples
#                                      verbose=TRUE)
##---------------------STOP TRAILING parameter optimziation
#            enable.rule(strat.st,type="chain",label = "StopLossLong")
#            enable.rule(strat.st,type="chain",label = "StopLossShort")
#            res <- apply.paramset(strat.st, paramset.label='StopLoss', portfolio.st=portf.st, account.st=acct.st, samples=0, verbose=FALSE)
##---------------------Timespan parameter optimziation           
           res <- apply.paramset(strat.st, paramset.label='Timespan', portfolio.st=portf.st, account.st=acct.st, samples=0, verbose=FALSE)
           
           stopCluster(cl)                      
           
           
           res$tradeStats %>% View()
           # results$tradeStats %>% View()
           