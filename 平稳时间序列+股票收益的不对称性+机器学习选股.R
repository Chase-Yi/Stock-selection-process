# pour nettoyer l'environnement
rm(list = ls())

# pour changer le repertoire courant
setwd("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")

# LeeStrazicichUnitRootTest.R
# Model = "crash"
ur.ls <- function(y, model = c("crash", "break"), breaks = 1, lags = NULL, method = c("GTOS","Fixed"), pn = 0.1, print.results = c("print", "silent")){
  #Starttime
  starttime <- Sys.time()
  
  #Check sanity of the function call
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  y <- as.vector(y)
  
  if(pn >= 1 || pn <= 0){
    stop("\n pn has to be between 0 and 1.")
  }
  if(method == "Fixed" && is.null(lags) == TRUE){
    stop("\n If fixed lag length should be estimated, the number 
         \n of lags to be included should be defined explicitely.")
  }
  
  #Add lagmatrix function
  lagmatrix <- function(x,max.lag){
    embed(c(rep(NA,max.lag),x),max.lag+1)
  }
  #Add diffmatrix function
  diffmatrix <- function(x,max.diff,max.lag){
    #Add if condition to make it possible to differentiate between matrix and vector                  
    if(is.vector(x) == TRUE ){
      embed(c(rep(NA,max.lag),diff(x,max.lag,max.diff)),max.diff)
    }
    
    else if(is.matrix(x) == TRUE){
      rbind(matrix(rep(NA,max.lag), ncol = ncol(x)), matrix(diff(x,max.lag,max.diff), ncol = ncol(x)))
    }
    #if matrix the result should be 0, if only a vector it should be 1
    else if(as.integer(is.null(ncol(x))) == 0 ){
      rbind(matrix(rep(NA,max.lag), ncol = ncol(x)), matrix(diff(x,max.lag,max.diff), ncol = ncol(x)))
      
    }
  }
  
  #Number of observations
  n <- length(y)
  model <- match.arg(model)
  lags <- as.integer(lags)
  method <- match.arg(method)
  breaks <- as.integer(breaks)
  #Percentage to eliminate endpoints in the lag calculation
  pn <- pn
  #Critical Values for the one break test
  model.one.crash.cval <- matrix(c(-4.239, -3.566, -3.211)
                                 , nrow = 1, ncol = 3, byrow = TRUE)
  
  colnames(model.one.crash.cval) <- c("1%","5%","10%")
  model.one.break.cval <- matrix(c(.1 , -5.11, -4.5, -4.21,
                                   .2 , -5.07, -4.47, -4.20,
                                   .3 , -5.15, -4.45, -4.18,
                                   .4 , -5.05, -4.50, -4.18,
                                   .5 , -5.11, -4.51, -4.17), nrow = 5, ncol = 4, byrow = TRUE)
  colnames(model.one.break.cval) <- c("lambda","1%","5%","10%")
  #   All critical values were derived in samples of size T = 100. Critical values
  #   in Model C (intercept and trend break) depend (somewhat) on the location of the
  #   break (λ = T_B /T) and are symmetric around λ and (1-λ). Model C critical values
  #   at additional break locations can be interpolated.
  #
  #   Critical values for the endogenous two break test  
  #   Model A - "crash" model  
  #   invariant to the location of the crash
  model.two.crash.cval <- matrix(c("LM_tau", -4.545, -3.842, -3.504,
                                   "LM_rho", -35.726, -26.894, -22.892), nrow = 2, ncol = 4, byrow = TRUE ) 
  
  colnames(model.two.crash.cval) <-  c("Statistic","1%","5%","10%")
  
  
  # Model C (i) - "break" model, breaks in the data generating process
  # Model C (i) - "break" model invariant to the location of the crash
  
  model.two.break.dgp.cval <- matrix(c("LM_tau", -5.823, -5.286, -4.989,
                                       "LM_rho", -52.550, -45.531, -41.663), nrow = 2, ncol = 4, byrow = TRUE ) 
  
  
  # Model C (ii) - "break" model, breaks are not considered in the data generating process
  # Model C (ii) - "break" model depends on the location of the crash
  ## highest level of list is the location of the second breakpoint - so the share inside 
  ## the matrix refers to the first breakpoint
  model.two.break.tau.cval <-  matrix(c( -6.16, -5.59, -5.27, -6.41, -5.74, -5.32, -6.33, -5.71, -5.33,
                                         NA    ,   NA,  NA  , -6.45, -5.67, -5.31, -6.42, -5.65, -5.32,
                                         NA    ,   NA,  NA  , NA   , NA   , NA   , -6.32, -5.73, -5.32)
                                      , nrow = 3, ncol = 9, byrow = TRUE )
  
  rownames(model.two.break.tau.cval) <- c("Break 1 - 0.2", "Break 1 - 0.4", "Break 1 - 0.6")
  colnames(model.two.break.tau.cval) <- c("Break 2 - 0.4 - 1%", "Break 2 - 0.4 - 5%", "Break 2 - 0.4 - 10%",
                                          "Break 2 - 0.6 - 1%", "Break 2 - 0.6 - 5%", "Break 2 - 0.6 - 10%",
                                          "Break 2 - 0.8 - 1%", "Break 2 - 0.8 - 5%", "Break 2 - 0.8 - 10%")
  
  
  model.two.break.rho.cval <-  matrix(c( -55.4 , -47.9, -44.0, -58.6, -49.9, -44.4, -57.6, -49.6, -44.6,
                                         NA    ,    NA,  NA  ,-59.3, -49.0, -44.3, -58.8, -48.7, -44.5,
                                         NA    ,    NA,  NA  , NA  , NA   , NA    ,-57.4, -49.8, -44.4)
                                      , nrow = 3, ncol = 9, byrow = TRUE )
  
  
  rownames(model.two.break.rho.cval) <- c("Break 1 - 0.2", "Break 1 - 0.4", "Break 1 - 0.6")
  colnames(model.two.break.rho.cval) <- c("Break 2 - 0.4 - 1%", "Break 2 - 0.4 - 5%", "Break 2 - 0.4 - 10%",
                                          "Break 2 - 0.6 - 1%", "Break 2 - 0.6 - 5%", "Break 2 - 0.6 - 10%",
                                          "Break 2 - 0.8 - 1%", "Break 2 - 0.8 - 5%", "Break 2 - 0.8 - 10%")
  #Number of observations to eliminate in relation to the sample length
  pnnobs <- round(pn*n)
  
  
  #Define the start values
  startl <- 0
  myBreakStart <- startl + 1 + pnnobs
  myBreakEnd <- n - pnnobs
  
  #Calculate Dy
  y.diff <- diffmatrix(y, max.diff = 1, max.lag = 1)
  
  #Calculation
  #trend for 1:n like in ur.sp 
  trend <- 1:n
  
  #Define minimum gap between the two possible break dates.
  #the gap is 2 in the crash case and 3 periods in the case of a break model
  gap <- 2 + as.numeric(model == "break")
  
  
  myBreaks <- matrix(NA, nrow = n - 2 * pnnobs, ncol =  breaks)
  if(breaks == 1){
    myBreaks [,1] <- myBreakStart:myBreakEnd
  } else if (breaks == 2){
    myBreaks[, 1:breaks] <- cbind(myBreakStart:myBreakEnd,(myBreakStart:myBreakEnd)+gap)
  }
  #Define the variables to hold the minimum t-stat
  tstat <- NA
  mint <- 1000
  tstat.matrix <- matrix(NA, nrow = n, ncol = n )
  tstat.result <- matrix()
  #Create lists to store the results
  #Lists for the one break case
  result.reg.coef <- list()
  result.reg.resid <- list()
  result.matrix <- list()
  
  #Function to analyze the optimal lags to remove autocorrelation from the residuals
  #Lag selection with general to specific procedure based on Ng,Perron (1995)
  myLagSelection <- function(y.diff, S.tilde, datmat, pmax, Dummy.diff){
    n <- length(y.diff)
    
    #               General to specific approach to determine the lag which removes autocorrelation from the residuals
    #               Ng, Perron 1995
    qlag <- pmax
    while (qlag >= 0) {
      
      #               Define optimal lags to include to remove autocorrelation from the residuals
      #               select p.value of the highest lag order and check if significant
      #test.coef <- coef(summary(lm(y.diff ~ 0 + lagmatrix(S.tilde,1)[,-1] + datmat[,-1][, 1:(qlag + 1)]  + Dummy.diff)))
      
      #lm.fit implementation
      test.reg.data <- na.omit(cbind(y.diff,lagmatrix(S.tilde,1)[,-1], datmat[,-1][, 1:(qlag + 1)], Dummy.diff))
      
      test.reg.lm. <-(lm.fit(x = test.reg.data[,-1], y = test.reg.data[, 1]))
      
      df.lm.fit <- length(test.reg.data[,1]) - test.reg.lm.$qr$rank
      sigma2 <- sum((test.reg.data[,1] - test.reg.lm.$fitted.values)^2)/df.lm.fit
      varbeta <- sigma2 * chol2inv(qr.R(test.reg.lm.$qr), size = ncol(test.reg.data) -2)
      SE <- sqrt(diag(varbeta))
      tstat <- na.omit(coef(test.reg.lm.))/SE
      pvalue <- 2* pt(abs(tstat), df = df.lm.fit, lower.tail =  FALSE)
      
      #                print(test.coef)
      #                print(paste("lm result:",qlag,test.coef[qlag + 1 , 4]))
      #                print(paste("lm.fit:",qlag,pvalue[qlag+1]))
      #               print(c("Number of qlag",qlag)) 
      if(pvalue[qlag+1] <= 0.1){
        slag <- qlag
        #                  print("break")
        break
      }
      qlag <- qlag - 1
      slag <- qlag
      
    }
    #            print(slag)
    return(slag)
  }
  
  # Function to calculate the test statistic and make the code shorter, because the function can be used in both cases for 
  # the one break as well as the two break case
  myLSfunc <- function(Dt, DTt, y.diff, est.function = c("estimation","results")){
    
    Dt.diff <- diffmatrix(Dt, max.diff = 1, max.lag = 1)
    
    DTt.diff <- diffmatrix(DTt, max.diff = 1, max.lag = 1)
    
    S.tilde <- 0
    S.tilde <- c(0, cumsum(lm.fit(x = na.omit(cbind(Dt.diff[,])), y=na.omit(y.diff))$residuals))
    S.tilde.diff <-  diffmatrix(S.tilde,max.diff = 1, max.lag = 1)
    #       Define optimal lags to include to remove autocorrelation
    #       max lag length pmax to include is based on Schwert (1989) 
    pmax <- min(round((12*(n/100)^0.25)),lags)
    
    #      Create matrix of lagged values of S.tilde.diff from 0 to pmax
    #      and check if there is autocorrelation if all these lags are included and iterate down to 1          
    datmat <- matrix(NA,n, pmax + 2)
    datmat[ , 1] <- S.tilde.diff
    #      Add column of 0 to allow the easy inclusion of no lags into the test estimation
    datmat[ , 2] <- rep(0, n)
    
    if(pmax > 0){
      datmat[, 3:(pmax + 2) ] <- lagmatrix(S.tilde.diff, pmax)[,-1]
      colnames(datmat) <- c("S.tilde.diff", "NoLags",  paste("S.tilde.diff.l",1:pmax, sep = ""))
    } else if(lags == 0){
      colnames(datmat) <- c("S.tilde.diff", "NoLags")
    }
    
    
    if(method == "Fixed"){
      slag <- lags
    } else if(method == "GTOS"){
      
      slag <- NA
      
      if(model == "crash"){
        slag <- myLagSelection(y.diff, S.tilde, datmat, pmax, Dt.diff)
        
      } else if(model == "break"){
        slag <- myLagSelection(y.diff, S.tilde, datmat, pmax, DTt.diff)
      }
    }
    
    S.tilde <- NA
    
    
    
    if(model == "crash"){
      S.tilde <- c(0, cumsum(lm.fit(x = na.omit(cbind(Dt.diff[,])), y=na.omit(y.diff))$residuals))
      S.tilde.diff <-  diffmatrix(S.tilde, max.diff = 1, max.lag = 1)
      
      #Add lag of S.tilde.diff to control for autocorrelation in the residuals
      if(est.function == "results"){
        break.reg <- summary(lm(y.diff ~ 0 + lagmatrix(S.tilde, 1)[,-1] + datmat[,2:(slag+2)]  + Dt.diff))
      } else if (est.function == "estimation"){
        #lm.fit() implementation
        roll.reg.data <- na.omit(cbind(y.diff, lagmatrix(S.tilde,1)[,-1], datmat[,2:(slag+2)], Dt.diff))
        
        roll.reg.lm. <- lm.fit(x = roll.reg.data[,-1], y = roll.reg.data[, 1])
        
        
        df.lm.fit <- length(roll.reg.data[, 1]) - roll.reg.lm.$qr$rank
        sigma2 <- sum((roll.reg.data[, 1] - roll.reg.lm.$fitted.values)^2)/df.lm.fit
        varbeta <- sigma2*chol2inv(qr.R(roll.reg.lm.$qr), size = ncol(roll.reg.data) - 2)
        SE <- sqrt(diag(varbeta))
        tstat.lm.fit <- na.omit(coef(roll.reg.lm.))/SE
        pvalue <- 2 * pt(abs(tstat.lm.fit),df = df.lm.fit, lower.tail =  FALSE)
        coef.roll.reg.lm <- cbind(na.omit(coef(roll.reg.lm.)), SE, tstat.lm.fit, pvalue)
        
        tstat.lm.fit <- tstat.lm.fit[1] 
        
        # tstat.lm <- coef(break.reg)[1,3]
        return(coef.roll.reg.lm)
      }
      
      # print(paste("lm.fit", tstat.lm.fit[1]))
      # print(paste("lm", tstat.lm))
      #print(roll.reg)
      if(est.function == "estimation"){
        return(coef.roll.reg.lm)
      } else if(est.function == "results"){
        return(break.reg)
      }
      
    } else if(model =="break"){
      S.tilde <- c(0, cumsum(lm.fit(x = na.omit(cbind(DTt.diff[,])), y=na.omit(y.diff))$residuals))
      S.tilde.diff <-  diffmatrix(S.tilde, max.diff = 1, max.lag = 1)
      
      
      #Add lag of S.tilde.diff to control for autocorrelation in the residuals
      if(est.function == "results"){
        break.reg <- summary(lm(y.diff ~ 0 + lagmatrix(S.tilde,1)[,-1] + datmat[,2:(slag+2)] + DTt.diff))
      } else if (est.function == "estimation"){
        #lm.fit() implementation
        roll.reg.data <- na.omit(cbind(y.diff, lagmatrix(S.tilde,1)[,-1], datmat[,2:(slag+2)], DTt.diff))
        
        roll.reg.lm. <- lm.fit(x = roll.reg.data[,-1], y = roll.reg.data[, 1])
        
        df.lm.fit <- length(roll.reg.data[, 1]) - roll.reg.lm.$qr$rank
        sigma2 <- sum((roll.reg.data[, 1] - roll.reg.lm.$fitted.values)^2)/df.lm.fit
        varbeta <- sigma2*chol2inv(qr.R(roll.reg.lm.$qr), size = ncol(roll.reg.data) -2)
        SE <- sqrt(diag(varbeta))
        tstat.lm.fit <- na.omit(coef(roll.reg.lm.))/SE
        pvalue <- 2 * pt(abs(tstat.lm.fit),df = df.lm.fit, lower.tail =  FALSE)
        coef.roll.reg.lm <- cbind(na.omit(coef(roll.reg.lm.)), SE, tstat.lm.fit, pvalue)
        
        tstat.lm.fit <- tstat.lm.fit[1] 
        return(coef.roll.reg.lm)
        # tstat.lm <- coef(break.reg)[1,3]
      }
      #Return Value
      #print(roll.reg)
      #print(paste("lm.fit", tstat.lm.fit))
      #print(paste("lm", tstat.lm))
      #print("break")
      if(est.function == "estimation"){
        return(coef.roll.reg.lm)
      } else if(est.function == "results"){
        return(break.reg)
      }
      
    }
    
    #   print(roll.reg)
    if(est.function == "estimation"){
      return(coef.roll.reg.lm)
    } else if(est.function == "results"){
      return(break.reg)
    }
    
  }
  
  
  # Start of the actual function call
  
  if(breaks == 1)
  {
    # Function to calculate the rolling t-stat
    # One Break Case
    for(myBreak1 in myBreaks[,1]){
      #Dummies for one break case
      Dt1 <-  as.matrix(cbind(trend, trend >= (myBreak1 + 1)))
      
      #       Dummy with break in intercept and in trend
      DTt1 <- as.matrix(cbind(Dt1, c(rep(0, myBreak1), 1:(n - myBreak1))))
      colnames(Dt1) <- c("Trend","D")
      colnames(DTt1) <- c("Trend","D","DTt")
      #print(paste("Break1: ",myBreak1, sep = ""))
      
      #Combine all Dummies into one big matrix to make it easier to include in the regressions
      
      Dt <- cbind(Dt1)
      DTt <- cbind(DTt1)
      
      result.reg <- myLSfunc(Dt, DTt, y.diff, est.function = c("estimation"))
      
      
      
      #Extract the t-statistic and if it is smaller than all previous 
      #t-statistics replace it and store the values of all break variables
      #Extract residuals and coefficients and store them in a list
      
      #result.matrix[[myBreak1]] <- result.reg
      #result.reg.coef[[myBreak1]] <- coefficients(result.reg)
      
      
      tstat <- result.reg[1,3]
      tstat.result[myBreak1] <- result.reg[1,3]
      #print(tstat)
      if(tstat < mint){
        mint <- tstat
        mybestbreak1 <- myBreak1
      }
      
      
    }#End of first for loop
    
  } else if(breaks == 2) {
    
    
    ## Two Break Case
    #First for loop for the two break case
    for(myBreak1 in myBreaks[,1]){
      #Dummies for one break case
      Dt1 <-  as.matrix(cbind(trend, trend >= (myBreak1 + 1)))
      
      #       Dummy with break in intercept and in trend
      DTt1 <- as.matrix(cbind(Dt1, c(rep(0, myBreak1), 1:(n - myBreak1))))
      colnames(Dt1) <- c("Trend","D")
      colnames(DTt1) <- c("Trend","D","DTt")
      #print(paste("Break1: ",myBreak1, sep = ""))
      
      #Second for loop for the two break case
      for(myBreak2 in  myBreaks[which(myBreaks[,2] < myBreakEnd & myBreaks[,2] >= myBreak1 + gap),2]){
        
        #Dummies for two break case
        Dt2 <-  as.matrix(trend >= (myBreak2 + 1))
        DTt2 <- as.matrix(cbind(Dt2, c(rep(0, myBreak2), 1:(n - myBreak2))))
        colnames(Dt2) <- c("D2")
        colnames(DTt2) <- c("D2","DTt2")
        #print(paste("Break2: ",myBreak2, sep = ""))
        
        #Combine all Dummies into one big matrix to make it easier to include in the regressions
        
        Dt <- cbind(Dt1, Dt2)
        DTt <- cbind(DTt1, DTt2)
        
        result.reg <- myLSfunc(Dt, DTt, y.diff, est.function = c("estimation"))
        
        #Extract the t-statistic and if it is smaller than all previous 
        #t-statistics replace it and store the values of all break variables
        #Extract residuals and coefficients and store them in a list
        
        
        #matrix to hold all the tstats
        tstat.matrix[myBreak1, myBreak2] <- result.reg[1,3]
        tstat <- result.reg[1,3]
        
        #print(tstat)
        if(tstat < mint){
          mint <- tstat
          mybestbreak1 <- myBreak1
          mybestbreak2 <- myBreak2
        }
        
      }#End of second for loop
    }#End of first for loop
  } else if(breaks > 2){
    
    print("Currently more than two possible structural breaks are not implemented.")
  }
  
  #Estimate regression results, based on the determined breaks and the selected lag 
  # to obtain all necessary information
  Dt1 <-  as.matrix(cbind(trend, trend >= (mybestbreak1 + 1)))
  
  #       Dummy with break in intercept and in trend
  DTt1 <- as.matrix(cbind(Dt1, c(rep(0, mybestbreak1), 1:(n - mybestbreak1))))
  colnames(Dt1) <- c("Trend","D")
  colnames(DTt1) <- c("Trend","D","DTt")
  #print(paste("Break1: ",myBreak1, sep = ""))
  
  if(breaks == 2){
    #Dummies for two break case
    Dt2 <-  as.matrix(trend >= (mybestbreak2 + 1))
    DTt2 <- as.matrix(cbind(Dt2, c(rep(0, mybestbreak2), 1:(n - mybestbreak2))))
    colnames(Dt2) <- c("D2")
    colnames(DTt2) <- c("D2","DTt2")
    #print(paste("Break2: ",myBreak2, sep = ""))
    
    #Combine all Dummies into one big matrix to make it easier to include in the regressions
    
    Dt <- cbind(Dt1, Dt2)
    DTt <- cbind(DTt1, DTt2)
  } else if (breaks == 1){
    Dt <- Dt1
    DTt <- DTt1
    
  }
  
  
  break.reg <- myLSfunc(Dt, DTt, y.diff, est.function = c("results")) 
  
  endtime <- Sys.time()
  myruntime <- difftime(endtime,starttime, units = "mins")
  if(print.results == "print"){
    print(mint)
    sval <<- mint
    print(paste("First possible structural break at position:", mybestbreak1))
    print(paste("The location of the first break - lambda_1:", round(mybestbreak1/n, digits = 1),", with the number of total observations:", n))  
    if(breaks == 2){
      print(paste("Second possible structural break at position:", mybestbreak2))
      print(paste("The location of the second break - lambda_2:", round(mybestbreak2/n, digits = 1),", with the number of total observations:", n))  
      
      
      # Output Critical Values    
      cat("Critical values:\n")
      print(model.two.break.tau.cval)
      
    }else if(breaks == 1){
      if(model == "crash"){
        cat("Critical values - Crash model:\n")
        print(model.one.crash.cval)
        cval <<- model.one.crash.cval[1,2] 
      } else if(model == "break"){
        cat("Critical values - Break model:\n")
        print(model.one.break.cval)
      }
      
    }
    
    if(method == "Fixed"){
      print(paste("Number of lags used:",lags))
    } else if(method == "GTOS"){
      print(paste("Number of lags determined by general-to-specific lag selection:" 
                  ,as.integer(substring(unlist(attr(delete.response(terms(break.reg)), "dataClasses")[3]),9))-1))
      op_lag <<- as.integer(substring(unlist(attr(delete.response(terms(break.reg)), "dataClasses")[3]),9))-1
    } 
    cat("Runtime:\n")
    print(myruntime)
  }
  # Create complete list with all the information and not only print it
  if(breaks == 2){
    results.return <- list(mint, mybestbreak1, mybestbreak2, myruntime)
    names(results.return) <- c("t-stat", "First break", "Second break", "Runtime")
  } else if(breaks == 1){
    results.return <- list(mint, mybestbreak1, myruntime)
    names(results.return) <- c("t-stat", "First break", "Runtime")
  }
  
  return(list(results.return, break.reg))
}#End of ur.ls function

# nouvelles variables globales : 
# sval : valeur de la statistique du test
# cval : valeur critique au seuil de risque à 5% 
# op_lag : lag optimal 

# myLags == op_lag 
# sval < cval

library(xts)
library(forecast) 
library(moments)

######################## Importation des données ###############################

options(max.print=5000)

theUrl <-"C:\\Users\\c4780\\Desktop\\desktop\\Quant\\Action_chinoise_A\\stockA.csv"
# prix de clôture :
stockA <- read.table(file=theUrl, header=TRUE, sep=",")

# is.data.frame(stockA)
# nrow(stockA)
# ncol(stockA)

dates <- stockA[,1]
dates

# dates exactes pour rt : dates_rt
dates_rt = dates[-1]
dates_rt

pt<- stockA[,-1] #将数据框的第一列删除，只留下剩余的列作为数据
pt

code = colnames(pt)# 返回列名
code

n = length(code)

df = as.data.frame(as.numeric(matrix(nrow=length(code),ncol=1))) #创建一个1列的空对象
names(df)[1] <-"Actions"

myBreaks <<- 1
myModel <<- "crash"
myLags <<- 5

############################ Stationnarité #####################################
# 数据如果不平稳的话, 任何机器学习模型都很难预测未来值。

# Le Test de LS est performant en taille et en puissance que si une seule date de rupture est supposée
# Par conséquent, on prend en compte seulement un changement structurel ici

# H0 : Présence de racine unitaire avec 1 changement structurel
# Ha : Absence de racine unitaire avec 1 changement structurel

{

start=Sys.time()

for (i in 1:n) {
    print(i)
    action_nom = code[i]
    
    stock_price = pt[,action_nom]
    # head(stock_price)
    
    # données journalières : 
    # diff() 函数返回连续元素之间的差值
    # lag 参数可以指定计算差值的元素之间的差距。默认为 1
    
    # rentabilité logarithmique :
    # rt = log(pt) − log(pt−1)
    rt <- diff(log(stock_price), lag=1)
    # rt
    
    # Commençons par trouver la valeur de pmax :
    N_rt = length(rt)
    # N_rt
    
    # Test de Lee et Strazicich (LS)
    myLS_test <- ur.ls(y=rt, model = myModel, breaks = myBreaks, lags = myLags, method = "GTOS",pn = 0.1, print.results = "print")
    
    if (is.na(op_lag)==TRUE) {
        op_lag <<- 0
    }
    
    while (myLags != op_lag ) {
        df[i,1] = paste(code[i],": NG")  # NG : Not Good => aucun résultat pour le test de LS
        break;
      }
    
    while (myLags == op_lag ) {
    
       if (sval < cval) {
          df[i,1] = paste(code[i],": OK") # => stationnaire
          break;
       }else{
          df[i,1] = paste(code[i],": NS")  # NS => Non Stationnaire
          break;
       }
    }
}

end=Sys.time()
print(end-start)

}

head(df)

tail(df)

new_df = df[grep(pattern="OK",df[,1]),]

new_df = as.data.frame(new_df)

new_df = strsplit(new_df[,1], ":", fixed= T)

nb = length(new_df)

act = as.data.frame((matrix(nrow=1,ncol=nb))) #创建一个1行的空对象

library(stringr)

for (j in 1:nb) {
   act[1,j]= new_df[[j]][1]
   
   # 删除字符串两边的空格
   act[1,j] = str_trim(string = act[1,j])
}

act 

new_pt = as.data.frame((matrix(nrow=nrow(pt),ncol=nb))) #创建一个空对象

colnames(new_pt)=act[1,]

for (a in 1:nb) {
   #根据列名提取想要的列 并储存到新的对应列中
    new_pt[,a] = pt[,act[1,a]]
}

head(new_pt)

########################## Asymétrie perte/gain ################################
# D'Agostino test of skewness :
# Si la skewness est statistiquement nulle, la distribution est symmétrique ;
# probabilité de gains = probabilité de pertes

# On veut tester si la skewness (le moment centré d’ordre 3) est statistiquement nulle :
#𝐻0 : 𝑠𝑘𝑒𝑤𝑛𝑒𝑠𝑠 = 0 [symétrie]
#𝐻𝑎: 𝑠𝑘𝑒𝑤𝑛𝑒𝑠𝑠 ≠ 0 [asymétrie]

# rt = lrt pour la suite

new_code = colnames(new_pt)

ddf = as.data.frame(as.numeric(matrix(nrow=length(new_code),ncol=1))) #创建一个1列的空对象
names(ddf)[1] <-"Actions"

for (b in 1:nb){
  
  print(b)
  
  action_name = new_code[b]
  
  new_stock_price = new_pt[,action_name]

  # rentabilité logarithmique :
  # lrt = log(pt) − log(pt−1)
  lrt <- diff(log(new_stock_price), lag=1)
  
  # D'Agostino skewness test
  print(agostino.test(lrt)) #par défaut bilatéral

  if (agostino.test(lrt)$p.value < 0.05) {
     ddf[b,1] = paste(new_code[b],": OK") 
     # => Rejeter H0 => La distribution est asymétrique
     
  }else{
     ddf[b,1] = paste(new_code[b],": NG") # => NG : Not Good 
     # => Accepter H0 =>  La distribution est symmétrique  
     
  }
    
} 

ddf

new_ddf = ddf[grep(pattern="OK",ddf[,1]),]

new_ddf = as.data.frame(new_ddf)

new_ddf = strsplit(new_ddf[,1], ":", fixed= T)

nbr = length(new_ddf)

stock_pick = as.data.frame((matrix(nrow=nbr,ncol=1))) #创建一个1列的空对象

library(stringr)

for (c in 1:nbr) {
  stock_pick[c,1]= new_ddf[[c]][1]
  
  # 删除字符串两边的空格
  stock_pick[c,1] = str_trim(string = stock_pick[c,1])
}

names(stock_pick)[1] <- "Actions choisies"

stock_pick

#writes the data frame to a CSV file with the output filename
write.csv(stock_pick,"stock_pick.csv")