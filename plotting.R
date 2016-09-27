
####################################################################################
#1 Load Data
myLoanData <- read.csv("LoansTrainingSet.csv", header=TRUE, stringsAsFactors = FALSE)

#2 Remove obvious duplicates
myLoanData<- myLoanData[!duplicated(myLoanData),]

#3A R Script - Column Names
columnnames<-c("LoanID","CustomerID","LoanStatus","CurrentLoanAmount","Term","CreditScore","YearsInCurrentJob","HomeOwnership","AnnualIncome","Purpose","MonthlyDebt","YearsOfCreditHistory","MonthsSinceLastDelinquent","NumberOfOpenAccounts","NumberOfCreditProblems","CurrentCreditBalance","MaximumOpenCredit","Bankruptcies","TaxLiens")
colnames(myLoanData)<-columnnames

#3B:  
myLoanData$Bankruptcies<-as.integer(myLoanData$Bankruptcies)
myLoanData$TaxLiens<-as.integer(myLoanData$TaxLiens)


#4:  R Script

#require(data.table)

#myLoanData$LoanStatusNum<-ifelse(myLoanData$LoanStatus=="Charged Off",1,0)  #Why?

#Convert String to Numeric
myLoanData$MonthlyDebt<-sub(",","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-sub("\\$","",as.character(myLoanData$MonthlyDebt))
myLoanData$MonthlyDebt<-as.numeric(myLoanData$MonthlyDebt)

#remove outliers
# myLoanData$MonthlyDebt[myLoanData$MonthlyDebt > 2299 | is.na(myLoanData$MonthlyDebt)] <- 2300
# summary(myLoanData$MonthlyDebt)

#Not using data table
myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit == "#VALUE!"] <- NA #2 NAs
myLoanData$MaximumOpenCredit = as.integer(myLoanData$MaximumOpenCredit)
myLoanData <- myLoanData[complete.cases(myLoanData$MaximumOpenCredit),] #####################################NEW!!!

# myLoanData$MaximumOpenCredit[myLoanData$MaximumOpenCredit >74999] <- 75000

myLoanData$AnnualIncome <- as.integer(myLoanData$AnnualIncome)
# myLoanData$AnnualIncome[myLoanData$AnnualIncome > 138999] <- 139000 
# myLoanData$AnnualIncome[myLoanData$AnnualIncome < 10001] <- 10000 

#Using my own credit score code
myLoanData$CreditScore[is.na(myLoanData$CreditScore)] <- 0
myLoanData$CreditScore[myLoanData$CreditScore > 999] <- myLoanData$CreditScore/10
summary(myLoanData$CreditScore)

myLoanData$CurrentCreditBalance <- as.integer(myLoanData$CurrentCreditBalance)
# myLoanData$CurrentCreditBalance[myLoanData$CurrentCreditBalance > 29999] <- 30000 

myLoanData$NumberOfOpenAccounts <- as.integer(myLoanData$NumberOfOpenAccounts)
# myLoanData$NumberOfOpenAccounts[myLoanData$NumberOfOpenAccounts > 21] <- 22

myLoanData$YearsOfCreditHistory <- as.integer(myLoanData$YearsOfCreditHistory)
# myLoanData$YearsOfCreditHistory[myLoanData$YearsOfCreditHistory > 32] <- 33

# myLoanData$CurrentLoanAmount <- as.integer(myLoanData$CurrentLoanAmount)
# myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 0] <- 1 #Do I really want to do this?
# myLoanData$CurrentLoanAmount[myLoanData$CurrentLoanAmount == 99999999] <- 1

summary(myLoanData$CurrentLoanAmount)#Big Difference

myLoanData$YearsInCurrentJob <- as.integer(gsub("([0-9]*).*","\\1",myLoanData$YearsInCurrentJob))
# myLoanData$PaidInFull<-0
# myLoanData$ChargedOff<-0

#DT[,NumberOfLoansClean:=length(LoanID),by="CustomerID"]
#Big differecne.  I deleted the duplicate, this counts them as 2

#5B  Loan Purpose Factors

myLoanData$Purpose <- as.factor(myLoanData$Purpose)
myLoanData$Purpose[myLoanData$Purpose == "other"] <- "Other"

#Add a level
# myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "BuyCarHouse"))
# myLoanData$Purpose[myLoanData$Purpose == "Buy House"] <- "BuyCarHouse"
# myLoanData$Purpose[myLoanData$Purpose == "Buy a Car"] <- "BuyCarHouse"
# 
# myLoanData$Purpose[myLoanData$Purpose == "small_business"] <- "Business Loan"
# 
# myLoanData$Purpose <- factor(myLoanData$Purpose, levels = c(levels(myLoanData$Purpose), "Misc"))
# myLoanData$Purpose[myLoanData$Purpose == "major_purpose"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "Educational Expenses"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "Home Improvements"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "major_purchase"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "moving"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "renewable_energy"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "vacation"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "Take a Trip"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "wedding"] <- "Misc"
# myLoanData$Purpose[myLoanData$Purpose == "Medical Bills"] <- "Misc"

myLoanData$Purpose <- droplevels(myLoanData$Purpose)


#12
myLoanData$DisposableIncome <- myLoanData$AnnualIncome -((myLoanData$MonthlyDebt *12) - ifelse(myLoanData$HomeOwnership == "Own Home", 0, -(myLoanData$AnnualIncome * .15)))

myLoanData$DTI <- (myLoanData$MonthlyDebt * 12)/myLoanData$AnnualIncome#Fixed
myLoanData$AllCreditProbs <- myLoanData$NumberOfCreditProblems + myLoanData$Bankruptcies + myLoanData$TaxLiens
myLoanData$DisposableIncomePct <- myLoanData$DisposableIncome/myLoanData$AnnualIncome
myLoanData$PctCreditUsed <- ifelse(myLoanData$MaximumOpenCredit==0 ,max(100,myLoanData$MaximumOpenCredit), myLoanData$CurrentCreditBalance / myLoanData$MaximumOpenCredit)
myLoanData$CreditHistoryWeight <- myLoanData$CreditScore * myLoanData$YearsOfCreditHistory
myLoanData$LoanToMaxAvailableRatio <- ifelse(myLoanData$MaximumOpenCredit == 0 ,100, myLoanData$CurrentLoanAmount/ myLoanData$MaximumOpenCredit)
myLoanData$TotalCreditProblems <- myLoanData$TaxLiens + myLoanData$Bankruptcies + myLoanData$NumberOfCreditProblems

# write.csv(myLoanData, "tempCSV.csv")

####################################################################################

## Features to plot 
all.cols <- names(myLoanData)
all.cols

num.cols1 <- c("CurrentLoanAmount", "CreditScore", "YearsInCurrentJob", "AnnualIncome", "MonthlyDebt", "YearsOfCreditHistory", 
               "MonthsSinceLastDelinquent", "NumberOfOpenAccounts", "NumberOfCreditProblems", "CurrentCreditBalance", 
               "MaximumOpenCredit")
num.cols2 <- c("Bankruptcies", "TaxLiens", "DisposableIncome", "AllCreditProbs", "DisposableIncomePct", "PctCreditUsed", 
               "CreditHistoryWeight", "LoanToMaxAvailableRatio", "TotalCreditProblems", "DTI")

pairs(~., data=myLoanData[sample(myLoanData, 8000), num.cols1])

#Conditioned histograms
plot.cols1 <- c("CurrentLoanAmount", "CreditScore", "YearsInCurrentJob", "AnnualIncome", "MonthlyDebt", "YearsOfCreditHistory", 
"DisposableIncome")
plot.cols2 <- c("DTI", "DisposableIncomePct", "PctCreditUsed", "CreditHistoryWeight", "LoanToMaxAvailableRatio", "TotalCreditProblems")

## Function to plot conditioned histograms
auto.hist <- function(x) {
    library(ggplot2)
    library(gridExtra)
    ## Compute the bin width
    rg = range(myLoanData[,x])
    bw = (rg[2] - rg[1])/30
    ## Define the title
    title <- paste("Histogram of", x, "conditioned on loan status")
    ## Create the histogram
    ggplot(myLoanData, aes_string(x)) +
      geom_histogram(aes(y = ..count..), binwidth = bw) +
      facet_grid(. ~ LoanStatus) +
      ggtitle(title) 
  }

lapply(num.cols1, auto.hist)

## Function to create conditioned box plots
auto.box <- function(x) {
  title <- paste("Box plot of", x, "by loan status")
  ggplot(myLoanData, aes_string("LoanStatus", x)) +
    geom_boxplot() + ggtitle(title)
}
lapply(num.cols2, auto.box)

## Scatter plot using color to differentiate points
auto.scatter <- function(x){
  require(ggplot2)
  title <- paste("CreditScore vs.", x, "with color by LoanStatus")
  ggplot(myLoanData, aes_string("CreditScore", x)) +
    geom_point(aes(color = factor(LoanStatus))) +
    ggtitle(title)
}
lapply(num.cols1, auto.scatter)

## Conditioned scatter plots 
auto.scatter.cond <- function(x){   
  require(ggplot2)   
  library(gridExtra)   
  title <- paste("price vs.", x, 'with color by credit score and DTI') 
  ggplot(myLoanData, aes_string("CreditScore", x)) + 
    geom_point(aes(color = factor(LoanStatus))) +
    facet_grid(CreditScore ~ DTI) + ggtitle(title)
  }

auto.scatter.cond(num.cols1)

## Bar plot of categorical features 
bar.categorical <- function(x){   
  library(ggplot2)   
  if(!is.numeric(myLoanData[sample(2000),x])){     
    capture.output(       
      plot( ggplot(myLoanData, aes_string(x)) + geom_bar() + facet_grid(. ~ LoanStatus) +
              ggtitle(paste("Counts of Loan Status level by",x))))
  }
}

lapply(all.cols[-c(1:3)], bar.categorical)

## Create Box plot of numeric features  
box.numeric <- function(x){
  library(ggplot2)   
  if(is.numeric(myLoanData[sample(2000),x])){     
    capture.output(
      plot(ggplot(myLoanData, aes_string('LoanStatus', x)) + geom_boxplot() + 
              ggtitle(paste("Counts of Loan Status by",x))))   
     }#The capture.output function is used to capture and suppress voluminous 
      #output from ggplot. 
  } 
lapply(all.cols[-c(1:3)], box.numeric)
