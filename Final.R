##The raw data is read into the R object "act"
##The date field is transformed from factor to date format
act<-read.csv("activity.csv",header = T)
act[,2]<-as.Date(act[,2])

##This step is to find the mean and median number of steps taken every day
##A data frame with the sum of number of steps taken for each day is made as follows
summ<-aggregate(act$step~act$date,sum,data = act)
##Ggplot2 system is used to make a histogram of the number of steps taken grouped by day
library(ggplot2)
ggplot(summ,aes(summ[,2])) + geom_histogram(binwidth=1000,fill=NA,color = 'black') +
  theme_bw() + xlab("Number of Steps") + ylab("Number of Days") + labs(title="Histogram of total number of steps taken each day")
##The mean and median values are calculated
mean(summ[,2])
median(summ[,2])

##This step is to find the average daily activity pattern
##A data frame with the average steps taken for each interval averaged over each day is created
library(reshape2)
new<-act[complete.cases(act),c(1,3)]
new$interval<-as.factor(new$interval)
summ1<-acast(melt(new,id="interval"),interval~variable,mean)
summ2<-data.frame(cbind(as.numeric(rownames(summ1)),as.numeric(summ1[,1])))
plot(summ2[,1],summ2[,2],xlim=c(0,2500),type="l",main="Time Series Plot of Average Number of Steps Averaged Across All Days",
     xlab="Time (Number of 5-minute Intervals)",ylab="Average Number of Steps per 5-minute Interval")
##The 5 minute interval which recorded the maximum average number of steps is calculated
subset(summ2,summ2[,2]==max(summ2[,2]),select=X1)

##The missing values are dealt with here
##The total number of rows is calculated here
sum(is.na(act[,1]))
##The missing values are imputed by using the Amelia Package. Please run install.packages("Amelia") if not installed
##The Amelia package as used as it offers many advantages over other methods of missing value imputation.
##It implements a rigorous Expectation maximization Bootstrapping algorithm to provide results quickly.
##The reasoning for not using mean/median imputation as suggested by the Assignment instructions is expressed well in the documentation -
##Amelia II performs multiple imputation, a general-purpose approach to data with missing values. 
##Multiple imputation has been shown to reduce bias and increase efficiency compared to listwise deletion. 
##Furthermore, ad-hoc methods of imputation, such as mean imputation, can lead to serious biases in variances and covariances.
library(Amelia)
imps<-amelia(act,m=3,idvars=2,ords=1)
imp<-imps$imputations[[3]]
##A data frame with the sum of number of steps taken for each day is made as follows
sumimp<-aggregate(imp$step~imp$date,sum,data = imp)
##Ggplot2 system is used to make a histogram of the number of steps taken grouped by day
library(ggplot2)
ggplot(sumimp,aes(sumimp[,2])) + geom_histogram(binwidth=1000,fill=NA,color = 'black') +
  theme_bw() + xlab("Number of Steps") + ylab("Number of Days") + 
  labs(title="Histogram of total number of steps taken each day with Missing Values Imputed")
##The mean and median values are calculated
mean(sumimp[,2])
median(sumimp[,2])
##The difference between the two approaches
((mean(sumimp[,2])-mean(summ[,2]))/mean(summ[,2]))*100
((median(sumimp[,2])-median(summ[,2]))/median(summ[,2]))*100

##The difference between acticvities on weekends and weekdays is analyzed
##A suitable data frame with a factor variable indicating if a day is a weekend or not is added to the dataset with imputed missing values
imp1<-cbind(imp,weekdays(imp$date))
imp1[,4]<-as.character(imp1[,4])

for (i in 1:length(imp1$steps)) {
  if (imp1[i,4] %in% c("Sunday","Saturday")) {
    imp1[i,4]<-"Weekend"
  }
  else {
    imp1[i,4]<-"Weekday"
  }
}

imp1[,4]<-as.factor(imp1[,4])
colnames(imp1)[4]<-"day"
##This step is to find the average daily activity pattern for the imputed data
##A data frame with the average steps taken for each interval averaged over each day is created for imputed data set
library(reshape2)
imp1$interval<-as.factor(imp1$interval)
fin<-tapply(imp1[,1],list(imp1[,4],imp1[,3]),mean)
finm<-melt(fin)
##Ggplot2 system is used to make a panel plot of the average number of steps grouped by 5-minute interval for weekkdays and weekends
##with the imputed data
library(ggplot2)
ggplot(finm,aes(Var2,value))+geom_line()+facet_grid(Var1 ~ .)+xlab("Time (Number of 5-minute Intervals)") + 
  ylab("Average Number of Steps per 5-minute Interval") + 
  labs(title="Panel Plot of Average Number of Steps Averaged Across all Days with Missing Values Imputed")
