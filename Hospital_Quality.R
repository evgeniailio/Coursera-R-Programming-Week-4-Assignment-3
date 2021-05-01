best<- function(state,outcome) {
     
##Read outcome data  
     
     mydat <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
     
##Check that state and outcome are valid   
     
     if(!any(state == mydat$State)){
          stop("invalid state")}
     else if((outcome %in% c("Heart Attack", "Heart Failure",
                             "Pneumonia")) == FALSE) {
          stop(print("invalid outcome"))
     }
     
##Return hospital name in that state with the lowest 30-day death rate 
     
##z is the subsseted data frame for the given state. y is the column that has the 30-day rate for 
##the given outcome. 'minimum' gives the minimum value of the y column. 'min_rate' gives the name of the Hospital
##for which the outcome is minimum. In the end we sort min_rate in  alphabetical order , in case there is a tie and 
##we eventually get the first one.
     
     if (outcome=="Heart Attack") {

          z<-subset(mydat,mydat$State==state)
          y <- as.numeric(z[,11])
          minimum<-min(y,na.rm=TRUE)
          min_rate<-subset(z[,2],z[,11]==minimum)
          min_rate_sorted<-sort(min_rate)
          print(min_rate_sorted[1])
          
     }
     
     if (outcome=="Heart Failure") {
          z<-subset(mydat,mydat$State==state)
          y <- as.numeric(z[,17])
          minimum<-min(y,na.rm=TRUE)
          min_rate<-subset(z[,2],z[,17]==minimum)
          min_rate_sorted<-sort(min_rate)
          print(min_rate_sorted[1])
          
          
     }
     
     
     if (outcome=="Pneumonia") {
          z<-subset(mydat,mydat$State==state)
          y <- as.numeric(z[,23])
          minimum<-min(y,na.rm=TRUE)
          min_rate<-subset(z[,2],z[,23]==minimum)
          min_rate_sorted<-sort(min_rate)
          print(min_rate_sorted[1])
          
          
     }
     
     
     
}



rankhospital <- function(state,outcome,num) {

##Read outcome data
     
     mydat <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
     
##Check that state and outcome are valid
     
     if(!any(state == mydat$State)){
          stop("invalid state")}
     else if((outcome %in% c("Heart Attack", "Heart Failure",
                             "Pneumonia")) == FALSE) {
          stop(print("invalid outcome"))
     }
     
##Return hospital name in that state with the given rank 30-day death rate
     
##z is the subsseted data frame for the given state. y is the subseted data frame that has  
##the hospital names and  the 30-day rate for 
##the given outcome. y_ordered is the ordered y, according to the hospital names.
     
     z<-subset(mydat,mydat$State==state)
     if (num>length(z$Hospital.Name)) {
          print(NA)
     }
     
     if (outcome=="Heart Attack") {
          z[,11]<-as.numeric(z[,11])
          y<-data.frame(z[,2],z[,11])
          y_ordered<-y[order(y[,2],y[,1]),1]
          print(y_ordered[num])
     }
     
     
     if (outcome=="Heart Failure") {
          z[,17]<-as.numeric(z[,17])
          y<-data.frame(z[,2],z[,17])
          y_ordered<-y[order(y[,2],y[,1]),1]
          print(y_ordered[num])
     }
     
     if (outcome=="Pneumonia") {
          z[,23]<-as.numeric(z[,23])
          y<-data.frame(z[,2],z[,23])
          y_ordered<-y[order(y[,2],y[,1]),1]
          print(y_ordered[num])
     }
     
     
}  


