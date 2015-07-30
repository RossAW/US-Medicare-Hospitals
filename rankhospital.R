rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    
    if(outcome=="heart attack"){
        MortalityOfInterest<-11
    }else if(outcome=="heart failure"){
        MortalityOfInterest<-17
    }else if(outcome=="pneumonia"){
        MortalityOfInterest<-23
    }else {
        stop("invalid outcome") 
    }
    
    
    StateVector<-unique(data$State)
    StateTest<-state %in% StateVector
    if(StateTest==FALSE){
        stop("invalid state")
    }
    
    
    splitList<-split(data,data$State)
    
    OrderedStateList<-suppressWarnings(lapply(splitList,function(x) {x[order(as.numeric(x[,MortalityOfInterest]),x[,2],na.last = NA),]}))
    
    StateDataFrame<-OrderedStateList[[state]]
    
    NumberOfHospitals<-length(StateDataFrame[,2])
    
    if(num=="best"){
        PositionOfInterest<-1
    }else if(num=="worst"){
        PositionOfInterest<-NumberOfHospitals
    }else if(num>NumberOfHospitals){
        return(NA)
    }else{
        PositionOfInterest<-num
    }
    
    HospitalOfInterest<-StateDataFrame[[PositionOfInterest,2]]
    HospitalOfInterest
    
    
    
    
    
    
    
}