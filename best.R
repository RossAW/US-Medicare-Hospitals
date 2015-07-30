## This will find the best hosipital in the state

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    
    
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ##StateIndex<-data$State
    
    ##myordered<-lapply(mysplit,function(x) {x[order(x$outcome),]})
    
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
    BestHospital<-StateDataFrame[[1,2]]
    BestHospital
        
#     StateData<-data[data$State==state,]
#     maxRow<-which.max(StateData[,MortalityOfInterest])
#     
#     BestHospital<-StateData[maxRow,2]
#     BestHospital
    
#     MinStateMortality<-min(MortalityOfInterest)
#     BestHospitalVector<-StateData[StateData[,11]==MinStateMortality,]
#     BestHospital<-BestHospitalVector[,2]
#     BestHospital
    
#     minMortality<-tapply(MortalityOfInterest,StateIndex,min)
#     minMortality
}