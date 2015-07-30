rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
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
    
    
    
#     StateTest<-state %in% StateVector
#     if(StateTest==FALSE){
#         stop("invalid state")
#     }
    
    
    splitList<-split(data,data$State)
    
    OrderedStateList<-suppressWarnings(lapply(splitList,function(x) {x[order(as.numeric(x[,MortalityOfInterest]),x[,2],na.last = NA),]}))
    
    
    StateVector<-unique(data$State)
    OutputDataFrame<-data.frame()
    numberOfStates<-length(OrderedStateList)
    for(i in 1:numberOfStates){
        
        StateDataFrame<-OrderedStateList[[StateVector[i]]]
        
        NumberOfHospitals<-length(StateDataFrame[,2])
        
        if(num=="best"){
            PositionOfInterest<-1
            OutputDataFrame[i,1]<-StateDataFrame[[PositionOfInterest,2]]
            OutputDataFrame[i,2]<-StateVector[i]
            
        }else if(num=="worst"){
            PositionOfInterest<-NumberOfHospitals
            OutputDataFrame[i,1]<-StateDataFrame[[PositionOfInterest,2]]
            OutputDataFrame[i,2]<-StateVector[i]
            
        }else if(num>NumberOfHospitals){
            OutputDataFrame[i,1]<-NA
            OutputDataFrame[i,2]<-StateVector[i]
            
        }else{
            PositionOfInterest<-num
            OutputDataFrame[i,1]<-StateDataFrame[[PositionOfInterest,2]]
            OutputDataFrame[i,2]<-StateVector[i]
        }
        
    }
    colnames(OutputDataFrame)<-c("hospital","state")
    OutputDataFrame
    
}
    
#     
#     StateDataFrame<-OrderedStateList[[state]]
#     
#     NumberOfHospitals<-length(StateDataFrame[,2])
#     
#     if(num=="best"){
#         PositionOfInterest<-1
#     }else if(num=="worst"){
#         PositionOfInterest<-NumberOfHospitals
#     }else if(num>NumberOfHospitals){
#         return(NA)
#     }else{
#         PositionOfInterest<-num
#     }
#     
#     HospitalOfInterest<-StateDataFrame[[PositionOfInterest,2]]
#     HospitalOfInterest
#     
#     
# }