corr <- function(directory, threshold=0) {
    ## Capture files names into list
    fileNames<-list.files(directory)
    
    ## For each file in id list read and count nods
    ## if nods greater than threshold, capture values
    ## and append correlation coefficient
    correlation<-vector('numeric')
    rowIndex<-1
    for(i in fileNames) {
        file<-paste(directory,"\\",i,sep="")
        raw<-read.csv(file,header=T,sep=",")        
        numCases<-sum(as.numeric(complete.cases(raw)))
        if (numCases>threshold) {
            sulfate<-raw[,"sulfate"]
            nitrate<-raw[,"nitrate"]
            correlation<-c(correlation,cor(sulfate,nitrate,use="complete"))
        }
        
    }
    correlation
}