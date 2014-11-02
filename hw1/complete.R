complete <- function(directory, id = 1:332) {
    ## Capture files names into list
    fileNames<-list.files(directory)
    
    ## For each file in id list read and count nods
    data<-data.frame()
    rowIndex<-1
    for(i in id) {
        file<-paste(directory,"\\",fileNames[i],sep="")
        numCases<-sum(as.numeric(complete.cases(read.csv(file,header=T,sep=","))))
        data[rowIndex,"id"]<-i
        data[rowIndex,"nobs"]<-numCases
        rowIndex<-rowIndex+1
    }
    data
}