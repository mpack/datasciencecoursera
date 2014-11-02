pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## Capture files names into list
    fileNames<-list.files(directory)
    
    ## For each file in id list read in data and append
    data<-list()
    for(i in id) {
        file<-paste(directory,"\\",fileNames[i],sep="")
        data<-rbind(data,read.csv(file,header=T,sep=","))
    }
    
    ## Capture pollutant subset and calculate mean
    data<-data[,pollutant]
    mean(data,na.rm=T)
}