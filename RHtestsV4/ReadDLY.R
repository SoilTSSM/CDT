
ReadDLY <- function(idata, MissingValue, pthr) {
    # if(!file.exists(idata)) { ErrorMSG<<-paste('Input datafile',idata,'does not exist!\n') return(-1) } if(is.csv(idata)){
    # itmp<-try(read.table(idata,sep=',',header=F,na.strings=MissingValue, colClasses=rep('real',4)),silent=T) if(inherits(itmp,'try-error')){
    # ErrorMSG<<-geterrmessage() return(-1) } else itable<-itmp } else{ itmp<-try(read.table(idata,sep='',header=F,na.strings=MissingValue,
    # colClasses=rep('real',4)),silent=T) if(inherits(itmp,'try-error')){ ErrorMSG<<-geterrmessage() return(-1) } else itable<-itmp } if(ncol(itable)!=4){
    # ErrorMSG<<-paste(idata,'has',ncol(itable),'columns. The number of columns should be 4\n') return(-1) } colnames(itable)<-c('id1','id2','id3','data')
    
    
    itable <- as.data.frame(idata)
    # itable[which(itable[,4]==MissingValue),4]<-NA
    names(itable) <- c("id1", "id2", "id3", "data")
    
    # keep input base data as ori.itable
    ori.itable <- itable
    Nall <- dim(itable)[1]
    ind <- c(1:Nall)
    otable <- cbind(ind, itable)[is.na(itable[, 4]) == F & itable[, 4] > pthr, ]
    Nday <- otable[, 1]
    IY0 <- otable[, 2] * 10000 + otable[, 3] * 100 + otable[, 4]
    IY0flg <- rep(0, length(IY0))
    Y0 <- otable[, 5]
    Iyr <- otable[, 2]
    Imd <- otable[, 3] * 100 + otable[, 4]
    Ti <- Nday/365.25
    for (i in 1:(length(IY0) - 1)) {
        if (Nday[i + 1] - Nday[i] == 1) 
            IY0flg[i] <- 1
    }
    if (sum(IY0flg) < 10) {
        # too few available data for autocorlh
        InsertMessagesTxt(main.txt.out, "Too many missing values to estimate autocorrelation", format = TRUE)
        # cat('Too many missing values to estimate autocorrelation','\n') ErrorMSG<<-paste('Too many missing values to estimate autocorrelation\n')
        return(-1)
    }
    itable <- otable
    assign("ori.itable", ori.itable, envir = .GlobalEnv)
    assign("itable", itable, envir = .GlobalEnv)
    assign("Ti", Ti, envir = .GlobalEnv)  # Time index for LS fitting
    assign("Y0", Y0, envir = .GlobalEnv)  # Data series for Base
    assign("IY0", IY0, envir = .GlobalEnv)  # Cycle index for Base
    assign("Imd", Imd, envir = .GlobalEnv)  # Cycle index for Base
    assign("IY0flg", IY0flg, envir = .GlobalEnv)  # continuous flag for Base
    return(0)
}


