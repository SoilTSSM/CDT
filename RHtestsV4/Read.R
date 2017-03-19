
### Read data table

Read <- function(idata, MissingValue) {
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
    ooflg <- is.na(ori.itable[, 4]) == F & ((itable[, 2] * 100 + itable[, 3]) != 229)
    # get rid of Feb 29th data
    itable <- itable[!(itable[, 2] == 2 & itable[, 3] == 29), ]
    iyrbegin <- itable[1, 1]
    imdbegin <- itable[1, 2] * 100 + itable[1, 3]
    iyrend <- itable[dim(itable)[1], 1]
    imdend <- itable[dim(itable)[1], 2] * 100 + itable[dim(itable)[1], 3]
    # check input data (both base and ref), no jump with begin and end
    Icy <- sort(unique(ori.itable[ooflg, 2] * 100 + ori.itable[ooflg, 3]))
    Ind2 <- iyrbegin * 10000 + Icy[Icy >= imdbegin]  # first year
    if (iyrend > (iyrbegin + 1)) 
        for (i in (iyrbegin + 1):(iyrend - 1)) Ind2 <- c(Ind2, i * 10000 + Icy)
    Ind2 <- c(Ind2, iyrend * 10000 + Icy[Icy <= imdend])
    Nt <- length(Icy)
    Nall <- dim(itable)[1]
    ind <- ori.itable[ooflg, 1] * 10000 + ori.itable[ooflg, 2] * 100 + ori.itable[ooflg, 3]
    if (sum(!(ind %in% Ind2)) > 0 | all.equal(ind, sort(ind)) != T) {
        InsertMessagesTxt(main.txt.out, "Input data series not continuous", format = TRUE)
        # cat('Input data series not continuous','\n') ErrorMSG<<-paste('Input data series not continuous\n')
        return(-1)
    }
    # for(i in 1:length(Ind2)) if(Ind2[i]!=ind[i]) { ErrorMSG<<-paste('Input data series not continuous at:',Ind2[i],ind[i],'\n') return(-1) }
    # IY0<-ind[is.na(itable[,4])==F]
    IY0 <- ind
    IY0flg <- rep(0, length(IY0))
    Y0 <- itable[is.na(itable[, 4]) == F, 4]
    Iyr <- floor(IY0/10000)
    Imd <- IY0 - Iyr * 10000
    Ti <- IY0
    for (i in 1:length(IY0)) {
        ith <- match(Imd[i], Icy)
        Ti[i] <- (Iyr[i] - iyrbegin) * Nt + ith
    }
    assign("IY0", IY0, env = .GlobalEnv)
    for (i in 1:(length(IY0) - 1)) {
        if (Ti[i + 1] - Ti[i] == 1) 
            IY0flg[i] <- 1
    }
    if (sum(IY0flg) < 10) {
        # too few available data for autocorlh
        InsertMessagesTxt(main.txt.out, "Too many missing values to estimate autocorrelation", format = TRUE)
        # cat('Too many missing values to estimate autocorrelation','\n') ErrorMSG<<-paste('Too many missing values to estimate autocorrelation\n')
        return(-1)
    }
    itable <- itable[is.na(itable[, 4]) == F, ]
    assign("ori.itable", ori.itable, envir = .GlobalEnv)
    assign("ooflg", ooflg, envir = .GlobalEnv)
    assign("itable", itable, envir = .GlobalEnv)
    assign("Ti", Ti, envir = .GlobalEnv)  # Time index for LS fitting
    assign("Y0", Y0, envir = .GlobalEnv)  # Data series for Base
    assign("IY0", IY0, envir = .GlobalEnv)  # Cycle index for Base
    assign("Imd", Imd, envir = .GlobalEnv)  # Cycle index for Base
    assign("IY0flg", IY0flg, envir = .GlobalEnv)  # continuous flag for Base
    assign("Icy", Icy, envir = .GlobalEnv)  # Cycle index
    assign("Nt", Nt, envir = .GlobalEnv)  # Cycle length
    return(0)
}



