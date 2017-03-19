
FindU.dlyPrcp <- function(InSeries, output, station, MissingValueCode = -99, pthr = 0, p.lev = 0.95, Iadj = 10000, Mq = 10, Ny4a = 0, GUI = FALSE) {
    
    ####################################################### 
    Station <- paste("BaseSeries_", station, sep = "")
    
    # ErrorMSG<-NA assign('ErrorMSG',ErrorMSG,envir=.GlobalEnv)
    Nmin <- 10
    Ncat.min <- 20
    if (!p.lev %in% c(0.75, 0.8, 0.9, 0.95, 0.99, 0.9999)) {
        InsertMessagesTxt(main.txt.out, paste("FindU.dlyPrcp: input p.lev", p.lev, "error"), format = TRUE)
        # cat(paste('FindU.dlyPrcp: input p.lev',p.lev,'error'),'\n') ErrorMSG<<-paste('FindU.dlyPrcp: input p.lev',p.lev,'error\n',
        # get('ErrorMSG',env=.GlobalEnv),'\n') if(!GUI) cat(ErrorMSG)
        return(-1)
    }
    plev <- p.lev
    pkth <- match(p.lev, c(0.75, 0.8, 0.9, 0.95, 0.99, 0.9999))
    assign("Nmin", Nmin, envir = .GlobalEnv)
    itmp <- ReadDLY(InSeries, MissingValueCode, pthr)
    if (itmp == (-1)) {
        InsertMessagesTxt(main.txt.out, paste("FindU.dlyPrcp: Error in read data from", station), format = TRUE)
        # cat(paste('FindU.dlyPrcp: Error in read data from',Station),'\n') ErrorMSG<<-paste('FindU.dlyPrcp: Error in read data from',Station,'\n',
        # get('ErrorMSG',env=.GlobalEnv),'\n') if(!GUI) cat(ErrorMSG)
        return(-1)
    }
    
    if (Ny4a > 0 & Ny4a <= 5) 
        Ny4a <- 5
    Nall <- dim(ori.itable)[1]
    N <- length(Y0)
    Pfreq <- N/Nall
    Nadj <- Ny4a * 366 * Pfreq
    readPFtable(N, pkth)
    # smallestP<-min(Y0)
    smallestP <- min(ori.itable[ori.itable[, "data"] > 0, "data"], na.rm = T)
    smallestP <- max(c(smallestP, pthr))
    sumLogPi <- sum(log(Y0))
    
    ########################################################## define output file
    
    ofileSout <- paste(output, "_Ustat.txt", sep = "")
    file.create(ofileSout)
    
    #################################################### write '_Ustat.txt'
    
    cat(paste("Input data filename:", Station, "; N=", N, "; Nday=", Nall, "; pthr=", pthr, "\n"), file = ofileSout)
    cat(paste("The nominal level of confidence (1-alpha)=", plev, "\n"), file = ofileSout, append = T)
    cat(paste("The smallest non-zero dailyP is:", smallestP, "\n"), file = ofileSout, append = T)
    
    P <- Y0
    aa <- (-1)
    bb <- 1
    lambdas <- seq(-1, 1, 0.01)
    LHs <- rep(NA, length(lambdas))
    Nss <- rep(NA, length(lambdas))
    Ipss <- matrix(NA, length(lambdas), 100)
    Niter0 <- 0
    LH.max <- (-9999999)
    lr.max <- (-1)
    for (dr in c(1, 0.5, 0.2, 0.1)) {
        lrs <- seq(aa, bb, by = dr)
        for (lr in 1:length(lrs)) {
            lambda <- aa + (lr - 1) * dr
            ind <- round((lambda + 1) * 100 + 1)
            if (abs(lambdas[ind] - lambda) > 1e-08) {
                print(c(lambdas[ind], lambda, lambdas[ind] - lambda))
                InsertMessagesTxt(main.txt.out, paste("ind=", ind, "lambda=", lambda, "error"), format = TRUE)
                return(-1)
                # stop(paste('ind=',ind,'lambda=',lambda,'error'))
            }
            if (is.na(LHs[ind]) == F) {
                # already done before
                LH <- LHs[ind]
                Ns <- Nss[ind]
                Ips <- Ipss[ind, 1:(Ns + 1)]
                Y1 <- BCtrans(P, lambda)
            } else {
                Niter0 <- Niter0 + 1
                Y1 <- BCtrans(P, lambda)
                
                Ip0 <- N
                Pk0 <- Pk.PMFT(N)
                
                oout <- PMFT(Y1, Ti, Pk0)
                I0 <- 0
                I2 <- oout$KPx
                I4 <- N
                I1 <- PMFxKxI0I2(Y1, Ti, I0, I2)$Ic
                I3 <- PMFxKxI0I2(Y1, Ti, I2, I4)$Ic
                I2 <- PMFxKxI0I2(Y1, Ti, I1, I3)$Ic
                
                Ns <- 1
                Ips <- c(I1, N)
                if (I1 > 0) {
                  otmp <- LSmultiple(Y1, Ti, Ips)
                  resi <- otmp$resi
                  fitted <- otmp$fitted
                  otmp <- Rphi(resi, Ips, Ns)
                  cor1 <- otmp$cor
                  corL1 <- otmp$corl
                  corU1 <- otmp$corh
                  W <- otmp$W + fitted
                  otmp <- PMFxKc(Y1, Ti, I0, I4, I1)
                  PFx1 <- otmp$PFc
                  otmp <- PMFxKc(W, Ti, I0, I4, I1)
                  prob1 <- otmp$prob
                } else {
                  prob1 <- 0
                  PFx1 <- 0
                  cor1 <- 0
                  corL1 <- 0
                  corU1 <- 0
                }
                
                Ips <- c(I2, N)
                if (I2 > 0) {
                  otmp <- LSmultiple(Y1, Ti, Ips)
                  resi <- otmp$resi
                  fitted <- otmp$fitted
                  otmp <- Rphi(resi, Ips, Ns)
                  cor2 <- otmp$cor
                  corL2 <- otmp$corl
                  corU2 <- otmp$corh
                  W <- otmp$W + fitted
                  otmp <- PMFxKc(Y1, Ti, I0, I4, I2)
                  PFx2 <- otmp$PFc
                  otmp <- PMFxKc(W, Ti, I0, I4, I2)
                  prob2 <- otmp$prob
                } else {
                  prob2 <- 0
                  PFx2 <- 0
                  cor2 <- 0
                  corL2 <- 0
                  corU2 <- 0
                }
                
                Ips <- c(I3, N)
                if (I3 > 0) {
                  otmp <- LSmultiple(Y1, Ti, Ips)
                  resi <- otmp$resi
                  fitted <- otmp$fitted
                  otmp <- Rphi(resi, Ips, Ns)
                  cor3 <- otmp$cor
                  corL3 <- otmp$corl
                  corU3 <- otmp$corh
                  W <- otmp$W + fitted
                  otmp <- PMFxKc(Y1, Ti, I0, I4, I3)
                  PFx3 <- otmp$PFc
                  otmp <- PMFxKc(W, Ti, I0, I4, I3)
                  prob3 <- otmp$prob
                } else {
                  prob3 <- 0
                  PFx3 <- 0
                  cor3 <- 0
                  corL3 <- 0
                  corU3 <- 0
                }
                
                tmp <- sort(c(PFx1, PFx2, PFx3), decreasing = T, index.return = T)
                PFx.mx <- tmp$x[1]
                prob.mx <- c(prob1, prob2, prob3)[tmp$ix[1]]
                Imx <- c(I1, I2, I3)[tmp$ix[1]]
                cor.mx <- c(corL1, corL2, corL3)[tmp$ix[1]]
                PFx95L <- getPFx95(cor.mx, N)
                if (PFx.mx < PFx95L) {
                  Ns <- 0
                  Ips <- N
                } else {
                  Ns <- 1
                  Ips <- c(Imx, N)
                  
                  tt <- TRUE
                  Niter <- 0
                  while (tt) {
                    # condition on is there any more Bps to insert in Ips?
                    Niter <- Niter + 1
                    tt <- FALSE
                    Ips0 <- NULL
                    for (i in 1:(Ns + 1)) {
                      # search for new break points
                      I0 <- if (i == 1) 
                        0 else Ips[i - 1]
                      I2 <- Ips[i]
                      otmp <- PMFxKxI0I2(Y1, Ti, I0, I2)
                      if (otmp$prob > 0) 
                        Ips0 <- sort(c(Ips0, otmp$Ic))
                    }
                    # finished search for possible new changepoints, start estimate the p-value of the Ips0 series and find the most significant changepoint Iseg.mx
                    tt1 <- TRUE
                    while (tt1) {
                      PFx.mx <- (-9999)
                      Iseg.mx <- 0
                      PFx95L.mx <- 0
                      if (length(Ips0) == 0) 
                        tt1 <- FALSE else {
                        for (i in 1:length(Ips0)) {
                          Ips1 <- sort(c(Ips, Ips0[i]))
                          ith <- match(Ips0[i], Ips1)
                          otmp <- PMFxIseg(Y1, Ti, Ips1, ith)
                          if (otmp$PFx < otmp$PFx95L) 
                            Ips0[i] <- 0 else if (otmp$PFx > PFx.mx) {
                            PFx.mx <- otmp$PFx
                            Iseg.mx <- Ips0[i]
                            PFx95L.mx <- otmp$PFx95L
                          }
                        }
                        if (PFx.mx >= PFx95L.mx) {
                          # Ic is significant
                          Ips <- sort(c(Ips, Iseg.mx))
                          Ns <- Ns + 1
                          Ips0 <- Ips0[Ips0 != Iseg.mx]
                          tt <- TRUE
                        } else tt1 <- FALSE
                        Ips0 <- Ips0[Ips0 != 0]
                      }
                    }
                  }
                  # finish finding any possible new changepoints
                  
                  # start to delete the least significant changepoint if it is insignificant
                  tt <- TRUE
                  while (tt) {
                    tt <- FALSE
                    Iseg.mn <- 0
                    PFx.mn <- 99999
                    PFx95L.mn <- 99999
                    for (i in 1:Ns) {
                      otmp <- PMFxIseg(Y1, Ti, Ips, i)
                      if (otmp$PFx < PFx.mn) {
                        Iseg.mn <- i
                        PFx.mn <- otmp$PFx
                        PFx95L.mn <- otmp$PFx95L
                      }
                    }
                    if (Iseg.mn > 0 & PFx.mn < PFx95L.mn) {
                      Ips <- Ips[-Iseg.mn]
                      Ns <- Ns - 1
                      if (Ns > 0) 
                        tt <- TRUE
                    }
                  }
                }
                otmp <- LSmultipleRed(Y1, Ti, Ips)
                SSEf <- sum(otmp$resi^2)
                LH <- -log(SSEf/(N - 2 - Ns)) * N/2 + (lambda - 1) * sumLogPi
                # ind<-floor((lambda+1)*100+1) if(lambdas[ind]!=lambda) stop(paste('ind=',ind,'lambda=',lambda,'error'))
                # print(paste('##1##',round(lambda,2),Ns,Niter0,Ns,Ips[1],round(LH,2)))
                LHs[ind] <- LH
                Nss[ind] <- Ns
                if (Ns > 0) 
                  Ipss[ind, 1:(Ns + 1)] <- Ips
            }
            if (LH > LH.max) {
                LH.max <- LH
                Ips.max <- Ips
                Ns.max <- Ns
                lambda.max <- lambda
            }
        }
        # end of detection
        if (dr > 0.1) {
            aa <- max(c(lambda.max - dr, -1))
            bb <- min(c(lambda.max + dr, 1))
            lrs <- seq(aa, bb, by = dr)
        }
    }
    
    # final output
    lambda <- lambda.max
    Ns <- Ns.max
    Ips <- Ips.max
    
    #################################################### write '_Ustat.txt'
    
    cat(paste("Best lambda=", round(lambda, 2), "; Ns=", Ns, "; likelihood=", round(LH.max, 2), "\n\n"), file = ofileSout, append = T)
    Y1 <- BCtrans(Y0, lambda)
    
    if (Ns > 0) {
        Nsegs <- Ips - c(0, Ips[1:Ns])
        Iseg.longest <- sort(Nsegs, index = T, decreasing = T)$ix[1]
    } else Iseg.longest <- 0
    
    if (Iadj > (Ns + 1) | Iseg.longest == 0) 
        Iseg.adj <- Ns + 1 else if (Iadj == 0) 
        Iseg.adj <- Iseg.longest else Iseg.adj <- Iadj
    
    Ipd <- c(N)
    dtmp <- LSmultipleRed(Y1, Ti, Ipd)
    beta0 <- dtmp$trend
    # cat(paste('Ignore changepoints -> trend0 =',round(beta0,6), '(p=', round(dtmp$p.tr,4), '); cor=', round(dtmp$cor,4),
    # '(',round(dtmp$corl,4),',',round(dtmp$corh,4),')\n\n'), file=ofileSout,append=TRUE)
    
    otmp <- LSmultipleRed(Y1, Ti, Ips)
    cor <- otmp$cor
    corL <- otmp$corl
    corU <- otmp$corh
    W <- otmp$W
    WL <- otmp$WL
    WU <- otmp$WU
    Rresi <- otmp$resi
    pcor <- pt(abs(cor) * sqrt((N - 2)/(1 - cor^2)), N - 2)
    meanhat <- otmp$meanhat
    Pmeanhat <- IVBCtrans(meanhat, lambda)
    meanhatA <- rep(NA, N)
    
    # cat('Common trend TPR fit to the transformed Base series:\n', file=ofileSout,append=T) cat(paste('#steps= ',Ns,'; trend=',round(otmp$trend,6),'unit/yr (p=',
    # round(otmp$p.tr,4),'); lambda=',round(lambda,2),'; cor=', round(cor,4),'(',round(corL,4),',',round(corU,4),') p=', round(pcor,4),'\n'),
    # file=ofileSout,append=T)
    
    
    #################################################### write '_Ustat.txt'
    
    if (Ns > 0) {
        cat(paste("Step-sizes of the transfromed/original series:\n"), file = ofileSout, append = T)
        C <- rep(NA, Ns)
        sumC <- 0
        E <- rep(0, Ns + 1)
        for (i in 1:Ns) {
            I1 <- if (i == 1) 
                1 else Ips[i - 1] + 1
            I2 <- Ips[i + 1]
            Ic <- Ips[i]
            meanhat0 <- otmp$mu[i] + otmp$trend * Ti[I1:I2]
            Y <- otmp$mu[i + 1] + otmp$trend * Ti[I1:I2]
            R0 <- IVBCtrans(meanhat0, lambda)
            R <- IVBCtrans(Y, lambda)
            C[i] <- R[Ic - I1 + 1] - R0[Ic - I1 + 1]
            sumC <- sumC + C[i]
            E[i + 1] <- sumC
            stepsize <- otmp$mu[i + 1] - otmp$mu[i]
            cat(paste(Ips[i], IY0[Ips[i]], "transfered stepsize=", round(stepsize, 4), "original stepsize=", round(C[i], 4), "\n"), file = ofileSout, append = T)
        }
        
        # calculate IBC adjustment
        Y <- rep(NA, N)
        for (i in 1:(Ns + 1)) {
            I1 <- if (i == 1) 
                1 else Ips[i - 1] + 1
            I2 <- Ips[i]
            Delta <- otmp$mu[Iseg.adj] - otmp$mu[i]
            Y[I1:I2] <- Y1[I1:I2] + Delta
            meanhatA[I1:I2] <- meanhat[I1:I2] + Delta
        }
        # PmeanhatA<-IVBCtrans(meanhatA,lambda)
        PmeanhatA <- rep(NA, N)
        dPmu <- PmeanhatA[1] - Pmeanhat[1]
        PdA <- rep(NA, N)
        for (i in 1:(Ns + 1)) {
            I1 <- if (i == 1) 
                1 else Ips[i - 1] + 1
            I2 <- Ips[i]
            if (i == Iseg.adj) {
                PdA[I1:I2] <- P[I1:I2]
                DeltaP <- 0
                PmeanhatA[I1:I2] <- Pmeanhat[I1:I2]
            } else {
                DeltaP <- E[Iseg.adj] - E[i]
                DeltaP0 <- DeltaP
                PdA[I1:I2] <- P[I1:I2] + DeltaP
                PmeanhatA[I1:I2] <- Pmeanhat[I1:I2] + DeltaP
                PdA[I1:I2][PdA[I1:I2] < smallestP] <- smallestP
                Diff <- mean(PdA[I1:I2] - P[I1:I2])
                Delta <- DeltaP - Diff
                tflg <- (Delta <= (-0.01))
                Niter <- 1
                while (tflg) {
                  Niter <- Niter + 1
                  Delta0 <- Delta
                  PdA[I1:I2] <- PdA[I1:I2] + Delta
                  PmeanhatA[I1:I2] <- PmeanhatA[I1:I2] + Delta
                  PdA[I1:I2][PdA[I1:I2] < smallestP] <- smallestP
                  Diff <- mean(PdA[I1:I2] - P[I1:I2])
                  Delta <- DeltaP - Diff
                  tflg <- (Delta <= (-0.01) & abs(Delta) < abs(Delta0) & Niter < 5)
                }
            }
        }
        # PdA[PdA<smallestP]<-smallestP
        Ptr.mx <- max(PmeanhatA)
        
        Pdtr <- P + Ptr.mx - PmeanhatA
        
        QMout <- adjDLYp(Pdtr, Ips, Mq, Iseg.adj, Ptr.mx, PmeanhatA, Ncat.min, Nadj)
        PA <- QMout$PA
        # PA[PA<smallestP]<-max(c(smallestP,pthr))
        PA[PA < smallestP] <- smallestP
        
        #################################################### write '_Ustat.txt'
        
        cat(paste("Nseg_shortest =", QMout$Nseg.mn, "; Mq = ", QMout$Mq, "\n"), file = ofileSout, append = T)
        cat(paste("\n Adjust to segment", Iseg.adj, ": from", if (Iseg.adj == 1) 
            1 else Ips[Iseg.adj - 1] + 1, "to", Ips[Iseg.adj], "\n"), file = ofileSout, append = T)
        
        cat("#Fcat, DP (CDF and Differnces in category mean)\n", file = ofileSout, append = T)
        write.table(round(QMout$osmean, 4), file = ofileSout, append = T, row.names = F, col.names = F)
        for (i in 1:(Ns + 1)) {
            I1 <- if (i == 1) 
                1 else Ips[i - 1] + 1
            I2 <- Ips[i]
            cat(paste("Seg. ", i, ": mean of QM-adjustments =", round(mean(QMout$W[I1:I2]), 4), "\n", sep = ""), file = ofileSout, append = T)
        }
        
        odata <- cbind(1:nrow(ori.itable), ori.itable[, 1] * 10000 + ori.itable[, 2] * 100 + ori.itable[, 3], ori.itable[, 4], NA, NA, NA, NA, NA, NA, NA, NA, NA, 
            NA)
        Imd <- itable[, 1]
        odata[Imd, 4] <- round(Pmeanhat, 4)
        odata[Imd, 5] <- round(PA, 4)
        odata[Imd, 6] <- round(PdA, 4)
        odata[Imd, 7] <- round(PmeanhatA, 4)
        odata[Imd, 8] <- round(Y1, 4)
        odata[Imd, 9] <- round(meanhat, 4)
        odata[Imd, 10] <- round(Y, 4)
        odata[Imd, 11] <- round(meanhatA, 4)
        odata[Imd, 12] <- round(PA - itable[, 5], 4)
        odata[Imd, 13] <- round(PdA - itable[, 5], 4)
        # odata[Imd,5]<-round(PA,4) odata[Imd,6]<-round(PdA,4) odata[Imd,7]<-round(PA-ori.itable[Imd,4],4) odata[Imd,8]<-round(PdA-ori.itable[Imd,4],4)
        odataP <- cbind(itable[, 1], IY0, itable[, 5], Pmeanhat, PA, PdA, PmeanhatA, Y1, meanhat, Y, meanhatA, PA - itable[, 5], PdA - itable[, 5])
        otrend.ori <- getMtrendFdly(itable[, 2:5])
        otrend.QM <- getMtrendFdly(cbind(itable[, 2:4], PA))
        otrend.IBC <- getMtrendFdly(cbind(itable[, 2:4], PdA))
        
        
        #################################################### write '_Ustat.txt'
        
        cat(paste("Linear trend in the original monthly total P is trend0=", round(otrend.ori, 5), "mm/month\n"), file = ofileSout, append = T)
        cat(paste("Linear trend in QMadjusted monthly total P is trendQM=", round(otrend.QM, 5), "mm/month\n"), file = ofileSout, append = T)
        cat(paste("Linear trend in mean adjusted(IBC) monthly total P is trend(IBC)=", round(otrend.IBC, 5), "mm/month\n\n"), file = ofileSout, append = T)
    } else {
        # odata<-cbind(ori.itable[,c(1,2,3,4,4,4)],0,0)
        odataP <- cbind(itable[, 1], IY0, itable[, 5], Pmeanhat, itable[, 5], itable[, 5], Pmeanhat, Y1, meanhat, Y1, meanhat, 0, 0)
        odata <- cbind(1:nrow(ori.itable), ori.itable[, 1] * 10000 + ori.itable[, 2] * 100 + ori.itable[, 3], ori.itable[, 4], ori.itable[, 4], 0, 0, 0, 0, 0, 0, 
            0, 0)
        otrend.ori <- getMtrendFdly(itable[, 2:5])
        cat(paste("Linear trend in the original monthly total P is trend0=", round(otrend.ori, 5), "mm/month\n"), file = ofileSout, append = T)
    }
    
    
    #################################################### write data *_U.dat
    
    ofilePout <- paste(output, "_U.dat", sep = "")
    # write.table(round(odataP,4),file=ofilePout,na=MissingValueCode,col.names=F,row.names=F)
    write.table(odata, file = ofilePout, na = as.character(MissingValueCode), col.names = F, row.names = F)
    
    
    
    ##################################################################################### PLOT
    
    ofilePdf <- paste(output, "_U.pdf", sep = "")
    
    
    pdf(file = ofilePdf, onefile = T)
    op <- par(no.readonly = TRUE)  # the whole list of settable par's.
    par(mfrow = c(2, 1))
    par(mar = c(2, 4, 3, 1) + 0.1)
    par(cex = 0.8, cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    if (Ns > 0) {
        # par(mar=c(3,4,3,2)+.1)
        
        p1data <- cbind(c(1:Nall), ori.itable[, 4], NA)
        p1data[Imd, 2] <- P
        p1data[Imd, 3] <- odataP[, 4]
        
        for (Iseg in 1:Ns) {
            # plot P~Pmeanhat
            I1 <- max(c(1, (itable[Ips[Iseg], 1] - 180)))
            Ic <- itable[Ips[Iseg], 1]
            I2 <- min(c(Nall, (itable[Ips[Iseg], 1] + 180)))
            plot(I1:I2, ori.itable[I1:I2, 4], type = "l", col = "black", ylab = "prcp(mm)", main = paste("Original dailyP>pthr series\n", "Changepoint ", Iseg, " at:", 
                IY0[Ips[Iseg]], sep = ""), xaxt = "n")
            ats <- c(I1, Ic, I2)
            labels <- ori.itable[ats, 1] * 10000 + ori.itable[ats, 2] * 100 + ori.itable[ats, 3]
            axis(side = 1, at = c(I1, Ic, I2), labels = labels)
            
            tflg <- is.na(p1data[I1:I2, 3]) == F
            lines(c(I1:I2)[tflg], p1data[I1:I2, 3][tflg], col = "red", lwd = 1.5)
        }
        for (Iseg in 1:Ns) {
            # plot Y1~meanhat
            I1 <- max(c(1, Ips[Iseg] - 180))
            Ic <- Ips[Iseg]
            I2 <- min(c(N, Ips[Iseg] + 180))
            plot(I1:I2, Y1[I1:I2], type = "l", col = "black", ylab = "", xaxt = "n", main = paste("Box-Cox transformed dailyP>pthr series\n", "Changepoint ", Iseg, 
                " at:", IY0[Ips[Iseg]], sep = ""))
            axis(side = 1, at = c(I1, Ic, I2), labels = IY0[c(I1, Ic, I2)])
            lines(c(I1:I2), meanhat[I1:I2], col = "red", lwd = 1.5)
        }
    }
    
    yrs <- unique(IY0/10000)
    yrs <- as.integer(seq(yrs[1], yrs[length(yrs)], length = 8))
    ymd <- yrs * 10000 + 101
    ats <- rep(NA, length(yrs))
    for (i in 1:length(yrs)) {
        it <- match(ymd[i], IY0)
        if (!is.na(it)) 
            ats[i] <- it else ats[i] <- min(c(1:N)[IY0 >= ymd[i]])
    }
    plot(c(1:N), P, type = "l", col = "black", ylab = "prcp(mm)", main = "Original dailyP>pthr series", xaxt = "n")
    axis(side = 1, at = ats, labels = yrs)
    lines(c(1:N), odataP[, 4], col = "red")
    
    if (Ns > 0) {
        plot(c(1:N), PA, type = "l", col = "black", ylab = "prcp(mm)", main = "QM adjusted dailyP>pthr series", xaxt = "n")
        axis(side = 1, at = ats, labels = yrs)
        plot(c(1:N), PdA, type = "l", col = "black", ylab = "prcp(mm)", main = "IBC adjusted dailyP>pthr series", xaxt = "n")
        axis(side = 1, at = ats, labels = yrs)
    }
    
    # test plot
    if (Ns > 0) {
        par(mar = c(4, 5, 3, 2) + 0.1, cex = 0.8, mfrow = c(1, 1))
        col <- 0
        np <- 0
        osp <- QMout$osp
        osmean <- QMout$osmean
        for (i in 1:(Ns + 1)) {
            Fd <- 0.5/QMout$Mq
            I1 <- if (i == 1) 1 else Ips[i - 1] + 1
            I2 <- Ips[i]

            ## nampiana
            I1 <- which(osp[, 1] == I1)
            I2 <- which(osp[, 1] == I2)

            ymax <- max(osp[, 2:3], na.rm = T)
            ymin <- min(osp[, 2:3], na.rm = T)
            if (i != Iseg.adj) {
                np <- np + 1
                if (col == 0) {
                  col <- 2
                  plot(osp[I1:I2, 2], osp[I1:I2, 3], xlim = c(0, 1), ylim = c(ymin, ymax), type = "l", lwd = 1, col = col, xlab = "Cumulative Frequency", ylab = "QM Adjustment", 
                    main = paste("distribution of QM adjustments with Mq=", QMout$Mq))
                  icol <- 2 * np
                  for (j in 1:QMout$Mq) {
                    lines(c(osmean[(j + 1), icol] - Fd, osmean[(j + 1), icol] + Fd), c(rep(osmean[(j + 1), (icol + 1)], 2)), col = col, lty = 2, lwd = 0.5)
                    if (j >= 1 & j < QMout$Mq) 
                      lines(rep(osmean[(j + 1), icol] + Fd, 2), c(osmean[(j + 1), (icol + 1)], osmean[(j + 2), (icol + 1)]), col = col, lty = 2, lwd = 0.5)
                  }
                } else {
                  col <- col + 1
                  lines(osp[I1:I2, 2], osp[I1:I2, 3], lwd = 1, col = col)
                  icol <- 2 * np
                  for (j in 1:QMout$Mq) {
                    lines(c(osmean[(j + 1), icol] - Fd, osmean[(j + 1), icol] + Fd), c(rep(osmean[(j + 1), (icol + 1)], 2)), col = col, lty = 2, lwd = 0.5)
                    if (j >= 1 & j < QMout$Mq) 
                      lines(rep(osmean[(j + 1), icol] + Fd, 2), c(osmean[(j + 1), (icol + 1)], osmean[(j + 2), (icol + 1)]), col = col, lty = 2, lwd = 0.5)
                  }
                }
                text(0.05, ymax - np * (ymax - ymin)/(Ns * 3), paste("Seg.", i))
                lines(c(0.15, 0.2), rep(ymax - np * (ymax - ymin)/(Ns * 3), 2), lwd = 2, col = col)
            } else np <- np + 1
        }
    }
    
    par(op)
    dev.off()
    
    ##################################################################################### 
    
    
    otmp <- LSmultiple(Y1, Ti, Ips)
    resi <- otmp$resi
    otmpW <- LSmultiple(W, Ti, Ips)
    resiW <- otmpW$resi
    otmpWL <- LSmultiple(WL, Ti, Ips)
    resiWL <- otmpWL$resi
    otmpWU <- LSmultiple(WU, Ti, Ips)
    resiWU <- otmpWU$resi
    
    
    ########################################################## define output file
    
    ofileIout <- paste(output, "_1Cs.txt", sep = "")
    ofileMout <- paste(output, "_mCs.txt", sep = "")
    file.create(ofileIout)
    
    
    
    #################################################### write '_1Cs.txt'
    
    if (Ns == 0) {
        cat(paste(Ns, "changepoints in Series", Station, "\n"), file = ofileIout)
        InsertMessagesTxt(main.txt.out, paste("PMF finds no Type-1 changepoints in the series", station))
        # cat('PMF finds no Type-1 changepoints in the series!\n')
    } else {
        cat(paste(Ns, "changepoints in Series", Station, "\n"), file = ofileIout)
        for (i in 1:Ns) {
            I1 <- if (i == 1) 
                1 else Ips[i - 1] + 1
            Ic <- Ips[i]
            I3 <- Ips[i + 1]
            Nseg <- I3 - I1 + 1
            PFx95 <- getPFx95(cor, Nseg)
            PFx95l <- getPFx95(corL, Nseg)
            PFx95h <- getPFx95(corU, Nseg)
            SSEf.Iseg <- sum(resi[I1:I3]^2)
            Ips1 <- Ips[-i]
            otmp1 <- LSmultiple(Y1, Ti, Ips1)
            SSE0.Iseg <- sum(otmp1$resi[I1:I3]^2)
            Fx <- (SSE0.Iseg - SSEf.Iseg) * (Nseg - 3)/SSEf.Iseg
            Pk1 <- Pk.PMFT(Nseg)
            PFx <- Fx * Pk1[Ic - I1 + 1]
            
            SSEf.Iseg <- sum(resiW[I1:I3]^2)
            otmp1 <- LSmultiple(W, Ti, Ips1)
            SSE0.Iseg <- sum(otmp1$resi[I1:I3]^2)
            Fx <- (SSE0.Iseg - SSEf.Iseg) * (Nseg - 3)/SSEf.Iseg
            if (Fx > 0) 
                prob <- pf(Fx, 1, Nseg - 3) else {
                Fx <- 0
                prob <- 0
                PFx <- 0
            }
            
            SSEf.Iseg <- sum(resiWL[I1:I3]^2)
            otmp1 <- LSmultiple(WL, Ti, Ips1)
            SSE0.Iseg <- sum(otmp1$resi[I1:I3]^2)
            FxL <- (SSE0.Iseg - SSEf.Iseg) * (Nseg - 3)/SSEf.Iseg
            if (FxL > 0) 
                probL <- pf(Fx, 1, Nseg - 3) else probL <- 0
            
            SSEf.Iseg <- sum(resiWU[I1:I3]^2)
            otmp1 <- LSmultiple(WU, Ti, Ips1)
            SSE0.Iseg <- sum(otmp1$resi[I1:I3]^2)
            FxU <- (SSE0.Iseg - SSEf.Iseg) * (Nseg - 3)/SSEf.Iseg
            if (FxU > 0) 
                probU <- pf(Fx, 1, Nseg - 3) else probU <- 0
            
            # else if(Id==1) { # type-1 changepoints
            if (PFx < PFx95l) 
                Idc <- "No  " else if (PFx >= PFx95l & PFx < PFx95h) 
                Idc <- "?   " else if (PFx >= PFx95h) 
                Idc <- "Yes "
            # }
            
            
            #################################################### write '_1Cs.txt'
            
            cat(paste(sprintf("%1.0f", 1), " ", sprintf("%-4.4s", Idc), sprintf("%10.0f", IY0[Ic]), " (", sprintf("%6.4f", probL), "-", sprintf("%6.4f", probU), 
                ")", sprintf("%6.3f", plev), sprintf("%10.4f", PFx), " (", sprintf("%10.4f", PFx95l), "-", sprintf("%10.4f", PFx95h), ")\n", sep = ""), file = ofileIout, 
                append = TRUE)
            
            #################################################### write '_Ustat.txt'
            
            cat(paste("PMF : c=", sprintf("%4.0f", Ic), "; (Time ", sprintf("%10.0f", IY0[Ic]), "); Type= 1; p=", sprintf("%10.4f", prob), "(", sprintf("%10.4f", 
                probL), "-", sprintf("%10.4f", probU), ")", "; PFmax=", sprintf("%10.4f", PFx), "; CV95=", sprintf("%10.4f", PFx95), "(", sprintf("%10.4f", PFx95l), 
                "-", sprintf("%10.4f", PFx95h), "); Nseg=", sprintf("%4.0f", Nseg), "\n", sep = ""), file = ofileSout, append = T)
        }
    }
    if (GUI) 
        return(0) else {
        file.copy(from = ofileIout, to = ofileMout, overwrite = TRUE)
        # cat('FindU.dlyPrcp finished successfully...\n')
        InsertMessagesTxt(main.txt.out, paste("FindU.dlyPrcp finished successfully for", station))
        return(0)
    }
}


