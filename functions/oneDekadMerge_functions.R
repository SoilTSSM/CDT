
Treat1DekRain<-function(){

	file.pars<-as.character(gal.params$file.io$Values)
	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jfile<-which(all.open.file==file.pars[1])
	donne<-file.opfiles[[jfile]][[2]]

	#######get data
	donne<-splitCDTData(donne,'dekadal')
	if(is.null(donne)) return(NULL)
	stn.lon<-donne$lon
	stn.lat<-donne$lat
	dates<-donne$dates
	donne<-donne$data

	daty<-as.character(gal.params$dates.mrg$Values)
	yrs<-as.numeric(daty[1])
	mon<-as.numeric(daty[2])
	dek<-as.numeric(daty[3])
	daty1<-paste(format(as.Date(paste(yrs,mon,dek,sep='-')),'%Y%m'),dek,sep='')

	ix<-which(dates%in%daty1)
	if(length(ix)==0){
		insert.txt(main.txt.out,"The input date does not match the date in the station data file",format=TRUE)
		return(NULL)
	}
	donne1<-as.numeric(donne[ix,])
	stnlist<-list(lon=stn.lon,lat=stn.lat,dates=daty1,data=donne1)

	###get elevation data
	if(gal.params$blankGrd=="2"){
		jncdf<-which(all.open.file==file.pars[5])
		fdem<-file.opfiles[[jncdf]][[2]]
		dem<-fdem$value
		dem[dem<0]<-0
		dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf<-data.frame(dem=c(dem))
		demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
		demlist<-list(lon=fdem$x,lat=fdem$y,demGrd=demdf)
	}else demlist<-NULL

	##Shapefile
	if(gal.params$blankGrd=="3"){
		jshp<-which(all.open.file==file.pars[6])
		shpd<-file.opfiles[[jshp]][[2]]
	}else shpd<-NULL

	mrgRaindat<-list(stnData=stnlist,demData=demlist,shpData=shpd)
	return(mrgRaindat)
}


#################################################

mergeOneDekadRain<-function(){
	mrgRaindat<-Treat1DekRain()
	if(is.null(mrgRaindat)) return(NULL)
	VarioModel<-c("Sph", "Exp", "Gau")

	origdir<-as.character(gal.params$file.io$Values[4])
	nmin<-as.numeric(as.character(gal.params$params.int$Values[1]))
	nozero<-as.numeric(as.character(gal.params$params.int$Values[2]))
	max.RnR.dist<-as.numeric(as.character(gal.params$params.int$Values[3]))
	maxdist<-as.numeric(as.character(gal.params$params.int$Values[4]))
	min.nbrs<-as.numeric(as.character(gal.params$params.int$Values[5]))
	max.nbrs<-as.numeric(as.character(gal.params$params.int$Values[6]))

	interpMethod<-as.character(gal.params$params.mrg$Values[1])
	RainNoRain<-as.character(gal.params$params.mrg$Values[2])

	####bias adj
	rfeDir<-as.character(gal.params$file.io$Values[2])
	biasDir<-as.character(gal.params$file.io$Values[3])

	rfeFileFormat<-as.character(gal.params$prefix$Values[1])
	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])

	biasFile<-file.path(biasDir,paste(meanBiasPrefix,'_1.nc',sep=''),fsep = .Platform$file.sep)
	if(!file.exists(biasFile)){
		insert.txt(main.txt.out,"Mean bias coefficients not found",format=TRUE)
		return(NULL)
	}

	####
	daty<-as.character(gal.params$dates.mrg$Values)
	yrs<-as.numeric(daty[1])
	mon<-as.numeric(daty[2])
	dek<-as.numeric(daty[3])
	daty1<-paste(format(as.Date(paste(yrs,mon,dek,sep='-')),'%Y%m'),dek,sep='')

	annual.dek<-expand.grid(dek=1:3,mon=1:12)
	ijt<-which(annual.dek$dek==dek & annual.dek$mon==mon)

	rfefl<-file.path(rfeDir,sprintf(rfeFileFormat,yrs,substr(daty1,5,6),dek))
	bsfl<-file.path(biasDir,paste(meanBiasPrefix,'_',ijt,'.nc',sep=''),fsep = .Platform$file.sep)
	outfl<-file.path(origdir,paste('rr_adj','_',daty1,'.nc',sep=''),fsep = .Platform$file.sep)

	nc <- open.ncdf(rfefl)
	rfe.lon <- nc$dim[[1]]$vals
	rfe.lat <- nc$dim[[2]]$vals
	rfe <- get.var.ncdf(nc,varid =nc$var[[1]]$name)
	close.ncdf(nc)
	xo<-order(rfe.lon)
	yo<-order(rfe.lat)
	rfe.lon <-rfe.lon[xo]
	rfe.lat <-rfe.lat[yo]
	rfe<-rfe[xo,yo]
	rfeObj<-list(x=rfe.lon,y=rfe.lat,z=rfe)

	nc <- open.ncdf(bsfl)
	bisa.lon<-nc$dim[[1]]$vals
	bisa.lat<-nc$dim[[2]]$vals
	bias <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
	close.ncdf(nc)
	grdnew<-list(x=bisa.lon,y=bisa.lat)
	newObj<-interp.surface.grid(rfeObj,grdnew)
	rfe<-newObj$z
	rfe.adj <- round(rfe * bias,2)

	###################################################
	dx <- dim.def.ncdf("Lon", "degreeE", bisa.lon)
	dy <- dim.def.ncdf("Lat", "degreeN", bisa.lat)
	xy.dim<-list(dx,dy)
	grd.bsadj <- var.def.ncdf("precip", "mm",xy.dim, -99, longname= " Mean Bias Adjusted RFE", prec="single")
	grd.out<-var.def.ncdf("precip", "mm",xy.dim,-99,longname=" Merged Station-Satellite Rainfall", prec="single")

	######################Save adjusted data
	rfe.adj1 <-rfe.adj
	rfe.adj1[is.na(rfe.adj1)] <- -99

	nc2 <- create.ncdf(outfl,grd.bsadj)
	put.var.ncdf(nc2,grd.bsadj,rfe.adj1)
	close.ncdf(nc2)

	###################################################
	nlon0<-length(bisa.lon)
	nlat0<-length(bisa.lat)
	newlocation.merging <- expand.grid(lon=bisa.lon, lat=bisa.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	########Blank mask
	usemask<-as.character(gal.params$blankGrd)
	if(usemask=="1") outMask<-NULL
	if(usemask=="2"){
		dem.grd<-krige(formula = dem ~ 1,locations=mrgRaindat$demData$demGrd,newdata=newlocation.merging,nmax=8,nmin =3,debug.level=0)
		dem<-ifelse(dem.grd@data$var1.pred<0,0,dem.grd@data$var1.pred)
		outMask<-matrix(dem,nrow=nlon0,ncol=nlat0)
		outMask[outMask==0]<-NA
	}
	if(usemask=="3"){
		shpd<-mrgRaindat$shpData
		shpd[['vtmp']]<-1
		shpMask<-over(newlocation.merging,shpd)[,'vtmp']
		outMask<-matrix(shpMask,nrow=nlon0,ncol=nlat0)
	}

	#########
	##Gauge data
	stn.lon<-mrgRaindat$stnData$lon
	stn.lat<-mrgRaindat$stnData$lat
	stn.data<-mrgRaindat$stnData$data

	#Index of new grid over stations
	stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- unname(over(stn.loc, geometry(grid.loc)))

	#rfe over stn location
	rfe_gg <- rfe.adj[ijGrd]
	dff <- stn.data - rfe_gg

	# Remove extremes differences between gauge and satellite
	q1 <- quantile(dff,0.0001,na.rm=T)
	q2 <- quantile(dff,0.9999, na.rm=T)
	dff[dff < q1] <- NA
	dff[dff > q2] <- NA
	ix<-which(!is.na(dff))
	rfe.vec<-c(rfe.adj )
	out.mrg<-rfe.vec  ##Initial rfe

	if(sum(stn.data,na.rm=TRUE)>0 & length(ix)>=nmin){

		rr.stn <-data.frame(cbind(stn.lon,jitter(stn.lat),stn.data,rfe_gg,dff))
		rr.stn<-rr.stn[ix,]
		names(rr.stn) <- c("lon", "lat", "gg","rfe","dff")
		coordinates(rr.stn) = ~lon+lat

		#ijx1 <- which(rr.stn$gg >0 & rr.stn$rfe >0)
		ijx1 <- which(rr.stn$gg >0)
		if(length(ijx1)>=nozero){
			#grd.newloc<-newlocation.merging
			#grd.newloc$rfe<-rfe.vec
			grd.newloc<-SpatialPointsDataFrame(coords=newlocation.merging,data=data.frame(rfe=rfe.vec))
			rr.glm <- glm(gg~rfe, rr.stn, family=gaussian)
			rr.stn$res <- residuals(rr.glm)
			pred.rr <- predict(rr.glm, newdata=grd.newloc, se.fit=T)

			if(interpMethod=="IDW"){
				grd.rr<- krige(res~1, locations=rr.stn,newdata=newlocation.merging,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0)
				res.pred<-grd.rr$var1.pred
			}else if(interpMethod=="Kriging"){
				grd.rr<-try(autoKrige(res~1,input_data=rr.stn,new_data=newlocation.merging,model=VarioModel,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0), silent=TRUE)
				if(!inherits(grd.rr, "try-error")){
					res.pred<-grd.rr$krige_output$var1.pred
				}else{
					grd.rr<- krige(res~1, locations=rr.stn,newdata=newlocation.merging,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0)
					res.pred<-grd.rr$var1.pred
				}
			}
			out.mrg<- as.numeric(res.pred+pred.rr$fit)
			out.mrg<-ifelse(out.mrg<0,0,out.mrg)
		}else{
			grd.rr <- idw(dff~1,locations=rr.stn,newdata=newlocation.merging,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist,debug.level=0)
			out.mrg<- grd.rr$var1.pred + rfe.vec
			out.mrg<-ifelse(out.mrg<0,0,out.mrg)
		}
		# Take RFE for areas where interpolation/merging was not possible
		ix <- which(is.na(out.mrg))
		out.mrg[ix] <- rfe.vec[ix]
		
		cells<-as(newlocation.merging,'SpatialPixels')@grid

		#Rain-non-Rain Mask
		if(RainNoRain!='None') {
			rr.stn$rnr <- ifelse(rr.stn$gg >=1,1,0)
			if (RainNoRain=='Gauge') {#Gauge only
				rnr.grd<-krige(rnr~1, locations=rr.stn, newdata=newlocation.merging,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.RnR.dist,debug.level=0)
				rnr.pred<-ifelse(is.na(rnr.grd$var1.pred),1,rnr.grd$var1.pred)
				RnoR<-round(rnr.pred)
				RnoR[is.na(RnoR)]<-1
			} else if(RainNoRain=='Satellite') {#Satellite only
				RnoR<-ifelse(rfe.vec >=1, 1, 0)
				RnoR[is.na(RnoR)]<-1
			} else if(RainNoRain=='GaugeSatellite') {
				rfe.rnr <- ifelse(rfe.vec >=1, 1, 0)
				rnr.grd<-krige(rnr~1, locations=rr.stn, newdata=newlocation.merging,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.RnR.dist, debug.level=0)
				RnoR <-rnr.grd$var1.pred
				RnoR<-round(RnoR)
				ix <- which(is.na(RnoR))
				RnoR[ix] <- rfe.rnr[ix]
				RnoR[is.na(RnoR)]<-1
				##smoothing???
				img.RnoR<-as.image(RnoR, x=coordinates(newlocation.merging), nx=cells@cells.dim[1], ny=cells@cells.dim[2])
				smooth.RnoR<-image.smooth(img.RnoR, theta= 0.08)
				RnoR <-round(c(smooth.RnoR$z))
			}
			out.mrg <- out.mrg * RnoR
		}
	}

	dim(out.mrg) <- c(nlon0,nlat0)
	out.mrg[is.na(out.mrg)] <- -99

	#Apply mask for area of interest
	if(!is.null(outMask)) out.mrg[is.na(outMask)] <- -99

	outfl<-file.path(origdir,paste('rr_mrg','_',daty1,'_MON.nc',sep=''),fsep = .Platform$file.sep)
	nc2 <- create.ncdf(outfl,grd.out)
	put.var.ncdf(nc2,grd.out,out.mrg)
	close.ncdf(nc2)
	return(0)
}

