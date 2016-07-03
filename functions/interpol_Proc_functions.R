#Inverse distance weighted interpolation using elevation to select neighbor stations

selectNeighbor<-function(i,dist,delv,nmin,nmax,maxdist,elvdiff,idp,omax){
	r<-dist[i,]
	z<-delv[i,]
	id<-which(r<=maxdist)
	if(length(id)==0 | length(id)<nmin) return(NULL)
	dat<-cbind(r,z,1:length(r))
	dat<-dat[id,]
	if(is.null(dim(dat))) dat<-data.frame(matrix(dat,ncol=3))
	dat<-dat[order(dat[,1]),]
	ielv<-which(abs(dat[,2])<=elvdiff)
	if(length(ielv)==0 | length(ielv)<nmin) return(NULL)
	dat<-dat[ielv,]
	if(!is.null(dim(dat))){
		if(nrow(dat)>nmax) dat<-dat[1:nmax,]
		if(!is.null(dim(dat))){
			invd<-1/(dat[,1])^idp
			idx<-dat[,3]
		}else{
			invd<-1/unname(dat[1])^idp
			idx<-dat[3]
		}
	}else{
		invd<-1/unname(dat[1])^idp
		idx<-dat[3]
	}
	return(list(invd=invd,idx=idx))
}

#############

idwElv<-function(var='z',locs=c('lon','lat'),elv='elv',data,newdata,nmin,nmax,maxdist,elvdiff,idp,omax=NULL){
	classnewdata<-class(newdata)
	data<-as.data.frame(data)
	newdata<-as.data.frame(newdata)
	res<-data.frame(newdata[,locs],NA,NA)
	names(res)<-c(locs,'var1.pred','var1.var')

	dist<-rdist.earth(newdata[,locs],data[,locs], miles=FALSE)
	delv<-t(t(matrix(rep(newdata[,elv],length(data[,elv])),nrow=length(newdata[,elv])))-data[,elv])
	weights<-lapply(1:nrow(newdata), function(i) selectNeighbor(i,dist,delv,nmin,nmax,maxdist,elvdiff,idp,omax))

	res$var1.pred<-sapply(seq_along(weights),function(j){
		x<-weights[[j]]
		if(!is.null(x)) sum(x$invd*data[,var][x$idx])/sum(x$invd)
		else NA
	})
	if(classnewdata!="data.frame") coordinates(res)<-locs
	return(res)
}


############################################################
interpolationProc<-function(donne,demdata,interpolParams){

	mthd<-interpolParams$mthd
	grdChx<-interpolParams$grdChx
	ncfila<-interpolParams$ncfila
	grdCR<-interpolParams$grdCR
	file2save<-interpolParams$file2save
	vgmChx<-interpolParams$vgmChx
	VgmMod<-interpolParams$VgmMod
	vgmModList<-interpolParams$vgmModList
	useELV<-interpolParams$useELV
	maxdist<-as.numeric(interpolParams$maxdist)
	nmin<-as.numeric(interpolParams$nmin)
	nmax<-as.numeric(interpolParams$nmax)
	idp<-as.numeric(interpolParams$idp)
	elvdiff<-as.numeric(interpolParams$elvdiff)
	omax<-as.numeric(interpolParams$omax)


	if(is.null(donne)){
		insert.txt(main.txt.out,'No station data found',format=TRUE)
		return(NULL)
	}

	if(useELV=='1' & is.null(donne$elv)) return(NULL)
	if(useELV=='1' & is.null(demdata)){
		insert.txt(main.txt.out,'No DEM data found',format=TRUE)
		return(NULL)
	}

	if(is.na(file2save) | file2save==''){
		insert.txt(main.txt.out,'No output file provided',format=TRUE)
		return(NULL)
	}

	odata<-data.frame(lon=donne$lon,lat=donne$lat,z=donne$z)
	if(useELV=='1'){
		odata$elv<-donne$elv
		odata$elv[is.na(odata$elv)]<-1
	}
	odata<-odata[!is.na(odata$z), ]

	if(nrow(odata)<5){
		insert.txt(main.txt.out,'Number of observations is too small to interpolate',format=TRUE)
		return(NULL)
	}


	coordinates(odata)<- ~lon+lat
	proj4string(odata) <-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
	#odata<-SpatialPointsDataFrame(coords=odata[,c('lon','lat')],data=data.frame(z=odata[,'z']),proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


	if(grdChx=='0'){
		if(is.na(ncfila) | ncfila==''){
			insert.txt(main.txt.out,'No NetCDF gridded data provided',format=TRUE)
			return(NULL)
		}
		all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
		jfile<-which(all.open.file==ncfila)
		fdem<-file.opfiles[[jfile]][[2]]
		xlon<-fdem$x
		xlat<-fdem$y
	}
	if(grdChx=='1'){
		parGrd<-as.numeric(as.character(grdCR$new.grid$Values))
		if(sum(is.na(parGrd))>0){
			insert.txt(main.txt.out,'Some values for grid are missing',format=TRUE)
			return(NULL)
		}
		xlon<-seq(parGrd[1],parGrd[2],parGrd[3])
		xlat<-seq(parGrd[4],parGrd[5],parGrd[6])
	}

	newgrid<-data.frame(expand.grid(lon=xlon,lat=xlat))
	coordinates(newgrid)<- ~lon+lat
	#gridded(newgrid)<-TRUE
	#proj4string(newgrid) <-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
	newgrid<-SpatialPixels(points =newgrid, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
	if(useELV=='1'){
		demgrid<-getDEMatNewGrid(newgrid,demdata)
		demgrid[is.na(demgrid)]<-0
		newgrid<-SpatialPixelsDataFrame(points=newgrid,data=data.frame(elv=demgrid))
	}

	if(mthd=='Kriging'){
		if(vgmChx=='0'){
			if(useELV=='1') evgm<- variogram(z~elv, odata) #residual variogram
			else evgm<- variogram(z~1, odata)
			#fvgm<-try(fit.variogram(evgm,model=vgm(psill=max(evgm$gamma,na.rm=T), model=VgmMod, range=max(evgm$dist,na.rm=T), nugget=min(evgm$gamma,na.rm=T))),silent=TRUE)
			fvgm<-try(fit.variogram(evgm,model=vgm(psill=var(odata$z,na.rm=T), model=VgmMod,range=quantile(evgm$dist,probs=0.8,na.rm=T), nugget=min(evgm$gamma,na.rm=T))),silent=TRUE)
			if(inherits(fvgm, "try-error")){
				insert.txt(main.txt.out,"Variogram fitting failed",format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',fvgm[1]),format=TRUE)
				return(NULL)
			}
		}
		if(vgmChx=='1'){
			if(useELV=='1') autovgm<-try(autofitVariogram(z~elv,model=vgmModList,input_data=odata),silent=TRUE)
			else autovgm<-try(autofitVariogram(z~1,model=vgmModList,input_data=odata),silent=TRUE)
			if(inherits(autovgm, "try-error")){
				insert.txt(main.txt.out,"Variogram fitting failed",format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',autovgm[1]),format=TRUE)
				return(NULL)
			}else{
				fvgm<-autovgm$var_model
			}
		}
		if(useELV=='1'){
			#intrpdata<-try(krige(z~elv, locations=odata,newdata=newgrid,model=fvgm,nmin=nmin,nmax=nmax,omax=omax,debug.level=0),silent=TRUE) #,maxdist=maxdist
			# regression part only
			intrpdata<-try(krige(z~elv, locations=odata,newdata=newgrid,nmin=nmin,nmax=nmax,maxdist=maxdist,omax=omax,debug.level=0),silent=TRUE)
			lm_elv<-lm(z~elv, odata)
			intrp_ROK<-krige(residuals(lm_elv)~1, locations=odata, newdata=newgrid, model=fvgm)  # OK of residuals
			intrpdata$var1.pred <- intrpdata$var1.pred + intrp_ROK$var1.pred
		}
		else intrpdata<-try(krige(z~1, locations=odata,newdata=newgrid,model=fvgm,nmin=nmin,nmax=nmax,maxdist=maxdist,omax=omax,debug.level=0),silent=TRUE)
	}

	if(mthd=='IDW'){
		if(useELV=='1'){
			intrpdata<-try(idwElv(var='z',locs=c('lon','lat'),elv='elv',data=odata,newdata=newgrid,nmin=nmin,nmax=nmax,maxdist=maxdist,elvdiff=elvdiff,idp=idp),silent=TRUE)
			#weighted least squares prediction
			#intrpdata<-try(krige(z~elv, locations=odata,newdata=newgrid,nmin=nmin,nmax=nmax,maxdist=maxdist,omax=omax, set=list(idp=idp),debug.level=0),silent=TRUE)
		}else{
			#intrpdata<-idw(z~1, locations=odata,newdata=newgrid,nmin=nmin,nmax=nmax,maxdist=maxdist,idp=idp,debug.level=0)
			intrpdata<-try(krige(z~1, locations=odata,newdata=newgrid,nmin=nmin,nmax=nmax,maxdist=maxdist,omax=omax, set=list(idp=idp),debug.level=0),silent=TRUE)
		}
	}

	if(mthd=='Nearest Neighbor'){
		intrpdata<-try(krige(z~1, locations=odata,newdata=newgrid,nmax=1,maxdist=maxdist,debug.level=0),silent=TRUE)
	}

	if(!inherits(intrpdata, "try-error")){
		#Defines netcdf output dims
		dx <- ncdim_def("Lon", "degreeE", xlon)
		dy <- ncdim_def("Lat", "degreeN", xlat)
		xy.dim<-list(dx,dy)
		grd.out<-ncvar_def("var", "units",list(dx,dy),-99,longname="Interpolated data", prec="short")

		out.interp<-intrpdata@data$var1.pred
		dim(out.interp) <- c(length(xlon),length(xlat))
		out.interp0<-out.interp
		out.interp[is.na(out.interp)] <- -99

		#Apply mask for area of interest
		#if(!is.null(outMask)) out.interp[is.na(outMask)] <- -99

		filename<-paste(getf.no.ext(basename(file2save))	,'_',donne$date,'.nc',sep='')
		outfl<-file.path(dirname(file2save),filename,fsep = .Platform$file.sep)
		nc2 <- nc_create(outfl,grd.out)
		ncvar_put(nc2,grd.out,out.interp)
		nc_close(nc2)
		return(list(filename,list(x=xlon,y=xlat,value=out.interp0),outfl))
	}else{
		insert.txt(main.txt.out,"Interpolation failed",format=TRUE)
		insert.txt(main.txt.out,gsub('[\r\n]','',intrpdata[1]),format=TRUE)
		return(NULL)
	}
}


