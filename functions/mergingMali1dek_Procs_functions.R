update1DekProc_Mali<-function(origdir){
	existRFE<-as.character(gal.params$rfeDownSep)
	existSTN<-as.character(gal.params$stnDataDispo)
	year<-str_trim((as.character(gal.params$dates.mrg$Values[1])))
	mon<-str_trim(as.character(gal.params$dates.mrg$Values[2]))
	mon<-ifelse(as.numeric(mon)<10,paste('0',mon,sep=''),mon)
	dek<-str_trim(as.character(gal.params$dates.mrg$Values[3]))
	rfeAfrica<-as.character(gal.params$file.io$Values[2])
	file.stn<-as.character(gal.params$file.io$Values[1])

	VarioModel<-c("Sph", "Exp", "Gau")

	deb<-try(as.Date(paste(year,mon,dek,sep='-')),silent=TRUE)
	if(inherits(deb, "try-error")| is.na(deb)){
		insert.txt(main.txt.out,'Verifier la date',format=TRUE)
		return(NULL)
	}
	deb<-strsplit(as.character(deb),'-')
	year1<-deb[[1]][1]
	mon1<-deb[[1]][2]
	dek1<-as.numeric(deb[[1]][3])
	if(dek1>3){
		insert.txt(main.txt.out,'La decade doit etre 1, 2 ou 3',format=TRUE)
		return(NULL)
	}

	minlon<- -12.375
	maxlon<- 4.3875
	minlat<- 10.0125
	maxlat<- 25.2

	if(existRFE=='0'){
		if(!testConnection()){
			insert.txt(main.txt.out,'No internet connection',format=TRUE)
			return(NULL)
		}
		url<-'http://tamsat.org.uk/public_data'
		outdir0<-file.path(origdir,'Dekad_TAMSAT_Africa',fsep = .Platform$file.sep)
		if(!file.exists(outdir0)) dir.create(outdir0,showWarnings=FALSE,recursive=TRUE)
		file0<-paste('rfe',year,'_',mon,'-dk',dek,'.nc',sep='')
		link<-paste(url,year,mon,file0,sep='/')
		destfile0<-file.path(outdir0,file0,fsep = .Platform$file.sep)
		test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
		if(inherits(test, "try-error")){
			insert.txt(main.txt.out,paste(file0,": n'est pas encore disponible ou la connexion internet est perdue"),format=TRUE)
			return(NULL)
		}

		insert.txt(main.txt.out,"Téléchargement.................")
		tcl("update")
		ret<-try(download.file(link,destfile0,mode="wb",quiet=TRUE),silent=TRUE)
		if(ret!=0){
			insert.txt(main.txt.out,paste('Échec du téléchargement pour:',file0),format=TRUE)
			return(NULL)
		}else{
			nc<-nc_open(destfile0)
			xm<-nc$dim[[2]]$vals
			ym<-nc$dim[[1]]$vals
			xdat<-ncvar_get(nc,varid=nc$var[[1]]$name)
			nc_close(nc)
			insert.txt(main.txt.out,paste('Téléchargement pour:',file0,'terminé'))
		}

	}else{
		if(!file.exists(rfeAfrica)){
			insert.txt(main.txt.out,"Les donnees de TAMSAT pour Africa n'existe pas",format=TRUE)
			return(NULL)
		}
		nc<-nc_open(rfeAfrica)
		xm<-nc$dim[[2]]$vals
		ym<-nc$dim[[1]]$vals
		xdat<-ncvar_get(nc,varid=nc$var[[1]]$name)
		nc_close(nc)
		insert.txt(main.txt.out,paste('Extraction  terminée pour:',basename(rfeAfrica)))
	}
	tcl("update")
	
	xo<-order(xm)
	xm<-xm[xo]
	yo<-order(ym)
	ym<-ym[yo]
	xdat<-xdat[xo,yo]
	idx<-which(xm>= minlon & xm<= maxlon)
	idy<-which(ym>= minlat & ym<= maxlat)
	xm<-xm[idx]
	ym<-ym[idy]
	xdat<-xdat[idx,idy]
	xdat[is.na(xdat)] <- -99
	outdir1<-file.path(origdir,'Dekad_TAMSAT_MALI',fsep = .Platform$file.sep)
	if(!file.exists(outdir1)) dir.create(outdir1,showWarnings=FALSE,recursive=TRUE)
	nc_out_file<-file.path(outdir1,paste('rfe',year,'_',mon,'-dk',dek,'.nc',sep=''),fsep=.Platform$file.sep)

	nlon0<-length(xm)
	nlat0<-length(ym)
	dx <- ncdim_def("Lon", "degreeE", xm)
	dy <- ncdim_def("Lat", "degreeN", ym)
	nc_out_var <- ncvar_def('precip', "mm", list(dx,dy), -99,longname= "TAMSAT Rain Fall Estimate (RFE)", prec="short")
	nc <- nc_create(nc_out_file,nc_out_var)
	ncvar_put(nc,nc_out_var,xdat)
	nc_close(nc)

	newlocation.merging <- expand.grid(lon=xm, lat=ym)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	shpd<-readShapeSpatial(file.path(apps.dir,'country','Mali','MLI_adm0.shp',fsep = .Platform$file.sep))
	shpd[['vtmp']]<-1
	shpMask<-over(newlocation.merging,shpd)[,'vtmp']
	outMask<-matrix(shpMask,nrow=nlon0,ncol=nlat0)

	outdir2<-file.path(origdir,'MALI_MERGED_DATA',fsep = .Platform$file.sep)
	if(!file.exists(outdir2)) dir.create(outdir2,showWarnings=FALSE,recursive=TRUE)
	rr_mrg_mon<-file.path(outdir2,paste('rr_mrg_',year,mon,dek,'_MON.nc',sep=''),fsep=.Platform$file.sep)

	if(existSTN=='1'){
		all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
		jfile<-which(all.open.file==file.stn[1])
		donne<-file.opfiles[[jfile]][[2]]
		stn.lon<-as.numeric(donne[2,-1])
		stn.lat<-as.numeric(donne[3,-1])
		dates<-as.character(donne[4,1])
		if(paste(year,mon,dek,sep='')!=dates){
			insert.txt(main.txt.out,"La date entrée ne correspond pas a la date des données de stations",format=TRUE)
			return(NULL)
		}
		stn.data<-as.numeric(donne[4,-1])
		stn.data[stn.data<0]<-NA
		stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
		stn.loc <- SpatialPoints(stn.loc)
		ijGrd <- unname(over(stn.loc, geometry(grid.loc)))
		
		xdat[xdat==-99]<-NA
		grd.out<-ncvar_def("precip", "mm",list(dx,dy),-99,longname=" Merged Station-Satellite Rainfall", prec="short")
		cells<-as(newlocation.merging,'SpatialPixels')@grid

		##block grid
		sDX <- cells@cellsize[1]/2
		dBX <- seq(-sDX, sDX, length.out=4)
		sDY <- cells@cellsize[2]/2
		dBY <- seq(-sDY, sDY, length.out=4)
		bGrd <- expand.grid(x=dBX, y=dBY)

		#rfe over stn location
		rfe_gg <- xdat[ijGrd]
		dff <- stn.data - rfe_gg

		stnData<-data.frame(lon=stn.lon,lat=stn.lat,gg=stn.data)
		stnData<-stnData[!is.na(stnData$gg),]
		coordinates(stnData) = ~lon+lat
		grdStnData<- krige(gg~1, locations=stnData,newdata=newlocation.merging,block=bGrd,nmin=1,nmax=5,maxdist=0.1, debug.level=0) #0.5
		out.stn<- grdStnData$var1.pred
		out.stn<-ifelse(out.stn<0,0,out.stn)
		dim(out.stn) <- c(nlon0,nlat0)

		# Remove extremes differences between gauge and satellite
		# q1 <- quantile(dff,0.0001,na.rm=T)
		# q2 <- quantile(dff,0.9999, na.rm=T)
		# dff[dff < q1] <- NA
		# dff[dff > q2] <- NA
		ix<-which(!is.na(dff))
		rfe.vec<-c(xdat)
		out.mrg<-rfe.vec  ##Initial rfe

		if(sum(stn.data,na.rm=TRUE)>0 & length(ix)>=10){
			rr.stn <-data.frame(cbind(stn.lon,stn.lat,stn.data,rfe_gg,dff))
			rr.stn<-rr.stn[ix,]
			names(rr.stn) <- c("lon", "lat", "gg","rfe","dff")
			coordinates(rr.stn) = ~lon+lat
			ijx1 <- which(rr.stn$gg >0)
			if(length(ijx1)>=7){
				grd.newloc<-SpatialPointsDataFrame(coords=newlocation.merging,data=data.frame(rfe=rfe.vec))
				rr.glm <- glm(gg~rfe, rr.stn, family=gaussian)
				rr.stn$res <- residuals(rr.glm)
				pred.rr <- predict(rr.glm, newdata=grd.newloc, se.fit=T)

				grd.rr<-try(autoKrige(res~1,input_data=rr.stn,new_data=newlocation.merging,block=bGrd,model=VarioModel,nmin=2,nmax=5,maxdist=0.5, debug.level=0), silent=TRUE) #0.5
				if(!inherits(grd.rr, "try-error")){
					res.pred<-grd.rr$krige_output$var1.pred
				}else{
					grd.rr<- krige(res~1, locations=rr.stn,newdata=newlocation.merging,block=bGrd,nmin=2,nmax=5,maxdist=0.5, debug.level=0) #0.5
					res.pred<-grd.rr$var1.pred
				}
				out.mrg<- as.numeric(res.pred+pred.rr$fit)
				out.mrg<-ifelse(out.mrg<0,0,out.mrg)
			}else{
				grd.rr <- krige(dff~1,locations=rr.stn,newdata=newlocation.merging,block=bGrd,nmin=2,nmax=5,maxdist=0.5,debug.level=0) #0.5
				out.mrg<- grd.rr$var1.pred + rfe.vec
				out.mrg<-ifelse(out.mrg<0,0,out.mrg)
			}
			# Take RFE for areas where interpolation/merging was not possible
			ix <- which(is.na(out.mrg))
			out.mrg[ix] <- rfe.vec[ix]

			#Rain-non-Rain Mask
			rr.stn$rnr <- ifelse(rr.stn$gg >=1,1,0)
			rfe.rnr <- ifelse(rfe.vec >=1, 1, 0)
			rnr.grd<-krige(rnr~1, locations=rr.stn, newdata=newlocation.merging,block=bGrd,nmin=2,nmax=5,maxdist=0.3, debug.level=0)
			RnoR <-rnr.grd$var1.pred
			RnoR<-round(RnoR)
			ix <- which(is.na(RnoR))
			RnoR[ix] <- rfe.rnr[ix]
			RnoR[is.na(RnoR)]<-1
			##smoothing???
			img.RnoR<-as.image(RnoR, x=coordinates(newlocation.merging), nx=cells@cells.dim[1], ny=cells@cells.dim[2])
			smooth.RnoR<-image.smooth(img.RnoR, theta= 0.075)
			RnoR <-round(c(smooth.RnoR$z))
			out.mrg <- out.mrg * RnoR
		}
		dim(out.mrg) <- c(nlon0,nlat0)

		out.mrg[!is.na(out.stn)]<-out.stn[!is.na(out.stn)]
		img.mrg<-as.image(out.mrg, x= coordinates(newlocation.merging), nx=cells@cells.dim[1], ny=cells@cells.dim[2])
		smooth.mrg<-image.smooth(img.mrg, theta= 0.03)
		out.mrg <-round(smooth.mrg$z,1)
		out.mrg[is.na(out.mrg)] <- -99

		#Apply mask for area of interest
		out.mrg[is.na(outMask)] <- -99
		nc2 <- nc_create(rr_mrg_mon,grd.out)
		ncvar_put(nc2,grd.out,out.mrg)
		nc_close(nc2)
	}else{
		xdat[is.na(outMask)] <- -99
		nc <- nc_create(rr_mrg_mon,nc_out_var)
		ncvar_put(nc,nc_out_var,xdat)
		nc_close(nc)
	}

	return(0)
}