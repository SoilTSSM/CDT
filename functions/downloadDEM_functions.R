getDEMFun<-function(parent.win){
	if(Sys.info()["sysname"] == "Windows") largeur<-33
	else largeur<-31

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0<-tkframe(tt,relief='raised',borderwidth=2)
	frGrd1<-tkframe(tt)


	frA1<-tkframe(frGrd0,relief='sunken',bd=2)
	frA2<-tkframe(frGrd0,relief='sunken',bd=2)
	tkgrid(frA1,row=0,column=0,sticky='we',rowspan=1,columnspan=1,padx=5,pady=5,ipadx=1,ipady=1)
	tkgrid(frA2,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=5,pady=5,ipadx=1,ipady=1)

	###

	fr_grd<-ttklabelframe(frA1,text="Area of interest",relief="groove",borderwidth=2)
	tkgrid(fr_grd,padx=5,pady=5)
	grd_llon<-tklabel(fr_grd, text="Longitude",anchor='e',justify='right')
	grd_llat<-tklabel(fr_grd, text="Latitude",anchor='e',justify='right')
	grd_lb1<-tklabel(fr_grd, text="Minimum")
	grd_lb2<-tklabel(fr_grd, text="Maximum")

	grd_vlon1<-tkentry.h(fr_grd,txt.stbr1,'Minimum longitude in degree','Minimum longitude in degree')
	grd_vlon2<-tkentry.h(fr_grd,txt.stbr1,'Maximum longitude in degree','Maximum longitude in degree')
	grd_vlat1<-tkentry.h(fr_grd,txt.stbr1,'Minimum latitude in degree','Minimum latitude in degree')
	grd_vlat2<-tkentry.h(fr_grd,txt.stbr1,'Maximum latitude in degree','Maximum latitude in degree')

	minLon<-tclVar('42')
	maxLon<-tclVar('52')
	minLat<-tclVar('-26')
	maxLat<-tclVar('-12')

	tkconfigure(grd_vlon1, width=8,justify = "right",textvariable=minLon)
	tkconfigure(grd_vlon2, width=8,justify = "right",textvariable=maxLon)
	tkconfigure(grd_vlat1, width=8,justify = "right",textvariable=minLat)
	tkconfigure(grd_vlat2, width=8,justify = "right",textvariable=maxLat)

	tkgrid(grd_lb1,row=0, column=1,sticky="ew")
	tkgrid(grd_lb2,row=0, column=2,sticky="ew")
	tkgrid(grd_llon,row=1, column=0,sticky="ew")
	tkgrid(grd_vlon1,row=1, column=1,sticky="ew")
	tkgrid(grd_vlon2,row=1, column=2,sticky="ew")
	tkgrid(grd_llat,row=2, column=0,sticky="ew")
	tkgrid(grd_vlat1,row=2, column=1,sticky="ew")
	tkgrid(grd_vlat2,row=2, column=2,sticky="ew")

	###
	lab2<-tklabel(frA2,text='Directory to save downloaded files',anchor='w',justify='left')

	dir2save <-tclVar('')
	enfile.save<-tkentry(frA2,textvariable=dir2save,width=largeur)  #
	btfile.save<-tkbutton.h(frA2, text="...",txt.stbr1,'or browse here','')
	tkconfigure(btfile.save,command=function() fileORdir2Save(dir2save,isFile=FALSE))

	infobulle(frA2,'Enter the full path to directory to save downloaded files')
	status.bar.display(frA2,txt.stbr1,'Enter the full path to directory to save downloaded files')

	tkgrid(lab2,row=0,column=0,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(enfile.save,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btfile.save,row=1,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	###
	btOK<-ttkbutton(frGrd1,text='Download')
	tkgrid(btOK,row=0,column=0,sticky='e',padx=5,pady=5)
	tkconfigure(btOK,command=function(){
		outdir<-tclvalue(dir2save)
		minlon<-as.numeric(tclvalue(minLon))
		maxlon<-as.numeric(tclvalue(maxLon))
		minlat<-as.numeric(tclvalue(minLat))
		maxlat<-as.numeric(tclvalue(maxLat))

		if(outdir=="" | outdir=="NA"){
			tkmessageBox(message="Browse or enter directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			if(testConnection()){
				insert.txt(main.txt.out,"Downloading.................")
				#return(ExecDownload_DEM(minlon,maxlon,minlat,maxlat,outdir))
				ExecDownload_DEM(minlon,maxlon,minlat,maxlat,outdir)
			}else{
				insert.txt(main.txt.out,'No internet connection',format=TRUE)
				return(NULL)
			}
		}
	})

	tkgrid(frGrd0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frGrd1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Digital Elevation Model')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
		return(NULL)
	})
	tkwait.window(tt)
	return(0)
}

###################
ExecDownload_DEM<-function(minlon,maxlon,minlat,maxlat,outdir){

	tkconfigure(main.win,cursor='watch');tcl('update')
	getDEM(minlon,maxlon,minlat,maxlat,outdir)
	tkconfigure(main.win,cursor='')
}

#######################

aggregateDEM<-function(destfile,xm,ym,outdir,varid,longname,res1=FALSE){
	nc<-open.ncdf(destfile)
	xd<-nc$dim[[1]]$vals
	yd<-nc$dim[[2]]$vals
	xz<-get.var.ncdf(nc,varid=varid)
	close.ncdf(nc)
	demobj<-list(x=xd,y=yd,z=xz)
	xnew<-xm[xm>=min(xd) & xm<=max(xd)]
	ynew<-ym[ym>=min(yd) & ym<=max(yd)]
	if(res1){
		xnew<-xnew[-1]
		ynew<-ynew[-c(1,length(ynew))]
	}
	grdnew<-list(x=xnew, y=ynew)
	newobj<-interp.surface.grid(demobj,grdnew)
	dx <- dim.def.ncdf("Lon", "degreeE", newobj$x)
	dy <- dim.def.ncdf("Lat", "degreeN", newobj$y)
	demnc <- var.def.ncdf('dem', "m",list(dx,dy),NA,longname=longname, prec="single")
	outfl<-file.path(outdir,'DEM_for_Merging.nc',fsep = .Platform$file.sep)
	nc2 <- create.ncdf(outfl,demnc)
	put.var.ncdf(nc2,demnc,newobj$z)
	close.ncdf(nc2)
}

#######################

getDEM<-function(minlon,maxlon,minlat,maxlat,outdir){
	##DEM NOAA NGDC ETOPO2v2: ETOPO2v2c Global Gridded 2-minute elevation and bathymetric data.
	url<-'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO2v2/.z'
	area<-paste('X',minlon-0.0375,maxlon,'RANGEEDGES','Y',minlat-0.0375,maxlat+0.0375,'RANGEEDGES',sep='/')
	destfile<-file.path(outdir,'DEM_2_Arc-Minute.nc',fsep = .Platform$file.sep)
	link<-paste(url,area,'data.nc',sep='/')
	ret<-try(download.file(link,destfile,method="auto",quiet=TRUE,mode="wb",cacheOK=TRUE),silent=TRUE)

	##NOAA NGDC ETOPO1: ETOPO1 Grid Registered 1 Arc-Minute Global Relief Model
	url1<-'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO1/.z_bedrock'
	area1<-paste('lon',minlon,maxlon,'RANGEEDGES','lat',minlat,maxlat,'RANGEEDGES',sep='/')
	destfile1<-file.path(outdir,'DEM_1_Arc-Minute.nc',fsep = .Platform$file.sep)
	link1<-paste(url1,area1,'data.nc',sep='/')
	ret1<-try(download.file(link1,destfile1,method="auto",quiet=TRUE,mode="wb",cacheOK=TRUE),silent=TRUE)

	xm<-read.table(file.path(apps.dir,'country','Longitude.txt',fsep = .Platform$file.sep))
	ym<-read.table(file.path(apps.dir,'country','Latitude.txt',fsep = .Platform$file.sep))
	xm<-round(xm[,1],4)
	ym<-round(ym[,1],4)

	aggregateDEM2Merge<-function(ret,destfile,res1,varid,longname,msg){
		if(ret==0){
			down<-try(aggregateDEM(destfile,xm,ym,outdir,varid,longname,res1),silent=TRUE)
			if(!inherits(down, "try-error")) insert.txt(main.txt.out,msg)
			else{
				insert.txt(main.txt.out,'Unable to aggregate DEM for mering',format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',down[1]),format=TRUE)
			}
		}else insert.txt(main.txt.out,gsub('[\r\n]','',ret[1]),format=TRUE)
	}

	aggregateFailedMsg<-function(ret,destfile,msg){
		unlink(destfile)
		insert.txt(main.txt.out,msg,format=TRUE)
		insert.txt(main.txt.out,gsub('[\r\n]','',ret[1]),format=TRUE)
	}

	if(!inherits(ret, "try-error")) aggregateDEM2Merge(ret,destfile,TRUE,'z',"Elevation and bathymetric data",'Download finished for DEM 2-min')
	else{
		if(!inherits(ret1, "try-error")) aggregateDEM2Merge(ret1,destfile1,FALSE,'z_bedrock',"Global Relief Model",'Download finished for DEM 1-min')
		else aggregateFailedMsg(ret1,destfile1,'Download failed for DEM 1-min')
		aggregateFailedMsg(ret,destfile,'Download failed for DEM 2-min')
	}
}

