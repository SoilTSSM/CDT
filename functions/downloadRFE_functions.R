DownloadRFE<-function(parent.win){
	if(Sys.info()["sysname"] == "Windows") largeur<-27
	else largeur<-21

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA<-tkframe(tt)
	frB<-tkframe(tt)
	tkgrid(frA,frB)

	fr.A0<-tkframe(frA,relief='sunken',borderwidth=2)
	tkgrid(fr.A0,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	fr.A01<-tkframe(fr.A0)
	fr.A02<-tkframe(fr.A0)
	tkgrid(fr.A01,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A02,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	fileSource <- tclVar('10-DAYS TAMSAT')
	cb.period<-ttkcombobox(fr.A02, values=c('10-DAYS TAMSAT','10-DAYS CHIRP', '------------------','DAILY TAMSAT','DAILY CHIRPS'), textvariable=fileSource,width=24)
	infobulle(cb.period,'Choose the data source')
	status.bar.display(cb.period,TextOutputVar,'Choose the data source')
	tkgrid(cb.period)

	#####################
	fr.B2<-tkframe(frA,relief='sunken',borderwidth=2)
	tkgrid(fr.B2,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	infobulle(fr.B2,'Start and end date for the merging')
	status.bar.display(fr.B2,TextOutputVar,'Start and end date for the merging')

	deb.txt<-tklabel(fr.B2,text='Start date',anchor='e',justify='right')
	fin.txt<-tklabel(fr.B2,text='End date',anchor='e',justify='right')
	yrs.txt<-tklabel(fr.B2,text='Year')
	mon.txt<-tklabel(fr.B2,text='Month')
	daytext<-tclVar('Dek')
	day.txt<-tklabel(fr.B2,text=tclvalue(daytext),textvariable=daytext)

	istart.yrs<-tclVar('1983')
	istart.mon<-tclVar('1')
	istart.day<-tclVar('1')
	iend.yrs<-tclVar('2014')
	iend.mon<-tclVar('12')
	iend.day<-tclVar('3')

	tkbind(cb.period,"<<ComboboxSelected>>",function(){
	if(tclvalue(fileSource)=='10-DAYS TAMSAT' | tclvalue(fileSource)=='10-DAYS CHIRP'){
		tclvalue(daytext)<-'Dek'
		tclvalue(iend.day)<-'3'
	}else{
		tclvalue(daytext)<-'Day'
		tclvalue(iend.day)<-'31'
	}
	})
	yrs1.v<-tkentry(fr.B2, width=4,textvariable=istart.yrs,justify = "right")
	mon1.v<-tkentry(fr.B2, width=4,textvariable=istart.mon,justify = "right")
	day1.v<-tkentry(fr.B2, width=4,textvariable=istart.day,justify = "right")
	yrs2.v<-tkentry(fr.B2, width=4,textvariable=iend.yrs,justify = "right")
	mon2.v<-tkentry(fr.B2, width=4,textvariable=iend.mon,justify = "right")
	day2.v<-tkentry(fr.B2, width=4,textvariable=iend.day,justify = "right")

	tkgrid(deb.txt,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(fin.txt,row=2,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(yrs.txt,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(mon.txt,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(day.txt,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(yrs1.v,row=1,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(mon1.v,row=1,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(day1.v,row=1,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(yrs2.v,row=2,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(mon2.v,row=2,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(day2.v,row=2,column=3,sticky='ew',padx=1,pady=1)

	bt.prm.OK<-tkbutton(frA, text=" Download ")
	tkgrid(bt.prm.OK,row=2,column=0,sticky='w',padx=25,pady=1,ipadx=1,ipady=1)
	tkconfigure(bt.prm.OK,command=function(){
		istart.yrs0<-as.numeric(tclvalue(istart.yrs))
		istart.mon0<-as.numeric(tclvalue(istart.mon))
		istart.day0<-as.numeric(tclvalue(istart.day))
		iend.yrs0<-as.numeric(tclvalue(iend.yrs))
		iend.mon0<-as.numeric(tclvalue(iend.mon))
		iend.day0<-as.numeric(tclvalue(iend.day))
		istart<-paste(istart.yrs0,istart.mon0,istart.day0,sep='-')
		iend<-paste(iend.yrs0,iend.mon0,iend.day0,sep='-')
		minlon<-as.numeric(tclvalue(minLon))
		maxlon<-as.numeric(tclvalue(maxLon))
		minlat<-as.numeric(tclvalue(minLat))
		maxlat<-as.numeric(tclvalue(maxLat))
		outdir<-tclvalue(file.save1)
		datasrc<-tclvalue(fileSource)
		if(outdir=="" | outdir=="NA"){
			tkmessageBox(message="Browse or enter directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			if(testConnection()){
				InsertMessagesTxt(main.txt.out,"Downloading.................")
				return(ExecDownload_SatData(datasrc,istart,iend,minlon,maxlon,minlat,maxlat,outdir))
			}else{
				InsertMessagesTxt(main.txt.out,'No internet connection',format=TRUE)
				return(NULL)
			}
		}
	})

	##################
	frGrd0<-tkframe(frB,relief='sunken',borderwidth=2)
	tkgrid(frGrd0,row=0,column=1,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	fr_grd<-ttklabelframe(frGrd0,text="Area of interest",relief="groove",borderwidth=2)
	tkgrid(fr_grd,sticky='we')
	grd_llon<-tklabel(fr_grd, text="Longitude",anchor='e',justify='right')
	grd_llat<-tklabel(fr_grd, text="Latitude",anchor='e',justify='right')
	grd_lb1<-tklabel(fr_grd, text="Minimum")
	grd_lb2<-tklabel(fr_grd, text="Maximum")

	grd_vlon1<-tkentry.h(fr_grd,TextOutputVar,'Minimum longitude in degree','Minimum longitude in degree')
	grd_vlon2<-tkentry.h(fr_grd,TextOutputVar,'Maximum longitude in degree','Maximum longitude in degree')
	grd_vlat1<-tkentry.h(fr_grd,TextOutputVar,'Minimum latitude in degree','Minimum latitude in degree')
	grd_vlat2<-tkentry.h(fr_grd,TextOutputVar,'Maximum latitude in degree','Maximum latitude in degree')

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
	########
	fr.A1<-tkframe(frB,relief='sunken',borderwidth=2)
	tkgrid(fr.A1,row=1,column=1,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	fr.A11<-tkframe(fr.A1)
	fr.A12<-tkframe(fr.A1)
	tkgrid(fr.A11,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A12,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frA11.txt<-tklabel(fr.A11,text='Directory to save downloaded files')
	tkgrid(frA11.txt)

	file.save1 <-tclVar('')
	en.file.save<-tkentry(fr.A12,textvariable=file.save1,width=largeur)
	infobulle(en.file.save,'Enter the full path to\ndirectory to save downloaded files')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path to directory to save downloaded files')
	bt.file.save<-tkbutton.h(fr.A12, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(getwd(), "")
			if(is.na(file2save1)) tclvalue(file.save1)<-getwd()
			else{
				dir.create(file2save1,showWarnings=FALSE,recursive=TRUE)
				tclvalue(file.save1)<-file2save1
			}
	})

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Download Satellite Rainfall Estimate data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
		return(NULL)
	})
	tkwait.window(tt)
}


###################
ExecDownload_SatData<-function(datasrc,istart,iend,minlon,maxlon,minlat,maxlat,outdir){

	tkconfigure(main.win,cursor='watch');tcl('update')
	ret<-downloadRFE_fun(datasrc,istart,iend,minlon,maxlon,minlat,maxlat,outdir)
	tkconfigure(main.win,cursor='')
	if(!is.null(ret)){
		if(ret==0) InsertMessagesTxt(main.txt.out,"Download Done!")
		else if(ret==-1) InsertMessagesTxt(main.txt.out,"Some files could not be downloaded",format=TRUE)
		else InsertMessagesTxt(main.txt.out,"Download Failed!",format=TRUE)
	}else InsertMessagesTxt(main.txt.out,"Download Failed!",format=TRUE)
}

##########
downloadRFE_fun<-function(datasrc,istart,iend,minlon,maxlon,minlat,maxlat,outdir){
	outRet<-0
	if(datasrc=='10-DAYS TAMSAT'){
		#url<-'http://www.met.reading.ac.uk/~tamsat/public_data'
		url<-'http://tamsat.org.uk/public_data'
		outdir0<-file.path(outdir,'Dekad_TAMSAT_Africa',fsep = .Platform$file.sep)
		if(!file.exists(outdir0)) dir.create(outdir0,showWarnings=FALSE,recursive=TRUE)
		outdir1<-file.path(outdir,'Dekad_TAMSAT_Extracted',fsep = .Platform$file.sep)
		if(!file.exists(outdir1)) dir.create(outdir1,showWarnings=FALSE,recursive=TRUE)

		deb<-try(as.Date(istart),silent=TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out,'Check the start date',format=TRUE)
			return(NULL)
		}
		deb<-strsplit(as.character(deb),'-')
		year1<-deb[[1]][1]
		mon1<-deb[[1]][2]
		dek1<-as.numeric(deb[[1]][3])
		if(dek1>3){
			InsertMessagesTxt(main.txt.out,'Dekad must be between 1 and 3',format=TRUE)
			return(NULL)
		}
		fin<-try(as.Date(iend),silent=TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out,'Check the end date',format=TRUE)
			return(NULL)
		}
		fin<-strsplit(as.character(fin),'-')
		year2<-fin[[1]][1]
		mon2<-fin[[1]][2]
		dek2<-as.numeric(fin[[1]][3])
		if(dek2>3){
			InsertMessagesTxt(main.txt.out,'Dekad must be between 1 and 3',format=TRUE)
			return(NULL)
		}

		dates<-seq(as.Date(paste(year1,mon1,dek1,sep='/')),as.Date(paste(year2,mon2,dek2,sep='/')),'day')
		dates<-format(dates[as.numeric(format(dates,'%d'))<=3],'%Y-%m-%d')

		tcl("update","idletasks")
		for(fl in dates){
			daty<-strsplit(as.character(fl),'-')
			year<-daty[[1]][1]
			mon<-daty[[1]][2]
			dek<-as.numeric(daty[[1]][3])
			file0<-paste('rfe',year,'_',mon,'-dk',dek,'.nc',sep='')
			link<-paste(url,year,mon,file0,sep='/')
			destfile0<-file.path(outdir0,file0,fsep = .Platform$file.sep)
			test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
			if(inherits(test, "try-error")){
				InsertMessagesTxt(main.txt.out,paste('Cannot open the connection or file does not exist:',file0),format=TRUE)
				outRet<- -1
				next
			}else{
				ret<-try(download.file(link,destfile0,mode="wb",quiet=TRUE),silent=TRUE)
				if(ret!=0){
					InsertMessagesTxt(main.txt.out,paste('Échec du téléchargement pour:',file0),format=TRUE)
					outRet<- -1
					next
				}else{
					InsertMessagesTxt(main.txt.out,paste('Téléchargement pour:',file0,'terminé'))
					nc<-nc_open(destfile0)
					xm<-nc$dim[[2]]$vals
					ym<-nc$dim[[1]]$vals
					xdat<-ncvar_get(nc,varid=nc$var[[1]]$name)
					nc_close(nc)
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
					dx <- ncdim_def("Lon", "degreeE", xm)
					dy <- ncdim_def("Lat", "degreeN", ym)
					rfeout <- ncvar_def('rfe', "mm", list(dx,dy), -99,longname= "TAMSAT 10-days rainfall estimate", prec="short")
					outfl<-file.path(outdir1,file0,fsep = .Platform$file.sep)
					nc2 <- nc_create(outfl,rfeout)
					ncvar_put(nc2,rfeout,xdat)
					nc_close(nc2)
					InsertMessagesTxt(main.txt.out,paste('Extraction de:',file0,'sur', paste('bbox',minlon,minlat,maxlon,maxlat,sep=':'),'terminée'))
				}
			}
			tcl("update")
		}
	}
	#####################################################

	if(datasrc=='DAILY TAMSAT'){
		# url<-'http://www.met.reading.ac.uk/~tamsat/public_data'
		url<-'http://tamsat.org.uk/public_data'
		outdir0<-file.path(outdir,'Daily_TAMSAT_Africa',fsep = .Platform$file.sep)
		if(!file.exists(outdir0)) dir.create(outdir0,showWarnings=FALSE,recursive=TRUE)
		outdir1<-file.path(outdir,'Daily_TAMSAT_Extracted',fsep = .Platform$file.sep)
		if(!file.exists(outdir1)) dir.create(outdir1,showWarnings=FALSE,recursive=TRUE)

		deb<-try(as.Date(istart),silent=TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out,'Check the start date',format=TRUE)
			return(NULL)
		}
		deb<-strsplit(as.character(deb),'-')
		year1<-deb[[1]][1]
		mon1<-deb[[1]][2]
		day1<-deb[[1]][3]

		fin<-try(as.Date(iend),silent=TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out,'Check the end date',format=TRUE)
			return(NULL)
		}
		fin<-strsplit(as.character(fin),'-')
		year2<-fin[[1]][1]
		mon2<-fin[[1]][2]
		day2<-fin[[1]][3]

		dates<-as.character(seq(as.Date(paste(year1,mon1,day1,sep='-')),as.Date(paste(year2,mon2,day2,sep='-')),'day'))

		tcl("update","idletasks")
		for(fl in dates){
			daty<-strsplit(as.character(fl),'-')
			year<-daty[[1]][1]
			mon<-daty[[1]][2]
			day<-daty[[1]][3]
			file0<-paste('rfe',year,'_',mon,'_',day,'.nc',sep='')
			link<-paste(url,year,mon,file0,sep='/')
			destfile0<-file.path(outdir0,file0,fsep = .Platform$file.sep)
			test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
			if(inherits(test, "try-error")){
				InsertMessagesTxt(main.txt.out,paste('Cannot open the connection or file does not exist:',file0),format=TRUE)
				outRet<- -1
				next
			}else{
				ret<-try(download.file(link,destfile0,mode="wb",quiet=TRUE),silent=TRUE)
				if(ret!=0){
					InsertMessagesTxt(main.txt.out,paste('Échec du téléchargement pour:',file0),format=TRUE)
					outRet<- -1
					next
				}else{
					InsertMessagesTxt(main.txt.out,paste('Téléchargement pour:',file0,'terminé'))
					nc<-nc_open(destfile0)
					xm<-nc$dim[[2]]$vals
					ym<-nc$dim[[1]]$vals
					xdat<-ncvar_get(nc,varid=nc$var[[1]]$name)
					nc_close(nc)
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
					dx <- ncdim_def("Lon", "degreeE", xm)
					dy <- ncdim_def("Lat", "degreeN", ym)
					rfeout <- ncvar_def('rfe', "mm", list(dx,dy), -99,longname= "TAMSAT daily rainfall estimate", prec="short")
					outfl<-file.path(outdir1,file0,fsep = .Platform$file.sep)
					nc2 <- nc_create(outfl,rfeout)
					ncvar_put(nc2,rfeout,xdat)
					nc_close(nc2)
					InsertMessagesTxt(main.txt.out,paste('Extraction de:',file0,'sur', paste('bbox',minlon,minlat,maxlon,maxlat,sep=':'),'terminée'))
				}
			}
			tcl("update")
		}
	}

	#####################################################

	if(datasrc=='10-DAYS CHIRP'){
		url<-'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.dekad/.prcp'
		mois<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
		if(!file.exists(outdir)) dir.create(outdir,showWarnings=FALSE,recursive=TRUE)

		area<-paste('X',minlon,maxlon,'RANGEEDGES','Y',minlat,maxlat,'RANGEEDGES',sep='/')

		deb<-try(as.Date(istart),silent=TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out,'Check the start date',format=TRUE)
			return(NULL)
		}
		dek1<-as.numeric(strsplit(as.character(deb),'-')[[1]][3])
		if(dek1>3){
			InsertMessagesTxt(main.txt.out,'Dekad must be between 1 and 3',format=TRUE)
			return(NULL)
		}
		fin<-try(as.Date(iend),silent=TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out,'Check the end date',format=TRUE)
			return(NULL)
		}
		dek2<-as.numeric(strsplit(as.character(fin),'-')[[1]][3])
		if(dek2>3){
			InsertMessagesTxt(main.txt.out,'Dekad must be between 1 and 3',format=TRUE)
			return(NULL)
		}

		dates<-seq(deb,fin,'day')
		dates<-do.call('rbind',strsplit(as.character(dates[which(as.numeric(format(dates,'%d'))<=3)]),'-'))
		dates<-dates[,c(1:3,2:3)]
		if(is.null(dim(dates))) dates<-matrix(dates,ncol=5)
		dates[,4]<-mois[as.numeric(dates[,2])]
		dates[,3]<-ifelse(dates[,3]=='01','1-10',ifelse(dates[,3]=='02','11-20','03'))
		if(length(which(dates[,3]=='03'))>0){
			endmois<-apply(matrix(dates[dates[,3]=='03',1:2],ncol=2),1,function(x) rev((28:31)[which(!is.na(as.Date(paste(x[1],x[2],28:31,sep='-'))))])[1])
			dates[dates[,3]=='03',3]<-paste(21,endmois,sep='-')
		}

		tcl("update","idletasks")
		for(j in 1:nrow(dates)){
			time<-paste('T/%28',dates[j,3],'%20',dates[j,4],'%20',dates[j,1],'%29/VALUES',sep='')
			link<-paste(url,area,time,'data.nc',sep='/')
			fileout<-paste('chirp_',dates[j,1],dates[j,2],as.numeric(dates[j,5]),'.nc',sep='')
			destfile<-file.path(outdir,fileout,fsep = .Platform$file.sep)
			ret<-try(download.file(link,destfile,mode="wb",quiet=TRUE),silent=TRUE)
			if(ret!=0){
				InsertMessagesTxt(main.txt.out,paste('Échec du téléchargement pour:',fileout),format=TRUE)
				outRet<- -1
				next
			}else{
				InsertMessagesTxt(main.txt.out,paste('Extraction de:',fileout,'sur',paste('bbox',minlon,minlat,maxlon,maxlat,sep=':'),'terminée'))
			}
			tcl("update")
		}
	}

	#####################################################

	if(datasrc=='DAILY CHIRPS'){
		url<-'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily/.global/.0p05/.prcp'
		mois<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
		if(!file.exists(outdir)) dir.create(outdir,showWarnings=FALSE,recursive=TRUE)

		area<-paste('X',minlon,maxlon,'RANGEEDGES','Y',minlat,maxlat,'RANGEEDGES',sep='/')

		deb<-try(as.Date(istart),silent=TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out,'Check the start date',format=TRUE)
			return(NULL)
		}
		fin<-try(as.Date(iend),silent=TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out,'Check the end date',format=TRUE)
			return(NULL)
		}

		dates<-seq(deb,fin,'day')
		dates<-do.call('rbind',strsplit(as.character(dates),'-'))

		dates<-dates[,c(1:3,2)]
		if(is.null(dim(dates))) dates<-matrix(dates,ncol=4)
		dates[,4]<-mois[as.numeric(dates[,2])]

		tcl("update","idletasks")
		for(j in 1:nrow(dates)){
			time<-paste('T/%28',as.numeric(dates[j,3]),'%20',dates[j,4],'%20',dates[j,1],'%29/VALUES',sep='')
			link<-paste(url,area,time,'data.nc',sep='/')
			fileout<-paste('chirps_',dates[j,1],dates[j,2],dates[j,3],'.nc',sep='')
			destfile<-file.path(outdir,fileout,fsep = .Platform$file.sep)
			ret<-try(download.file(link,destfile,mode="wb",quiet=TRUE),silent=TRUE)
			if(ret!=0){
				InsertMessagesTxt(main.txt.out,paste('Échec du téléchargement pour:',fileout),format=TRUE)
				outRet<- -1
				next
			}else{
				InsertMessagesTxt(main.txt.out,paste('Extraction de:',fileout,'sur',
				paste('bbox',minlon,minlat,maxlon,maxlat,sep=':'),'terminée'))
			}
			tcl("update")
		}
	}

	return(outRet)
}

