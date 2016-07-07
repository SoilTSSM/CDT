coefBiasGetInfoRain<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur<-23
	else largeur<-21
	##tkentry width, rfe filename format
	if (Sys.info()["sysname"] == "Windows") largeur1<-23
	else largeur1<-21

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0<-tkframe(tt,relief='raised',borderwidth=2)
	frMRG1<-tkframe(tt)

	fr.A<-tkframe(frMRG0,relief="groove",borderwidth=2)
	fr.C<-tkframe(frMRG0,relief="groove",borderwidth=2)
	tkgrid(fr.A,fr.C)
	tkgrid.configure(fr.A,row=0,column=0,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)
	tkgrid.configure(fr.C,row=0,column=1,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)

	pr.relief.set<-c('sunken','sunken','sunken','sunken')
	for(i in 0:3) assign(paste('fr.A',i,sep=''),tkframe(fr.A,relief=pr.relief.set[i+1],borderwidth=2))
	for(i in 0:3) tkgrid(get(paste('fr.A',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	###################
	fr.A00<-tkframe(fr.A0)
	#fr.A01<-tkframe(fr.A0)
	tkgrid(fr.A00,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#tkgrid(fr.A01,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period)=='daily',
	'Daily data',ifelse(as.character(GeneralParameters$period)=='dekadal','Dekadal data','Monthly data'))

	cb.period<-ttkcombobox(fr.A00, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)
	infobulle(cb.period,'Choose the time step of the data')
	status.bar.display(cb.period,TextOutputVar,'Choose the time step of the data')
	tkgrid(cb.period)

	###################
	for(i in 0:5) assign(paste('fr.A1',i,sep=''),tkframe(fr.A1))
	for(i in 0:5) tkgrid(get(paste('fr.A1',i,sep='')))
	for(i in 0:5) tkgrid.configure(get(paste('fr.A1',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	###
	frA10.txt<-tklabel(fr.A10,text='Station data file')
	tkgrid(frA10.txt)

	###
	cb.stnfl<-ttkcombobox(fr.A11, values=unlist(listOpenFiles), textvariable=file.stnfl)
	infobulle(cb.stnfl,'Choose the file in the list')
	status.bar.display(cb.stnfl,TextOutputVar,'Choose the file containing the gauge data')

	bt.stnfl<-tkbutton.h(fr.A11, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.stnfl,bt.stnfl)
	tkgrid.configure(cb.stnfl,row=0,column=0,sticky='w')
	tkgrid.configure(bt.stnfl,row=0,column=1,sticky='e')
	tkconfigure(bt.stnfl,command=function(){
		dat.opfiles<-getOpenFiles(tt,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
		}else{
			return(NULL)
		}
	})

	###
	frA12.txt<-tklabel(fr.A12,text='Directory of RFE files')
	tkgrid(frA12.txt)

	dir.rfe <-tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.dir.rfe<-tkentry(fr.A13,textvariable=dir.rfe,width=largeur)
	infobulle(en.dir.rfe,'Enter the full path to\ndirectory containing the RFE files')
	status.bar.display(en.dir.rfe,TextOutputVar,'Enter the full path to directory containing the RFE files')
	bt.dir.rfe<-tkbutton.h(fr.A13, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.dir.rfe,bt.dir.rfe)
	tkgrid.configure(en.dir.rfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.dir.rfe,row=0,column=1,sticky='e')
	tkconfigure(bt.dir.rfe,command=function(){
		dir4rfe<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe)<-""
		else tclvalue(dir.rfe)<-dir4rfe
	})
	#####
	file.grdrfe <- tclVar()
	tclvalue(file.grdrfe) <- as.character(GeneralParameters$file.io$Values[3])

	frA14.txt<-tklabel(fr.A14,text="RFE's sample file")
	tkgrid(frA14.txt)

	###from RFE
	cb.grdrfe<-ttkcombobox(fr.A15, values=unlist(listOpenFiles), textvariable=file.grdrfe)
	infobulle(cb.grdrfe,'Choose the file in the list')
	status.bar.display(cb.grdrfe,TextOutputVar,'File containing a sample of RFE data in netcdf')
	bt.grdrfe<-tkbutton.h(fr.A15, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grdrfe,bt.grdrfe)
	tkgrid.configure(cb.grdrfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grdrfe,row=0,column=1,sticky='e')
	tkconfigure(bt.grdrfe,command=function(){
		fileopen<-tclvalue(tkgetOpenFile(initialdir=tclvalue(dir.rfe),initialfile = "",
		filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen=="" | is.na(fileopen)) return(NULL)
		nc.opfiles1<-preview.data.nc(tt,fileopen,"")
		nc.opfiles<-list(basename(fileopen),nc.opfiles1,fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles,"end",basename(fileopen))
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
		}else{
			return(NULL)
		}
	})

	##########################
	fr.A20<-tkframe(fr.A2)
	fr.A21<-tkframe(fr.A2)
	tkgrid(fr.A20,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A21,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	###################
	if(as.character(GeneralParameters$CreateGrd)=='2') statedem<-'normal'
	else statedem<-'disabled'

	#####
	frA20.txt<-tklabel(fr.A20,text="Elevation data(NetCDF)")
	tkgrid(frA20.txt)

	###from DEM
	cb.grddem<-ttkcombobox(fr.A21, values=unlist(listOpenFiles), textvariable=file.grddem,state=statedem)
	infobulle(cb.grddem,'Choose the file in the list')
	status.bar.display(cb.grddem,TextOutputVar,'File containing the elevation data in netcdf')
	bt.grddem<-tkbutton.h(fr.A21, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grddem,bt.grddem)
	tkgrid.configure(cb.grddem,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grddem,row=0,column=1,sticky='e')
	tkconfigure(bt.grddem,state=statedem,command=function(){
		nc.opfiles<-getOpenNetcdf(tt,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
		}else{
			return(NULL)
		}
	})

	#######
	fr.A30<-tkframe(fr.A3)
	fr.A31<-tkframe(fr.A3)
	tkgrid(fr.A30,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A31,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frA30.txt<-tklabel(fr.A30,text='Directory to save result')
	tkgrid(frA30.txt)

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[5]))
	en.file.save<-tkentry(fr.A31,textvariable=file.save1,width=largeur)
	infobulle(en.file.save,'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path to directory to save result')
	bt.file.save<-tkbutton.h(fr.A31, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[5]), "")
			if(is.na(file2save1)) tclvalue(file.save1)<-as.character(GeneralParameters$file.io$Values[5])
			else{
				dir.create(file2save1,showWarnings=FALSE,recursive=TRUE)
				tclvalue(file.save1)<-file2save1
			}
	})

	#######################

	pr.relief.set2<-c('sunken','sunken','sunken','sunken','flat')
	for(i in 0:3) assign(paste('fr.C',i,sep=''),tkframe(fr.C,relief=pr.relief.set2[i+1],borderwidth=2))
	for(i in 0:3) tkgrid(get(paste('fr.C',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#####

	for(i in 0:3) assign(paste('fr.C0',i,sep=''),tkframe(fr.C0))
	for(i in 0:3) tkgrid(get(paste('fr.C0',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C0',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=0,ipadx=1,ipady=0)

	frC00.txt<-tklabel(fr.C00,text='Input RFE file format')
	tkgrid(frC00.txt)

	inrfeff <-tclVar(as.character(GeneralParameters$prefix$Values[1]))
	en.inrfeff<-tkentry(fr.C01,textvariable=inrfeff,width=largeur1)
	infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
	status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
	tkgrid(en.inrfeff,row=0,column=0,sticky='w')

	####
	frC02.txt<-tklabel(fr.C02,text='Mean bias filename prefix')
	tkgrid(frC02.txt)

	outmrgff <-tclVar(as.character(GeneralParameters$prefix$Values[2]))
	en.outmrgff<-tkentry(fr.C03,textvariable=outmrgff,width=largeur1)
	infobulle(en.outmrgff,'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.outmrgff,TextOutputVar,'Prefix for the file name of the mean bias coefficient')
	tkgrid(en.outmrgff,row=0,column=0,sticky='w')

	#####
	tkbind(cb.period,"<<ComboboxSelected>>",function(){
		if(tclvalue(file.period)=='Daily data'){
			tclvalue(inrfeff)<-"rfe%s_%s_%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01_01.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01_01.nc')
		}
		if(tclvalue(file.period)=='Dekadal data'){
			tclvalue(inrfeff)<-"rfe%s_%s-dk%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
		}
		if(tclvalue(file.period)=='Monthly data'){
			tclvalue(inrfeff)<-"rfe%s_%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01.nc')
		}
	})

	########################
	for(i in 0:3) assign(paste('fr.C1',i,sep=''),tkframe(fr.C1))
	for(i in 0:3) tkgrid(get(paste('fr.C1',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C1',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=0,ipadx=1,ipady=0)

	infobulle(fr.C1,'Create the grid to interpolate the merged data')
	status.bar.display(fr.C1,TextOutputVar,'Create the grid to interpolate the merged data')

	frC10.txt<-tklabel(fr.C10,text='Create grid for interpolation')
	tkgrid(frC10.txt)

	varCreateGrd <- tclVar(as.character(GeneralParameters$CreateGrd))
	grdRFE.rbt <- tkradiobutton(fr.C11,text="From RFE",anchor='w',justify='left')
	tkgrid(grdRFE.rbt)
	grdDEM.rbt <- tkradiobutton(fr.C12,text="From DEM",anchor='w',justify='left')
	tkgrid(grdDEM.rbt)
	grdNEW.rbt <- tkradiobutton(fr.C13,text="New Grid",anchor='w',justify='left')

	tkconfigure(grdRFE.rbt,variable=varCreateGrd,value="1")
	tkconfigure(grdDEM.rbt,variable=varCreateGrd,value="2")
	tkconfigure(grdNEW.rbt,variable=varCreateGrd,value="3")

	if(as.character(GeneralParameters$CreateGrd)=='3') stategrd<-'normal'
	else stategrd<-'disabled'

	bt.getNewgrid<-tkbutton.h(fr.C13, text="Create",TextOutputVar,'Set the new grid','Set the new grid')
	tkconfigure(bt.getNewgrid,state=stategrd,command=function(){
		GeneralParameters<<-getParamNewGrid(tt,GeneralParameters)
	})
	tkgrid(grdNEW.rbt,row=0,column=0)
	tkgrid(bt.getNewgrid,row=0,column=1,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	###########
	tkbind(grdRFE.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='disabled')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.grddem,state='disabled')
	})
	tkbind(grdDEM.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='disabled')
			tkconfigure(bt.grddem,state='normal')
			tkconfigure(cb.grddem,state='normal')
	})
	tkbind(grdNEW.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='normal')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.grddem,state='disabled')
	})

	###############################
	years1.l<-tklabel.h(fr.C2,'StartYear',TextOutputVar,'Start year to be used to compute mean Gauge-RFE bias',
	'Start year to be used to compute mean Gauge-RFE bias')
	years2.l<-tklabel.h(fr.C2,'EndYear',TextOutputVar,'End year to be used to compute mean Gauge-RFE bias',
	'End year to be used to compute mean Gauge-RFE bias')

	years1.v<-tkentry.h(fr.C2,TextOutputVar,'Start year to be used to compute mean Gauge-RFE bias','Start year to be used to compute mean Gauge-RFE bias')
	years2.v<-tkentry.h(fr.C2,TextOutputVar,'End year to be used to compute mean Gauge-RFE bias','End year to be used to compute mean Gauge-RFE bias')

	tkgrid(years1.l,row=0,column=0,padx=5,pady=4)
	tkgrid(years1.v,row=0,column=1,padx=5,pady=4)
	tkgrid(years2.l,row=1,column=0,padx=5,pady=4)
	tkgrid(years2.v,row=1,column=1,padx=5,pady=4)

	tkconfigure(years1.l,anchor='e',justify='right')
	tkconfigure(years2.l,anchor='e',justify='right')

	year1<-tclVar(as.character(GeneralParameters$dates.coef$Values[1]))
	year2<-tclVar(as.character(GeneralParameters$dates.coef$Values[2]))

	tkconfigure(years1.v,width=8,textvariable=year1,justify='right')
	tkconfigure(years2.v,width=8,textvariable=year2,justify='right')

	###################
	min.nbrs.l<-tklabel.h(fr.C3,'MinStn',TextOutputVar,
	'Minimum number of neighbours to be used to interpolate the bias',
	'Minimum number of neighbours to be used to interpolate the bias')
	max.nbrs.l<-tklabel.h(fr.C3,'MaxStn',TextOutputVar,
	'Maximum number of neighbours to be used to interpolate the bias',
	'Maximum number of neighbours to be used to interpolate the bias')
	max.dst.l<-tklabel.h(fr.C3,'MaxDist',TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the bias',
	'Maximum distance (in  decimal degree) to be used to interpolate the bias')

	min.nbrs.v<-tkentry.h(fr.C3,TextOutputVar,
	'Minimum number of neighbours to be used to interpolate the bias',
	'Minimum number of neighbours to be used to interpolate the bias')
	max.nbrs.v<-tkentry.h(fr.C3,TextOutputVar,
	'Maximum number of neighbours to be used to interpolate the bias',
	'Maximum number of neighbours to be used to interpolate the bias')
	max.dst.v<-tkentry.h(fr.C3,TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the bias',
	'Maximum distance (in  decimal degree) to be used to interpolate the bias')

	tkgrid(min.nbrs.l,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(min.nbrs.v,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(max.nbrs.l,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(max.nbrs.v,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(max.dst.l,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(max.dst.v,row=1,column=1,sticky='ew',padx=1,pady=1)

	tkconfigure(min.nbrs.l,anchor='e',justify='right')
	tkconfigure(max.nbrs.l,anchor='e',justify='right')
	tkconfigure(max.dst.l,anchor='e',justify='right')

	min.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[1]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[2]))
	max.dst <- tclVar(as.character(GeneralParameters$params.int$Values[3]))

	tkconfigure(min.nbrs.v,width=4,textvariable=min.nbrs,justify='right')
	tkconfigure(max.nbrs.v,width=4,textvariable=max.nbrs,justify='right')
	tkconfigure(max.dst.v,width=4,textvariable=max.dst,justify='right')

	###############

	bt.prm.OK<-tkbutton(frMRG1, text=" OK ")
	bt.prm.CA<-tkbutton(frMRG1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=10,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){

		if(tclvalue(file.stnfl)==""){
			tkmessageBox(message="Choose the file containing the gauge data",icon="warning",type="ok")
			#tkwait.window(tt)
		}else if(tclvalue(dir.rfe)=="" | tclvalue(dir.rfe)=="NA"){
			tkmessageBox(message="Choose or enter the  directory containing the RFE files",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grdrfe)==""){
			tkmessageBox(message="You have to provide a RFE's sample file",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(varCreateGrd)=='2'  & tclvalue(file.grddem)=="" ){
			tkmessageBox(message="You have to choose DEM data in NetCDF format",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Choose or enter the path to directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period<<-ifelse(tclvalue(file.period)=='Daily data','daily',
			ifelse(tclvalue(file.period)=='Dekadal data','dekadal','monthly'))
			GeneralParameters$file.io$Values<<-c(tclvalue(file.stnfl),tclvalue(file.grddem),
			tclvalue(file.grdrfe),tclvalue(dir.rfe),tclvalue(file.save1))
			GeneralParameters$CreateGrd<<-tclvalue(varCreateGrd)
			GeneralParameters$prefix$Values<<-c(tclvalue(inrfeff),tclvalue(outmrgff))
			GeneralParameters$dates.coef$Values<<-c(tclvalue(year1),tclvalue(year2))
			GeneralParameters$params.int$Values<<-c(tclvalue(min.nbrs),tclvalue(max.nbrs),tclvalue(max.dst))
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frMRG0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frMRG1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Mean Bias computation - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

##############################################################################################

rmvBiasGetInfoRain<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()
	#tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur<-23
	else largeur<-21
	##tkentry width, filename format, prefix
	if (Sys.info()["sysname"] == "Windows") largeur1<-23
	else largeur1<-21

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0<-tkframe(tt,relief='raised',borderwidth=2)
	frMRG1<-tkframe(tt)

	fr.A<-tkframe(frMRG0,relief="groove",borderwidth=2)
	fr.C<-tkframe(frMRG0,relief="groove",borderwidth=2)
	tkgrid(fr.A,fr.C)
	tkgrid.configure(fr.A,row=0,column=0,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)
	tkgrid.configure(fr.C,row=0,column=1,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)

	pr.relief.set<-c('sunken','sunken','sunken','sunken')
	for(i in 0:3) assign(paste('fr.A',i,sep=''),tkframe(fr.A,relief=pr.relief.set[i+1],borderwidth=2))
	for(i in 0:3) tkgrid(get(paste('fr.A',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	###################
	fr.A00<-tkframe(fr.A0)
	#fr.A01<-tkframe(fr.A0)
	tkgrid(fr.A00,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#tkgrid(fr.A01,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period)=='daily',
	'Daily data',ifelse(as.character(GeneralParameters$period)=='dekadal','Dekadal data','Monthly data'))

	cb.period<-ttkcombobox(fr.A00, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)
	infobulle(cb.period,'Choose the time step of the data')
	status.bar.display(cb.period,TextOutputVar,'Choose the time step of the data')
	tkgrid(cb.period)

	###################
	for(i in 0:3) assign(paste('fr.A1',i,sep=''),tkframe(fr.A1))
	for(i in 0:3) tkgrid(get(paste('fr.A1',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A1',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	###
	frA10.txt<-tklabel(fr.A10,text='Directory of RFE files')
	tkgrid(frA10.txt)

	dir.rfe <-tclVar(as.character(GeneralParameters$file.io$Values[2]))
	en.dir.rfe<-tkentry(fr.A11,textvariable=dir.rfe,width=largeur)
	infobulle(en.dir.rfe,'Enter the full path to\ndirectory containing the RFE files')
	status.bar.display(en.dir.rfe,TextOutputVar,'Enter the full path to directory containing the RFE files')
	bt.dir.rfe<-tkbutton.h(fr.A11, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.dir.rfe,bt.dir.rfe)
	tkgrid.configure(en.dir.rfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.dir.rfe,row=0,column=1,sticky='e')
	tkconfigure(bt.dir.rfe,command=function(){
		dir4rfe<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[2]), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe)<-""
		else tclvalue(dir.rfe)<-dir4rfe
	})
	#####
	file.grdrfe <- tclVar()
	tclvalue(file.grdrfe) <- as.character(GeneralParameters$file.io$Values[1])

	frA12.txt<-tklabel(fr.A12,text="RFE's sample file")
	tkgrid(frA12.txt)

	###from RFE
	cb.grdrfe<-ttkcombobox(fr.A13, values=unlist(listOpenFiles), textvariable=file.grdrfe)
	infobulle(cb.grdrfe,'Choose the file in the list')
	status.bar.display(cb.grdrfe,TextOutputVar,'File containing a sample of RFE data in netcdf')
	bt.grdrfe<-tkbutton.h(fr.A13, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grdrfe,bt.grdrfe)
	tkgrid.configure(cb.grdrfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grdrfe,row=0,column=1,sticky='e')
	tkconfigure(bt.grdrfe,command=function(){
		fileopen<-tclvalue(tkgetOpenFile(initialdir=tclvalue(dir.rfe),initialfile = "",
		filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen=="" | is.na(fileopen)) return(NULL)
		nc.opfiles1<-preview.data.nc(tt,fileopen,"")
		nc.opfiles<-list(basename(fileopen),nc.opfiles1,fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles,"end",basename(fileopen))
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
		}else{
			return(NULL)
		}
	})

	###################
	fr.A20<-tkframe(fr.A2)
	fr.A21<-tkframe(fr.A2)
	tkgrid(fr.A20,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A21,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	#####
	frA20.txt<-tklabel(fr.A20,text="Directory of mean bias files")
	tkgrid(frA20.txt)

	##
	dir.bias <-tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.dir.bias<-tkentry(fr.A21,textvariable=dir.bias,width=largeur)
	infobulle(en.dir.bias,'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias,TextOutputVar,'Enter the full path to directory containing the mean bias files')
	bt.dir.bias<-tkbutton.h(fr.A21, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.dir.bias,bt.dir.bias)
	tkgrid.configure(en.dir.bias,row=0,column=0,sticky='w')
	tkgrid.configure(bt.dir.bias,row=0,column=1,sticky='e')
	tkconfigure(bt.dir.bias,command=function(){
		dir4bias<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(is.na(dir4bias)) tclvalue(dir.bias)<-""
		else tclvalue(dir.bias)<-dir4bias
	})

	#######
	fr.A30<-tkframe(fr.A3)
	fr.A31<-tkframe(fr.A3)
	tkgrid(fr.A30,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A31,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frA30.txt<-tklabel(fr.A30,text='Directory to save result')
	tkgrid(frA30.txt)

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save<-tkentry(fr.A31,textvariable=file.save1,width=largeur)
	infobulle(en.file.save,'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path to directory to save result')
	bt.file.save<-tkbutton.h(fr.A31, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
			if(is.na(file2save1)) tclvalue(file.save1)<-as.character(GeneralParameters$file.io$Values[4])
			else{
				dir.create(file2save1,showWarnings=FALSE,recursive=TRUE)
				tclvalue(file.save1)<-file2save1
			}
	})

	#################
	pr.relief.set2<-c('sunken','sunken','sunken','sunken','flat')
	for(i in 0:1) assign(paste('fr.C',i,sep=''),tkframe(fr.C,relief=pr.relief.set2[i+1],borderwidth=2))
	for(i in 0:1) tkgrid(get(paste('fr.C',i,sep='')))
	for(i in 0:1) tkgrid.configure(get(paste('fr.C',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#####

	for(i in 0:5) assign(paste('fr.C0',i,sep=''),tkframe(fr.C0))
	for(i in 0:5) tkgrid(get(paste('fr.C0',i,sep='')))
	for(i in 0:5) tkgrid.configure(get(paste('fr.C0',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=0,ipadx=1,ipady=0)

	frC00.txt<-tklabel(fr.C00,text='Input RFE file format')
	tkgrid(frC00.txt)

	inrfeff <-tclVar(as.character(GeneralParameters$prefix$Values[1]))
	en.inrfeff<-tkentry(fr.C01,textvariable=inrfeff,width=largeur1)
	infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
	status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
	tkgrid(en.inrfeff,row=0,column=0,sticky='w')

	####
	frC02.txt<-tklabel(fr.C02,text='Mean bias filename prefix')
	tkgrid(frC02.txt)

	outmrgff <-tclVar(as.character(GeneralParameters$prefix$Values[2]))
	en.outmrgff<-tkentry(fr.C03,textvariable=outmrgff,width=largeur1)
	infobulle(en.outmrgff,'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.outmrgff,TextOutputVar,'Prefix for the file name of the mean bias coefficient')
	tkgrid(en.outmrgff,row=0,column=0,sticky='w')

	####
	frC04.txt<-tklabel(fr.C04,text='Adjusted data filename prefix')
	tkgrid(frC04.txt)

	adjPrefix <-tclVar(as.character(GeneralParameters$prefix$Values[3]))
	en.adjPrefix<-tkentry(fr.C05,textvariable=adjPrefix,width=largeur1)
	infobulle(en.outmrgff,'Prefix for the file name of the adjusted RFE data')
	status.bar.display(en.adjPrefix,TextOutputVar,'Prefix for the file name of the adjusted RFE data')
	tkgrid(en.adjPrefix,row=0,column=0,sticky='w')

	########################
	infobulle(fr.C1,'Start and end date for adjusting RFE data')
	status.bar.display(fr.C1,TextOutputVar,'Start and end date for adjusting RFE data')

	deb.txt<-tklabel(fr.C1,text='Start date',anchor='e',justify='right')
	fin.txt<-tklabel(fr.C1,text='End date',anchor='e',justify='right')
	yrs.txt<-tklabel(fr.C1,text='Year')
	mon.txt<-tklabel(fr.C1,text='Month')
	if(as.character(GeneralParameters$period)=='dekadal') day.txtVar<-tclVar('Dek')
	else day.txtVar<-tclVar('Day')
	day.txt<-tklabel(fr.C1,text=tclvalue(day.txtVar),textvariable=day.txtVar)

	istart.yrs<-tclVar(as.character(GeneralParameters$dates.adj$Values[1]))
	istart.mon<-tclVar(as.character(GeneralParameters$dates.adj$Values[2]))
	istart.day<-tclVar(as.character(GeneralParameters$dates.adj$Values[3]))
	iend.yrs<-tclVar(as.character(GeneralParameters$dates.adj$Values[4]))
	iend.mon<-tclVar(as.character(GeneralParameters$dates.adj$Values[5]))
	iend.day<-tclVar(as.character(GeneralParameters$dates.adj$Values[6]))

	yrs1.v<-tkentry(fr.C1, width=4,textvariable=istart.yrs,justify = "right")
	mon1.v<-tkentry(fr.C1, width=4,textvariable=istart.mon,justify = "right")
	day1.v<-tkentry(fr.C1, width=4,textvariable=istart.day,justify = "right",state='normal')
	yrs2.v<-tkentry(fr.C1, width=4,textvariable=iend.yrs,justify = "right")
	mon2.v<-tkentry(fr.C1, width=4,textvariable=iend.mon,justify = "right")
	day2.v<-tkentry(fr.C1, width=4,textvariable=iend.day,justify = "right",state='normal')

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

	tkbind(cb.period,"<<ComboboxSelected>>",function(){
		if(tclvalue(file.period)=='Daily data'){
			tclvalue(day.txtVar)<-"Day"
			tkconfigure(day1.v,state='normal')
			tkconfigure(day2.v,state='normal')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.adj$Values[6])
			tclvalue(inrfeff)<-"rfe%s_%s_%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01_01.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01_01.nc')
		}
		if(tclvalue(file.period)=='Dekadal data'){
			tclvalue(day.txtVar)<-"Dek"
			tkconfigure(day1.v,state='normal')
			tkconfigure(day2.v,state='normal')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.adj$Values[6])
			tclvalue(inrfeff)<-"rfe%s_%s-dk%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
		}
		if(tclvalue(file.period)=='Monthly data'){
			tclvalue(day.txtVar)<-"Day"
			tkconfigure(day1.v,state='disabled')
			tkconfigure(day2.v,state='disabled')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.adj$Values[6])
			tclvalue(inrfeff)<-"rfe%s_%s.nc"
			infobulle(en.inrfeff,'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01.nc')
			status.bar.display(en.inrfeff,TextOutputVar,'Enter the format of the RFE files names in NetCDF, example: rfe1983_01.nc')
		}
	})

	###############################

	bt.prm.OK<-tkbutton(frMRG1, text=" OK ")
	bt.prm.CA<-tkbutton(frMRG1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=10,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
		if(tclvalue(dir.rfe)=="" | tclvalue(dir.rfe)=="NA"){
			tkmessageBox(message="Choose or enter the  directory containing the RFE files",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grdrfe)==""){
			tkmessageBox(message="You have to provide a RFE's sample file",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.bias)=="" | tclvalue(dir.bias)=="NA"){
			tkmessageBox(message="Choose or enter the path to directory or file containing mean bias files",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Choose or enter the path to directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period<<-ifelse(tclvalue(file.period)=='Daily data','daily', ifelse(tclvalue(file.period)=='Dekadal data','dekadal','monthly'))
			GeneralParameters$file.io$Values<<-c(tclvalue(file.grdrfe),tclvalue(dir.rfe), tclvalue(dir.bias),tclvalue(file.save1))
			GeneralParameters$prefix$Values<<-c(tclvalue(inrfeff),tclvalue(outmrgff),tclvalue(adjPrefix))
			GeneralParameters$dates.adj$Values<<-c(tclvalue(istart.yrs),tclvalue(istart.mon), tclvalue(istart.day),tclvalue(iend.yrs),tclvalue(iend.mon),tclvalue(iend.day))
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frMRG0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frMRG1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Bias Adjustment Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


##############################################################################################

mergeGetInfoRain<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur<-23
	else largeur<-21
	##tkentry width, filename format, prefix
	if (Sys.info()["sysname"] == "Windows") largeur1<-23
	else largeur1<-21

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0<-tkframe(tt,relief='raised',borderwidth=2)
	frMRG1<-tkframe(tt)

	fr.A<-tkframe(frMRG0,relief="groove",borderwidth=2)
	fr.C<-tkframe(frMRG0,relief="groove",borderwidth=2)
	tkgrid(fr.A,fr.C)
	tkgrid.configure(fr.A,row=0,column=0,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)
	tkgrid.configure(fr.C,row=0,column=1,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)

	pr.relief.set<-c('sunken','sunken','sunken','sunken')
	for(i in 0:3) assign(paste('fr.A',i,sep=''),tkframe(fr.A,relief=pr.relief.set[i+1],borderwidth=2))
	for(i in 0:3) tkgrid(get(paste('fr.A',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	###################
	fr.A00<-tkframe(fr.A0)
	#fr.A01<-tkframe(fr.A0)
	tkgrid(fr.A00,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#tkgrid(fr.A01,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period)=='daily',
	'Daily data',ifelse(as.character(GeneralParameters$period)=='dekadal','Dekadal data','Monthly data'))

	cb.period<-ttkcombobox(fr.A00, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)
	infobulle(cb.period,'Choose the time step of the data')
	status.bar.display(cb.period,TextOutputVar,'Choose the time step of the data')
	tkgrid(cb.period)

	###################
	for(i in 0:3) assign(paste('fr.A1',i,sep=''),tkframe(fr.A1))
	for(i in 0:3) tkgrid(get(paste('fr.A1',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A1',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	###
	frA10.txt<-tklabel(fr.A10,text='Station data file')
	tkgrid(frA10.txt)

	###
	cb.stnfl<-ttkcombobox(fr.A11, values=unlist(listOpenFiles), textvariable=file.stnfl)
	infobulle(cb.stnfl,'Choose the file in the list')
	status.bar.display(cb.stnfl,TextOutputVar,'Choose the file containing the gauge data')

	bt.stnfl<-tkbutton.h(fr.A11, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.stnfl,bt.stnfl)
	tkgrid.configure(cb.stnfl,row=0,column=0,sticky='w')
	tkgrid.configure(bt.stnfl,row=0,column=1,sticky='e')
	tkconfigure(bt.stnfl,command=function(){
		dat.opfiles<-getOpenFiles(tt,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.blkshp,values=unlist(listOpenFiles), textvariable=file.blkshp)
		}else{
			return(NULL)
		}
	})

	###
	adjORnot<-tclVar('Directory of adjusted RFE files')
	frA12.txt<-tklabel(fr.A12,text=tclvalue(adjORnot),textvariable=adjORnot)
	tkgrid(frA12.txt)

	dir.rfe <-tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.dir.rfe<-tkentry(fr.A13,textvariable=dir.rfe,width=largeur)
	infobulle(en.dir.rfe,'Enter the full path to\ndirectory containing the adjusted RFE files')
	status.bar.display(en.dir.rfe,TextOutputVar,'Enter the full path to directory containing the adjusted RFE files')
	bt.dir.rfe<-tkbutton.h(fr.A13, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.dir.rfe,bt.dir.rfe)
	tkgrid.configure(en.dir.rfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.dir.rfe,row=0,column=1,sticky='e')
	tkconfigure(bt.dir.rfe,command=function(){
		dir4rfe<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe)<-""
		else tclvalue(dir.rfe)<-dir4rfe
	})

	#######
	fr.A20<-tkframe(fr.A2)
	fr.A21<-tkframe(fr.A2)
	tkgrid(fr.A20,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A21,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frA20.txt<-tklabel(fr.A20,text='Directory to save result')
	tkgrid(frA20.txt)

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[5]))
	en.file.save<-tkentry(fr.A21,textvariable=file.save1,width=largeur)
	infobulle(en.file.save,'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path to directory to save result')
	bt.file.save<-tkbutton.h(fr.A21, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[5]), "")
			if(is.na(file2save1)) tclvalue(file.save1)<-as.character(GeneralParameters$file.io$Values[5])
			else{
				dir.create(file2save1,showWarnings=FALSE,recursive=TRUE)
				tclvalue(file.save1)<-file2save1
			}
	})

	#############################################
	fr.A30<-tkframe(fr.A3)
	fr.A31<-tkframe(fr.A3)
	fr.A32<-tkframe(fr.A3)
	fr.A33<-tkframe(fr.A3)

	tkgrid(fr.A30,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A31,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A32,row=2,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A33,row=3,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	####Use DEM
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[7])

	#####
	frA20.txt<-tklabel(fr.A30,text="Elevation data(NetCDF)")
	tkgrid(frA20.txt)

	if(as.character(GeneralParameters$blankGrd)=='2') statedem<-'normal'
	else statedem<-'disabled'
	#statedem<-'disabled'

	###
	cb.grddem<-ttkcombobox(fr.A31, values=unlist(listOpenFiles), textvariable=file.grddem,state=statedem)
	infobulle(cb.grddem,'Choose the file in the list')
	status.bar.display(cb.grddem,TextOutputVar,'Choose the file containing the elevation data in netcdf')
	bt.grddem<-tkbutton.h(fr.A31, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grddem,bt.grddem)
	tkgrid.configure(cb.grddem,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grddem,row=0,column=1,sticky='e')
	tkconfigure(bt.grddem,state=statedem,command=function(){
		nc.opfiles<-getOpenNetcdf(tt,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles
			tclvalue(file.grddem)<-AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.blkshp,values=unlist(listOpenFiles), textvariable=file.blkshp)
		}else{
			return(NULL)
		}
	})

	####################################
	file.blkshp <- tclVar()
	tclvalue(file.blkshp) <- as.character(GeneralParameters$file.io$Values[6])

	#####Use shp
	frA32.txt<-tklabel(fr.A32,text="ESRI shapefiles for blanking")
	tkgrid(frA32.txt)

	if(as.character(GeneralParameters$blankGrd)=='3') stateshp<-'normal'
	else stateshp<-'disabled'

	##
	cb.blkshp<-ttkcombobox(fr.A33, values=unlist(listOpenFiles), textvariable=file.blkshp,state=stateshp)
	infobulle(cb.blkshp,'Choose the file in the list')
	status.bar.display(cb.blkshp,TextOutputVar,'Choose the file containing the ESRI shapefiles')
	bt.blkshp<-tkbutton.h(fr.A33, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.blkshp,bt.blkshp)
	tkgrid.configure(cb.blkshp,row=0,column=0,sticky='w')
	tkgrid.configure(bt.blkshp,row=0,column=1,sticky='e')
	tkconfigure(bt.blkshp,state=stateshp,command=function(){
		shp.opfiles<-getOpenShp(tt,all.opfiles)

		if(!is.null(shp.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'shp'
			AllOpenFilesData[[nopf+1]]<<-shp.opfiles
			tclvalue(file.blkshp)<-AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.blkshp)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.blkshp,values=unlist(listOpenFiles), textvariable=file.blkshp)
		}else{
			return(NULL)
		}
	})

	###############################################
	pr.relief.set2<-c('sunken','sunken','sunken','sunken','sunken')
	for(i in 0:4) assign(paste('fr.C',i,sep=''),tkframe(fr.C,relief=pr.relief.set2[i+1],borderwidth=2))
	for(i in 0:4) tkgrid(get(paste('fr.C',i,sep='')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.C',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	#####
	infobulle(fr.C0,'Set the new grid to merge data in case using no adjusted RFE data')
	status.bar.display(fr.C0,TextOutputVar,'Set the new grid to merge data in case using no adjusted RFE data')

	frC0.txt<-tklabel(fr.C0,text='Use Original RFE data')

	newGrd <- tclVar(as.character(GeneralParameters$NewGrd))
	cb.newGrd <- tkcheckbutton(fr.C0,variable=newGrd,anchor='w',justify='left')
	bt.newGrd<-tkbutton(fr.C0, text="Set New Grid")
	tkconfigure(bt.newGrd,state='disabled',command=function(){
		GeneralParameters<<-createNewGrid2Merge(tt,GeneralParameters)
	})
	tkgrid(frC0.txt,row=0,column=0,columnspan=2,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(cb.newGrd,row=1,column=0,columnspan=1,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(bt.newGrd,row=1,column=1,columnspan=1,sticky='e',padx=1,pady=1,ipadx=1,ipady=1)

	###############################
	for(i in 0:3) assign(paste('fr.C1',i,sep=''),tkframe(fr.C1))
	for(i in 0:3) tkgrid(get(paste('fr.C1',i,sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C1',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=0,ipadx=1,ipady=0)

	adjORnot1<-tclVar('Adjusted data filename prefix')
	frC10.txt<-tklabel(fr.C10,text=tclvalue(adjORnot1),textvariable=adjORnot1)
	tkgrid(frC10.txt)

	adjPrefix <-tclVar(as.character(GeneralParameters$prefix$Values[1]))
	en.adjPrefix<-tkentry(fr.C11,textvariable=adjPrefix,width=largeur1)
	infobulle(en.adjPrefix,'Prefix for the file name of the adjusted RFE data')
	status.bar.display(en.adjPrefix,TextOutputVar,'Prefix for the file name of the  adjusted RFE data')
	tkgrid(en.adjPrefix,row=0,column=0,sticky='w')

	####
	frC12.txt<-tklabel(fr.C12,text='Merged data filename prefix/suffix')
	tkgrid(frC12.txt)

	mrgPrefix <-tclVar(as.character(GeneralParameters$prefix$Values[2]))
	en.mrgPrefix<-tkentry(fr.C13,textvariable=mrgPrefix,width=largeur1-6)
	mrgSuffix <- tclVar(as.character(GeneralParameters$prefix$Values[3]))
	cb.mrgSuffix<-ttkcombobox(fr.C13, values=c("ALL", "CLM","MON"), textvariable=mrgSuffix,width=4)

	tkgrid(en.mrgPrefix,row=0,column=0,sticky='w')
	tkgrid(cb.mrgSuffix,row=0,column=1,sticky='w')

	infobulle(fr.C13,'Prefix for the file name of the merged data')
	status.bar.display(fr.C13,TextOutputVar,'Prefix for the file name of the merged data')

	###############################
	infobulle(fr.C2,'Start and end date for merging RFE data')
	status.bar.display(fr.C2,TextOutputVar,'Start and end date for merging RFE data')

	deb.txt<-tklabel(fr.C2,text='Start date',anchor='e',justify='right')
	fin.txt<-tklabel(fr.C2,text='End date',anchor='e',justify='right')
	yrs.txt<-tklabel(fr.C2,text='Year')
	mon.txt<-tklabel(fr.C2,text='Month')
	if(as.character(GeneralParameters$period)=='dekadal') day.txtVar<-tclVar('Dek')
	else day.txtVar<-tclVar('Day')
	day.txt<-tklabel(fr.C2,text=tclvalue(day.txtVar),textvariable=day.txtVar)

	istart.yrs<-tclVar(as.character(GeneralParameters$dates.mrg$Values[1]))
	istart.mon<-tclVar(as.character(GeneralParameters$dates.mrg$Values[2]))
	istart.day<-tclVar(as.character(GeneralParameters$dates.mrg$Values[3]))
	iend.yrs<-tclVar(as.character(GeneralParameters$dates.mrg$Values[4]))
	iend.mon<-tclVar(as.character(GeneralParameters$dates.mrg$Values[5]))
	iend.day<-tclVar(as.character(GeneralParameters$dates.mrg$Values[6]))

	yrs1.v<-tkentry(fr.C2, width=4,textvariable=istart.yrs,justify = "right")
	mon1.v<-tkentry(fr.C2, width=4,textvariable=istart.mon,justify = "right")
	day1.v<-tkentry(fr.C2, width=4,textvariable=istart.day,justify = "right",state='normal')
	yrs2.v<-tkentry(fr.C2, width=4,textvariable=iend.yrs,justify = "right")
	mon2.v<-tkentry(fr.C2, width=4,textvariable=iend.mon,justify = "right")
	day2.v<-tkentry(fr.C2, width=4,textvariable=iend.day,justify = "right",state='normal')

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

	###########################################
	fr.C30<-tkframe(fr.C3)
	fr.C31<-tkframe(fr.C3)
	tkgrid(fr.C30,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.C31,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frC30.txt<-tklabel(fr.C30,text='Blank grid')
	tkgrid(frC30.txt)

	blankGrd <- tclVar(as.character(GeneralParameters$blankGrd))
	tclvalue(blankGrd) <- ifelse(as.character(GeneralParameters$blankGrd)=='1',
	'None',ifelse(as.character(GeneralParameters$blankGrd)=='2','Use DEM','Use ESRI shapefile'))

	cb.blankGrd<-ttkcombobox(fr.C31, values=c("None", "Use DEM","Use ESRI shapefile"), textvariable=blankGrd)
	infobulle(cb.blankGrd,'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd,TextOutputVar,
	'Blank grid outside the country boundaries  or over ocean given by the DEM mask or the shapefile')
	tkgrid(cb.blankGrd)

	###########################################
	bt.opt.set<-tkbutton.h(fr.C4, text="Options - Settings",TextOutputVar,
	'Set general options for merging','Set general options for merging')
	tkgrid(bt.opt.set,sticky='we',padx=25,pady=5,ipadx=1,ipady=1)
	tkconfigure(bt.opt.set,command=function(){
		GeneralParameters<<-getParamMering(tt,GeneralParameters)
	})
	############################################
	tkbind(cb.newGrd,"<Button-1>",function(){
		if(tclvalue(newGrd)=='0'){
			tkconfigure(bt.newGrd,state='normal')
			if(tclvalue(file.period)=='Daily data'){
				tclvalue(adjPrefix)<-"rfe%s_%s_%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01_01.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01_01.nc')
			}
			if(tclvalue(file.period)=='Dekadal data'){
				tclvalue(adjPrefix)<-"rfe%s_%s-dk%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
			}
			if(tclvalue(file.period)=='Monthly data'){
				tclvalue(adjPrefix)<-"rfe%s_%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01.nc')
			}
			tclvalue(adjORnot)<-'Directory of RFE data files'
			infobulle(en.dir.rfe,'Enter the full path to\ndirectory containing the RFE data files')
			status.bar.display(en.dir.rfe,TextOutputVar,'Enter the full path to directory containing the RFE data files')
			tclvalue(adjORnot1)<-'Input RFE file format'

		}else{
			tkconfigure(bt.newGrd,state='disabled')
			tclvalue(adjPrefix)<-'rr_adj'
			tclvalue(adjORnot)<-'Directory of adjusted RFE files'
			infobulle(en.dir.rfe,'Enter the full path to\ndirectory containing the adjusted RFE files')
			status.bar.display(en.dir.rfe,TextOutputVar,'Enter the full path to directory containing the adjusted RFE files')
			tclvalue(adjORnot1)<-'Adjusted data filename prefix'
			infobulle(en.adjPrefix,'Prefix for the file name of the adjusted RFE data')
			status.bar.display(en.adjPrefix,TextOutputVar,'Prefix for the file name of the  adjusted RFE data')
		}
	})

	############################################
	tkbind(cb.period,"<<ComboboxSelected>>",function(){
		if(tclvalue(file.period)=='Daily data'){
			tclvalue(day.txtVar)<-"Day"
			tkconfigure(day1.v,state='normal')
			tkconfigure(day2.v,state='normal')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.mrg$Values[6])
			if(tclvalue(newGrd)=='1'){
				tclvalue(adjPrefix)<-"rfe%s_%s_%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01_01.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01_01.nc')
			}else{
				tclvalue(adjPrefix)<-'rr_adj'
			}

		}
		if(tclvalue(file.period)=='Dekadal data'){
			tclvalue(day.txtVar)<-"Dek"
			tkconfigure(day1.v,state='normal')
			tkconfigure(day2.v,state='normal')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.mrg$Values[6])
			if(tclvalue(newGrd)=='1'){
				tclvalue(adjPrefix)<-"rfe%s_%s-dk%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
			}else{
				tclvalue(adjPrefix)<-'rr_adj'
			}
		}
		if(tclvalue(file.period)=='Monthly data'){
			tclvalue(day.txtVar)<-"Day"
			tkconfigure(day1.v,state='disabled')
			tkconfigure(day2.v,state='disabled')
			#tclvalue(day.val)<-as.character(GeneralParameters$dates.mrg$Values[6])
			if(tclvalue(newGrd)=='1'){
				tclvalue(adjPrefix)<-"rfe%s_%s.nc"
				infobulle(en.adjPrefix,
				'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01.nc')
				status.bar.display(en.adjPrefix,TextOutputVar,
				'Enter the format of the RFE files names in NetCDF, example: rfe1983_01.nc')
			}else{
				tclvalue(adjPrefix)<-'rr_adj'
			}
		}
	})

	############
	tkbind(cb.blankGrd,"<<ComboboxSelected>>",function(){
		if(tclvalue(blankGrd)=="None"){
			tkconfigure(cb.grddem,state='disabled')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.blkshp,state='disabled')
			tkconfigure(bt.blkshp,state='disabled')
		}
		if(tclvalue(blankGrd)=="Use DEM"){
			tkconfigure(cb.grddem,state='normal')
			tkconfigure(bt.grddem,state='normal')
			tkconfigure(cb.blkshp,state='disabled')
			tkconfigure(bt.blkshp,state='disabled')
		}
		if(tclvalue(blankGrd)=="Use ESRI shapefile"){
			tkconfigure(cb.grddem,state='disabled')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.blkshp,state='normal')
			tkconfigure(bt.blkshp,state='normal')
		}
	})

	#################

	bt.prm.OK<-tkbutton(frMRG1, text=" OK ")
	bt.prm.CA<-tkbutton(frMRG1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=10,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
		if(tclvalue(file.stnfl)==""){
			tkmessageBox(message="Choose the file containing the gauge data",icon="warning",type="ok")
			#tkwait.window(tt)
		}else if(tclvalue(dir.rfe)=="" | tclvalue(dir.rfe)=="NA"){
			tkmessageBox(message="Choose or enter the  directory containing the RFE files",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(newGrd)=='1' &  as.character(GeneralParameters$file.io$Values[3])==""){
			tkmessageBox(message="You have to provide a RFE's sample file",icon="warning",type="ok")
			tkwait.window(tt)
		}else if((tclvalue(newGrd)=='1' &  as.character(GeneralParameters$CreateGrd)=='2' & as.character(GeneralParameters$file.io$Values[2])=="") | (tclvalue(blankGrd)=="Use DEM" & tclvalue(file.grddem)=="")){
			tkmessageBox(message="You have to provide DEM data in NetCDF format",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(blankGrd)=="Use ESRI shapefile" & tclvalue(file.blkshp)==""){
			tkmessageBox(message="You have to provide the shapefile",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Choose or enter the path to directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period<<-ifelse(tclvalue(file.period)=='Daily data','daily', ifelse(tclvalue(file.period)=='Dekadal data','dekadal','monthly'))
			GeneralParameters$prefix$Values<<-c(tclvalue(adjPrefix),tclvalue(mrgPrefix),tclvalue(mrgSuffix))
			valfl<-as.character(GeneralParameters$file.io$Values)
			valfl[c(1,4,5,6,7)]<-c(tclvalue(file.stnfl),tclvalue(dir.rfe),tclvalue(file.save1), tclvalue(file.blkshp),tclvalue(file.grddem))
			GeneralParameters$file.io$Values<<-valfl
			GeneralParameters$dates.mrg$Values<<-c(tclvalue(istart.yrs),tclvalue(istart.mon), tclvalue(istart.day),tclvalue(iend.yrs),tclvalue(iend.mon),tclvalue(iend.day))
			GeneralParameters$NewGrd<<-tclvalue(newGrd)
			GeneralParameters$blankGrd<<-ifelse(tclvalue(blankGrd)=='None','1', ifelse(tclvalue(blankGrd)=='Use DEM','2','3'))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frMRG0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frMRG1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

##############################################################################################

getParamMering<-function(tt,GeneralParameters){
	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0<-tkframe(tt1,relief='raised',borderwidth=2)
	frMRG1<-tkframe(tt1)

	tkgrid(frMRG0,sticky='ew',padx=2,pady=1)
	tkgrid(frMRG1,sticky='ew',padx=2,pady=1)

	fr.C<-tkframe(frMRG0,relief="sunken",borderwidth=2)
	fr.D<-tkframe(frMRG0,relief="sunken",borderwidth=2)
	tkgrid(fr.C,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(fr.D,row=1,column=0,sticky='ew',padx=1,pady=1)

	###########
	nmin.l<-tklabel.h(fr.C,'NminStn',TextOutputVar,
	'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.non.zero.l<-tklabel.h(fr.C,'NminNo0',TextOutputVar,
	'Minimum number of non-zero gauge values to perform the merging',
	'Minimum number of non-zero gauge values to perform the merging')
	max.rnr.dst.l<-tklabel.h(fr.C,'MaxRnRDist',TextOutputVar,
	'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask',
	'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	max.dst.l<-tklabel.h(fr.C,'MaxDist',TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')
	min.nbrs.l<-tklabel.h(fr.C,'MinStn',TextOutputVar,
	'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate data')
	max.nbrs.l<-tklabel.h(fr.C,'MaxStn',TextOutputVar,
	'Maximum number of neighbours to be used to interpolate data',
	'Maximum number of neighbours to be used to interpolate data')

	nmin.v<-tkentry.h(fr.C,TextOutputVar,
	'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.non.zero.v<-tkentry.h(fr.C,TextOutputVar,
	'Minimum number of non-zero gauge values to perform the merging',
	'Minimum number of non-zero gauge values to perform the merging')
	max.rnr.dst.v<-tkentry.h(fr.C,TextOutputVar,
	'Maximum distance (in decimal degrees) for interpolating Rain-no-Rain mask',
	'Maximum distance (in decimal degrees) for interpolating Rain-no-Rain mask')
	max.dst.v<-tkentry.h(fr.C,TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')
	min.nbrs.v<-tkentry.h(fr.C,TextOutputVar,
	'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate data')
	max.nbrs.v<-tkentry.h(fr.C,TextOutputVar,
	'Maximum number of neighbours to be used to interpolate data',
	'Maximum number of neighbours to be used to interpolate data')

	tkgrid(nmin.l,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(nmin.v,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(min.non.zero.l,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(min.non.zero.v,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(max.rnr.dst.l,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(max.rnr.dst.v,row=1,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(max.dst.l,row=1,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(max.dst.v,row=1,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(min.nbrs.l,row=2,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(min.nbrs.v,row=2,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(max.nbrs.l,row=2,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(max.nbrs.v,row=2,column=3,sticky='ew',padx=1,pady=1)

	tkconfigure(nmin.l,anchor='e',justify='right')
	tkconfigure(min.non.zero.l,anchor='e',justify='right')
	tkconfigure(max.rnr.dst.l,anchor='e',justify='right')
	tkconfigure(max.dst.l,anchor='e',justify='right')
	tkconfigure(min.nbrs.l,anchor='e',justify='right')
	tkconfigure(max.nbrs.l,anchor='e',justify='right')

	nmin <- tclVar(as.character(GeneralParameters$params.int$Values[1]))
	min.non.zero <- tclVar(as.character(GeneralParameters$params.int$Values[2]))
	max.rnr.dst <- tclVar(as.character(GeneralParameters$params.int$Values[3]))
	max.dst <- tclVar(as.character(GeneralParameters$params.int$Values[4]))
	min.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[5]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[6]))

	tkconfigure(nmin.v,width=4,textvariable=nmin,justify='right')
	tkconfigure(min.non.zero.v,width=4,textvariable=min.non.zero,justify='right')
	tkconfigure(max.rnr.dst.v,width=4,textvariable=max.rnr.dst,justify='right')
	tkconfigure(max.dst.v,width=4,textvariable=max.dst,justify='right')
	tkconfigure(min.nbrs.v,width=4,textvariable=min.nbrs,justify='right')
	tkconfigure(max.nbrs.v,width=4,textvariable=max.nbrs,justify='right')

	####################################
	interpMethod <- tclVar(as.character(GeneralParameters$params.mrg$Values[1]))
	txt.interpMethod<-tklabel(fr.D,text='Interpolation method')
	cb.interpMethod<-ttkcombobox(fr.D, values=c('IDW', 'Kriging'), textvariable=interpMethod)
	infobulle(cb.interpMethod,
	'Interpolation techniques: Kriging or Inverse Distance Weighted')
	status.bar.display(cb.interpMethod,TextOutputVar,
	'Interpolation techniques: Kriging or Inverse Distance Weighted')

	RainNoRain <- tclVar(as.character(GeneralParameters$params.mrg$Values[2]))
	txt.RainNoRain<-tklabel(fr.D,text='Rain-no-Rain mask')
	cb.RainNoRain<-ttkcombobox(fr.D, values=c('None', 'Gauge','Satellite','GaugeSatellite'), textvariable=RainNoRain)
	infobulle(cb.RainNoRain,'Mask applied to handle no rain')
	status.bar.display(cb.RainNoRain,TextOutputVar,'Mask applied to handle no rain')

	tkgrid(txt.interpMethod,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(cb.interpMethod,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(txt.RainNoRain,row=2,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(cb.RainNoRain,row=3,column=0,sticky='ew',padx=1,pady=1)

	################################
	bt.prm.OK<-tkbutton(frMRG1, text=" OK ")
	bt.prm.CA<-tkbutton(frMRG1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=5,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
		GeneralParameters$params.mrg$Values<<-c(tclvalue(interpMethod),tclvalue(RainNoRain))
		GeneralParameters$params.int$Values<<-c(tclvalue(nmin),tclvalue(min.non.zero), tclvalue(max.rnr.dst),tclvalue(max.dst),tclvalue(min.nbrs),tclvalue(max.nbrs))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(frMRG0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frMRG1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Merging Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

##############################################################################################

createNewGrid2Merge<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0<-tkframe(tt,relief='raised',borderwidth=2)
	frMRG1<-tkframe(tt)

	fr.A<-tkframe(frMRG0,relief="groove",borderwidth=2)
	fr.C<-tkframe(frMRG0,relief="groove",borderwidth=2)
	tkgrid(fr.A,fr.C)
	tkgrid.configure(fr.A,row=0,column=0,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)
	tkgrid.configure(fr.C,row=0,column=1,sticky='news',padx=5,pady=1,ipadx=1,ipady=1)

	###########################
	fr.A0<-tkframe(fr.A,relief='sunken',borderwidth=2)
	fr.A1<-tkframe(fr.A,relief='sunken',borderwidth=2)
	tkgrid(fr.A0,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A1,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	#########3
	fr.A00<-tkframe(fr.A0)
	fr.A01<-tkframe(fr.A0)
	tkgrid(fr.A00,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A01,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	file.grdrfe <- tclVar()
	tclvalue(file.grdrfe) <- as.character(GeneralParameters$file.io$Values[3])

	frA00.txt<-tklabel(fr.A00,text="RFE's sample file")
	tkgrid(frA00.txt)

	###from RFE
	cb.grdrfe<-ttkcombobox(fr.A01, values=unlist(listOpenFiles), textvariable=file.grdrfe)
	infobulle(cb.grdrfe,'Choose the file in the list')
	status.bar.display(cb.grdrfe,TextOutputVar,'File containing a sample of RFE data in netcdf')
	bt.grdrfe<-tkbutton.h(fr.A01, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grdrfe,bt.grdrfe)
	tkgrid.configure(cb.grdrfe,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grdrfe,row=0,column=1,sticky='e')
	tkconfigure(bt.grdrfe,command=function(){
		fileopen<-tclvalue(tkgetOpenFile(initialdir=as.character(GeneralParameters$file.io$Values[4]),
		initialfile = "",filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen=="" | is.na(fileopen)) return(NULL)
		nc.opfiles1<-preview.data.nc(tt,fileopen,"")
		nc.opfiles<-list(basename(fileopen),nc.opfiles1,fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles,"end",basename(fileopen))
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe)<-AllOpenFilesData[[nopf+1]][[1]]
			#tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
		}else{
			return(NULL)
		}
	})

	###########################
	fr.A10<-tkframe(fr.A1)
	fr.A11<-tkframe(fr.A1)
	tkgrid(fr.A10,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.A11,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	###################
	if(as.character(GeneralParameters$CreateGrd)=='2') statedem<-'normal'
	else statedem<-'disabled'

	#####
	frA10.txt<-tklabel(fr.A10,text="Elevation data(NetCDF)")
	tkgrid(frA10.txt)

	###from DEM
	cb.grddem<-ttkcombobox(fr.A11, values=unlist(listOpenFiles), textvariable=file.grddem,state=statedem)
	infobulle(cb.grddem,'Choose the file in the list')
	status.bar.display(cb.grddem,TextOutputVar,'File containing the elevation data in netcdf')
	bt.grddem<-tkbutton.h(fr.A11, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.grddem,bt.grddem)
	tkgrid.configure(cb.grddem,row=0,column=0,sticky='w')
	tkgrid.configure(bt.grddem,row=0,column=1,sticky='e')
	tkconfigure(bt.grddem,state=statedem,command=function(){
		nc.opfiles<-getOpenNetcdf(tt,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem)<-AllOpenFilesData[[nopf+1]][[1]]
			#tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.grddem,values=unlist(listOpenFiles), textvariable=file.grddem)
			tkconfigure(cb.grdrfe,values=unlist(listOpenFiles), textvariable=file.grdrfe)
		}else{
			return(NULL)
		}
	})

	###################
	fr.C0<-tkframe(fr.C,relief='sunken',borderwidth=2)
	tkgrid(fr.C0,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	infobulle(fr.C0,'Create the grid to interpolate the merged data')
	status.bar.display(fr.C0,TextOutputVar,'Create the grid to interpolate the merged data')

	fr.C00<-tkframe(fr.C0)
	fr.C01<-tkframe(fr.C0)
	fr.C02<-tkframe(fr.C0)
	fr.C03<-tkframe(fr.C0)
	tkgrid(fr.C00,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.C01,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.C02,row=2,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)
	tkgrid(fr.C03,row=3,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=0)

	frC00.txt<-tklabel(fr.C00,text='Create grid for interpolation')
	tkgrid(frC00.txt)

	varCreateGrd <- tclVar(as.character(GeneralParameters$CreateGrd))
	grdRFE.rbt <- tkradiobutton(fr.C01,text="From RFE",anchor='w',justify='left')
	tkgrid(grdRFE.rbt)
	grdDEM.rbt <- tkradiobutton(fr.C02,text="From DEM",anchor='w',justify='left')
	tkgrid(grdDEM.rbt)
	grdNEW.rbt <- tkradiobutton(fr.C03,text="New Grid",anchor='w',justify='left')

	tkconfigure(grdRFE.rbt,variable=varCreateGrd,value="1")
	tkconfigure(grdDEM.rbt,variable=varCreateGrd,value="2")
	tkconfigure(grdNEW.rbt,variable=varCreateGrd,value="3")

	if(as.character(GeneralParameters$CreateGrd)=='3') stategrd<-'normal'
	else stategrd<-'disabled'

	bt.getNewgrid<-tkbutton.h(fr.C03, text="Create",TextOutputVar,'Set the new grid','Set the new grid')
	tkconfigure(bt.getNewgrid,state=stategrd,command=function(){
		GeneralParameters<<-getParamNewGrid(tt,GeneralParameters)
	})
	tkgrid(grdNEW.rbt,row=0,column=0)
	tkgrid(bt.getNewgrid,row=0,column=1,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	###########
	tkbind(grdRFE.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='disabled')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.grddem,state='disabled')
	})
	tkbind(grdDEM.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='disabled')
			tkconfigure(bt.grddem,state='normal')
			tkconfigure(cb.grddem,state='normal')
	})
	tkbind(grdNEW.rbt,"<Button-1>",function(){
			tkconfigure(bt.getNewgrid,state='normal')
			tkconfigure(bt.grddem,state='disabled')
			tkconfigure(cb.grddem,state='disabled')
	})

	##############
	bt.prm.OK<-tkbutton(frMRG1, text=" OK ")
	bt.prm.CA<-tkbutton(frMRG1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=10,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
			valfl<-as.character(GeneralParameters$file.io$Values)
			valfl[c(2,3)]<-c(tclvalue(file.grddem),tclvalue(file.grdrfe))
			GeneralParameters$file.io$Values<<-valfl
			GeneralParameters$CreateGrd<<-tclvalue(varCreateGrd)
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)

	})
	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frMRG0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frMRG1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'New grid - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

##############################################################################################

getParamNewGrid<-function(tt,GeneralParameters){
	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0<-tkframe(tt1,relief='raised',borderwidth=2)
	frGrd1<-tkframe(tt1)

	fr_grd<-ttklabelframe(frGrd0,text="Create new grid",relief="groove",borderwidth=2)
	tkgrid(fr_grd)
	grd_llon<-tklabel(fr_grd, text="Longitude",anchor='e',justify='right')
	grd_llat<-tklabel(fr_grd, text="Latitude",anchor='e',justify='right')
	grd_lb1<-tklabel(fr_grd, text="Min")
	grd_lb2<-tklabel(fr_grd, text="Max")
	grd_lb3<-tklabel(fr_grd, text="Res")

	grd_vlon1<-tkentry.h(fr_grd,TextOutputVar,'Minimum longitude in degree','Minimum longitude in degree')
	grd_vlon2<-tkentry.h(fr_grd,TextOutputVar,'Maximum longitude in degree','Maximum longitude in degree')
	grd_vlon3<-tkentry.h(fr_grd,TextOutputVar,'Resolution in degree','Resolution in degree')
	grd_vlat1<-tkentry.h(fr_grd,TextOutputVar,'Minimum latitude in degree','Minimum latitude in degree')
	grd_vlat2<-tkentry.h(fr_grd,TextOutputVar,'Maximum latitude in degree','Maximum latitude in degree')
	grd_vlat3<-tkentry.h(fr_grd,TextOutputVar,'Resolution in degree','Resolution in degree')

	minLon<-tclVar(as.character(GeneralParameters$new.grid$Values[1]))
	maxLon<-tclVar(as.character(GeneralParameters$new.grid$Values[2]))
	resLon<-tclVar(as.character(GeneralParameters$new.grid$Values[3]))
	minLat<-tclVar(as.character(GeneralParameters$new.grid$Values[4]))
	maxLat<-tclVar(as.character(GeneralParameters$new.grid$Values[5]))
	resLat<-tclVar(as.character(GeneralParameters$new.grid$Values[6]))

	tkconfigure(grd_vlon1, width=5,justify = "right",textvariable=minLon)
	tkconfigure(grd_vlon2, width=5,justify = "right",textvariable=maxLon)
	tkconfigure(grd_vlon3, width=6,justify = "right",textvariable=resLon)
	tkconfigure(grd_vlat1, width=5,justify = "right",textvariable=minLat)
	tkconfigure(grd_vlat2, width=5,justify = "right",textvariable=maxLat)
	tkconfigure(grd_vlat3, width=6,justify = "right",textvariable=resLat)

	tkgrid(grd_lb1,row=0, column=1,sticky="ew")
	tkgrid(grd_lb2,row=0, column=2,sticky="ew")
	tkgrid(grd_lb3,row=0, column=3,sticky="ew")
	tkgrid(grd_llon,row=1, column=0,sticky="ew")
	tkgrid(grd_vlon1,row=1, column=1,sticky="ew")
	tkgrid(grd_vlon2,row=1, column=2,sticky="ew")
	tkgrid(grd_vlon3,row=1, column=3,sticky="ew")
	tkgrid(grd_llat,row=2, column=0,sticky="ew")
	tkgrid(grd_vlat1,row=2, column=1,sticky="ew")
	tkgrid(grd_vlat2,row=2, column=2,sticky="ew")
	tkgrid(grd_vlat3,row=2, column=3,sticky="ew")

	################################

	bt.prm.OK<-tkbutton(frGrd1, text=" OK ")
	bt.prm.CA<-tkbutton(frGrd1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=5,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
		GeneralParameters$new.grid$Values<<-c(as.character(tclvalue(minLon)),as.character(tclvalue(maxLon)),as.character(tclvalue(resLon)),
		as.character(tclvalue(minLat)),as.character(tclvalue(maxLat)),as.character(tclvalue(resLat)))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(frGrd0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frGrd1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Grid Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

