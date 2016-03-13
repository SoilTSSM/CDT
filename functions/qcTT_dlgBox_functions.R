qc.get.info.txtn<-function(parent.win,gal.params){
	file.list<-openFile_ttkcomboList()
	##tkentry width, file path
	if (Sys.info()["sysname"] == "Windows") largeur<-28
	else largeur<-26

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog<-tkframe(tt,relief='raised',borderwidth=2)
	frButt<-tkframe(tt)
	fr.A<-tkframe(frDialog,relief="groove",borderwidth=2)
	fr.B<-tkframe(frDialog,relief="groove",borderwidth=2)
	#fr.C<-tkframe(tt,relief="flat",borderwidth=2)
	#tkgrid(fr.A,fr.B,fr.C)
	tkgrid(fr.A,fr.B)
	tkgrid.configure(fr.A,row=0,column=0,sticky='news',padx=5,pady=5,ipadx=1,ipady=1)
	tkgrid.configure(fr.B,row=0,column=1,sticky='news',padx=5,pady=5,ipadx=1,ipady=1)
	#tkgrid.configure(fr.C,row=0,column=2,sticky='news',padx=5,pady=5,ipadx=1,ipady=1)
	
	pr.relief.set<-c('sunken','sunken','sunken','sunken','sunken')
	for(i in 0:4) assign(paste('fr.A',i,sep=''),tkframe(fr.A,relief=pr.relief.set[i+1],borderwidth=2))
	for(i in 0:4) tkgrid(get(paste('fr.A',i,sep='')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	################################
	fr.A01<-tkframe(fr.A0)
	fr.A02<-tkframe(fr.A0)
	tkgrid(fr.A01,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A02,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(gal.params$period)=='daily',
	'Daily data',ifelse(as.character(gal.params$period)=='dekadal','Dekadal data','Monthly data'))

	cb.period<-ttkcombobox(fr.A02, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)
	infobulle(cb.period,'Choose the time step of the data')
	status.bar.display(cb.period,txt.stbr1,'Choose the time step of the data')
	tkgrid(cb.period)
	
	################################
	if(gal.params$AllOrOne=='one') state1s<-'normal'
	if(gal.params$AllOrOne=='all') state1s<-'disabled'

	if(as.character(gal.params$use.method$Values[2])=='0') temp.state<-'disabled'
	if(as.character(gal.params$use.method$Values[2])=='1') temp.state<-'normal'

	if(as.character(gal.params$use.method$Values[1])=='0' & as.character(gal.params$use.method$Values[3])=='0'){
		state<-c('disabled','normal','disabled')
		state1<-'disabled'
	}else if(as.character(gal.params$use.method$Values[1])=='1'){
		state<-c('disabled','normal','disabled')
		state1<-'normal'
	}else if(as.character(gal.params$use.method$Values[3])=='1'){
		state1<-'disabled'
		if(as.character(gal.params$use.method$Values[4])=='0') state<-c('normal','normal','normal')
		else state<-c('disabled','normal','normal')
	}

	############################
	fr.A11<-tkframe(fr.A1)
	fr.A12<-tkframe(fr.A1)
	tkgrid(fr.A11,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A12,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	
	file.choix1a <- tclVar()
	tclvalue(file.choix1a) <- as.character(gal.params$file.io$Values[1])
	file.choix1b <- tclVar()
	tclvalue(file.choix1b) <- as.character(gal.params$file.io$Values[2])
	file.choix2 <- tclVar()
	tclvalue(file.choix2) <- as.character(gal.params$file.io$Values[3])

	frA11.txt1<-tklabel(fr.A11,text='Data to control')
	vartx <- tkradiobutton(fr.A11,text="Tmax",anchor='w',justify='left')
	vartn <- tkradiobutton(fr.A11,text="Tmin",anchor='w',justify='left')
	tkgrid(frA11.txt1,vartx,vartn)
	vartxtn <- tclVar(gal.params$test.tx)
	tkconfigure(vartx,variable=vartxtn,value="1")
	tkconfigure(vartn,variable=vartxtn,value="0")


	txtntxt1<-tclVar("Tmax")
	txtn.file.stn1<-tklabel(fr.A12,text=tclvalue(txtntxt1),textvariable=txtntxt1)
	cb.file.stn1<-ttkcombobox(fr.A12, values=unlist(file.list), textvariable=file.choix1a)
	infobulle(cb.file.stn1,'Choose the file in the list')
	status.bar.display(cb.file.stn1,txt.stbr1,'Choose the file containing the data to control')

	bt.file.stn1<-tkbutton.h(fr.A12, text="...",txt.stbr1,'Browse file if not listed','Browse file if not listed') 
	tkgrid(txtn.file.stn1,cb.file.stn1,bt.file.stn1) 
	tkgrid.configure(txtn.file.stn1,row=0,column=0,sticky='w')
	tkgrid.configure(cb.file.stn1,row=0,column=1,sticky='we')
	tkgrid.configure(bt.file.stn1,row=0,column=2,sticky='e')
	
	tkconfigure(bt.file.stn1,command=function(){
		dat.opfiles<-getOpenFiles(parent.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'ascii'
			file.opfiles[[nopf+1]]<<-dat.opfiles

			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]] 
			tclvalue(file.choix1a)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1,values=unlist(file.list), textvariable=file.choix1a)
			tkconfigure(cb.file.stn2,values=unlist(file.list), textvariable=file.choix1b)
			tkconfigure(cb.file.elv,values=unlist(file.list), textvariable=file.choix2)
		}else{
			return(NULL)
		}
	})
	
	txtntxt2<-tclVar("Tmin")
	txtn.file.stn2<-tklabel(fr.A12,text=tclvalue(txtntxt2),textvariable=txtntxt2)
	cb.file.stn2<-ttkcombobox(fr.A12, values=unlist(file.list), textvariable=file.choix1b)
	infobulle(cb.file.stn2,'Choose the file in the list')
	status.bar.display(cb.file.stn2,txt.stbr1,'Choose the file containing the data to control')

	bt.file.stn2<-tkbutton.h(fr.A12, text="...",txt.stbr1,'Browse file if not listed','Browse file if not listed') 
	tkgrid(txtn.file.stn2,cb.file.stn2,bt.file.stn2) 
	tkgrid.configure(txtn.file.stn2,row=1,column=0,sticky='w')
	tkgrid.configure(cb.file.stn2,row=1,column=1,sticky='we')
	tkgrid.configure(bt.file.stn2,row=1,column=2,sticky='e')
	tkconfigure(cb.file.stn2,state=temp.state)
	tkconfigure(bt.file.stn2,state=temp.state)
	
	tkconfigure(bt.file.stn2,command=function(){
		dat.opfiles<-getOpenFiles(parent.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'ascii'
			file.opfiles[[nopf+1]]<<-dat.opfiles

			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]] 
			tclvalue(file.choix1b)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1,values=unlist(file.list), textvariable=file.choix1a)
			tkconfigure(cb.file.stn2,values=unlist(file.list), textvariable=file.choix1b)
			tkconfigure(cb.file.elv,values=unlist(file.list), textvariable=file.choix2)
		}else{
			return(NULL)
		}
	})
	
	tkbind(vartx,"<Button-1>",function(){
		tclvalue(txtntxt1)<-'Tmax'
		tclvalue(txtntxt2)<-'Tmin'
	})
	tkbind(vartn,"<Button-1>",function(){
		tclvalue(txtntxt1)<-'Tmin'
		tclvalue(txtntxt2)<-'Tmax'
	})

	################################

	fr.A21<-tkframe(fr.A2)
	fr.A22<-tkframe(fr.A2)
	tkgrid(fr.A21,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A22,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	frA21.txt1<-tklabel(fr.A21,text='Elevation Data (NetCDF)')
	tkgrid(frA21.txt1)

	lab.space.elv<-tklabel(fr.A22,text='        ')
	cb.file.elv<-ttkcombobox(fr.A22, values=unlist(file.list), textvariable=file.choix2,state=state[1])
	infobulle(cb.file.elv,'Choose the file in the list')
	status.bar.display(cb.file.elv,txt.stbr1,'Choose the file containing the elevation data')
	bt.file.elv<-tkbutton.h(fr.A22, text="...",txt.stbr1,'Browse file if not listed','Browse file if not listed') 
	tkgrid(lab.space.elv,cb.file.elv,bt.file.elv) 
	tkgrid.configure(lab.space.elv,row=0,column=0,sticky='w')
	tkgrid.configure(cb.file.elv,row=0,column=1,sticky='we')
	tkgrid.configure(bt.file.elv,row=0,column=2,sticky='e')
	tkconfigure(bt.file.elv,state=state[1],command=function(){
		nc.opfiles<-getOpenNetcdf(parent.win,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'netcdf'
			file.opfiles[[nopf+1]]<<-nc.opfiles
			tclvalue(file.choix2)<-file.opfiles[[nopf+1]][[1]]
		
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.choix2)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(cb.file.elv,values=unlist(file.list), textvariable=file.choix2)
			tkconfigure(cb.file.stn1,values=unlist(file.list), textvariable=file.choix1a)
			tkconfigure(cb.file.stn2,values=unlist(file.list), textvariable=file.choix1b)
		}else{
			return(NULL)
		}
	})

	################################
	fr.A31<-tkframe(fr.A3)
	fr.A32<-tkframe(fr.A3)
	tkgrid(fr.A31,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A32,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	frA31.txt1<-tklabel(fr.A31,text='Directory to save result')
	tkgrid(frA31.txt1)

	file.save1 <-tclVar(as.character(gal.params$file.io$Values[4]))
	en.file.save<-tkentry(fr.A32,textvariable=file.save1,width=largeur) 
	infobulle(en.file.save,'Enter the full path of the\ndirectory to save result')
	status.bar.display(en.file.save,txt.stbr1,'Enter the full path of the directory to save result')
	bt.file.save<-tkbutton.h(fr.A32, text="...",txt.stbr1,'or browse here','')
	tkgrid(en.file.save,bt.file.save) 
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(gal.params$file.io$Values[4]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1,'does not exist.\n It will be created.',sep=' '),icon="warning",type="ok")
			dir.create(file2save1,recursive=TRUE)
			tclvalue(file.save1)<-file2save1
		}else tclvalue(file.save1)<-file2save1
	})

	################################
	bt.opt.set<-tkbutton.h(fr.A4, text="Options - Settings",txt.stbr1,'Set general options for QC','Set general options for QC') 
	tkgrid(bt.opt.set,sticky='we',padx=25,pady=5,ipadx=1,ipady=1)

	tkconfigure(bt.opt.set,command=function(){
		if(tclvalue(cb.1series.val)=="0") state.parm<-c('normal','normal')
		else state.parm<-c('disabled','normal')
		gal.params<<-get.param.temperature(tt,gal.params,state.parm)
	})

	################################
	pr.relief.set1<-c('sunken','sunken','sunken','flat')
	for(i in 0:2) assign(paste('fr.B',i,sep=''),tkframe(fr.B,relief=pr.relief.set1[i+1],borderwidth=2))
	for(i in 0:2) tkgrid(get(paste('fr.B',i,sep='')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.B',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	cb.1series.val <- tclVar(as.character(gal.params$use.method$Values[1]))
	cb.1series <- tkcheckbutton(fr.B0,variable=cb.1series.val,text='Single Series',anchor='w',justify='left')
	infobulle(cb.1series,'Check for one station series')
	status.bar.display(cb.1series,txt.stbr1,'Check if the data is a series of one station')
	tkgrid(cb.1series,row=0,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkconfigure(cb.1series,state=state1s)
	
	cb.const.val <- tclVar(as.character(gal.params$use.method$Values[2]))
	cb.const.chk <- tkcheckbutton(fr.B0,variable=cb.const.val,text='Consistency check',anchor='w',justify='left')
	infobulle(cb.const.chk,'Tmin cannot exceed Tmax\nfor the same observing period')
	status.bar.display(cb.const.chk,txt.stbr1,'Tmin cannot exceed Tmax for the same observing period')
	tkgrid(cb.const.chk,row=1,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	
	cb.1uselv.val <- tclVar(as.character(gal.params$use.method$Values[3]))
	cb.1uselv <- tkcheckbutton(fr.B0,variable=cb.1uselv.val,text='Use Elevation',state=state[2],anchor='w',justify='left')
	infobulle(cb.1uselv,'Check to use elevation data\n to choose neighbors stations')
	status.bar.display(cb.1uselv,txt.stbr1,'Check for using elevation data to choose neighbor stations')
	tkgrid(cb.1uselv,row=2,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	
	cb.1intelv<- tkradiobutton(fr.B0,text="Elevation from DEM",anchor='w',justify='left',state=state[3])
	infobulle(cb.1intelv,'Check to extract\nelevation data from DEM')
	status.bar.display(cb.1intelv,txt.stbr1,'If no elevation data are provided for each station, must be checked')
	cb.1datelv<- tkradiobutton(fr.B0,text="Elevation from STN data",anchor='w',justify='left',state=state[3])
	infobulle(cb.1datelv,'Check to use elevation data\nfrom the data to be controled')
	status.bar.display(cb.1datelv,txt.stbr1,'Check to use elevation data from the data to be controled')
	uselv.ch <- tclVar(as.character(gal.params$use.method$Values[4]))
	tkconfigure(cb.1intelv,variable=uselv.ch,value="0")
	tkconfigure(cb.1datelv,variable=uselv.ch,value="1")
	tkgrid(cb.1intelv,row=3,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(cb.1datelv,row=4,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)

	##########

	fr.B11<-ttklabelframe(fr.B1,text="File Format",labelanchor="nw",relief="flat",borderwidth=2)
	tkgrid(fr.B11)
	ffrmt1 <- tkradiobutton(fr.B11,text="One variable",state=state1,anchor='w',justify='left')
	infobulle(ffrmt1,'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1,txt.stbr1,'In case of single serie: The file contains 1 variable')
	ffrmt2 <- tkradiobutton(fr.B11,text="Rain Tmax Tmin",state=state1,anchor='w',justify='left')
	infobulle(ffrmt2,'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2,txt.stbr1,'In case of single serie:The file contains Rain, Tmax and Tmin in this order')
	tkgrid(ffrmt1,row=0,column=0,sticky="we")
	tkgrid(ffrmt2,row=1,column=0,sticky="we")
	rbffrmt <- tclVar(as.character(gal.params$file.date.format$Values[1]))
	tkconfigure(ffrmt1,variable=rbffrmt,value="1")
	tkconfigure(ffrmt2,variable=rbffrmt,value="0")

	fr.B21<-ttklabelframe(fr.B2,text="Dates Format",labelanchor="nw",relief="flat",borderwidth=2)
	tkgrid(fr.B21)
	vdtfrmt1<-tclVar("YYYYMMDD")
	dtfrmt1 <- tkradiobutton(fr.B21,text=tclvalue(vdtfrmt1),textvariable=vdtfrmt1,state=state1,anchor='w',justify='left')
	infobulle(dtfrmt1,'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1,txt.stbr1,'In case of single serie: dates are merged')
	vdtfrmt2<-tclVar("YYYY MM DD")
	dtfrmt2 <- tkradiobutton(fr.B21,text=tclvalue(vdtfrmt2),textvariable=vdtfrmt2,state=state1,anchor='w',justify='left')
	infobulle(dtfrmt2,'In case of single serie:\ndates are separated by space\nor tabulation')
	status.bar.display(dtfrmt2,txt.stbr1,'In case of single serie:dates are separated by space or tabulation')
	tkgrid(dtfrmt1,row=0,column=0,sticky="we")
	tkgrid(dtfrmt2,row=1,column=0,sticky="we")
	rbdtfrmt <- tclVar(as.character(gal.params$file.date.format$Values[2]))
	tkconfigure(dtfrmt1,variable=rbdtfrmt,value="1")
	tkconfigure(dtfrmt2,variable=rbdtfrmt,value="0")

	tkbind(cb.period,"<<ComboboxSelected>>",function(){
		if(tclvalue(file.period)=='Daily data'){
			tclvalue(vdtfrmt1)<-"YYYYMMDD"
			tclvalue(vdtfrmt2)<-"YYYY MM DD"
		}
		if(tclvalue(file.period)=='Dekadal data'){
			tclvalue(vdtfrmt1)<-"YYYYMMD"
			tclvalue(vdtfrmt2)<-"YYYY MM D"
		}
		if(tclvalue(file.period)=='Monthly data'){
			tclvalue(vdtfrmt1)<-"YYYYMM"
			tclvalue(vdtfrmt2)<-"YYYY MM"
		}
	})		
	
	##################################
	tkbind(cb.1series,"<Button-1>",function(){
		if(gal.params$AllOrOne=='one'){
			tkconfigure(cb.file.elv,state='disabled')
			tkconfigure(bt.file.elv,state='disabled')
			tkconfigure(cb.1uselv,state='normal')
			tkconfigure(cb.1intelv,state='disabled')
			tkconfigure(cb.1datelv,state='disabled')
			tclvalue(cb.1uselv.val) <- "0"
			if(tclvalue(cb.1series.val)=="0"){
				tkconfigure(ffrmt1,state='normal')
				tkconfigure(ffrmt2,state='normal')
				tkconfigure(dtfrmt1,state='normal')
				tkconfigure(dtfrmt2,state='normal')
			}else{
				tkconfigure(ffrmt1,state='disabled')
				tkconfigure(ffrmt2,state='disabled')
				tkconfigure(dtfrmt1,state='disabled')
				tkconfigure(dtfrmt2,state='disabled')
			}
		}
	})
	####
	tkbind(cb.1uselv,"<Button-1>",function(){
		tkconfigure(ffrmt1,state='disabled')
		tkconfigure(ffrmt2,state='disabled')
		tkconfigure(dtfrmt1,state='disabled')
		tkconfigure(dtfrmt2,state='disabled')
		tclvalue(cb.1series.val) <- "0"
		if(tclvalue(cb.1uselv.val)=="0"){
			if(tclvalue(uselv.ch)=="0") state1<-'normal'
			else state<-'disabled'
			tkconfigure(cb.file.elv,state=state1)
			tkconfigure(bt.file.elv,state=state1)
			tkconfigure(cb.1uselv,state='normal')
			tkconfigure(cb.1intelv,state='normal')
			tkconfigure(cb.1datelv,state='normal')
		}else{
			tkconfigure(cb.file.elv,state='disabled')
			tkconfigure(bt.file.elv,state='disabled')
			tkconfigure(cb.1uselv,state='normal')
			tkconfigure(cb.1intelv,state='disabled')
			tkconfigure(cb.1datelv,state='disabled')
		}
	})

	###
	tkbind(cb.1intelv,"<Button-1>",function(){
		if(tclvalue(cb.1uselv.val)=="1"){
			tkconfigure(cb.file.elv,state='normal')
			tkconfigure(bt.file.elv,state='normal')
		}
	})	
	tkbind(cb.1datelv,"<Button-1>",function(){
		if(tclvalue(cb.1uselv.val)=="1"){
			tkconfigure(cb.file.elv,state='disabled')
			tkconfigure(bt.file.elv,state='disabled')
		}
	})	
	###
	tkbind(cb.const.chk,"<Button-1>",function(){
		if(tclvalue(cb.const.val)=="1"){
			tkconfigure(cb.file.stn2,state='disabled')
			tkconfigure(bt.file.stn2,state='disabled')
		}else{
			tkconfigure(cb.file.stn2,state='normal')
			tkconfigure(bt.file.stn2,state='normal')
		}
	})
	
	###################################################
	bt.opt.OK<-tkbutton(frButt, text="OK") 
	bt.opt.CA<-tkbutton(frButt, text="Cancel")
	tkgrid(bt.opt.OK,row=0,column=0,padx=5,pady=5,ipadx=5,sticky='w')
	tkgrid(bt.opt.CA,row=0,column=1,padx=5,pady=5,sticky='e')
	
	tkconfigure(bt.opt.OK,command=function(){
		if(tclvalue(file.choix1a)==""){
			tkmessageBox(message="Choose the file to control",icon="warning",type="ok")
		}else if(tclvalue(cb.1series.val)=="1" & tclvalue(rbffrmt)=="1" & tclvalue(cb.const.val)=="1" & tclvalue(file.choix1b)==""){
			msgtxtn<-if(tclvalue(vartxtn)=="1") 'Tmin' else  'Tmax'
			tkmessageBox(message=paste("Provide the file contaning",msgtxtn,'\n for the consistency check'),icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(cb.1series.val)=="0" & tclvalue(cb.const.val)=="1" & tclvalue(file.choix1b)==""){
			msgtxtn<-if(tclvalue(vartxtn)=="1") 'Tmin' else  'Tmax'
			tkmessageBox(message=paste("Provide the file contaning",msgtxtn,'\n for the consistency check'),icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(cb.1uselv.val)=="1" & tclvalue(uselv.ch)=="0" & tclvalue(file.choix2)==""){
			tkmessageBox(message="You have to choose DEM data in NetCDF format\n if you want to extract elevation from DEM",
			icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Choose a directory to save results",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
			jfile<-which(all.open.file==tclvalue(file.choix1a))
			if(length(jfile)==0){
				tkmessageBox(message="Station data not found or in the wrong format",icon="warning",type="ok")
				tkwait.window(tt)
			}

			if(tclvalue(cb.const.val)=="1"){
				if(tclvalue(cb.1series.val)=="0" | (tclvalue(cb.1series.val)=="1" & tclvalue(rbffrmt)=="1")){
					 jfile1<-which(all.open.file==tclvalue(file.choix1b))
					if(length(jfile1)==0){
						tkmessageBox(message="Data to be used for the consistency check not found or in the wrong format",icon="warning",type="ok")
						tkwait.window(tt)
					}
				}
			}

			##
			dirQcTemp<-file.path(tclvalue(file.save1), paste('QcTemp',getf.no.ext(tclvalue(file.choix1a)),sep='_'),fsep = .Platform$file.sep)
			dirparams<-file.path(dirQcTemp,'OriginalData',fsep = .Platform$file.sep)
			if(!file.exists(dirparams)) dir.create(dirparams,showWarnings=FALSE,recursive=TRUE)
			fileparams<-file.path(dirparams, 'Parameters.RData',fsep = .Platform$file.sep)

			gal.params$file.io$Values<<-c(tclvalue(file.choix1a),tclvalue(file.choix1b),tclvalue(file.choix2),tclvalue(file.save1))
			gal.params$file.date.format$Values<<-c(tclvalue(rbffrmt),tclvalue(rbdtfrmt))
			gal.params$use.method$Values<<-c(tclvalue(cb.1series.val),tclvalue(cb.const.val),tclvalue(cb.1uselv.val),tclvalue(uselv.ch))
			gal.params$period<<-ifelse(tclvalue(file.period)=='Daily data','daily',ifelse(tclvalue(file.period)=='Dekadal data','dekadal','monthly'))
			gal.params$test.tx<<-tclvalue(vartxtn)

			######
			getInitDataParams<-function(gal.params){
				if(tclvalue(cb.1series.val)=="0"){
					donstn<-splitCDTData(file.opfiles[[jfile]][[2]],gal.params$period)
					donOut<-list(donstn)
					parsFile<-list(file.opfiles[[jfile]][3:4])
					if(tclvalue(cb.const.val)=="1"){
						donstn1<-splitCDTData(file.opfiles[[jfile1]][[2]],gal.params$period)
						donOut<-list(donstn,donstn1)
						parsFile<-list(file.opfiles[[jfile]][3:4],file.opfiles[[jfile1]][3:4])
					} 
					if(!is.null(donstn)){
						# limUL<-apply(donstn$data,2,function(x){
						# 	x<-x[!is.na(x)]
						# 	Q<-quantile(x,prob=c(0.0027,0.25,0.75,0.9973),names=F)
						# 	c(round(Q[1]-3*(Q[3]-Q[2])),round(Q[4]+3*(Q[3]-Q[2])))
						# })
						# limUL<-t(limUL)
						# limLo<-ifelse(is.na(limUL[,1]),min(limUL[,1],na.rm=T),limUL[,1])
						# limLo<-as.vector(limLo)
						# limUp<-ifelse(is.na(limUL[,2]),max(limUL[,2],na.rm=T),limUL[,2])
						# limUp<-as.vector(limUp)
						# limControl<-data.frame(donstn$id,limLo,limUp,donstn$lon,donstn$lat)

						limControl<-data.frame(donstn$id,-40,60,donstn$lon,donstn$lat)
						names(limControl)<-c('Station ID','Lower Bounds','Upper Bounds','Lon','Lat')
						gal.params$parameter[[2]]<-limControl
						#gal.params$parameter[[2]]<-getTempInitParams(donstn)
						stn.choix<-as.character(donstn$id)
						xycrds<-paste(c(as.character(donstn$lon),as.character(donstn$lat)),sep='',collapse=' ')
					}else tkwait.window(tt)
				}else{
					donstn<-splitTsData(file.opfiles[[jfile]][[2]],gal.params$period,tclvalue(rbffrmt),tclvalue(rbdtfrmt))
					donOut<-list(donstn)
					parsFile<-list(file.opfiles[[jfile]][3:4])
					if(tclvalue(cb.const.val)=="1" & tclvalue(rbffrmt)=="1"){
						donstn1<-splitTsData(file.opfiles[[jfile1]][[2]],gal.params$period,tclvalue(rbffrmt),tclvalue(rbdtfrmt))
						donOut<-list(donstn,donstn1)
						parsFile<-list(file.opfiles[[jfile]][3:4],file.opfiles[[jfile1]][3:4])
					} 
					if(!is.null(donstn)){
						# if(donstn$nbvar==1) xval<-donstn$var$var
						# else{
						# 	if(tclvalue(vartxtn)=='1') xval<-donstn$var$tx
						# 	else xval<-donstn$var$tn
						# } 
						# xval<-xval[!is.na(xval)]
						# Quant<-quantile(xval,prob=c(0.0027,0.25,0.75,0.9973),names=F)
						# limLo<-round(Q[1]-3*(Q[3]-Q[2]))
						# limUp<-round(Q[4]+3*(Q[3]-Q[2]))

						stn.choix<-getf.no.ext(tclvalue(file.choix1a))
						limControl<-data.frame(stn.choix,-40,60)
						names(limControl)<-c('Station ID','Lower Bounds','Upper Bounds')
						gal.params$parameter[[2]]<-limControl
					}else tkwait.window(tt)
					xycrds<-NULL
				}
				paramsGAL<-list(inputPars=gal.params,dataPars=parsFile,data=donOut)
				save(paramsGAL,file=fileparams)
				return(list(paramsGAL,stn.choix,xycrds))
			}

			#####
			getConsistData<-function(){
				if(tclvalue(cb.1series.val)=="0") donstn1<-splitCDTData(file.opfiles[[jfile1]][[2]],gal.params$period)
				else{
					donstn1<-splitTsData(file.opfiles[[jfile1]][[2]],gal.params$period,tclvalue(rbffrmt),tclvalue(rbdtfrmt))
				} 
				if(is.null(donstn1)){
					tkmessageBox(message="Data to be used for the consistency check not found or in the wrong format",icon="warning",type="ok")
					tkwait.window(tt)	
				}
				load(fileparams)
				paramsGAL$data[[2]]<-donstn1
				paramsGAL$dataPars[[2]]<-file.opfiles[[jfile1]][3:4]
				save(paramsGAL,file=fileparams)
				return(paramsGAL)
			}

			######
			xtest1<- tclvalue(cb.1series.val)=="0" & tclvalue(cb.const.val)=="1"
			xtest2<- tclvalue(cb.1series.val)=="1" & tclvalue(cb.const.val)=="1" & tclvalue(rbffrmt)=="1"

			if(file.exists(fileparams)){
				load(fileparams)
				intest1<- paramsGAL$inputPars$period==gal.params$period & all(file.opfiles[[jfile]][3:4]%in%paramsGAL$dataPars[[1]])
				if(intest1){
					donstn<-paramsGAL$data[[1]]
					if(xtest1 | xtest2){
						if(length(paramsGAL$dataPars)==1){
							paramsGAL<-getConsistData()
						}else{
							ctest1<- all(file.opfiles[[jfile1]][3:4]%in%paramsGAL$dataPars[[2]])
							if(!ctest1) paramsGAL<-getConsistData()
						} 
						donstn1<-paramsGAL$data[[2]]
					}
					gal.params$parameter[[2]]<<-paramsGAL$inputPars$parameter[[2]]
					stn.choix<<-as.character(gal.params$parameter[[2]][,1])
					if(tclvalue(cb.1series.val)=="0"){ 
						tclvalue(XYCoordinates)<<-paste(c(as.character(gal.params$parameter[[2]][,4]),as.character(gal.params$parameter[[2]][,5])),sep='',collapse=' ')
					}
					rm(paramsGAL)
				}else{
					retDonPar<-getInitDataParams(gal.params)
					donstn<-retDonPar[[1]]$data[[1]]
					if(xtest1 | xtest2) donstn1<-retDonPar[[1]]$data[[2]]
					gal.params<<-retDonPar[[1]]$inputPars
					stn.choix<<-retDonPar[[2]]
					if(tclvalue(cb.1series.val)=="0") tclvalue(XYCoordinates)<<-retDonPar[[3]]
					rm(retDonPar)
				}
			}else{
				retDonPar<-getInitDataParams(gal.params)
				donstn<-retDonPar[[1]]$data[[1]]
				if(xtest1 | xtest2) donstn1<-retDonPar[[1]]$data[[2]]
				gal.params<<-retDonPar[[1]]$inputPars
				stn.choix<<-retDonPar[[2]]
				if(tclvalue(cb.1series.val)=="0") tclvalue(XYCoordinates)<<-retDonPar[[3]]
				rm(retDonPar)
			} 
			assign('donnees1',donstn,envir=EnvQcOutlierData)
			if(xtest1 | xtest2 ) assign('donnees2',donstn1,envir=EnvQcOutlierData)
			assign('baseDir',dirQcTemp,envir=EnvQcOutlierData)

			##################
			##set choix stn

			if(gal.params$retpar==0){
				if(stn.choix[1]!='') tclvalue(stn.choix.val)<-stn.choix[1]
				else tclvalue(stn.choix.val)<-stn.choix[2]
			}else{
				istn<-as.numeric(tclvalue(tcl(stn.choix.cb,"current")))+1
				if(istn>0) istn<-istn
				else istn<-1
				tclvalue(stn.choix.val)<-stn.choix[istn]
			}
			
			tkconfigure(stn.choix.cb,values=stn.choix, textvariable=stn.choix.val)
			if(gal.params$AllOrOne=='one'){
				tkconfigure(setting.button,state='normal')
				stateReplaceAll<-'disabled'
			} 
			if(gal.params$AllOrOne=='all'){
				tkconfigure(setting.button,state='disabled')
				stateReplaceAll<-'normal'
			} 
			if(tclvalue(cb.1series.val)=='0'){
				tkconfigure(stn.choix.prev,state='normal')
				tkconfigure(stn.choix.next,state='normal')
			}
			tkconfigure(spinH,state='normal')
			tkconfigure(spinV,state='normal')
			
			####button command
			if(is.null(lcmd.frame_qc)){
				lcmd.frame<<-QcCmdBut(stateReplaceAll)
				lcmd.frame_qc<<-1
			}
			gal.params$retpar<<-gal.params$retpar+1
			
			######
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.opt.CA,command=function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frDialog,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frButt,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	###############################################################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt))
	tt.h<-as.integer(tkwinfo("reqheight",tt))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt)
	tkwm.title(tt,'Quality Control - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(gal.params)
}

###################################################################################################
##Edit parameter temperature
get.param.temperature<-function(tt,gal.params,state.parm){
	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)
	
	fr.C<-tkframe(tt1,relief="sunken",borderwidth=2)
	fr.F<-tkframe(tt1)
	tkgrid(fr.C,row=0,column=0,sticky='ew',padx=5,pady=5,ipadx=1,ipady=1)
	tkgrid(fr.F,row=2,column=0,sticky='ew',padx=15,pady=2,ipadx=1,ipady=1)

	min.stn.l<-tklabel.h(fr.C,'Min.stn',txt.stbr1,'Minimum number of\n neighbor stations to use','Minimum number of neighbor stations to use')
	max.stn.l<-tklabel.h(fr.C,'Max.stn',txt.stbr1,'Maximum number of\n neighbor stations to use','Maximum number of neighbor stations to use')
	
	file.period<-get("file.period", parent.frame())
	if(tclvalue(file.period)=='Daily data') xper<-'days'
	if(tclvalue(file.period)=='Dekadal data') xper<-'dekad'
	if(tclvalue(file.period)=='Monthly data') xper<-'months'
	xperHelp<-paste('Time window: number of',xper,'applied to form a regression')
	win.len.l<-tklabel.h(fr.C,'Win.len',txt.stbr1,xperHelp,xperHelp)
	
	CInt.l<-tklabel.h(fr.C,'Conf.lev(%)',txt.stbr1,'Confidence level (%)','Confidence level  (%)')
	max.dist.l<-tklabel.h(fr.C,'Max.dist(km)',txt.stbr1,'Maximum distance of \n neighbor stations to use (km)','Maximum distance of neighbor stations to use (km)')
	elv.diff.l<-tklabel.h(fr.C,'Elv.diff(m)',txt.stbr1,'Maximum altitude difference of \n neighbor stations to use (m)','Maximum altitude difference of neighbor stations to use (m)')

	min.stn.v<-tkentry.h(fr.C,txt.stbr1,'Minimum number of \n neighbor stations to use','Minimum number of neighbor stations to use')
	max.stn.v<-tkentry.h(fr.C,txt.stbr1,'Maximum number of\n neighbor stations to use','Maximum number of neighbor stations to use')
	win.len.v<-tkentry.h(fr.C,txt.stbr1,xperHelp,xperHelp)
	
	CInt.v<-tkentry.h(fr.C,txt.stbr1,'Confidence level (%)','Confidence level (%)')
	max.dist.v<-tkentry.h(fr.C,txt.stbr1,'Maximum distance of\n neighbor stations to use (km)','Maximum distance of neighbor stations to use (km)')
	elv.diff.v<-tkentry.h(fr.C,txt.stbr1,'Maximum altitude difference of \n neighbor stations to use (m)','Maximum altitude difference of neighbor stations to use (m)')

	tkgrid(min.stn.l,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(min.stn.v,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(max.stn.l,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(max.stn.v,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(win.len.l,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(win.len.v,row=1,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(CInt.l,row=1,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(CInt.v,row=1,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(max.dist.l,row=2,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(max.dist.v,row=2,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(elv.diff.l,row=2,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(elv.diff.v,row=2,column=3,sticky='ew',padx=1,pady=1)

	tkconfigure(min.stn.l,anchor='e',justify='right')
	tkconfigure(max.stn.l,anchor='e',justify='right')
	tkconfigure(win.len.l,anchor='e',justify='right')
	tkconfigure(CInt.l,anchor='e',justify='right')
	tkconfigure(max.dist.l,anchor='e',justify='right')
	tkconfigure(elv.diff.l,anchor='e',justify='right')

	min.stn<-tclVar(as.character(gal.params$parameter[[1]]$Values[1]))
	max.stn<-tclVar(as.character(gal.params$parameter[[1]]$Values[2]))
	win.len<-tclVar(as.character(gal.params$parameter[[1]]$Values[3]))
	CInt<-tclVar(as.character(gal.params$parameter[[1]]$Values[4]))
	max.dist<-tclVar(as.character(gal.params$parameter[[1]]$Values[5]))
	elv.diff<-tclVar(as.character(gal.params$parameter[[1]]$Values[6]))
	tkconfigure(min.stn.v,width=6,textvariable=min.stn,state=state.parm[1])
	tkconfigure(max.stn.v,width=6,textvariable=max.stn,state=state.parm[1])
	tkconfigure(win.len.v,width=6,textvariable=win.len,state=state.parm[1])
	tkconfigure(CInt.v,width=6,textvariable=CInt,state=state.parm[2])
	tkconfigure(max.dist.v,width=6,textvariable=max.dist,state=state.parm[1])
	tkconfigure(elv.diff.v,width=6,textvariable=elv.diff,state=state.parm[1])

	################################
	ret.param.temp1<<-NULL
	bt.prm.OK<-tkbutton(fr.F, text=" OK ") 
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=25,pady=1,ipadx=1,ipady=1)
	tkconfigure(bt.prm.OK,command=function(){
	gal.params$parameter[[1]][,2]<<-c(as.character(tclvalue(min.stn)),as.character(tclvalue(max.stn)),as.character(tclvalue(win.len)),
	as.character(tclvalue(CInt)),as.character(tclvalue(max.dist)),as.character(tclvalue(elv.diff)))
	ret.param.temp1<<-0
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	bt.prm.CA<-tkbutton(fr.F, text="Cancel") 
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=25,pady=1,ipadx=1,ipady=1)
	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Options- Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(gal.params)
}


