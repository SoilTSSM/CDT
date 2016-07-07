rhtests_inputData<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()

	if (Sys.info()["sysname"] == "Windows") largeur<-28
	else largeur<-26

	## tkcombo width, listOpenFiles
	wtkcombo<-25

	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog<-tkframe(tt,relief='raised',borderwidth=2)
	frButt<-tkframe(tt)

	###############################################################
	fr.A<-tkframe(frDialog,relief="groove",borderwidth=2)
	fr.B<-ttklabelframe(frDialog,text='One Station data - Options',relief='groove',borderwidth=2)
	tkgrid(fr.A,row=0,column=0,sticky='news',padx=5,pady=5,ipadx=1,ipady=1)
	tkgrid(fr.B,row=0,column=1,sticky='news',padx=5,pady=5,ipadx=1,ipady=1)

	###############################################################
	for(i in 0:2) assign(paste('fr.A',i,sep=''),tkframe(fr.A,relief='sunken',borderwidth=2))
	for(i in 0:2) tkgrid(get(paste('fr.A',i,sep='')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.A',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)


	##########################
	for(i in 0:4) assign(paste('fr.A0',i,sep=''),tkframe(fr.A0))
	for(i in 0:4) tkgrid(get(paste('fr.A0',i,sep='')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A0',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period)=='daily','Daily data',ifelse(as.character(GeneralParameters$period)=='dekadal','Dekadal data','Monthly data'))

	cb.period<-ttkcombobox(fr.A00, values=c('Daily data','Dekadal data','Monthly data'),textvariable=file.period,width=wtkcombo)
	infobulle(cb.period,'Choose the time step of the data')
	status.bar.display(cb.period,TextOutputVar,'Choose the time step of the data')
	tkgrid(cb.period)


	#########
	homo.file.stn1<-tklabel(fr.A01,text='Data containing the canditate series')
	tkgrid(homo.file.stn1)

	file.choix1a <- tclVar()
	tclvalue(file.choix1a) <-as.character(GeneralParameters$file.io$Values[1])
	file.choix1b <- tclVar()
	tclvalue(file.choix1b) <-as.character(GeneralParameters$file.io$Values[2])

	cb.file.stn1<-ttkcombobox(fr.A02, values=unlist(listOpenFiles), textvariable=file.choix1a,width=wtkcombo)
	infobulle(cb.file.stn1,'Choose the file in the list')
	status.bar.display(cb.file.stn1,TextOutputVar,'Choose the file containing the candidate series')

	bt.file.stn1<-tkbutton.h(fr.A02, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.file.stn1,bt.file.stn1)
	tkgrid.configure(cb.file.stn1,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.stn1,row=0,column=1,sticky='e')

	tkconfigure(bt.file.stn1,command=function(){
		dat.opfiles<-getOpenFiles(parent.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix1a)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1,values=unlist(listOpenFiles), textvariable=file.choix1a)
			tkconfigure(cb.file.stn2,values=unlist(listOpenFiles), textvariable=file.choix1b)
		}else{
			return(NULL)
		}
	})
	#####################################
	if(as.character(GeneralParameters$single.series)=='1' & as.character(GeneralParameters$use.ref.series)=='1'){
		homo.state<-'normal'
	}else{
		homo.state<-'disabled'
	}

	homo.file.stn2<-tklabel(fr.A03,text='Reference series')
	tkgrid(homo.file.stn2)

	cb.file.stn2<-ttkcombobox(fr.A04, values=unlist(listOpenFiles), textvariable=file.choix1b,width=wtkcombo)
	infobulle(cb.file.stn2,'Choose the reference series in the list')
	status.bar.display(cb.file.stn2,TextOutputVar,'Choose the file containing the reference series')

	bt.file.stn2<-tkbutton.h(fr.A04, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.file.stn2,bt.file.stn2)

	tkgrid.configure(cb.file.stn2,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.stn2,row=0,column=1,sticky='e')
	tkconfigure(cb.file.stn2,state=homo.state)
	tkconfigure(bt.file.stn2,state=homo.state)

	tkconfigure(bt.file.stn2,command=function(){
		dat.opfiles<-getOpenFiles(parent.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix1b)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1,values=unlist(listOpenFiles), textvariable=file.choix1a)
			tkconfigure(cb.file.stn2,values=unlist(listOpenFiles), textvariable=file.choix1b)
		}else{
			return(NULL)
		}
	})

	##########
	fr.A11<-tkframe(fr.A1)
	fr.A12<-tkframe(fr.A1)
	tkgrid(fr.A11,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr.A12,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	frA11.txt1<-tklabel(fr.A11,text='Directory to save result')
	tkgrid(frA11.txt1)

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save<-tkentry(fr.A12,textvariable=file.save1,width=largeur)
	infobulle(en.file.save,'Enter the full path of the\ndirectory to save result')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path of the directory to save result')
	bt.file.save<-tkbutton.h(fr.A12, text="...",TextOutputVar,'or browse here','')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1,'does not exist.\n It will be created.',sep=' '),icon="warning",type="ok")
			dir.create(file2save1,recursive=TRUE)
			tclvalue(file.save1)<-file2save1
		}else tclvalue(file.save1)<-file2save1
	})

###################################
	computefun.l<-tklabel(fr.A2,text='Aggregation')

	cmptfun<-tclVar(as.character(GeneralParameters$compute.var$Values[1]))
	cb.cmptfun<-ttkcombobox(fr.A2, values=c("mean","sum"),textvariable=cmptfun,width=5)
	infobulle(cb.cmptfun,'Function to be used to compute\ndekadal and monthly series')
	status.bar.display(cb.cmptfun,TextOutputVar,'Function to be used to compute dekadal and monthly series')

	missfrac.l<-tklabel(fr.A2,text='Min.frac')

	miss.frac<-tclVar(as.character(GeneralParameters$compute.var$Values[2]))
	missfrac.v<-tkentry(fr.A2,textvariable=miss.frac,width=5)
	infobulle(missfrac.v,'Minimum fraction of available data\nthat must be present for the time period\nto compute')
	status.bar.display(missfrac.v,TextOutputVar,'Minimum fraction of available data that must be present for the time period to compute')

	tkgrid(computefun.l,cb.cmptfun,missfrac.l,missfrac.v)



	#################################################
	if(as.character(GeneralParameters$single.series)=='1'){
		ffdt.state<-'normal'
	}else{
		ffdt.state<-'disabled'
	}


	for(i in 0:2) assign(paste('fr.B',i,sep=''),tkframe(fr.B,relief='sunken',borderwidth=2))
	for(i in 0:2) tkgrid(get(paste('fr.B',i,sep='')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.B',i,sep='')),row=i,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	cb.1series.val <- tclVar(as.character(GeneralParameters$single.series))
	cb.1series <- tkcheckbutton(fr.B0,variable=cb.1series.val,text='Single Series',anchor='w',justify='left')
	infobulle(cb.1series,'Homogenization for one station series')
	status.bar.display(cb.1series,TextOutputVar,'The data is a series of one station')

	cb.rfseries.val <- tclVar(as.character(GeneralParameters$use.ref.series))
	cb.rfseries <- tkcheckbutton(fr.B0,variable=cb.rfseries.val,text='Use reference series',anchor='w',justify='left')
	infobulle(cb.rfseries,'Using a reference series to\nperform the homogenization test')
	status.bar.display(cb.rfseries,TextOutputVar,'Using a reference series to perform the homogenization test')

	tkgrid(cb.1series,row=0,column=0,sticky="we")
	tkgrid(cb.rfseries,row=1,column=0,sticky="we")


	ffrmtLab<-tklabel(fr.B1,text='File Format',anchor='w',justify='left')

	ffrmt1 <- tkradiobutton(fr.B1,text="One variable",anchor='w',justify='left',state=ffdt.state)
	infobulle(ffrmt1,'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1,TextOutputVar,'In case of single serie: The file contains 1 variable')

	ffrmt2 <- tkradiobutton(fr.B1,text="Rain Tmax Tmin",anchor='w',justify='left',state=ffdt.state)
	infobulle(ffrmt2,'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2,TextOutputVar,'In case of single serie:The file contains Rain, Tmax and Tmin in this order')
	tkgrid(ffrmtLab,row=0,column=0,sticky="we")
	tkgrid(ffrmt1,row=1,column=0,sticky="we")
	tkgrid(ffrmt2,row=2,column=0,sticky="we")

	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))
	tkconfigure(ffrmt1,variable=rbffrmt,value="1")
	tkconfigure(ffrmt2,variable=rbffrmt,value="0")

	varframe<-tkframe(fr.B1)
	tkgrid(varframe,row=3,column=0,sticky="e")
	infobulle(varframe,'Choose the variable to test')
	status.bar.display(varframe,TextOutputVar,'Choose the variable to test')

	var2test1 <- tkradiobutton(varframe,state=ffdt.state)
	var2test2 <- tkradiobutton(varframe,state=ffdt.state)
	var2test3 <- tkradiobutton(varframe,state=ffdt.state)

	tkgrid(var2test1,row=0,column=0,padx=2)
	tkgrid(var2test2,row=0,column=1,padx=2)
	tkgrid(var2test3,row=0,column=2,padx=2)
	varcat <- tclVar(as.character(GeneralParameters$file.date.format$Values[3]))
	tkconfigure(var2test1,variable=varcat,value="1")
	tkconfigure(var2test2,variable=varcat,value="2")
	tkconfigure(var2test3,variable=varcat,value="3")

	dtfrmtLab<-tklabel(fr.B2,text='Dates Format',anchor='w',justify='left')

	vdtfrmt1<-tclVar("YYYYMMD")
	dtfrmt1 <- tkradiobutton(fr.B2,text=tclvalue(vdtfrmt1),textvariable=vdtfrmt1,anchor='w',justify='left',state=ffdt.state)
	infobulle(dtfrmt1,'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1,TextOutputVar,'In case of single serie: dates are merged')

	vdtfrmt2<-tclVar("YYYY MM D")
	dtfrmt2 <- tkradiobutton(fr.B2,text=tclvalue(vdtfrmt2),textvariable=vdtfrmt2,anchor='w',justify='left',state=ffdt.state)
	infobulle(dtfrmt2,'In case of single serie:\ndates are separated by space,\ntabulation or CSV format')
	status.bar.display(dtfrmt2,TextOutputVar,'In case of single serie: dates are separated by space, tabulation or CSV format')

	tkgrid(dtfrmtLab,row=0,column=0,sticky="we")
	tkgrid(dtfrmt1,row=1,column=0,sticky="we")
	tkgrid(dtfrmt2,row=2,column=0,sticky="we")

	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))
	tkconfigure(dtfrmt1,variable=rbdtfrmt,value="1")
	tkconfigure(dtfrmt2,variable=rbdtfrmt,value="0")

	###############################################################
	tkbind(cb.rfseries,"<Button-1>",function(){
	if(tclvalue(cb.rfseries.val)=="0" & tclvalue(cb.1series.val)=="1"){
			tkconfigure(cb.file.stn2,state='normal')
			tkconfigure(bt.file.stn2,state='normal')
		}else if(tclvalue(cb.1series.val)=="1"){
			homo.state<-'disabled'
			tkconfigure(cb.file.stn2,state=homo.state)
			tkconfigure(bt.file.stn2,state=homo.state)
		}else{
			tkconfigure(cb.file.stn2,state='disabled')
			tkconfigure(bt.file.stn2,state='disabled')
		}
	})

	############################
	tkbind(cb.1series,"<Button-1>",function(){
		if(tclvalue(cb.1series.val)=="0" & tclvalue(cb.rfseries.val)=="1"){
			tkconfigure(cb.file.stn2,state='normal')
			tkconfigure(bt.file.stn2,state='normal')
		}else{
			tkconfigure(cb.file.stn2,state='disabled')
			tkconfigure(bt.file.stn2,state='disabled')
		}
		##
		if(tclvalue(cb.1series.val)=="0"){
			tkconfigure(ffrmt1,state='normal')
			tkconfigure(ffrmt2,state='normal')
			tkconfigure(var2test1,state='normal')
			tkconfigure(var2test2,state='normal')
			tkconfigure(var2test3,state='normal')
			tkconfigure(dtfrmt1,state='normal')
			tkconfigure(dtfrmt2,state='normal')
		}else{
			tkconfigure(ffrmt1,state='disabled')
			tkconfigure(ffrmt2,state='disabled')
			tkconfigure(var2test1,state='disabled')
			tkconfigure(var2test2,state='disabled')
			tkconfigure(var2test3,state='disabled')
			tkconfigure(dtfrmt1,state='disabled')
			tkconfigure(dtfrmt2,state='disabled')
		}
	})

	############################
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

	###############################################################

	bt.opt.OK<-tkbutton(frButt, text="OK")
	bt.opt.CA<-tkbutton(frButt, text="Cancel")
	tkgrid(bt.opt.OK,row=0,column=0,padx=5,pady=5,ipadx=5,sticky='w')
	tkgrid(bt.opt.CA,row=0,column=1,padx=5,pady=5,sticky='e')

	tkconfigure(bt.opt.OK,command=function(){
		if(tclvalue(file.choix1a)==""){
			tkmessageBox(message="Provide the file to test",icon="warning",type="ok")
		}else if(tclvalue(cb.1series.val)=='1' & tclvalue(cb.rfseries.val)=='1' & tclvalue(file.choix1b)==""){
			tkmessageBox(message="Provide the file containing\nthe reference series",icon="warning",type="ok")
		}else{
			GeneralParameters$file.io$Values<<-c(tclvalue(file.choix1a),tclvalue(file.choix1b),as.character(GeneralParameters$file.io$Values[3]),tclvalue(file.save1))
			GeneralParameters$file.date.format$Values<<-c(tclvalue(rbffrmt),tclvalue(rbdtfrmt),tclvalue(varcat))
			GeneralParameters$single.series<<-tclvalue(cb.1series.val)
			GeneralParameters$use.ref.series<<-tclvalue(cb.rfseries.val)
			GeneralParameters$compute.var$Values<<-c(tclvalue(cmptfun),tclvalue(miss.frac))
			GeneralParameters$period<<-ifelse(tclvalue(file.period)=='Daily data','daily',ifelse(tclvalue(file.period)=='Dekadal data','dekadal','monthly'))
			GeneralParameters$getdata<<-TRUE

			##set choix stn
			if(tclvalue(cb.1series.val)=='0'){
				donne<-getCDTdata(file.choix1a,file.period)
				if(!is.null(donne)){
					stn.choix<<-donne$id
					tclvalue(stn.choix.val)<-stn.choix[1]
					tkconfigure(stn.choix.prev,state='normal')
					tkconfigure(stn.choix.next,state='normal')
				}else InsertMessagesTxt(main.txt.out,'Error loading station data',format=TRUE)
			}else{
				stn.choix<<-getf.no.ext(tclvalue(file.choix1a))
				tclvalue(stn.choix.val)<-stn.choix
			}
			tkconfigure(stn.choix.cb,values=stn.choix, textvariable=stn.choix.val)

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.opt.CA,command=function(){
		GeneralParameters$getdata<<-FALSE
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
	tkwm.title(tt,'Input Data - Settings')
	tkwm.deiconify(tt)

	##################################################################
	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}


##############################################################################################################################


rhtests_nghbStnOpts<-function(top.win,GeneralParameters){
	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont<-tkframe(tt1,relief='raised',borderwidth=2)
	frok<-tkframe(tt1)

	frE12a<-tkframe(frcont,relief='sunken',borderwidth=2)
	tkgrid(frE12a,sticky='nswe',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=5)

	###############################################################
	min.stn.l<-tklabel(frE12a,text='Min.stn')
	infobulle(min.stn.l,'Minimum number of\nneighbor stations to use')
	status.bar.display(min.stn.l,TextOutputVar,'Minimum number of neighbor stations to use')
	max.stn.l<-tklabel(frE12a,text='Max.stn')
	infobulle(max.stn.l,'Maximum number of\nneighbor stations to use')
	status.bar.display(max.stn.l,TextOutputVar,'Maximum number of neighbor stations to use')
	max.dist.l<-tklabel(frE12a,text='Max.dist(km)')
	infobulle(max.dist.l,'Maximum distance of \nneighbor stations to use (km)')
	status.bar.display(max.dist.l,TextOutputVar,'Maximum distance of neighbor stations to use (km)')
	elv.diff.l<-tklabel(frE12a,text='Elv.diff(m)')
	infobulle(elv.diff.l,'Maximum altitude difference of \n neighbor stations to use (m)')
	status.bar.display(elv.diff.l,TextOutputVar,'Maximum altitude difference of neighbor stations to use (m)')
	min.rho.l<-tklabel(frE12a,text='Min.rho')
	infobulle(min.rho.l,'Minimum correlation coef between candidate\nand neighbor series')
	status.bar.display(min.rho.l,TextOutputVar,'Minimum correlation coef between candidate and neighbor series')

	min.stn<-tclVar(as.character(GeneralParameters$ref.series.choix$Values[4]))
	max.stn<-tclVar(as.character(GeneralParameters$ref.series.choix$Values[5]))
	max.dist<-tclVar(as.character(GeneralParameters$ref.series.choix$Values[6]))
	elv.diff<-tclVar(as.character(GeneralParameters$ref.series.choix$Values[7]))
	min.rho<-tclVar(as.character(GeneralParameters$ref.series.choix$Values[8]))

	min.stn.v<-tkentry(frE12a,width=8,textvariable=min.stn)
	infobulle(min.stn.v,'Minimum number of\nneighbor stations to use')
	status.bar.display(min.stn.v,TextOutputVar,'Minimum number of neighbor stations to use')
	max.stn.v<-tkentry(frE12a,width=8,textvariable=max.stn)
	infobulle(max.stn.v,'Maximum number of\nneighbor stations to use')
	status.bar.display(max.stn.v,TextOutputVar,'Maximum number of neighbor stations to use')
	max.dist.v<-tkentry(frE12a,width=8,textvariable=max.dist)
	infobulle(max.dist.v,'Maximum distance of \nneighbor stations to use (km)')
	status.bar.display(max.dist.v,TextOutputVar,'Maximum distance of neighbor stations to use (km)')
	elv.diff.v<-tkentry(frE12a,width=8,textvariable=elv.diff)
	infobulle(elv.diff.v,'Maximum altitude difference of \n neighbor stations to use (m)')
	status.bar.display(elv.diff.v,TextOutputVar,'Maximum altitude difference of neighbor stations to use (m)')
	min.rho.v<-tkentry(frE12a,width=8,textvariable=min.rho)
	infobulle(min.rho.v,'Minimum correlation coef between candidate\nand neighbor series')
	status.bar.display(min.rho.v,TextOutputVar,'Minimum correlation coef between candidate and neighbor series')

	tkgrid(min.stn.l,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(min.stn.v,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(max.stn.l,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(max.stn.v,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(max.dist.l,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(max.dist.v,row=1,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(elv.diff.l,row=1,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(elv.diff.v,row=1,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(min.rho.l,row=2,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(min.rho.v,row=2,column=1,sticky='ew',padx=1,pady=1)

	tkconfigure(min.stn.l,anchor='e',justify='right')
	tkconfigure(max.stn.l,anchor='e',justify='right')
	tkconfigure(max.dist.l,anchor='e',justify='right')
	tkconfigure(elv.diff.l,anchor='e',justify='right')
	tkconfigure(min.rho.l,anchor='e',justify='right')


	###############################################################
	bt.prm.OK<-tkbutton(frok, text=" OK ")
	bt.prm.CA<-tkbutton(frok, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=5,ipadx=5)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=5)

	tkconfigure(bt.prm.OK,command=function(){
		GeneralParameters$ref.series.choix$Values<<-c(as.character(GeneralParameters$ref.series.choix$Values[1:3]),
		tclvalue(min.stn),tclvalue(max.stn),tclvalue(max.dist),tclvalue(elv.diff),tclvalue(min.rho))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(frcont,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frok,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	###############################################################
	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Neighbor stations selection')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}



##############################################################################################################################


rhtests_useElv<-function(top.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()

	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont<-tkframe(tt1,relief='raised',borderwidth=2)
	frok<-tkframe(tt1)

	frE21<-tkframe(frcont,relief='sunken',borderwidth=2)
	frE22<-tkframe(frcont,relief='sunken',borderwidth=2)
	tkgrid(frE21,row=0,column=0,sticky='nswe',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=5)
	tkgrid(frE22,row=1,column=0,sticky='nswe',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=5)

	###############################################################

	uselv.dem<- tkradiobutton(frE21,text="Elevation from DEM",anchor='w',justify='left')
	infobulle(uselv.dem,'Choose to extract\nelevation data from DEM')
	status.bar.display(uselv.dem,TextOutputVar,'If no elevation data are provided for each station, must be choosen')
	uselv.dat<- tkradiobutton(frE21,text="Elevation from STN data",anchor='w',justify='left')
	infobulle(uselv.dat,'Choose to use elevation data\nfrom the data to be homogenized')
	status.bar.display(uselv.dat,TextOutputVar,'Check to use elevation data from the data to be homogenized')
	uselv.ch <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[3]))
	tkconfigure(uselv.dem,variable=uselv.ch,value="0")
	tkconfigure(uselv.dat,variable=uselv.ch,value="1")
	tkgrid(uselv.dem,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(uselv.dat,row=1,column=0,sticky='ew',padx=1,pady=1)

	frE22a<-tkframe(frE22)
	frE22b<-tkframe(frE22)
	tkgrid(frE22a,row=0,column=0,sticky='w')
	tkgrid(frE22b,row=1,column=0,sticky='w')
	frE22a.txt<-tklabel(frE22a,text='Elevation Data (NetCDF)',anchor='w',justify='left')
	tkgrid(frE22a.txt,row=0,column=0,sticky='w')

	if(as.character(GeneralParameters$ref.series.choix$Values[3])=='0') statedem<-'normal'
	else statedem<-'disabled'

	file.choix2 <- tclVar()
	tclvalue(file.choix2) <-as.character(GeneralParameters$file.io$Values[3])
	cb.file.elv<-ttkcombobox(frE22b, values=unlist(listOpenFiles), textvariable=file.choix2,state=statedem)
	infobulle(cb.file.elv,'Choose the file in the list')
	status.bar.display(cb.file.elv,TextOutputVar,'Choose the file containing the elevation data')
	bt.file.elv<-tkbutton.h(frE22b, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkconfigure(bt.file.elv,state=statedem,command=function(){
		nc.opfiles<-getOpenNetcdf(top.win,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'netcdf'
			AllOpenFilesData[[nopf+1]]<<-nc.opfiles
			tclvalue(file.choix2)<-AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv,values=unlist(listOpenFiles), textvariable=file.choix2)
		}
	})

	tkgrid(cb.file.elv,row=0,column=1,sticky='we')
	tkgrid(bt.file.elv,row=0,column=2,sticky='e')

###########

	tkbind(uselv.dat,"<Button-1>",function(){
		if(tclvalue(uselv.ch)=="0"){
			tkconfigure(cb.file.elv,state='disabled')
			tkconfigure(bt.file.elv,state='disabled')
		}
	})

	tkbind(uselv.dem,"<Button-1>",function(){
		if(tclvalue(uselv.ch)=="1"){
			tkconfigure(cb.file.elv,state='normal')
			tkconfigure(bt.file.elv,state='normal')
		}
	})


	###############################################################

	bt.prm.OK<-tkbutton(frok, text=" OK ")
	bt.prm.CA<-tkbutton(frok, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=5,ipadx=5)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=5)

	tkconfigure(bt.prm.OK,command=function(){
		if(tclvalue(file.choix2)=="" & tclvalue(uselv.ch)=='0'){
			tkmessageBox(message="You have to choose DEM data in NetCDF format\n if you want to extract elevation from DEM",icon="warning",type="ok")
		}else{
			GeneralParameters$file.io$Values<<-c(as.character(GeneralParameters$file.io$Values[1:2]),tclvalue(file.choix2),as.character(GeneralParameters$file.io$Values[4]))
			GeneralParameters$ref.series.choix$Values<<-c(as.character(GeneralParameters$ref.series.choix$Values[1]),"1",tclvalue(uselv.ch),as.character(GeneralParameters$ref.series.choix$Values[4:8]))
			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(frcont,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frok,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	###############################################################
	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Elevation data')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


