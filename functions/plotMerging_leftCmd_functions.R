PlotMergingOutputCmd<-function(){
	file.list<-openFile_ttkcomboList()

	##tkcombo& tkentry width
	#largeur<-27  
	largeur<-as.integer(w.scale(21)/sfont0)
	##tkentry nc filename format tkentry wdth
	#wncdf_ff<-19
	wncdf_ff<-as.integer(w.scale(14)/sfont0) 
	#scrollable frame width
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin<-w.scale(18)
		#hscrlwin<-h.scale(31.5)
		hscrlwin<-h.scale(27)
	}else{
		wscrlwin<-w.scale(21.7)  
		#hscrlwin<-h.scale(38.5) 
		hscrlwin<-h.scale(34) 
	}

	###################

	cmd.frame<-tkframe(panel.left)

	tknote.cmd<-bwNoteBook(cmd.frame)
	plotBut.cmd<-tkframe(cmd.frame)
	tkgrid(tknote.cmd,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2)
	tkgrid(plotBut.cmd,row=1,column=1,sticky='se',rowspan=1,columnspan=1)

	cmd.tab1 <- bwAddTab(tknote.cmd,text="General")
	cmd.tab2 <- bwAddTab(tknote.cmd,text="NetCDF data")
	cmd.tab3 <- bwAddTab(tknote.cmd,text="Options")
	bwRaiseTab(tknote.cmd,cmd.tab1)
	
	#######################################################################################################

	#Tab1
	frTab1<-tkframe(cmd.tab1)
	tkgrid(frTab1,padx=0,pady=1,ipadx=1,ipady=1)

	scrw1<-bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	subfr1<-bwScrollableFrame(scrw1,width=wscrlwin,height=hscrlwin)

	##############
	frameStn<-ttklabelframe(subfr1,text="Station data file",relief='groove')

	file.period <- tclVar()
	tclvalue(file.period) <- 'Dekadal data'
	combPrd.tab1<-ttkcombobox(frameStn, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)

	file.stnfl <- tclVar()
	combStnfl.tab1<-ttkcombobox(frameStn,values=unlist(file.list),textvariable=file.stnfl,width=largeur)
	btStnfl.tab1<-tkbutton(frameStn, text="...") 
	tkconfigure(btStnfl.tab1,command=function(){
		dat.opfiles<-getOpenFiles(main.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'ascii'
			file.opfiles[[nopf+1]]<<-dat.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]] 
			tclvalue(file.stnfl)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1,values=unlist(file.list), textvariable=file.stnfl)
			tkconfigure(combShp.tab1,values=unlist(file.list), textvariable=file.plotShp)
		}else return(NULL)
	})
	infobulle(combStnfl.tab1,'Choose the station data in the list')
	status.bar.display(combStnfl.tab1,txt.stbr1,'Choose the file containing the station data')
	infobulle(btStnfl.tab1,'Browse file if not listed')
	status.bar.display(btStnfl.tab1,txt.stbr1,'Browse file if not listed')
	#######################
	labDate.tab1<-tklabel(frameStn,text="Date",anchor='e',justify='right')
	yrsLab.tab1<-tklabel(frameStn,text='Year',anchor='w',justify='left')
	monLab.tab1<-tklabel(frameStn,text='Month',anchor='w',justify='left')
	dayLabTab1_Var<-tclVar('Dek')
	dayLab.tab1<-tklabel(frameStn,text=tclvalue(dayLabTab1_Var),textvariable=dayLabTab1_Var,anchor='w',justify='left')
	
	idate_yrs<-tclVar('1983')
	idate_mon<-tclVar('1')
	idate_day<-tclVar('1')
	
	yrs1.tab1<-tkentry(frameStn, width=4,textvariable=idate_yrs,justify = "left")
	mon1.tab1<-tkentry(frameStn, width=4,textvariable=idate_mon,justify = "left")
	day1.tab1<-tkentry(frameStn, width=4,textvariable=idate_day,justify = "left")

	obs_name<-tclVar('Observation')	
	obsLab.tab1<-tklabel(frameStn,text='Title',anchor='e',justify='right')
	obsEd.tab1<-tkentry(frameStn, width=14,textvariable=obs_name,justify = "left")
	infobulle(obsEd.tab1,'Title of the panel')
	status.bar.display(obsEd.tab1,txt.stbr1,'Title of the panel')

	unit_sym<-tclVar('mm')	
	unitLab.tab1<-tklabel(frameStn,text='Units',anchor='e',justify='right')
	unitEd.tab1<-tkentry(frameStn, width=8,textvariable=unit_sym,justify = "left")
	infobulle(unitEd.tab1,'Display unit on colorscale')
	status.bar.display(unitEd.tab1,txt.stbr1,'Display unit on colorscale')

	####################
	tkgrid(combPrd.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(combStnfl.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btStnfl.tab1,row=1,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(yrsLab.tab1,row=2,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(monLab.tab1,row=2,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(dayLab.tab1,row=2,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(labDate.tab1,row=3,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(yrs1.tab1,row=3,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(mon1.tab1,row=3,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(day1.tab1,row=3,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(unitLab.tab1,row=4,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(unitEd.tab1,row=4,column=1,sticky='we',rowspan=1,columnspan=2,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(obsLab.tab1,row=5,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(obsEd.tab1,row=5,column=1,sticky='we',rowspan=1,columnspan=4,padx=1,pady=2,ipadx=1,ipady=1)

	##############################
	frameShp<-ttklabelframe(subfr1,text="Shapefiles for boundary",relief='groove')
	
	file.plotShp <- tclVar()
	combShp.tab1<-ttkcombobox(frameShp, values=unlist(file.list), textvariable=file.plotShp,width=largeur) 
	btShp.tab1<-tkbutton(frameShp, text="...") 
	tkconfigure(btShp.tab1,command=function(){
		shp.opfiles<-getOpenShp(main.win,all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'shp'
			file.opfiles[[nopf+1]]<<-shp.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]		
			tclvalue(file.plotShp)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1,values=unlist(file.list), textvariable=file.stnfl)
			tkconfigure(combShp.tab1,values=unlist(file.list), textvariable=file.plotShp)
		}
	})
	
	###################
	tkgrid(combShp.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btShp.tab1,row=0,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)

	#############################
	tkgrid(frameStn,row=0,column=0,sticky='we')
	tkgrid(frameShp,row=1,column=0,sticky='we')

	#######################################################################################################
	tkbind(combPrd.tab1,"<<ComboboxSelected>>",function(){
		if(tclvalue(file.period)=='Daily data'){
			tclvalue(dayLabTab1_Var)<-'Day'
			tkconfigure(day1.tab1,state='normal')
		}
		if(tclvalue(file.period)=='Dekadal data'){
			tclvalue(dayLabTab1_Var)<-'Dek'
			tkconfigure(day1.tab1,state='normal')
		}
		if(tclvalue(file.period)=='Monthly data'){
			tkconfigure(day1.tab1,state='disabled')
		}
	})		

	#######################################################################################################

	addNcdfFun<-function(nfr,contFrame){
		frameNcdf<-ttklabelframe(contFrame,text=paste("NetCDF data file",nfr),relief='groove')

		file.netcdf <- tclVar()
		combNetcdf.tab2<-ttkcombobox(frameNcdf,values=unlist(file.list),textvariable=file.netcdf,width=largeur)
		btNetcdf.tab2<-tkbutton(frameNcdf, text="...") 
		tkconfigure(btNetcdf.tab2,command=function(){
			nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
			if(!is.null(nc.opfiles)){
				nopf<-length(type.opfiles)
				type.opfiles[[nopf+1]]<<-'netcdf'
				file.opfiles[[nopf+1]]<<-nc.opfiles
				file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]] 
				tclvalue(file.netcdf)<-file.opfiles[[nopf+1]][[1]]
				tkconfigure(combNetcdf.tab2,values=unlist(file.list), textvariable=file.netcdf)
			}else return(NULL)
		})
		infobulle(combNetcdf.tab2,'Choose a NetCDF data in the list')
		status.bar.display(combNetcdf.tab2,txt.stbr1,'Choose the file containing the NetCDF data')
		infobulle(btNetcdf.tab2,'Browse file if not listed')
		status.bar.display(btNetcdf.tab2,txt.stbr1,'Browse file if not listed')

		#######################
		ncdf_name<-tclVar(paste('NetCDF',nfr))	
		ncdfLab.tab2<-tklabel(frameNcdf,text='Title',anchor='e',justify='right')
		ncdfEd.tab2<-tkentry(frameNcdf, width=14,textvariable=ncdf_name,justify = "left")
		infobulle(ncdfEd.tab2,'Title of the panel')
		status.bar.display(ncdfEd.tab2,txt.stbr1,'Title of the panel')

		#######################
		tkgrid(combNetcdf.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
		tkgrid(btNetcdf.tab2,row=0,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
		tkgrid(ncdfLab.tab2,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
		tkgrid(ncdfEd.tab2,row=1,column=1,sticky='we',rowspan=1,columnspan=4,padx=1,pady=2,ipadx=1,ipady=1)

		######
		tkgrid(frameNcdf,row=nfr,column=0,sticky='we')
		return(list(list(frameNcdf,combNetcdf.tab2),file.netcdf,ncdf_name))
	}
	
	#######################################################################################################
	#Tab2	
	frTab2<-tkframe(cmd.tab2) #,relief='sunken',bd=2
	tkgrid(frTab2,padx=5,pady=5,ipadx=2,ipady=2)

	scrw2<-bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	subfr2<-bwScrollableFrame(scrw2,width=wscrlwin,height=hscrlwin)

	dataNCDF<-list()
	#########################
	addNcdfData.tab2<-tkbutton(subfr2,text='Add Other NetCDF data')
	dataNCDF[[1]]<-addNcdfFun(1,subfr2)
	jCDF<-2
	tkconfigure(addNcdfData.tab2,command=function(){
		dataNCDF[[jCDF]]<<-addNcdfFun(jCDF,subfr2)
		for(jj in jCDF:1) tkconfigure(dataNCDF[[jj]][[1]][[2]],values=unlist(file.list))
		jCDF<<-jCDF+1
	})
	
	##########
	tkgrid(addNcdfData.tab2,row=0,column=0,sticky='we',pady=2)
	
	#######################################################################################################

	#Tab3	
	frTab3<-tkframe(cmd.tab3) #,relief='sunken',bd=2
	tkgrid(frTab3,padx=5,pady=5,ipadx=2,ipady=2)

	scrw3<-bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	subfr3<-bwScrollableFrame(scrw3,width=wscrlwin,height=hscrlwin)

	wPreview<-wscrlwin-10
	nb.color<-tclVar('10')
	preset.color <- tclVar()
	tclvalue(preset.color) <- 'tim.colors'

	labPresetCol.tab3<-tklabel(subfr3,text='Presets colorkey',anchor='w',justify='left')
	combPresetCol.tab3<-ttkcombobox(subfr3,values=c('tim.colors','rainbow','heat.colors','cm.colors','topo.colors','terrain.colors'), textvariable=preset.color,width=13)
	nbPresetCol.tab3<-tkentry(subfr3, width=3,textvariable=nb.color,justify = "left")

	reverse.color <- tclVar(0)
	labRevCol.tab3<-tklabel(subfr3,text='Reverse',anchor='e',justify='right')
	chkRevCol.tab3<-tkcheckbutton(subfr3,variable=reverse.color,anchor='w',justify='left')

	sep1.tab3<-ttkseparator(subfr3)
	previewPresetCol.tab3<-tkcanvas(subfr3,width=wPreview,height=20,bg='white')

	sep2.tab3<-ttkseparator(subfr3)
	custom.color <- tclVar(0)
	chkCustoCol.tab3<-tkcheckbutton(subfr3,variable=custom.color,text='User customized  colorkey',anchor='w',justify='left')
	butCustoCol.tab3<-tkbutton(subfr3, text="Custom",state='disabled')

	sep3.tab3<-ttkseparator(subfr3)
	custom.level <- tclVar(0)
	chkCustoLev.tab3<-tkcheckbutton(subfr3,variable=custom.level,text='User customized  levels',anchor='w',justify='left')
	butCustoLev.tab3<-tkbutton(subfr3, text="Custom",state='disabled')

	infobulle(combPresetCol.tab3,'Predefined color palettes')
	status.bar.display(combPresetCol.tab3,txt.stbr1,'Predefined color palettes')
	infobulle(nbPresetCol.tab3,'Number of color levels to be in the palette')
	status.bar.display(nbPresetCol.tab3,txt.stbr1,'Number of color levels to be in the palette')
	infobulle(chkRevCol.tab3,'Reverse the color palettes')
	status.bar.display(chkRevCol.tab3,txt.stbr1,'Reverse the color palettes')

	#####
	tkgrid(labPresetCol.tab3,row=0,column=0,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(combPresetCol.tab3,row=0,column=2,sticky='we',rowspan=1,columnspan=3,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(nbPresetCol.tab3,row=0,column=5,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(labRevCol.tab3,row=1,column=2,sticky='e',rowspan=1,columnspan=3,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(chkRevCol.tab3,row=1,column=5,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(sep1.tab3,row=2,column=0,sticky='we',rowspan=1,columnspan=6,pady=5)
	tkgrid(previewPresetCol.tab3,row=3,column=0,sticky='w',rowspan=1,columnspan=6,padx=1,pady=1)
	tkgrid(sep2.tab3,row=4,column=0,sticky='we',rowspan=1,columnspan=6,pady=5)
	tkgrid(chkCustoCol.tab3,row=5,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(butCustoCol.tab3,row=5,column=4,sticky='w',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep3.tab3,row=6,column=0,sticky='we',rowspan=1,columnspan=6,pady=5)
	tkgrid(chkCustoLev.tab3,row=7,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(butCustoLev.tab3,row=7,column=4,sticky='w',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)

	########################
	##Preview Color
	kolor<-getGradientColor(tim.colors(10),0:wPreview)
	tkdelete(previewPresetCol.tab3,'gradlines0')
	for(i in 0:wPreview) tkcreate(previewPresetCol.tab3, "line",i,0,i,20,fill=kolor[i],tags='gradlines0')

	tkbind(combPresetCol.tab3,"<<ComboboxSelected>>",function(){
		n<-as.numeric(tclvalue(nb.color))
		colFun<-match.fun(tclvalue(preset.color))
		listCol<-colFun(n)
		if(tclvalue(reverse.color)=='1') listCol<-rev(listCol)
		kolor<-getGradientColor(listCol,0:wPreview)
		tkdelete(previewPresetCol.tab3,'gradlines0')
		for(i in 0:wPreview) tkcreate(previewPresetCol.tab3, "line",i,0,i,20,fill=kolor[i],tags='gradlines0')
	})

	#reverse
	tkbind(chkRevCol.tab3,"<Button-1>",function(){
		if(tclvalue(custom.color)=='0'){
			n<-as.numeric(tclvalue(nb.color))
			colFun<-match.fun(tclvalue(preset.color))
			listCol<-colFun(n)
			if(tclvalue(reverse.color)=='0') listCol<-rev(listCol)
			kolor<-getGradientColor(listCol,0:wPreview)
			tkdelete(previewPresetCol.tab3,'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab3, "line",i,0,i,20,fill=kolor[i],tags='gradlines0')
		}
	})
	
	########################
	##Customized color	
	tkbind(chkCustoCol.tab3,"<Button-1>",function(){
		if(tclvalue(custom.color)=='0') tkconfigure(butCustoCol.tab3,state='normal')
		else tkconfigure(butCustoCol.tab3,state='disabled')
	})

	listCol<-NULL	
	tkconfigure(butCustoCol.tab3,command=function(){
		listCol<<-createColorkey(main.win,listCol)
		if(!is.null(listCol) & length(listCol)>0){
			kolor<-getGradientColor(listCol,0:wPreview)
			tkdelete(previewPresetCol.tab3,'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab3, "line",i,0,i,20,fill=kolor[i],tags='gradlines0')
		}
	})

	########################
	##Customized level	
	tkbind(chkCustoLev.tab3,"<Button-1>",function(){
		if(tclvalue(custom.level)=='0') tkconfigure(butCustoLev.tab3,state='normal')
		else tkconfigure(butCustoLev.tab3,state='disabled')
	})
	
	atLev<-NULL
	tkconfigure(butCustoLev.tab3,command=function(){
		if(is.null(atLev)){
			donne<-getCDTdata(file.stnfl,file.period)
			donne<-getCDTdata1Date(donne,tclvalue(idate_yrs),tclvalue(idate_mon),tclvalue(idate_day))
			donne<-donne$z
			for(j in 1:(jCDF-1)){
				ncdat<-getNcdfOpenData(dataNCDF[[j]][[2]])[[2]]
				donne<-c(donne,ncdat$value)
			}
			
			if(!is.null(donne)){
				atLev<-pretty(donne)
			}
		}
		atLev<<-customLevels(main.win,atLev)
	})
	
	#######################################################################################################

	notebookTab<-NULL
	#######

	plotDataBut<-tkbutton(plotBut.cmd, text="Plot Data")
	tkgrid(plotDataBut,row=0,column=0,sticky='e',padx=5,pady=5)
	
	tkconfigure(plotDataBut,command=function(){
		if(tclvalue(custom.color)=='0' | length(listCol)==0){
			n<-as.numeric(tclvalue(nb.color))
			if(is.na(n)) n<-10
			colFun<-match.fun(tclvalue(preset.color))
			listCol<-colFun(n)
			if(tclvalue(reverse.color)=='1') listCol<-rev(listCol)
		}
		
		allDATA<-list()

		donne<-getCDTdata(file.stnfl,file.period)
		donne<-getCDTdata1Date(donne,tclvalue(idate_yrs),tclvalue(idate_mon),tclvalue(idate_day))
		allDATA[[1]]<-list(list(x=donne$lon,y=donne$lat,value=donne$z),tclvalue(obs_name),tclvalue(file.stnfl),donne$date,donne$id)

		for(j in 1:(jCDF-1)){
			ncdat<-getNcdfOpenData(dataNCDF[[j]][[2]])[[2]]
			allDATA[[j+1]]<-list(ncdat,tclvalue(dataNCDF[[j]][[3]]))
		}

		if(tclvalue(custom.level)=='0' | length(atLev)==0){
			datt<-unlist(lapply(allDATA,function(x) x[[1]]$value))
			if(!is.null(datt)) atLev<-pretty(datt)
		}
		shpf<-getShpOpenData(file.plotShp)[[2]]
		
		imgContainer<-displayPlotMerging(tknotes,notebookTab,allDATA,atLev,listCol,shpf,tclvalue(unit_sym))
		if(!is.null(imgContainer)){
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab,tab.type,tab.data)
			notebookTab<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}
	})	

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	tkgrid.columnconfigure(cmd.frame,0,weight=1)
	######
	return(cmd.frame)
}


