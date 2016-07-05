
################
QcCmdBut<-function(stateReplaceAll){

	file.list<-openFile_ttkcomboList()

	wttkcombo<-as.integer(as.numeric(w.scale(19)*0.95)/9)

	#scrollable frame width
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin<-w.scale(18)
		hscrlwin<-h.scale(31.5)
	}else{
		wscrlwin<-w.scale(21.7)
		hscrlwin<-h.scale(38.5)
	}

	########################################
	cmd.frame<-tkframe(panel.left)

	tknote.cmd<-bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd ,sticky='nwes')

	cmd.tab1 <- bwAddTab(tknote.cmd,text="QC Outputs")
	cmd.tab2 <- bwAddTab(tknote.cmd,text="Plot Outputs")
	cmd.tab3 <- bwAddTab(tknote.cmd,text="Zoom")
	cmd.tab4 <- bwAddTab(tknote.cmd,text="Add Maps")
	bwRaiseTab(tknote.cmd,cmd.tab1)

	#######################################################################################################

	#Tab1
	frTab1<-tkframe(cmd.tab1)
	tkgrid(frTab1,padx=1,pady=1,ipadx=1,ipady=1)

	scrw1<-bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	subfr1<-bwScrollableFrame(scrw1,width=wscrlwin,height=hscrlwin)

	stats<-tclVar('0.0')

	btPreview.tab1<-tkbutton(subfr1, text="Output Preview")
	sep1.tab1<-ttkseparator(subfr1)
	btSetting.tab1<-tkbutton(subfr1, text="Outlier-Settings")
	btReplace.tab1<-tkbutton(subfr1, text="Replace")
	sep2.tab1<-ttkseparator(subfr1)
	labThresReplace.tab1<-tklabel(subfr1,text='Statistic threshold',anchor='e',justify='right')
	enThresReplace.tab1<-tkentry(subfr1,textvariable=stats,width=4)
	btThresReplace.tab1<-tkbutton(subfr1, text="Replace")
	sep3.tab1<-ttkseparator(subfr1)
	btAllReplace.tab1<-tkbutton(subfr1, text="Replace all outliers with NA",state=stateReplaceAll)

	infobulle(btSetting.tab1,'Edit results')
	status.bar.display(btSetting.tab1,txt.stbr1,'Edit results')
	infobulle(btReplace.tab1,'Replaces outliers with missing values after editing')
	status.bar.display(btReplace.tab1,txt.stbr1,'Replaces outliers with missing values after editing')
	infobulle(enThresReplace.tab1,'Replace outliers with missing if the statistic\nis greater than or equal to this threshold\nand the box in the right is checked')
	status.bar.display(enThresReplace.tab1,txt.stbr1,'Replace outliers with missing  if the statistic is greater than  or equal to this threshold and the box in the right is checked')
	infobulle(btThresReplace.tab1,'Replace outliers with missing if the statistic\nis greater than or equal to this threshold\nand the box in the right is checked')
	status.bar.display(btThresReplace.tab1,txt.stbr1,'Replace outliers with missing  if the statistic is greater than  or equal to this threshold and the box in the right is checked')
	infobulle(btAllReplace.tab1,'Replaces all outliers with missing values at one time,\ntaking into account the statistic threshold')
	status.bar.display(btAllReplace.tab1,txt.stbr1,'Replaces all outliers with missing values at one time, taking into account the statistic threshold')

	tkgrid(btPreview.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep1.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(btSetting.tab1,row=2,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btReplace.tab1,row=2,column=4,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep2.tab1,row=3,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(labThresReplace.tab1,row=4,column=0,sticky='we',rowspan=1,columnspan=3,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(enThresReplace.tab1,row=4,column=3,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btThresReplace.tab1,row=4,column=5,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep3.tab1,row=5,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(btAllReplace.tab1,row=6,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)

	#######################################################################################################

	#Tab2
	frTab2<-tkframe(cmd.tab2)
	tkgrid(frTab2,padx=1,pady=1,ipadx=1,ipady=1)

	scrw2<-bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	subfr2<-bwScrollableFrame(scrw2,width=wscrlwin,height=hscrlwin)

	choix.mois <- tclVar(format(ISOdate(2014,1:12,1),"%B")[1])
	spchkQcDateVal<-tclVar()

	labplotOutl.tab2<-tklabel(subfr2,text='Plot Outliers by month',anchor='w',justify='right')
	btOutlPrev.tab2<-tkbutton(subfr2, text="<<")
	combOutlmonth.tab2<-ttkcombobox(subfr2, values=format(ISOdate(2014,1:12,1),"%B"), textvariable=choix.mois,width=17)
	btOutlNext.tab2<-tkbutton(subfr2, text=">>")
	
	sep1.tab2<-ttkseparator(subfr2)
	labplotSpChk.tab2<-tklabel(subfr2,text='Plot Spatial Check',anchor='w',justify='right')
	btSpChkPrev.tab2<-tkbutton(subfr2, text="<<")
	combSpChkDate.tab2<-ttkcombobox(subfr2, values='', textvariable=spchkQcDateVal,width=17)
	btSpChkNext.tab2<-tkbutton(subfr2, text=">>")

	sep2.tab2<-ttkseparator(subfr2)
	vShowVal <- tclVar("1")
	cbValshp <- tclVar("0")
	cbValdem <- tclVar("0")
	cbValrfe <- tclVar("0")
	cbShowVal.tab2<-tkcheckbutton(subfr2,text="Show station values",variable=vShowVal,anchor='w',justify='left')
	cbSHP.tab2 <- tkcheckbutton(subfr2,text="Add administrative boundaries to map",variable=cbValshp,anchor='w',justify='left')
	cbDEM.tab2 <- tkcheckbutton(subfr2,text="Add DEM to map",variable=cbValdem,anchor='w',justify='left')
	cbRFE.tab2 <- tkcheckbutton(subfr2,text="Add satellite data to map",variable=cbValrfe,anchor='w',justify='left')

	################################
	# frameShpDEM<-ttklabelframe(subfr2,text="Add Boundary-DEM-RFE",relief='groove')

	# file.plotShp <- tclVar()
	# file.plotDem<-tclVar()

	# labSHP.tab2<-tklabel(frameShpDEM,text="Shapefiles for boundary",anchor='w',justify='right')
	# combShp.tab2<-ttkcombobox(frameShpDEM, values=unlist(file.list), textvariable=file.plotShp,width=wttkcombo)
	# btShp.tab2<-tkbutton(frameShpDEM, text="...")
	# tkconfigure(btShp.tab2,command=function(){
	# 	shp.opfiles<-getOpenShp(main.win,all.opfiles)
	# 	if(!is.null(shp.opfiles)){
	# 		nopf<-length(type.opfiles)
	# 		type.opfiles[[nopf+1]]<<-'shp'
	# 		file.opfiles[[nopf+1]]<<-shp.opfiles
	# 		file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
	# 		tclvalue(file.plotShp)<-file.opfiles[[nopf+1]][[1]]
	# 		tkconfigure(combDem.tab2,values=unlist(file.list), textvariable=file.plotDem)
	# 		tkconfigure(combShp.tab2,values=unlist(file.list), textvariable=file.plotShp)
	# 	}
	# })

	# labDEM.tab2<-tklabel(frameShpDEM,text="Elevation Data (NetCDF)",anchor='w',justify='right')
	# combDem.tab2<-ttkcombobox(frameShpDEM, values=unlist(file.list), textvariable=file.plotDem,width=wttkcombo)
	# btDem.tab2<-tkbutton(frameShpDEM, text="...")
	# tkconfigure(btDem.tab2,command=function(){
	# 	nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
	# 	if(!is.null(nc.opfiles)){
	# 		nopf<-length(type.opfiles)
	# 		type.opfiles[[nopf+1]]<<-'netcdf'
	# 		file.opfiles[[nopf+1]]<<-nc.opfiles
	# 		file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
	# 		tclvalue(file.plotDem)<-file.opfiles[[nopf+1]][[1]]
	# 		tkconfigure(combDem.tab2,values=unlist(file.list), textvariable=file.plotDem)
	# 		tkconfigure(combShp.tab2,values=unlist(file.list), textvariable=file.plotShp)
	# 	}
	# })

	# dir_ncdf <-tclVar()
	# dir_ncdfLab.tab2<-tklabel(frameShpDEM,text='Directory of RFE files',anchor='w',justify='left')
	# dir_ncdfEd.tab2<-tkentry(frameShpDEM,textvariable=dir_ncdf,width=wttkcombo)
	# dir_ncdfBt.tab2<-tkbutton(frameShpDEM, text="...") 
	# tkconfigure(dir_ncdfBt.tab2,command=function(){
	# 	dir4ncdf<-tk_choose.dir(getwd(), "")
	# 	if(is.na(dir4ncdf)) tclvalue(dir_ncdf)<-""
	# 	else tclvalue(dir_ncdf)<-dir4ncdf
	# })
	# infobulle(dir_ncdfEd.tab2,'Enter the full path to\ndirectory containing the RFE files')
	# status.bar.display(dir_ncdfEd.tab2,txt.stbr1,'Enter the full path to directory containing the RFE files')
	# infobulle(dir_ncdfBt.tab2,'Select directory here')
	# status.bar.display(dir_ncdfBt.tab2,txt.stbr1,'Select directory here')

	# ff_ncdf <-tclVar("rfe%s_%s_%s.nc")
	# ff_ncdfLab.tab2<-tklabel(frameShpDEM,text='RFE filename format',anchor='w',justify='left')
	# ff_ncdfEd.tab2<-tkentry(frameShpDEM,width=14,textvariable=ff_ncdf,justify = "left")
	# infobulle(ff_ncdfEd.tab2,'Enter the format of the RFE files names,\nexample: rfe1983_01_01.nc')
	# status.bar.display(ff_ncdfEd.tab2,txt.stbr1,'Enter the format of the RFE files names, example: rfe1983_01_01.nc')

	########
	tkgrid(labplotOutl.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btOutlPrev.tab2,row=1,column=0,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(combOutlmonth.tab2,row=1,column=1,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btOutlNext.tab2,row=1,column=7,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(sep1.tab2,row=2,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(labplotSpChk.tab2,row=3,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btSpChkPrev.tab2,row=4,column=0,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(combSpChkDate.tab2,row=4,column=1,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btSpChkNext.tab2,row=4,column=7,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(sep2.tab2,row=5,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(cbShowVal.tab2,row=6,column=0,sticky='we',rowspan=1,columnspan=8,pady=2)
	tkgrid(cbSHP.tab2,row=7,column=0,sticky='we',rowspan=1,columnspan=8,pady=2)
	tkgrid(cbDEM.tab2,row=8,column=0,sticky='we',rowspan=1,columnspan=8,pady=2)
	tkgrid(cbRFE.tab2,row=9,column=0,sticky='we',rowspan=1,columnspan=8,pady=2)

	################################
	# tkgrid(frameShpDEM,row=5,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)

	# tkgrid(labSHP.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(combShp.tab2,row=1,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(btShp.tab2,row=1,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	# tkgrid(labDEM.tab2,row=2,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(combDem.tab2,row=3,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(btDem.tab2,row=3,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	# tkgrid(dir_ncdfLab.tab2,row=4,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(dir_ncdfEd.tab2,row=5,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(dir_ncdfBt.tab2,row=5,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(ff_ncdfLab.tab2,row=6,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	# tkgrid(ff_ncdfEd.tab2,row=7,column=1,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)

	#######################################################################################################

	#Tab3
	frTab3<-tkframe(cmd.tab3)
	tkgrid(frTab3,padx=1,pady=1,ipadx=1,ipady=1)

	scrw3<-bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	subfr3<-bwScrollableFrame(scrw3,width=wscrlwin,height=hscrlwin)

	######

	if(tclvalue(XYCoordinates)!=""){
		xycrd<-matrix(as.numeric(strsplit(tclvalue(XYCoordinates),' ')[[1]]),ncol=2)
		stn_lon<-xycrd[,1]
		stn_lat<-xycrd[,2]
		xminlo<-round(min(stn_lon,na.rm=T)-0.1,4)
		xmaxlo<-round(max(stn_lon,na.rm=T)+0.1,4)
		xminla<-round(min(stn_lat,na.rm=T)-0.1,4)
		xmaxla<-round(max(stn_lat,na.rm=T)+0.1,4)
		ZoomXYval0<-c(xminlo,xmaxlo,xminla,xmaxla)
	}else{
		xminlo<-''
		xmaxlo<-''
		xminla<-''
		xmaxla<-''
		ZoomXYval0<-NULL
	}
	xx1<<-tclVar(xminlo)
	xx2<<-tclVar(xmaxlo)
	yy1<<-tclVar(xminla)
	yy2<<-tclVar(xmaxla)

	#####
	xentr1.tab3<-tkentry(subfr3,width=7,justify="left",textvariable=xx1)
	xentr2.tab3<-tkentry(subfr3,width=7,justify="left",textvariable=xx2)
	yentr1.tab3<-tkentry(subfr3,width=7,justify="left",textvariable=yy1)
	yentr2.tab3<-tkentry(subfr3,width=7,justify="left",textvariable=yy2)
	btCentre.tab3<-tklabel(subfr3,image=pikCentre)

	btZoomP.tab3<<-tkbutton(subfr3,image=pikZoomPlus,relief='raised',bg='lightblue',state='normal')
	btZoomM.tab3<<-tkbutton(subfr3,image=pikZoomMinus,relief='raised',bg='lightblue',state='normal')
	btZoomRect.tab3<<-tkbutton(subfr3,image=pikZoomRect,relief='raised',bg='lightblue',state='normal')
	btPanImg.tab3<<-tkbutton(subfr3,image=pikPanImg,relief='raised',bg='lightblue',state='normal')
	btRedraw.tab3<-tkbutton(subfr3,image=pikRedraw,relief='raised',bg='lightblue')
	btReset.tab3<-tkbutton(subfr3,image=pikReset,relief='raised')

	infobulle(btZoomP.tab3,'Zoom In')
	status.bar.display(btZoomP.tab3,txt.stbr1,'Zoom In')
	infobulle(btZoomM.tab3,'Zoom Out')
	status.bar.display(btZoomM.tab3,txt.stbr1,'Zoom Out')
	infobulle(btZoomRect.tab3,'Zoom Area')
	status.bar.display(btZoomRect.tab3,txt.stbr1,'Zoom Area')
	infobulle(btPanImg.tab3,'Pan Tool')
	status.bar.display(btPanImg.tab3,txt.stbr1,'Pan Tool')
	infobulle(btRedraw.tab3,'Redraw Map')
	status.bar.display(btRedraw.tab3,txt.stbr1,'Redraw Map')
	infobulle(btReset.tab3,' Zoom Reset')
	status.bar.display(btReset.tab3,txt.stbr1,' Zoom Reset')

	##
	tkgrid(xentr1.tab3,row=1,column=0,sticky='we',rowspan=1,columnspan=1)
	tkgrid(xentr2.tab3,row=1,column=2,sticky='we',rowspan=1,columnspan=1)
	tkgrid(yentr1.tab3,row=2,column=1,sticky='we',rowspan=1,columnspan=1)
	tkgrid(yentr2.tab3,row=0,column=1,sticky='we',rowspan=1,columnspan=1)
	tkgrid(btCentre.tab3,row=1,column=1,sticky='nswe',rowspan=1,columnspan=1)

	tkgrid(btReset.tab3,row=0,column=3,sticky='nswe',rowspan=1,columnspan=1)
	tkgrid(btRedraw.tab3,row=1,column=3,sticky='nswe',rowspan=1,columnspan=1)
	tkgrid(btPanImg.tab3,row=2,column=3,sticky='nswe',rowspan=1,columnspan=1)

	tkgrid(btZoomP.tab3,row=0,column=4,sticky='nswe',rowspan=1,columnspan=1)
	tkgrid(btZoomM.tab3,row=1,column=4,sticky='nswe',rowspan=1,columnspan=1)
	tkgrid(btZoomRect.tab3,row=2,column=4,sticky='nswe',rowspan=1,columnspan=1)

	#######################################################################################################

	#Tab4
	frTab4<-tkframe(cmd.tab4)
	tkgrid(frTab4,padx=1,pady=1,ipadx=1,ipady=1)

	scrw4<-bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	subfr4<-bwScrollableFrame(scrw4,width=wscrlwin,height=hscrlwin)

	frameShp<-ttklabelframe(subfr4,text="Boundaries Shapefiles",relief='groove')
	file.plotShp <- tclVar()
	combShp.tab4<-ttkcombobox(frameShp, values=unlist(file.list), textvariable=file.plotShp,width=wttkcombo)
	btShp.tab4<-tkbutton(frameShp, text="...")
	tkconfigure(btShp.tab4,command=function(){
		shp.opfiles<-getOpenShp(main.win,all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'shp'
			file.opfiles[[nopf+1]]<<-shp.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.plotShp)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combDem.tab4,values=unlist(file.list), textvariable=file.plotDem)
			tkconfigure(combShp.tab4,values=unlist(file.list), textvariable=file.plotShp)
		}
	})

	frameDEM<-ttklabelframe(subfr4,text="Elevation Data (NetCDF)",relief='groove')
	file.plotDem<-tclVar()
	combDem.tab4<-ttkcombobox(frameDEM, values=unlist(file.list), textvariable=file.plotDem,width=wttkcombo)
	btDem.tab4<-tkbutton(frameDEM, text="...")
	tkconfigure(btDem.tab4,command=function(){
		nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'netcdf'
			file.opfiles[[nopf+1]]<<-nc.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.plotDem)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combDem.tab4,values=unlist(file.list), textvariable=file.plotDem)
			tkconfigure(combShp.tab4,values=unlist(file.list), textvariable=file.plotShp)
		}
	})

	frameRFE<-ttklabelframe(subfr4,text="Satellite Data (NetCDF)",relief='groove')
	dir_ncdf <-tclVar()
	dir_ncdfLab.tab4<-tklabel(frameRFE,text='Directory of satellite files',anchor='w',justify='left')
	dir_ncdfEd.tab4<-tkentry(frameRFE,textvariable=dir_ncdf,width=wttkcombo+2)
	dir_ncdfBt.tab4<-tkbutton(frameRFE, text="...") 
	tkconfigure(dir_ncdfBt.tab4,command=function(){
		dir4ncdf<-tk_choose.dir(getwd(), "")
		if(is.na(dir4ncdf)) tclvalue(dir_ncdf)<-""
		else tclvalue(dir_ncdf)<-dir4ncdf
	})
	infobulle(dir_ncdfEd.tab4,'Enter the full path to\ndirectory containing the satellite files')
	status.bar.display(dir_ncdfEd.tab4,txt.stbr1,'Enter the full path to directory containing the satellite files')
	infobulle(dir_ncdfBt.tab4,'Select directory here')
	status.bar.display(dir_ncdfBt.tab4,txt.stbr1,'Select directory here')

	ff_ncdf <-tclVar("rfe%s_%s_%s.nc")
	ff_ncdfLab.tab4<-tklabel(frameRFE,text='RFE filename format',anchor='w',justify='left')
	ff_ncdfEd.tab4<-tkentry(frameRFE,width=14,textvariable=ff_ncdf,justify = "left")
	infobulle(ff_ncdfEd.tab4,'Enter the format of the satellite files names,\nexample: rfe1983_01_01.nc')
	status.bar.display(ff_ncdfEd.tab4,txt.stbr1,'Enter the format of the satellite files names, example: rfe1983_01_01.nc')

	################################

	tkgrid(combShp.tab4,row=0,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btShp.tab4,row=0,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(combDem.tab4,row=0,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btDem.tab4,row=0,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(dir_ncdfLab.tab4,row=0,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dir_ncdfEd.tab4,row=1,column=0,sticky='we',rowspan=1,columnspan=7,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dir_ncdfBt.tab4,row=1,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(ff_ncdfLab.tab4,row=2,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(ff_ncdfEd.tab4,row=3,column=1,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)

	################################
	tkgrid(frameShp,row=0,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frameDEM,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(frameRFE,row=2,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	#######################################################################################################

	tkconfigure(btRedraw.tab3,command=function(){
		ZoomXYval<<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
		shpf <- if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
		dem <- if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
		showval<- if(tclvalue(vShowVal)=="1") TRUE else FALSE
		rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
		tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
		if(length(tab.type)>0){
			if(tab.type[[tabid]]=="img"){
				assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
				hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
				tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
			}
		}
	})

	##########################
	tkconfigure(btReset.tab3,command=function(){
		ZoomXYval<<-ZoomXYval0
		tclvalue(xx1)<<-ZoomXYval0[1]
		tclvalue(xx2)<<-ZoomXYval0[2]
		tclvalue(yy1)<<-ZoomXYval0[3]
		tclvalue(yy2)<<-ZoomXYval0[4]
		showval<- if(tclvalue(vShowVal)=="1") TRUE else FALSE
		shpf <- if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
		dem <- if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
		rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
		tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
		if(length(tab.type)>0){
			if(tab.type[[tabid]]=="img"){
				assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
				hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))

				tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
			}
		}
	})


	#################################
	##display station values
	tkbind(cbShowVal.tab2,"<Button-1>",function(){
		ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))

		showval <- if(tclvalue(vShowVal)=="0") TRUE else FALSE
		shpf <- if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
		dem <- if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
		rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
		tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
		if(length(tab.type)>0){
			if(tab.type[[tabid]]=="img"){
				assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]]$fun))
				assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
				refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
				hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
				tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
			}
		}
	})


	#Adding administrative boundaries
	tkbind(cbSHP.tab2,"<Button-1>",function(){
		if(!is.null(noteQcSpatCheck)){
			if(tclvalue(cbValshp)=='0'){
				shpf<-getShpOpenData(file.plotShp)[[2]]
				if(is.null(shpf)) insert.txt(main.txt.out,'No administrative boundaries provided',format=TRUE)
			}else  shpf<-NULL

			#shpf<-if(tclvalue(cbValshp)=='0') getShpOpenData(file.plotShp)[[2]] else NULL
			showval <- if(tclvalue(vShowVal)=="1") TRUE else FALSE
			dem <- if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
			rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
			ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
			tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
			if(length(tab.type)>0){
				if(tab.type[[tabid]]=="img"){
					assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]]$fun))
					assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
					hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
				}
			}
		}
	})

	##Adding DEM
	tkbind(cbDEM.tab2,"<Button-1>",function(){
		if(!is.null(noteQcSpatCheck)){
			if(tclvalue(cbValdem)=='0'){
				dem<-getDemOpenData(file.plotDem)
				if(is.null(dem)) insert.txt(main.txt.out,'No elevation data provided',format=TRUE)
			}else  dem<-NULL

			#dem <- if(tclvalue(cbValdem)=='0') getDemOpenData(file.plotDem) else NULL
			showval<- if(tclvalue(vShowVal)=="1") TRUE else FALSE
			shpf<- if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
			rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
			ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
			tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
			if(length(tab.type)>0){
				if(tab.type[[tabid]]=="img"){
					assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]]$fun))
					assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
					hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
				}
			}
		}
	})

	##Adding RFE
	tkbind(cbRFE.tab2,"<Button-1>",function(){
		if(!is.null(noteQcSpatCheck)){
			if(tclvalue(cbValrfe)=='0'){
				rfedat<-getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal)
				if(is.null(rfedat)) insert.txt(main.txt.out,'No satellite data provided',format=TRUE)
			}else  rfedat<-NULL

			#rfedat <- if(tclvalue(cbValrfe)=='0') getSatelliteData(file.plotDem) else NULL
			showval<- if(tclvalue(vShowVal)=="1") TRUE else FALSE
			shpf<- if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
			dem <- if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
			ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
			tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
			if(length(tab.type)>0){
				if(tab.type[[tabid]]=="img"){
					assign("ZoomXYval", ZoomXYval, envir=environment(tab.data[[tabid]][[2]]$fun))
					assign("showval", showval, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("shpf", shpf, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("dem", dem, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					assign("rfedat", rfedat, envir=environment(tab.data[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W=tab.data[[tabid]][[2]][[1]],img=tab.data[[tabid]][[2]][[2]],
					hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
				}
			}
		}
	})

	##########################
	pressButP<<-tclVar('0')
	pressButM<<-tclVar('0')
	pressButRect<<-tclVar('0')
	pressButDrag<<-tclVar('0')
	pressGetCoords<<-tclVar('0')

	tkbind(btRedraw.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-0
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	})

	tkbind(btReset.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-0
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	})

	tkbind(btZoomP.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-1
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-0
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='red',state='disabled')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	})

	tkbind(btZoomM.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-1
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-0
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='red',state='disabled')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	})

	tkbind(btZoomRect.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-1
		tclvalue(pressButDrag)<<-0
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='red',state='disabled')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	})


	tkbind(btPanImg.tab3,"<Button-1>",function(){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-1
		tclvalue(pressGetCoords)<<-0
		tkconfigure(btRedraw.tab3,relief='raised',bg='lightblue')
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='red',state='disabled')
	})


	#####
	initializeButZoom<-function(){
			initXYval0<<-str_trim(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
			tclvalue(pressButP)<<-0
			tclvalue(pressButM)<<-0
			tclvalue(pressButRect)<<-0
			tclvalue(pressButDrag)<<-0
			tclvalue(pressGetCoords)<<-0
			tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
			tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
			tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
			tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
	}

	activateButRedraw<-function(){
		initXYval1<-str_trim(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
		if(!all(initXYval0==initXYval1)) tkconfigure(btRedraw.tab3,relief='raised',bg='red')
	}

	####
	tkbind(xentr1.tab3,"<FocusIn>",initializeButZoom)
	tkbind(xentr1.tab3,"<FocusOut>",activateButRedraw)

	tkbind(xentr2.tab3,"<FocusIn>",initializeButZoom)
	tkbind(xentr2.tab3,"<FocusOut>",activateButRedraw)

	tkbind(yentr1.tab3,"<FocusIn>",initializeButZoom)
	tkbind(yentr1.tab3,"<FocusOut>",activateButRedraw)

	tkbind(yentr2.tab3,"<FocusIn>",initializeButZoom)
	tkbind(yentr2.tab3,"<FocusOut>",activateButRedraw)

	######################################################################################################
	#Display QC results as console output
	PrevwQcIdTab<-NULL

	tkconfigure(btPreview.tab1,command=function(){
		if(!is.null(ret.results)){
			if(gal.params$AllOrOne=='one'){
				IJstation<-ret.results$station
				todisplay<-ret.results
			}
			if(gal.params$AllOrOne=='all'){
				ijstn<-which(as.character(gal.params$parameter[[2]][,1])==tclvalue(stn.choix.val))
				IJstation<-ret.results$station[[ijstn]]
				todisplay<-list(action=ret.results$action,period=ret.results$period,station=ret.results$station[[ijstn]],
				res=ret.results$res[[ijstn]],outputdir=ret.results$outputdir[[ijstn]],AllOrOne=ret.results$AllOrOne)
			}

			retNBTab<-consolOutNotebookTab_unik(tknotes,todisplay,paste(IJstation,'_QC-Output Preview',sep=''),PrevwQcIdTab,tab.type,tab.data)
			PrevwQcIdTab<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})


	###################################################
	##display table for setting
	QcEditSetTab<-NULL

	tkconfigure(btSetting.tab1,command=function(){
		if(!is.null(ret.results)){
			if(gal.params$AllOrOne=='one'){
				IJstation<-ret.results$station
			}
			if(gal.params$AllOrOne=='all'){
				ijstn<-which(as.character(gal.params$parameter[[2]][,1])==tclvalue(stn.choix.val))
				IJstation<-ret.results$station[[ijstn]]
			}
			
			retNBTab<-tableQcEditNotebookTab_unik(tknotes,paste(IJstation,'_QC-Output Edit',sep=''),QcEditSetTab,tab.type,tab.data)
			QcEditSetTab<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	###################################################
	# replace outliers
	tkconfigure(btReplace.tab1,command=function(){
		if(!is.null(ret.results)){
			if(gal.params$AllOrOne=='one'){
				IJstation<-ret.results$station
			}
			if(gal.params$AllOrOne=='all'){
				ijstn<-which(as.character(gal.params$parameter[[2]][,1])==tclvalue(stn.choix.val))
				IJstation<-ret.results$station[[ijstn]]
			}
			isOK<-try(replaceOutlier(IJstation,tclvalue(stats),isReplace=FALSE),silent=TRUE)
			if(!inherits(isOK, "try-error")){
				insert.txt(main.txt.out,paste("Outliers replacement finished successfully for", IJstation))
			}else{
				insert.txt(main.txt.out,paste("Outliers replacement failed for", IJstation),format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',isOK[1]),format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	#######################
	# replace outliers with threshold
	tkconfigure(btThresReplace.tab1,command=function(){
		if(!is.null(ret.results)){
			if(gal.params$AllOrOne=='one'){
				IJstation<-ret.results$station
			}
			if(gal.params$AllOrOne=='all'){
				ijstn<-which(as.character(gal.params$parameter[[2]][,1])==tclvalue(stn.choix.val))
				IJstation<-ret.results$station[[ijstn]]
			}
			isOK<-try(replaceOutlier(IJstation,tclvalue(stats),isReplace=TRUE),silent=TRUE)
			if(!inherits(isOK, "try-error")){
				insert.txt(main.txt.out,paste("Outliers replacement finished successfully for", IJstation))
			}else{
				insert.txt(main.txt.out,paste("Outliers replacement failed for", IJstation),format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',isOK[1]),format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	####################
	##replace all outliers with threshold
	tkconfigure(btAllReplace.tab1,command=function(){
		if(!is.null(ret.results)){
			stns<-unlist(ret.results$station)
			tkconfigure(main.win,cursor='watch');tcl("update","idletasks")
			lapply(stns,function(IJstation){
				isOK<-try(replaceOutlier(IJstation,tclvalue(stats),isReplace=TRUE),silent=TRUE)
				if(!inherits(isOK, "try-error")){
					insert.txt(main.txt.out,paste("Outliers replacement finished successfully for", IJstation))
				}else{
					insert.txt(main.txt.out,paste("Outliers replacement failed for", IJstation),format=TRUE)
					insert.txt(main.txt.out,gsub('[\r\n]','',isOK[1]),format=TRUE)
				}
				tcl("update")
			})
			tkconfigure(main.win,cursor='')
			insert.txt(main.txt.out,"Outliers replacement finished!")
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	###################################################
	##plot outliers by month
	nmois<-format(ISOdate(2014,1:12,1),"%B")
	ncmois<-1:12
	ncmois<-ifelse(ncmois<10,paste('0',ncmois,sep=''),ncmois)
	xnmois<-data.frame(nmois,ncmois)
	noteQcOutlierCheck<-NULL

	#####

	tkconfigure(btOutlNext.tab2,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			imois<-as.numeric(tclvalue(tcl(combOutlmonth.tab2,"current")))+1
			imois<-imois+1
			if(imois>12) imois<-1
			tclvalue(choix.mois)<-as.character(xnmois[,1])[imois]
			jmo<-as.character(xnmois[imois,2])

			imgContainer<-DisplayOutliers(tknotes,jmo,noteQcOutlierCheck)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcOutlierCheck,tab.type,tab.data)
			noteQcOutlierCheck<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	#####
	tkconfigure(btOutlPrev.tab2,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			imois<-as.numeric(tclvalue(tcl(combOutlmonth.tab2,"current")))+1
			imois<-imois-1
			if(imois<1) imois<-12
			tclvalue(choix.mois)<-as.character(xnmois[,1])[imois]
			jmo<-as.character(xnmois[imois,2])

			imgContainer<-DisplayOutliers(tknotes,jmo,noteQcOutlierCheck)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcOutlierCheck,tab.type,tab.data)
			noteQcOutlierCheck<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	#####
	tkbind(combOutlmonth.tab2,"<<ComboboxSelected>>",function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			jmo<-as.character(xnmois[which(xnmois[,1]==tclvalue(tkget(combOutlmonth.tab2))),2])

			imgContainer<-DisplayOutliers(tknotes,jmo,noteQcOutlierCheck)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcOutlierCheck,tab.type,tab.data)
			noteQcOutlierCheck<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	###################################################
	##plot spatial check
	noteQcSpatCheck<-NULL
	actualSTN<-'INITSTN'
	qcoutDate<<-NULL
	##########
	tkconfigure(btSpChkNext.tab2,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			spchkoutdates<-isSpatialCheckOk()
			if(nrow(spchkoutdates)!=0){
				if(as.character(spchkoutdates[1,1])!=actualSTN){
					qcoutDate<<-as.character(spchkoutdates[,2])
					tkconfigure(combSpChkDate.tab2,values=qcoutDate)
					tclvalue(spchkQcDateVal)<-qcoutDate[1]
					actualSTN<<-as.character(spchkoutdates[1,1])
				}

				shpf<-if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
				dem<-if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
				rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
				showval<-if(tclvalue(vShowVal)=="1") TRUE else FALSE
				ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))

				##########
		 		ijsp<-as.numeric(tclvalue(tcl(combSpChkDate.tab2,"current")))+1
		   		ijsp<-ijsp+1
		 		if(ijsp>length(qcoutDate)) ijsp<-1
				tclvalue(spchkQcDateVal)<-qcoutDate[ijsp]

		 		imgContainer<-DisplaySpatialCheck(tknotes,ijsp,ZoomXYval,dem,rfedat,shpf,showval,noteQcSpatCheck)
				retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcSpatCheck,tab.type,tab.data)
				noteQcSpatCheck<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}else{
				tkconfigure(combSpChkDate.tab2,values='')
				tclvalue(spchkQcDateVal)<-''
				actualSTN<<-'INITSTN'
				insert.txt(main.txt.out,'No spatial check performed',format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	##########
	tkconfigure(btSpChkPrev.tab2,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			spchkoutdates<-isSpatialCheckOk()
			if(nrow(spchkoutdates)!=0){
				if(as.character(spchkoutdates[1,1])!=actualSTN){
					qcoutDate<<-as.character(spchkoutdates[,2])
					tkconfigure(combSpChkDate.tab2,values=qcoutDate)
					tclvalue(spchkQcDateVal)<-qcoutDate[1]
					actualSTN<<-as.character(spchkoutdates[1,1])
				}

				shpf<-if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
				dem<-if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
				rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
				showval<-if(tclvalue(vShowVal)=="1") TRUE else FALSE
				ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))

				#######
		 		ijsp<-as.numeric(tclvalue(tcl(combSpChkDate.tab2,"current")))+1
		   		ijsp<-ijsp-1
		 		if(ijsp<1) ijsp<-length(qcoutDate)
				tclvalue(spchkQcDateVal)<-qcoutDate[ijsp]
		 		imgContainer<-DisplaySpatialCheck(tknotes,ijsp,ZoomXYval,dem,rfedat,shpf,showval,noteQcSpatCheck)

				retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcSpatCheck,tab.type,tab.data)
				noteQcSpatCheck<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}else{
				tkconfigure(combSpChkDate.tab2,values='')
				tclvalue(spchkQcDateVal)<-''
				actualSTN<<-'INITSTN'
				insert.txt(main.txt.out,'No spatial check performed',format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	##########
	tkbind(combSpChkDate.tab2,"<<ComboboxSelected>>",function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			spchkoutdates<-isSpatialCheckOk()
			if(nrow(spchkoutdates)!=0){
				shpf<-if(tclvalue(cbValshp)=='1') getShpOpenData(file.plotShp)[[2]] else NULL
				dem<-if(tclvalue(cbValdem)=='1') getDemOpenData(file.plotDem) else NULL
				rfedat<-if(tclvalue(cbValrfe)=='1') getSatelliteData(dir_ncdf,ff_ncdf,spchkQcDateVal) else NULL
				showval<-if(tclvalue(vShowVal)=="1") TRUE else FALSE
				ZoomXYval<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))

				ijsp<-as.numeric(tclvalue(tcl(combSpChkDate.tab2,"current")))+1
		 		imgContainer<-DisplaySpatialCheck(tknotes,ijsp,ZoomXYval,dem,rfedat,shpf,showval,noteQcSpatCheck)

				retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteQcSpatCheck,tab.type,tab.data)
				noteQcSpatCheck<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}else{
				tkconfigure(combSpChkDate.tab2,values='')
				tclvalue(spchkQcDateVal)<-''
				actualSTN<<-'INITSTN'
				insert.txt(main.txt.out,'No spatial check performed',format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no qc-results outputs',format=TRUE)
	})

	######
	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	######

	return(cmd.frame)

}


