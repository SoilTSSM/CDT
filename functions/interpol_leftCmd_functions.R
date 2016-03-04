
InterpolationPanelCmd<-function(){
	file.list<-openFile_ttkcomboList()

	##tkcombo& tkentry width
	#largeur<-27
	largeur<-as.integer(w.scale(20)/sfont0)
	##tkentry nc filename format tkentry wdth
	#wncdf_ff<-19
	wncdf_ff<-as.integer(w.scale(14)/sfont0)
	#scrollable frame width
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin<-w.scale(17.7) #18
		hscrlwin<-h.scale(31.5)
	}else{
		wscrlwin<-w.scale(20.7)  #21
		hscrlwin<-h.scale(37)
	}


	###################

	cmd.frame<-tkframe(panel.left)

	tknote.cmd<-bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd ,sticky='nwes')
	tkgrid.columnconfigure(tknote.cmd,0,weight=1)

	cmd.tab1 <- bwAddTab(tknote.cmd,text="General")
	cmd.tab2 <- bwAddTab(tknote.cmd,text="Options")
	cmd.tab3 <- bwAddTab(tknote.cmd,text="Interpolation")
#	cmd.tab4 <- bwAddTab(tknote.cmd,text="Extraction")
	bwRaiseTab(tknote.cmd,cmd.tab1)



#######################################################################################################

	#Tab1
	frTab1<-tkframe(cmd.tab1)
	tkgrid(frTab1,padx=0,pady=1,ipadx=1,ipady=1)
	tkgrid.columnconfigure(frTab1,0,weight=1)

	scrw1<-bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	subfr1<-bwScrollableFrame(scrw1,width=wscrlwin,height=hscrlwin)


	##############
	frameStn<-ttklabelframe(subfr1,text="Station data file",relief='groove')

	#######################
	file.period <- tclVar()
	tclvalue(file.period) <- 'Dekadal data'
	combPrd.tab1<-ttkcombobox(frameStn, values=c('Daily data','Dekadal data','Monthly data'), textvariable=file.period)

	#######################
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
			tkconfigure(combgrdCDF.tab1,values=unlist(file.list), textvariable=file.grdCDF)
		}else return(NULL)
	})
	infobulle(combStnfl.tab1,'Choose the station data in the list')
	status.bar.display(combStnfl.tab1,txt.stbr1,'Choose the file containing the station data in CDT format')
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

	#############################
	tkgrid(combPrd.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(combStnfl.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btStnfl.tab1,row=1,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(yrsLab.tab1,row=2,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(monLab.tab1,row=2,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(dayLab.tab1,row=2,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(labDate.tab1,row=3,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(yrs1.tab1,row=3,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(mon1.tab1,row=3,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)
	tkgrid(day1.tab1,row=3,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=0,ipadx=1,ipady=1)

	#############################
	frameInterp<-ttklabelframe(subfr1,text="Interpolation Method",relief='groove')

	Interp.Method <- tclVar()
	tclvalue(Interp.Method) <- 'IDW'
	combInterp.tab1<-ttkcombobox(frameInterp, values=c('Nearest Neighbor','IDW','Kriging'), textvariable=Interp.Method,width=largeur)

	#############################
	tkgrid(combInterp.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)

	#############################
	frameGrid<-ttklabelframe(subfr1,text="Grid for interpolation",relief='groove')

	varCreateGrd <- tclVar('1')
	grdCDF.tab1 <- tkradiobutton(frameGrid,text="From gridded NetCDF data",anchor='w',justify='left')


	###from CDF
	file.grdCDF <- tclVar()
	combgrdCDF.tab1<-ttkcombobox(frameGrid, values=unlist(file.list), textvariable=file.grdCDF,state='disabled',width=largeur)
	infobulle(combgrdCDF.tab1,'Choose the file in the list')
	status.bar.display(combgrdCDF.tab1,txt.stbr1,'File containing a gridded data in netcdf')
	btgrdCDF.tab1<-tkbutton(frameGrid, text="...")
	infobulle(btgrdCDF.tab1,'Browse file if not listed')
	status.bar.display(btgrdCDF.tab1,txt.stbr1,'Browse file if not listed')
	tkconfigure(btgrdCDF.tab1,state='disabled',command=function(){
		nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'netcdf'
			file.opfiles[[nopf+1]]<<-nc.opfiles

			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.grdCDF)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1,values=unlist(file.list), textvariable=file.stnfl)
			tkconfigure(combgrdCDF.tab1,values=unlist(file.list), textvariable=file.grdCDF)
			tkconfigure(combDem.tab2,values=unlist(file.list), textvariable=file.plotDem)
			tkconfigure(combShp.tab1,values=unlist(file.list), textvariable=file.plotShp)
		}else return(NULL)
	})

	grdNEW.tab1 <- tkradiobutton(frameGrid,text="Create new grid",anchor='w',justify='left')

	new.grid<-data.frame(c('minLon','maxLon','resLon','minLat','maxLat','resLat'),c('42','52','0.1','-26','-12','0.1'))
	names(new.grid)<-c('Parameters','Values')
	newgrdPars<-list(new.grid=new.grid)

	btNewgrid.tab1<-tkbutton(frameGrid, text="Create",state='normal')
	infobulle(btNewgrid.tab1,'Set the new grid')
	status.bar.display(btNewgrid.tab1,txt.stbr1,'Set the new grid')

	tkconfigure(btNewgrid.tab1,command=function(){
		newgrdPars<<-getParamNewGrid(main.win,newgrdPars)
	})


	tkconfigure(grdCDF.tab1,variable=varCreateGrd,value="0")
	tkconfigure(grdNEW.tab1,variable=varCreateGrd,value="1")

	#############################
	tkgrid(grdCDF.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(combgrdCDF.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btgrdCDF.tab1,row=1,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(grdNEW.tab1,row=2,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btNewgrid.tab1,row=2,column=4,sticky='we',rowspan=1,columnspan=2,padx=1,pady=2,ipadx=1,ipady=1)


	#############################
	tkgrid(frameStn,row=0,column=0,sticky='we')
	tkgrid(frameInterp,row=1,column=0,sticky='we',pady=3)
	tkgrid(frameGrid,row=2,column=0,sticky='we',pady=3)

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

	#############################
	tkbind(grdCDF.tab1,"<Button-1>",function(){
		tkconfigure(btNewgrid.tab1,state='disabled')
		tkconfigure(combgrdCDF.tab1,state='normal')
		tkconfigure(btgrdCDF.tab1,state='normal')
	})
	tkbind(grdNEW.tab1,"<Button-1>",function(){
		tkconfigure(btNewgrid.tab1,state='normal')
		tkconfigure(combgrdCDF.tab1,state='disabled')
		tkconfigure(btgrdCDF.tab1,state='disabled')
	})


#######################################################################################################

	#Tab2
	frTab2<-tkframe(cmd.tab2) #,relief='sunken',bd=2
	tkgrid(frTab2,padx=5,pady=5,ipadx=2,ipady=2)
	tkgrid.columnconfigure(frTab2,0,weight=1)

	scrw2<-bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	subfr2<-bwScrollableFrame(scrw2,width=wscrlwin,height=hscrlwin)

	##############
	frameOpt1<-ttklabelframe(subfr2,text="General Options",relief='groove')

	maxdistL.tab2<-tklabel(frameOpt1,text="Maxdist",anchor='e',justify='right')
	nminL.tab2<-tklabel(frameOpt1,text="Nmin",anchor='e',justify='right')
	nmaxL.tab2<-tklabel(frameOpt1,text="Nmax",anchor='e',justify='right')
	idpL.tab2<-tklabel(frameOpt1,text="Power",anchor='e',justify='right')
	elvdiffL.tab2<-tklabel(frameOpt1,text="ElvDiff",anchor='e',justify='right')
	omaxL.tab2<-tklabel(frameOpt1,text="Omax",anchor='e',justify='right')

	maxdist_vars<-tclVar('100')
	nmin_vars<-tclVar('1')
	nmax_vars<-tclVar('4')
	idp_vars<-tclVar('2.0')
	elvdiff_vars<-tclVar('200')
	omax_vars<-tclVar('0')

	maxdist.tab2<-tkentry(frameOpt1, width=6,textvariable=maxdist_vars,justify = "left")
	nmin.tab2<-tkentry(frameOpt1, width=6,textvariable=nmin_vars,justify = "left")
	nmax.tab2<-tkentry(frameOpt1, width=6,textvariable=nmax_vars,justify = "left")
	idp.tab2<-tkentry(frameOpt1, width=6,textvariable=idp_vars,justify = "left")
	elvdiff.tab2<-tkentry(frameOpt1, width=6,textvariable=elvdiff_vars,justify = "left")
	omax.tab2<-tkentry(frameOpt1, width=6,textvariable=omax_vars,justify = "left")

	infobulle(maxdist.tab2,'Radius of search (in km)')
	status.bar.display(maxdist.tab2,txt.stbr1,'Only data within a distance of ‘maxdist’ from the prediction location are used')
	infobulle(nmin.tab2,'Minimum number of data to use')
	status.bar.display(nmin.tab2,txt.stbr1,'Minimum number of data to use')
	infobulle(nmax.tab2,'Maximum number of data to use')
	status.bar.display(nmax.tab2,txt.stbr1,'Maximum number of data to use')
	infobulle(idp.tab2,'Inverse distance weighting power')
	status.bar.display(idp.tab2,txt.stbr1,'Inverse distance weighting power')
	infobulle(elvdiff.tab2,'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elvdiff.tab2,txt.stbr1,'Maximum altitude difference of neighbor stations to use (m)')
	infobulle(omax.tab2,'Maximum number of data to use from each quadrant')
	status.bar.display(omax.tab2,txt.stbr1,'Maximum number of data to use from each quadrant, (0 means option not used)')

	#############################
	tkgrid(maxdistL.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(maxdist.tab2,row=0,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(nminL.tab2,row=0,column=2,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(nmin.tab2,row=0,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(nmaxL.tab2,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(nmax.tab2,row=1,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(idpL.tab2,row=1,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(idp.tab2,row=1,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(elvdiffL.tab2,row=2,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(elvdiff.tab2,row=2,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
#	tkgrid(omaxL.tab2,row=2,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
#	tkgrid(omax.tab2,row=2,column=3,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)


	##############
	frameOpt2<-ttklabelframe(subfr2,text="Fitting a variogram",relief='groove')

	fitVgmChx<- tclVar('0')
	fitvgm.tab2 <- tkradiobutton(frameOpt2,text="Fit a variogram",anchor='w',justify='left',state='disabled')
	fitAvgm.tab2 <- tkradiobutton(frameOpt2,text="Automatic fitting",anchor='w',justify='left',state='disabled')
	tkconfigure(fitvgm.tab2,variable=fitVgmChx,value="0")
	tkconfigure(fitAvgm.tab2,variable=fitVgmChx,value="1")

	fitVgmMod <- tclVar('Sph')
	combfitVgm.tab2<-ttkcombobox(frameOpt2, values=c("Sph", "Exp", "Gau","Log","Ste","Mat"), textvariable=fitVgmMod,state='disabled',width=6)
	btfitAVgm.tab2<-tkbutton(frameOpt2, text="Models",state='disabled')

	vgmModList<-c("Sph", "Exp", "Gau","Log","Ste","Mat")
	tkconfigure(btfitAVgm.tab2,command=function(){
		vgmModList<<-editVgmModel(main.win,vgmModList)
	})

	infobulle(fitvgm.tab2,'Fit a variogram by choosing a model')
	status.bar.display(fitvgm.tab2,txt.stbr1,'Fit a variogram by choosing a model')
	infobulle(combfitVgm.tab2,'Fit a variogram by choosing a model')
	status.bar.display(combfitVgm.tab2,txt.stbr1,'Fit a variogram by choosing a model')

	infobulle(fitAvgm.tab2,'Automatically fit a variogram')
	status.bar.display(fitAvgm.tab2,txt.stbr1,'Automatically fit a variogram')
	infobulle(btfitAVgm.tab2,'Edit the models to use')
	status.bar.display(btfitAVgm.tab2,txt.stbr1,'Edit the models to use, see list provided by gstat packages')

	##############
	tkgrid(fitvgm.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(combfitVgm.tab2,row=0,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(fitAvgm.tab2,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btfitAVgm.tab2,row=1,column=1,sticky='we',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)

	#############################
	frameOpt3<-ttklabelframe(subfr2,text="Elevation data",relief='groove')

	useELV <- tclVar("0")
	useELV.tab2 <- tkcheckbutton(frameOpt3,text="Use elevation to interpolate the data",variable=useELV,anchor='w',justify='left')


	file.plotDem<-tclVar()
	combDem.tab2<-ttkcombobox(frameOpt3, values=unlist(file.list), textvariable=file.plotDem,width=largeur,state='disabled')
	btDem.tab2<-tkbutton(frameOpt3,state='disabled', text="...")
	tkconfigure(btDem.tab2,command=function(){
		nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'netcdf'
			file.opfiles[[nopf+1]]<<-nc.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.plotDem)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1,values=unlist(file.list), textvariable=file.stnfl)
			tkconfigure(combgrdCDF.tab1,values=unlist(file.list), textvariable=file.grdCDF)
			tkconfigure(combDem.tab2,values=unlist(file.list), textvariable=file.plotDem)
			tkconfigure(combShp.tab1,values=unlist(file.list), textvariable=file.plotShp)

		}
	})


	##############
	tkgrid(useELV.tab2,row=0,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(combDem.tab2,row=1,column=1,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btDem.tab2,row=1,column=7,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	#############################
	frameShp<-ttklabelframe(subfr2,text="Shapefiles for boundary",relief='groove')

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
			tkconfigure(combgrdCDF.tab1,values=unlist(file.list), textvariable=file.grdCDF)
			tkconfigure(combDem.tab2,values=unlist(file.list), textvariable=file.plotDem)
			tkconfigure(combShp.tab1,values=unlist(file.list), textvariable=file.plotShp)
		}
	})

	#############################
	tkgrid(combShp.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btShp.tab1,row=0,column=4,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)


	#############################
	tkgrid(frameOpt1,row=0,column=0,sticky='we')
	tkgrid(frameOpt2,row=1,column=0,sticky='we',pady=3)
	tkgrid(frameOpt3,row=2,column=0,sticky='we',pady=3)
	tkgrid(frameShp,row=3,column=0,sticky='we')

	#######################################################################################################
	tkbind(combInterp.tab1,"<<ComboboxSelected>>",function(){
		if(tclvalue(Interp.Method)=='Nearest Neighbor'){
			tkconfigure(maxdist.tab2,state='normal')
			tkconfigure(nmin.tab2,state='disabled')
			tkconfigure(nmax.tab2,state='disabled')
			tkconfigure(idp.tab2,state='disabled')
			tkconfigure(omax.tab2,state='disabled')

			tkconfigure(fitvgm.tab2,state='disabled')
			tkconfigure(fitAvgm.tab2,state='disabled')
			tkconfigure(combfitVgm.tab2,state='disabled')
			tkconfigure(btfitAVgm.tab2,state='disabled')

			tkconfigure(plotVgm.tab3,state='disabled')
		}
		if(tclvalue(Interp.Method)=='IDW'){
			tkconfigure(maxdist.tab2,state='normal')
			tkconfigure(nmin.tab2,state='normal')
			tkconfigure(nmax.tab2,state='normal')
			tkconfigure(idp.tab2,state='normal')
			tkconfigure(omax.tab2,state='normal')

			tkconfigure(fitvgm.tab2,state='disabled')
			tkconfigure(fitAvgm.tab2,state='disabled')
			tkconfigure(combfitVgm.tab2,state='disabled')
			tkconfigure(btfitAVgm.tab2,state='disabled')

			tkconfigure(plotVgm.tab3,state='disabled')
		}
		if(tclvalue(Interp.Method)=='Kriging'){
			tkconfigure(maxdist.tab2,state='normal')
			tkconfigure(nmin.tab2,state='normal')
			tkconfigure(nmax.tab2,state='normal')
			tkconfigure(idp.tab2,state='disabled')
			tkconfigure(omax.tab2,state='normal')

			tkconfigure(fitvgm.tab2,state='normal')
			tkconfigure(fitAvgm.tab2,state='normal')
			if(tclvalue(fitVgmChx)=='0'){
				tkconfigure(combfitVgm.tab2,state='normal')
				tkconfigure(btfitAVgm.tab2,state='disabled')
			}else{
				tkconfigure(combfitVgm.tab2,state='disabled')
				tkconfigure(btfitAVgm.tab2,state='normal')
			}

			tkconfigure(plotVgm.tab3,state='normal')
		}
	})

	######
	tkbind(fitvgm.tab2,"<Button-1>",function(){
		if(tclvalue(Interp.Method)=='Kriging'){
			tkconfigure(combfitVgm.tab2,state='normal')
			tkconfigure(btfitAVgm.tab2,state='disabled')
		}
	})
	tkbind(fitAvgm.tab2,"<Button-1>",function(){
		if(tclvalue(Interp.Method)=='Kriging'){
			tkconfigure(combfitVgm.tab2,state='disabled')
			tkconfigure(btfitAVgm.tab2,state='normal')
		}
	})


	######
	tkbind(useELV.tab2,"<Button-1>",function(){
		if(tclvalue(useELV)=='0'){
			tkconfigure(combDem.tab2	,state='normal')
			tkconfigure(btDem.tab2,state='normal')
		}else{
			tkconfigure(combDem.tab2,state='disabled')
			tkconfigure(btDem.tab2,state='disabled')
		}
	})

	######

#######################################################################################################

	#Tab3
	frTab3<-tkframe(cmd.tab3)
	tkgrid(frTab3,padx=0,pady=1,ipadx=1,ipady=1)
	tkgrid.columnconfigure(frTab3,0,weight=1)

	scrw3<-bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	subfr3<-bwScrollableFrame(scrw3,width=wscrlwin,height=hscrlwin)

	file.save1 <-tclVar()
	lab1.tab3<-tklabel(subfr3,text='File to save interpolated data',anchor='w',justify='left')
	fl2sav.tab3<-tkentry(subfr3,textvariable=file.save1,width=largeur-8)
	bfl2sav.tab3<-tkbutton(subfr3, text="...")
	tkconfigure(bfl2sav.tab3,command=function(){
		file2save <- tkgetSaveFile(initialdir=getwd(),initialfile = "",filetypes = "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}")
		if(is.na(file2save)) tclvalue(file.save1)<-""
		else tclvalue(file.save1)<-file2save
	})

	plotData.tab3<-tkbutton(subfr3, text="Plot Data")
	plotVgm.tab3<-tkbutton(subfr3, text="Plot Variogram",state='disabled')
	InterpData.tab3<-tkbutton(subfr3, text="Interpolate")
	plotInterpVal.tab3<-tkbutton(subfr3, text="Plot Interpolated values")
	remData.tab3<-tkbutton(subfr3, text="Delete some observations")

	sep1.tab3<-ttkseparator(subfr3)
	sep2.tab3<-ttkseparator(subfr3)
	sep3.tab3<-ttkseparator(subfr3)
	sep4.tab3<-ttkseparator(subfr3)
	sep5.tab3<-ttkseparator(subfr3)

	#############
	tkgrid(lab1.tab3,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fl2sav.tab3,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(bfl2sav.tab3,row=1,column=5,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep1.tab3,row=2,column=0,sticky='we',rowspan=1,columnspan=6,pady=5)
	tkgrid(tklabel(subfr3,text=' ',width=3),row=3,column=0,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(plotData.tab3,row=3,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(tklabel(subfr3,text=' ',width=3),row=3,column=0,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep2.tab3,row=4,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(plotVgm.tab3,row=5,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep3.tab3,row=6,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(InterpData.tab3,row=7,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep4.tab3,row=8,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(plotInterpVal.tab3,row=9,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep5.tab3,row=10,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(remData.tab3,row=11,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)

	###############
	##Used to change some obs
	assign('donnees',NULL,envir=EnvInterpolation)
	assign('getDEMFirst',NULL,envir=EnvInterpolation)
	assign('filechange','',envir=EnvInterpolation)

	getDONNE<-function(){
		if(is.null(EnvInterpolation$donnees)){
			donne<-getCDTdata(file.stnfl,file.period)
			donne<-getCDTdata1Date(donne,tclvalue(idate_yrs),tclvalue(idate_mon),tclvalue(idate_day))
#			assign('elvStn',donne$elv,envir=EnvInterpolation)
			if(tclvalue(useELV)=='1'){
				if(EnvInterpolation$filechange!=tclvalue(file.plotDem) | is.null(EnvInterpolation$getDEMFirst)){
					demdata<-getNcdfOpenData(file.plotDem)[[2]]
					if(!is.null(demdata)){
						elv<-getDEMatStationsLoc(donne,demdata)
						assign('getDEMFirst',1,envir=EnvInterpolation)
						assign('filechange',tclvalue(file.plotDem),envir=EnvInterpolation)
					}else{
						elv<-NULL
						insert.txt(main.txt.out,'No DEM data found',format=TRUE)
					}
					 assign('elvDem',elv,envir=EnvInterpolation)
				}else{
					elv<-EnvInterpolation$elvDem
					if(is.null(elv)) insert.txt(main.txt.out,'No DEM data found',format=TRUE)
				}
				donne$elv<-elv
				assign("ELV",elv,envir=EnvInterpolation)
			}else{
				if(is.null(EnvInterpolation$ELV)) donne$elv<-donne$elv
				else donne$elv<-EnvInterpolation$ELV
			}
		}else{
			donne<-EnvInterpolation$donnees
			if(tclvalue(useELV)=='1'){
				if(EnvInterpolation$filechange!=tclvalue(file.plotDem) | is.null(EnvInterpolation$getDEMFirst)){
					demdata<-getNcdfOpenData(file.plotDem)[[2]]
					if(!is.null(demdata)){
						elv<-getDEMatStationsLoc(donne,demdata)
						assign('getDEMFirst',1,envir=EnvInterpolation)
						assign('filechange',tclvalue(file.plotDem),envir=EnvInterpolation)
					}else{
						elv<-NULL
						insert.txt(main.txt.out,'No DEM data found',format=TRUE)
					}
					assign('elvDem',elv,envir=EnvInterpolation)
				}else{
					elv<-EnvInterpolation$elvDem
					#elv<-donne$elv
					if(is.null(elv)) insert.txt(main.txt.out,'No DEM data found',format=TRUE)
				}
				donne$elv<-elv
				assign("ELV",elv,envir=EnvInterpolation)
			}else{
				if(is.null(EnvInterpolation$ELV)) donne$elv<-donne$elv
				else donne$elv<-EnvInterpolation$ELV
			}
		}
		return(donne)
	}


	#############
	notebookTab0<-NULL

	tkconfigure(plotData.tab3,command=function(){

		donne<-getDONNE()

############
##comment after
#		assign("donne",donne,envir=.GlobalEnv)
#
#		interpolParams<-list(mthd=tclvalue(Interp.Method),grdChx=tclvalue(varCreateGrd),ncfila=tclvalue(file.grdCDF),grdCR=newgrdPars,
#		file2save=tclvalue(file.save1),vgmChx=tclvalue(fitVgmChx),VgmMod=tclvalue(fitVgmMod),vgmModList=vgmModList,useELV=tclvalue(useELV),
#		maxdist=tclvalue(maxdist_vars),nmin=tclvalue(nmin_vars),nmax=tclvalue(nmax_vars),idp=tclvalue(idp_vars),omax=tclvalue(omax_vars),elvdiff=tclvalue(elvdiff_vars))
#		assign('interpolParams',interpolParams,envir=.GlobalEnv)
#
#		demdata<-getNcdfOpenData(file.plotDem)[[2]]
#		assign("demdata",demdata,envir=.GlobalEnv)
###########

		if(!is.null(donne)) atLev<-pretty(donne$z)
		listCol<-rainbow(10)
		shpf<-getShpOpenData(file.plotShp)[[2]]
		units<-NA

		imgContainer<-displayCDTdata(tknotes,notebookTab0,donne,atLev,listCol,shpf,units)
		if(!is.null(imgContainer)){
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab0,tab.type,tab.data)
			notebookTab0<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}
	})


	#############
	notebookTab1<-NULL

	tkconfigure(plotVgm.tab3,command=function(){

		donne<-getDONNE()

		imgContainer<-displayVariogramFun(tknotes,notebookTab1,donne,tclvalue(fitVgmChx),tclvalue(fitVgmMod),vgmModList,tclvalue(useELV))
		if(!is.null(imgContainer)){
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab1,tab.type,tab.data)
			notebookTab1<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}
	})


	#############
	outNCdata<-NULL

	tkconfigure(InterpData.tab3,command=function(){

		donne<-getDONNE()
		demdata<-getNcdfOpenData(file.plotDem)[[2]]

		interpolParams<-list(mthd=tclvalue(Interp.Method),grdChx=tclvalue(varCreateGrd),ncfila=tclvalue(file.grdCDF),grdCR=newgrdPars,
		file2save=tclvalue(file.save1),vgmChx=tclvalue(fitVgmChx),VgmMod=tclvalue(fitVgmMod),vgmModList=vgmModList,useELV=tclvalue(useELV),
		maxdist=tclvalue(maxdist_vars),nmin=tclvalue(nmin_vars),nmax=tclvalue(nmax_vars),idp=tclvalue(idp_vars),omax=tclvalue(omax_vars),elvdiff=tclvalue(elvdiff_vars))

		tkconfigure(main.win,cursor='watch');tcl('update')

		tryCatch(outNCdata<<-interpolationProc(donne,demdata,interpolParams),
		#warning=function(w) warningFun(w),
		error=function(e) errorFun(e),finally={
			tkconfigure(main.win,cursor='')
		})

		if(!is.null(outNCdata)) insert.txt(main.txt.out,"Interpolation finished successfully")
	})

	#############
	notebookTab2<-NULL

	tkconfigure(plotInterpVal.tab3,command=function(){
		if(!is.null(outNCdata)) atLev<-pretty(outNCdata[[2]]$value)
		listCol<-rainbow(10)
		shpf<-getShpOpenData(file.plotShp)[[2]]
		units<-NA

		imgContainer<-displayNetCDFdata(tknotes,notebookTab2,outNCdata,atLev,listCol,shpf,units)
		if(!is.null(imgContainer)){
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab2,tab.type,tab.data)
			notebookTab2<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}
	})

	#############
	notebookTab3<-NULL

	tkconfigure(remData.tab3,command=function(){
		donne<-getDONNE()

		if(!is.null(donne)){
#			retNBTab<-tableInterpNotebookTab_unik(tknotes,donne,notebookTab3,tab.type,tab.data)
#			notebookTab3<<-retNBTab$notebookTab
#			tab.type<<-retNBTab$tab.type
#			tab.data<<-retNBTab$tab.data
#			popupAddRemoveRow(tknotes)
#
			if(is.null(donne$elv)) elv<-NA
			else elv<-donne$elv
			dat2disp<-data.frame(id=donne$id,lon=donne$lon,lat=donne$lat,z=donne$z,elv=elv)
			dat2disp<-dat2disp[!is.na(dat2disp$z),]
			dat2disp<-list(donne$date,dat2disp,'/may/be/file/to/save/table')
			retdata<-DisplayInterpData(tknotes,dat2disp,paste('Obs',donne$date,sep='_'))

			ntab<-length(tab.type)
			tab.type[[ntab+1]]<<-'arrInterp'
			tab.data[[ntab+1]]<<-retdata
			tkselect(tknotes,ntab)
			popupAddRemoveRow(tknotes)

			ret.results<<-donne
		}else insert.txt(main.txt.out,'No station data found',format=TRUE)
	})


#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	tkgrid.columnconfigure(cmd.frame,0,weight=1)
	######
	return(cmd.frame)
}


#######################################################################################################
#######################################################################################################



editVgmModel<-function(tt,vgmModList){
	tt1<-tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0<-tkframe(tt1,relief='raised',borderwidth=2)
	frGrd1<-tkframe(tt1)

	yscr<-tkscrollbar(frGrd0, repeatinterval=4,command=function(...) tkyview(textObj,...))
	textObj<-tktext(frGrd0,bg="white",yscrollcommand=function(...) tkset(yscr,...),wrap="word",height=5,width=w.opfiles+5) #
	tkgrid(textObj,yscr)
	tkgrid.configure(yscr,sticky="ns")
	tkgrid.configure(textObj,sticky='nswe')

	tcl("update","idletasks")
	if(length(vgmModList)>0)	 tkinsert(textObj,"end",paste(vgmModList,collapse=', '))

	################################

	bt.prm.OK<-tkbutton(frGrd1, text=" OK ")
	bt.prm.CA<-tkbutton(frGrd1, text="Cancel")
	tkgrid(bt.prm.OK,row=0,column=0,sticky='w',padx=5,pady=1,ipadx=5,ipady=1)
	tkgrid(bt.prm.CA,row=0,column=1,sticky='e',padx=5,pady=1,ipadx=1,ipady=1)

	tkconfigure(bt.prm.OK,command=function(){
		retvars<-tclvalue(tkget(textObj,"0.0","end"))
		retvars<-gsub("[\t\r\n]", "", retvars)
		retvars<-gsub('\\s+', '',retvars)
		retvars<-strsplit(retvars,",")[[1]]
		retvars<-retvars[!is.na(retvars) & retvars!='']
		vgmModList<<-retvars

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA,command=function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(frGrd0,row=0,column=0,sticky='nswe',rowspan=1,columnspan=2,padx=2,pady=2,ipadx=1,ipady=1)
	tkgrid(frGrd1,row=1,column=1,sticky='se',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",tt1))
	tt.h<-as.integer(tkwinfo("reqheight",tt1))
	tt.x<-as.integer(width.scr*0.5-tt.w*0.5)
	tt.y<-as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'Edit Variogram Model')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(vgmModList)
}

