
ValidationPanelCmd<-function(){
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
		hscrlwin<-h.scale(31.5)
	}else{
		wscrlwin<-w.scale(21.7)
		hscrlwin<-h.scale(38.5)
	}


	###################

	cmd.frame<-tkframe(panel.left)

	tknote.cmd<-bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd ,sticky='nwes')

	cmd.tab1 <- bwAddTab(tknote.cmd,text="General")
	cmd.tab2 <- bwAddTab(tknote.cmd,text="Validation")
	bwRaiseTab(tknote.cmd,cmd.tab1)


#######################################################################################################

	#Tab1
	frTab1<-tkframe(cmd.tab1)
	tkgrid(frTab1,padx=0,pady=1,ipadx=1,ipady=1)

	scrw1<-bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	subfr1<-bwScrollableFrame(scrw1,width=wscrlwin,height=hscrlwin)

	##############
	frameStn<-ttklabelframe(subfr1,text="Gauge data file",relief='groove')

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

	#############################
	tkgrid(combPrd.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(combStnfl.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=2,ipadx=1,ipady=1)
	tkgrid(btStnfl.tab1,row=1,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=2,ipadx=1,ipady=1)

	##############
	frameNcdf<-ttklabelframe(subfr1,text="NetCDF files",relief='groove')

	#######################
	labNcdir.tab1<-tklabel(frameNcdf,text="Directory of NetCDF files",anchor='w',justify='left')

	dirNetCDF <-tclVar()
	dirCDF.tab1<-tkentry(frameNcdf,textvariable=dirNetCDF,width=largeur) #
	bdirCDF.tab1<-tkbutton(frameNcdf, text="...")
	tkconfigure(bdirCDF.tab1,command=function(){
		dir4cdf<-tk_choose.dir(tclvalue(dirNetCDF), "")
		if(is.na(dir4cdf)) tclvalue(dirNetCDF)<-""
		else tclvalue(dirNetCDF)<-dir4cdf
	})


	netCDFff<-tclVar("rfe%s_%s-dk%s.nc")
	cap1.tab1<-tklabel(frameNcdf,text="NetCDF file format",anchor='e',justify='right')
	netCDFff.tab1<-tkentry(frameNcdf,textvariable=netCDFff,width=wncdf_ff)
	infobulle(netCDFff.tab1,'Enter the format of the NetCDF files names,\nexample: rfe1983_01-dk1.nc')
	status.bar.display(netCDFff.tab1,txt.stbr1,'Enter the format of the NetCDF files names, example: rfe1983_01-dk1.nc')

	labRFE.tab1<-tklabel(frameNcdf,text="NetCDF's sample file",anchor='w',justify='left')

	file.grdCDF <- tclVar()
	combgrdCDF.tab1<-ttkcombobox(frameNcdf, values=unlist(file.list), textvariable=file.grdCDF,width=largeur)
	btgrdCDF.tab1<-tkbutton(frameNcdf, text="...")
	tkconfigure(btgrdCDF.tab1,command=function(){
		fileopen<-tclvalue(tkgetOpenFile(initialdir=tclvalue(dirNetCDF),initialfile = "",filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen=="" | is.na(fileopen) | fileopen=="NA") return(NULL)
		nc.opfiles1<-preview.data.nc(main.win,fileopen,"")
		nc.opfiles<-list(basename(fileopen),nc.opfiles1,fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles,"end",basename(fileopen))
			nopf<-length(type.opfiles)
			type.opfiles[[nopf+1]]<<-'netcdf'
			file.opfiles[[nopf+1]]<<-nc.opfiles
			file.list[[length(file.list)+1]]<<-file.opfiles[[nopf+1]][[1]]
			tclvalue(file.grdCDF)<-file.opfiles[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1,values=unlist(file.list), textvariable=file.stnfl)
			tkconfigure(combgrdCDF.tab1,values=unlist(file.list), textvariable=file.grdCDF)
		}else return(NULL)
	})
	infobulle(combgrdCDF.tab1,'Choose the file in the list')
	status.bar.display(combgrdCDF.tab1,txt.stbr1,'File containing a sample of NetCDF data')
	infobulle(btgrdCDF.tab1,'Browse file if not listed')
	status.bar.display(btgrdCDF.tab1,txt.stbr1,'Browse file if not listed')


	#############################
	tkgrid(labNcdir.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dirCDF.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(bdirCDF.tab1,row=1,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(cap1.tab1,row=2,column=0,sticky='e',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(netCDFff.tab1,row=2,column=2,sticky='w',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)

	tkgrid(labRFE.tab1,row=3,column=0,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(combgrdCDF.tab1,row=4,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btgrdCDF.tab1,row=4,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	##############
	frameDirSav<-ttklabelframe(subfr1,text="Directory to save result",relief='groove')

	#######################
	file.save1 <-tclVar()
	fl2sav.tab1<-tkentry(frameDirSav,textvariable=file.save1,width=largeur) #
	bfl2sav.tab1<-tkbutton(frameDirSav, text="...")
	tkconfigure(bfl2sav.tab1,command=function() fileORdir2Save(file.save1,isFile=FALSE))

	infobulle(fl2sav.tab1,'Enter the full path to the directory  to save result')
	status.bar.display(fl2sav.tab1,txt.stbr1,'Enter the full path to the directory to save extracted data')
	infobulle(bfl2sav.tab1,'Browse here the full path to the directory to save result')
	status.bar.display(bfl2sav.tab1,txt.stbr1,'Browse here the full path to the directory to save extracted data')

	#############################
	tkgrid(fl2sav.tab1,row=0,column=0,sticky='we',rowspan=1,columnspan=5,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(bfl2sav.tab1,row=0,column=5,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	#############################
	tkgrid(frameStn,row=0,column=0,sticky='we')
	tkgrid(frameNcdf,row=1,column=0,sticky='we',pady=3)
	tkgrid(frameDirSav,row=2,column=0,sticky='we',pady=3)

#######################################################################################################

	#Tab2
	frTab2<-tkframe(cmd.tab2) #,relief='sunken',bd=2
	tkgrid(frTab2,padx=5,pady=5,ipadx=2,ipady=2)

	scrw2<-bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	subfr2<-bwScrollableFrame(scrw2,width=wscrlwin,height=hscrlwin)

	validate.tab2<-tkbutton(subfr2, text="EXECUTE")
	stats.tab2<-tkbutton(subfr2, text="Statistics")
	scatt.tab2<-tkbutton(subfr2, text="Gauge-Satellite Plot")
	ecdf.tab2<-tkbutton(subfr2, text="CDF Plot")

	sep1.tab2<-ttkseparator(subfr2)
	sep2.tab2<-ttkseparator(subfr2)
	sep3.tab2<-ttkseparator(subfr2)

	#############################
	tkgrid(tklabel(subfr2,text=' ',width=6),row=0,column=0,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(validate.tab2,row=0,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(tklabel(subfr2,text=' ',width=6),row=0,column=5,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep1.tab2,row=1,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(stats.tab2,row=2,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep2.tab2,row=3,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(scatt.tab2,row=4,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep3.tab2,row=5,column=0,sticky='we',rowspan=1,columnspan=6,pady=3)
	tkgrid(ecdf.tab2,row=6,column=2,sticky='we',rowspan=1,columnspan=2,padx=1,pady=1,ipadx=1,ipady=1)

	#############################

	outValiddata<-NULL

	tkconfigure(validate.tab2,command=function(){

		donne<-getCDTdata(file.stnfl,file.period)
		rfedata<-getNcdfOpenData(file.grdCDF)
		retValidParams<-list(donne=donne,rfedata=rfedata,ncdir=tclvalue(dirNetCDF),ncformat=tclvalue(netCDFff),dir2sav=tclvalue(file.save1),filestn=tclvalue(file.stnfl))

#assign('retValidParams',retValidParams,envir=.GlobalEnv)

		tkconfigure(main.win,cursor='watch')
		insert.txt(main.txt.out,"Validation.................")
		tcl('update')

		tryCatch(outValiddata<<-ValidationDataFun(retValidParams),
		#warning=function(w) warningFun(w),
		error=function(e) errorFun(e),finally={
			tkconfigure(main.win,cursor='')
		})
		if(!is.null(outValiddata)) insert.txt(main.txt.out,"Validation finished successfully")
	})

	####

	tkconfigure(stats.tab2,command=function(){
		if(!is.null(outValiddata)){
			dat2disp<-outValiddata$stat
			retdata<-DisplayHomInfo(tknotes,dat2disp,'Statistics')
			ntab<-length(tab.type)
			tab.type[[ntab+1]]<<-'arrValid'
			tab.data[[ntab+1]]<<-retdata
			tkselect(tknotes,ntab)
		}
	})

	####
	notebookTab1<-NULL
	tkconfigure(scatt.tab2,command=function(){
		if(!is.null(outValiddata)){
			imgContainer<-displayGGvsSatFun(tknotes,notebookTab1,outValiddata)
			if(!is.null(imgContainer)){
				retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab1,tab.type,tab.data)
				notebookTab1<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}
		}
	})

	####
	notebookTab2<-NULL
	tkconfigure(ecdf.tab2,command=function(){
		if(!is.null(outValiddata)){
			imgContainer<-displayCDFGGvsSatFun(tknotes,notebookTab2,outValiddata)
			if(!is.null(imgContainer)){
				retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,notebookTab2,tab.type,tab.data)
				notebookTab2<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}
		}
	})

#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	######
	return(cmd.frame)
}


