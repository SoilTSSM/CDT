
##########xxxxxxxxxxxxxxxxxx Menu File xxxxxxxxxxxxxxxxxx##########

menu.file <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

##########
tkadd(menu.file, "command", label="Open data.frame",command=function(){
	tkconfigure(main.win,cursor='watch');tcl('update')
	dat.opfiles<-getOpenFiles(main.win,all.opfiles)
	tkconfigure(main.win,cursor='')
	if(!is.null(dat.opfiles)){
		nopf<-length(AllOpenFilesType)
		AllOpenFilesType[[nopf+1]]<<-'ascii'
		AllOpenFilesData[[nopf+1]]<<-dat.opfiles
	}else{
		return(NULL)
	}
})

##########
tkadd(menu.file, "command", label="Open Netcdf file",command=function(){
	tkconfigure(main.win,cursor='watch');tcl('update')
	nc.opfiles<-getOpenNetcdf(main.win,all.opfiles)
	tkconfigure(main.win,cursor='')
	if(!is.null(nc.opfiles)){
		nopf<-length(AllOpenFilesType)
		AllOpenFilesType[[nopf+1]]<<-'netcdf'
		AllOpenFilesData[[nopf+1]]<<-nc.opfiles
	}else{
		return(NULL)
	}
})

##########
tkadd(menu.file, "command", label="Open ESRI Shapefile",command=function(){
	tkconfigure(main.win,cursor='watch');tcl('update')
	shp.opfiles<-getOpenShp(main.win,all.opfiles)
	tkconfigure(main.win,cursor='')
	if(!is.null(shp.opfiles)){
		nopf<-length(AllOpenFilesType)
		AllOpenFilesType[[nopf+1]]<<-'shp'
		AllOpenFilesData[[nopf+1]]<<-shp.opfiles
	}else{
		return(NULL)
	}
})

##########
tkadd(menu.file, "separator")

##########
tkadd(menu.file, "command", label="Open QC/Homogenization session",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	OpenOldCDTtask()
})

##########
tkadd(menu.file, "command", label="Save QC/Homogenization session",command=function() saveCurrentCDTtask())

##########
tkadd(menu.file, "separator")

##########
tkadd(menu.file, "command", label="Save table",command=function(){
	if(!is.null(ReturnExecResults)){
		tkconfigure(main.win,cursor='watch');tcl('update')
		tab2sav<-try(SaveNotebookTabArray(tknotes), silent=TRUE)
		tkconfigure(main.win,cursor='')
		is.ok<- !inherits(tab2sav, "try-error")
		if(is.ok){
			InsertMessagesTxt(main.txt.out,"Table saved successfully")
		}else{
			InsertMessagesTxt(main.txt.out,"The table could not be saved",format=TRUE)
			InsertMessagesTxt(main.txt.out,gsub('[\r\n]','',tab2sav[1]),format=TRUE)
			return(NULL)
		}
	}else{
		return(NULL)
	}
 })
#
##########
tkadd(menu.file, "command", label="Save table As...        ",command=function(){
	tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
	if(!is.na(tabid)){
		if(AllOpenTabType[[tabid]]=="arr"){
			file.to.save<-tclvalue(tkgetSaveFile(initialdir=getwd(),initialfile = "",
			filetypes="{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"))
			Objarray<-AllOpenTabData[[tabid]][[2]]
			tkconfigure(main.win,cursor='watch');tcl('update')
			dat2sav<-tclArray2dataframe(Objarray)
			extFl<-file_ext(basename(file.to.save))
			if(extFl=="csv" | extFl=="CSV") write.csv(dat2sav,file=file.to.save,row.names=FALSE,col.names=TRUE,quote=FALSE)
			else write.table(dat2sav,file.to.save,row.names=FALSE,col.names=TRUE,quote=FALSE)
			tkconfigure(main.win,cursor='')
		}else return(NULL)
	}else return(NULL)
})

##########
tkadd(menu.file, "command", label="Save Image As...",command=function() SavePlot())

##########
tkadd(menu.file, "separator")

##########
tkadd(menu.file,"command", label="Configurations", command=function() configCDT())

##########
tkadd(menu.file, "separator")

##########
tkadd(menu.file,"command", label="Quit CDT", command=function(){
	on.exit({
		#sink(type="message")
		#close(msgOUT)
		options(warn=0)
	})
	tkdestroy(main.win)
})

##########xxxxxxxxxxxxxxxxxx Menu Data Preparation xxxxxxxxxxxxxxxxxx##########

menu.dataprep <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
tkadd(top.menu, "cascade", label="Data Preparation", menu=menu.dataprep, underline=0)

##########
tkadd(menu.dataprep, "command", label="Format CDTs Input Data",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='agg.stn') initpars<-GeneralParameters
		else initpars<-init.params('agg.stn','daily')
	}else initpars<-init.params('agg.stn','daily')
	GeneralParameters<<-AggregateInputStationData(main.win,initpars)
})

##########
tkadd(menu.dataprep, "separator")

##########
##
tkadd(menu.dataprep, "command", label="Assess Data Availability",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	tkconfigure(stn.choix.cb,state='disabled')
	tkconfigure(setting.button,state='disabled')
	tkconfigure(stn.choix.prev,state='disabled')
	tkconfigure(stn.choix.next,state='disabled')
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_assdata)){
		lcmd.frame<<-AssessDataPanelCmd()
		lcmd.frame_assdata<<-1
	}
})

##########
tkadd(menu.dataprep, "separator")

##########
tkadd(menu.dataprep, "command", label="Download DEM",command=function(){
	getDEMFun(main.win)
})

##########
tkadd(menu.dataprep, "command", label="Download Country boundary",command=function(){
	getCountryShapefile(main.win)
})

##########
#tkadd(menu.dataprep, "separator")

##########

tkadd(menu.dataprep, "command", label="Download RFE data",command=function(){
	DownloadRFE(main.win)
})


##########xxxxxxxxxxxxxxxxxx Menu Quality Control xxxxxxxxxxxxxxxxxx##########

menu.qchom <- tkmenu(top.menu, tearoff = FALSE, relief="flat")
tkadd(top.menu, "cascade", label = "Quality Control", menu = menu.qchom , underline=0)


##########
##check coordninates
tkadd(menu.qchom, "command", label="Check Stations Coordinates",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='chk.coords') initpars<-GeneralParameters
		else{
			initpars<-init.params('chk.coords','daily')
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('chk.coords','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}

	GeneralParameters<<-excludeOutStn(main.win,initpars)
})

##########
tkadd(menu.qchom, "separator")

##########
##QC one Run
menu.qc <- tkmenu(top.menu, tearoff = FALSE)
tkadd(menu.qchom, "cascade", label = "QC Check: One station",menu=menu.qc)


##########Rain
menu.qcRain <- tkmenu(menu.qc, tearoff = FALSE)
tkadd(menu.qc, "cascade", label = "Rainfall",menu=menu.qcRain)

##Zeros check
tkadd(menu.qcRain, "command", label = "Zeros check",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='zero.check') initpars<-GeneralParameters
		else{
			initpars<-init.params('zero.check','daily')
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('zero.check','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'one'
	GeneralParameters<<-qcGetZeroCheckInfo(main.win,initpars)
})

##Outliers check
tkadd(menu.qcRain, "command", label = "Outliers Check",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='qc.rain') initpars<-GeneralParameters
		else{
			initpars<-init.params('qc.rain',as.character(GeneralParameters$period))
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('qc.rain','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'one'
	GeneralParameters<<-qc.get.info.rain(main.win,initpars)
 })

##########
tkadd(menu.qc, "separator")

##########Temperatures
tkadd(menu.qc, "command", label = "Temperatures",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='qc.temp') initpars<-GeneralParameters
		else{
			initpars<-init.params('qc.temp',as.character(GeneralParameters$period))
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('qc.temp','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'one'
	GeneralParameters<<-qc.get.info.txtn(main.win,initpars)
})

##########
#tkadd(menu.qchom, "separator")

##########
##Qc all Run
menu.qc1 <- tkmenu(top.menu, tearoff = FALSE)
tkadd(menu.qchom, "cascade", label = "QC Check: All stations",menu=menu.qc1)

##########Rain
menu.qcRain1 <- tkmenu(menu.qc1, tearoff = FALSE)
tkadd(menu.qc1, "cascade", label = "Rainfall",menu=menu.qcRain1)

##Zeros check
tkadd(menu.qcRain1, "command", label = "Zeros check",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='zero.check') initpars<-GeneralParameters
		else{
			initpars<-init.params('zero.check','daily')
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('zero.check','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'all'
	GeneralParameters<<-qcGetZeroCheckInfo(main.win,initpars)
})

##Outliers check
tkadd(menu.qcRain1, "command", label = "Outliers Check",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='qc.rain') initpars<-GeneralParameters
		else{
			initpars<-init.params('qc.rain',as.character(GeneralParameters$period))
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('qc.rain','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'all'
	GeneralParameters<<-qc.get.info.rain(main.win,initpars)
 })

##########
tkadd(menu.qc1, "separator")

##########Temperatures
tkadd(menu.qc1, "command", label = "Temperatures",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='qc.temp') initpars<-GeneralParameters
		else{
			initpars<-init.params('qc.temp',as.character(GeneralParameters$period))
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('qc.temp','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	initpars$AllOrOne<-'all'
	GeneralParameters<<-qc.get.info.txtn(main.win,initpars)
})

##########
tkadd(menu.qchom, "separator")

##########
tkadd(menu.qchom, "command", label="Aggregate zeros checked stations",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	initpars<-init.params('agg.zc','daily')
	GeneralParameters<<-AggregateOutputStationData(main.win,initpars)
})


##########
tkadd(menu.qchom, "command", label="Aggregate checked stations",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	initpars<-init.params('agg.qc','daily')
	GeneralParameters<<-AggregateOutputStationData(main.win,initpars)
})

##########
tkadd(menu.qchom, "separator")

##########
# Menu homogenization

menu.homog<-tkmenu(top.menu, tearoff = FALSE)
tkadd(menu.qchom, "cascade", label = "Homogeneity Test",menu=menu.homog)

##########
###RHtestsV4
tkadd(menu.homog, "command", label="RHtestsV4",command=function(){
	agreementfl<-file.path(apps.dir,'configure','RHtestsV4_User_Agreement',fsep = .Platform$file.sep)
	if(file.exists(agreementfl)) proceed<-TRUE
	else{
		yesAgree<-RHtests_license()
		if(yesAgree){
			proceed<-TRUE
			cat("I Agree",file=agreementfl)
		}
		else proceed<-FALSE
	}
	if(proceed){
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
		tkconfigure(spinH,state='normal')
		tkconfigure(spinV,state='normal')

		if(is.null(lcmd.frame_rhtests)){
			lcmd.frame<<-RHtestsV4Cmd()
			lcmd.frame_rhtests<<-1
		}
	}
})

##########
tkadd(menu.homog, "separator")

#########
###Methods used by CDT
tkadd(menu.homog, "command", label="CDT Homogenization Methods",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='homog') initpars<-GeneralParameters
		else{
			initpars<-init.params('homog','dekadal')
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('homog','dekadal')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	GeneralParameters<<-homogen.get.info(main.win,initpars)
})

##########
tkadd(menu.qchom, "command", label="Aggregate homogenized stations",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	initpars<-init.params('agg.hom','dekadal')
	GeneralParameters<<-AggregateOutputStationData(main.win,initpars)
})


##########xxxxxxxxxxxxxxxxxx Menu Merging Data xxxxxxxxxxxxxxxxxx##########

menu.mrg <- tkmenu(top.menu, tearoff = FALSE, relief="flat")
tkadd(top.menu, "cascade", label = "Merging Data", menu = menu.mrg, underline=0)

##########rain
menu.mrg.rain <- tkmenu(top.menu, tearoff = FALSE)
tkadd(menu.mrg, "cascade", label = "Merging Rainfall",menu=menu.mrg.rain)

##########
tkadd(menu.mrg.rain, "command", label = "Compute mean Gauge-RFE bias",background='lightblue',command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='coefbias.rain') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('coefbias.rain','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('coefbias.rain','dekadal')
	}
	GeneralParameters<<-coefBiasGetInfoRain(main.win,initpars)
})

##########
tkadd(menu.mrg.rain, "separator")

##########
tkadd(menu.mrg.rain, "command", label = "Apply bias correction",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='rmbias.rain') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('rmbias.rain','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('rmbias.rain','dekadal')
	}
	GeneralParameters<<-rmvBiasGetInfoRain(main.win,initpars)
})

##########
tkadd(menu.mrg.rain, "command", label = "Merging Data",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='merge.rain') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('merge.rain','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('merge.rain','dekadal')
	}
	GeneralParameters<<-mergeGetInfoRain(main.win,initpars)
})

##########temperature
tkadd(menu.mrg, "separator")
menu.mrg.temp <- tkmenu(top.menu, tearoff = FALSE)
tkadd(menu.mrg, "cascade", label = "Merging Temperature",menu=menu.mrg.temp)

##########
tkadd(menu.mrg.temp, "command", label = "Compute Downscaling Coefficients",background='lightblue',command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='coefdown.temp') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('coefdown.temp','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('coefdown.temp','dekadal')
	}
	GeneralParameters<<-coefDownGetInfoTemp(main.win,initpars)
})

##########
tkadd(menu.mrg.temp, "separator")

##########
tkadd(menu.mrg.temp, "command", label = "Reanalysis Downscaling",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='down.temp') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('down.temp','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('down.temp','dekadal')
	}
	GeneralParameters<<-downGetInfoDekTempReanal(main.win,initpars)
})

##########
tkadd(menu.mrg.temp, "separator")

##########
tkadd(menu.mrg.temp, "command", label = "Compute Bias Coefficients",background='lightblue',command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='coefbias.temp') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('coefbias.temp','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('coefbias.temp','dekadal')
	}
	GeneralParameters<<-biasGetInfoTempDown(main.win,initpars)
})

##########
tkadd(menu.mrg.temp, "separator")

##########
tkadd(menu.mrg.temp, "command", label = "Bias Adjustment",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='adjust.temp') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('adjust.temp','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('adjust.temp','dekadal')
	}
	GeneralParameters<<-adjGetInfoTempDownReanal(main.win,initpars)
})

##########
tkadd(menu.mrg.temp, "command", label = "Merging Data",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='merge.temp') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('merge.temp','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('merge.temp','dekadal')
	}
	GeneralParameters<<-mrgGetInfoTemp(main.win,initpars)
})

##########
tkadd(menu.mrg, "separator")

##########
tkadd(menu.mrg, "command", label="Updating dekadal Rainfall",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='merge.dekrain') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('merge.dekrain','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('merge.dekrain','dekadal')
	}
	GeneralParameters<<-mergeDekadInfoRain(main.win,initpars)
})

##########
tkadd(menu.mrg, "separator")

##########
tkadd(menu.mrg, "command", label="Mali - Mise à jour décadaire",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='mali.dekrain') initpars<-GeneralParameters
		else{
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
			initpars<-init.params('mali.dekrain','dekadal')
		}
	}else{
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		initpars<-init.params('mali.dekrain','dekadal')
	}
	GeneralParameters<<-update1DekInfo_Mali(main.win,initpars)
})

##########
#tkadd(menu.mrg, "separator")
#menu.mrgtls <- tkmenu(top.menu, tearoff = FALSE)
#tkadd(menu.mrg, "cascade", label = "Merging Tools",menu=menu.mrgtls)
#tkadd(menu.mrgtls, "command", label = "Handling NetCDF Files",command=function(){
#source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
#})
#tkadd(menu.mrgtls, "command", label = "Format Input Data",command=function(){
#source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
#})

##########xxxxxxxxxxxxxxxxxx Menu Data Processing xxxxxxxxxxxxxxxxxx##########

menu.dataproc <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
tkadd(top.menu, "cascade", label="Data Processing", menu=menu.dataproc, underline=5)


##########
tkadd(menu.dataproc, "command", label="Interpolation",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_interpol)){
		lcmd.frame<<-InterpolationPanelCmd()
		lcmd.frame_interpol<<-1
	}
})

##########
tkadd(menu.dataproc, "separator")

##########
tkadd(menu.dataproc, "command", label="Precipitation Validation",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_valid)){
		lcmd.frame<<-ValidationPanelCmd()
		lcmd.frame_valid<<-1
	}
})


##########
tkadd(menu.dataproc, "separator")


##########
tkadd(menu.dataproc, "command", label="Aggregating Time Series",command=function(){
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action=='agg.ts') initpars<-GeneralParameters
		else{
			initpars<-init.params('agg.ts',as.character(GeneralParameters$period))
			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
		}
	}else{
		initpars<-init.params('agg.ts','daily')
		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	}
	GeneralParameters<<-mainDialogAggTs(main.win,initpars)
})

##########
tkadd(menu.dataproc, "separator")

##########
tkadd(menu.dataproc, "command", label="Extract NetCDF gridded data",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_extrdata)){
		lcmd.frame<<-ExtractDataPanelCmd()
		lcmd.frame_extrdata<<-1
	}
})

###########
#tkadd(menu.dataproc, "command", label="Compute Climatology",state='disabled',command=function(){
#	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
#	if(!is.null(GeneralParameters)){
#		if(GeneralParameters$action=='clim.ts') initpars<-GeneralParameters
#		else{
#			#initpars<-init.params('clim.ts',as.character(GeneralParameters$period))
#			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
#		}
#	}else{
#		#initpars<-init.params('clim.ts','dekadal')
#		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
#	}
#	#GeneralParameters<<-mainDialogClimatogy(main.win,initpars)
#})

###########
#tkadd(menu.dataproc, "separator")

###########
#tkadd(menu.dataproc, "command", label="Calculate  Anomalies",state='disabled',command=function(){
#	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
#	if(!is.null(GeneralParameters)){
#		if(GeneralParameters$action=='anom.ts') initpars<-GeneralParameters
#		else{
#			#initpars<-init.params('anom.ts',as.character(GeneralParameters$period))
#			source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
#		}
#	}else{
#		#initpars<-init.params('anom.ts','dekadal')
#		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
#	}
#	#GeneralParameters<<-mainDialogAnomalies(main.win,initpars)
#})


##########
#menu.map <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
#tkadd(top.menu, "cascade", label="Map", menu=menu.map)
#tkadd(menu.map, "command", label="CDT Station Data",state='disabled',command=function(){stationPlot()})
#tkadd(menu.map, "separator")
#tkadd(menu.map, "command", label="NetCDF Gridded Data",state='disabled',command=function(){netcdfPlot()})
#tkadd(menu.map, "separator")
#tkadd(menu.map, "command", label="Merging Output Maps",state='disabled',command=function(){mergingComparePlot()})

##########xxxxxxxxxxxxxxxxxx Menu Plot Data xxxxxxxxxxxxxxxxxx##########

menu.plot <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
tkadd(top.menu, "cascade", label="Plot Data", menu=menu.plot, underline=4)

##########
tkadd(menu.plot, "command", label="Plot CDT data format",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_CDTffrtPlot)){
		lcmd.frame<<-PlotCDTDataFormatCmd()
		lcmd.frame_CDTffrtPlot<<-1
	}
})

##########
tkadd(menu.plot, "separator")

##########
tkadd(menu.plot, "command", label="Plot NetCDF gridded data",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_grdNcdfPlot)){
		lcmd.frame<<-PlotGriddedNcdfCmd()
		lcmd.frame_grdNcdfPlot<<-1
	}
})

##########
tkadd(menu.plot, "separator")

##########
tkadd(menu.plot, "command", label="Plot Merging Outputs",command=function(){
	source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
	source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
	tkconfigure(spinH,state='normal')
	tkconfigure(spinV,state='normal')
	if(is.null(lcmd.frame_mergePlot)){
		lcmd.frame<<-PlotMergingOutputCmd()
		lcmd.frame_mergePlot<<-1
	}
})

##########xxxxxxxxxxxxxxxxxx Menu help xxxxxxxxxxxxxxxxxx##########

menu.aide <- tkmenu(top.menu, tearoff=FALSE, relief="flat")
tkadd(top.menu, "cascade", label="Help", menu=menu.aide, underline=0)

##########
tkadd(menu.aide, "command", label="Help         ",command=function(){
	browseURL(paste0('file://',file.path(apps.dir,'help','html','index.html',fsep = .Platform$file.sep)))
})

##########
tkadd(menu.aide, "separator")

##########
tkadd(menu.aide, "command", label="About CDT",command=function() aboutCDT())

########
