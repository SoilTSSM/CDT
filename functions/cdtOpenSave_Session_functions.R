saveCurrentCDTtask<-function(){
	filename <- tclvalue(tkgetSaveFile(initialfile="", defaultextension=".cdt",filetypes="{{CDT Files} {.cdt}} {{All files} {*.*}}"))
	if (filename != "" | filename !='NA' | !is.na(filename)){
		lastStnChoix<-tclvalue(stn.choix.val)
		save(type.opfiles,file.opfiles,
		gal.params,ret.results,
		stn.choix,lastStnChoix,
		EnvQcOutlierData,EnvQcZeroChkData,
		EnvHomogzData,adjDon,
		file=filename)
	}else{
		insert.txt(main.txt.out,'Current session could not be saved correctly',format=TRUE)
		return(NULL)
	}
}

#######################################################
OpenOldCDTtask<-function(){
	file.opfiles<<-vector(mode='list',length = 0)
	type.opfiles<<-vector(mode='list',length = 0)
	tkdelete(all.opfiles,0,as.numeric(tclvalue(tksize(all.opfiles)))-1)

	fileOpenCDT<-tkgetOpenFile(initialdir=getwd(),initialfile = "",filetypes="{{CDT Files} {.cdt}} {{All files} {*.*}}")
	if(tclvalue(fileOpenCDT)!='' | !is.na(tclvalue(fileOpenCDT))) load(tclvalue(fileOpenCDT))
	if(gal.params$action=='qc.rain'	| gal.params$action=='qc.temp' | gal.params$action=='homog' | gal.params$action=='zero.check'){
		if(gal.params$action=='qc.rain'	| gal.params$action=='qc.temp'){
			EnvQcOutlierData<<-EnvQcOutlierData
			if(gal.params$AllOrOne=='one'){
				tkconfigure(setting.button,state='normal')
				stateReplaceAll<-'disabled'
			} 
			if(gal.params$AllOrOne=='all'){
				tkconfigure(setting.button,state='disabled')
				stateReplaceAll<-'normal'
			} 
			if(as.character(gal.params$use.method$Values[1])=='0'){
				tkconfigure(stn.choix.prev,state='normal')
				tkconfigure(stn.choix.next,state='normal')
			}
			if(gal.params$action=='qc.temp') tclvalue(XYCoordinates)<<-paste(c(as.character(gal.params$parameter[[2]][,4]),as.character(gal.params$parameter[[2]][,5])),sep='',collapse=' ')
			if(gal.params$action=='qc.rain') tclvalue(XYCoordinates)<<-paste(c(as.character(gal.params$parameter[[2]][,3]),as.character(gal.params$parameter[[2]][,4])),sep='',collapse=' ')
			lcmd.frame<<-QcCmdBut(stateReplaceAll)
			lcmd.frame_qc<<-1
		}else if(gal.params$action=='homog'){
			EnvHomogzData<<-EnvHomogzData
			tkconfigure(setting.button,state='normal')
			if(as.character(gal.params$use.method$Values[2])=='0'){
				tkconfigure(stn.choix.prev,state='normal')
				tkconfigure(stn.choix.next,state='normal')
			}
			retcmdpars<-HomogCmdBut(gal.params)
			lcmd.frame<<-retcmdpars[[1]]
			lcmd.frame_homo<<-1
			gal.params<-retcmdpars[[2]]
			adjDon<<-adjDon
		}else if(gal.params$action=='zero.check'){
			EnvQcZeroChkData<<-EnvQcZeroChkData
			if(gal.params$AllOrOne=='one'){
				tkconfigure(setting.button,state='normal')
				stateReplaceAll<-'disabled'
			}
			if(gal.params$AllOrOne=='all'){
				tkconfigure(setting.button,state='disabled')
				stateReplaceAll<-'normal'
			}
			tkconfigure(stn.choix.prev,state='normal')
			tkconfigure(stn.choix.next,state='normal')
			lcmd.frame<<-QcZeroCheckCmd(stateReplaceAll)
			lcmd.frame_qc0Chck<<-1
		}
		#####
		tclvalue(stn.choix.val)<-lastStnChoix
		tkconfigure(stn.choix.cb,values=stn.choix, textvariable=stn.choix.val)

		tkconfigure(spinH,state='normal')
		tkconfigure(spinV,state='normal')

		nopfs<-length(type.opfiles)
		if(nopfs>0) for(j in 1:nopfs) tkinsert(all.opfiles,"end",file.opfiles[[j]][[1]])

		gal.params<<-gal.params
		ret.results<<-ret.results
		stn.choix<<-stn.choix
		type.opfiles<<-type.opfiles
		file.opfiles<<-file.opfiles
	}else insert.txt(main.txt.out,'Not QC or Homogenization',format=TRUE)
}

#####################################################
