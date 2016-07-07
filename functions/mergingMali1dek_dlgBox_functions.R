update1DekInfo_Mali<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows") largeur<-33
	else largeur<-31

	######################################
	tt<-tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog<-tkframe(tt,relief='raised',borderwidth=2)
	frButt<-tkframe(tt)

	fr.A<-tkframe(frDialog,relief='sunken',borderwidth=2)
	fr.B<-tkframe(frDialog,relief='sunken',borderwidth=2)
	fr.C<-tkframe(frDialog,relief='sunken',borderwidth=2)
	fr.D<-tkframe(frDialog,relief='sunken',borderwidth=2)

	tkgrid(fr.A,sticky='w',padx=5,pady=5)
	tkgrid(fr.B,sticky='w',padx=5,pady=5)
	tkgrid(fr.C,sticky='we',padx=5,pady=5)
	tkgrid(fr.D,sticky='w',padx=5,pady=5)

	####
	infobulle(fr.A,'Cocher la case et fournir le fichier RFE si le fichier est téléchargé séparément')
	status.bar.display(fr.A,TextOutputVar,'Cocher la case et fournir le fichier RFE si le fichier est téléchargé séparément')

	cbAdownRFE <- tclVar(as.character(GeneralParameters$rfeDownSep))
	cbAdownRFE_f <- tkcheckbutton(fr.A,variable=cbAdownRFE,text='RFE déjà télécharger',anchor='w',justify='left')

	if(as.character(GeneralParameters$rfeDownSep)=='1') stateRFE<-'normal'
	else stateRFE<-'disabled'

	fileRFE <- tclVar(as.character(GeneralParameters$file.io$Values[2]))
	enAdownRFE<-tkentry(fr.A,textvariable=fileRFE,width=largeur,state=stateRFE)
	btAdownRFE<-tkbutton(fr.A, text="...",state=stateRFE)
	tkconfigure(btAdownRFE,command=function(){
		ncfileopen<-tclvalue(tkgetOpenFile(initialdir=getwd(),initialfile = "",filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(ncfileopen=="" | is.na(ncfileopen)){
			tkmessageBox(message="Choisir le fichier contenant les données RFE",icon="warning",type="ok")
		}

		if(!file.exists(ncfileopen)){
			tkmessageBox(message=paste("Le fichier",ncfileopen,"n'existe pas"),icon="warning",type="ok")
		}
		tclvalue(fileRFE)<-ncfileopen
	})

	tkgrid(cbAdownRFE_f,row=0,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(enAdownRFE,row=1,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(btAdownRFE,row=1,column=1,sticky='e',padx=1,pady=1,ipadx=1,ipady=1)

	#####
	infobulle(fr.B,'Cocher la case si les données des stations pour la décade sont disponiples')
	status.bar.display(fr.B,TextOutputVar,'Cocher la case si les données des stations pour la décade sont disponiples')

	cbStnDispo <- tclVar(as.character(GeneralParameters$stnDataDispo))
	cbStnDispo_f <- tkcheckbutton(fr.B,variable=cbStnDispo,text='Données des stations disponibles',anchor='w',justify='left')

	if(as.character(GeneralParameters$stnDataDispo)=='1') stateSTN<-'normal'
	else stateSTN<-'disabled'

	file.stnfl <- tclVar(as.character(GeneralParameters$file.io$Values[1]))
	cb.stnfl<-ttkcombobox(fr.B, values=unlist(listOpenFiles), textvariable=file.stnfl,state=stateSTN,width=largeur-2)
	bt.stnfl<-tkbutton(fr.B, text="...",state=stateSTN)
	tkconfigure(bt.stnfl,command=function(){
		dat.opfiles<-getOpenFiles(tt,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
		}else{
			return(NULL)
		}
	})

	tkgrid(cbStnDispo_f,row=0,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(cb.stnfl,row=1,column=0,sticky='w',padx=2,pady=1,ipadx=1,ipady=1)
	tkgrid(bt.stnfl,row=1,column=1,sticky='e',padx=2,pady=1,ipadx=1,ipady=1)

	###
	infobulle(fr.C,'Préciser ici la date de la décade')
	status.bar.display(fr.C,TextOutputVar,'Préciser ici la date de la décade')

	date.yrs<-tclVar(as.character(GeneralParameters$dates.mrg$Values[1]))
	date.mon<-tclVar(as.character(GeneralParameters$dates.mrg$Values[2]))
	date.dek<-tclVar(as.character(GeneralParameters$dates.mrg$Values[3]))

	date.txt<-tklabel(fr.C,text='Date',anchor='e',justify='right')
	yrs.txt<-tklabel(fr.C,text='Année')
	mon.txt<-tklabel(fr.C,text='Mois')
	dek.txt<-tklabel(fr.C,text='Décade')

	yrs1.v<-tkentry(fr.C, width=6,textvariable=date.yrs,justify = "right")
	mon1.v<-tkentry(fr.C, width=6,textvariable=date.mon,justify = "right")
	dek1.v<-tkentry(fr.C, width=6,textvariable=date.dek,justify = "right")

	tkgrid(date.txt,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(yrs.txt,row=0,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(mon.txt,row=0,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(dek.txt,row=0,column=3,sticky='ew',padx=1,pady=1)
	tkgrid(yrs1.v,row=1,column=1,sticky='ew',padx=1,pady=1)
	tkgrid(mon1.v,row=1,column=2,sticky='ew',padx=1,pady=1)
	tkgrid(dek1.v,row=1,column=3,sticky='ew',padx=1,pady=1)

	####
	dirsave.txt<-tklabel(fr.D,text='Répertoire pour sauvegarder les résultats')

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.file.save<-tkentry(fr.D,textvariable=file.save1,width=largeur)
	bt.file.save<-tkbutton(fr.D, text="...")
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
			if(is.na(file2save1)) tclvalue(file.save1)<-as.character(GeneralParameters$file.io$Values[3])
			else{
				dir.create(file2save1,showWarnings=FALSE,recursive=TRUE)
				tclvalue(file.save1)<-file2save1
			}
	})

	tkgrid(dirsave.txt,row=0,column=0,columnspan=2,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(en.file.save,row=1,column=0,sticky='w',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(bt.file.save,row=1,column=1,sticky='e',padx=1,pady=1,ipadx=1,ipady=1)


	#######################

	tkbind(cbAdownRFE_f,"<Button-1>",function(){
		if(tclvalue(cbAdownRFE)=="0"){
			tkconfigure(enAdownRFE,state='normal')
			tkconfigure(btAdownRFE,state='normal')
		}else{
			tkconfigure(enAdownRFE,state='disabled')
			tkconfigure(btAdownRFE,state='disabled')
		}
	})

	tkbind(cbStnDispo_f,"<Button-1>",function(){
		if(tclvalue(cbStnDispo)=="1"){
			tkconfigure(cb.stnfl,state='disabled')
			tkconfigure(bt.stnfl,state='disabled')
		}else{
			tkconfigure(cb.stnfl,state='normal')
			tkconfigure(bt.stnfl,state='normal')
		}
	})

	#######################
	bt.opt.OK<-tkbutton(frButt, text="OK")
	bt.opt.CA<-tkbutton(frButt, text="Cancel")
	tkgrid(bt.opt.OK,row=0,column=0,sticky='w',padx=5,pady=5,ipadx=10,ipady=1)
	tkgrid(bt.opt.CA,row=0,column=1,sticky='e',padx=5,pady=5,ipadx=1,ipady=1)

	tkconfigure(bt.opt.OK,command=function(){
		if(tclvalue(cbAdownRFE)=="1" & (tclvalue(fileRFE)=="" | tclvalue(fileRFE)=="NA")){
			tkmessageBox(message="Fournir le fichier RFE déja téléchargé",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(cbStnDispo)=="1" & tclvalue(file.stnfl)==""){
			tkmessageBox(message="Fournir le fichier contenant les données de stations",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Préciser le répertoire pour sauvegarder les résultats",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$file.io$Values<<-c(tclvalue(file.stnfl),tclvalue(fileRFE),tclvalue(file.save1))
			GeneralParameters$rfeDownSep<<-tclvalue(cbAdownRFE)
			GeneralParameters$stnDataDispo<<-tclvalue(cbStnDispo)
			GeneralParameters$dates.mrg$Values<<-c(tclvalue(date.yrs),tclvalue(date.mon), tclvalue(date.dek))
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
	tkwm.title(tt,'Mali - Mise à jour décadaire Pluie')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
		#return(NULL)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}

