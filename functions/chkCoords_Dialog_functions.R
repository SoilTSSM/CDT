excludeOutStn<-function(parent.win,GeneralParameters){
	listOpenFiles<-openFile_ttkcomboList()
	wtkcombo<-as.integer(as.numeric(w.scale(23)*0.95)/9)
	if(Sys.info()["sysname"] == "Windows") wtkentry<-as.integer(as.numeric(w.scale(25)*0.95)/9)
	else wtkentry<-as.integer(as.numeric(w.scale(24)*0.95)/9)

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
	tkgrid(fr.C,sticky='w',padx=5,pady=5)
	tkgrid(fr.D,padx=5,pady=5)

	###
	fr.A0<-tkframe(fr.A)
	fr.A1<-tkframe(fr.A)
	tkgrid(fr.A0,sticky='w')
	tkgrid(fr.A1,sticky='w')

	frA0.txt<-tklabel(fr.A0,text='Station data file')
	tkgrid(frA0.txt)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	cb.stnfl<-ttkcombobox(fr.A1, values=unlist(listOpenFiles), textvariable=file.stnfl,width=wtkcombo)
	infobulle(cb.stnfl,'Choose the station data in the list')
	status.bar.display(cb.stnfl,TextOutputVar,'Choose the file containing the station data')
	bt.stnfl<-tkbutton.h(fr.A1, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.stnfl,bt.stnfl)
	tkgrid.configure(cb.stnfl,row=0,column=0,sticky='w')
	tkgrid.configure(bt.stnfl,row=0,column=1,sticky='e')
	tkconfigure(bt.stnfl,command=function(){
		dat.opfiles<-getOpenFiles(parent.win,all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'ascii'
			AllOpenFilesData[[nopf+1]]<<-dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.blkshp,values=unlist(listOpenFiles), textvariable=file.blkshp)
		}else{
			return(NULL)
		}
	})

	#####
	fr.B0<-tkframe(fr.B)
	fr.B1<-tkframe(fr.B)
	tkgrid(fr.B0,sticky='w')
	tkgrid(fr.B1,sticky='w')

	frB0.txt<-tklabel(fr.B0,text=" Country Boundaries Shapefiles")
	tkgrid(frB0.txt)

	file.blkshp <- tclVar()
	tclvalue(file.blkshp) <- as.character(GeneralParameters$file.io$Values[2])

	cb.blkshp<-ttkcombobox(fr.B1, values=unlist(listOpenFiles), textvariable=file.blkshp,width=wtkcombo)
	infobulle(cb.blkshp,'Choose the file in the list')
	status.bar.display(cb.blkshp,TextOutputVar,'File containing the ESRI shapefiles')
	bt.blkshp<-tkbutton.h(fr.B1, text="...",TextOutputVar,'Browse file if not listed','Browse file if not listed')
	tkgrid(cb.blkshp,bt.blkshp)
	tkgrid.configure(cb.blkshp,row=0,column=0,sticky='w')
	tkgrid.configure(bt.blkshp,row=0,column=1,sticky='e')
	tkconfigure(bt.blkshp,command=function(){
		shp.opfiles<-getOpenShp(parent.win,all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf<-length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]]<<-'shp'
			AllOpenFilesData[[nopf+1]]<<-shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]]<<-AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.blkshp)<-AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl,values=unlist(listOpenFiles), textvariable=file.stnfl)
			tkconfigure(cb.blkshp,values=unlist(listOpenFiles), textvariable=file.blkshp)
		}else{
			return(NULL)
		}
	})

	####
	fr.C0<-tkframe(fr.C)
	fr.C1<-tkframe(fr.C)
	tkgrid(fr.C0,sticky='w')
	tkgrid(fr.C1,sticky='w')

	frC0.txt<-tklabel(fr.C0,text='Directory to save result')
	tkgrid(frC0.txt)

	file.save1 <-tclVar(as.character(GeneralParameters$file.io$Values[3]))

	en.file.save<-tkentry(fr.C1,textvariable=file.save1,width=wtkentry)
	infobulle(en.file.save,'Enter the full path to directory to save result')
	status.bar.display(en.file.save,TextOutputVar,'Enter the full path to directory to save result')
	bt.file.save<-tkbutton.h(fr.C1, text=" ...",TextOutputVar,'or browse here','or browse here')
	tkgrid(en.file.save,bt.file.save)
	tkgrid.configure(en.file.save,row=0,column=0,sticky='w')
	tkgrid.configure(bt.file.save,row=0,column=1,sticky='e')
	tkconfigure(bt.file.save,command=function(){
		file2save1<-tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1,'does not exist.\n It will be created.',sep=' '),icon="warning",type="ok")
			dir.create(file2save1,recursive=TRUE)
			tclvalue(file.save1)<-file2save1
		}else tclvalue(file.save1)<-file2save1
	})


###
	infobulle(fr.D,'Distance in km, outside the country boudanries to considere')
	status.bar.display(fr.D,TextOutputVar,'Distance in km, outside the country boudanries to considere')

	bufftxt<-tklabel(fr.D,text='Distance from boundaries(km)')
	buffw <-tclVar(as.character(GeneralParameters$buffer))
	en.buffw<-tkentry(fr.D,textvariable=buffw,width=6)
	tkgrid(bufftxt,en.buffw)

###
	bt.opt.OK<-tkbutton(frButt, text="OK")
	bt.opt.CA<-tkbutton(frButt, text="Cancel")
	tkgrid(bt.opt.OK,row=0,column=0,sticky='w',padx=5,pady=5,ipadx=10,ipady=1)
	tkgrid(bt.opt.CA,row=0,column=1,sticky='e',padx=5,pady=5,ipadx=1,ipady=1)

	retval<-NULL
	tkconfigure(bt.opt.OK,command=function(){
		if(tclvalue(file.stnfl)==""){
			tkmessageBox(message="Provide the station data file",icon="warning",type="ok")
		}else if(tclvalue(file.blkshp)==""){
			tkmessageBox(message="Provide the shapefile of country boundaries",icon="warning",type="ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1)=="" | tclvalue(file.save1)=="NA"){
			tkmessageBox(message="Enter the file to save result",icon="warning",type="ok")
			tkwait.window(tt)
		}else{
			idsstn<-which(unlist(lapply(1:length(AllOpenFilesData),function(j) AllOpenFilesData[[j]][[1]]))==tclvalue(file.stnfl))
			if(length(idsstn)==0){
				tkmessageBox(message="File not found or in the wrong format",icon="warning",type="ok")
				tkwait.window(tt)
			}else{
				GeneralParameters$file.io$Values<<-c(tclvalue(file.stnfl),tclvalue(file.blkshp),tclvalue(file.save1))
				GeneralParameters$buffer<<-tclvalue(buffw)

				##set choix stn
				donstn<-AllOpenFilesData[[idsstn]][[2]]
				stn.choix<<-as.character(donstn[1,-1])

				tkconfigure(stn.choix.cb,state='disabled')
				tkconfigure(setting.button,state='disabled')
				tkconfigure(stn.choix.prev,state='disabled')
				tkconfigure(stn.choix.next,state='disabled')
				tkconfigure(spinH,state='normal')
				tkconfigure(spinV,state='normal')

				####button command
				if(is.null(lcmd.frame_chk)){
					lcmd.frame<<-ChkCoordCmdBut()
					lcmd.frame_chk<<-1
				}

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			}
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
	tkwm.title(tt,'Exclude Outside Stations')
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

