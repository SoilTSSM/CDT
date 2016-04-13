configCDT<-function(){
	config.win<-tktoplevel()
	width.conf<-as.integer(tkwinfo("screenwidth",config.win))
	height.conf<-as.integer(tkwinfo("screenheight",config.win))

	tkgrab.set(config.win)
	tkfocus(config.win)

	if(Sys.info()["sysname"] == "Windows") width.entry<-61	
	else width.entry<-43	

	font0.conf <- tkfont.create(family="courier",size=11)

	fr1.conf<-tkframe(config.win,relief='sunken',borderwidth=2)
	fr2.conf<-tkframe(config.win,relief='sunken',borderwidth=2)
	fr3.conf<-tkframe(config.win)
	fr4.conf<-tkframe(config.win)
	tkgrid(fr1.conf,sticky='ew',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr2.conf,sticky='ew',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr3.conf)
	tkgrid(fr4.conf)

	fr1a.conf<-tkframe(fr1.conf)
	fr1b.conf<-tkframe(fr1.conf)
	tkgrid(fr1a.conf,row=0,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(fr1b.conf,row=1,column=0,sticky='we',padx=1,pady=1,ipadx=1,ipady=1)

	wdtxt<-tklabel(fr1a.conf,text='Set working directory')
	tkgrid(wdtxt)

	dirpath <-tclVar(getwd())
	en.dirpath<-tkentry(fr1b.conf,textvariable=dirpath,width=width.entry) #try dynamic width
	bt.dirpath<-tkbutton(fr1b.conf, text="...")
	tkgrid(en.dirpath,bt.dirpath) 
	tkgrid.configure(en.dirpath,row=0,column=0,sticky='w')
	tkgrid.configure(bt.dirpath,row=0,column=1,sticky='e')
	tkconfigure(bt.dirpath,command=function(){
		dirpath1<-tk_choose.dir(getwd(), "")
		tclvalue(dirpath)<-dirpath1
	})	

	########
	for(i in 0:3) assign(paste('fr2',i,'.conf',sep=''),tkframe(fr2.conf))
	for(i in 0:3) tkgrid(get(paste('fr2',i,'.conf',sep='')))
	for(i in 0:3) tkgrid.configure(get(paste('fr2',i,'.conf',sep='')),row=i,column=0,sticky='we',padx=1,pady=0,ipadx=1,ipady=0)


	confpath<-file.path(apps.dir,'configure','configure0',fsep = .Platform$file.sep)
	if(file.exists(confpath)){
		conffile<-as.character(read.table(confpath,colClasses='character')[,1])
		tktablepath<-tclVar(conffile[2])
		bwidgetpath<-tclVar(conffile[3])
	}else{
		#if (.Platform$OS.type=='unix')
		if(Sys.info()["sysname"] == "Windows") {
			if(.Machine$sizeof.pointer==8){
					tktablepath<-tclVar("C:/Tcl/lib/teapot/package/win32-x86_64/lib/Tktable2.11")
					bwidgetpath<-tclVar("C:/Tcl/lib/teapot/package/tcl/lib/BWidget1.9.8")
			}else{
				tktablepath<-tclVar("C:/Tcl/lib/teapot/package/win32-ix86/lib/Tktable2.11")
				bwidgetpath<-tclVar("C:/Tcl/lib/teapot/package/tcl/lib/BWidget1.9.6")		
			}			
		}else if(Sys.info()["sysname"] == "Darwin"){
			tktablepath<-tclVar("/usr/local/lib/tcl8.6/Tktable2.10")
			bwidgetpath<-tclVar("/usr/local/lib/tcl8.6/bwidget-1.9.6")		
		}else if(Sys.info()["sysname"] == "Linux"){
			tktablepath<-tclVar("/usr/local/lib/teapot/package/linux-glibc2.3-x86_64/lib/Tktable2.11")
			bwidgetpath<-tclVar("/usr/local/lib/teapot/package/tcl/lib/BWidget1.9.7")	
		}
	}

	tktabletxt<-tklabel(fr20.conf,text='Tktable library path')
	tkgrid(tktabletxt)

	en.tktable<-tkentry(fr21.conf,textvariable=tktablepath,width=width.entry) #try dynamic width
	bt.tktable<-tkbutton(fr21.conf, text="...")
	tkgrid(en.tktable,bt.tktable) 
	tkgrid.configure(en.tktable,row=0,column=0,sticky='w')
	tkgrid.configure(bt.tktable,row=0,column=1,sticky='e')
	tkconfigure(bt.tktable,command=function(){
		tktablepath1<-tk_choose.dir(default = "", caption = "")
		tclvalue(tktablepath)<-tktablepath1
	})		

	bwidgettxt<-tklabel(fr22.conf,text='BWidget library path')
	tkgrid(bwidgettxt)

	en.bwidget<-tkentry(fr23.conf,textvariable=bwidgetpath,width=width.entry) #try dynamic width
	bt.bwidget<-tkbutton(fr23.conf, text="...")
	tkgrid(en.bwidget,bt.bwidget) 
	tkgrid.configure(en.bwidget,row=0,column=0,sticky='w')
	tkgrid.configure(bt.bwidget,row=0,column=1,sticky='e')
	tkconfigure(bt.bwidget,command=function(){
		bwidgetpath1<-tk_choose.dir(default = "", caption = "")
		tclvalue(bwidgetpath)<-bwidgetpath1
	})		

	####
	fr3a.conf<-tkframe(fr3.conf,relief='groove',borderwidth=2)
	tkgrid(fr3a.conf,sticky='ew')
	xscr.conf<- tkscrollbar(fr3a.conf, repeatinterval=5,orient="horizontal",command=function(...)tkxview(txta.conf,...))
	yscr.conf<- tkscrollbar(fr3a.conf, repeatinterval=5,command=function(...)tkyview(txta.conf,...))
	txta.conf<- tktext(fr3a.conf,bg="white",font="courier",xscrollcommand=function(...)tkset(xscr.conf,...),
	yscrollcommand=function(...)tkset(yscr.conf,...), wrap="word",height=5,width=37)
		 
	tkgrid(txta.conf,yscr.conf)
	tkgrid.configure(txta.conf,sticky="nsew")
	tkgrid.configure(yscr.conf,sticky="ns")
	tktag.configure(txta.conf, "font0f.conf", font=font0.conf)
	infofl.conf<-file.path(apps.dir,'text','pref_info.txt',fsep = .Platform$file.sep)
	rdL.conf<-readLines(infofl.conf,warn=FALSE)

	tcl("update","idletasks")
	for(i in 1:length(rdL.conf))	tkinsert(txta.conf,"end",paste(rdL.conf[i],"\n"),"font0f.conf")
	
	bt.close.conf<-tkbutton(fr4.conf, text=" OK ") 
	tkgrid(bt.close.conf,ipadx=10)
	tkconfigure(bt.close.conf,command=function(){
		workdir<-tclvalue(dirpath)
		if(!file.exists(workdir)) dir.create(workdir,recursive=TRUE,showWarnings=FALSE)
		setwd(workdir)
		addTclPath(path = tclvalue(tktablepath))
		addTclPath(path = tclvalue(bwidgetpath))			
		is.notkt<-tclRequire("Tktable")
		is.nobw<-tclRequire("BWidget") 
		if(is.logical(is.notkt)){
			tkmessageBox(message="Tcl package 'Tktable' not found",icon="error",type="ok")
			tkwait.window(config.win)
		}
		if(is.logical(is.nobw)){
			tkmessageBox(message="Tcl package 'BWidget' not found",icon="error",type="ok")
			tkwait.window(config.win)
		}			

		params<-c(tclvalue(dirpath),tclvalue(tktablepath),tclvalue(bwidgetpath))
		write.table(params,file.path(apps.dir,'configure','configure0',fsep = .Platform$file.sep),col.names=F,row.names=F)
		tkgrab.release(config.win)
		tkdestroy(config.win)
	})	
	
	tkwm.withdraw(config.win)
	tcl('update')
	tt.w<-as.integer(tkwinfo("reqwidth",config.win))
	tt.h<-as.integer(tkwinfo("reqheight",config.win))
	tt.x<-as.integer(width.conf*0.5-tt.w*0.5)
	tt.y<-as.integer(height.conf*0.5-tt.h*0.5)
	tkwm.geometry(config.win, paste('+',tt.x,'+',tt.y,sep=''))
	tkwm.transient(config.win)
	tkwm.title(config.win,'CDT configuration')
	tkwm.deiconify(config.win)

	tkfocus(config.win)
	tkbind(config.win, "<Destroy>", function() {tkgrab.release(config.win)})
	tkwait.window(config.win)
}
