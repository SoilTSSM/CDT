################## error and warning handling

warningFun<-function(w){
	txt<-as.character(w)
	retW<-gsub('[\r\n]','',txt)
	insert.txt(main.txt.out,retW,format=TRUE)
}

errorFun<-function(e){
	txt<-as.character(e)
	retE<-gsub('[\r\n]','',txt)
	insert.txt(main.txt.out,retE,format=TRUE)
}

#####**************************** Initialize variables ************************######

source(file.path(apps.dir,'functions','initialize0',fsep = .Platform$file.sep))

#####**************************** General&Generic functions ************************######

source(file.path(apps.dir,'functions','cdtGeneral_functions.R',fsep = .Platform$file.sep))

#####**************************** MAIN WINDOW ************************######

main.win<-tktoplevel()
tkwm.resizable(main.win, TRUE, TRUE)
tkgrid.columnconfigure(main.win,0,weight=1)  
#for(i in 1:2) tkgrid.rowconfigure(main.win,i,weight=1) 
tkgrid.rowconfigure(main.win,1,weight=1)

##Window geometry
width.scr<-as.integer(tkwinfo("screenwidth",main.win))
height.scr<-as.integer(tkwinfo("screenheight",main.win))

##function to scale dialog in %
w.scale<-function(per) as.integer(per*width.scr/100)
h.scale<-function(per) as.integer(per*height.scr/100)

#######################################################################
###dim
##font width
sfont0<-as.numeric(tclvalue(tkfont.measure(main.win,paste("0123456789",paste(letters[1:26],LETTERS[1:26],collapse='',sep=''),sep=''))))/(10+2*26)
##left panel width
#wpanel.left<-w.scale(25)
wpanel.left<-w.scale(30)
hpanel.left<-h.scale(70)
##List open files width
w.opfiles<-as.integer(w.scale(29)/sfont0)
##stations choice width
wframe.choix.stn<-as.integer(w.scale(21.5)/sfont0)
##Status bar width (from left)
wbstatus1<-as.integer(w.scale(50)/sfont0) 
wbstatus2a<-as.integer(w.scale(15)/sfont0)
wbstatus2b<-as.integer(w.scale(15)/sfont0)
wbstatus3<-as.integer(w.scale(15)/sfont0)
##Output message, tktext height
txtHeight<-7
if(Sys.info()["sysname"] == "Windows") txtHeight<-6



#####**************************** TOP MENU ************************######

top.menu <- tkmenu(main.win, tearoff=FALSE)
tkconfigure(main.win, menu=top.menu)
source(file.path(apps.dir,'functions','cdtMenu_functions.R',fsep = .Platform$file.sep))

#####**************************** TOOLBARS ************************######

tools.frame<-tkframe(main.win,bd=2,relief='ridge')
source(file.path(apps.dir,'functions','cdtToolbar_functions.R',fsep = .Platform$file.sep))

#####**************************** MAIN FRAME ************************######

main.frame0<-tkframe(main.win,bd=2,relief='ridge')
#panedwindow
main.pane0<-ttkpanedwindow (main.frame0,orient='vertical')

#####**************************** MAIN PANEL ************************######

main.frame<-ttkpanedwindow (main.pane0,orient='horizontal',height=hpanel.left)
#main.frame<-tkframe(main.win,bd=2,relief='ridge')
#left panel
panel.left<-tkframe(main.frame,relief='raised',bd=2,width=wpanel.left)  
#right panel
panel.right<-tkframe(main.frame)

source(file.path(apps.dir,'functions','cdtMainPanel_functions.R',fsep = .Platform$file.sep))

#####**************************** TEXT OUTPUT ************************######

out.frame<-tkframe(main.pane0,bd=2,relief='groove')
#out.frame<-tkframe(main.win,bd=2,relief='ridge')
source(file.path(apps.dir,'functions','cdtOutputmsg_functions.R',fsep = .Platform$file.sep))

#####**************************** STATUS BAR ************************######

frstatusbar<-tkframe(main.win)
source(file.path(apps.dir,'functions','cdtStatusbar_functions.R',fsep = .Platform$file.sep))

#####**************************** Manage grid ************************######

tkadd(main.frame,panel.left)
tkadd(main.frame,panel.right)
#left panel
#tkgrid(panel.left,row=0,column=0,rowspan=1,columnspan=1,sticky="snew")
#tkgrid.rowconfigure(panel.left,0,weight=1)
tkgrid.columnconfigure(panel.left,0,weight=1)

#right panel
#tkgrid(panel.right,row=0,column=1,rowspan=1,columnspan=1,sticky="snew")
tkgrid.columnconfigure(panel.right,0,weight=1)
tkgrid.rowconfigure(panel.right,0,weight=1)
# tkgrid.rowconfigure(panel.right,1,weight=1)
# tkgrid.rowconfigure(panel.right,2,weight=1)

#####
tkgrid(tools.frame,row=0,column=0,rowspan=1,columnspan=2,sticky="new",padx=5)

###
tkadd(main.pane0,main.frame)
tkadd(main.pane0,out.frame)

#tkgrid(main.frame,row=1,column=0,rowspan=1,columnspan=2,sticky="snew",padx=5) 
tkgrid.columnconfigure(main.frame,0,weight=1)  #
tkgrid.columnconfigure(main.frame,1,weight=1)
tkgrid.rowconfigure(main.frame,0,weight=1)
#tkgrid(out.frame,row=2,column=0,rowspan=1,columnspan=2,sticky='swe',padx=5)
tkgrid.columnconfigure(out.frame,0,weight=1)
tkgrid.rowconfigure(out.frame,0,weight=1)

## panned frame
tkgrid(main.pane0,row=0,column=0,rowspan=1,columnspan=2,sticky="snew",padx=1) 
tkgrid.rowconfigure(main.pane0,0,weight=1)
tkgrid.rowconfigure(main.pane0,1,weight=1)
tkgrid.columnconfigure(main.pane0,0,weight=1)

##main frame
tkgrid(main.frame0,row=1,column=0,rowspan=1,columnspan=2,sticky="snew",padx=5) 
tkgrid.rowconfigure(main.frame0,0,weight=1)
tkgrid.columnconfigure(main.frame0,0,weight=1)

##statusbar
tkgrid(frstatusbar,row=2,column=0,rowspan=1,columnspan=1,sticky="snew") #,padx=5
tkgrid.columnconfigure(frstatusbar,0,weight=1)

grip.right <- ttksizegrip(main.win)
tkgrid(grip.right,row=2,column=1,sticky="se")

#####**************************** Manage geometry ************************######

tkwm.withdraw(main.win)
tcl('update')
tkwm.geometry(main.win, paste(width.scr,'x',height.scr,'+',0,'+',0,sep=''))
#posxMain<-as.integer(width.scr*0.5-as.integer(tkwinfo("width",main.win))*0.5)
#posyMain<-as.integer(height.scr*0.5-as.integer(tkwinfo("height",main.win))*0.5)
#tkwm.geometry(main.win, paste(width.scr,'x',height.scr,'+',posxMain,'+',posyMain,sep=''))

tkwm.transient(main.win)
tkwm.title(main.win, paste("Climate Data Tools, v",cdtVersion,sep=''))
tkwm.deiconify(main.win)
#tkwm.iconify(main.win)


#####**************************** fullscreen option ************************######

if(Sys.info()["sysname"] == "Linux") {
	tcl('wm','attributes',main.win,fullscreen=FALSE,zoomed=TRUE) #on linux
}else if(Sys.info()["sysname"] == "Darwin"){
	tcl('wm','attributes',main.win,fullscreen=FALSE,zoomed=TRUE)
}else if(Sys.info()["sysname"] == "Windows"){
	tcl('wm','attributes',main.win,fullscreen=FALSE)
	tcl('wm','state',main.win,'zoomed') ##on Win 
}

#####**************************** Close CDT ************************######

tcl("wm", "protocol", main.win, "WM_DELETE_WINDOW", function() {
	on.exit({
		#sink(type="message")
		#close(msgOUT)
		options(warn=0)
	})
	tkdestroy(main.win)
})

#####**************************** LOAD Functions ************************######

#source(file.path(apps.dir,'functions','loadAll_functions.R',fsep = .Platform$file.sep))
tryCatch(source(file.path(apps.dir,'functions','loadAll_functions.R',fsep = .Platform$file.sep)),
		warning=function(w) warningFun(w),
		error=function(e) errorFun(e))

