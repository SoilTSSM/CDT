
if(Sys.info()["sysname"] == "Windows") {
	horiz<-w.scale(70)/385
	verti<-h.scale(60)/385
}else{
	horiz<-w.scale(70)/480
	verti<-h.scale(60)/480
}
horizS<-round(horiz,1)
vertiS<-round(verti,1)

hRedraw <- tkimage.create('photo','-file',file.path(imgdir,'RedrawButton24.gif',fsep = .Platform$file.sep))
hRedraw1 <- tkimage.create('photo','-file',file.path(imgdir,'RedrawButton-Change24.gif',fsep = .Platform$file.sep))


####################################################################################################

tb.open.file<-tkbutton.toolbar(tools.frame,imgdir,"open24.gif",txt.stbr1,"Open file","Open file format: txt,csv,...")
#tb.save.file<-tkbutton.toolbar(tools.frame,imgdir,"save24.gif",txt.stbr1,"Save file","Save file")
tb.save.image<-tkbutton.toolbar(tools.frame,imgdir,"save_img24.gif",txt.stbr1,"Save image","Save image")
tb.exit.win<-tkbutton.toolbar(tools.frame,imgdir,"exit24.gif",txt.stbr1,"Exit window","Exit the main window")
tb.run<-tkbutton.toolbar(tools.frame,imgdir,"run24.gif",txt.stbr1,"Execute","Execute the append task")
tb.close.tab<-tkbutton.toolbar(tools.frame,imgdir,"close_tab24.gif",txt.stbr1,"Close active tab","Close active tab")

tb.separator<-ttkseparator(tools.frame,orient='vertical')
tb.separator1<-ttkseparator(tools.frame,orient='vertical')
tb.separator2<-ttkseparator(tools.frame,orient='vertical')
tb.separator3<-ttkseparator(tools.frame,orient='vertical')
tb.separator4<-ttkseparator(tools.frame,orient='vertical')
tb.separator5<-ttkseparator(tools.frame,orient='vertical')
tb.separator6<-ttkseparator(tools.frame,orient='vertical')
#tb.separator7<-ttkseparator(tools.frame,orient='vertical')
#tb.separator8<-ttkseparator(tools.frame,orient='vertical')

tb.open.table<-tkbutton.toolbar(tools.frame,imgdir,"open_table24.gif",txt.stbr1,"Open table","Open table")
tb.save.table<-tkbutton.toolbar(tools.frame,imgdir,"save_table24.gif",txt.stbr1,"Save table","Save table")
#tb.edit.table<-tkbutton.toolbar(tools.frame,imgdir,"edit_table24.gif",txt.stbr1,"Edit table","Edit table")
#tb.close.table<-tkbutton.toolbar(tools.frame,imgdir,"close_table24.gif",txt.stbr1,"Close table","Close table")

#tb.plot.point<-tkbutton.toolbar(tools.frame,imgdir,"pointplot24.gif",txt.stbr1,"Graduate color points","Graduate color points")
#tb.plot.raster<-tkbutton.toolbar(tools.frame,imgdir,"rasterplot24.gif",txt.stbr1,"Raster plot","Raster plot")

lspinH<-tklabel.h(tools.frame,'Width:',txt.stbr1,'Horizontal scale factor for image size','Horizontal scale factor for image size')
spinH<-ttkspinbox(tools.frame,from=0.5,to=5.0,increment=0.1,justify='center',width=6,state='disabled')
tkset(spinH,horizS)
infobulle(spinH,'Horizontal scale factor for image size')
status.bar.display(spinH,txt.stbr1,'Horizontal scale factor for image size')

lspinV<-tklabel.h(tools.frame,'Height:',txt.stbr1,'Vertical scale factor for image size','Vertical scale factor for image size')
spinV<-ttkspinbox(tools.frame,from=0.5,to=5.0,increment=0.1,justify='center',width=6,state='disabled')
tkset(spinV,vertiS)
infobulle(spinV,'Vertical scale factor for image size')
status.bar.display(spinV,txt.stbr1,'Vertical scale factor for image size')

plotRedraw<-tkbutton.toolbar(tools.frame,imgdir,"RedrawButton24.gif",txt.stbr1,"Redraw plot","Redraw plot")

########
#tkgrid(tb.open.file,tb.open.table,tb.separator,tb.save.image,tb.save.table,
#tb.separator1,tb.run,tb.separator2,tb.exit.win,tb.separator3,tb.close.tab,tb.separator4,
#lspinH,spinH,tb.separator5,lspinV,spinV,tb.separator6,plotRedraw,tb.separator7,
#tb.plot.point,tb.plot.raster,tb.separator8)

#######
tkgrid(tb.open.file,tb.open.table,tb.separator,tb.save.image,tb.save.table,
tb.separator1,tb.run,tb.separator2,tb.exit.win,tb.separator3,tb.close.tab,tb.separator4,
lspinH,spinH,tb.separator5,lspinV,spinV,tb.separator6,plotRedraw)

#######
tkgrid.configure(tb.separator,sticky='ns')
tkgrid.configure(tb.separator1,sticky='ns')
tkgrid.configure(tb.separator2,sticky='ns',padx=10)
tkgrid.configure(tb.separator3,sticky='ns',padx=10)
tkgrid.configure(tb.separator4,sticky='ns',padx=10)
tkgrid.configure(tb.separator5,sticky='ns',padx=10)
tkgrid.configure(tb.separator6,sticky='ns',padx=10)
#tkgrid.configure(tb.separator7,sticky='ns',padx=10)
#tkgrid.configure(tb.separator8,sticky='ns',padx=10)

tkgrid.configure(tb.open.table,padx=5)
tkgrid.configure(tb.save.table,padx=5)
#tkgrid.configure(tb.edit.table,padx=2)
#tkgrid.configure(tb.close.table,padx=2)

tkgrid.configure(tb.open.file,padx=5)
#tkgrid.configure(tb.save.file,padx=2)
tkgrid.configure(tb.save.image,padx=5)

tkgrid.configure(tb.exit.win,padx=5,sticky='w')
tkgrid.configure(tb.run,padx=5)
tkgrid.configure(tb.close.tab,padx=5)

#####**************************** Change plot window scale ************************######

tkconfigure(plotRedraw,relief='raised',command=function(){
	tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))+1
	if(length(tab.type)>0){
		if(tab.type[[tabid]]=="img"){

			if(class(tab.data[[tabid]][[2]])=="tkwin"){
				#W<-tkwinfo('children',tab.data[[tabid]][[1]][[2]])
				W<-tab.data[[tabid]][[2]]
				img<-tab.data[[tabid]][[2]]
				refreshPlot1(W=W,img=img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
			}
			if(class(tab.data[[tabid]][[2]])=="list"){
				W<-tab.data[[tabid]][[2]][[1]]
				img<-tab.data[[tabid]][[2]][[2]]
				refreshPlot1(W=W,img=img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
				if(tclvalue(tkwinfo('class',tkwinfo('children',tab.data[[tabid]][[1]][[2]])))=="Frame"){
					# w<-as.double(tkwinfo("width", tab.data[[tabid]][[1]][[1]]))
					# h<-as.double(tkwinfo("height", tab.data[[tabid]][[1]][[1]]))
					w<-as.double(tkwinfo("width", panel.right))
					h<-as.double(tkwinfo("height", panel.right))
					setScrollCanvas(W,w,h)
					#setScrollCanvas1(W)
				}
			}
			tkconfigure(plotRedraw,image=hRedraw)
		}
	}
})

#######
tkbind(plotRedraw,"<ButtonRelease>",function(){
	tkconfigure(plotRedraw,image=hRedraw)
})

tkbind(spinH,"<<Increment>>",function(){
	tkconfigure(plotRedraw,image=hRedraw1)
})
tkbind(spinH,"<<Decrement>>",function(){
	tkconfigure(plotRedraw,image=hRedraw1)
})
tkbind(spinV,"<<Increment>>",function(){
	tkconfigure(plotRedraw,image=hRedraw1)
})
tkbind(spinV,"<<Decrement>>",function(){
	tkconfigure(plotRedraw,image=hRedraw1)
})

#####**************************** Plot ************************######

#tkconfigure(tb.plot.point,relief='raised',state='disabled',command=function(){
#	if(is.null(lcmd.frame_plot1)){
#		source(file.path(apps.dir,'functions','initialize',fsep = .Platform$file.sep))
#		source(file.path(apps.dir,'functions','initialize_stn_button',fsep = .Platform$file.sep))
#		lcmd.frame<<-addLatticePlot()
#		lcmd.frame_plot1<<-1
#	}
#})

########
#tkconfigure(tb.plot.raster,relief='raised',state='disabled')

#####**************************** Configure command toolbars ************************######

tkconfigure(tb.open.file,state='normal',command=function(){
	tkconfigure(main.win,cursor='watch');tcl('update')
	dat.opfiles<-getOpenFiles(main.win,all.opfiles)
	tkconfigure(main.win,cursor='')
	if(!is.null(dat.opfiles)){
		nopf<-length(type.opfiles)
		type.opfiles[[nopf+1]]<<-'ascii'
		file.opfiles[[nopf+1]]<<-dat.opfiles
	}else{
		return(NULL)
	}
})

#######
tkconfigure(tb.save.image,state='normal',command=function(){
	SavePlot()
})

#######
tkconfigure(tb.open.table,state='normal',command=function() {
	tab.array<-displayArrayTab(main.win,tknotes)
	if(!is.null(tab.array)){
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'arr'
		tab.data[[ntab+1]]<<-tab.array
		tkselect(tknotes,ntab)
		#getTableInChange(tknotes)
	}else{
		return(NULL)
	}
})

#######
tkconfigure(tb.save.table,state='normal',command=function(){
	if(!is.null(ret.results)){
		tkconfigure(main.win,cursor='watch');tcl('update')
		tab2sav<-try(SaveNotebookTabArray(tknotes), silent=TRUE)
		tkconfigure(main.win,cursor='')
		is.ok<- !inherits(tab2sav, "try-error")
		if(is.ok){
			insert.txt(main.txt.out,"Table saved successfully")
		}else{
			insert.txt(main.txt.out,"The table could not be saved",format=TRUE)
			insert.txt(main.txt.out,gsub('[\r\n]','',tab2sav[1]),format=TRUE)
			return(NULL)
		}
	}else{
		return(NULL)
	}
})

#######
#tkconfigure(tb.save.file,state='disabled',command=function() return(NULL))
#tkconfigure(tb.edit.table,state='disabled',command=function() return(NULL))
#tkconfigure(tb.close.table,state='disabled',command=function() return(NULL))

#####**************************** Run Task ************************######
tkconfigure(tb.run,state='normal',command=function(){
	if(is.null(gal.params)){
		return(NULL)
	}else{
		tkconfigure(main.win,cursor='watch');tcl('update')
		ret.results<<-tryCatch(execute.fun(tclvalue(stn.choix.val)),
			#warning=function(w) warningFun(w),
			error=function(e) errorFun(e),finally={
			tkconfigure(main.win,cursor='')
		})
	}
})

#####**************************** Close CDT ************************######
##??? demande de sauver s'il y a encore des onglets ouverts???
tkconfigure(tb.exit.win,state='normal',command=function(){
	on.exit({
		#sink(type="message")
		#close(msgOUT)
		options(warn=0)
	})
	tkdestroy(main.win)
})

#####**************************** Close Notebook Tab ************************######
tkconfigure(tb.close.tab,state='normal',command=function(){
	tabid<-as.numeric(tclvalue(tkindex(tknotes,'current')))
	CloseNotebookTab(tabid)
})

