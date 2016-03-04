
#####**************************** LEFT ************************######
###Widgets left panel

frame.opfiles<-ttklabelframe(panel.left,text="Open Files",relief='groove',width=wpanel.left)
tkgrid(frame.opfiles,sticky='nwe')
#tkgrid.columnconfigure(frame.opfiles,0,weight=1)

scr.opfiles <- tkscrollbar(frame.opfiles, repeatinterval=5, command=function(...)tkyview(all.opfiles,...))
all.opfiles<-tklistbox(frame.opfiles,selectmode="single",selectbackground="yellow",selectforeground="blue",
yscrollcommand=function(...)tkset(scr.opfiles,...),background="white")
tkgrid(all.opfiles,scr.opfiles)
tkgrid.configure(all.opfiles,sticky="n")
tkgrid.configure(scr.opfiles,rowspan=4,sticky="ns")
tkconfigure(all.opfiles,height=5,width=w.opfiles)

#tkgrid.columnconfigure(all.opfiles,0,weight=1)


####Choix station
frame.optexe<-tkframe(panel.left,relief='groove',bd=2)
tkgrid(frame.optexe,sticky='nwe')

frame.choix.stn<-ttklabelframe(frame.optexe,text="Choose station",relief='groove')
setting.frame<-tkframe(frame.optexe)
tkgrid(frame.choix.stn,setting.frame)

stn.choix<-c('')
stn.choix.val <- tclVar()
tclvalue(stn.choix.val) <- stn.choix[1]
stn.choix.cb<-ttkcombobox(frame.choix.stn, values=stn.choix, textvariable=stn.choix.val,state='normal',
width=wframe.choix.stn)
tkgrid(stn.choix.cb,padx=1,pady=5)
frame.choix.prevnext<-tkframe(frame.choix.stn)
tkgrid(frame.choix.prevnext)
stn.choix.prev<-tkbutton(frame.choix.prevnext, text="<<-Prev",state='disabled')
stn.choix.next<-tkbutton(frame.choix.prevnext, text="Next->>",state='disabled')
#tkgrid(stn.choix.prev,stn.choix.next)
tkgrid(stn.choix.prev,row=0,column=0,padx=5)
tkgrid(stn.choix.next,row=0,column=2,padx=5)


button_next<-function(){
	istn<-as.numeric(tclvalue(tcl(stn.choix.cb,"current")))+1
	istn<-istn+1
	if(istn>length(stn.choix)) istn<-1
	tclvalue(stn.choix.val)<-stn.choix[istn]
}

button_prev<-function(){
	istn<-as.numeric(tclvalue(tcl(stn.choix.cb,"current")))+1
	istn<-istn-1
	if(istn<1) istn<-length(stn.choix)
	tclvalue(stn.choix.val)<-stn.choix[istn]
}

tkconfigure(stn.choix.prev,command=button_prev)
tkconfigure(stn.choix.next,command=button_next)

#####################################
setting.button<-tkbutton(setting.frame, text="Options",state='disabled')
tkgrid(setting.button,row=0,column=0)

tkconfigure(setting.button,command=function(){
	if(gal.params$action=='zero.check') gal.params<<-qcGetZeroCheckInfo(main.win,gal.params)
	if(gal.params$action=='qc.rain') gal.params<<-qc.get.info.rain(main.win,gal.params)
	if(gal.params$action=='qc.temp') gal.params<<-qc.get.info.txtn(main.win,gal.params)
	if(gal.params$action=='homog') gal.params<<-homogen.get.info(main.win,gal.params)
#	if(gal.params$action=='rhtests') gal.params<<-rhtests_inputData(main.win,gal.params)
})


###################Output cmd
cmd.frame<-tkframe(panel.left,relief='groove',bd=2)
#tkgrid(cmd.frame,sticky='we',pady=1)
lcmd.frame<-cmd.frame

#ladjset.frame<-NULL
#adjset.frame<-tkframe(panel.left,relief='groove',bd=2)
#tkgrid(adjset.frame,sticky='we',pady=1)
#ladjset.frame<-adjset.frame


#####**************************** RIGHT ************************######
##Onglet right panel
area.frame<-tkframe(panel.right) #,bd=2,relief='sunken'
tkgrid(area.frame)
tkgrid.configure(area.frame,row=0,column=0,sticky='nswe')
tkgrid.columnconfigure(area.frame,0,weight=1)
tkgrid.rowconfigure(area.frame,0,weight=1)

#NOTEBOOKS
tknotes <- ttknotebook(area.frame)
tkgrid(tknotes)
#tkconfigure(tknotes)
tkgrid.configure(tknotes,row=0,column=0,sticky='nswe')
tkgrid.columnconfigure(tknotes,0,weight=1)
tkgrid.rowconfigure(tknotes,0,weight=1)
tkgrid.rowconfigure(tknotes,1,weight=1)
tkgrid.rowconfigure(tknotes,2,weight=1)
tkgrid.rowconfigure(tknotes,3,weight=1)


pressed_index<-tclVar('')
tkbind(tknotes,"<ButtonPress-1>", function(x,y,W) btn_press(x,y,W))
tkbind(tknotes,"<ButtonRelease-1>", function(x,y,W) btn_releases(x,y,W))

