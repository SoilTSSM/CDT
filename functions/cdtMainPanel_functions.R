
#####**************************** LEFT ************************######
###Widgets left panel

frame.opfiles<-ttklabelframe(panel.left,text="Open Files",relief='groove',width=wpanel.left)
choixStn.frame<-ttklabelframe(panel.left,text="Choose station",relief='groove')
cmd.frame<-tkframe(panel.left,relief='groove',bd=2)
lcmd.frame<-cmd.frame

##### List open files
scr.opfiles <- tkscrollbar(frame.opfiles, repeatinterval=5, command=function(...)tkyview(all.opfiles,...))
all.opfiles<-tklistbox(frame.opfiles,selectmode="single",height=5,width=w.opfiles,
						 selectbackground="yellow",selectforeground="blue",background="white",
						 yscrollcommand=function(...)tkset(scr.opfiles,...))
###
tkgrid(all.opfiles,row=0,column=0,sticky="n")
tkgrid(scr.opfiles,row=0,column=1,rowspan=4,sticky="ns")

#### Choix station
stn.choix<-c('')
stn.choix.val <- tclVar()
tclvalue(stn.choix.val) <- stn.choix[1]

stn.choix.cb<-ttkcombobox(choixStn.frame, values=stn.choix, textvariable=stn.choix.val,state='normal', width=wframe.choix.stn)
stn.choix.prev<-tkbutton(choixStn.frame, text="<<-Prev",state='disabled')
stn.choix.next<-tkbutton(choixStn.frame, text="Next->>",state='disabled')
setting.button<-tkbutton(choixStn.frame, text="Options",state='disabled')

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
tkconfigure(setting.button,command=function(){
	if(GeneralParameters$action=='zero.check') GeneralParameters<<-qcGetZeroCheckInfo(main.win,GeneralParameters)
	if(GeneralParameters$action=='qc.rain') GeneralParameters<<-qc.get.info.rain(main.win,GeneralParameters)
	if(GeneralParameters$action=='qc.temp') GeneralParameters<<-qc.get.info.txtn(main.win,GeneralParameters)
	if(GeneralParameters$action=='homog') GeneralParameters<<-homogen.get.info(main.win,GeneralParameters)
#	if(GeneralParameters$action=='rhtests') GeneralParameters<<-rhtests_inputData(main.win,GeneralParameters)
})

###
tkgrid(stn.choix.cb,row=0,column=0,sticky='we',rowspan=1,columnspan=3,padx=1,pady=1,ipadx=1,ipady=1)
tkgrid(stn.choix.prev,row=1,column=0,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
tkgrid(stn.choix.next,row=1,column=2,sticky='we',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
tkgrid(setting.button,row=0,column=3,sticky='we',rowspan=2,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

#######
tkgrid(frame.opfiles,sticky='nwe')
tkgrid(choixStn.frame,sticky='nwe',pady=5)

#####**************************** RIGHT ************************######
##Onglet right panel

area.frame<-tkframe(panel.right)
tknotes <- ttknotebook(area.frame)

tkgrid(tknotes,row=0,column=0,sticky='nswe')
tkgrid(area.frame,row=0,column=0,sticky='nswe')

###
tkgrid.columnconfigure(tknotes,0,weight=1)
for(i in 0:3) tkgrid.rowconfigure(tknotes,i,weight=1)

tkgrid.columnconfigure(area.frame,0,weight=1)
tkgrid.rowconfigure(area.frame,0,weight=1)

#####
pressed_index<-tclVar('')
tkbind(tknotes,"<ButtonPress-1>", function(x,y,W) btn_press(x,y,W))
tkbind(tknotes,"<ButtonRelease-1>", function(x,y,W) btn_releases(x,y,W))

