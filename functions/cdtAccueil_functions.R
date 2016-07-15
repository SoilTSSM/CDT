
#tcl('update')
######################################################################################

wcdtAccInfo<-as.integer(w.scale(70)/10)

w.main.frame<-as.integer(tkwinfo("height",main.frame))

frcdtAcc1 <- tkframe(tknotes,height=as.integer(w.main.frame*0.38))
frcdtAcc2 <- tkframe(tknotes,height=as.integer(w.main.frame*0.38))
frcdtAcc3 <- tkframe(tknotes,height=as.integer(w.main.frame*0.18))

cdtfont1 <- tkfont.create(family="times",size=30,weight="bold")
cdtfont2 <- tkfont.create(family="times",size=18,weight="bold")
cdtfont3 <- tkfont.create(family="times",size=12)
cdtfont4 <- tkfont.create(family="courier",size=12)

cdtname<-tklabel(frcdtAcc1,text='Climate Data Tools',font=cdtfont1,foreground='blue4')
cdtver<-tklabel(frcdtAcc1,text=paste('Version', cdtVersion),font=cdtfont2)
tkgrid(cdtname,sticky='nsew')
tkgrid(cdtver,sticky='nsew')

cdtauth1<-tklabel(frcdtAcc1,text='Rija Faniriantsoa',font=cdtfont3)
cdtauth2<-tklabel(frcdtAcc1,text='Tufa Dinku',font=cdtfont3)
tkgrid(cdtauth1,sticky='nsew')
tkgrid(cdtauth2,sticky='nsew')

cdtAccInfo<-tkframe(frcdtAcc2,relief='groove',borderwidth=2)
tkgrid(cdtAccInfo,sticky='ew')
yscr.cdtAcc<- tkscrollbar(cdtAccInfo, repeatinterval=5,command=function(...)tkyview(txta.cdtAcc,...))
txta.cdtAcc<- tktext(cdtAccInfo,bg="white",font="courier",cursor="",wrap="word",height=10,width=wcdtAccInfo,
	yscrollcommand=function(...)tkset(yscr.cdtAcc,...))

tkgrid(txta.cdtAcc,yscr.cdtAcc)
tkgrid.configure(txta.cdtAcc,sticky="n",padx=5)
tkgrid.configure(yscr.cdtAcc,sticky="ns")
tkgrid.rowconfigure(txta.cdtAcc,0,weight=1)
tkgrid.columnconfigure(txta.cdtAcc,0,weight=1)


tktag.configure(txta.cdtAcc, "cdtfont4f", font=cdtfont4)
infofl.cdtAcc<-file.path(apps.dir,'text','cdt_welcome.txt',fsep = .Platform$file.sep)
rdL.cdtAcc<-readLines(infofl.cdtAcc,warn=FALSE)

tcl("update","idletasks")
for(i in 1:length(rdL.cdtAcc)) tkinsert(txta.cdtAcc,"end",paste(rdL.cdtAcc[i],"\n"),"cdtfont4f")

tkconfigure(txta.cdtAcc,state="disabled")

imgfl.cdtAcc<-file.path(imgdir,"iri_logo_full.gif",fsep = .Platform$file.sep)
cdtirilogo <- tkimage.create('photo',file=imgfl.cdtAcc)
imgcdtirilogo<- tklabel(frcdtAcc3, image=cdtirilogo)
tkgrid(imgcdtirilogo,sticky="e")

tkgrid(frcdtAcc1,row=0,column=0,columnspan=3,padx=5,pady=25,sticky='new')
tkgrid(frcdtAcc2,row=1,column=0,columnspan=3,padx=1)
tkgrid(frcdtAcc3,row=2,column=2,padx=5,pady=5,sticky='se')
tkgrid.rowconfigure(frcdtAcc1,0,weight=1)
tkgrid.columnconfigure(frcdtAcc1,0,weight=1)
tkgrid.rowconfigure(frcdtAcc2,0,weight=1)
tkgrid.columnconfigure(frcdtAcc2,0,weight=1)
tkgrid.rowconfigure(frcdtAcc3,0,weight=1)
tkgrid.columnconfigure(frcdtAcc3,2,weight=1)



