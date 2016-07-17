
#tcl('update')
######################################################################################

wcdtAccInfo<-as.integer(w.scale(70)/28)

cdtfont1 <- tkfont.create(family="times",size=30,weight="bold")
cdtfont2 <- tkfont.create(family="times",size=18,weight="bold")
cdtfont3 <- tkfont.create(family="times",size=12)
cdtfont4 <- tkfont.create(family="courier",size=12)

infofl.cdtAcc<-file.path(apps.dir,'text','cdt_welcome.txt',fsep = .Platform$file.sep)
rdL.cdtAcc<-readLines(infofl.cdtAcc,warn=FALSE)

imgfl.cdtAcc<-tkimage.create('photo',file=file.path(imgdir,"CDT_acceuil.gif",fsep = .Platform$file.sep))
irifl.cdtAcc<-tkimage.create('photo',file=file.path(imgdir,"iri_logo_full.gif",fsep = .Platform$file.sep))
# irifl.cdtAcc<-resizeTclImage(file.path(imgdir,"iri_logo_full.gif",fsep = .Platform$file.sep),factor=2,zoom=FALSE)

#####
frame_cdt <- tkframe(tknotes)
frame_msg <- tkframe(tknotes,relief='groove',borderwidth=2)
frame_img <- tkframe(tknotes)
frame_iri <- tkframe(tknotes)

## CDT
cdtname<-tklabel(frame_cdt,text='Climate Data Tools',font=cdtfont1,foreground='blue4')
cdtver<-tklabel(frame_cdt,text=paste('Version', cdtVersion),font=cdtfont2)
cdtauth1<-tklabel(frame_cdt,text='Rija Faniriantsoa, Tufa Dinku',font=cdtfont3)
# cdtauth2<-tklabel(frame_cdt,text='Tufa Dinku',font=cdtfont3)

## MSG
# xscr.cdtAcc<- tkscrollbar(frame_msg, repeatinterval=5,orient="horizontal",command=function(...) tkxview(txta.cdtAcc,...))
# yscr.cdtAcc<- tkscrollbar(frame_msg, repeatinterval=5,command=function(...) tkyview(txta.cdtAcc,...))
# txta.cdtAcc<- tktext(frame_msg,bg="white",font="courier",cursor="",wrap="word",height=10,width=wcdtAccInfo,
# 						xscrollcommand=function(...) tkset(xscr.cdtAcc,...),
# 						yscrollcommand=function(...) tkset(yscr.cdtAcc,...))

txta.cdtAcc<- tktext(frame_msg,bg="white",font="courier",cursor="",wrap="word",height=10,width=wcdtAccInfo)
tktag.configure(txta.cdtAcc, "cdtfont4f", font=cdtfont4)

## IMG & IRI
imgcdtoutput<- tklabel(frame_img, image=imgfl.cdtAcc)
imgirilogo<- tklabel(frame_iri, image=irifl.cdtAcc)

#####
tkgrid(cdtname,sticky='nsew')
tkgrid(cdtver,sticky='nsew')
tkgrid(cdtauth1,sticky='nsew')
# tkgrid(cdtauth2,sticky='nsew')

# tkgrid(txta.cdtAcc,yscr.cdtAcc)
# tkgrid(xscr.cdtAcc)
# tkgrid.configure(txta.cdtAcc,sticky="nsew")
# tkgrid.configure(yscr.cdtAcc,sticky="ns")
# tkgrid.configure(xscr.cdtAcc,sticky="ew")
tkgrid(txta.cdtAcc,sticky="nsew")
tkgrid(imgcdtoutput,sticky="nsew")
tkgrid(imgirilogo,sticky="nsew")

######
tkgrid(frame_cdt,row=0,column=0,sticky='nswe',rowspan=4,columnspan=60,padx=1,pady=25,ipadx=1,ipady=1) #'news'
tkgrid(frame_msg,row=4,column=0,sticky='nswe',rowspan=6,columnspan=20,padx=5,pady=2,ipadx=1,ipady=1)
tkgrid(frame_img,row=4,column=20,sticky='nse',rowspan=6,columnspan=40,padx=2,pady=2,ipadx=1,ipady=1)
tkgrid(frame_iri,row=11,column=40,sticky='se',rowspan=2,columnspan=20,padx=2,pady=2,ipadx=1,ipady=1)

for(i in 0:3) tkgrid.rowconfigure(frame_cdt,i,weight=1) 
tkgrid.columnconfigure(frame_cdt,0,weight=1)
tkgrid.rowconfigure(frame_msg,0,weight=1)
tkgrid.columnconfigure(frame_msg,0,weight=1)
tkgrid.rowconfigure(frame_img,0,weight=1)
tkgrid.columnconfigure(frame_img,0,weight=1)
tkgrid.rowconfigure(frame_iri,0,weight=1)
tkgrid.columnconfigure(frame_iri,0,weight=1)

tcl("update","idletasks")
for(i in 1:length(rdL.cdtAcc)) tkinsert(txta.cdtAcc,"end",paste(rdL.cdtAcc[i],"\n"),"cdtfont4f")
tkconfigure(txta.cdtAcc,state="disabled")
