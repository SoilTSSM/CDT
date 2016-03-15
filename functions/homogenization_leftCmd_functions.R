
HomogCmdBut<-function(gal.params){
	cmd.frame<-tkframe(panel.left,relief='groove',bd=2)
#	tkgrid(cmd.frame,sticky='we',pady=5)

	homogframe<-tkframe(cmd.frame)
	tkgrid(homogframe,sticky='we')
	cmd.preview<-tkbutton(homogframe, text="Output Preview")
	cmd.plot<-tkbutton(homogframe, text="Display Output")
	cmd.chgcpt<-tkbutton(homogframe, text="Change Breakpoints")
	cmd.replot<-tkbutton(homogframe, text="Display Change")
	cmd.reset<-tkbutton(homogframe, text="Undo Change")
	cmd.adjust<-tkbutton(homogframe, text="Adjust")
	cmd.ltbx<-tklistbox(homogframe,height=3,selectbackground="yellow",selectforeground="blue",selectmode="multiple",background="white")
	fr.adjplot<-tkframe(homogframe)
	cmd.adjplot<-tkbutton(fr.adjplot, text="Display Adjusted")
	#cmd.readjplot<-tkbutton(fr.adjplot, text="Replot")

	tkgrid(cmd.preview,row=0,column=0,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.plot,row=0,column=1,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.chgcpt,row=1,column=0,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.replot,row=1,column=1,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.reset,row=2,column=0,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.adjust,row=2,column=1,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.ltbx,row=3,column=0,padx=1,pady=1,sticky="ew")
	tkgrid(fr.adjplot,row=3,column=1,padx=1,pady=1,sticky="ew")
	tkgrid(cmd.adjplot,row=0,column=0,padx=1,pady=1,sticky="ew")
	#tkgrid(cmd.readjplot,row=1,column=0,padx=1,pady=1,sticky="ew")

	xseries <- c("Base series","Adjusted by mean","Adjusted by QM")
	for (i in 1:3)  tkinsert(cmd.ltbx,"end",xseries[i])
	tkselection.set(cmd.ltbx,0)

	#homAdjframe<-tkframe(cmd.frame)
	homAdjframe<-ttklabelframe(cmd.frame,text="Adjusted series selection",labelanchor="nw",relief="groove",borderwidth=2)
	tkgrid(homAdjframe,padx=2,pady=5,sticky='we')
	infobulle(homAdjframe,'Select the adjusted series to be retained')
	status.bar.display(homAdjframe,txt.stbr1,'Select the adjusted series to be retained')


	AdjMthdRadio1<- tkradiobutton(homAdjframe,text="By Mean",anchor='w',justify='left')
	AdjMthdRadio2<- tkradiobutton(homAdjframe,text="By Quantile Matching",anchor='w',justify='left')
	AdjMthdRadio3<- tkradiobutton(homAdjframe,text="Base Series",anchor='w',justify='left')
	tkgrid(AdjMthdRadio1,row=0,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(AdjMthdRadio2,row=1,column=0,sticky='ew',padx=1,pady=1)
	tkgrid(AdjMthdRadio3,row=2,column=0,sticky='ew',padx=1,pady=1)
	ChoixAjustment <- tclVar('1')
	tkconfigure(AdjMthdRadio1,variable=ChoixAjustment,value="1")
	tkconfigure(AdjMthdRadio2,variable=ChoixAjustment,value="2")
	tkconfigure(AdjMthdRadio3,variable=ChoixAjustment,value="3")

####################
	PrevwHomIdTab<-NULL
	tkconfigure(cmd.preview,command=function(){
		if(!is.null(ret.results)){
			res2disp<-ret.results
			res2disp$refSerie<-NULL

			retNBTab<-consolOutNotebookTab_unik(tknotes,res2disp,paste(res2disp$station,'-Output Preview'),PrevwHomIdTab,tab.type,tab.data)
			PrevwHomIdTab<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no homogenization results',format=TRUE)
	})

########
	noteHomBrkPlot<-NULL
	tkconfigure(cmd.plot,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			imgContainer<-plotHomogBreakPts(tknotes,noteHomBrkPlot,replotBreak=FALSE)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteHomBrkPlot,tab.type,tab.data)
			noteHomBrkPlot<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no homogenization results',format=TRUE)
	})

#######
	noteHomBrkPlot1<-NULL
	tkconfigure(cmd.replot,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			imgContainer<-plotHomogBreakPts(tknotes,noteHomBrkPlot1,replotBreak=TRUE)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteHomBrkPlot1,tab.type,tab.data)
			noteHomBrkPlot1<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no homogenization results',format=TRUE)
	})

######
	noteHomAdjPlot<-NULL
	tkconfigure(cmd.adjplot,command=function(){
		if(!is.null(ret.results) & !is.null(adjDon)){
			iselect<-as.numeric(tkcurselection(cmd.ltbx))+1

			imgContainer<-plotadjusted(tknotes,iselect,noteHomAdjPlot)
			retNBTab<-imageNotebookTab_unik(tknotes,imgContainer,noteHomAdjPlot,tab.type,tab.data)
			noteHomAdjPlot<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no adjusted data yet',format=TRUE)
	})

#######
	HomEditSetTab<-NULL
	tkconfigure(cmd.chgcpt,command=function(){
		if(!is.null(ret.results)){
			retNBTab<-tableHomogNotebookTab_unik(tknotes,paste(ret.results$station,'_Edit',sep=''),HomEditSetTab,tab.type,tab.data)
			HomEditSetTab<<-retNBTab$notebookTab
			tab.type<<-retNBTab$tab.type
			tab.data<<-retNBTab$tab.data
		}else insert.txt(main.txt.out,'There is no homogenization results',format=TRUE)
	})

#######
	tkconfigure(cmd.reset,command=function(){
		if(!is.null(ret.results)) undoBreaksChange()
		else insert.txt(main.txt.out,'There is no homogenization results',format=TRUE)
	})

	tkconfigure(cmd.adjust,command=function(){
		if(!is.null(ret.results) & !is.null(gal.params)){
			adj2run<-try(adjDon<<-AdjustInHomog(as.numeric(tclvalue(ChoixAjustment))), silent=TRUE)
			is.ok.adj <- !inherits(adj2run, "try-error")
			if(is.ok.adj){
				retdata<-DisplayHomInfo(tknotes,data.frame(adjDon$Info),paste(ret.results$station,'Adj.Info'))
				ntab<-length(tab.type)
				tab.type[[ntab+1]]<<-'homInfo'
				tab.data[[ntab+1]]<<-retdata
				tkselect(tknotes,ntab)
				insert.txt(main.txt.out,paste("Adjustment finished successfully for", ret.results$station))
			}else{
				insert.txt(main.txt.out,paste("Adjustment failed for",ret.results$station),format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',adj2run[1]),format=TRUE)
			}
		}else insert.txt(main.txt.out,'Reinitialize the operation. Parameters or Outputs are not a homogenization results',format=TRUE)
	})


	######
	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	######

	return(list(cmd.frame,gal.params))
}

