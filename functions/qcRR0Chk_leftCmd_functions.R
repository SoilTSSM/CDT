QcZeroCheckCmd<-function(stateReplaceAll){

	cmd.frame<-tkframe(panel.left,relief='groove',bd=2)

	pnddframe<-tkframe(cmd.frame)
	tkgrid(pnddframe ,sticky='nwes')

	dispPnddTab<-tkbutton(pnddframe, text="View & Edit Result")
	replacePndd<-tkbutton(pnddframe, text="Replace false zeros")
	sep1.tab1<-ttkseparator(pnddframe)
	replaceAllPndd<-tkbutton(pnddframe, text="Replace All false zeros",state=stateReplaceAll)

	infobulle(replacePndd,'Replaces false zeros with missing values')
	status.bar.display(replacePndd,txt.stbr1,'Replaces false zeros with missing values')

	infobulle(replaceAllPndd,'Replaces all false zeros with missing values at one time')
	status.bar.display(replaceAllPndd,txt.stbr1,'Replaces all false zeros with missing values at one time')

	tkgrid(dispPnddTab,row=0,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(replacePndd,row=0,column=4,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep1.tab1,row=1,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(replaceAllPndd,row=2,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)

	###################################################
	##display pNDD table
	pNddQcIdTab<-NULL

	tkconfigure(dispPnddTab,command=function(){
		if(!is.null(ret.results)){
			if(ret.results$AllOrOne=='one'){
				IJstation<-ret.results$station
			}
			if(ret.results$AllOrOne=='all'){
				stns<-sapply(ret.results$res, function(x) x$station)
				ijstn<-which(stns==tclvalue(stn.choix.val))
				IJstation<-ret.results$res[[ijstn]]$station
			}

			if(ret.results$action=='zero.check'){
				retNBTab<-tableZeroCheckNotebookTab_unik(tknotes,paste(IJstation,'-Zero-Check'),pNddQcIdTab,tab.type,tab.data)
				pNddQcIdTab<<-retNBTab$notebookTab
				tab.type<<-retNBTab$tab.type
				tab.data<<-retNBTab$tab.data
			}else insert.txt(main.txt.out,'NOT a zeros check outputs',format=TRUE)
		}else insert.txt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})

	###################################################
	##replace
	tkconfigure(replacePndd,command=function(){
		if(!is.null(ret.results)){
			if(ret.results$AllOrOne=='one'){
				IJstation<-ret.results$station
			}
			if(ret.results$AllOrOne=='all'){
				stns<-sapply(ret.results$res, function(x) x$station)
				ijstn<-which(stns==tclvalue(stn.choix.val))
				IJstation<-ret.results$res[[ijstn]]$station
			}
			repret<-try(replaceZeroChkbyNA(IJstation,ret.results),silent=TRUE)
			if(!inherits(repret, "try-error")){
				if(!is.null(repret)){
					if(repret==0) insert.txt(main.txt.out,paste("Zeros replacement finished successfully for", IJstation))
					#else insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
				}else  insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
			}else{
				insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
				insert.txt(main.txt.out,gsub('[\r\n]','',repret[1]),format=TRUE)
			}
		}else insert.txt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})


	###################################################
	##replace all
	tkconfigure(replaceAllPndd,command=function(){
		if(!is.null(ret.results)){
			stns<-sapply(ret.results$res, function(x) x$station)
			tkconfigure(main.win,cursor='watch');tcl("update","idletasks")
			lapply(stns,function(IJstation){
				repret<-try(replaceZeroChkbyNA(IJstation,ret.results),silent=TRUE)
				if(!inherits(repret, "try-error")){
					if(!is.null(repret)){
						if(repret==0) insert.txt(main.txt.out,paste("Zeros replacement finished successfully for", IJstation))
						#else insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
					}else  insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
				}else{
					insert.txt(main.txt.out,"Zeros replacement failed",format=TRUE)
					insert.txt(main.txt.out,gsub('[\r\n]','',repret[1]),format=TRUE)
				}
				tcl("update")
			})
			tkconfigure(main.win,cursor='')
		}else insert.txt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})


	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	######
	return(cmd.frame)
}


