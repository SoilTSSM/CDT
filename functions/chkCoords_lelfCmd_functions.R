
ChkCoordCmdBut<-function(){
	cmd.frame<-tkframe(panel.left)
	tkgrid(cmd.frame,sticky='nswe',pady=5)

	chkframe<-tkframe(cmd.frame,relief='groove',bd=2)
	tkgrid(chkframe,sticky='we',ipadx=5,ipady=10)

	cmd.outTable<-tkbutton(chkframe, text="View/Edit Results")
	cmd.correctCoord<-tkbutton(chkframe, text="Correct Coordinates")

	tkgrid(cmd.outTable,row=0,column=0,padx=1,pady=1,sticky="w")
	tkgrid(cmd.correctCoord,row=0,column=1,padx=1,pady=1,sticky="e")


#############
	tkconfigure(cmd.outTable,command=function(){
		if(!is.null(ret.results)){
		retdata<-DisplayHomInfo(tknotes,ret.results$Stndoute,
		paste(getf.no.ext(as.character(gal.params$file.io$Values[1])),'_COORDS',sep=''))
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'StnInfo'
		tab.data[[ntab+1]]<<-retdata
		tkselect(tknotes,ntab)
		popupAddRemoveRow(tknotes)
		}else insert.txt(main.txt.out,'There is no coordinates check performed yet',format=TRUE)
	})

#############
	tkconfigure(cmd.correctCoord,command=function(){
		if(!is.null(ret.results)){
			if(!is.na(ret.results$Stndoute[1,1])){
				tkconfigure(main.win,cursor='watch');tcl('update')
				chk2run<-try(ret.results<<-checkCDTcoords(ret.results,gal.params), silent=TRUE)
				is.ok.chk <- !inherits(chk2run, "try-error")
				if(is.ok.chk){
					insert.txt(main.txt.out,"Coordinates were corrected successfully")
					tkconfigure(main.win,cursor='')
				}else{
					insert.txt(main.txt.out,'Coordinates correction failed',format=TRUE)
					insert.txt(main.txt.out,gsub('[\r\n]','',chk2run[1]),format=TRUE)
					tkconfigure(main.win,cursor='')
				}
			}
		}else insert.txt(main.txt.out,'There is no coordinates check performed yet',format=TRUE)
	})
#############

	return(cmd.frame)
}

#######################################


