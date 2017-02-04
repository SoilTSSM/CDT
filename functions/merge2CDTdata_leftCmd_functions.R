
merge2CDTDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()

	cmd.frame <- tkframe(panel.left)

	input.cmd <- tkframe(cmd.frame, relief = 'groove', bd = 2)
	merge.cmd <- tkframe(cmd.frame)

	tkgrid(input.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2)
	tkgrid(merge.cmd, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(input.cmd, 0, weight = 1)
	tkgrid.columnconfigure(merge.cmd, 0, weight = 1)

	#############

	file.stnfl1 <- tclVar()
	file.stnfl2 <- tclVar()
	file.save1 <- tclVar()

	txtStnfl1 <- tklabel(input.cmd, text = 'CDT data #1', anchor = 'w', justify = 'left')
	cbStnfl1 <- ttkcombobox(input.cmd, values = unlist(listOpenFiles), textvariable = file.stnfl1)
	btStnfl1 <- tkbutton(input.cmd, text = "...")
	txtStnfl2 <- tklabel(input.cmd, text = 'CDT data #2', anchor = 'w', justify = 'left')
	cbStnfl2 <- ttkcombobox(input.cmd, values = unlist(listOpenFiles), textvariable = file.stnfl2)
	btStnfl2 <- tkbutton(input.cmd, text = "...")
	txtFileSave <- tklabel(input.cmd, text = 'File to save merged data', anchor = 'w', justify = 'left')
	enFileSave <- tkentry(input.cmd, textvariable = file.save1)
	btFileSave <- tkbutton(input.cmd, text = "...")

	#############

	tkconfigure(btStnfl1, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl1) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cbStnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
			tkconfigure(cbStnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
		}else return(NULL)
	})

	tkconfigure(btStnfl2, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cbStnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
			tkconfigure(cbStnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
		}else return(NULL)
	})

	tkconfigure(btFileSave, command = function(){
		filetypes  <-  "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if (Sys.info()["sysname"] == "Windows") file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes)
		tclvalue(file.save1) <- if(is.na(file2save1)) "" else file2save1
	})

	#############

	tkgrid(txtStnfl1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cbStnfl1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btStnfl1, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txtStnfl2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cbStnfl2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btStnfl2, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txtFileSave, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(enFileSave, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btFileSave, row = 5, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############

	mrgDataBut <- ttkbutton(merge.cmd, text = "Merge Data")

	tkconfigure(mrgDataBut, command = function(){
 		GeneralParameters <- list(file1 = tclvalue(file.stnfl1),
								file2 = tclvalue(file.stnfl2),
								file2save = tclvalue(file.save1))

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Merge data...........")
		tcl('update')
		ret <- tryCatch(merge2CDTdata(GeneralParameters),
		#warning = function(w) warningFun(w),
		error = function(e) errorFun(e),
		finally = {
			tkconfigure(main.win, cursor = '')
		})

		if(!is.null(ret)){
			if(ret == 0) InsertMessagesTxt(main.txt.out, "Merging CDT data finished successfully")
			else InsertMessagesTxt(main.txt.out, "Merging CDT data failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Merging CDT data failed", format = TRUE)
		}
	})

	tkgrid(mrgDataBut, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)

	#############
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 10)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	return(cmd.frame)
}
