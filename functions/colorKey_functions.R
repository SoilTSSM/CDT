
createColorkey <- function(parent.win, listCol){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	####
	fr1 <- tkframe(frDialog)
	fr2 <- tkframe(frDialog, relief = 'sunken', bd = 2, highlightcolor = 'black')
	fr3 <- tkframe(frDialog)
	fr4 <- tkframe(frDialog, relief = 'groove', bd = 2)
	fr5 <- tkframe(frDialog)
	fr6 <- tkframe(frDialog)
	tkgrid(fr1, row = 0, column = 0, columnspan = 3, sticky = 'w')
	tkgrid(fr2, row = 1, column = 0, columnspan = 3, sticky = 'w')
	tkgrid(fr3, row = 2, column = 0, columnspan = 3)
	tkgrid(fr4, row = 3, column = 0, columnspan = 2, sticky = 'we')
	tkgrid(fr5, row = 3, column = 2, columnspan = 1, sticky = 'e')
	tkgrid(fr6, row = 4, column = 0, columnspan = 3)

	#########
	if(length(listCol) > 0){
		nlCol <- length(listCol)
		varCol <- tclVar(listCol[nlCol])
	}else{
		varCol <- tclVar('red')
	}

	labColor <- tklabel(fr1, text = 'Color:')
	canColor <- tkcanvas(fr1, width = "80", height = "25", bg = tclvalue(varCol))	
	addColor <- tkbutton(fr1, text = "Add")
	saveColor <- tkbutton(fr1, text = "Save")
	loadColor <- tkbutton(fr1, text = "Load")
	tkgrid(labColor, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(canColor, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(addColor, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(saveColor, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(loadColor, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	
	#########
	if(Sys.info()["sysname"] == "Windows"){
		txtCol_width <- 30
	}else{
		txtCol_width <- 40
	}
	
	yscrColor <- tkscrollbar(fr3, repeatinterval = 4, command = function(...) tkyview(textColor,...))
	textColor <- tktext(fr3, bg = "white", yscrollcommand = function(...) tkset(yscrColor,...), wrap = "word", height = 5, width = txtCol_width)
	tkgrid(textColor, yscrColor)
	tkgrid.configure(yscrColor, sticky = "ns")
	tkgrid.configure(textColor, sticky = 'nswe') 

	#########
	editColor <- tclVar(0)
	chkButColor <- tkcheckbutton(fr4, variable = editColor, text = 'Edit Colors', anchor = 'w', justify = 'left')
	dispButColor <- tkbutton(fr4, text = "Show")
	tkgrid(chkButColor, row = 0, column = 0, sticky = 'w')
	tkgrid(dispButColor, row = 0, column = 1, sticky = 'e')

	#########
	dispGradColor <- tkbutton(fr5, text = "Preview")
	tkgrid(dispGradColor, sticky = 'e')

	#########
	canWidth <- as.double(tkwinfo('reqwidth', textColor))
	canPreview <- tkcanvas(fr6, width = canWidth, height = 20, bg = 'white')
	tkgrid(canPreview, sticky = 'we')
	
	#########
	listCanColor <- list()

	if(length(listCol) > 0){
		for(j in seq_along(listCol)){
			listCanColor[[j]] <- tkcanvas(fr2, width = "20", height = "20", bg = listCol[j])
			pj <- j-1
			if(Sys.info()["sysname"] == "Windows") tkgrid(listCanColor[[j]], row = floor(pj/10), column = pj%%10)
			else tkgrid(listCanColor[[j]], row = floor(pj/14), column = pj%%14)
			tkinsert(textColor, "end", paste(listCol[j],', ',sep = ''))		
		}
		iColor <- length(listCol)
	}else{
		iColor <- 0
		listCol[iColor+1] <- tclvalue(varCol)
	}

	#########
	tkconfigure(addColor, command = function(){
		color <- tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(varCol), title = "Colors"))
		if(nchar(color) > 0){
			tkconfigure(canColor, bg = color)
			tclvalue(varCol) <- color
			listCol[iColor+1] <<- color
			listCanColor[[iColor+1]] <<- tkcanvas(fr2, width = "20", height = "20", bg = listCol[iColor+1])
			if(Sys.info()["sysname"] == "Windows") tkgrid(listCanColor[[iColor+1]], row = floor(iColor/10), column = iColor%%10)
			else tkgrid(listCanColor[[iColor+1]], row = floor(iColor/14), column = iColor%%14)
			tkinsert(textColor, "end", paste(color,', ',sep = ''))
			iColor <<- iColor+1
		}
	})

	#########
	tkconfigure(saveColor, command = function(){
		if(tclvalue(editColor) == '1'){
			vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
			vcolor <- gsub("[\t\r\n]", "", vcolor)
			vcolor <- gsub('\\s+', '',vcolor)
			vcolor <- strsplit(vcolor,",")[[1]]
			vcolor <- vcolor[!is.na(vcolor) & vcolor != '']
			listCol <- vcolor
		}else{
			listCol <- listCol[!is.na(listCol)]
		} 
		file2save <- tclvalue(tkgetSaveFile(initialdir = getwd(), initialfile = "", defaultextension=".clr", filetypes="{{Color Files} {.clr .CLR}} {{All files} *}"))
		write.table(listCol, file2save, row.names = F, col.names = F)
	})

	#########
	tkconfigure(loadColor, command = function(){
		fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes="{{Color Files} {.clr .CLR}} {{All files} *}"))
		if(fileopen != "" | !is.na(fileopen)){
			isOK<-!inherits(try(datColor <- read.table(fileopen, strip.white = TRUE, colClasses = "character"), silent = TRUE), "try-error")
			if(isOK){
				listCol <<- as.character(datColor[,1])
				if(length(listCanColor) > 0) for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
				tkdelete(textColor, "0.0", "end")
				listCanColor <<- list()
				 for(icvs1 in seq_along(listCol)){
					 tkinsert(textColor, "end", paste(listCol[icvs1],', ',sep = ''))		
					 listCanColor[[icvs1]] <<- tkcanvas(fr2, width = "20", height = "20", bg = listCol[icvs1])
					 pj <- icvs1-1
					 if(Sys.info()["sysname"] == "Windows") tkgrid(listCanColor[[icvs1]], row = floor(pj/10), column = pj%%10)
					 else tkgrid(listCanColor[[icvs1]], row = floor(pj/14), column = pj%%14)
				 }
				nlCol <- length(listCol)
				if(nlCol > 0){
					tkconfigure(canColor, bg = listCol[nlCol])
					tclvalue(varCol) <- listCol[nlCol]
					iColor <<- nlCol
				}else iColor <<- 0
			}	
		}
	})
	
	#########
	tkconfigure(dispButColor, command = function(){
		if(tclvalue(editColor) == '1'){
			vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
			vcolor <- gsub("[\t\r\n]", "", vcolor)
			vcolor <- gsub('\\s+', '',vcolor)
			vcolor <- strsplit(vcolor,",")[[1]]
			vcolor <- vcolor[!is.na(vcolor) & vcolor != '']
			listCol <<- vcolor
			if(length(listCol) > 0){
				if(length(listCanColor) > 0) for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
				tkdelete(textColor, "0.0", "end")
				listCanColor <<- list()
				 for(icvs1 in seq_along(listCol)){
					 tkinsert(textColor, "end", paste(listCol[icvs1],', ',sep = ''))		
					 listCanColor[[icvs1]] <<- tkcanvas(fr2, width = "20", height = "20", bg = listCol[icvs1])
					 pj <- icvs1-1
					 if(Sys.info()["sysname"] == "Windows") tkgrid(listCanColor[[icvs1]], row = floor(pj/10), column = pj%%10)
					 else tkgrid(listCanColor[[icvs1]], row = floor(pj/14), column = pj%%14)
				 }
				nlCol <- length(listCol)
				if(nlCol > 0){
					tkconfigure(canColor, bg = listCol[nlCol])
					tclvalue(varCol) <- listCol[nlCol]
					iColor <<- nlCol
				}else iColor <<- 0
			}
		}
	})
	
	#########
	tkconfigure(dispGradColor, command = function(){
		if(tclvalue(editColor) == '1'){
			vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
			vcolor <- gsub("[\t\r\n]", "", vcolor)
			vcolor <- gsub('\\s+', '',vcolor)
			vcolor <- strsplit(vcolor,",")[[1]]
			vcolor <- vcolor[!is.na(vcolor) & vcolor != '']
			listCol <- vcolor
		}else{
			listCol <- listCol[!is.na(listCol)]
		}
		kolor <- getGradientColor(listCol, 0:canWidth)
		tkdelete(canPreview, 'gradlines')
		for(i in 0:canWidth) tkcreate(canPreview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines')
		tcl('update')
	})
	
	#########
	varRemColor <- tclVar()	
	popMenuRemColor <- tkmenu(fr2, tearoff = FALSE)

	tkadd(popMenuRemColor, "command", label = "Change", command = function(){
		icanvas <- match(tclvalue(varRemColor), sapply(listCanColor, function(x) x$ID))
		color <- tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(varCol), title = "Colors"))
		if(nchar(color) > 0){
			tkconfigure(canColor, bg = color)
			tclvalue(varCol) <- color
			listCol[icanvas] <<- color
			tkconfigure(listCanColor[[icanvas]], bg = color)
			tkdelete(textColor, "0.0", "end")
			for(icvs in seq_along(listCol)) tkinsert(textColor, "end", paste(listCol[icvs],', ',sep = ''))
		}
	})

	tkadd(popMenuRemColor, "separator")

	tkadd(popMenuRemColor, "command", label = "Remove", command = function(){
		icanvas <- match(tclvalue(varRemColor), sapply(listCanColor, function(x) x$ID))
		if(!is.na(icanvas)){
			for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
			tkdelete(textColor, "0.0", "end")
			listCol <<- listCol[-icanvas]
			listCanColor <<- list()
			 for(icvs1 in seq_along(listCol)){
				 tkinsert(textColor, "end", paste(listCol[icvs1],', ',sep = ''))		
				 listCanColor[[icvs1]] <<- tkcanvas(fr2, width = "20", height = "20", bg = listCol[icvs1])
				 pj <- icvs1-1
				 if(Sys.info()["sysname"] == "Windows") tkgrid(listCanColor[[icvs1]], row = floor(pj/10), column = pj%%10)
				 else tkgrid(listCanColor[[icvs1]], row = floor(pj/14), column = pj%%14)
			 }
			nlCol <- length(listCol)
			if(nlCol > 0){
				tkconfigure(canColor, bg = listCol[nlCol])
				tclvalue(varCol) <- listCol[nlCol]
				iColor <<- nlCol
			}else iColor <<- 0
		}
	})

	getColorCanvas <- function(W, x, y){
		rootx <- as.integer(tkwinfo("rootx", W))  
		rooty <- as.integer(tkwinfo("rooty", W))
		xpos <- as.integer(x) + rootx
		ypos <- as.integer(y) + rooty
		wdgt <- tclvalue(tkwinfo('containing', displayof = W, xpos, ypos))
		tclvalue(varRemColor) <<- wdgt
		.Tcl(paste("tk_popup",.Tcl.args(popMenuRemColor, xpos, ypos)))
	}
	
	#########
	tkbind(fr2,"<Enter>", function(){ 
		for(jj in seq_along(listCanColor)){
			tkbind(listCanColor[[jj]], "<Button-3>", function(W, x, y){
				getColorCanvas(W, x, y)
			})
		}
	})	
	
	#########	
	bt.opt.OK <- tkbutton(frButt, text = "OK") 
	bt.opt.CA <- tkbutton(frButt, text = "Cancel") 	
	tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, sticky = 'w')
	tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 5, sticky = 'e')

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(editColor) == '1'){
			vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
			vcolor <- gsub("[\t\r\n]", "", vcolor)
			vcolor <- gsub('\\s+', '',vcolor)
			vcolor <- strsplit(vcolor,",")[[1]]
			vcolor <- vcolor[!is.na(vcolor) & vcolor != '']
			listCol <<- vcolor
		}else{
			listCol <<- listCol[!is.na(listCol)]
		} 
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})
	
	tkconfigure(bt.opt.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############################################################	
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	#tkwm.geometry(tt, paste(tt.w, 'x', tt.h,'+',tt.x,'+',tt.y, sep = ''))
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Color Selection')
	tkwm.deiconify(tt)
	
	##################################################################	
	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(listCol)
}
