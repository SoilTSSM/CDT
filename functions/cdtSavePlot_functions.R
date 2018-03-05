
SavePlot <- function(){
	if(Sys.info()["sysname"] == "Windows"){
		largeur <- 37
	}else{
		largeur <- 37
	}

	######################################
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	####################
	frA <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

	fileImage <- tclVar()

	txt.file.save <- tklabel(frA, text = 'File to save the image', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frA, textvariable = fileImage, width = largeur)
	bt.file.save <- tkbutton(frA, text = "...")

	tkconfigure(bt.file.save, command = function(){
		filetypes <- "{{JPEG format} {.jpeg .JPEG}} {{PNG format} {.png .PNG}} {{All files} *}"
		if(Sys.info()["sysname"] == "Windows"){
			file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "",
										filetypes = filetypes, defaultextension = TRUE)
		}else{
			file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes)
		}
		tclvalue(fileImage) <- if(!is.na(file2save)) file2save else ""
	})

	#########
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	####################
	# frB <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)
	frB <- ttklabelframe(frDialog, text = "Image properties", relief = 'sunken', borderwidth = 2)

	uImage <- tclVar("px")
	wImage <- tclVar(480)
	hImage <- tclVar(480)
	rImage <- tclVar(100)
	txtUnit <- tclVar("px")

	stateRes <- if(tclvalue(uImage) == "px") "disabled" else "normal"

	# txt.img.prop <- tklabel(frB, text = 'Image properties', anchor = 'w', justify = 'left')

	txt.img.unit <- tklabel(frB, text = 'Units', anchor = 'w', justify = 'left')
	cb.img.unit <- ttkcombobox(frB, values = c("px", "in", "cm", "mm"), textvariable = uImage, width = 3)

	txt.img.res <- tklabel(frB, text = 'Resolution', anchor = 'e', justify = 'right')
	en.img.res <- tkentry(frB, textvariable = rImage, width = 4, state = stateRes)
	txt.img.res1 <- tklabel(frB, text = 'ppi', anchor = 'w', justify = 'left')

	txt.img.width <- tklabel(frB, text = 'Width', anchor = 'e', justify = 'right')
	en.img.width <- tkentry(frB, textvariable = wImage, width = 4)
	txt.img.width1 <- tklabel(frB, text = tclvalue(txtUnit), textvariable = txtUnit, anchor = 'w', justify = 'left')

	txt.img.height <- tklabel(frB, text = 'Height', anchor = 'e', justify = 'right')
	en.img.height <- tkentry(frB, textvariable = hImage, width = 4)
	txt.img.height1 <- tklabel(frB, text = tclvalue(txtUnit), textvariable = txtUnit, anchor = 'w', justify = 'left')


	# tkgrid(txt.img.prop, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	tkgrid(txt.img.unit, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.img.unit, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.img.res, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.img.res, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.img.res1, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.img.width, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.img.width, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.img.width1, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.img.height, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.img.height, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.img.height1, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


	tkbind(cb.img.unit, "<<ComboboxSelected>>", function(){
		stateRes <- if(tclvalue(uImage) == "px") "disabled" else "normal"
		tkconfigure(en.img.res, state = stateRes)

		tclvalue(txtUnit) <- tclvalue(uImage)

		if(tclvalue(uImage) == "px"){
			tclvalue(wImage) <- 480
			tclvalue(hImage) <- 480
		}

		if(tclvalue(uImage) == "in"){
			tclvalue(wImage) <- 4
			tclvalue(hImage) <- 4
		}

		if(tclvalue(uImage) == "cm"){
			tclvalue(wImage) <- 6
			tclvalue(hImage) <- 6
		}

		if(tclvalue(uImage) == "mm"){
			tclvalue(wImage) <- 600
			tclvalue(hImage) <- 600
		}
	})

	####################
	tkgrid(frA, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	######################################

	bt.opt.OK <- tkbutton(frButt, text = "OK")
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(fileImage)%in%c("", "NA")){
			tkmessageBox(message = "File to save the image is missing", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			filename <- str_trim(tclvalue(fileImage))
			units <- str_trim(tclvalue(uImage))
			res <- as.numeric(str_trim(tclvalue(rImage)))
			width <- as.numeric(str_trim(tclvalue(wImage)))
			height <- as.numeric(str_trim(tclvalue(hImage)))

			foo <- tolower(tools::file_ext(filename))
			if(foo == "jpg") foo <- "jpeg"
			image.fun <- match.fun(foo)

			tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
			if(length(AllOpenTabType) > 0){
				if(AllOpenTabType[[tabid]] == "img"){
					image.fun(file = filename, width = width, height = height, units = units, res = res)
					if(class(AllOpenTabData[[tabid]][[2]]) == "tkwin") AllOpenTabData[[tabid]][[2]]$fun()
					else AllOpenTabData[[tabid]][[2]][[2]]$fun()
					dev.off()
				}
			}

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(main.win)
		}
	})



	tkconfigure(bt.opt.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(main.win)
	})

	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Image export options')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(main.win)
	})
	tkwait.window(tt)
	return(0)
}

