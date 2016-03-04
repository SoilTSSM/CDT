plotSpatialCheck<-function(ijsp,dem,shpf,ZoomXYval,showval){
	outlparams<-getOutlier.params()
	outqcf<-outlparams$qcout
	xdates<-outlparams$dates

	if(ret.results$action=='qc.rain' & gal.params$action=='qc.rain'){
		lon<-EnvQcOutlierData$donnees$lon
		lat<-EnvQcOutlierData$donnees$lat
		idStn<-as.character(EnvQcOutlierData$donnees$id)

		arrData<-EnvQcOutlierData$donnees$data	
		outdates<-as.character(outqcf$dates)
		outlq<-as.character(outqcf$spatial.check)
		#outdates1<-outdates[!is.na(outlq) & as.numeric(outlq)>0]
		outdates1<-outdates[!is.na(outlq)]
	}else if(ret.results$action=='qc.temp' & gal.params$action=='qc.temp'){
		lon<-EnvQcOutlierData$donnees1$lon
		lat<-EnvQcOutlierData$donnees1$lat
		idStn<-as.character(EnvQcOutlierData$donnees1$id)

		arrData<-EnvQcOutlierData$donnees1$data
		outdates<-as.character(outqcf$dates)
		outlq<-as.character(outqcf$spatial.reg.check)
		outlv<-as.numeric(outqcf$estimated.values)
		#testv<-!is.na(outlq) & as.numeric(outlq)>0
		testv<-!is.na(outlq)
		outdates1<-outdates[testv]
		outlv1<-outlv[testv]
	}

	if(length(outdates1)>0){  #inutile deja tester sur button next et prev
		ijDate<-outdates1[ijsp]
		ijSpat<-which(xdates==ijDate)
		valm<-as.vector(arrData[ijSpat,])

		ix<-!is.na(valm)
		lon<-lon[ix]
		lat<-lat[ix]
		idStn<-idStn[ix]
		valm<-valm[ix]
		ijStn<-which(idStn==as.character(outqcf$stn[1]))

		ocrds<-getBoundaries(shpf)
		dem.lon<-dem$lon
		dem.lat<-dem$lat
		dem.val<-dem$dem
		xlon1<-ZoomXYval[1]
		xlon2<-ZoomXYval[2]
		xlat1<-ZoomXYval[3]
		xlat2<-ZoomXYval[4]

		outex<-0.1
#		xmin<-if(!is.na(xlon1) | is.null(xlon1) | is.infinite(xlon1)) xlon1 else min(lon)-outex
#		xmax<-if(!is.na(xlon2) | is.null(xlon2) | is.infinite(xlon2)) xlon2 else max(lon)+outex
#		ymin<-if(!is.na(xlat1) | is.null(xlat1) | is.infinite(xlat1)) xlat1 else min(lat)-outex
#		ymax<-if(!is.na(xlat2) | is.null(xlat2) | is.infinite(xlat2)) xlat2 else max(lat)+outex

		if(!is.na(xlon1) | is.null(xlon1) | is.infinite(xlon1)) xmin<-xlon1
		else{
			xmin<-min(lon)-outex
			tclvalue(xx1)<<-xmin
		}
		if(!is.na(xlon2) | is.null(xlon2) | is.infinite(xlon2)) xmax<-xlon2
		else{
			xmax<-max(lon)+outex
			tclvalue(xx2)<<-xmax
		}
		if(!is.na(xlat1) | is.null(xlat1) | is.infinite(xlat1)) ymin<-xlat1
		else{
			ymin<-min(lat)-outex
			tclvalue(yy1)<<-ymin
		}
		if(!is.na(xlat2) | is.null(xlat2) | is.infinite(xlat2)) ymax<-xlat2
		else{
			ymax<-max(lat)+outex
			tclvalue(yy2)<<-ymax
		}

		#######
		opar<-par(mar = c(4,4,2.5,5))
		plot(1,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="",ylab="",type="n",xaxt = 'n',yaxt = 'n')

		if(!is.null(dem)) image.plot(dem.lon,dem.lat,dem.val,col=gray(seq(0.9,0.1,length=64)),add=TRUE,legend.mar=5)
		if(!is.null(shpf)) lines(ocrds)

		points(lon[-ijStn],lat[-ijStn],pch=19,col='darkred',cex=0.5)
		points(lon[ijStn],lat[ijStn],pch=1,col='darkgoldenrod1',cex=1)
		points(lon[ijStn],lat[ijStn],pch=20,col='red',cex=0.5)
		abline(h = axTicks(2), v =axTicks(1) , col = "lightgray",lty=3)
		if(showval){
#if(length(valm[ijStn])==0)
			if(gal.params$action=='qc.temp'){
				text(lon[ijStn],lat[ijStn],labels=valm[ijStn],pos=1,cex=0.6,col='red')
				text(lon[ijStn],lat[ijStn],labels=outlv1[ijsp],pos=3,cex=0.6,col='green2')
				text(lon[-ijStn],lat[-ijStn],labels=valm[-ijStn],pos=1,cex=0.6,col='blue')
			}else if(gal.params$action=='qc.rain'){
				text(lon[ijStn],lat[ijStn],labels=valm[ijStn],pos=1,cex=0.6,col='red')
				text(lon[-ijStn],lat[-ijStn],labels=valm[-ijStn],pos=1,cex=0.6,col='blue')
			}
		}
		if(Sys.info()["sysname"] == "Windows") axlabs<-LatLonAxisLabels(axTicks(1),axTicks(2))
		else axlabs<-LatLonAxisLabels1(axTicks(1),axTicks(2))
		axis(side=1, at=axTicks(1),labels=axlabs$xaxl,tck=-0.01,cex.axis=0.8)
		axis(side=2, at=axTicks(2), labels=axlabs$yaxl,tck=-0.01,las=1,cex.axis=0.8)
		title(main=paste('STN:',idStn[ijStn],'Date:',ijDate),cex.main=1,font.main=1)
		box()
		plt <- par("plt")
		usr<-par("usr")
		par(opar)

		return(list(plt=plt,usr=usr,lon=lon,lat=lat,idStn=idStn))
	}else{
		return(NULL)
	}
}


#################################################################################################

DisplaySpatialCheck<-function(parent,ijsp,ZoomXYval,dem,shpf,showval,noteQcSpatCheck){

	############
	if(gal.params$AllOrOne=='one'){
		IJstation<-ret.results$station
	}
	if(gal.params$AllOrOne=='all'){
		ijstn<-which(as.character(gal.params$parameter[[2]][,1])==tclvalue(stn.choix.val))
		IJstation<-ret.results$station[[ijstn]]
	}

	##########PLOT
	pltusr <- NULL
	parPlotSize1<-tclVar()
	parPlotSize2<-tclVar()
	parPlotSize3<-tclVar()
	parPlotSize4<-tclVar()
	usrCoords1<-tclVar()
	usrCoords2<-tclVar()
	usrCoords3<-tclVar()
	usrCoords4<-tclVar()
	plotIt <- function(){
		op<-par(bg='white')
		pltusr <<-plotSpatialCheck(ijsp,dem,shpf,ZoomXYval,showval)
		tclvalue(parPlotSize1)<<-pltusr$plt[1]
		tclvalue(parPlotSize2)<<-pltusr$plt[2]
		tclvalue(parPlotSize3)<<-pltusr$plt[3]
		tclvalue(parPlotSize4)<<-pltusr$plt[4]
		tclvalue(usrCoords1)<<-pltusr$usr[1]
		tclvalue(usrCoords2)<<-pltusr$usr[2]
		tclvalue(usrCoords3)<<-pltusr$usr[3]
		tclvalue(usrCoords4)<<-pltusr$usr[4]
		par(op)
	}

	parPltCrd<-list(parPlotSize1=parPlotSize1,parPlotSize2=parPlotSize2,
	parPlotSize3=parPlotSize3,parPlotSize4=parPlotSize4,
	usrCoords1=usrCoords1,usrCoords2=usrCoords2,usrCoords3=usrCoords3,usrCoords4=usrCoords4)

	###################################################################
	onglet<-imageNotebookTab_open(parent,noteQcSpatCheck,tabTitle=paste(IJstation,'Spatial Check',sep='-'),tab.type,tab.data)
	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

	canvas<-tkcanvas(onglet[[2]])
	tkgrid(canvas)

	img<-tkrplot1(canvas,fun=plotIt,hscale=hscale, vscale=vscale)
	img_w<-as.double(tcl('image','width',img$image))
	img_h<-as.double(tcl('image','height',img$image))
	tkconfigure(canvas,width=img_w,height=img_h)
	tkcreate(canvas, "image", 0, 0,anchor='nw',image=img$image)

	tcl('raise',canvas)
	tcl('update')

	######
	tkbind(canvas,"<Enter>",function(){
		if(tclvalue(pressButP)=="1") tkconfigure(canvas,cursor='sizing')
		else if(tclvalue(pressButM)=="1") tkconfigure(canvas,cursor='sizing')
		else if(tclvalue(pressButRect)=="1") tkconfigure(canvas,cursor='sizing')
		else if(tclvalue(pressButDrag)=="1") tkconfigure(canvas,cursor='hand1')
		else if(tclvalue(pressGetCoords)=="1") tkconfigure(canvas,cursor='draped_box')
		else tkconfigure(canvas,cursor='crosshair')
	})

	tkbind(canvas,"<Leave>",function() tkconfigure(canvas,cursor=''))


	#####
	##draw rectangle initial value
	lastX <- 0
	lastY <- 0

	##zoom factor
	factZoom<-0.2

	##zoom rectangle
	rectZoomInit<-ZoomXYval

	##Pan Image
	panZoomInit<-c(0,0,0,0,0,0)
	factPan<-0.2

	############

	tkbind(canvas, "<Button-1>", function(W,x,y){
		ret<-getXYCoords(W,x,y,parPltCrd)
		tkdelete(W,'rect')

		#Zoom plus
		if(tclvalue(pressButP)=="1" & !ret$oin){
			rgX<-as.numeric(tclvalue(usrCoords2))-as.numeric(tclvalue(usrCoords1))
			rgY<-as.numeric(tclvalue(usrCoords4))-as.numeric(tclvalue(usrCoords3))
			shiftX<-rgX*(1-factZoom)/2
			shiftY<-rgY*(1-factZoom)/2
			xmin1<-ret$xc-shiftX
			xmax1<-ret$xc+shiftX
			ymin1<-ret$yc-shiftY
			ymax1<-ret$yc+shiftY
			ZoomXYval<<-c(xmin1,xmax1,ymin1,ymax1)
			tclvalue(xx1)<<-round(xmin1,4)
			tclvalue(xx2)<<-round(xmax1,4)
			tclvalue(yy1)<<-round(ymin1,4)
			tclvalue(yy2)<<-round(ymax1,4)
			refreshPlot1(W,img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
		}

		#Zoom Moins
		if(tclvalue(pressButM)=="1"  & !ret$oin){
			rgX<-as.numeric(tclvalue(usrCoords2))-as.numeric(tclvalue(usrCoords1))
			rgY<-as.numeric(tclvalue(usrCoords4))-as.numeric(tclvalue(usrCoords3))
			shiftX<-rgX*(1+factZoom)/2
			shiftY<-rgY*(1+factZoom)/2
			xmin1<-ret$xc-shiftX
			xmax1<-ret$xc+shiftX
			ymin1<-ret$yc-shiftY
			ymax1<-ret$yc+shiftY

			if(xmin1< -180 | xmax1>180 | ymin1< -90 | ymax1>90){
				tclvalue(pressButP)<<-0
				tclvalue(pressButM)<<-0
				tclvalue(pressButRect)<<-0
				tclvalue(pressButDrag)<<-0
				tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
				tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
				tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
				tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
				tkconfigure(W,cursor='crosshair')
			}else{
				ZoomXYval<<-c(xmin1,xmax1,ymin1,ymax1)
				tclvalue(xx1)<<-round(xmin1,4)
				tclvalue(xx2)<<-round(xmax1,4)
				tclvalue(yy1)<<-round(ymin1,4)
				tclvalue(yy2)<<-round(ymax1,4)
				refreshPlot1(W,img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
			}
		}

		##Zoom rectangle
		if(tclvalue(pressButRect)=="1"  & !ret$oin){
			pPressRect(W,x, y,width=1, outline="red")
			rectZoomInit[1]<<-ret$xc
			rectZoomInit[3]<<-ret$yc
		}

		##Pan image
		if(tclvalue(pressButDrag)=="1"  & !ret$oin){
			panZoomInit[1]<<-ret$xc
			panZoomInit[2]<<-ret$yc
			panZoomInit[3]<<-as.numeric(tclvalue(xx1))
			panZoomInit[4]<<-as.numeric(tclvalue(xx2))
			panZoomInit[5]<<-as.numeric(tclvalue(yy1))
			panZoomInit[6]<<-as.numeric(tclvalue(yy2))
			tkconfigure(canvas,cursor='hand2')
		}
	})


	##########
	tkbind(canvas, "<Motion>", function(W,x,y){
		ret<-getXYCoords(W,x,y,parPltCrd)
		displayCursorPosition3Var(W,x,y,parPltCrd,xpcoord,ypcoord,zpcoord,getStnIDLabel,pltusr=pltusr,inout=ret$oin)
	})

	##########
	tkbind(canvas, "<B1-Motion>",function(W,x,y){
		ret<-getXYCoords(W,x,y,parPltCrd)

		##Zoom rectangle
		if(tclvalue(pressButRect)=="1"){
			pMoveRect(W,x, y)
		}

		##Pan image
		if(tclvalue(pressButDrag)=="1"){
			transX<-ret$xc-panZoomInit[1]
			transY<-ret$yc-panZoomInit[2]
			tclvalue(xx1)<<-round(panZoomInit[3]+factPan*transX,4)
			tclvalue(xx2)<<-round(panZoomInit[4]+factPan*transX,4)
			tclvalue(yy1)<<-round(panZoomInit[5]+factPan*transY,4)
			tclvalue(yy2)<<-round(panZoomInit[6]+factPan*transY,4)
			ZoomXYval<<-as.numeric(c(tclvalue(xx1),tclvalue(xx2),tclvalue(yy1),tclvalue(yy2)))
			refreshPlot1(W,img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
		}
	})


	#########
	tkbind(canvas, "<ButtonRelease>", function(W,x,y){
		ret<-getXYCoords(W,x,y,parPltCrd)

		##Zoom rectangle
		if(tclvalue(pressButRect)=="1"){
			rectZoomInit[2]<<-ret$xc
			rectZoomInit[4]<<-ret$yc
			if(rectZoomInit[1]>rectZoomInit[2]) rectZoomInit<-rectZoomInit[c(2,1,3,4)]
			if(rectZoomInit[3]>rectZoomInit[4]) rectZoomInit<-rectZoomInit[c(1,2,4,3)]
			ZoomXYval<<-rectZoomInit
			tclvalue(xx1)<<-round(rectZoomInit[1],4)
			tclvalue(xx2)<<-round(rectZoomInit[2],4)
			tclvalue(yy1)<<-round(rectZoomInit[3],4)
			tclvalue(yy2)<<-round(rectZoomInit[4],4)
			tkdelete(W,'rect')

			refreshPlot1(W,img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
		}

		##Pan image
		if(tclvalue(pressButDrag)=="1"){
			tkconfigure(canvas,cursor='hand1')
		}
	})

	###############################################

	tkbind(canvas, "<Button-3>", function(W){
		tclvalue(pressButP)<<-0
		tclvalue(pressButM)<<-0
		tclvalue(pressButRect)<<-0
		tclvalue(pressButDrag)<<-0
		tkconfigure(btZoomP.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomM.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btZoomRect.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(btPanImg.tab3,relief='raised',bg='lightblue',state='normal')
		tkconfigure(canvas,cursor='crosshair')
		tkdelete(W,'rect')

		refreshPlot1(W,img,hscale=as.numeric(tclvalue(tkget(spinH))), vscale=as.numeric(tclvalue(tkget(spinV))))
	})

	###
	return(list(onglet,list(canvas,img)))
}

