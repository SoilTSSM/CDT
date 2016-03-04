
plotCDTdata<-function(donne,atLev,listCol,ocrds,units){
	##color and tick
	loko <- colorRampPalette(listCol)
	ticks<-as.numeric(atLev)
	nticks<-length(ticks)
	labticks<-ticks  #paste(ticks,expression(paste(degree,"C",sep=''))) 
	units<-str_trim(units)

	if(!is.na(units)){
		 if(units!="") colorkeyTitle<-paste('(',units,')',sep='')
		 else colorkeyTitle<-''
	} else colorkeyTitle<-''
	
	##X-Y Axis
	toutLon<-c(ocrds[,1],donne$lon)
	toutLat<-c(ocrds[,2],donne$lat)
	parLon<-parAxisPlotFun(toutLon)
	parLat<-parAxisPlotFun(toutLat)
	grid.x<-parLon$axp
	grid.y<-parLat$axp
	axis.x<-grid.x
	axis.y<-grid.y
	xlim<-parLon$usr
	ylim<-parLat$usr

	##Axis lab
	axlabs<-LatLonAxisLabels(axis.x,axis.y)
	##X-axis
	Xaxis=list(relation="same",draw=T,alternating=1,at=axis.x,labels=axlabs$xaxl,tck=c(1,0))
	###Y-axis
	Yaxis=list(relation="same",draw=T,alternating=1,at=axis.y,labels=axlabs$yaxl,tck=c(1,0))

	###Colorkey position
	if(diff(xlim)>=diff(ylim)){
		colorkeyPlace<-'bottom' 
	}else{
		colorkeyPlace<-'right'
	}

	### Colorkey option
	if(colorkeyPlace=='bottom'){
		layout.pad<-c(1,1,1,2) #left, right, top, bottom
		posTitle<-'right'
		xyposTitle<-c(1,0.2)
		justTitle<-c("center","center")
		rotTitle<-0 
	}else if(colorkeyPlace=='right'){
		layout.pad<-c(1,2,1,1)
		posTitle<-'top'
		xyposTitle<-c(1,1.5)
		justTitle<-c("right","center")
		rotTitle<-0
	}

	##par.settings
	parSettings<-list(background=list(alpha=1,col='white'),
	layout.widths=list(left.padding=layout.pad[1], right.padding=layout.pad[2]),
	layout.heights=list(top.padding=layout.pad[3], bottom.padding=layout.pad[4]))

	##Colorkey
	colorkey<-list(space=colorkeyPlace,col=loko, width=1.5,height=1,raster=TRUE,interpolate=TRUE, 
		at=1:nticks,labels=list(labels=labticks,at=1:nticks,cex=0.8,col='black',rot=0),
		axis.line=list(alpha=0.5,lty=1,lwd=1,col='black'))
	colorkeyFrame<-draw.colorkey(key=colorkey,draw=FALSE,vp=NULL)
	grobObj<-textGrob(colorkeyTitle,x=xyposTitle[1],y=xyposTitle[2],just=justTitle,rot=rotTitle,
		gp=gpar(fontsize=12,fontface='plain',col="black",cex=0.8))
	
	##add legend title
	lezandyGrob<-packGrob(frame=colorkeyFrame,grob=grobObj,side=posTitle,dynamic=T)

	##legend function
	if(colorkeyPlace=='bottom'){
		lezandy<-list(bottom=list(fun=lezandyGrob))
	}else if(colorkeyPlace=='right'){
		lezandy<-list(right=list(fun=lezandyGrob))
	}
		
	plotStn<-levelplot(z~lon+lat,data=donne,at=ticks,
	prepanel=prepanel.default.xyplot,
	panel = function(x,y,z,...){
		panel.lines(ocrds,col="gray",lwd=0.5)
		panel.abline(h = grid.y, v =grid.x , col = "lightgray",lty=3)
		panel.levelplot.points(x,y,z,type ="p",cex=1,pch=21,...)
	},colorkey = FALSE,
	par.settings=parSettings,
	xlab='',ylab='',xlim=xlim,ylim=ylim,col.regions=loko,
	scales = list(x=Xaxis,y=Yaxis),legend=lezandy)

	print(plotStn)	
}


####################################################################################

displayCDTdata<-function(parent,notebookTab,donne,atLev,listCol,shpf,units){
	if(is.null(donne)){
		insert.txt(main.txt.out,'No station data found',format=TRUE)
		return(NULL)
	}
	atLev<-as.numeric(atLev)
	atLev<-atLev[!is.na(atLev)]
	if(length(atLev)<2){
		insert.txt(main.txt.out,'Levels must be at least 2',format=TRUE)
		return(NULL)
	}
	
	ocrds<-getBoundaries(shpf)
	
	plotIt <- function(){
		plotCDTdata(donne,atLev,listCol,ocrds,units)
	}
	
	###################################################################	

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle=paste('Map -',donne$date),tab.type,tab.data)

	# area_plot_horiz<-as.numeric(tclvalue(tkget(spinH)))
	# area_plot_verti<-as.numeric(tclvalue(tkget(spinV)))
	# par_usr_horiz<-getParUsrPlot(c(donne$lon,ocrds[,1]))
	# par_usr_verti<-getParUsrPlot(c(donne$lat,ocrds[,2]))
	# ratio_plot_horiz<-diff(par_usr_horiz)/diff(par_usr_verti)
	# ratio_plot_verti<-diff(par_usr_verti)/diff(par_usr_horiz)
	# if(ratio_plot_horiz<=1){
		# hscale<-round(area_plot_horiz*ratio_plot_horiz,1)
		# vscale<-round(area_plot_verti,1)
	# }else{
		# if(ratio_plot_verti>=1){
			# hscale<-round(area_plot_horiz,1)
			# vscale<-round(area_plot_verti*ratio_plot_verti,1)
		# }else{
			# hscale<-round(area_plot_horiz,1)
			# vscale<-round(area_plot_verti,1)
		# }
	# }
	# tkset(spinH,hscale)
	# tkset(spinV,vscale)

	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))
	
	# hscrFrame<-as.integer(tclvalue(tkwinfo("height", onglet[[1]])))
	# wscrFrame<-as.integer(tclvalue(tkwinfo("width", onglet[[1]])))
	hscrFrame<-as.integer(tclvalue(tkwinfo("height", panel.right)))
	wscrFrame<-as.integer(tclvalue(tkwinfo("width", panel.right)))
	
	scrollwin<-bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin,0,weight=1)
	tkgrid.columnconfigure(scrollwin,0,weight=1)
	containerFrame<-bwScrollableFrame(scrollwin,width=wscrFrame,height=hscrFrame)
	
	img<-tkrplot(containerFrame,fun=plotIt,hscale=hscale, vscale=vscale)
	tkgrid(img)
	
	return(list(onglet,img))
}
