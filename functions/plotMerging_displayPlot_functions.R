plotMergingOutData<-function(allDATA,atLev,listCol,ocrds,units){
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
	toutLon<-c(ocrds[,1],unlist(lapply(allDATA,function(v) v[[1]]$x)))
	toutLat<-c(ocrds[,2],unlist(lapply(allDATA,function(v) v[[1]]$y)))
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
	Xaxis=list(relation="same",draw=T,alternating=c(1,2),at=axis.x,labels=axlabs$xaxl,tck=c(1,1))
	###Y-axis
	Yaxis=list(relation="same",draw=T,alternating=c(1,2),at=axis.y,labels=axlabs$yaxl,tck=c(1,1))

	#########################################
	donStn<-allDATA[[1]][[1]]
	plotStn<-levelplot(donStn$value~donStn$x+donStn$y,at=ticks,
	prepanel=prepanel.default.xyplot,
	panel = function(x,y,z,...){
		panel.lines(ocrds,col="gray",lwd=0.5)
		panel.abline(h=grid.y, v=grid.x , col="lightgray",lty=3)
		panel.levelplot.points(x,y,z,type ='p',cex=0.9,...)
	},colorkey = FALSE)

	PlotObj0<-plotStn
	####
	donNetCDF<-allDATA[-1]
	ijc<-sapply(donNetCDF,function(x) !is.null(x[[1]]))
	donNetCDF<-donNetCDF[ijc]
	donNcdf2Plot<-lapply(donNetCDF,function(x) x[[1]])

	for(jj in 1:length(donNcdf2Plot)){
		xydon<-donNcdf2Plot[[jj]]
		donne1<-data.frame(expand.grid(x=xydon$x,y=xydon$y),z=c(xydon$value))
#		plotXYdon<-levelplot(xydon$value,row.values=xydon$x,column.values=xydon$y,at=ticks,
		plotXYdon<-levelplot(z~x+y,data=donne1,at=ticks,
		interpolate = TRUE,region = TRUE,
		panel = function(...){
			panel.levelplot(...)
			panel.lines(ocrds,col="gray",lwd=0.5)
			panel.abline(h=grid.y, v=grid.x , col="lightgray",lty=3)
		},colorkey = FALSE)
		PlotObj0<-c(PlotObj0,plotXYdon)
	}

	###############
	nbPlot<-length(donNetCDF)+1
	
	panelTitle<-c(allDATA[[1]][[2]],sapply(donNetCDF,function(x) x[[2]]))
	LayoutObj<-manageLayout(nbPlot,transpose=FALSE)
	PlotObj<-c(PlotObj0,layout=LayoutObj$dim)

#cat(nbPlot,LayoutObj$dim,'\n')
	###################
	###Colorkey position
	if(diff(xlim)*LayoutObj$dim[1]>=diff(ylim)*LayoutObj$dim[2]){
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
	
	##par.strip.text
	parStripText<-list(cex=0.8,col='black',font=2)
	##strip
	stripCust<-strip.custom(factor.levels=panelTitle,bg='lightblue')

	print(update(PlotObj,aspect='fill',as.table=TRUE,par.settings=parSettings,
	xlab='',ylab='',xlim=xlim,ylim=ylim,col.regions=loko,
	par.strip.text=parStripText,strip=stripCust,
	scales = list(x=Xaxis,y=Yaxis),legend=lezandy))

}


#########################################################################################################################

displayPlotMerging<-function(parent,notebookTab,allDATA,atLev,listCol,shpf,units){

	if(is.null(allDATA[[1]][[1]]$x)){
		insert.txt(main.txt.out,'No Station data found',format=TRUE)
		return(NULL)
	}

	if(sum(sapply(allDATA[-1],function(x) !is.null(x[[1]])))==0){
		insert.txt(main.txt.out,'Need at least one NetCDF data',format=TRUE)
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
		plotMergingOutData(allDATA,atLev,listCol,ocrds,units)
	}
	
	###################################################################	

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle=getf.no.ext(allDATA[[1]][[3]]),tab.type,tab.data)

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
	
	img<-tkrplot1(containerFrame,fun=plotIt,hscale=hscale, vscale=vscale)
	tkgrid(img)
	
	return(list(onglet,img))
}




#########################################################################################################################
# displayPlotMerging<-function(parent,notebookTab){
	
	# plotIt <- function(){
		# op<-par(bg='white')
		# plot(1:20)
		# par(op)
	# }
	
	# ###################################################################	

	# onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle=' Map ',tab.type,tab.data)

	# # hscrCan<-as.integer(tclvalue(tkwinfo("height", onglet[[1]])))
	# # wscrCan<-as.integer(tclvalue(tkwinfo("width", onglet[[1]])))

	# hscrCan<-as.integer(tclvalue(tkwinfo("height", panel.right)))
	# wscrCan<-as.integer(tclvalue(tkwinfo("width", panel.right)))

	# # hscale<-as.numeric(tclvalue(tkget(spinH)))
	# # vscale<-as.numeric(tclvalue(tkget(spinV)))
	# hscale<-1
	# vscale<-1
	# scrollwin<-bwScrolledWindow(onglet[[2]])
	# tkgrid(scrollwin)
	# tkgrid.rowconfigure(scrollwin,0,weight=1)
	# tkgrid.columnconfigure(scrollwin,0,weight=1)

	# canvas<-ScrollCanvas(scrollwin,width=wscrCan,height=hscrCan)
	# tkgrid(canvas)

	# img<-tkrplot1(canvas,fun=plotIt,hscale=hscale, vscale=vscale)
	# img_w<-as.double(tcl('image','width',img$image))
	# img_h<-as.double(tcl('image','height',img$image))

	# xcan<-(wscrCan-img_w)/2
	# ycan<-(hscrCan-img_h)/2

	# tkcreate(canvas, "image", xcan, ycan,anchor='nw',image=img$image,tag='aho')
	# #tkaddtag(canvas, "image", "withtag", img)
	# setScrollCanvas(canvas,width=wscrCan,height=hscrCan)
	# #setScrollCanvas1(canvas)
	
	# ######
	# tkbind(canvas,"<Enter>",function(){
		# tkconfigure(canvas,cursor='crosshair')
	# })

	# tkbind(canvas,"<Leave>",function() tkconfigure(canvas,cursor=''))
	# ####
	
	# #tkitembind(canvas, "image", "<Any-Enter>", function() tkitemconfigure(canvas, "current", fill = "red"))
	# #tkitembind(canvas, "image", "<Any-Leave>", function() tkitemconfigure(canvas, "current", fill = "blue"))

	# ############
	# return(list(onglet,list(canvas,img)))
# }
