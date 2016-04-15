plotGGvsSatellite<-function(outValiddata,dataType){
	if(tclvalue(dataType)=='All Data'){
		x<-outValiddata$x
		y<-outValiddata$y
	}else{
		x<-outValiddata$xs
		y<-outValiddata$ys
	}
	plot(x,y,xlab="Gauge",ylab="RFE")
	abline(a=0, b=1, lwd=2,col='red')
}


###############################
displayGGvsSatFun<-function(parent,notebookTab,outValiddata,dataType){

	plotIt <- function(){
		plotGGvsSatellite(outValiddata,dataType)
	}

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle='Gauge-RFE',tab.type,tab.data)

	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

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


######################################################
cdfGGvsSatellite<-function(outValiddata,dataType){
	if(tclvalue(dataType)=='All Data'){
		x<-outValiddata$x
		y<-outValiddata$y
	}else{
		x<-outValiddata$xs
		y<-outValiddata$ys
	}
	plot(ecdf(x),xlab="Rainfall [mm]",main='CDF',col='blue',lwd=2,cex=0.4,ylim=c(0,1))
	plot(ecdf(y),add=T, col="red",lwd=2,cex=0.4)
	legend('bottomright',c('Gauge','RFE'),col=c('blue','red'),lwd=3,bg='lightgray')
}


###############################
displayCDFGGvsSatFun<-function(parent,notebookTab,outValiddata,dataType){

	plotIt <- function(){
		cdfGGvsSatellite(outValiddata,dataType)
	}

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle='CDF Gauge-RFE',tab.type,tab.data)

	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

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


