plotGGvsSatellite<-function(outValiddata){
	plot(outValiddata$x,outValiddata$y,xlab="Gauge",ylab="Satellite")
	abline(a=0, b=1, lwd=2)
}


###############################
displayGGvsSatFun<-function(parent,notebookTab,outValiddata){

	plotIt <- function(){
		plotGGvsSatellite(outValiddata)
	}

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle='Gauge-Satellite',tab.type,tab.data)

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
cdfGGvsSatellite<-function(outValiddata){
	plot(ecdf(outValiddata$x),xlab="Rainfall [mm]",main='CDF',col='blue',lwd=3)
	plot(ecdf(outValiddata$y),add=T, col="red",lwd=1,cex=0.4)
	legend('bottomright',c('Gauge','Satellite'),col=c('blue','red'),lwd=3,bg='lightgray')
}


###############################
displayCDFGGvsSatFun<-function(parent,notebookTab,outValiddata){

	plotIt <- function(){
		cdfGGvsSatellite(outValiddata)
	}

	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle='CDF Gauge-Satellite',tab.type,tab.data)

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


