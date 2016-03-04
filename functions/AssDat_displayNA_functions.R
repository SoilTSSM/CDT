
######################################################################################################

plotNASummary<-function(dates,xseries,jstn,period){
	years<-as.numeric(substr(dates,1,4))
	yNonNA<-tapply(xseries,list(years),FUN=function(x) length(which(complete.cases(x))))
	taxis<-as.numeric(names(yNonNA))
	at_tick <- seq_len(length(taxis) + 1)
	at_lab <- seq_along(taxis) - 0.5
	yval<-as.vector(yNonNA)
	at_val<-at_lab[yval>0]
	yval<-yval[yval>0]
	if(period=='daily') ylim<-c(0,400)
	if(period=='dekadal') ylim<-c(0,40)
	if(period=='monthly') ylim<-c(0,14)

	opar<-par(mar=c(3.5,4,3,2))
	barplot(yNonNA,space = 0,ylab='Number of Non-Missing data/Year',ylim=ylim,col='cyan',xaxt='n',yaxt='n',main=jstn)
	axis(side = 2)
	axis(side = 1, at = at_tick - 1, labels = FALSE)
	axis(side = 1, at = at_lab , tick = FALSE, labels = taxis)
	if(length(yval)>0) text(at_val,yval,labels=yval,pos=3)
	abline(h=axTicks(2),col = "lightgray", lty = "dotted")
	box()
	plt <- par("plt")
	usr<-par("usr")
	par(opar)
	return(list(plt=plt,usr=usr,years=taxis))
}
######################################################################################################

plotAllNASummary<-function(donne,dates,period){
	if(period=='daily'){
		xdates<-as.Date(dates,format='%Y%m%d')
	}else if(period=='dekadal'){
		xdates<-as.Date(paste(substr(dates,1,6),ifelse(substr(dates,7,7)=="1","05",
		ifelse(substr(dates,7,7)=="2","15","25")),sep=''),format='%Y%m%d')
	}else if(period=='monthly'){
		xdates<-as.Date(paste(dates,16,sep=''),format='%Y%m%d')
	}

	stnTworking<-function(x){
		nonNA<-which(complete.cases(x))
		c(min(nonNA),max(nonNA))
	}

	nbstn<-ncol(donne)
	nlstn<-nrow(donne)
	vNonNA<-nbstn-apply(donne,1,function(x) sum(is.na(x)))
	tstn<-t(apply(donne,2,stnTworking))
	mat<-matrix(0,nrow=nlstn,ncol=nbstn)
	for(j in 1:nbstn) mat[tstn[j,1]:tstn[j,2],j]<-1
	sworking<-apply(mat,1,sum)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.1,1), respect = FALSE)
	op<-par(mar=c(0,0,0,0))
	plot.new()
	legend("center","groups",legend=c('Working Satitions','Non Missing Data'),fill=c('cyan1','pink'),horiz=TRUE)
	par(op)
	op<-par(mar=c(3.5,4,0,2))
	plot(xdates,sworking,xlab='',ylab='Number of stations',type='n')
	polygon(c(rev(xdates), xdates), c(rev(vNonNA), rep(0,nlstn)), col = 'pink', border = NA)
	polygon(c(rev(xdates), xdates), c(rev(sworking),vNonNA), col = 'cyan1', border = NA)
	abline(v = pretty(extendrange(xdates)),h = pretty(extendrange(sworking)),col = 'lightgrey', lty = "dotted")
	box()
	plt<-par("plt")
	usr<-par("usr")
	par(op)
	return(list(plt=plt,usr=usr))
}

######################################################################################################

funCorDist<-function(donne,latlon){
	lon<-latlon[,1]
	lat<-latlon[,2]
	#######

	test_equal<-function(x){
		x<-x[!is.na(x)]
		all(x == x[1])
	}
	###
	summaryDistCor<-function(x,y){
		nbreaks<-function(n) round(exp(0.4607)*n^0.6003)
		o<-order(x)
		x<-x[o]
		y<-y[o]
		#
		nbrk<-nbreaks(length(x))
		ibrk<-seq(0,length(x),nbrk)
		if(length(ibrk[length(ibrk)]:length(x))<nbrk/4){
			breaks<-c(0,x[ibrk[-c(1,length(ibrk))]],max(x))
		}else{
			breaks<-c(0,x[ibrk[-1]],max(x))
		}

		xquant<-tapply(y,list(cut(x,breaks=breaks)),quantile,prob=c(0.05,0.5,0.95))
		intrv<-names(xquant)
		intrv<-gsub('\\(|\\]|\\[','',intrv)
		Intrv<-matrix(as.numeric(do.call('rbind',strsplit(intrv,','))),ncol=2)
		mids<-(Intrv[,2]+Intrv[,1])/2
		xquant<-do.call('rbind',xquant)
		return(unname(cbind(mids,xquant)))
	}
	###
	fDistCor<-function(don,dist){
		corm<-suppressWarnings(cor(don,use='pairwise.complete.obs'))
		corm[lower.tri(corm,diag=TRUE)]<-NA
		dist[lower.tri(dist,diag=TRUE)]<-NA
		idn<-which(!is.na(corm))
		xcor<-corm[idn]
		xdst<-dist[idn]
		return(cbind(xdst,xcor))
	}
	###################

	id0<-apply(donne,2,test_equal)
	##
	infoIDen<-NULL
	if(sum(id0)>0){
		val<-sapply(which(id0),function(j){
			x<-donne[,j]
			x<-x[!is.na(x)]
			x[1]
		})

		infoIDen<-cbind(idStn[id0],lon[id0],lat[id0],val)
		lon<-lon[!id0]
		lat<-lat[!id0]
		idStn<-idStn[!id0]
		donne<-donne[!id0]
	}
	##
	coord<-matrix(c(lon,lat),ncol=2)
	dists<-rdist.earth(coord, miles=FALSE)
	###
	xdist<-fDistCor(donne,dists)
	x<-xdist[,1]
	y<-xdist[,2]
	###

	xysumm<-summaryDistCor(x,y)
	###
	loess<-loess.smooth(x, y)
	###
	op<-par(mar=c(4,4,2,2))
	plot(x,y,xlab="Distance (km)",ylab="Correlation",xlim=c(0,max(x,na.rm=T)),ylim=c(min(xysumm[,2],na.rm=T),
	ifelse(max(xysumm[,4],na.rm=T)<0.9,max(xysumm[,4],na.rm=T)+0.1,1)),type = 'n')
	polygon(c(rev(xysumm[,1]), xysumm[,1]), c(rev(xysumm[,4]), xysumm[,2]), col = "darkslategray1", border = NA)
	lines(xysumm[,1],xysumm[,3],col='blue',lwd=2)
	lines(loess$x,loess$y,lwd=2,col='red')
	abline(v = axTicks(1),h = axTicks(2),col = 'lightgrey', lty = "dotted")
	legend(x='topright',legend=c("5/95th Percentile","Median",'Loess smooth'),
	col=c('darkslategray1','blue','red'),lty = c(0,1,1),lwd=c(0,2,2),
	pch = c(22, NA, NA),pt.bg =c('darkslategray1',NA,NA),pt.cex = 2, bg='gray97')
	par(op)
}

######################################################################################################

DisplayStnNASum<-function(parent,jstn,donne,vdates,notebookTab){

	if(vdates=='Daily data'){
		period<-'daily'
		if(nchar(as.character(donne[5,1]))!=8){
			insert.txt(main.txt.out,'Station data: not a daily data',format=TRUE)
			return(NULL)
		}
	}
	if(vdates=='Dekadal data'){
		period<-'dekadal'
		if(nchar(as.character(donne[5,1]))!=7){
			insert.txt(main.txt.out,'Station data: not a dekadal data',format=TRUE)
			return(NULL)
		}
	}
	if(vdates=='Monthly data'){
		period<-'monthly'
		if(nchar(as.character(donne[5,1]))!=6){
			insert.txt(main.txt.out,'Station data: not a monthly data',format=TRUE)
			return(NULL)
		}
	}

	if(length(grep('alt|elev|elv',donne[4,1],ignore.case=TRUE))==1){
		Info<-data.frame(t(donne[1:4,-1]))
		dates<-as.character(donne[-c(1:4),1])
		donne<-donne[-c(1:4),-1]
	}else{
		Info<-data.frame(t(donne[1:3,-1]))
		dates<-as.character(donne[-c(1:3),1])
		donne<-donne[-c(1:3),-1]
	}

	IdStn<-as.character(Info[,1])
	ijx<-which(IdStn==jstn)
	xseries<-as.numeric(donne[,ijx])

	########PLOT
	pltusr <- NULL
	plotIt <- function(){
		op<-par(bg="white")
		pltusr <<-plotNASummary(dates,xseries,jstn,period)
		par(op)
	}

	#########
	onglet<-imageNotebookTab_open(parent,notebookTab,tabTitle=paste(jstn,'Miss.Summary',sep='-'),tab.type,tab.data)
	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

	img<-tkrplot(onglet[[2]],fun=plotIt,hscale=hscale, vscale=vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img,0,weight=1)
	tkgrid.columnconfigure(img,0,weight=1)
	tcl("update")
	parPlotSize<-pltusr$plt
	usrCoords<-pltusr$usr

	display.cursor.type<-function(x,y){
		xmouse<-as.numeric(x)
		ymouse<-as.numeric(y)

		imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", img)))
		imgh<- as.numeric(tclvalue(tkwinfo("reqheight", img)))

		imgmw<-as.numeric(tclvalue(tkwinfo("width", img)))
		imgmh<- as.numeric(tclvalue(tkwinfo("height", img)))

		posimgx<-round((imgmw-imgw)/2)
		posimgy<-round((imgmh-imgh)/2)
		orgx<-ifelse(posimgx<0,0,posimgx)
		orgy<-ifelse(posimgy<0,0,posimgy)

		xpos<-(xmouse-orgx)/imgw
		ypos<-1-(ymouse-orgy)/imgh

		xplt1 <- parPlotSize[1]
		xplt2 <- parPlotSize[2]
		yplt1 <- parPlotSize[3]
		yplt2 <- parPlotSize[4]

		minX<-usrCoords[1]
		rangeX<-usrCoords[2]-usrCoords[1]
		minY<-usrCoords[3]
		rangeY<-usrCoords[4]-usrCoords[3]

		xcoord<- minX+(xpos-xplt1)*rangeX/(xplt2-xplt1)
		ycoord<- minY+(ypos-yplt1)*rangeY/(yplt2-yplt1)

		ipos<-ceiling(xcoord)
		labdates<-pltusr$years[ipos]

		frxcoord<-ifelse(ipos<1 | ipos>length(pltusr$years) | ycoord<usrCoords[3] | ycoord>usrCoords[4],'',labdates)
		frycoord<-ifelse(xcoord<usrCoords[1] | xcoord>usrCoords[2] | ycoord<usrCoords[3] | ycoord>usrCoords[4],'',round(ycoord,1))

		tclvalue(xpcoord)<-frxcoord
		tclvalue(ypcoord)<-frycoord
	}

	tkbind(img,"<Enter>",function() tkconfigure(img,cursor='crosshair'))
	tkbind(img,"<Leave>",function() tkconfigure(img,cursor=''))
	tkbind(img, "<Motion>", function(x,y){
		display.cursor.type(x,y)
	})

	return(list(onglet,img))
}

######################################################################################################


DisplayAllStnNASum<-function(parent,donne,vdates){

	if(vdates=='Daily data'){
		period<-'daily'
		if(nchar(as.character(donne[5,1]))!=8){
			insert.txt(main.txt.out,'Station data: not a daily data',format=TRUE)
			return(NULL)
		}
	}
	if(vdates=='Dekadal data'){
		period<-'dekadal'
		if(nchar(as.character(donne[5,1]))!=7){
			insert.txt(main.txt.out,'Station data: not a dekadal data',format=TRUE)
			return(NULL)
		}
	}
	if(vdates=='Monthly data'){
		period<-'monthly'
		if(nchar(as.character(donne[5,1]))!=6){
			insert.txt(main.txt.out,'Station data: not a monthly data',format=TRUE)
			return(NULL)
		}
	}

	if(length(grep('alt|elev|elv',donne[4,1],ignore.case=TRUE))==1){
		Info<-data.frame(t(donne[1:4,-1]))
		dates<-as.character(donne[-c(1:4),1])
		donne<-donne[-c(1:4),-1]
	}else{
		Info<-data.frame(t(donne[1:3,-1]))
		dates<-as.character(donne[-c(1:3),1])
		donne<-donne[-c(1:3),-1]
	}

	donne<-apply(donne,2,as.numeric)
	nna<-apply(donne,2,function(x) sum(is.na(x)))
	donne<-donne[,nna!=nrow(donne)]

	########PLOT
	pltusr <- NULL
	plotIt <- function(){
		op<-par(bg="white")
		pltusr <<-plotAllNASummary(donne,dates,period)
		par(op)
	}

	#########
	onglet<-addNewTab(parent,tab.title='Missing data Summary')
	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

	img<-tkrplot(onglet[[2]],fun=plotIt,hscale=hscale, vscale=vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img,0,weight=1)
	tkgrid.columnconfigure(img,0,weight=1)
	tcl("update")
	parPlotSize<-pltusr$plt
	usrCoords<-pltusr$usr

	display.cursor.type<-function(x,y){
		xmouse<-as.numeric(x)
		ymouse<-as.numeric(y)

		imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", img)))
		imgh<- as.numeric(tclvalue(tkwinfo("reqheight", img)))

		imgmw<-as.numeric(tclvalue(tkwinfo("width", img)))
		imgmh<- as.numeric(tclvalue(tkwinfo("height", img)))

		posimgx<-round((imgmw-imgw)/2)
		posimgy<-round((imgmh-imgh)/2)
		orgx<-ifelse(posimgx<0,0,posimgx)
		orgy<-ifelse(posimgy<0,0,posimgy)

		xpos<-(xmouse-orgx)/imgw
		ypos<-1-(ymouse-orgy)/imgh

		xplt1 <- parPlotSize[1]
		xplt2 <- parPlotSize[2]
		yplt1 <- parPlotSize[3]
		yplt2 <- parPlotSize[4]

		minX<-usrCoords[1]
		rangeX<-usrCoords[2]-usrCoords[1]
		minY<-usrCoords[3]
		rangeY<-usrCoords[4]-usrCoords[3]

		xcoord<- minX+(xpos-xplt1)*rangeX/(xplt2-xplt1)
		ypos1<-(ypos-0)/((1/(0.1+1))-0)
		ycoord<- minY+(ypos1-yplt1)*rangeY/(yplt2-yplt1)

		if(period=='daily') labdates<-format(as.Date(xcoord,origin='1970-1-1'),'%d-%b-%Y')
		if(period=='dekadal'){
			dtdk<-as.Date(xcoord,origin='1970-1-1')
			dek<-ifelse(as.numeric(format(dtdk,'%d'))<=10,1,ifelse(as.numeric(format(dtdk,'%d'))>20,3,2))
			moyr<-format(dtdk,'%b-%Y')
			labdates<-paste(dek,moyr,sep='-')
		}
		if(period=='monthly') labdates<- format(as.Date(xcoord,origin='1970-1-1'),'%b-%Y')

		frxcoord<-ifelse(xcoord<usrCoords[1] | xcoord>usrCoords[2] | ycoord<usrCoords[3] | ycoord>usrCoords[4],'',labdates)
		frycoord<-ifelse(xcoord<usrCoords[1] | xcoord>usrCoords[2] | ycoord<usrCoords[3] | ycoord>usrCoords[4],'',round(ycoord,1))

		tclvalue(xpcoord)<-frxcoord
		tclvalue(ypcoord)<-frycoord
	}

	tkbind(img,"<Enter>",function() tkconfigure(img,cursor='crosshair'))
	tkbind(img,"<Leave>",function() tkconfigure(img,cursor=''))
	tkbind(img, "<Motion>", function(x,y){
		display.cursor.type(x,y)
	})

	return(list(onglet,img))
}

######################################################################################################

DisplayDistCorr<-function(parent,donne){

	if(length(grep('alt|elev|elv',donne[4,1],ignore.case=TRUE))==1){
		Info<-data.frame(t(donne[1:4,-1]))
		dates<-as.character(donne[-c(1:4),1])
		donne<-donne[-c(1:4),-1]
	}else{
		Info<-data.frame(t(donne[1:3,-1]))
		dates<-as.character(donne[-c(1:3),1])
		donne<-donne[-c(1:3),-1]
	}

	latlon<-apply(Info[,2:3],2,as.numeric)
	donne<-apply(donne,2,as.numeric)
	nna<-apply(donne,2,function(x) sum(is.na(x)))
	latlon<-latlon[nna!=nrow(donne),]
	donne<-donne[,nna!=nrow(donne)]

	########PLOT
	plotIt <- function(){
		op<-par(bg="white")
		funCorDist(donne,latlon)
		par(op)
	}

	onglet<-addNewTab(parent,tab.title='Distance_Corrorelation')
	#########
	hscale<-as.numeric(tclvalue(tkget(spinH)))
	vscale<-as.numeric(tclvalue(tkget(spinV)))

	img<-tkrplot(onglet[[2]],fun=plotIt,hscale=hscale, vscale=vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img,0,weight=1)
	tkgrid.columnconfigure(img,0,weight=1)

	tkbind(img,"<Enter>",function() tkconfigure(img,cursor='crosshair'))
	tkbind(img,"<Leave>",function() tkconfigure(img,cursor=''))
	return(list(onglet,img))
}

