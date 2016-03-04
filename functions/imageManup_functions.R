
######################################
#redifine tkrreplot
refreshPlot<-function(W,img,hscale,vscale){
	tkrreplot(img,hscale=hscale, vscale=vscale)
	img_w<-as.double(tcl('image','width',img$image))
	img_h<-as.double(tcl('image','height',img$image))
	tkconfigure(W,width=img_w,height=img_h)
}

refreshPlot1<-function(W,img,hscale,vscale){
	tkrreplot1(img,hscale=hscale, vscale=vscale)
	img_w<-as.double(tcl('image','width',img$image))
	img_h<-as.double(tcl('image','height',img$image))
	tkconfigure(W,width=img_w,height=img_h)
}

######################################
#Draw rectangle above image
## 'rect' the tagged rectangle

#####
pPressRect <- function(W,x, y,...){
	x <- as.numeric(x)
	y <- as.numeric(y)
	tkdelete(W,'rect')
	tkcreate(W, "rectangle", x, y, x, y,tag='rect',...)
	lastX <<- x
	lastY <<- y
}

#####
pMoveRect <- function(W,x,y){
	x <- as.numeric(x)
	y <- as.numeric(y)

	if(x < lastX){
		x1<-x
		x2<-lastX
	}else{
		x1<-lastX
		x2<-x
	}
	if(y < lastY){
		y1<-y
		y2<-lastY
	}else{
		y1<-lastY
		y2<-y
	}
	tkcoords(W,'rect',x1,y1,x2,y2)
}

#####
pReleaseRect<-function(W,rectCrds){
	tclvalue(rectCrds)<-tclvalue(tkcoords(W,'rect'))
	#tkdelete(W,'rect')
}


######################################
#mouvement de souris
#Mouse.Mouvment <=old

mouseMouvment<- function(W,x,y,parPltCrd) {
	xmouse<-as.numeric(x)
	ymouse<-as.numeric(y)

	###compense pad sur x et y
	xmouse<-xmouse+2
	ymouse<-ymouse+2

	imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", W)))
	imgh<- as.numeric(tclvalue(tkwinfo("reqheight", W)))
	imgmw<-as.numeric(tclvalue(tkwinfo("width", W)))
	imgmh<- as.numeric(tclvalue(tkwinfo("height", W)))
	posimgx<-round((imgmw-imgw)/2)
	posimgy<-round((imgmh-imgh)/2)
	orgx<-ifelse(posimgx<0,0,posimgx)
	orgy<-ifelse(posimgy<0,0,posimgy)

	xpos<-(xmouse-orgx)/imgw
	ypos<-1-(ymouse-orgy)/imgh

	xplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize1))
	xplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize2))
	yplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize3))
	yplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize4))
	usrcrd1<-as.numeric(tclvalue(parPltCrd$usrCoords1))
	usrcrd2<-as.numeric(tclvalue(parPltCrd$usrCoords2))
	usrcrd3<-as.numeric(tclvalue(parPltCrd$usrCoords3))
	usrcrd4<-as.numeric(tclvalue(parPltCrd$usrCoords4))

	minX<-usrcrd1
	rangeX<-usrcrd2-usrcrd1
	minY<-usrcrd3
	rangeY<-usrcrd4-usrcrd3

	xcoord<- minX+(xpos-xplt1)*rangeX/(xplt2-xplt1)
	ycoord<- minY+(ypos-yplt1)*rangeY/(yplt2-yplt1)
	outsideArea<- xcoord<usrcrd1 | xcoord>usrcrd2 | ycoord<usrcrd3 | ycoord>usrcrd4

	return(list(x=xcoord,y=ycoord,inout=outsideArea))
}

######################################
#get coordinates
#get.xyCoords <=old

getXYCoords <- function(W,x,y,parPltCrd) {
	xyMouse<-mouseMouvment(W,x,y,parPltCrd)
	return(list(xc=xyMouse$x,yc=xyMouse$y,oin=xyMouse$inout))
}

######################################
##Display coordinates on status bar
##display.cursor.type <=old

displayCursorPosition3Var<-function(W,x,y,parPltCrd,xpcrd,ypcrd,zpcrd,FUN,...){
	xyMouse<-mouseMouvment(W,x,y,parPltCrd)
	xcoord<- xyMouse$x
	ycoord<- xyMouse$y
	outsideArea<- xyMouse$inout

	xydisp<-LatLonLabels(xcoord,ycoord)
	frxcoord<-ifelse(outsideArea,'',xydisp$xdisp)
	frycoord<-ifelse(outsideArea,'',xydisp$ydisp)
	FUN<-match.fun(FUN)
	frzcoord<-FUN(xcoord,ycoord,...)

	tclvalue(xpcrd)<-frxcoord
	tclvalue(ypcrd)<-frycoord
	tclvalue(zpcrd)<-frzcoord
}

####
##spatial check
##displayCursorPosition3Var(W,x,y,parPltCrd,xpcoord,ypcoord,zpcoord,getStnIDLabel,pltusr=pltusr,inout=outsideArea)

##extract gridded NetCDF Data
##displayCursorPosition3Var(W,x,y,parPltCrd,xpcoord,ypcoord,zpcoord,getAdminLabel,shp=shpf,idField=adminVar.tab1)


######################################
#get name of polygon @x,y position

getAdminLabel<-function(xx,yy,shp,idField){
	xypts<-data.frame(x=xx,y=yy)
	coordinates(xypts)=~x+y
	admin_name<-over(xypts,shp)
	admin_name<-c(t(admin_name[1,]))

	if(tclvalue(tkwinfo('exists',idField$ID))=="1"){
		admin_name<-admin_name[as.numeric(tclvalue(tcl(idField,'current')))+1]
		labAdmin_name<-ifelse(is.na(admin_name),'',as.character(admin_name))
	}else labAdmin_name<-''
	return(labAdmin_name)
}

######################################
#get the name of sations id (spatial check)

getStnIDLabel<-function(xx,yy,pltusr,inout){
	#si le pointer se trouve dans ce rayon afficher l'id de la station
	fdispIdStn<-function(x){
		if(x<=2) y<-0.0006944444*x
		else y<-0.002777778
		return(y)
	}

	lon<-pltusr$lon
	lat<-pltusr$lat
	idStn<-pltusr$idStn
	usrcrd1<-as.numeric(pltusr$usr[1])
	usrcrd2<-as.numeric(pltusr$usr[2])

	sdist<-(xx-lon)^2+(yy-lat)^2
	inear<-which.min(sdist)
	rayondisp<-sdist[inear]>fdispIdStn(usrcrd2-usrcrd1)
	labstn<-ifelse(inout | rayondisp,'',idStn[inear])
	return(labstn)
}


