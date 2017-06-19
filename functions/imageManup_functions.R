
##tkrplot for Windows

CountImgIndex <- local({
	k <- 0
	function() {
		k <<- k + 1
		return(k)
	}
})

tkrplot.win <- function(parent, fun, hscale = 1, vscale = 1) {
	image <- paste("RImage", CountImgIndex(), sep = "")
	tmpfl <- paste(tempfile(), '.jpg', sep = '')
	jpeg(tmpfl, width = 385*hscale, height = 385*vscale, quality = 100, restoreConsole = FALSE)
	try(fun())
	dev.off()
	tcl('image', 'create', 'photo', image, file = tmpfl)
	unlink(tmpfl)
	lab <- tklabel(parent, image = image)
	tkbind(lab, "<Destroy>", function() tcl('image', 'delete', image))
	lab$image <- image
	lab$fun <- fun
	lab$hscale <- hscale
	lab$vscale <- vscale
	lab
}

tkrreplot.win <- function(lab, fun = lab$fun, hscale = lab$hscale, vscale = lab$vscale) {
	tmpfl <- paste(tempfile(), '.jpg', sep = '')
	jpeg(tmpfl, width = 385*hscale, height = 385*vscale, quality = 100, restoreConsole = FALSE)
	try(fun())
	dev.off()
	tcl('image', 'create', 'photo', lab$image, file = tmpfl)
	unlink(tmpfl)
}

#########
tkrplot1 <- function(...){
	if (Sys.info()["sysname"] == "Windows") tkrplot.win(...) 
	else tkrplot(...)
}

tkrreplot1 <- function(...){
	if (Sys.info()["sysname"] == "Windows") tkrreplot.win(...)
	else tkrreplot(...)
}


######################################
#redifine tkrreplot
## Linux & mac only
refreshPlot <- function(W, img, hscale, vscale){
	tkrreplot(img, hscale = hscale, vscale = vscale)
	img_w <- as.double(tcl('image', 'width', img$image))
	img_h <- as.double(tcl('image', 'height', img$image))
	tkconfigure(W, width = img_w, height = img_h)
}

## All
refreshPlot1 <- function(W, img, hscale, vscale){
	tkrreplot1(img, hscale = hscale, vscale = vscale)
	img_w <- as.double(tcl('image', 'width', img$image))
	img_h <- as.double(tcl('image', 'height', img$image))
	tkconfigure(W, width = img_w, height = img_h)
	tcl('update')
}

##################################################################################
#Draw rectangle above image
## 'rect' the tagged rectangle

#####
pPressRect <- function(W, x, y, ...){
	x <- as.numeric(x)
	y <- as.numeric(y)
	tkdelete(W, 'rect')
	tkcreate(W, "rectangle", x, y, x, y, tag = 'rect', ...)
	lastX <<- x
	lastY <<- y
}

#####
pMoveRect <- function(W, x, y){
	x <- as.numeric(x)
	y <- as.numeric(y)

	if(x < lastX){
		x1 <- x
		x2 <- lastX
	}else{
		x1 <- lastX
		x2 <- x
	}
	if(y < lastY){
		y1 <- y
		y2 <- lastY
	}else{
		y1 <- lastY
		y2 <- y
	}
	tkcoords(W, 'rect', x1, y1, x2, y2)
}

#####
pReleaseRect <- function(W, rectCrds){
	tclvalue(rectCrds) <- tclvalue(tkcoords(W, 'rect'))
	#tkdelete(W, 'rect')
}


##################################################################################
#mouvement de souris
#Mouse.Mouvment <= old

mouseMouvment <- function(W, x, y, parPltCrd, xdiv = c(0, 1), ydiv = c(0, 1)) {
	xmouse <- as.numeric(x)
	ymouse <- as.numeric(y)

	###compense pad sur x et y
	xmouse <- xmouse+2
	ymouse <- ymouse+2

	imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", W)))
	imgh <- as.numeric(tclvalue(tkwinfo("reqheight", W)))
	imgmw <- as.numeric(tclvalue(tkwinfo("width", W)))
	imgmh <- as.numeric(tclvalue(tkwinfo("height", W)))
	posimgx <- round((imgmw-imgw)/2)
	posimgy <- round((imgmh-imgh)/2)
	orgx <- ifelse(posimgx < 0, 0, posimgx)
	orgy <- ifelse(posimgy < 0, 0, posimgy)

	xpos <- (xmouse-orgx)/imgw
	ypos <- 1-(ymouse-orgy)/imgh

	xplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize1))
	xplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize2))
	yplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize3))
	yplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize4))
	usrcrd1 <- as.numeric(tclvalue(parPltCrd$usrCoords1))
	usrcrd2 <- as.numeric(tclvalue(parPltCrd$usrCoords2))
	usrcrd3 <- as.numeric(tclvalue(parPltCrd$usrCoords3))
	usrcrd4 <- as.numeric(tclvalue(parPltCrd$usrCoords4))

	minX <- usrcrd1
	rangeX <- usrcrd2 - usrcrd1
	minY <- usrcrd3
	rangeY <- usrcrd4 - usrcrd3

	xposD <- (xpos - xdiv[1]) / (xdiv[2] - xdiv[1])
	xcoord <- minX + (xposD - xplt1) * rangeX / (xplt2 - xplt1)
	yposD <- (ypos - ydiv[1]) / (ydiv[2] - ydiv[1])
	ycoord <- minY + (yposD - yplt1) * rangeY / (yplt2 - yplt1)
	outsideArea <- xcoord < usrcrd1 | xcoord > usrcrd2 | ycoord < usrcrd3 | ycoord > usrcrd4

	return(list(x = xcoord, y = ycoord, inout = outsideArea, xym = list(x = xpos, y = ypos)))
}

##################################################################################
#get coordinates
#get.xyCoords <= old

getXYCoords <- function(W, x, y, parPltCrd) {
	xyMouse <- mouseMouvment(W, x, y, parPltCrd)
	return(list(xc = xyMouse$x, yc = xyMouse$y, oin = xyMouse$inout))
}

##################################################################################
##Display coordinates on status bar
##display.cursor.type <= old

displayCursorPosition3Var <- function(W, x, y, parPltCrd, xpcrd, ypcrd, zpcrd, FUN, ...){
	xyMouse <- mouseMouvment(W, x, y, parPltCrd)
	xcoord <- xyMouse$x
	ycoord <- xyMouse$y
	outsideArea <- xyMouse$inout

	xydisp <- LatLonLabels(xcoord, ycoord)
	frxcoord <- ifelse(outsideArea, '', xydisp$xdisp)
	frycoord <- ifelse(outsideArea, '', xydisp$ydisp)
	FUN <- match.fun(FUN)
	frzcoord <- FUN(xcoord, ycoord, ...)

	tclvalue(xpcrd) <- frxcoord
	tclvalue(ypcrd) <- frycoord
	tclvalue(zpcrd) <- frzcoord
}

####
##spatial check
##displayCursorPosition3Var(W, x, y, parPltCrd, xpcoord, ypcoord, zpcoord, getStnIDLabel, pltusr = pltusr, inout = outsideArea)

##extract gridded NetCDF Data
##displayCursorPosition3Var(W, x, y, parPltCrd, xpcoord, ypcoord, zpcoord, getAdminLabel, shp = shpf, idField = adminVar.tab1)


##################################################################################
#get name of polygon @x, y position

getAdminLabel <- function(xx, yy, shp, idField){
	xypts <- data.frame(x = xx, y = yy)
	coordinates(xypts) <- ~x+y
	admin_name <- over(xypts, shp)
	admin_name <- c(t(admin_name[1, ]))

	if(tclvalue(tkwinfo('exists', idField$ID)) == "1"){
		admin_name <- admin_name[as.numeric(tclvalue(tcl(idField, 'current')))+1]
		labAdmin_name <- ifelse(is.na(admin_name), '', as.character(admin_name))
	}else labAdmin_name <- ''
	return(labAdmin_name)
}

##################################################################################
#get the name of sations id (spatial check)

getStnIDLabel <- function(xx, yy, pltusr, inout){
	#si le pointer se trouve dans ce rayon afficher l'id de la station
	fdispIdStn <- function(x){
		y <- if(x <= 2) 0.0006944444 * x else 0.002777778
		return(y)
	}

	lon <- pltusr$lon
	lat <- pltusr$lat
	idStn <- pltusr$idStn
	usrcrd1 <- as.numeric(pltusr$usr[1])
	usrcrd2 <- as.numeric(pltusr$usr[2])

	sdist <- (xx-lon)^2+(yy-lat)^2
	inear <- which.min(sdist)
	rayondisp <- sdist[inear] > fdispIdStn(usrcrd2-usrcrd1)
	labstn <- ifelse(inout | rayondisp, '', idStn[inear])
	return(labstn)
}


##################################################################################
#Gradient Color

reScaleC <- function(x, newrange){
	xrange <- range(x)
	if(xrange[1] == xrange[2]) return(x)
	mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
	retScaled <- newrange[1]+(x-xrange[1])*mfac
	return(retScaled)
}

getGradientColor <- function(listCol, cW){
	ncolors <- length(cW)
	xrange <- range(cW)
	rgbC <- col2rgb(listCol)
	rouge <- rgbC[1, ]
	verte <- rgbC[2, ]
	bleue <- rgbC[3, ]
	nCl <- length(rouge)
	if(nCl > 1){
		rEd <- rep(rouge[nCl], ncolors)
		gEd <- rep(verte[nCl], ncolors)
		bEd <- rep(bleue[nCl], ncolors)
		xstart <- xrange[1]
		xinc <- diff(xrange)/(nCl-1)
		for(seg in 1:(nCl-1)){
		   segindex <- which((cW >= xstart) & (cW <= (xstart+xinc)))
		   rEd[segindex] <- reScaleC(cW[segindex], rouge[c(seg, seg+1)])
		   gEd[segindex] <- reScaleC(cW[segindex], verte[c(seg, seg+1)])
		   bEd[segindex] <- reScaleC(cW[segindex], bleue[c(seg, seg+1)])
		   xstart <- xstart+xinc
		}
		rEd <- ifelse(rEd < 0, 0, rEd)
		rEd <- ifelse(rEd > 255, 255, rEd)
		rEd <- as.integer(rEd)
		gEd <- ifelse(gEd < 0, 0, gEd)
		gEd <- ifelse(gEd > 255, 255, gEd)
		gEd <- as.integer(gEd)
		bEd <- ifelse(bEd < 0, 0, bEd)
		bEd <- ifelse(bEd > 255, 255, bEd)
		bEd <- as.integer(bEd)
	}else{
		rEd <- rep(rouge, ncolors)
		gEd <- rep(verte, ncolors)
		bEd <- rep(bleue, ncolors)
	}
	gradientColor <- paste('#', sprintf("%2.2x", rEd), sprintf("%2.2x", gEd), sprintf("%2.2x", bEd), sep = '')
	return(gradientColor)
}

