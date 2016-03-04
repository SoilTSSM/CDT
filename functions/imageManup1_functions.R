
manageLayout<-function(nbPlot,transpose=FALSE){
	n<-5 #nmaxPlot=n*n+n
	nLayout<-cbind(c(rep(1:n,seq(1,n*2,2)),rep(n+1,n)),rep(1:n,seq(2,n*2,2)))
	dimLayout<-nLayout[nbPlot,]
	if(transpose) dimLayout<-rev(dimLayout)
	matdim<-dimLayout[1]*dimLayout[2]
	mat<-rep(NA,matdim)
	mat[((matdim-nbPlot)+1):matdim]<-1:nbPlot	
	matLayout<-matrix(mat,ncol=dimLayout[1],nrow=dimLayout[2],byrow=T)
	line1<-matLayout[1,]
	line1<-line1[!is.na(line1)]
	ltmp<-rep(NA,dimLayout[1])
	ltmp[line1]<-line1
	matLayout[1,]<-ltmp
	matLayout<-matLayout[dimLayout[2]:1,]
	orderLayout<-c(t(matLayout))
	orderLayout<-orderLayout[!is.na(orderLayout)]
	return(list(dim=dimLayout,order=orderLayout))
}

