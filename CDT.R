
rm(list=ls(all=TRUE))
library(stringr)

cdtVersion<<-'2.3'
apps.dir <<- dirname(parent.frame(2)$ofile)
imgdir<<-file.path(apps.dir,'images',fsep = .Platform$file.sep)

libFpath<-file.path(apps.dir,'configure','configure.txt',fsep = .Platform$file.sep)
tclPath<-readLines(libFpath,warn=FALSE)

lvar<-lapply(tclPath,function(x) strsplit(x,"="))
vars<-str_trim(unlist(lapply(lvar,function(x) x[[1]][1])))
iTcl<-which(vars=="UseOtherTclTk")
useTcl<-str_trim(lvar[[iTcl]][[1]][[2]])
if(useTcl=="1"){
	if(Sys.info()["sysname"] == "Windows") {
		xvr<-gsub("\\{","",gsub("\\}", "", lvar[[which(vars=="Windows")]][[1]][2]))
		xvr<-strsplit(xvr,";")[[1]]
	}else if(Sys.info()["sysname"] == "Darwin") {
		xvr<-gsub("\\{","",gsub("\\}", "", lvar[[which(vars=="MacOS")]][[1]][2]))
		xvr<-strsplit(xvr,";")[[1]]
	}else if(Sys.info()["sysname"] == "Linux") {
		xvr<-gsub("\\{","",gsub("\\}", "", lvar[[which(vars=="Linux")]][[1]][2]))
		xvr<-strsplit(xvr,";")[[1]]
	}
	tclbin<-str_trim(eval(parse(text=xvr[1])))
	tcllib<-str_trim(eval(parse(text=xvr[2])))
	Sys.setenv(MY_TCLTK=tclbin)
	Sys.setenv(TCL_LIBRARY=tcllib)
	library.dynam("tcltk", "tcltk", .libPaths(), DLLpath =tclbin)
}

#########Load library
options(warn=-1)
library(tools)
library(tcltk)
library(tkrplot)
library(ncdf)
library(ncdf4)
library(gmt)
library(fields)
library(lattice)
library(latticeExtra)
library(sp)
library(gstat)
library(automap)
library(maptools)
library(rgeos)
library(grid)
library(reshape2)
library(compiler)
library(parallel)
library(foreach)
library(doParallel)

#compilePKGS(enable=TRUE)
#enableJIT(3)
nb_cores<-detectCores()-1
doparallel<- if(nb_cores<2) FALSE else TRUE
# `%parLoop%`<- if(nb_cores<2) `%do%` else `%dopar%`

#######################

if (Sys.info()["sysname"] == "Windows"){
	is.noImg<-tclRequire("Img")
	if(is.logical(is.noImg)){
		tkmessageBox(message="Tcl package 'Img' not found",icon="error",type="ok")
		stop("Tcl package 'Img' not found")
	}
}

#msgOUT<-file(file.path(apps.dir,'parameters','WarnMsg.txt',fsep = .Platform$file.sep),open="w+")
#sink(msgOUT, type="message")

source(file.path(apps.dir,'functions','cdtConfiguration_function.R',fsep = .Platform$file.sep))
confpath<-file.path(apps.dir,'configure','configure0',fsep = .Platform$file.sep)

if(file.exists(confpath)){
	conffile<-as.character(read.table(confpath,colClasses='character')[,1])
	setwd(conffile[1])
	addTclPath(path=conffile[2])
	addTclPath(path=conffile[3])		
	tclRequire("Tktable")
	tclRequire("BWidget")
	tcl("package","require","BWidget") 	
	source(file.path(apps.dir,'functions','cdtMain_window.R',fsep = .Platform$file.sep))
}else{
	configCDT()
	source(file.path(apps.dir,'functions','cdtMain_window.R',fsep = .Platform$file.sep))
}


