
##########List exchange (dialog qc,hom,merging)

listExchange<-function(list1,list2,list3){
	#retList<-list2[sapply(1:length(list2), function(x) !identical(list2[[x]], list1[[x]]))]  	#index list
	retList<-list2[sapply(names(list2), function(x) !identical(list2[[x]], list1[[x]]))]        #named list
	if(length(retList)>0){
		#id<-which(!list2%in%list1) #index list
		id<-names(retList)    #named list
		for(i in id){
			diffElement<-list2[[i]]!=list1[[i]]
			list3[[i]][diffElement]<-list2[[i]][diffElement]
		}
	}
	return(list3)
}

###############################
#initialize parameters

init.params<-function(action,period){

	#homogenization CDT
	if(action=='homog'){
		hom.opts<-data.frame(c("crop.bounds","h","conf.lev","Kmax","min.int"),c("0","0.025","95.0","10","24"))
		names(hom.opts)<-c('Parameters','Values')
		file.io<-data.frame(c('candidate','reference','DEM.netcdf','dir2save'),c('','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		use.method<-data.frame(c('hom.test','single.series','use.ref'),c("SNHT(Alexandersson & Moberg,1997)","0","0"))
		names(use.method)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format','vars'),c("1","1","2"))
		names(file.date.format)<-c('Parameters','Values')
		ref.series.choix<-data.frame(c('diff.ratio','weight.mean','use.elv','interp.dem','min.stn','max.stn','max.dist','elv.diff','min.rho'), c('1','1','0','1','2','7','100','200','0.5'))
		names(ref.series.choix)<-c('Parameters','Values')
		compute.var<-data.frame(c('function','miss.frac'),c('mean','0.95'))
		names(compute.var)<-c('Parameters','Values')
		Adjust.method<-data.frame(c('minAdjmo','minAdjdk','minAdjdy','SegAdjmo','SegAdjdk','SegAdjdy'), c('32','32','32','0','0','0'))
		names(Adjust.method)<-c('Parameters','Values')
		ref.series.user<-'0'
		stn.user.choice<-NULL
		ret.params<-list(action=action,period=period,file.io=file.io,use.method=use.method,hom.opts=hom.opts,file.date.format=file.date.format,
		ref.series.choix=ref.series.choix,compute.var=compute.var,Adjust.method=Adjust.method,
		ref.series.user=ref.series.user,stn.user.choice=stn.user.choice,retpar=0)
	}

	#RHtestsV4
	if(action=='rhtests'){
		file.io<-data.frame(c('candidate','reference','DEM.netcdf','dir2save'),c('','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format','vars'),c("1","1","2"))
		names(file.date.format)<-c('Parameters','Values')
		compute.var<-data.frame(c('function','miss.frac'),c('mean','0.95'))
		names(compute.var)<-c('Parameters','Values')
		rhtests.pars<-data.frame(c('p.lev','Iadj','Mq','Ny4a','pthr'),c("0.95","10000","12","0","0"))
		names(rhtests.pars)<-c('Parameters','Values')
		single.series<-'0'
		use.ref.series<-'0'
		ref.series.choix<-data.frame(c('weight.mean','use.elv','interp.dem','min.stn','max.stn','max.dist','elv.diff','min.rho'), c('Distance','0','1','2','7','100','200','0.5'))
		names(ref.series.choix)<-c('Parameters','Values')
		ref.series.user<-'0'
		stn.user.choice<-NULL
		prcpdata<-data.frame(c('rr.data','rr.log'),c('0','0'))
		names(prcpdata)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,file.date.format=file.date.format,
		single.series=single.series,rhtests.pars=rhtests.pars,compute.var=compute.var,prcpdata=prcpdata,
		ref.series.choix=ref.series.choix,use.ref.series=use.ref.series,
		ref.series.user=ref.series.user,stn.user.choice=stn.user.choice,getdata=FALSE)
	}
##########
	#qc.txtn
	if(action=='qc.temp'){
		test.tx<-'1'
		file.io<-data.frame(c('input.txtn1','input.txtn2','DEM.netcdf','dir2save'),c('','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		use.method<-data.frame(c('single.series','consist.check','use.elv','interp.dem'),c('0','0','0','0'))
		names(use.method)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format'),c("1","1"))
		names(file.date.format)<-c('Parameters','Values')
		param.temp<-data.frame(c('min.stn','max.stn','win.len','conf.lev','max.dist','elv.diff'),c('2','7','30','99.73','50','200'))
		names(param.temp)<-c('Parameters','Values')
		limControl<-data.frame(NA,-40,60,NA,NA)
		names(limControl)<-c('Station ID','Lower Bounds','Upper Bounds','Lon','Lat')
		parameter<-list(param.temp,limControl)
		AllOrOne<-'one'
		ret.params<-list(action=action,period=period,test.tx=test.tx,file.io=file.io,use.method=use.method,
		file.date.format=file.date.format,parameter=parameter,AllOrOne=AllOrOne,retpar=0)
	}

############
	#qc.rainfall
	##Zeros check
	if(action=='zero.check'){
		file.io<-data.frame(c('input.rainfall','dir2save'),c('',getwd()))
		names(file.io)<-c('Parameters','Values')
		param.zero<-data.frame(c('min.nbrs','max.nbrs','min.days','max.dst','pct.trsh'),c('3','6','22','100','1.5'))
		names(param.zero)<-c('Parameters','Values')
		AllOrOne<-'one'
		ret.params<-list(action=action,period=period,file.io=file.io,param.zero=param.zero,AllOrOne=AllOrOne,retpar=0)
	}

	##Outliers check
	if(action=='qc.rain'){
		file.io<-data.frame(c('input.rainfall','DEM.netcdf','dir2save'),c('','',getwd()))
		names(file.io)<-c('Parameters','Values')
		use.method<-data.frame(c('single.series','use.elv','interp.dem'),c('0','0','0'))
		names(use.method)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format'),c("1","1"))
		names(file.date.format)<-c('Parameters','Values')
		param.rain<-data.frame(c('min.stn','conf.lev','max.dist','elv.diff'),c('5','99.73','45','200'))
		names(param.rain)<-c('Parameters','Values')
		limControl<-data.frame(NA,300,NA,NA)
		names(limControl)<-c('Station ID','Upper Bounds','Lon','Lat')
		spatparam<-list(ispmax='1.0',ispobs='10.0',isdmin='3.0',isdobs='1.0',isdq1='10.0',ftldev='2.8')
		parameter<-list(param.rain,limControl,spatparam)

		AllOrOne<-'one'
		ret.params<-list(action=action,period=period,file.io=file.io,use.method=use.method,
		file.date.format=file.date.format,parameter=parameter,AllOrOne=AllOrOne,retpar=0)
	}


#########################################################################
###Mean bias rainfall
	if(action=='coefbias.rain'){
		file.io<-data.frame(c('stn.file','DEM.file','RFE.file','RFE.dir','dir2save'),c('','','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		prefix<-data.frame(c('rfeFileFormat','meanBiasPrefix'),c("rfe%s_%s-dk%s.nc","Gauge-rfe_mean.bias"))
		names(prefix)<-c('Parameters','Values')
		CreateGrd<-'1'
		dates.coef<-data.frame(c('year1','year2'),c('1983','2010'))
		names(dates.coef)<-c('Parameters','Values')
		params.int<-data.frame(c('min.nbrs','max.nbrs','max.dist'),c('3','5','1.0'))
		names(params.int)<-c('Parameters','Values')
		new.grid<-data.frame(c('minLon','maxLon','resLon','minLat','maxLat','resLat'),c('42','52','0.1','-26','-12','0.1'))
		names(new.grid)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,prefix=prefix,dates.coef=dates.coef,
		CreateGrd=CreateGrd,params.int=params.int,new.grid=new.grid)
	}

###Remove bias

	if(action=='rmbias.rain'){
		file.io<-data.frame(c('RFE.file','RFE.dir','Bias.dir','dir2save'),
		c('','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		prefix<-data.frame(c('rfeFileFormat','meanBiasPrefix','adjPrefix'),
		c("rfe%s_%s-dk%s.nc","Gauge-rfe_mean.bias","rr_adj"))
		names(prefix)<-c('Parameters','Values')
		dates.adj<-data.frame(c('istart.yrs','istart.mon','istart.dek',
		'iend.yrs','iend.mon','iend.dek'),c('1983','1','1','2014','12','3'))
		names(dates.adj)<-c('Parameters','Values')

		ret.params<-list(action=action,period=period,file.io=file.io,prefix=prefix,dates.adj=dates.adj)
	}

###Merging rainfall
	if(action=='merge.rain'){
		file.io<-data.frame(c('stn.file','DEM.file','RFE.file','RFE.dir','dir2save','shp.file','DEM.file'), c('','','','',getwd(),'',''))
		names(file.io)<-c('Parameters','Values')
		prefix<-data.frame(c('adjPrefix','mrgPrefix','mrgSuffix'),	c("rr_adj","rr_mrg","ALL"))
		names(prefix)<-c('Parameters','Values')
		CreateGrd<-'1'
		NewGrd<-'0'
		blankGrd<-'1'
		dates.mrg<-data.frame(c('istart.yrs','istart.mon','istart.dek', 'iend.yrs','iend.mon','iend.dek'),c('1983','1','1','2014','12','3'))
		names(dates.mrg)<-c('Parameters','Values')
		params.int<-data.frame(c('nmin.stn','min.non0','max.RnR.dist','max.dist','min.nbrs','max.nbrs'),c('10','7','0.75','1.0','3','5'))
		names(params.int)<-c('Parameters','Values')
		new.grid<-data.frame(c('minLon','maxLon','resLon','minLat','maxLat','resLat'), c('42','52','0.1','-26','-12','0.1'))
		names(new.grid)<-c('Parameters','Values')
		params.mrg<-data.frame(c('interpMethod','RainNoRain'),c('IDW','GaugeSatellite'))
		names(params.mrg)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,prefix=prefix,dates.mrg=dates.mrg,
		NewGrd=NewGrd,CreateGrd=CreateGrd,blankGrd=blankGrd,params.int=params.int, new.grid=new.grid,params.mrg=params.mrg)
	}
####dekadal update

	if(action=='merge.dekrain'){
		file.io<-data.frame(c('stn.file','RFE.dir','Bias.dir','dir2save','DEM.file','shp.file'), c('','','',getwd(),'',''))
		names(file.io)<-c('Parameters','Values')
		prefix<-data.frame(c('rfeFileFormat','meanBiasPrefix'),	c("rfe%s_%s-dk%s.nc","Gauge-rfe_mean.bias"))
		names(prefix)<-c('Parameters','Values')
		blankGrd<-'1'
		dates.mrg<-data.frame(c('istart.yrs','istart.mon','istart.dek'),c('2015','8','3'))
		names(dates.mrg)<-c('Parameters','Values')
		params.int<-data.frame(c('nmin.stn','min.non0','max.nbrs','max.RnR.dist'),c('10','7','4','0.75'))
		names(params.int)<-c('Parameters','Values')
		params.mrg<-data.frame(c('interpMethod','RainNoRain'),c('IDW','GaugeSatellite'))
		names(params.mrg)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,prefix=prefix,dates.mrg=dates.mrg,
		blankGrd=blankGrd,params.int=params.int,params.mrg=params.mrg)
	}



#################################################################
##compute regression parameters for downscaling
	if(action=='coefdown.temp'){
		file.io<-data.frame(c('stn.file','DEM.file','dir2save'), c('','',getwd()))
		names(file.io)<-c('Parameters','Values')
		dates.coef<-data.frame(c('year1','year2'),c('1961','2010'))
		names(dates.coef)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,dates.coef=dates.coef)
	}

######downscaling
	if(action=='down.temp'){
		file.io<-data.frame(c('stn.file','DEM.file','sampleReanalysis.file','dirReanalysis','dir2save'), c('','','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		IO.file.format<-data.frame(c('Reanalysis.file.format','downPrefix'),c("tmax_%s%s%s.nc","tmax_down"))
		names(IO.file.format)<-c('Parameters','Values')
		CreateGrd<-'1'
		new.grid<-data.frame(c('minLon','maxLon','resLon','minLat','maxLat','resLat'), c('42','52','0.1','-26','-12','0.1'))
		names(new.grid)<-c('Parameters','Values')
		dates.down<-data.frame(c('istart.yrs','istart.mon','istart.dek', 'iend.yrs','iend.mon','iend.dek'),c('1961','1','1','2014','12','3'))
		names(dates.down)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,IO.file.format=IO.file.format,
		CreateGrd=CreateGrd,new.grid=new.grid,dates.down=dates.down)
	}

##Bias coeff

	if(action=='coefbias.temp'){
		file.io<-data.frame(c('stn.file','DEM.file','dirDown','dir2save'), c('','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		dates.coef<-data.frame(c('year1','year2'),c('1961','2010'))
		names(dates.coef)<-c('Parameters','Values')
		bias.method<-'Bias-kriging'
		prefix<-data.frame(c('downPrefix','meanBiasPrefix'), c("tmax_down","tmax_JRAMeanBias_KR"))
		names(prefix)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,
		dates.coef=dates.coef,bias.method=bias.method,prefix=prefix)
	}

##Adjustment

	if(action=='adjust.temp'){
		file.io<-data.frame(c('stn.file','DEM.file','Down.dir','Bias.dir','dir2save'), c('','','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		bias.method<-'Bias-kriging'
		prefix<-data.frame(c('downPrefix','meanBiasPrefix','adjPrefix'), c("tmax_down","tmax_JRAMeanBias_KR","tmax_adj"))
		names(prefix)<-c('Parameters','Values')
		dates.adj<-data.frame(c('istart.yrs','istart.mon','istart.dek', 'iend.yrs','iend.mon','iend.dek'),c('1961','1','1','2014','12','3'))
		names(dates.adj)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,bias.method=bias.method,
		prefix=prefix,dates.adj=dates.adj)
	}


##Merging

	if(action=='merge.temp'){
		file.io<-data.frame(c('stn.file','DEM.file','shp.file','Adj.dir','dir2save'), c('','','','',getwd()))
		names(file.io)<-c('Parameters','Values')
		blankGrd<-'1'
		prefix<-data.frame(c('adjPrefix','mrgPrefix','mrgSuffix'), c("tmax_adj","tmax_mrg","ALL"))
		names(prefix)<-c('Parameters','Values')
		dates.mrg<-data.frame(c('istart.yrs','istart.mon','istart.dek', 'iend.yrs','iend.mon','iend.dek'),c('1961','1','1','2014','12','3'))
		names(dates.mrg)<-c('Parameters','Values')
		params.mrg<-data.frame(c('min.nbrs','max.nbrs','nmin','interpMethod'),c('3','5','10','IDW'))
		names(params.mrg)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,blankGrd=blankGrd,
		prefix=prefix,dates.mrg=dates.mrg,params.mrg=params.mrg)
	}

#############################################################################################3

#######################
	if(action=='chk.coords'){
		file.io<-data.frame(c('stn.file','shp.file','dir2save'),c('','',getwd()))
		names(file.io)<-c('Parameters','Values')
		buffer<-'10.0'
		ret.params<-list(action=action,period='daily',file.io=file.io,buffer=buffer)
	}
######################
	if(action=='agg.qc'){
		file.io<-getwd()
		ret.params<-list(action=action,period='daily',file.io=file.io)
	}
	if(action=='agg.zc'){
		file.io<-getwd()
		ret.params<-list(action=action,period='daily',file.io=file.io)
	}
	if(action=='agg.hom'){
		file.io<-getwd()
		ret.params<-list(action=action,period='daily',file.io=file.io)
	}
################
	if(action=='agg.stn'){
		period<-'daily'
		file.io<-data.frame(c('Stn.sample','Stn.Info','Stn.dir','File.save'),c('','','',''))
		names(file.io)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format'),c("1","1"))
		names(file.date.format)<-c('Parameters','Values')
		StartEnd.date<-data.frame(c('istart.yrs','istart.mon','istart.dek','iend.yrs','iend.mon','iend.dek'),
		c('1961','1','1','2014','12','31'))
		names(StartEnd.date)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,file.io=file.io,file.date.format=file.date.format,StartEnd.date=StartEnd.date)
	}
################
	if(action=='agg.ts'){
		period1<-'dekadal'
		data.type<-'cdt'
		file.io<-data.frame(c('stn.file','dir2save'),c('',''))
		names(file.io)<-c('Parameters','Values')
		compute.var<-data.frame(c('function','miss.frac'),c('mean','0.95'))
		names(compute.var)<-c('Parameters','Values')
		file.date.format<-data.frame(c('file.format','date.format'),c("1","1"))
		names(file.date.format)<-c('Parameters','Values')
		datesSE<-data.frame(c('istart.yrs','istart.mon','istart.day','iend.yrs','iend.mon','iend.day'),c('2010','1','1','2014','12','31'))
		names(datesSE)<-c('Parameters','Values')
		IO.file.format<-data.frame(c('Input','Output'),c("daily_%s%s%s.nc","dek_%s%s%s.nc"))
		names(IO.file.format)<-c('Parameters','Values')
		netcdf.var<-data.frame(c('varid.out','longname.out'),c('tmax','Maximum temperature'))
		names(netcdf.var)<-c('Parameters','Values')
		ret.params<-list(action=action,period=period,period1=period1,file.io=file.io,compute.var=compute.var,
		data.type=data.type,file.date.format=file.date.format,datesSE=datesSE,IO.file.format=IO.file.format,netcdf.var=netcdf.var)
	}
###########################################

	if(action=='extrct.ts'){
		file.io<-data.frame(c('NetCDF.dir','Shp.file','file2save'), c('','',getwd()))
		names(file.io)<-c('Parameters','Values')
		prefix<-data.frame(c('cdfFileFormat','tsPrefix'), c("rr_mrg_%s%s%s.nc","rr_adj"))
		names(prefix)<-c('Parameters','Values')
		dates.ts<-data.frame(c('istart.yrs','istart.mon','istart.dek', 'iend.yrs','iend.mon','iend.dek'),c('1983','1','1','2014','12','3'))
		names(dates.ts)<-c('Parameters','Values')

		ret.params<-list(action=action,period=period,file.io=file.io,prefix=prefix,dates.ts=dates.ts)
	}

#############
	return(ret.params)
}

