######### READ DISPLACE EFFORT OUTPUTS (area, metier, time)
#
# DATE ..........................: Mon May 17 14:50:00 2021
# AUTHOR.........................: Audric Vigier
#
#----------------------------------------------------------------
#
# R version......................: R version 3.6.0 (2019-04-26)
#----------------------------------------------------------------

rm(list=ls())
library(broom)
library(raster)
library(reshape2)
library(rgdal)
library(RSQLite) # To handle databases https://statkclee.github.io/R-ecology-lesson/06-r-and-sql.html
library(sp)
library(tidyverse)
library(gifski)
library(gganimate)
library(grDevices)
library(viridis) # colour-blind friendly palettes
# library(displaceplot) # Some functions exist to produce some plots, but not the ones I'm looking for.
# library(help=displaceplot)
source("D:/work/Displace/DISPLACE_RShiny_plots/R-scripts/setGeneralVariable.R", local = TRUE)

##################
###
###INPUTS
###
##################

general <- setGeneralOverallVariable (pathToRawInputs =file.path("D:/work/Displace/", paste0("DISPLACE_input_gis_","CelticSea")),
                                      pathToDisplaceInputs = file.path("D:/work/Displace/", paste0("DISPLACE_input_","CelticSea")),
                                      pathToOutputs =file.path("D:","DISPLACE_outputs"),
                                      caseStudy="CelticSea",
                                      iGraph=3,
                                      iYear="2010", # Beginning of time series
                                      iYearEnd="2020", # End of time series
                                      iCountry=NULL, #???
                                      nbPops=27,
                                      nbSzgroup=14,
                                      theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
                                      nbSimus=20,
                                      useSQLite=FALSE)

#load(file=paste(general$main.path,general$case_study,general$namefolderoutput[1],"output/forEffortPlots.Rdata",sep="/"))
##################
###
###END INPUTS
###
##################

sce = general$namefolderoutput[1]
# myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
# dbListTables(myConn)
# 
# PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
# VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike") # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
# VesselLogLikeCatches = dbGetQuery(myConn,"SELECT * FROM VesselLogLikeCatches")
# VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike") # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one).... Is it 2 hours slice I have, and not the actual effort? Ask.
# NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
# NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")
# 
# dbDisconnect(myConn) # Close connection

##################
###
###LOAD CATCH AND EFFORT SIMULATED VALUES, PREPARE CPUE DATASETS (EXPLICIT ONLY)
###
##################

getStockNames = function(){
  codes=read.table(file=paste(general$main.path.ibm, "/pop_names_CelticSea.txt",sep=""),header=T)
  stockNames=read.table(file=paste(general$main.path.param,"/POPULATIONS/Stock_biological_traits.csv",sep=""),header=T,sep=",") %>% 
    select(c(stock,species)) %>% 
    rename(spp=stock) %>% 
    merge(codes,by=c("spp")) %>% 
    rename(PopId=idx)
}

stockNames = getStockNames() %>% 
  arrange(PopId)

getMetierNames = function(){
  codes=read.table(file=paste(general$main.path.ibm, "/metiersspe_CelticSea/metier_names.dat",sep=""),header=T) %>% 
    rename(metierId=idx)
}

metierNames = getMetierNames() %>% 
  arrange(metierId)

#Only on 1 year...
load(file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsPlots.Rdata",sep="/"))

#Shapefiles for ICES rectangles and RTI rectangles
## Create shapefile VERIFIED, ALL NAMES MATCH THE GOOD RECTANGLES
icesquarterrectangle=raster(xmn=-13, xmx=-4, ymn=47.5, ymx=57, crs=CRS("+proj=longlat +datum=WGS84"), resolution=c(0.5,0.25)) # Create a raster bigger than necessary; encompass all the harbours!
#values will be their ICES name. Main rectangle: ususal name. Quarter name : 1 upper left, 2 upper right, 3 lower left, 4 lower right
xcoord=47:55 #D is replaced by 4; E is replaced by 5 since DISPLACE needs integers, hence are D7 to E5
ycoord=seq(42,24,-1)
icesNames=matrix(rep(paste(rep(ycoord, each=length(xcoord)),rep(xcoord,times=length(ycoord)),sep=""),each=2),ncol=length(ycoord))
icesNames=icesNames[,rep(1:ncol(icesNames), each = 2) ]
icesNames=paste(icesNames, rep(c(rep(c(1,2),times=length(xcoord)),rep(c(3,4),times=length(xcoord))),times=length(ycoord)),sep="")
icesNames=matrix(icesNames,ncol=2*length(ycoord))
icesNames=as.numeric(icesNames)
RTIrectangle=setValues(icesquarterrectangle, icesNames)
RTIrectangle=as.data.frame(RTIrectangle,xy=T)

icesquarterrectangle=raster(xmn=-13, xmx=-4, ymn=47.5, ymx=57, crs=CRS("+proj=longlat +datum=WGS84"), resolution=c(1,0.5)) # Create a raster bigger than necessary; encompass all the harbours!
icesNames=matrix(paste(rep(ycoord, each=length(xcoord)),rep(xcoord,times=length(ycoord)),sep=""),ncol=length(ycoord))
icesNames=as.numeric(icesNames)
icesquarterrectangle=setValues(icesquarterrectangle, icesNames)
icesquarterrectangle=as.data.frame(icesquarterrectangle,xy=T)

explicitCatchSpatialICES = explicitCatchSpatial %>%
  group_by(PopId,month,metierId,icesrectanglecode,year,Fraction) %>%
  summarize(value=sum(value),effort=sum(effort)) %>%
  rename(layer=icesrectanglecode) %>%
  merge(icesquarterrectangle,by=c("layer"))

explicitCatchSpatialRTI = explicitCatchSpatial %>%
  group_by(PopId,month,metierId,rtirectangle,year,Fraction) %>%
  summarize(value=sum(value),effort=sum(effort)) %>%
  rename(layer=rtirectangle) %>%
  merge(RTIrectangle,by=c("layer"))

##################
# Prepare time series plots

getExplicitCPUETimeSeries = function(explicitCatchSpatial,aggScale="year",metierSel="All",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = explicitCatchSpatial %>% 
    mutate(metierId=factor(metierId,metierNames$metierId,labels=metierNames$name)) %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if("All" %in% metierSel){
    data2plot$metierId="All"
  }else{
    data2plot=subset(data2plot,metierId%in%levels(data2plot$metierId)[metierSel])
  }
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    group_by(PopId,metierId,Fraction,time) %>% 
    summarize(value=sum(value),effort=sum(effort)) %>% 
    filter(Fraction%in%chosenFraction) %>% 
    ungroup() %>% 
    mutate(CPUE=value/effort) %>% 
    ggplot(aes(x=time,y=CPUE,colour=metierId,linetype=Fraction,shape=Fraction,group=interaction(metierId,Fraction,PopId)))+
      geom_line(size=1)+
      geom_point(size=2)+
      facet_wrap(~PopId,scales="free_y")+
      labs(x=paste("Time step (",aggScale,")",sep=""),y="CPUE\n(tons/h)",colour="Métier",linetype="Fraction",shape="Fraction", title = paste("Explicit CPUE time series\n",sce,sep=""))+
      theme_minimal()+
      theme(axis.title.y = element_text(angle=0,vjust=0.5))
  return(data2plot)
}


##################
# Prepare bar plots

getExplicitCPUEBarPlot = function(explicitCatchSpatial,aggScale="year",metierSel="All",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = explicitCatchSpatial %>% 
    mutate(metierId=factor(metierId,metierNames$metierId,labels=metierNames$name)) %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if("All" %in% metierSel){
    data2plot$metierId="All"
  }else{
    data2plot=subset(data2plot,metierId%in%levels(data2plot$metierId)[metierSel])
  }
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    filter(Fraction%in%chosenFraction & time%in%c(min(time),max(time))) %>% 
    group_by(PopId,metierId,Fraction,time) %>% 
    summarize(value=sum(value),effort=sum(effort)) %>% 
    ungroup() %>% 
    mutate(CPUE=value/effort) %>% 
    ggplot(aes(x=interaction(Fraction,metierId),y=CPUE,fill=PopId,group=interaction(metierId,Fraction,PopId)))+
      geom_bar(stat="identity",colour="black")+
      facet_wrap(~time)+
      labs(x="Métier and fraction",y="CPUE\n(tons/h)",fill="Population", title = paste("Explicit CPUE per species and metier\n",sce,sep=""))+
      theme_minimal()+
      theme(axis.title.y = element_text(angle=0,vjust=0.5),axis.text.x=element_text(angle=45,vjust=1,hjust=1))
  return(data2plot)
}


##################
# Prepare maps

# CPUE
getExplicitCPUEMap = function(data2process,popNum=0,timeStep=1,metierNum=0,fractionName="Landings",sce=sce,aggScale="month",resScale="All",gif=F){
  # Will be done in the argument, the use will have to be careful
  if(resScale=="All"){
    data2plot=data2process %>% 
      rename(layer=NodeId)
  }else{
    data2plot=data2process %>% 
      rename(Long=x,Lat=y)
  }
  
  data2plot = data2plot %>% 
    filter(PopId==popNum) %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp)) %>% 
    mutate(PopId=droplevels(PopId))
  
  if(aggScale=="month")data2plot$time= data2plot$month
  if(aggScale=="year")data2plot$time= data2plot$year
  
  plotrange = rbind(range(data2plot$Long),range(data2plot$Lat))
  
  if(metierNum=="All"){
    data2plot = data2plot %>% 
      mutate(metierId="All") %>% 
      group_by(PopId,time,metierId,layer,Long,Lat,Fraction) %>% 
      summarize(value=sum(value,na.rm=T),effort=sum(effort,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(CPUE=value/effort)
  }else{
    data2plot=data2plot %>% 
      filter(metierId==metierNum) %>% 
      mutate(metierId=factor(metierId,metierNames$metierId,labels=metierNames$name)) %>% 
      group_by(PopId,time,metierId,layer,Long,Lat,Fraction) %>% 
      summarize(value=sum(value,na.rm=T),effort=sum(effort,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(CPUE=value/effort)
  }
  
  if(!gif){
    maxScale = max(data2plot$CPUE[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=CPUE))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste("CPUE (", fractionName,")\n(tons/hour)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="RTI"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=CPUE))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste("CPUE (", fractionName,")\n(tons/hour)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="ICES"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=CPUE))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste("CPUE (",fractionName,")\n(tons)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    return(data2plot)
  }
  if (gif){
    maxScale = max(data2plot$CPUE[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=CPUE, group=seq_along(time)))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste("CPUE (",fractionName,")\n(tons)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="RTI"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=CPUE, group=seq_along(time)))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste("CPUE (",fractionName,")\n(tons)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="ICES"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=CPUE, group=seq_along(time)))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste("CPUE (",fractionName,")\n(tons)",sep=""),title=paste("Explicit CPUE (",fractionName,") ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    
    if(aggScale=="month") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=round(length(unique(data2plot$time))/4), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(data2plot$time)), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    
    if(resScale=="All") anim_save(animation=plotMapAnim,filename=paste("explicit_CPUE_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="RTI") anim_save(animation=plotMapAnim,filename=paste("explicit_CPUE_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_RTI.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="ICES") anim_save(animation=plotMapAnim,filename=paste("explicit_CPUE_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_ICES.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    
  }
}

#Test Options
popNum=7
for(metierNum in c(3,"All")){
  for(fractionName in c("Landings","Discards")){
    for(aggScale in c("month","year")){
      timeStep=1
      if (aggScale=="year")timeStep = timeStep+2009
      for(resScale in c("All","RTI","ICES")){
        if (resScale=="All") data2process = explicitCatchSpatial
        if (resScale=="ICES") data2process = explicitCatchSpatialICES
        if (resScale=="RTI") data2process = explicitCatchSpatialRTI
        getExplicitCPUEMap(data2process,popNum,timeStep,metierNum,fractionName,sce,aggScale,resScale,gif=T)
        png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/explicitCPUE_",fractionName,"_",resScale,"_",aggScale,"_",timeStep,"_",popNum,"_",metierNum,".png",sep=""),height=800,width=800,units="px",res=100)
        print(getExplicitCPUEMap(data2process,popNum,timeStep,metierNum,fractionName,sce,aggScale,resScale,gif=F))
        dev.off()
      }
    }
  }
}

# RTI tariffs derived from LPUE or DPUE (so that it can be re-used generically
# Set of species: cod, haddock, herring, Norway lobster, plaice, whiting
# Average over cell values (instead of sum of L over sum of E)

explicitCatchSpatialRTI
popNum=7
timeStep=1
metierNum="All"
fractionName="Landings"
sce=sce
aggScale="year"
gif=F

# Add an option for set of metiers withing a gear

getExplicitCPUERTILikeMap = function(explicitCatchSpatialRTI,popNum=7,timeStep=1,metierNum=0,fractionName="Landings",sce=sce,aggScale="year",gif=F){
  # Will be done in the argument, the use will have to be careful
  
  data2plot=explicitCatchSpatialRTI %>% 
    rename(Long=x,Lat=y)
  
  if (is.na(popNum)) popNum = c(1,5,6,11,12,13,14,16,24)
  
  data2plot = data2plot %>% 
    filter(PopId%in%popNum) %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp)) %>% 
    mutate(PopId=droplevels(PopId))
  
  if(aggScale=="month")data2plot$time= data2plot$month
  if(aggScale=="year")data2plot$time= data2plot$year
  
  plotrange = rbind(range(data2plot$Long),range(data2plot$Lat))
  
  if(metierNum=="All"){
    data2plot = data2plot %>% 
      mutate(metierId="All") %>% 
      group_by(PopId,time,metierId,layer,Long,Lat,Fraction) %>% 
      summarize(value=sum(value,na.rm=T),effort=sum(effort,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(CPUE=value/effort) %>% 
      group_by(PopId,time,metierId,Fraction) %>% 
      mutate(averageCPUE=mean(CPUE,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(RTI=CPUE/averageCPUE) %>%
      mutate(RTI=replace(RTI,averageCPUE==0,0)) %>% 
      mutate(RTI=cut(RTI,breaks=c(-Inf,0.1,0.5,1,2,Inf),labels=1:5)) %>% 
      mutate(RTI=as.numeric(levels(RTI))[RTI])%>% 
      group_by(time,metierId,Fraction,layer,Long,Lat) %>% 
      summarize(RTI=max(RTI)) %>% 
      ungroup() %>% 
      mutate(RTI=factor(RTI,levels=1:5,labels=1:5))
    
  }else{
    data2plot = data2plot %>% 
      group_by(PopId,time,metierId,layer,Long,Lat,Fraction) %>% 
      summarize(value=sum(value,na.rm=T),effort=sum(effort,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(CPUE=value/effort) %>% 
      group_by(PopId,time,Fraction) %>% 
      mutate(averageCPUE=mean(CPUE,na.rm=T)) %>% 
      ungroup() %>% 
      mutate(RTI=CPUE/averageCPUE) %>%
      mutate(RTI=replace(RTI,averageCPUE==0,0)) %>% 
      mutate(RTI=cut(RTI,breaks=c(-Inf,0.1,0.5,1,2,Inf),labels=1:5)) %>% 
      mutate(RTI=as.numeric(levels(RTI))[RTI])%>% 
      group_by(time,metierId,Fraction,layer,Long,Lat) %>% 
      summarize(RTI=max(RTI)) %>% 
      ungroup() %>% 
      mutate(RTI=factor(RTI,levels=1:5,labels=1:5))%>% 
      filter(metierId==metierNum) %>% 
      mutate(metierId=factor(metierId,metierNames$metierId,labels=metierNames$name))
  }
  
  if(!gif){
    
      plot2return = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=RTI))+
          geom_tile(width=0.5,height=0.25,colour="black")+
          scale_fill_manual(values = viridis(5))+
          labs(x="Longitude",y="Latitude",fill="RTI category",title=paste("RTI category pop(s) ",paste(popNum,collapse=" ")," ",aggScale," ",timeStep," ", " metier(s) ",sort(unique(data2plot$metierId)),"\n",sce ,sep=""))+
          expand_limits(x=plotrange[1,],y=plotrange[2,])+
          theme_minimal()+
          theme(axis.title.y=element_text(angle=0,vjust=0.5))
    
    return(plot2return)
  }
  if (gif){
    
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=RTI, group=seq_along(time)))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_manual(values = viridis(5))+
        labs(x="Longitude",y="Latitude",fill="RTI category",title=paste("RTI category pop(s) ",paste(popNum,collapse=" ")," ",aggScale," {frame}  metier(s) ",sort(unique(data2plot$metierId)),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    
    if(aggScale=="month") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=round(length(unique(data2plot$time))/4), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(data2plot$time)), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    
    anim_save(animation=plotMapAnim,filename=paste("CPUE_RTIlike_",fractionName,"_met",sort(unique(data2plot$metierId)),"_pop",paste(popNum,collapse="-"),"_",aggScale,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))

  }
}

#Test Options
for(popNum in c(NA,7)){
  popNumPrint = popNum
  if (is.na(popNum)) popNumPrint = c(1,5,6,11,12,13,14,16,24)
  for(metierNum in c(3,"All")){
    for(fractionName in c("Landings","Discards")){
      for(aggScale in c("month","year")){
        timeStep=1
        #if (aggScale=="year")timeStep = timeStep+2009
        getExplicitCPUERTILikeMap(explicitCatchSpatialRTI,popNum=popNum,timeStep,metierNum,fractionName,sce,aggScale,gif=T)
        png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/CPUE_RTIlike_",fractionName,"_met",sort(unique(data2plot$metierId)),"_pop",paste(popNumPrint,collapse="-"),"_",aggScale,"_",timeStep,".png",sep=""),height=800,width=800,units="px",res=100)
        print(getExplicitCPUERTILikeMap(explicitCatchSpatialRTI,popNum=popNum,timeStep,metierNum,fractionName,sce,aggScale,gif=F))
        dev.off()
      }
    }
  }
}