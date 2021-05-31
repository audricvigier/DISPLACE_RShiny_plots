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

##################
###
###CONNECT TO THE DATABASE AND LOAD SOME TABLES
###
##################

sce=general$namefolderoutput[1]
myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2]),"_out.db",sep=""))
dbListTables(myConn)

PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike") # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
VesselLogLikeCatches = dbGetQuery(myConn,"SELECT * FROM VesselLogLikeCatches") 
VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike") # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one).... Is it 2 hours slice I have, and not the actual effort? Ask.
NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")

dbDisconnect(myConn) # Close connection

##################
###
###CATCH TIME SERIES: FRACTION, POP, TIME STEP (MONTH), METIER, SIZE, IMPLICIT/EXPLICIT. AREA INFO ONLY FOR THE MAPS. I IGNORE SIZE OF COMPOSITION OF CTACH AT THE MOMENT.
###
##################

##################
# Prepare dataset

months = data.frame(TStep = sort(unique(NodesStat$TStep)), month= 1:length(sort(unique(NodesStat$TStep))))
months = data.frame(TStep = c(sort(unique(NodesStat$TStep)),(max(NodesStat$TStep)+100)), month= c(1:length(sort(unique(NodesStat$TStep))),length(sort(unique(NodesStat$TStep))))) # Adding one more row to avoid crashes

nodes2merge=subset(NodesDef,select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0) %>%
  rename(rtirectangle=icesrectanglecode) %>%
  mutate(icesrectanglecode=as.numeric(sapply(as.character(rtirectangle), function(x) substr(x,1,4))))

metierCorr= unique(subset(VesselLogLike, select=c(Id,metierId)))

VesselVmsLikeCond = VesselVmsLike %>%  # Pb: VesselLogLike's NodeId is the harbour, not the actual fishing location. get the fishing location here
  filter(State==1) %>% 
  group_by(Id,TStepDep) %>% 
  filter(TStep==max(TStep)) %>% 
  ungroup() %>% 
  select(-c(CumFuel,Course,State)) %>%
  unique() %>%
  merge(nodes2merge,by=c("Long","Lat"))

##################
# WARNING : these plots are not meant for quality of fit evaluation, hence implicit catch can be accounted for on 2010:2011

# Get implicit and explicit catch and discards for all time steps (months), metiers, pops NOT SPATIAL
getExplicitCatch = function(VesselLogLike,VesselLogLikeCatches,months,nodes2merge){
  stepLogLikeId = VesselLogLike %>% 
    select(c(RowId,TStepDep,TStep,metierId)) %>% 
    rename(LoglikeId=RowId)
  
  cumcatchLog=VesselLogLikeCatches %>% 
    merge(stepLogLikeId, by=c("LoglikeId"),all=T) %>% 
    select(-c(TStepDep))%>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] ))
  # TStep is the time step at the end of the month, e.g. 745 is the end of Jan 2010
  
  cumcatchLog = cumcatchLog %>% 
    group_by(PopId,month,metierId) %>% 
    summarise_at(c("Catches","Discards"),sum) %>% 
    mutate(year=2010+floor((month-1)/12)) %>%
    ungroup() %>% 
    melt(id.vars=c("PopId","month","metierId","year")) %>% 
    rename(Fraction=variable) %>% 
    mutate(value=value/1000) # Convert to tons
  
  return(cumcatchLog)
}

#LIMITES TO 1 YEAR SO FAR BECAUS OF DISPLACE HARD CODING
getExplicitCatchSpatial = function(VesselLogLike,VesselLogLikeCatches,months,nodes2merge){
  stepLogLikeId = VesselLogLike %>% 
    select(c(RowId,TStepDep,TStep,metierId,Id)) %>% 
    unique() %>% 
    rename(LoglikeId=RowId)
  
  cumcatchLog=VesselLogLikeCatches %>% 
    merge(stepLogLikeId, by=c("LoglikeId"),all=T) %>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] ))
  # TStep is the time step at the end of the month, e.g. 745 is the end of Jan 2010
  
  cumcatchLog = cumcatchLog %>% 
    merge(subset(VesselVmsLikeCond, select=-c(TStep)), by=c("Id","TStepDep"))%>% 
    group_by(PopId,month,metierId,NodeId,icesrectanglecode,rtirectangle,Long,Lat) %>% 
    summarise_at(c("Catches","Discards"),sum) %>% 
    mutate(year=2010+floor((month-1)/12)) %>%
    ungroup() %>% 
    melt(id.vars=c("PopId","month","metierId","NodeId","icesrectanglecode","rtirectangle","Long","Lat","year")) %>% 
    rename(Fraction=variable) %>% 
    mutate(value=value/1000) # Convert to tons
  
  return(cumcatchLog)
}

getImplicitCatch = function(PopValues,cumcatchLog){
  cumcatchLog = explicitCatch %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    reshape2::dcast(.,PopId+month+year~Fraction,value.var="value")
  
  ImplicitCatch = PopValues %>% 
    group_by(TStep,PopId,NodeId) %>% # Eliminate duplicates rows at last time step
    filter(row_number() == 1) %>% 
    ungroup()
  
  ImplicitCatch = ImplicitCatch %>% 
    group_by(TStep,PopId) %>% 
    summarise_at(c("CumDiscards","CumCatches"),sum) %>% # COnvert to tons
    mutate(CumDiscards=CumDiscards/1000,CumCatches=CumCatches/1000) %>% 
    arrange(PopId,TStep) %>% 
    group_by(PopId) %>% 
    mutate(CumDiscards=c(min(CumDiscards),diff(CumDiscards)),CumCatches=c(min(CumCatches),diff(CumCatches))) %>% # From cumulative time series to time series
    ungroup() %>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] )) %>% 
    group_by(month,PopId) %>% 
    summarise_at(c("CumDiscards","CumCatches"),sum) %>%
    merge(cumcatchLog, by=c("month","PopId"))%>% # Convert to tons
    mutate(Catches =CumCatches-Catches, Discards =CumDiscards-Discards) %>% 
    select(-c(CumDiscards,CumCatches)) %>% 
    group_by(month,PopId,year) %>% 
    melt(id.vars=c("PopId","month","year")) %>% 
    rename(Fraction=variable)
  return(ImplicitCatch)
}

explicitCatch = getExplicitCatch(VesselLogLike,VesselLogLikeCatches,months,nodes2merge) # Includes discards
explicitCatchSpatial = getExplicitCatchSpatial(VesselLogLike,VesselLogLikeCatches)
implicitCatch = getImplicitCatch(PopValues,cumcatchLog) # Includes discards
implicitCatchSpatial = getImplicitCatchSpatial(PopValues,cumcatchLog,stockNames)

##################
# Prepare time series plots

##################
# Prepare bar plots


##################
###
###CREATE ONE EFFORT MAP GIF FOR THE WHOLE FISHERY OR PER METIER, ONE FRAME BEING A MONTH (AT THE SAMLLEST SCALE), FOR DIFFERENT AGGREGATION SCALES (NODE,RECT STAT, RTI RECTANGLE)
###
##################

# Per node
getmapEffortNodeAll=function(VesselVmsLikeCond,gif=FALSE,idMetier=0,monthNum=1,scename=""){ # 3 sec per metier for one year
  plotrange = rbind(range(VesselVmsLikeCond$Long),range(VesselVmsLikeCond$Lat))
  if(is.na(idMetier)) {
    VesselVmsLikeCond$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(is.na(monthNum) & !gif){
    VesselVmsLikeCond$month="all months cumulated"
    monthNum="all months cumulated"
  } 
  
  VesselVmsLikeCond = VesselVmsLikeCond %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(NodeId,Long,Lat,month,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(VesselVmsLikeCond$effort)
    VesselVmsLikeCond = VesselVmsLikeCond %>% 
      filter(month==monthNum)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(VesselVmsLikeCond$effort)
    mapEffortNode = VesselVmsLikeCond %>% 
      ggplot(aes(x=Long,y=Lat,colour=effort, group=seq_along(month)))+
      geom_point(size=1)+
      scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",colour="Effort\n(hours)",title=paste("Fishing effort month {frame} ", " metier ",idMetier,"\n",scename,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(month)
    
    plotMapAnim = animate(mapEffortNode, renderer = gifski_renderer(),duration=length(unique(VesselVmsLikeCond$month))/4, nframes=length(unique(VesselVmsLikeCond$month)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
    mapEffortNode =VesselVmsLikeCond %>% 
      ggplot(aes(x=Long,y=Lat,colour=effort))+
      geom_point(size=1)+
      scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",colour="Effort\n(hours)",title=paste("Fishing effort month ",monthNum," metier ",idMetier,"\n",scename,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))
    return(mapEffortNode)
  }
}


# Per RTI rectangle
preconditionRTI = function(VesselVmsLikeCond,RTIrectangle){
  mapEffortRTI = VesselVmsLikeCond %>% 
    group_by(icesrectanglecode,month,metierId) %>% 
    summarize(effort=sum(effort)) %>% 
    rename(layer=icesrectanglecode) %>% 
    ungroup() %>% 
    select(c(layer,effort,month,metierId))
  
  polygonsRTI=RTIrectangle %>% 
    merge(mapEffortRTI,by=c("layer"),all.x=T)
  return(polygonsRTI)
}
                            
getmapEffortRTIAll=function(polygonsRTI,gif=FALSE,idMetier=0,monthNum=1,scename=""){ #if metier is NA : do the whole fishery instead

  plotrange = rbind(range(polygonsRTI$x),range(polygonsRTI$y))
  if(is.na(idMetier)) {
    polygonsRTI$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(is.na(monthNum) & !gif){
    polygonsRTI$month="all months cumulated"
    monthNum="all months cumulated"
  } 
  
  polygonsRTI = polygonsRTI %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(layer,x,y,month,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(polygonsRTI$effort)
    polygonsRTI = polygonsRTI %>% 
      filter(month==monthNum)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(polygonsRTI$effort)
    mapEffortRTITemp = polygonsRTI %>% 
      ggplot(aes(x=x,y=y,fill=effort, group=seq_along(month)))+
      geom_tile(colour="black",width=0.5,height=0.25)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort month {frame} ", " metier ",idMetier,"\n",scename ,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(month)
    
    plotMapAnim = animate(mapEffortRTITemp, renderer = gifski_renderer(),duration=length(unique(VesselVmsLikeCond$month))/4, nframes=length(unique(VesselVmsLikeCond$month)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_RTIcell_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_RTIcell_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
      mapEffortRTITemp =polygonsRTI %>% 
        ggplot(aes(x=x,y=y,fill=effort))+
        geom_tile(colour="black",width=0.5,height=0.25)+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort month ",monthNum," metier ",idMetier,"\n",scename,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
      return(mapEffortRTITemp)
    }
}


# Per ICES rectangle
preconditionICES = function(VesselVmsLikeCond,icesquarterrectangle){
  mapEffortICES = VesselVmsLikeCond %>% 
    ungroup() %>% 
    mutate(icesrectanglecode=as.numeric(substr(as.character(icesrectanglecode),1,4))) %>% 
    group_by(icesrectanglecode,month,metierId) %>% 
    summarize(effort=sum(effort)) %>% 
    rename(layer=icesrectanglecode) %>% 
    ungroup() %>% 
    select(c(layer,effort,month,metierId))
  
  polygonsICES=icesquarterrectangle %>% 
    merge(mapEffortICES,by=c("layer"),all.x=T)
  
  return(polygonsICES)
}

getmapEffortICESAll=function(polygonsICES,gif=FALSE,idMetier=0,monthNum=1,scename=""){ # 3 sec per metier for one year
  # monthNum=NA
  # idMetier=NA
  # gif=F
  plotrange = rbind(range(polygonsICES$x),range(polygonsICES$y))
  if(is.na(idMetier)) {
    polygonsICES$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(is.na(monthNum) & !gif){
    polygonsICES$month="all months cumulated"
    monthNum="all months cumulated"
  } 
  
  polygonsICES = polygonsICES %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(layer,x,y,month,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(polygonsICES$effort)
    polygonsICES = polygonsICES %>% 
      filter(month==monthNum)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(polygonsICES$effort)
    mapEffortICESTemp = polygonsICES %>% 
      ggplot(aes(x=x,y=y,fill=effort, group=seq_along(month)))+
      geom_tile(colour="black",width=1,height=0.5)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort month {frame} ", " metier ",idMetier,"\n",scename ,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(month)
    
    plotMapAnim = animate(mapEffortICESTemp, renderer = gifski_renderer(),duration=length(unique(VesselVmsLikeCond$month))/4, nframes=length(unique(VesselVmsLikeCond$month)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_ICEScell_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_ICEScell_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
    mapEffortICESTemp =polygonsICES %>% 
      ggplot(aes(x=x,y=y,fill=effort))+
      geom_tile(colour="black",width=1,height=0.5)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort month ",monthNum," metier ",idMetier,"\n",scename,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))
    return(mapEffortICESTemp)
  }
}

# getmapEffortNodeAll(VesselVmsLikeCond,gif=FALSE,idMetier=10,monthNum=1)
# getmapEffortRTIAll(polygonsRTI,gif=FALSE,idMetier=NA,monthNum=10)
# getmapEffortICESAll(polygonsICES,gif=FALSE,idMetier=NA,monthNum=10)