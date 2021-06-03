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
library(displaceplot) # Some functions exist to produce some plots, but not the ones I'm looking for.
library(help=displaceplot)
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
explicit_pops = 0:(general$nbpops-1)

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

myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
dbListTables(myConn)

NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")  # Cumulated fishing time per node and time step (NOT metier though)
#VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike")  # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike")  # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one)....
NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)

dbDisconnect(myConn) # Close connection

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

#metierCorr= unique(subset(VesselLogLike, select=c(Id,metierId)))
months = data.frame(TStep = sort(unique(NodesStat$TStep)), month= 1:length(sort(unique(NodesStat$TStep))))

fishingLocations = VesselVmsLike %>% 
  filter(State==1) %>% 
  select(-c(Course,CumFuel,State,TStep)) %>% 
  group_by(Id,TStepDep,Long,Lat) %>% 
  summarize(effort=n()) %>% 
  group_by(Id,TStepDep) %>% 
  mutate(prop=effort/sum(effort))

# Assume that for each trip, catch spatial distribution is proportional to effort spatial distribution

catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
names(catchAndEffortPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   

effortPertrip = catchAndEffortPertrip %>% 
  mutate(effort=TStep-TStepDep-cumsteaming) %>% 
  select(c(effort,TStep,TStepDep,Id,freq_metiers)) %>% 
  mutate(metierId = sapply(as.character(freq_metiers), function(x) strsplit(x,split=")")[[1]][1])) %>% 
  mutate(metierId = as.numeric(sapply(metierId, function(x) strsplit(x,split="\\(")[[1]][2]))) %>% 
  select(-freq_metiers) %>% 
  unique() %>% 
  merge(fishingLocations,by=c("Id","TStepDep"),all.y=T) %>% 
  #mutate(sanityCheck=abs(effort.y/prop-effort.x)) # All good, except what is cut at the end of first year. Keep effort.x and prop
  mutate(effort=effort.x*prop) %>% 
  select(-c(effort.x,effort.y)) %>% 
  merge(subset(NodesDef, select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0), by=c("Long","Lat")) %>% 
  group_by(Long,Lat,Id,TStep,metierId,NodeId,icesrectanglecode) %>% 
  summarize(effort=sum(effort,na.rm=T)) %>%  # Works since State = 1 when fishing
  ungroup() %>% 
  mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>x]))] )) %>% 
  group_by(Long,Lat,month,metierId,NodeId,icesrectanglecode) %>% 
  summarize(effort=sum(effort,na.rm=T)) %>% 
  ungroup()

polygonsRTI=preconditionRTI(effortPertrip,RTIrectangle)
polygonsICES=preconditionICES(effortPertrip,icesquarterrectangle)

##################
###
###CREATE ONE EFFORT MAP GIF FOR THE WHOLE FISHERY OR PER METIER, ONE FRAME BEING A MONTH (AT THE SAMLLEST SCALE), FOR DIFFERENT AGGREGATION SCALES (NODE,RECT STAT, RTI RECTANGLE)
###
##################

# Per node
getmapEffortNodeAll=function(effortPertrip,gif=FALSE,idMetier=0,monthNum=1,scename=""){ # 3 sec per metier for one year
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
preconditionRTI = function(effortPertrip,RTIrectangle){
  mapEffortRTI = effortPertrip %>% 
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
preconditionICES = function(effortPertrip,icesquarterrectangle){
  mapEffortICES = effortPertrip %>% 
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