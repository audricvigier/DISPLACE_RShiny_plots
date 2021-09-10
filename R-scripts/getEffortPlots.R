######### READ DISPLACE EFFORT OUTPUTS (area, metier, time)
#
# DATE ..........................: Mon May 17 14:50:00 2021
# AUTHOR.........................: Audric Vigier
#
#----------------------------------------------------------------
#
# R version......................: R version 3.6.0 (2019-04-26)
#----------------------------------------------------------------

# rm(list=ls())
# library(broom)
# library(raster)
# library(reshape2)
# library(rgdal)
# library(RSQLite) # To handle databases https://statkclee.github.io/R-ecology-lesson/06-r-and-sql.html
# library(sp)
# library(tidyverse)
# library(gifski)
# library(gganimate)
# library(grDevices)
# library(viridis) # colour-blind friendly palettes
# library(displaceplot) # Some functions exist to produce some plots, but not the ones I'm looking for.
# library(help=displaceplot)
# # source("D:/work/Displace/DISPLACE_RShiny_plots/R-scripts/setGeneralVariable.R", local = TRUE)

##################
###
###INPUTS
###
##################

# general <- setGeneralOverallVariable (pathToRawInputs =file.path("D:/work/Displace/", paste0("DISPLACE_input_gis_","CelticSea")),
#                                       pathToDisplaceInputs = file.path("D:/work/Displace/", paste0("DISPLACE_input_","CelticSea")),
#                                       pathToOutputs =file.path("D:","DISPLACE_outputs"),
#                                       caseStudy="CelticSea",
#                                       iGraph=3,
#                                       iYear="2010", # Beginning of time series
#                                       iYearEnd="2020", # End of time series
#                                       iCountry=NULL, #???
#                                       nbPops=27,
#                                       nbSzgroup=14,
#                                       theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
#                                       nbSimus=20,
#                                       useSQLite=FALSE)

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

##################
###
###CREATE ONE EFFORT MAP GIF FOR THE WHOLE FISHERY OR PER METIER, ONE FRAME BEING A MONTH (AT THE SAMLLEST SCALE), FOR DIFFERENT AGGREGATION SCALES (NODE,RECT STAT, RTI RECTANGLE)
###
##################

# Per Node
getmapEffortNodeAll=function(effortPertrip,gif=FALSE,idMetier=0,timeStep=1,aggScale="month",scename=""){ # 3 sec per metier for one year
  plotrange = rbind(range(effortPertrip$Long),range(effortPertrip$Lat))
  if(is.na(idMetier)) {
    effortPertrip$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(aggScale=="month") effortPertrip$time = effortPertrip$month
  if(aggScale=="year") effortPertrip$time = floor((effortPertrip$month-1)/12)+2010
  if(is.na(timeStep) & !gif){
    effortPertrip$time="all time steps cumulated"
    timeStep="all time steps cumulated"
  } 
  
  
  VesselVmsLikeCond = effortPertrip %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(NodeId,Long,Lat,time,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(VesselVmsLikeCond$effort)
    VesselVmsLikeCond = VesselVmsLikeCond %>% 
      filter(time==timeStep)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(VesselVmsLikeCond$effort)
    mapEffortNode = VesselVmsLikeCond %>% 
      ggplot(aes(x=Long,y=Lat,colour=effort, group=seq_along(time)))+
      geom_point(size=1)+
      scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",colour="Effort\n(hours)",title=paste("Fishing effort month {frame} ", " metier ",idMetier,"\n",scename,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(time)
    
    if(aggScale=="month") plotMapAnim = animate(mapEffortNode, renderer = gifski_renderer(),duration=length(unique(VesselVmsLikeCond$time))/4, nframes=length(unique(VesselVmsLikeCond$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(mapEffortNode, renderer = gifski_renderer(),duration=length(unique(VesselVmsLikeCond$time)), nframes=length(unique(VesselVmsLikeCond$time)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
    mapEffortNode =VesselVmsLikeCond %>% 
      ggplot(aes(x=Long,y=Lat,colour=effort))+
      geom_point(size=1)+
      scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",colour="Effort\n(hours)",title=paste("Fishing effort time step ",timeStep," metier ",idMetier,"\n",scename,sep=""))+
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
                            
getmapEffortRTIAll=function(polygonsRTI,gif=FALSE,idMetier=0,timeStep=1,aggScale="month",scename=""){ #if metier is NA : do the whole fishery instead

  plotrange = rbind(range(polygonsRTI$x),range(polygonsRTI$y))
  if(is.na(idMetier)) {
    polygonsRTI$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(aggScale=="month") polygonsRTI$time = polygonsRTI$month
  if(aggScale=="year") polygonsRTI$time = floor((polygonsRTI$month-1)/12)+2010
  if(is.na(timeStep) & !gif){
    polygonsRTI$time="all months cumulated"
    timeStep="all months cumulated"
  } 
  
  polygonsRTI = polygonsRTI %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(layer,x,y,time,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(polygonsRTI$effort)
    polygonsRTI = polygonsRTI %>% 
      filter(time==timeStep)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(polygonsRTI$effort)
    mapEffortRTITemp = polygonsRTI %>% 
      ggplot(aes(x=x,y=y,fill=effort, group=seq_along(time)))+
      geom_tile(colour="black",width=0.5,height=0.25)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort time step {frame} ", " metier ",idMetier,"\n",scename ,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(time)
    
    if(aggScale=="month") plotMapAnim = animate(mapEffortRTITemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$effortPertrip$time))/4, nframes=length(unique(effortPertrip$effortPertrip$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(mapEffortRTITemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$time)), nframes=length(unique(effortPertrip$effortPertrip$time)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_RTIcell_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_RTIcell_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
      mapEffortRTITemp =polygonsRTI %>% 
        ggplot(aes(x=x,y=y,fill=effort))+
        geom_tile(colour="black",width=0.5,height=0.25)+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort time step ",timeStep," metier ",idMetier,"\n",scename,sep=""))+
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

getmapEffortICESAll=function(polygonsICES,gif=FALSE,idMetier=0,timeStep=1,aggScale="month",scename=""){ # 3 sec per metier for one year
  # timeStep=NA
  # idMetier=NA
  # gif=F
  plotrange = rbind(range(polygonsICES$x),range(polygonsICES$y))
  if(is.na(idMetier)) {
    polygonsICES$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(aggScale=="month") polygonsICES$time = polygonsICES$month
  if(aggScale=="year") polygonsICES$time = floor((polygonsICES$month-1)/12)+2010
  if(is.na(timeStep) & !gif){
    polygonsICES$time="all months cumulated"
    timeStep="all months cumulated"
  } 
  
  polygonsICES = polygonsICES %>% 
    filter(metierId==idMetier) %>%  # Still selects the whole fishery or all time steps if that's what's wanted
    group_by(layer,x,y,time,metierId) %>% 
    summarize(effort=sum(effort, na.rm = TRUE)) %>% 
    filter(effort>0)
  
  if(!gif){
    maxScale = max(polygonsICES$effort)
    polygonsICES = polygonsICES %>% 
      filter(time==timeStep)  # Still selects the whole fishery or all time steps if that's what's wanted
  }
  
  if(gif){
    maxScale = max(polygonsICES$effort)
    mapEffortICESTemp = polygonsICES %>% 
      ggplot(aes(x=x,y=y,fill=effort, group=seq_along(time)))+
      geom_tile(colour="black",width=1,height=0.5)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort time step {frame} ", " metier ",idMetier,"\n",scename ,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(time)
    
    if (aggScale=="month") plotMapAnim = animate(mapEffortICESTemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$time))/4, nframes=length(unique(effortPertrip$time)),height=800,width=800,units="px",res=100)
    if (aggScale=="year") plotMapAnim = animate(mapEffortICESTemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$time)), nframes=length(unique(effortPertrip$time)),height=800,width=800,units="px",res=100)
    
    if(idMetier=="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_ICEScell_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(idMetier!="whole fishery") anim_save(animation=plotMapAnim,filename=paste("effort_ICEScell_All_",idMetier,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
  
  if(!gif){
    mapEffortICESTemp =polygonsICES %>% 
      ggplot(aes(x=x,y=y,fill=effort))+
      geom_tile(colour="black",width=1,height=0.5)+
      scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
      labs(x="Longitude",y="Latitude",fill="Effort\n(hours)",title=paste("Fishing effort time step ",timeStep," metier ",idMetier,"\n",scename,sep=""))+
      expand_limits(x=plotrange[1,],y=plotrange[2,])+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))
    return(mapEffortICESTemp)
  }
}

# getmapEffortNodeAll(VesselVmsLikeCond,gif=FALSE,idMetier=10,monthNum=1)
# getmapEffortRTIAll(polygonsRTI,gif=FALSE,idMetier=NA,monthNum=10)
# getmapEffortICESAll(polygonsICES,gif=FALSE,idMetier=NA,timeStep=52,aggScale="month")

##################
###
###CREATE TIME SERIES PLOTS
###
##################

# effortPertrip = effortTimeSeries
# aggScale="month"
# metNum="All"
# ybeg=2010
# cumulTime=F

getEffortTimeSeries <- function(effortPertrip,aggScale="month",metNum="All",ybeg=2010,cumulTime=F){

  effortPertrip = effortPertrip %>% 
    group_by(month,metierId,scename) %>% 
    summarize(effort=sum(effort,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(year=floor((month-1)/12)) %>% 
    mutate(time=month)
  
  if(aggScale=="year") effortPertrip$time = effortPertrip$year + ybeg
  
  if("All" %in% metNum){
    metNum = "All"
    effortPertrip$metierId="All"
  }
  
  if(!"All" %in% metNum){
    effortPertrip = effortPertrip %>% 
      filter(metierId%in%metNum) %>% 
      mutate(metierId = sapply(metierId, function(x) metierNames$name[metierNames$metierId==x]))
  }
  
  plot2return = effortPertrip %>% 
    group_by(time,metierId,scename) %>% 
    summarize(effort=sum(effort,na.rm=T)) %>% 
    ungroup()
  
  #Cumulative option
  if(cumulTime){
    plot2return = plot2return %>% 
      arrange(time,metierId,scename) %>% 
      group_by(metierId,scename) %>% 
      mutate(effort=cumsum(effort)) %>% 
      ungroup()
  }
  
  plot2return = plot2return %>% 
    ggplot(aes(x=time,y=effort,colour=scename,linetype=scename,shape=scename))+
      geom_line(size=1)+
      geom_point(size=2)+
      facet_wrap(~metierId,scales="free_y")+
      labs(x=paste("Time step (",aggScale,")",sep=""),y="Effort\n(hours)", title ="Effort")+
      scale_colour_manual(name="Scenario", values=rep(scales::hue_pal()(5),3)[1:length(unique(effortPertrip$scename))])+
      scale_linetype_manual(name="Scenario", values=c(rep(1:2,each=5),3)[1:length(unique(effortPertrip$scename))])+
      scale_shape_manual(name="Scenario", values=c(rep(c(15:16),each=5),17)[1:length(unique(effortPertrip$scename))])+
      expand_limits(y=0)+
      theme_minimal()+
      theme(axis.title.y = element_text(angle=0,vjust=0.5))
  
  return(plot2return)
}