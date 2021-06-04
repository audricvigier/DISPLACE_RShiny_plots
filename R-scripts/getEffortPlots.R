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

# Per node
getmapEffortNodeAll=function(effortPertrip,gif=FALSE,idMetier=0,monthNum=1,scename=""){ # 3 sec per metier for one year
  plotrange = rbind(range(effortPertrip$Long),range(effortPertrip$Lat))
  if(is.na(idMetier)) {
    effortPertrip$metierId="whole fishery"
    idMetier="whole fishery"
  }
  if(is.na(monthNum) & !gif){
    effortPertrip$month="all months cumulated"
    monthNum="all months cumulated"
  } 
  
  VesselVmsLikeCond = effortPertrip %>% 
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
    
    plotMapAnim = animate(mapEffortRTITemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$month))/4, nframes=length(unique(effortPertrip$month)),height=800,width=800,units="px",res=100)
    
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
    
    plotMapAnim = animate(mapEffortICESTemp, renderer = gifski_renderer(),duration=length(unique(effortPertrip$month))/4, nframes=length(unique(effortPertrip$month)),height=800,width=800,units="px",res=100)
    
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