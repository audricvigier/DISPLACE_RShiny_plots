######### READ DISPLACE BIOMASS OUTPUTS (abundance/numbers, pop, area, size, time, SSB)
#
# DATE ..........................: Mon May 17 14:50:00 2021
# AUTHOR.........................: Audric Vigier
#
#----------------------------------------------------------------
#
# R version......................: R version 3.6.0 (2019-04-26)
#----------------------------------------------------------------

# rm(list=ls())
# library(raster)
# library(reshape2)
# library(rgdal)
# library(RSQLite) # To handle databases https://statkclee.github.io/R-ecology-lesson/06-r-and-sql.html
# library(sp)
# library(tidyverse)
# library(gifski)
# library(gganimate)
# library(grDevices)
# library(viridis)
# source("D:/work/Displace/DISPLACE_RShiny_plots/R-scripts/setGeneralVariable.R", local = TRUE)
# # library(displaceplot) # Some functions exist to produce some plots, but not the ones I'm looking for.
# # library(help=displaceplot)

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

# LOAD STUFF STILL GENERATE WITH useDisplacePlots.R
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

# # Catch in Pop Values. Any difference with log like (= non explicit?) If so, I could track what I want.
# sce=general$namefolderoutput[1]
# myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2]),"_out.db",sep=""))
# dbListTables(myConn)
# 
# #dbListFields(myConn,"VesselLogLike")
# PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
# PopDyn = dbGetQuery(myConn,"SELECT * FROM PopDyn") # To get pop dynamics (spatially aggregated)
# NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # To get Irish registered catch only population
# 
# dbDisconnect(myConn) # Close connection


##################
###
### FIT TO BIOMASS (INDICATIVE, BASED ON WHAT I USED FOR THE SSM)
###
##################

##################
# Compare to "observations" / get global N and B

#Load observations
getBiomassObs = function(){
  load("D:/work/Displace/batch/CALIBRATION/datasets/observationsCatchBiomass4SSMDefinitive.Rdata")
  
  biomassObs = biomassData %>% 
    filter(variable%in%c("2010","2011","2012")) %>% 
    select(-c(ponderation)) %>% 
    rename(species=Species)
  
  return(biomassObs)
}

getStockNames = function(){
  codes=read.table(file=paste(general$main.path.ibm, "/pop_names_CelticSea.txt",sep=""),header=T)
  stockNames=read.table(file=paste(general$main.path.param,"/POPULATIONS/Stock_biological_traits.csv",sep=""),header=T,sep=",") %>% 
    select(c(stock,species)) %>% 
    rename(spp=stock) %>% 
    merge(codes,by=c("spp")) %>% 
    rename(PopId=idx)
}

getBiomassFitPlot = function(interim,stockNames,biomassObs){
  biomassPlot = interim %>%
    merge(stockNames, by=c("PopId")) %>%
    group_by(TStep,species) %>% 
    summarize(TotalW=sum(TotalW)) %>% 
    mutate(variable=2010+floor(TStep/8762)) %>% 
    merge(biomassObs, by=c("species","variable")) %>% 
    rename(year=variable) %>% 
    mutate(TotalW = TotalW/1000 ) %>%  # Weight in tons
    melt(id.vars=c("species","TStep","year")) %>% 
    ggplot(aes(TStep,value,colour=variable,linetype=variable)) +
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~species, scales="free_y")+
    labs(x="Time step (month)",y="Biomass\n(tons)",colour="",linetype="")+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
  return(biomassPlot)
}

getAbundancePlot = function(interim,stockNames,biomassObs){
  abundancePlot = interim %>%
    merge(stockNames, by=c("PopId")) %>%
    group_by(TStep,species) %>% 
    summarize(TotalN=sum(TotalN)) %>% 
    mutate(variable=2010+floor(TStep/8762))%>% 
    rename(year=variable) %>% 
    ggplot(aes(TStep,TotalN)) +
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~species, scales="free_y")+
    labs(x="Time step (month)",y="Abundance\n(thousands)")+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
  return(abundancePlot)
}

# biomassObs = getBiomassObs()
# stockNames = getStockNames()

# Create an interim dataset to reduce computation time
getInterim = function(PopValues){
  interim = PopValues %>% 
    group_by(TStep,PopId,NodeId) %>% # Eliminate duplicates rows at last time step
    filter(row_number() == 1)
  
  return(interim)
}

# interimMap = getInterim(PopValues)
# 
# interim = interimMap %>% 
#   group_by(TStep,PopId) %>%
#   summarize(TotalW=sum(TotalW),TotalN=sum(TotalN),CumCatches=sum(CumCatches),CumDiscards=sum(CumDiscards)) # N is in thousands
# 
# biomassPlot = getBiomassFitPlot(interim,stockNames,biomassObs)
# abundancePlot = getAbundancePlot(interim,stockNames,biomassObs)

#Pb: a lot of populations' biomass collapse, despite the model being at equilibrium when in standalone version.

##################
# Compare to what I intended to put in from the SSM
getSSMBiomass = function(){
  load("D:/work/SSM_Celtic_Sea_PaperRuns/calibration_run2_inter7/output/ExplorationKappaKri_Corr_Whole.Rdata")
  species = data.frame(stockName = runBestGA[[8]]$cnames, PopId = c(0,25,10,26,1,4,6,2,8,20,3,9,5,24,7,23,11,11,16,19,18,22,21,15,17), wInf = runBestGA[[8]]$wInf)
  SSMoutputBiomass = species%>% 
    mutate(`2010`=t(runBestGA[[7]][[129]]$Biomass/1000000)) %>% # Late 2009 = Beginning 2010
    mutate(`2011`=t(runBestGA[[7]][[130]]$Biomass/1000000)) %>% # Late 2009 = Beginning 2010
    mutate(`2012`=t(runBestGA[[7]][[131]]$Biomass/1000000)) %>% # Late 2009 = Beginning 2010
    melt(id.vars=c("stockName","PopId","wInf")) %>% 
    rename(biomassTons=value) %>% 
    group_by(PopId,variable) %>% 
    summarise(biomassTons=sum(biomassTons)) %>% 
    ungroup() %>%  # In tons
    merge(stockNames, by=c("PopId")) %>% 
    select(-c(spp))
  return(SSMoutputBiomass)
}

getBiomassInputPlot = function(interim,stockNames,SSMoutputBiomass){
  biomassPlot = interim %>% 
    merge(stockNames, by=c("PopId")) %>%
    group_by(TStep,species) %>% 
    summarize(TotalW=sum(TotalW))  %>% 
    mutate(variable=2010+floor(TStep/8762)) %>%  
    merge(SSMoutputBiomass, by=c("species","variable")) %>% 
    rename(year=variable) %>% 
    mutate(TotalW = TotalW/1000 ) %>%  # Weight in tons
    select(-c(PopId)) %>% 
    melt(id.vars=c("species","TStep","year")) %>% 
    ggplot(aes(TStep,value,colour=variable,linetype=variable)) +
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~species, scales="free_y")+
    labs(x="Time step (month)",y="Abundance",colour="",linetype="")+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
  return(biomassPlot)
}

# SSMoutputBiomass = getSSMBiomass()
# biomassInputPlot = getBiomassInputPlot(interim,stockNames,SSMoutputBiomass)

##################
# Get SSB (time,pop)
getSSB = function(PopDyn){
  SSBPlot = PopDyn %>% 
    group_by(TStep,PopId)  %>%
    merge(stockNames, by=c("PopId")) %>%
    group_by(TStep,species) %>%  
    summarize(SSB=sum(SSB)/1000) %>% # Convert in tons 
    ggplot(aes(TStep,SSB)) +
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~species, scales="free_y")+
    labs(x="Time step (month)",y="SSB\n(tons)")+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
  return(SSBPlot)
}

# SSBPlot = getSSBPlot(PopDyn)

##################
# Get N, B and SSB per length bin,pop,time. 1 list per N, B or SSB. Inside each list, 3 plots showing the same thing in a different way : 1 - x= time step, colours = size bin ; 2 - x = size bin, colours = time step


getNBSSBLengthBin = function(data2plot=biomassTimeSeries,aggScale="year",variable="N",colourVar="sizeBin"){
  NBSSBLengthBin = data2plot %>% 
    merge(stockNames, by=c("PopId")) %>%
    group_by(TStep,species,PopGroup,scename) %>%  
    mutate(B=SSB/PropMature) %>% 
    summarize(SSB=sum(SSB)/1000,B=sum(B)/1000,Nz=sum(Nz)) %>%   # Convert in tons 
    ungroup()
  
  if(aggScale=="year"){
    NBSSBLengthBin = NBSSBLengthBin %>% 
      mutate(TStep=factor(TStep,sort(unique(TStep)),labels=0:(length(unique(TStep))-1))) %>% 
      filter((as.numeric(levels(TStep))[TStep]-1)/12==floor((as.numeric(levels(TStep))[TStep]-1)/12)) %>% 
      mutate(TStep=as.numeric(levels(TStep))[TStep])
  }
  if(variable=="SSB"){
    NBSSBLengthBin$value=NBSSBLengthBin$SSB
    ylabel="SSB\n(tons)"
  }
  if(variable=="B"){
    NBSSBLengthBin$value=NBSSBLengthBin$B
    ylabel="B\n(tons)"
  }
  if(variable=="N"){
    NBSSBLengthBin$value=NBSSBLengthBin$Nz
    ylabel="N\n(1000s)"
  }
  
    if(colourVar=="sizeBin"){
      plot2render =NBSSBLengthBin %>% 
        mutate(PopGroup=as.factor(PopGroup)) %>% 
        ggplot(aes(TStep,value,colour=PopGroup,linetype=PopGroup,shape=PopGroup)) +
        geom_line(size=1)+
        geom_point(size=2)+
        facet_wrap(~species, scales="free_y")+
        geom_vline(xintercept=seq(0.5,max(NBSSBLengthBin$TStep),12),linetype="dotted")+
        labs(x="Time step (month)",y=ylabel,colour="Size bin",linetype="Size bin",shape="Size bin",title=unique(NBSSBLengthBin$scename))+
        expand_limits(y=0)+
        scale_colour_manual(values=rep(scales::hue_pal()(5),3)[1:14])+
        scale_linetype_manual(values=rep(1:3,each=5)[1:14])+
        scale_shape_manual(values=rep(16:18,each=5)[1:14])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
    }
    
    if(colourVar=="TStep"){
      aes1 = length(unique(NBSSBLengthBin$TStep))
      if(aes1>12) aes1=12
      aes2 = 1+floor((length(unique(NBSSBLengthBin$TStep))-1)/12)
      colourPalette = rep(scales::hue_pal()(aes1),aes2)[1:length(unique(NBSSBLengthBin$TStep))]
      linetypes = rep(1:aes2,each=aes1)[1:length(unique(NBSSBLengthBin$TStep))]
      shapes = rep((15+1:aes2),each=aes1)[1:length(unique(NBSSBLengthBin$TStep))]
      plot2render = NBSSBLengthBin %>% 
        ungroup() %>% 
        mutate(TStep=as.factor(TStep)) %>% 
        ggplot(aes(PopGroup,value,colour=TStep,linetype=TStep,shape=TStep)) +
        geom_line(size=1)+
        geom_point(size=2)+
        facet_wrap(~species, scales="free_y")+
        labs(x="Size bin",y=ylabel,colour="Time\nstep\n(month)",linetype="Time\nstep\n(month)",shape="Time\nstep\n(month)",title=unique(NBSSBLengthBin$scename))+
        expand_limits(y=0)+
        scale_colour_manual(values=colourPalette)+
        scale_linetype_manual(values=linetypes)+
        scale_shape_manual(values=shapes)+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
    }
    
  return(plot2render)
}

# NBSSBLengthBinPlots = getNBSSBLengthBin(PopDyn)

#DO ANOTHER FUNCTION NOT ACCOUNTING FOR LENGTH BINS
getNBSSBTimeSeries = function(data2plot=biomassTimeSeries,aggScale="year",variable="SSB"){
  data2plot=biomassTimeSeries %>% 
    mutate(TStep=factor(TStep,sort(unique(TStep)),labels=0:(length(unique(TStep))-1)))
  
  if(aggScale=="year"){
    data2plot = data2plot %>% 
      filter((as.numeric(levels(TStep))[TStep]-1)/12==floor((as.numeric(levels(TStep))[TStep]-1)/12))
  }
  
  data2plot = data2plot %>% 
    merge(stockNames, by=c("PopId")) %>% 
    group_by(TStep,species,scename) %>%  
    mutate(B=SSB/PropMature) %>% 
    summarize(SSB=sum(SSB)/1000,B=sum(B)/1000,Nz=sum(Nz)) %>%   # Convert in tons 
    ungroup()
  
  if(variable=="SSB"){
    data2plot$value=data2plot$SSB
    ylabel="SSB\n(tons)"
  }
  if(variable=="B"){
    data2plot$value=data2plot$B
    ylabel="B\n(tons)"
  }
  if(variable=="N"){
    data2plot$value=data2plot$Nz
    ylabel="N\n(1000s)"
  }
  
  data2plot = data2plot  %>%
    mutate(TStep=as.numeric(levels(TStep)[TStep]))
  
  plot2return=ggplot(data2plot,aes(x=TStep,y=value,colour=scename,linetype=scename,shape=scename)) +
      geom_line(size=1)+
      geom_point(size=2)+
      geom_vline(xintercept=seq(0.5,max(data2plot$TStep),12),linetype="dotted")+
      facet_wrap(~species, scales="free_y")+
      labs(x="Time step (month)",y=ylabel,colour="Scenario",linetype="Scenario",shape="Scenario")+
      expand_limits(y=0)+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5),strip.text=element_text(face="italic"))
  return(plot2return)
}

##################
# Get maps of B per pop,time (no info in length bin structure, no possibility to do SSB maps)

# getBiomassMap = function(interimMap,coords,timeStep,popNum){
#   plotMap = interimMap %>%
#     ungroup() %>% 
#     filter(TStep==timeStep & PopId==popNum) %>% 
#     mutate(TotalW=TotalW/sum(TotalW)) %>% 
#     mutate(TotalW=replace(TotalW,TotalW<10^(-15),0)) %>% # Considered equal to 0 if biomass distribution is too low in some cells
#     merge(coords,by=c("NodeId")) %>% 
#     ggplot(aes(x=x,y=y,colour=TotalW))+
#       geom_point(size=1)+
#       #scale_colour_gradient(low = "green", high = "red")+
#       scale_colour_gradientn(trans="log",colours = terrain.colors(7),limits=c(10^(-15),1))+
#       labs(x="Longitude",y="Latitude",colour="Biomass\ndistribution\n(ratio)",title=paste("Population ",popNum,"  time step ",timeStep," biomass distribution",sep=""))+
#       theme_minimal()+
#       theme(axis.title.y=element_text(angle=0,vjust=0.5))
#   return(plotMap)
# }
# 
# BiomassMaps =list()
# for(popNum in sort(unique(interimMap$PopId))){
#   a=Sys.time()
#   #popNum=0
#   tstepIndex=0
#   BiomassMaps = list()
#   for(timeStep in sort(unique(interimMap$TStep))){
#     tstepIndex = tstepIndex+1
#     BiomassMaps[[tstepIndex]]=getBiomassMap(interimMap,coords,timeStep,popNum)
#   }
#   b=Sys.time()
#   b-a
# }

#Time
#5.860186 mins for 24*37 + 24 plots ; around 0.386 sec per plot
#Plots for pop 0 : Time difference of 16.37035 secs, BiomassMaps list arounf 75Mb

#Create gifs ; for one pop, creating the animation takes 36 sec per pop FOR 3 YEARS OF SIMULATION and only 1/4 frames
# 16.67197 mins for all pops



# #Shapefiles for ICES rectangles and RTI rectangles
# ## Create shapefile VERIFIED, ALL NAMES MATCH THE GOOD RECTANGLES
# icesquarterrectangle=raster(xmn=-13, xmx=-4, ymn=47.5, ymx=57, crs=CRS("+proj=longlat +datum=WGS84"), resolution=c(0.5,0.25)) # Create a raster bigger than necessary; encompass all the harbours!
# #values will be their ICES name. Main rectangle: ususal name. Quarter name : 1 upper left, 2 upper right, 3 lower left, 4 lower right
# xcoord=47:55 #D is replaced by 4; E is replaced by 5 since DISPLACE needs integers, hence are D7 to E5
# ycoord=seq(42,24,-1)
# icesNames=matrix(rep(paste(rep(ycoord, each=length(xcoord)),rep(xcoord,times=length(ycoord)),sep=""),each=2),ncol=length(ycoord))
# icesNames=icesNames[,rep(1:ncol(icesNames), each = 2) ]
# icesNames=paste(icesNames, rep(c(rep(c(1,2),times=length(xcoord)),rep(c(3,4),times=length(xcoord))),times=length(ycoord)),sep="")
# icesNames=matrix(icesNames,ncol=2*length(ycoord))
# icesNames=as.numeric(icesNames)
# RTIrectangle=setValues(icesquarterrectangle, icesNames)
# RTIrectangle=as.data.frame(RTIrectangle,xy=T)
# 
# icesquarterrectangle=raster(xmn=-13, xmx=-4, ymn=47.5, ymx=57, crs=CRS("+proj=longlat +datum=WGS84"), resolution=c(1,0.5)) # Create a raster bigger than necessary; encompass all the harbours!
# icesNames=matrix(paste(rep(ycoord, each=length(xcoord)),rep(xcoord,times=length(ycoord)),sep=""),ncol=length(ycoord))
# icesNames=as.numeric(icesNames)
# icesquarterrectangle=setValues(icesquarterrectangle, icesNames)
# icesquarterrectangle=as.data.frame(icesquarterrectangle,xy=T)
# 
# nodes2merge=subset(NodesDef,select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0) %>% 
#   rename(rtirectangle=icesrectanglecode) %>% 
#   mutate(icesrectanglecode=as.numeric(sapply(as.character(rtirectangle), function(x) substr(x,1,4))))
# 
# interimMap = interimMap %>% 
#   group_by(TStep,PopId) %>% 
#   mutate(TotalW=TotalW/sum(TotalW)) %>% 
#   ungroup() %>% 
#   mutate(TotalW=replace(TotalW,TotalW<10^(-15),0)) %>% # Considered equal to 0 if biomass distribution is too low in some cells
#   merge(nodes2merge, by=c("NodeId"))
# 
# interimMap = interimMap %>% 
#   mutate(TStep=as.factor(TStep))
# 
# levels(interimMap$TStep)=0:(length(levels(interimMap$TStep))-1)
# interimMap$TStep=as.numeric(levels(interimMap$TStep))[interimMap$TStep]
# 
# save(interimMap,RTIrectangle,icesquarterrectangle,file=paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))

getBiomassMapNode= function(interimMap,popNum,timeStep=NA,gif=F,scename=sce,scale="Node"){
  
  plotMap = interimMap %>%
    filter(PopId==popNum)
  
  # if(scale=="RTI rectangle"){ 
  #   plotMap = plotMap %>% 
  #     rename(layer=rtirectangle) %>% 
  #     group_by(TStep,layer) %>% 
  #     summarize(TotalW=sum(TotalW,na.rm=T)) %>% 
  #     merge(RTIrectangle, by=c("layer")) %>% 
  #     rename(Long=x,Lat=y)
  # }
  # 
  # if(scale=="ICES rectangle"){ 
  #   plotMap = plotMap %>% 
  #     rename(layer=icesrectanglecode) %>% 
  #     group_by(TStep,layer) %>% 
  #     summarize(TotalW=sum(TotalW,na.rm=T)) %>% 
  #     merge(icesquarterrectangle, by=c("layer")) %>% 
  #     rename(Long=x,Lat=y)
  # }
  
  if(!gif){
    plotMap = plotMap %>%
      filter(TStep==timeStep) %>% 
      ggplot(aes(x=Long,y=Lat))+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))
    
    if(scale=="Node"){ 
      plotMap = plotMap+
        geom_point(size=1,aes(colour=TotalW))+
        scale_colour_gradientn(trans="log",colours = viridis(7),limits=c(10^(-15),1))+
        labs(x="Longitude",y="Latitude",colour="Biomass\ndistribution\n(ratio)",title=paste("Population ",popNum,"  month  ",timeStep," biomass distribution\n",scename,sep=""))
    }else{
      plotMap = plotMap+
        geom_tile(size=1,aes(fill=TotalW),colour="black")+
        scale_fill_gradientn(trans="log",colours = viridis(7),limits=c(10^(-15),1))+
        labs(x="Longitude",y="Latitude",fill="Biomass\ndistribution\n(ratio)",title=paste("Population ",popNum,"  month  ",timeStep," biomass distribution\n",scename,sep=""))
    }
    
    return(plotMap)
    
  }
  if(gif){
    plotMap = plotMap %>%
      ggplot(aes(x=Long,y=Lat, group=seq_along(TStep)))+
      theme_minimal()+
      theme(axis.title.y=element_text(angle=0,vjust=0.5))+ 
      transition_manual(TStep)
    
    if(scale=="Node"){ 
      plotMap = plotMap+
        geom_point(size=1,aes(colour=TotalW))+
        scale_colour_gradientn(trans="log",colours = viridis(7),limits=c(10^(-15),1))+
        labs(x="Longitude",y="Latitude",colour="Biomass\ndistribution\n(ratio)",title=paste("Population ",popNum,"  month  {frame} biomass distribution\n",scename,sep=""))
    }else{
      plotMap = plotMap+
        geom_tile(size=1,aes(fill=TotalW),colour="black")+
        scale_fill_gradientn(trans="log",colours = viridis(7),limits=c(10^(-15),1))+
        labs(x="Longitude",y="Latitude",fill="Biomass\ndistribution\n(ratio)",title=paste("Population ",popNum,"  month  {frame} biomass distribution\n",scename,sep=""))
    }
    
    plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(interimMap$TStep))/4, nframes=length(unique(interimMap$TStep))/4,height=800,width=800,units="px",res=100) #36 sec Only way to reduce rendering time is to reduce the number of frames / I can't render in svg (I tried...)
    if(scale=="Node"){ 
      anim_save(animation=plotMapAnim,filename=paste("biomass_",popNum,"_",scename,".gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    }
    if(scale=="RTI rectangle"){ 
      anim_save(animation=plotMapAnim,filename=paste("biomass_",popNum,"_",scename,"_RTI.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    }
    if(scale=="ICES rectangle"){ 
      anim_save(animation=plotMapAnim,filename=paste("biomass_",popNum,"_",scename,"_ICES.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    }
  }
}

# a=Sys.time()
# for(popNum in sort(unique(interimMap$PopId))){
#   getBiomassMapNode(interimMap,coords,popNum,gif=T,timeStep=NA)
# }
# b=Sys.time()
# b-a

# getBiomassMapNode(interimMap,popNum=0,timeStep=1,gif=F,scename=sce,scale="ICES rectangle")
# very few variations, apart from areas getting quickly depleted for most populations.

