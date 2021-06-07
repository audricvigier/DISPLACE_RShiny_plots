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
# # library(displaceplot) # Some functions exist to produce some plots, but not the ones I'm looking for.
# # library(help=displaceplot)
#source("D:/work/Displace/DISPLACE_RShiny_plots/R-scripts/setGeneralVariable.R", local = TRUE)

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
# explicit_pops = 0:(general$nbpops-1)

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

# sce=general$namefolderoutput[1]
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
###CATCH TIME SERIES: FRACTION, POP, TIME STEP (MONTH), METIER, SIZE, IMPLICIT/EXPLICIT. AREA INFO ONLY FOR THE MAPS. I IGNORE SIZE OF COMPOSITION OF CTACH AT THE MOMENT.
###
##################

# ##################
# # Prepare dataset
# 
# getStockNames = function(){
#   codes=read.table(file=paste(general$main.path.ibm, "/pop_names_CelticSea.txt",sep=""),header=T)
#   stockNames=read.table(file=paste(general$main.path.param,"/POPULATIONS/Stock_biological_traits.csv",sep=""),header=T,sep=",") %>%
#     select(c(stock,species)) %>%
#     rename(spp=stock) %>%
#     merge(codes,by=c("spp")) %>%
#     rename(PopId=idx)
# }
# 
# stockNames = getStockNames() %>%
#   arrange(PopId)
# 
# getMetierNames = function(){
#   codes=read.table(file=paste(general$main.path.ibm, "/metiersspe_CelticSea/metier_names.dat",sep=""),header=T) %>%
#     rename(metierId=idx)
# }
# 
# metierNames = getMetierNames() %>%
#   arrange(metierId)
# 
# months = data.frame(TStep = sort(unique(NodesStat$TStep)), month= 1:length(sort(unique(NodesStat$TStep))))
# months = data.frame(TStep = c(sort(unique(NodesStat$TStep)),(max(NodesStat$TStep)+100)), month= c(1:length(sort(unique(NodesStat$TStep))),length(sort(unique(NodesStat$TStep))))) # Adding one more row to avoid crashes
# 
# nodes2merge=subset(NodesDef,select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0) %>%
#   rename(rtirectangle=icesrectanglecode) %>%
#   mutate(icesrectanglecode=as.numeric(sapply(as.character(rtirectangle), function(x) substr(x,1,4))))
# 
# metierCorr= unique(subset(VesselLogLike, select=c(Id,metierId)))
# 
# fishingLocations = VesselVmsLike %>% 
#   filter(State==1) %>% 
#   select(-c(Course,CumFuel,State,TStep)) %>% 
#   group_by(Id,TStepDep,Long,Lat) %>% 
#   summarize(effort=n()) %>% 
#   group_by(Id,TStepDep) %>% 
#   mutate(prop=effort/sum(effort))
# 
# catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
# names(catchAndEffortPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
# 
# catchPertrip = catchAndEffortPertrip %>% 
#   mutate(effort=TStep-TStepDep-cumsteaming) %>% # Use it for CPUE plots
#   select(c(effort,TStep,TStepDep,Id,freq_metiers,starts_with("pop."),starts_with("disc_"))) %>% 
#   mutate(metierId = sapply(as.character(freq_metiers), function(x) strsplit(x,split=")")[[1]][1])) %>% 
#   mutate(metierId = as.numeric(sapply(metierId, function(x) strsplit(x,split="\\(")[[1]][2]))) %>% 
#   select(-freq_metiers) %>% 
#   merge(fishingLocations,by=c("Id","TStepDep"),all.y=T) %>% 
#   #mutate(sanityCheck=abs(effort.y/prop-effort.x)) # All good, except what is cut at the end of first year. Keep effort.x and prop
#   mutate(effort=effort.x*prop) %>% 
#   select(-c(effort.x,effort.y)) %>% 
#   merge(subset(NodesDef, select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0), by=c("Long","Lat")) %>% 
#   melt(id.vars=c("Long","Lat","Id","TStepDep","TStep","metierId","prop","effort","NodeId","icesrectanglecode")) %>% 
#   mutate(variable=as.character(variable)) %>% 
#   mutate(Fraction=fct_recode(factor(sapply(variable, function(x) substr(x,1,3))),"Discards"="dis","Landings"="pop")) %>% 
#   mutate(PopId=as.numeric(unlist(regmatches(x=variable, m=gregexpr("[[:digit:]]+",variable))))) %>% 
#   mutate(value=prop*value) %>% 
#   group_by(Long,Lat,Id,TStep,metierId,NodeId,icesrectanglecode,Fraction,PopId) %>% 
#   summarize(effort=sum(effort,na.rm=T),value=sum(value,na.rm=T)) %>% 
#   ungroup() %>% 
#   mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>x]))] )) %>% 
#   group_by(Long,Lat,month,metierId,NodeId,icesrectanglecode,Fraction,PopId) %>%
#   summarize(effort=sum(effort,na.rm=T),value=sum(value,na.rm=T)) %>% 
#   ungroup()
# 
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

##################
# WARNING : these plots are not meant for quality of fit evaluation, hence implicit catch can be accounted for on 2010:2011

# Get implicit and explicit catch and discards for all time steps (months), metiers, pops NOT SPATIAL
getExplicitCatch = function(VesselLogLike,VesselLogLikeCatches,months,fortnights,nodes2merge,step="month"){
  
  stepLogLikeId = VesselLogLike %>% 
    select(c(RowId,TStepDep,TStep,metierId)) %>% 
    rename(LoglikeId=RowId)
  
  cumcatchLog=VesselLogLikeCatches %>% 
    merge(stepLogLikeId, by=c("LoglikeId"),all=T) %>% 
    select(-c(TStepDep))%>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] )) %>% 
    mutate(fortnight = sapply(TStep, function(x) fortnights$fortnight[which(fortnights$TStep==min(fortnights$TStep[fortnights$TStep>x]))] ))
  # TStep is the time step at the end of the month, e.g. 745 is the end of Jan 2010
  
  if(step=="month"){
    cumcatchLog = cumcatchLog %>% 
      group_by(PopId,month,metierId) %>% 
      summarise_at(c("Catches","Discards"),sum) %>% 
      rename(Landings=Catches) %>% 
      ungroup() %>% 
      mutate(year=2010+floor((month-1)/12)) %>%
      melt(id.vars=c("PopId","month","metierId","year")) %>% 
      rename(Fraction=variable) %>% 
      mutate(value=value/1000) # Convert to tons
  }
  if(step=="fortnight"){
    cumcatchLog = cumcatchLog %>% 
      group_by(PopId,fortnight,metierId) %>% 
      summarise_at(c("Catches","Discards"),sum) %>% 
      rename(Landings=Catches) %>% 
      ungroup() %>% 
      melt(id.vars=c("PopId","fortnight","metierId")) %>% 
      rename(Fraction=variable) %>% 
      mutate(value=value/1000) # Convert to tons
  }
  
  return(cumcatchLog)
}

#LIMITED TO 1 YEAR SO FAR BECAUS OF DISPLACE HARD CODING
getExplicitCatchSpatial = function(catchPertrip,step="month"){
  
  if(step=="month"){
    catch2return = catchPertrip %>% 
      mutate(value=value/1000) %>% # Convert to tons
      mutate(year=floor((month-1)/12)) %>%
      rename(rtirectangle=icesrectanglecode) %>% 
      mutate(icesrectanglecode=as.numeric(sapply(rtirectangle, function(x) substr(x,1,4))))
    return(catch2return)
  }
  if(step=="fortnight"){
    catch2return = catchPertrip %>% 
      mutate(value=value/1000) %>% # Convert to tons
      rename(rtirectangle=icesrectanglecode) %>% 
      mutate(icesrectanglecode=as.numeric(sapply(rtirectangle, function(x) substr(x,1,4))))
    return(catch2return)
  }    
  # stepLogLikeId = VesselLogLike %>% 
  #   select(c(RowId,TStepDep,TStep,metierId,Id)) %>% 
  #   unique() %>% 
  #   rename(LoglikeId=RowId)
  # 
  # cumcatchLog=VesselLogLikeCatches %>% 
  #   merge(stepLogLikeId, by=c("LoglikeId"),all=T) %>% 
  #   mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] ))
  # # TStep is the time step at the end of the month, e.g. 745 is the end of Jan 2010
  # 
  # cumcatchLog = cumcatchLog %>% 
  #   merge(subset(VesselVmsLikeCond, select=-c(TStep)), by=c("Id","TStepDep"))%>% 
  #   group_by(PopId,month,metierId,NodeId,icesrectanglecode,rtirectangle,Long,Lat) %>% 
  #   summarise_at(c("Catches","Discards"),sum) %>% 
  #   ungroup() %>% 
  #   mutate(year=2010+floor((month-1)/12)) %>%
  #   melt(id.vars=c("PopId","month","metierId","NodeId","icesrectanglecode","rtirectangle","Long","Lat","year")) %>% 
  #   rename(Fraction=variable) %>% 
  #   mutate(value=value/1000) # Convert to tons
  # 
  # return(cumcatchLog)
}

getImplicitCatch = function(PopValues,explicitCatch){
  cumcatchLog = explicitCatch %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    reshape2::dcast(.,PopId+month+year~Fraction,value.var="value")
  
  # 1'30" for 3 years, unavoidable
  ImplicitCatch = PopValues %>% 
    group_by(TStep,PopId,NodeId) %>% # Eliminate duplicates rows at last time step
    filter(row_number() == 1) %>% 
    ungroup()
  
  # 1.3sec for 3 years
  ImplicitCatch = ImplicitCatch %>% 
    group_by(TStep,PopId) %>% 
    summarise_at(c("CumDiscards","CumCatches"),sum) %>% # COnvert to tons
    ungroup() %>% 
    mutate(CumDiscards=CumDiscards/1000,CumCatches=CumCatches/1000) %>% 
    arrange(PopId,TStep) %>% 
    group_by(PopId) %>% 
    mutate(CumDiscards=c(min(CumDiscards),diff(CumDiscards)),CumCatches=c(min(CumCatches),diff(CumCatches))) %>% # From cumulative time series to time series
    ungroup() %>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>=x]))] )) %>% 
    group_by(month,PopId) %>% 
    summarise_at(c("CumDiscards","CumCatches"),sum) %>%
    ungroup() %>% 
    merge(cumcatchLog, by=c("month","PopId"))%>% # Convert to tons
    mutate(Landings =CumCatches-Landings, Discards =CumDiscards-Discards) %>% 
    select(-c(CumDiscards,CumCatches)) %>% 
    #group_by(month,PopId,year) %>% 
    melt(id.vars=c("PopId","month","year")) %>% 
    rename(Fraction=variable)
  return(ImplicitCatch)
}

#LIMITED TO 1 YEAR SO FAR BECAUS OF DISPLACE HARD CODING
getImplicitCatchSpatial = function(PopValues,explicitCatchSpatial,nodes2merge){
  cumcatchLog = explicitCatchSpatial %>% 
    group_by(PopId,month,year,Fraction,NodeId,icesrectanglecode,rtirectangle,Long,Lat) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    as.data.frame() %>% 
    reshape2::dcast(.,PopId+month+year+NodeId+icesrectanglecode+rtirectangle+Long+Lat~Fraction,value.var="value")
  
  ImplicitCatch = PopValues %>% # Takes 2'
    group_by(TStep,PopId,NodeId) %>% # Eliminate duplicates rows at last time step
    filter(row_number() == 1) %>% 
    select(-c(TotalN,TotalW,Impact)) %>% # COnvert to tons
    ungroup() %>%
    mutate(CumDiscards=CumDiscards/1000,CumCatches=CumCatches/1000) %>%  # Convert to tons
    arrange(NodeId,PopId,TStep) %>% 
    group_by(PopId,NodeId) %>% 
    mutate(CumDiscards=c(min(CumDiscards),diff(CumDiscards)),CumCatches=c(min(CumCatches),diff(CumCatches))) %>% # From cumulative time series to time series
    ungroup() %>% 
    mutate(month = as.factor(TStep))
  
  levels(ImplicitCatch$month)=0:(length(levels(ImplicitCatch$month))-1)
  
  ImplicitCatch = ImplicitCatch%>%
    mutate(month=as.numeric(levels(month))[month]) %>% 
    filter(month!=0)
  
  # 1'30 for 3 years.
  ImplicitCatch2 = list()
  for (monthNum in sort(unique(ImplicitCatch$month))){
    ImplicitCatch2[[monthNum]] = ImplicitCatch %>% 
      filter(month ==monthNum) %>% 
      merge(subset(cumcatchLog, month==monthNum), by=c("month","PopId","NodeId"),all.x=T) %>% 
      select(-c(icesrectanglecode,rtirectangle,Long,Lat,year)) %>%  # to be redone
      mutate(Landings=replace_na(Landings,0),Discards=replace_na(Discards,0))%>% 
      merge(nodes2merge,by=c("NodeId")) %>% 
      mutate(year=floor((month-1)/12))
  }
  
  ImplicitCatch2 = plyr::ldply(ImplicitCatch2) %>% 
    mutate(Landings =CumCatches-Landings, Discards =CumDiscards-Discards) %>% 
    select(-c(CumDiscards,CumCatches,TStep)) %>% 
    #group_by(month,PopId,NodeId,year,icesrectanglecode,rtirectangle,Long,Lat) %>% Useless to group it
    melt(id.vars=c("PopId","month","year","NodeId","icesrectanglecode","rtirectangle","Long","Lat")) %>% 
    rename(Fraction=variable)
  
  return(ImplicitCatch2)
}

# explicitCatch = getExplicitCatch(VesselLogLike,VesselLogLikeCatches,months,nodes2merge) # Includes discards, 13 sec for 3 years
# explicitCatchSpatial = getExplicitCatchSpatial(catchPertrip) # LIMITED TO ONE YEAR SO FAR BECAUSE OF DISPLACE HARD CODING
# implicitCatch = getImplicitCatch(PopValues,explicitCatchSpatial) # Includes discards 2' for 3 years
# implicitCatchSpatial = getImplicitCatchSpatial(PopValues,explicitCatchSpatial,nodes2merge)# WARNING: ONLY 2010 IMPLICIT CATCH IS PROPERLY DERIVED AT THAT SCALE DUE TO DISPLACE HARDCODING ON VMSLIKE TABLE; 4.42904 mins for 3 years
# 
# #save(explicitCatch,explicitCatchSpatial,implicitCatch,implicitCatchSpatial,file=paste(general$main.path,general$case_study,sce,"output/forCatchsPlots.Rdata",sep="/")) # 177MB
# 
# #save(explicitCatch,explicitCatchSpatial,implicitCatch,file=paste(general$main.path,general$case_study,sce,"output/forCatchsPlots.Rdata",sep="/")) # 5.5MB
# 
# #22 secs to load 3 years (177MB)
# #load(file=paste(general$main.path,general$case_study,sce,"output/forCatchsPlots.Rdata",sep="/"))
# 
# explicitCatchSpatialICES = explicitCatchSpatial %>%
#   group_by(PopId,month,metierId,icesrectanglecode,year,Fraction) %>%
#   summarize(value=sum(value)) %>%
#   rename(layer=icesrectanglecode) %>%
#   merge(icesquarterrectangle,by=c("layer"))
# 
# explicitCatchSpatialRTI = explicitCatchSpatial %>%
#   group_by(PopId,month,metierId,rtirectangle,year,Fraction) %>%
#   summarize(value=sum(value)) %>%
#   rename(layer=rtirectangle) %>%
#   merge(RTIrectangle,by=c("layer"))
# 
# implicitCatchSpatialICES = implicitCatchSpatial %>%
#   group_by(PopId,month,icesrectanglecode,year,Fraction) %>%
#   summarize(value=sum(value)) %>%
#   rename(layer=icesrectanglecode) %>%
#   merge(icesquarterrectangle,by=c("layer"))
# 
# implicitCatchSpatialRTI = implicitCatchSpatial %>%
#   group_by(PopId,month,rtirectangle,year,Fraction) %>%
#   summarize(value=sum(value)) %>%
#   rename(layer=rtirectangle) %>%
#   merge(RTIrectangle,by=c("layer"))
# 
# data2process = explicitCatchSpatial %>%
#   mutate(year=year-min(year)) %>%
#   select(-c(metierId)) %>%
#   bind_rows(implicitCatchSpatial)
# 
# data2process2= list()
# for(monthNum in sort(unique(data2process$month))){
#   data2process2[[monthNum]] = data2process %>%
#     filter(month==monthNum) %>%
#     group_by(PopId,month,NodeId,icesrectanglecode,rtirectangle,Long,Lat,year,Fraction) %>%
#     summarize(value=sum(value,na.rm=T)) %>%
#     ungroup()
# }
# allCatchSpatial=plyr::ldply(data2process2)
# 
# data2process = explicitCatchSpatialICES %>%
#   mutate(year=year-min(year)) %>%
#   select(-c(metierId)) %>%
#   bind_rows(implicitCatchSpatialICES)
# 
# data2process2= list()
# for(monthNum in sort(unique(data2process$month))){
#   data2process2[[monthNum]] = data2process %>%
#     filter(month==monthNum) %>%
#     group_by(layer,PopId,month,year,Fraction,x,y) %>%
#     summarize(value=sum(value,na.rm=T)) %>%
#     ungroup()
# }
# allCatchSpatialICES=plyr::ldply(data2process2)
# 
# data2process = explicitCatchSpatialRTI %>%
#   mutate(year=year-min(year)) %>%
#   select(-c(metierId)) %>%
#   bind_rows(implicitCatchSpatialRTI)
# 
# data2process2= list()
# for(monthNum in sort(unique(data2process$month))){
#   data2process2[[monthNum]] = data2process %>%
#     filter(month==monthNum) %>%
#     group_by(layer,PopId,month,year,Fraction,x,y) %>%
#     summarize(value=sum(value,na.rm=T)) %>%
#     ungroup()
# }
# allCatchSpatialRTI=plyr::ldply(data2process2)

# save(explicitCatch,explicitCatchSpatial,explicitCatchSpatialICES,explicitCatchSpatialRTI,file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsPlots.Rdata",sep="/"))
# save(implicitCatch,implicitCatchSpatial,implicitCatchSpatialICES,implicitCatchSpatialRTI,file=paste(general$main.path,general$case_study,sce,"output/forImplicitCatchsPlots.Rdata",sep="/"))
# save(allCatchSpatialRTI,allCatchSpatialICES,allCatchSpatial,file=paste(general$main.path,general$case_study,sce,"output/forAllCatchsPlots.Rdata",sep="/"))
# 
# load(file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsPlots.Rdata",sep="/"))
# load(file=paste(general$main.path,general$case_study,sce,"output/forImplicitCatchsPlots.Rdata",sep="/"))
# load(file=paste(general$main.path,general$case_study,sce,"output/forAllCatchsPlots.Rdata",sep="/"))

##################
# Prepare time series plots

getExplicitCatchTimeSeries = function(explicitCatch,aggScale="year",metierSel="All",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = explicitCatch %>% 
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
    summarize(value=sum(value)) %>% 
    filter(Fraction%in%chosenFraction) %>% 
    ungroup() %>% 
    ggplot(aes(x=time,y=value,colour=metierId,linetype=Fraction,shape=Fraction,group=interaction(metierId,Fraction,PopId)))+
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~PopId,scales="free_y")+
    labs(x=paste("Time step (",aggScale,")",sep=""),y="Catch\n(tons)",colour="Métier",linetype="Fraction",shape="Fraction", title = paste("Explicit catch time series\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5))
  return(data2plot)
}

getImplicitCatchTimeSeries = function(implicitCatch,aggScale="year",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = implicitCatch %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    group_by(PopId,Fraction,time) %>% 
    summarize(value=sum(value)) %>% 
    filter(Fraction%in%chosenFraction) %>% 
    ungroup() %>% 
    ggplot(aes(x=time,y=value,colour=Fraction,linetype=Fraction,shape=Fraction,group=interaction(Fraction,PopId)))+
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~PopId,scales="free_y")+
    labs(x=paste("Time step (",aggScale,")",sep=""),y="Catch\n(tons)",colour="Fraction",linetype="Fraction",shape="Fraction", title = paste("Implicit catch time series\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5))
  return(data2plot)
}

getAllCatchTimeSeries = function(explicitCatch,implicitCatch,aggScale="year",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  interim = explicitCatch %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    mutate(year=year-min(year))
  
  data2plot = implicitCatch %>% 
    bind_rows(interim) %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    group_by(PopId,Fraction,time) %>% 
    summarize(value=sum(value)) %>% 
    filter(Fraction%in%chosenFraction) %>% 
    ungroup() %>% 
    ggplot(aes(x=time,y=value,colour=Fraction,linetype=Fraction,shape=Fraction,group=interaction(Fraction,PopId)))+
    geom_line(size=1)+
    geom_point(size=2)+
    facet_wrap(~PopId,scales="free_y")+
    labs(x=paste("Time step (",aggScale,")",sep=""),y="Catch\n(tons)",colour="Fraction",linetype="Fraction",shape="Fraction", title = paste("Explicit + implicit catch time series\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5))
  return(data2plot)
}

##################
# Prepare bar plots

getExplicitCatchBarPlot = function(explicitCatch,aggScale="year",metierSel="All",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = explicitCatch %>% 
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
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x=interaction(Fraction,metierId),y=value,fill=PopId,group=interaction(metierId,Fraction,PopId)))+
    geom_bar(stat="identity",colour="black")+
    facet_wrap(~time)+
    labs(x="Métier and fraction",y="Catch\n(tons)",fill="Population", title = paste("Explicit catch species composition\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5),axis.text.x=element_text(angle=45,vjust=1,hjust=1))
  return(data2plot)
}

getImplicitCatchBarPlot = function(implicitCatch,aggScale="year",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  data2plot = implicitCatch %>% 
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    filter(Fraction%in%chosenFraction & time%in%c(min(time),max(time))) %>% 
    group_by(PopId,Fraction,time) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x=Fraction,y=value,fill=PopId,group=interaction(Fraction,PopId)))+
    geom_bar(stat="identity",colour="black")+
    facet_wrap(~time)+
    labs(x="Fraction",y="Catch\n(tons)",fill="Population", title = paste("Implicit catch species composition\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5),axis.text.x=element_text(angle=45,vjust=1,hjust=1))
  return(data2plot)
}

getAllCatchBarPlot = function(explicitCatch,implicitCatch,aggScale="year",popSel="All",chosenFraction=c("Landings","Discards"),sce=sce){
  interim = explicitCatch %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    mutate(year=year-min(year))
  
  data2plot = implicitCatch %>% 
    bind_rows(interim) %>% 
    group_by(PopId,month,year,Fraction) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>%   
    mutate(PopId=factor(PopId,stockNames$PopId,labels=stockNames$spp))
  
  if(aggScale=="month") data2plot$time=data2plot$month
  if(aggScale=="year") data2plot$time=data2plot$year
  if(!"All" %in% popSel) data2plot = subset(data2plot,PopId%in%levels(data2plot$PopId)[popSel])
  
  data2plot = data2plot %>% 
    filter(Fraction%in%chosenFraction & time%in%c(min(time),max(time))) %>% 
    group_by(PopId,Fraction,time) %>% 
    summarize(value=sum(value)) %>% 
    ungroup() %>% 
    ggplot(aes(x=Fraction,y=value,fill=PopId,group=interaction(Fraction,PopId)))+
    geom_bar(stat="identity",colour="black")+
    facet_wrap(~time)+
    labs(x="Fraction",y="Catch\n(tons)",fill="Population", title = paste("Explicit and implicit catch species composition\n",sce,sep=""))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5),axis.text.x=element_text(angle=45,vjust=1,hjust=1))
  return(data2plot)
}

##################
# Prepare maps


# data2process=explicitCatchSpatial
# popNum=7
# timeStep=2010
# fractionName="Landings"
# aggScale="year"
# metierNum=3
# resScale="All"
# gif=F

getExplicitCatchMap = function(data2process,popNum=0,timeStep=1,metierNum=0,fractionName="Landings",sce=sce,aggScale="month",resScale="All",gif=F){
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
      summarize(value=sum(value,na.rm=T)) %>% 
      ungroup()
  }else{
    data2plot=data2plot %>% 
      filter(metierId==metierNum) %>% 
      mutate(metierId=factor(metierId,metierNames$metierId,labels=metierNames$name)) %>% 
      group_by(PopId,time,metierId,layer,Long,Lat,Fraction) %>% 
      summarize(value=sum(value,na.rm=T)) %>% 
      ungroup()
  }
  
  if(!gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="RTI"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="ICES"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep," ", " metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    return(data2plot)
  }
  if (gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value, group=seq_along(time)))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="RTI"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="ICES"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Explicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}  metier ",unique(data2plot$metierId),"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    
    if(aggScale=="month") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=round(length(unique(data2plot$time))/4), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(data2plot$time)), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    
    if(resScale=="All") anim_save(animation=plotMapAnim,filename=paste("explicit_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="RTI") anim_save(animation=plotMapAnim,filename=paste("explicit_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_RTI.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="ICES") anim_save(animation=plotMapAnim,filename=paste("explicit_",fractionName,"_met",metierNum,"_pop",popNum,"_",aggScale,timeStep,"_ICES.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    
  }
}

#Test Option gif = F
#getExplicitCatchMap(explicitCatchSpatialRTI,popNum=7,timeStep=3,metierNum="All",fractionName="Discards",sce,aggScale="month",resScale="RTI",gif=F)
#Test Option gif = T
# popNum=7
# for(metierNum in c(3,"All")){
#   for(fractionName in c("Landings","Discards")){
#     for(aggScale in c("month","year")){
#       timeStep=1
#       if (aggScale=="year")timeStep = timeStep+2009
#       for(resScale in c("All","RTI","ICES")){
#         if (resScale=="All") data2process = explicitCatchSpatial
#         if (resScale=="ICES") data2process = explicitCatchSpatialICES
#         if (resScale=="RTI") data2process = explicitCatchSpatialRTI
#         getExplicitCatchMap(data2process,popNum,timeStep,metierNum,fractionName,sce,aggScale,resScale,gif=T)
#       }
#     }
#   }
# }

# data2process=implicitCatchSpatial
# popNum=7
# timeStep=0
# fractionName="Landings"
# aggScale="year"
# resScale="All"
# gif=F
# 
# sce

getImplicitCatchMap = function(data2process,popNum=0,timeStep=1,fractionName="Landings",sce=sce,aggScale="month",resScale="All",gif=F){
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
  data2plot = data2plot %>% 
    group_by(PopId,time,layer,Long,Lat,Fraction) %>% 
    summarize(value=sum(value,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(value=replace(value,value<0,0))
  
  if(!gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="RTI"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="ICES"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    return(data2plot)
  }
  if (gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value, group=seq_along(time)))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="RTI"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="ICES"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("Implicit ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    
    if(aggScale=="month") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=round(length(unique(data2plot$time))/4), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(data2plot$time)), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    
    if(resScale=="All") anim_save(animation=plotMapAnim,filename=paste("implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="RTI") anim_save(animation=plotMapAnim,filename=paste("implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_RTI.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="ICES") anim_save(animation=plotMapAnim,filename=paste("implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_ICES.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    
  }
}

# #Test Options
# popNum=7
# for(fractionName in c("Landings","Discards")){
#   for(aggScale in c("month","year")){
#     timeStep=1
#     if (aggScale=="year")timeStep = timeStep-1
#     for(resScale in c("All","RTI","ICES")){
#       if (resScale=="All"){ 
#         data2process = implicitCatchSpatial
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_All.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       if (resScale=="ICES"){ 
#         data2process = implicitCatchSpatialICES
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_ICES.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       if (resScale=="RTI"){ 
#         data2process = implicitCatchSpatialRTI
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/implicit_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_RTI.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       print(getImplicitCatchMap(data2process,popNum,timeStep,fractionName,sce,aggScale,resScale,gif=F))
#       dev.off()
#     }
#   }
# }
# 
# for(fractionName in c("Landings","Discards")){
#   for(aggScale in c("month","year")){
#     timeStep=1
#     if (aggScale=="year")timeStep = timeStep-1
#     for(resScale in c("All","RTI","ICES")){
#       if (resScale=="All") data2process = implicitCatchSpatial
#       if (resScale=="ICES") data2process = implicitCatchSpatialICES
#       if (resScale=="RTI") data2process = implicitCatchSpatialRTI
#       getImplicitCatchMap(data2process,popNum,timeStep,fractionName,sce,aggScale,resScale,gif=T)
#     }
#   }
# }


# popNum=7
# timeStep=0
# fractionName="Landings"
# aggScale="year"
# resScale="All"
# gif=F
# 
# sce

getAllCatchMap = function(data2process,popNum=0,timeStep=1,fractionName="Landings",sce=sce,aggScale="month",resScale="All",gif=F){
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
  data2plot = data2plot %>% 
    group_by(PopId,time,layer,Long,Lat,Fraction) %>% 
    summarize(value=sum(value,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(value=replace(value,value<0,0))
  
  if(!gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="RTI"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    if(resScale=="ICES"){
      data2plot = data2plot%>% 
        filter(time==timeStep & Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," ",timeStep,"\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))
    }
    return(data2plot)
  }
  if (gif){
    maxScale = max(data2plot$value[data2plot$Fraction==fractionName]) # Metier selection done previously
    
    if(resScale=="All"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,colour=value, group=seq_along(time)))+
        geom_point(size=1)+
        scale_colour_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",colour=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="RTI"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=0.5,height=0.25,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    if(resScale=="ICES"){
      plotMap = data2plot%>% 
        filter(Fraction==fractionName & value>0) %>% # Metier selection done previously
        ggplot(aes(x=Long,y=Lat,fill=value, group=seq_along(time)))+
        geom_tile(width=1,height=0.5,colour="black")+
        scale_fill_gradientn(colours = viridis(7),limits=c(0,maxScale))+
        labs(x="Longitude",y="Latitude",fill=paste(fractionName,"\n(tons)",sep=""),title=paste("All ",fractionName," ",levels(data2plot$PopId)," ",aggScale," {frame}\n",sce ,sep=""))+
        expand_limits(x=plotrange[1,],y=plotrange[2,])+
        theme_minimal()+
        theme(axis.title.y=element_text(angle=0,vjust=0.5))+
        transition_manual(time)
    }
    
    if(aggScale=="month") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=round(length(unique(data2plot$time))/4), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    if(aggScale=="year") plotMapAnim = animate(plotMap, renderer = gifski_renderer(),duration=length(unique(data2plot$time)), nframes=length(unique(data2plot$time)),height=800,width=800,units="px",res=100)
    
    if(resScale=="All") anim_save(animation=plotMapAnim,filename=paste("all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_All.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="RTI") anim_save(animation=plotMapAnim,filename=paste("all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_RTI.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    if(resScale=="ICES") anim_save(animation=plotMapAnim,filename=paste("all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_ICES.gif",sep=""),path=paste(general$main.path,general$case_study,sce,"output",sep="/"))
    
  }
}

# #Test Options
# popNum=7
# for(fractionName in c("Landings","Discards")){
#   for(aggScale in c("month","year")){
#     timeStep=1
#     if (aggScale=="year")timeStep = timeStep-1
#     for(resScale in c("All","RTI","ICES")){
#       if (resScale=="All"){
#         data2process = allCatchSpatial
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_All.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       if (resScale=="ICES"){
#         data2process = allCatchSpatialICES
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_ICES.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       if (resScale=="RTI"){
#         data2process = allCatchSpatialRTI
#         png(filename=paste(general$main.path,"/",general$case_study,"/",sce,"/output/all_",fractionName,"_pop",popNum,"_",aggScale,timeStep,"_RTI.png",sep=""),height=800,width=800,units="px",res=100)
#       }
#       print(getAllCatchMap(data2process,popNum,timeStep,fractionName,sce,aggScale,resScale,gif=F))
#       dev.off()
#     }
#   }
# }
# 
# for(fractionName in c("Landings","Discards")){
#   for(aggScale in c("month","year")){
#     timeStep=1
#     if (aggScale=="year")timeStep = timeStep-1
#     for(resScale in c("All","RTI","ICES")){
#       if (resScale=="All") data2process = allCatchSpatial
#       if (resScale=="ICES") data2process = allCatchSpatialICES
#       if (resScale=="RTI") data2process = allCatchSpatialRTI
#       getAllCatchMap(data2process,popNum,timeStep,fractionName,sce,aggScale,resScale,gif=T)
#     }
#   }
# }

