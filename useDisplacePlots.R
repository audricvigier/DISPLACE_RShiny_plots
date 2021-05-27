######### USE DISPLATE PLOTS + SHINY APP TO READ DISPLACE OUTPUTS
#
# DATE ..........................: Mon May 24 17:19:34 2021
# AUTHOR.........................: Audric Vigier
#
#----------------------------------------------------------------
#
# R version......................: R version 3.6.0 (2019-04-26)
#----------------------------------------------------------------

rm(list=ls())
library(broom)
library(gifski)
library(raster)
library(gganimate)
library(grDevices)
library(lattice)         
library(plyr)
library(reshape2)
library(rgdal)
library(RSQLite) # To handle databases https://statkclee.github.io/R-ecology-lesson/06-r-and-sql.html
library(sp)
library(tidyverse)
library(viridis) # colour-blind friendly palettes
displaceplotLib="D:/work/Displace/displaceplot/R"
for (file in list.files(displaceplotLib)) source(paste(displaceplotLib,file,sep="/"))
shinyLib="D:/work/Displace/DISPLACE_RShiny_plots/R-scripts"
for (file in list.files(shinyLib,pattern=".R")[-c(5,7,8)]) source(paste(shinyLib,file,sep="/")) # Issue with "makeStudyAreaMap.R" 
for (file in list.files(paste(shinyLib,"/fromFrancois"),pattern=".R")) source(paste(paste(shinyLib,"/fromFrancois"),file,sep="/"))

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

##################
###
###END INPUTS
###
##################

##################
###
###CONDITION THE OUTPUTS - WRITE FILES AND .RDATA WITH THE GETTERS
###USE THESE CONDTIONNED OUTPUTS FOR MANY NON-SPATIAL PLOTS
###
##################

#Landings, CPUE and economics per pop VID, metier, year, month. No spatial dimension
getAggLoglikeFiles(general,explicit_pops=0:26,implicit_pops=NULL,what="weight")
getAggLoglikeFiles(general,explicit_pops=0:26,implicit_pops=NULL,what="CPUE")

#N IN THOUSANDS per group, time step and pop. No spatial dimension. Also does some plots. WHATEVER THESE PLOTS are supposed to be, redo them with tidyverse. (an SSB plot I've done better (check though); N per size class pop and month (redo with tidyverse)). ALso throws an error because legends are poorly managed.
getAggPoplikeFiles(general=general,explicit_pops=0:26,the_baseline ="calib_multipliers_")
  
#Produce average spatial layers over simulations, at a specific time step. Generates a text file with the average layer. How may should I generate (time step, type ?)
#a_type : anything in that list cumcatches cumcatches_with_threshold cumdiscards cumdiscardsratio cumftime cumsweptarea cumulcatches end inc impact impact_per_szgroup nbchoked start.
getAggNodeLayerFiles(general, a_type="cumcatches", a_tstep="34321")

#I don't use benthos
getAggNodeBenthosLayerFiles(general,  a_tstep="34321")

#Get indicators
#makeCumulativeMap to create .rds files to be read, requires calls to getAggNodeLayerFiles. But is it reallyw hat I want? (maps not accounting for species or metiers)

# get fleet stuff (outcomes_all_simus). Is it that interesting given that I don't intend to compare stochastic simulations?
# create metier-wise indicators TO BE DONE)
#This call is equivalent to expressAggLoglikeFilesIndicatorsRelativeToBaselineSce.R, but better implemented
getSimusOutcomes(general,a_baseline="calib_multipliers_",explicit_pops=0:26,selected="_met_")
#Does a bar plot on the previously derived indicators
#This call is equivalent to boxplotAggLoglikeFilesIndicators.R, making it deprectaed
doOutcomesbarPlot(selected="all",selected_variables = c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit", "totland_explicit", "sweptarea", "npv", "av_vpuf_month", "hoover"),selected_scenarios= c("baseline","calib_multipliers_SCE_"))
    
    
##################
###
###EFFORT PLOTS
###
##################
  

# Catch in Pop Values. Any difference with log like (= non explicit?) If so, I could track what I want.
for(sce in general$namefolderoutput){
  #sce= general$namefolderoutput[2]
  myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2]),"_out.db",sep=""))
  dbListTables(myConn)
  
  NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")  # Cumulated fishing time per node and time step (NOT metier though)
  VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike")  # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
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
  
  metierCorr= unique(subset(VesselLogLike, select=c(Id,metierId)))
  months = data.frame(TStep = sort(unique(NodesStat$TStep)), month= 1:length(sort(unique(NodesStat$TStep))))
  
  VesselVmsLikeCond = VesselVmsLike %>% 
    filter(State==1) %>% 
    merge(subset(NodesDef, select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0), by=c("Long","Lat")) %>% 
    group_by(Long,Lat,Id,TStepDep,NodeId,icesrectanglecode) %>% 
    summarize(effort=sum(State,na.rm=T)) %>%  # Works since State = 1 when fishing
    ungroup() %>% 
    merge(metierCorr, by=c("Id")) %>% 
    mutate(month = sapply(TStepDep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>x]))] )) %>% 
    group_by(Long,Lat,NodeId,icesrectanglecode,metierId,month) %>%
    summarize(effort=sum(effort,na.rm=T)) %>% 
    ungroup()
    
  polygonsRTI=preconditionRTI(VesselVmsLikeCond,RTIrectangle)
  polygonsICES=preconditionICES(VesselVmsLikeCond,icesquarterrectangle)
  
  save(polygonsRTI,polygonsICES,RTIrectangle,icesquarterrectangle,VesselVmsLikeCond,file=paste(general$main.path,general$case_study,sce,"output/forEffortPlots.Rdata",sep="/"))
  
  for(numMet in c(NA,unique(polygonsRTI$metierId))){
    getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=T)
    getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=T)
    getmapEffortNodeAll(VesselVmsLikeCond,monthNum=NA,idMetier=numMet,gif=T)
  }
  for(numMet in c(NA,unique(polygonsRTI$metierId))){
    if(!is.na(numMet)){
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_RTIcell_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_ICEScell_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortNodeAll(VesselVmsLikeCond,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
    }
    if(is.na(numMet)){
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_RTIcell_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_ICEScell_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
      png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
      print(getmapEffortNodeAll(VesselVmsLikeCond,monthNum=NA,idMetier=numMet,gif=F))
      dev.off()
    }
  }
}


