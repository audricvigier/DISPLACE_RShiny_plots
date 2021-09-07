######### USE DISPLATE PLOTS + SHINY APP TO READ DISPLACE OUTPUTS
#
# DATE ..........................: Mon May 24 17:19:34 2021
# AUTHOR.........................: Audric Vigier
#
#----------------------------------------------------------------
#
# R version......................: R version 3.6.0 (2019-04-26)
#----------------------------------------------------------------

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
for (fileName in list.files(displaceplotLib)) source(paste(displaceplotLib,fileName,sep="/"))
shinyLib="D:/work/Displace/DISPLACE_RShiny_plots/R-scripts"
for (fileName in list.files(shinyLib,pattern=".R")[-c(11,13,14)]) source(paste(shinyLib,fileName,sep="/")) # Issue with "makeStudyAreaMap.R" 
1
for (fileName in list.files(paste(shinyLib,"/fromFrancois"),pattern=".R")) source(paste(paste(shinyLib,"/fromFrancois"),fileName,sep="/"))

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
                                      #theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
                                      theScenarios= c("baseline0Selected","baseline1Selected"),
                                      #nbSimus=20,
                                      nbSimus=1,
                                      useSQLite=FALSE)
calib=FALSE
if(! calib) for (i in 1:length(general$namesimu)) general$namesimu[[i]] = "simu1"
explicit_pops = 0:(general$nbpops-1)

##################
###
###END INPUTS
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

getFortnights = function(){
  fortnights=read.table(file=paste(general$main.path.ibm, "/simusspe_CelticSea/tstep_days.dat",sep=""),header=F) %>% 
    rename(TStep=V1) %>% 
    mutate(day=2:(n()+1)) %>% 
    mutate(keep=(((day-1)/14)==floor((day-1)/14))) %>% 
    filter(keep) %>% 
    select(-keep) %>% 
    arrange(TStep) %>%
    mutate(fortnight=(day-1)/14)%>%
    mutate(TStep=TStep-1,day=day-1) # Hours give the end of the fortnight, with the day of the end of the fortnight
}

metierNames = getMetierNames() %>% 
  arrange(metierId)

fortnights= getFortnights() # hours give the end of fortnights
fortnights= rbind(fortnights, data.frame(TStep=96504,day=4018,fortnight=287))

getMonths = function(){
  months=read.table(file=paste(general$main.path.ibm, "/simusspe_CelticSea/tstep_months.dat",sep=""),header=F) %>% 
    rename(TStep=V1) %>% 
    mutate(month=1:n())# Hours give the end of the month
}

months=getMonths()
months$TStep[months$month==132]=96433

#Create output directories if not already existing
for(sce in general$namefolderoutput){
  if(!file.exists(paste(general$main.path,general$case_study,sce,"output",sep="/"))){
    dir.create(paste(general$main.path,general$case_study,sce,"output",sep="/"))
  }
}
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
#getAggPoplikeFiles(general=general,explicit_pops=0:26,the_baseline ="calib_multipliers_")
getAggPoplikeFiles(general=general,explicit_pops=0:26,the_baseline ="baseline0Selected")

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
#getSimusOutcomes(general,a_baseline="calib_multipliers_",explicit_pops=0:26,selected="_met_")
getSimusOutcomes(general,a_baseline="baseline0Selected",explicit_pops=0:26,selected="_met_")

#Does a bar plot on the previously derived indicators
#This call is equivalent to boxplotAggLoglikeFilesIndicators.R, making it deprectaed
#doOutcomesbarPlot(selected="all",selected_variables = c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit", "totland_explicit", "sweptarea", "npv", "av_vpuf_month", "hoover"),selected_scenarios= c("baseline","calib_multipliers_SCE_"))
doOutcomesbarPlot(selected="all",selected_variables = c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit", "totland_explicit", "sweptarea", "npv", "av_vpuf_month", "hoover"),selected_scenarios= general$namefolderoutput)


##################
###
###EFFORT PLOTS
###
##################

# Catch in Pop Values. Any difference with log like (= non explicit?) If so, I could track what I want.
# WARNING issue with last month for 11 years run
for(sce in general$namefolderoutput){
  if(calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
  if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
  #dbListTables(myConn)
  
  #VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike")  # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
  VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike")  # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one)....
  NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
  NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")  # Cumulated fishing time per node and time step (NOT metier though)
  
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
  #months = data.frame(TStep = sort(unique(NodesStat$TStep)), month= 1:length(sort(unique(NodesStat$TStep))))
  
  fishingLocations = VesselVmsLike %>% 
    filter(State==1) %>% 
    select(-c(Course,CumFuel,State,TStep)) %>% 
    group_by(Id,TStepDep,Long,Lat) %>% 
    summarize(effort=n()) %>% 
    group_by(Id,TStepDep) %>% 
    mutate(prop=effort/sum(effort))
  
  # Assume that for each trip, catch spatial distribution is proportional to effort spatial distribution
  
  if(calib) catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
  if(!calib) catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",general$namesimu[[which(general$namefolderoutput==sce)]],".dat",sep=""))
  names(catchAndEffortPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
  
  effortPertrip = catchAndEffortPertrip %>% 
    mutate(effort=TStep-TStepDep-cumsteaming) %>% 
    select(c(effort,TStep,TStepDep,Id,freq_metiers)) %>% 
    mutate(metierId = sapply(as.character(freq_metiers), function(x) strsplit(x,split=")")[[1]][1])) %>% 
    mutate(metierId = as.numeric(sapply(metierId, function(x) strsplit(x,split="\\(")[[1]][2]))) %>% 
    select(-freq_metiers) %>% 
    unique() %>% 
    merge(fishingLocations,by=c("Id","TStepDep"),all.y=T) %>% 
    filter(!is.na(TStep)) %>% # Last month is poorly registered, and gives NA
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
  
  save(polygonsRTI,polygonsICES,RTIrectangle,icesquarterrectangle,effortPertrip,file=paste(general$main.path,general$case_study,sce,"output/forEffortPlots.Rdata",sep="/"))
  
  # for(numMet in c(NA,unique(polygonsRTI$metierId))){
  #   getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=T,sce)
  #   getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=T,sce)
  #   getmapEffortNodeAll(effortPertrip,monthNum=NA,idMetier=numMet,gif=T,sce)
  # }
  # for(numMet in c(NA,unique(polygonsRTI$metierId))){
  #   if(!is.na(numMet)){
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_RTIcell_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_ICEScell_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_AllTime_",numMet,".png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortNodeAll(effortPertrip,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #   }
  #   if(is.na(numMet)){
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_RTIcell_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortRTIAll(polygonsRTI,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_ICEScell_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortICESAll(polygonsICES,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #     png(filename=paste(paste(general$main.path,general$case_study,sce,"output",sep="/"),"/effort_AllAll.png",sep=""),height=800,width=800,units="px",res=100)
  #     print(getmapEffortNodeAll(effortPertrip,monthNum=NA,idMetier=numMet,gif=F,sce))
  #     dev.off()
  #   }
  # }
}

##################
###
###BIOMASS PLOTS
###
##################


keepUniqueRows = function(df2process){
  df2process = df2process%>% 
    arrange(NodeId,PopId,TStep,TotalN,TotalW,CumCatches,CumDiscards) %>% # Eliminate duplicate rows at last time step, putting the minimal value (the one being kept next step) first
    distinct(NodeId,PopId,TStep,.keep_all=T)
  return(df2process)
}

for(sce in general$namefolderoutput){
  if(!file.exists(paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))){
    if(calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
    if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
    
    #dbListFields(myConn,"PopValues")
    PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
    #PopDyn = dbGetQuery(myConn,"SELECT * FROM PopDyn") # Saved in a separate loop
    NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # To get Irish registered catch only population
    
    dbDisconnect(myConn) # Close connection
    
    PopValues = PopValues %>% 
      group_by(TStep) %>%  #Split PopValues into a list (one element per time step) to manipulate it more easily
      group_split() %>%
      map(keepUniqueRows) %>% # One operation per list item with map
      ldply() # From a list to a dataframe
    
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
    
    nodes2merge=subset(NodesDef,select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0) %>% 
      rename(rtirectangle=icesrectanglecode) %>% 
      mutate(icesrectanglecode=as.numeric(sapply(as.character(rtirectangle), function(x) substr(x,1,4))))
    
    a=Sys.time()
    PopValues = PopValues %>% 
      group_by(TStep,PopId) %>% 
      mutate(TotalW=TotalW/sum(TotalW)) %>%
      mutate(TotalW=replace(TotalW,TotalW<10^(-15),0)) %>% # Considered equal to 0 if biomass distribution is too low in some cells
      filter(TotalW!=0) %>% # To have a lighter save
      select(-c(TotalN,CumCatches,CumDiscards,Impact)) %>% # To have a lighter save
      nest() %>% 
      mutate(data = map(data, ~merge(.x,nodes2merge, by=c("NodeId")))) %>% 
      #map(data,merge(nodes2merge, by=c("NodeId")))%>%
      unnest() %>% 
      ungroup()
    b=Sys.time()
    print(b-a)
    
    PopValues = PopValues %>% 
      mutate(TStep=as.factor(TStep))
    levels(PopValues$TStep)=0:(length(levels(PopValues$TStep))-1)
    PopValues$TStep=as.numeric(levels(PopValues$TStep))[PopValues$TStep]
    
    interimMapRTI = PopValues %>% 
      rename(layer=rtirectangle) %>% 
      group_by(TStep,layer,PopId) %>% 
      summarize(TotalW=sum(TotalW,na.rm=T)) %>% 
      merge(RTIrectangle, by=c("layer")) %>% 
      rename(Long=x,Lat=y)
    
    interimMapICES = PopValues %>% 
      rename(layer=icesrectanglecode) %>% 
      group_by(TStep,layer,PopId) %>% 
      summarize(TotalW=sum(TotalW,na.rm=T)) %>% 
      merge(icesquarterrectangle, by=c("layer")) %>% 
      rename(Long=x,Lat=y)
    
    #save(PopValues,PopValues,PopDyn,interim,RTIrectangle,icesquarterrectangle,file=paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
    #save(PopDyn,PopValues,interimMapRTI,interimMapICES,interim,RTIrectangle,icesquarterrectangle,file=paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
    #PopDyn is saved in a separate loop
    save(interimMapRTI,interimMapICES,RTIrectangle,icesquarterrectangle,file=paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
    
  }
  
  # if(file.exists(paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))){
  #   load(paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
  # }
  # 
  # # LOOP ON POPS DO ONE GIF PER AGG SCALE
  # for(numPop in sort(unique(interimMap$PopId))){
  #   getBiomassMapNode(interimMap,popNum=numPop,timeStep=NA,gif=T,scename=sce,scale="Node")
  #   getBiomassMapNode(interimMapRTI,popNum=numPop,timeStep=NA,gif=T,scename=sce,scale="RTI rectangle")
  #   getBiomassMapNode(interimMapICES,popNum=numPop,timeStep=NA,gif=T,scename=sce,scale="ICES rectangle")
  # }
  # 
}

# Do a separate loop to save PopDyn and interim object
for(sce in general$namefolderoutput){
  if(calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
  if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
  PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
  PopDyn = dbGetQuery(myConn,"SELECT * FROM PopDyn") # To get pop dynamics (spatially aggregated)
  dbDisconnect(myConn) # Close connection
  
  PopValues = PopValues %>% 
    group_by(TStep) %>%  #Split PopValues into a list (one element per time step) to manipulate it more easily
    group_split() %>%
    map(keepUniqueRows) %>% # One operation per list item with map
    ldply() # From a list to a dataframe
  
  interim = PopValues %>% 
    group_by(TStep,PopId) %>%
    summarize(TotalW=sum(TotalW),TotalN=sum(TotalN),CumCatches=sum(CumCatches),CumDiscards=sum(CumDiscards)) # N is in thousands
  
  load(paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
  save(PopDyn,interimMapRTI,interimMapICES,interim,RTIrectangle,icesquarterrectangle,file=paste(general$main.path,general$case_study,sce,"output/forBiomassPlots.Rdata",sep="/"))
}

##################
###
###CATCH AND DISCARDS PLOTS
###
##################

keepUniqueRows = function(df2process){
  df2process = df2process%>% 
    arrange(NodeId,PopId,TotalN,TotalW,CumCatches,CumDiscards) %>% # Eliminate duplicate rows at last time step, putting the minimal value (the one being kept next step) first
    distinct(NodeId,PopId,.keep_all=T)
  return(df2process)
}

#1 first loop to create some interim files: 7'10" for 11 years
for(sce in general$namefolderoutput){
  startTime=Sys.time()
  a=Sys.time()
  myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
  if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
  dbListTables(myConn)
  
  #PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
  VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike") # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
  #VesselLogLikeCatches = dbGetQuery(myConn,"SELECT * FROM VesselLogLikeCatches")
  VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike") # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one).... Is it 2 hours slice I have, and not the actual effort? Ask.
  NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
  #NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")
  
  dbDisconnect(myConn)
  
  # aaa = PopValues %>%
  #   group_by(TStep,PopId,NodeId) %>% # Eliminate duplicates rows at last time step
  #   filter(row_number() == 1) %>%
  #   group_by(TStep) %>%
  #   summarize(CumCatches=sum(CumCatches)) %>%
  #   arrange(TStep) %>%
  #   mutate(Catches=CumCatches) %>%
  #   mutate(Catches = replace(Catches,TStep!=0,diff(Catches)))
  # 
  # #Catch happens only in Jan 2010
  # 
  # bbb = VesselLogLikeCatches %>%
  #   mutate(Catches!=0 | Discards!=0) %>%
  #   select(LoglikeId) %>%
  #   unique()
  # 
  # range(VesselLogLike$TStepDep[VesselLogLike$RowId %in% bbb$LoglikeId])
  # range(VesselLogLike$TStep)
  # 
  # # Vessels go fishing only in the first 712 first hours of the simulation (first month only, 1-2 day(s) before Jan ends) according to recrods...
  # # # Recording stops at hour 1603
  
  #months = data.frame(TStep = c(sort(unique(NodesStat$TStep)),(max(NodesStat$TStep)+100)), month= c(1:length(sort(unique(NodesStat$TStep))),length(sort(unique(NodesStat$TStep))))) # Adding one more row to avoid crashes
  #fortnight = data.frame(TStep = c(sort(unique(NodesStat$TStep)),(max(NodesStat$TStep)+100)), month= c(1:length(sort(unique(NodesStat$TStep))),length(sort(unique(NodesStat$TStep))))) # Adding one more row to avoid crashes

  nodes2merge=subset(NodesDef,select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0) %>%
    rename(rtirectangle=icesrectanglecode) %>%
    mutate(icesrectanglecode=as.numeric(sapply(as.character(rtirectangle), function(x) substr(x,1,4))))
  
  metierCorr= unique(subset(VesselLogLike, select=c(Id,metierId)))
  
  fishingLocations = VesselVmsLike %>% 
    filter(State==1) %>% 
    select(-c(Course,CumFuel,State,TStep)) %>% 
    group_by(Id,TStepDep,Long,Lat) %>% 
    summarize(effort=n()) %>% 
    group_by(Id,TStepDep) %>% 
    mutate(prop=effort/sum(effort))
  
  if(calib) catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
  if(!calib) catchAndEffortPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",general$namesimu[[which(general$namefolderoutput==sce)]],".dat",sep=""))

  names(catchAndEffortPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
  
  # This chunk = 2min for 11 years
  catchPertrip = catchAndEffortPertrip %>% 
    mutate(effort=TStep-TStepDep-cumsteaming) %>% # Use it for CPUE plots
    select(c(effort,TStep,TStepDep,Id,freq_metiers,starts_with("pop."),starts_with("disc_")))%>% 
    group_by(freq_metiers) %>% 
    nest() %>% 
    mutate(metierId = sapply(as.character(freq_metiers), function(x) strsplit(x,split=")")[[1]][1])) %>% 
    mutate(metierId = as.numeric(sapply(metierId, function(x) strsplit(x,split="\\(")[[1]][2]))) %>% 
    unnest() %>% 
    ungroup() %>% 
    select(-freq_metiers) %>% 
    merge(fishingLocations,by=c("Id","TStepDep"),all.y=T) %>% 
    #mutate(sanityCheck=abs(effort.y/prop-effort.x)) # All good, except what is cut at the end of first year. Keep effort.x and prop
    mutate(effort=effort.x*prop) %>% 
    select(-c(effort.x,effort.y)) %>% 
    merge(subset(NodesDef, select=c(NodeId,Long,Lat,icesrectanglecode), HarbourId==0), by=c("Long","Lat"))%>% 
    filter(!is.na(TStep)) %>% # Pb with last month
    melt(id.vars=c("Long","Lat","Id","TStepDep","TStep","metierId","prop","effort","NodeId","icesrectanglecode")) %>% 
    mutate(variable=as.character(variable)) %>% 
    group_by(variable) %>% 
    nest() %>% 
    mutate(Fraction = substr(variable,1,3)) %>% 
    mutate(Fraction=fct_recode(factor(Fraction),"Discards"="dis","Landings"="pop")) %>% 
    mutate(PopId=as.numeric(unlist(regmatches(x=variable, m=gregexpr("[[:digit:]]+",variable))))) %>%
    unnest() %>% 
    mutate(value=prop*value) %>% 
    group_by(Long,Lat,Id,TStep,metierId,NodeId,icesrectanglecode,Fraction,PopId)%>% # First long step
    summarize(effort=sum(effort,na.rm=T),value=sum(value,na.rm=T)) %>% 
    group_by(TStep) %>% 
    nest() %>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>x]))] )) %>% 
    mutate(fortnight = sapply(TStep, function(x) fortnights$fortnight[which(fortnights$TStep==min(fortnights$TStep[fortnights$TStep>x]))] )) %>% 
    unnest() %>% 
    ungroup()
  
  #1 min for 11 years
  catchPertripMonth = catchPertrip %>% 
    group_by(Long,Lat,month,metierId,NodeId,icesrectanglecode,Fraction,PopId) %>%
    summarize(effort=sum(effort,na.rm=T),value=sum(value,na.rm=T)) %>% 
    ungroup()
  
  #1 min for 11 years
  catchPertripFortnight = catchPertrip %>% 
    group_by(Long,Lat,fortnight,metierId,NodeId,icesrectanglecode,Fraction,PopId) %>%
    summarize(effort=sum(effort,na.rm=T),value=sum(value,na.rm=T)) %>% 
    ungroup()
  
  save(nodes2merge,metierCorr,fishingLocations,catchPertrip,catchPertripMonth,catchPertripFortnight,file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsInterim.Rdata",sep="/"))
  b=Sys.time()
  print(b-a)
}

#A second loop to create some interim files: 7'
for(sce in general$namefolderoutput){
  startTime=Sys.time()
  c=Sys.time()
  myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
  if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
  dbListTables(myConn)
  
  PopValues = dbGetQuery(myConn,"SELECT * FROM PopValues") # To get Cumulated catch per population
  # Conditioning PopValues for 11 years: 2'
  a=Sys.time()
  PopValues = PopValues %>%
    group_by(TStep) %>% 
    nest() %>% 
    mutate(month = sapply(TStep, function(x) months$month[which(months$TStep==min(months$TStep[months$TStep>x]))] )) %>% 
    mutate(year=floor((month-1)/12)) %>% 
    mutate(data=map(data,keepUniqueRows)) %>% 
    unnest() %>% 
    ungroup()
  b=Sys.time()
  
  VesselLogLike = dbGetQuery(myConn,"SELECT * FROM VesselLogLike") # time at sea for each vessel/metier/trip/harbour (NodeId is the harbour, not the fishing location) . But no fishing time?
  VesselLogLikeCatches = dbGetQuery(myConn,"SELECT * FROM VesselLogLikeCatches")
  #VesselVmsLike = dbGetQuery(myConn,"SELECT * FROM VesselVmsLike") # State (including fishing (1), steaming (2) and harbour (3)) for each vessel/node/time step . In theory enough to get the information on effort I want, BUT it's only for 1 year (the first one).... Is it 2 hours slice I have, and not the actual effort? Ask.
  NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
  #NodesStat = dbGetQuery(myConn,"SELECT * FROM NodesStat")
  
  dbDisconnect(myConn)
  
  d=Sys.time()
  print(d-c)
  
  #Up to this mark, for 11 years, 6'40"
  
  # a=Sys.time()
  # PopValues = PopValues %>% 
  #   group_by(TStep) %>%  #Split PopValues into a list (one element per time step) to manipulate it more easily
  #   group_split() %>%
  #   map(keepUniqueRows) %>% # One operation per list item with map
  #   ldply()
  # b=Sys.time()
  # b-a
  #Load interim files
  load(paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsInterim.Rdata",sep="/"))

    # 3 mins for 11 years for these 3 functions.
  explicitCatch = getExplicitCatch(VesselLogLike,VesselLogLikeCatches,months,fortnights,nodes2merge,"month") # Includes discards, 13 sec for 3 years ; 2 min for 11 years
  explicitCatchSpatial = getExplicitCatchSpatial(catchPertripMonth,"month") # 51 sec for 11 years
  implicitCatch = getImplicitCatch(PopValues,explicitCatchSpatial) # 7.3 sec for 11 years
  
  save(explicitCatch,explicitCatchSpatial,implicitCatch,PopValues,VesselLogLike,VesselLogLikeCatches,NodesDef,file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsInterim2.Rdata",sep="/"))
}

#A third loop to create the wanted outputs. Code could be improved regarding memory usage. Time: 7'
for(sce in general$namefolderoutput){
  #1'6" to load files
  startTime=Sys.time()
  a=Sys.time()
  #Load interim files
  load(paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsInterim.Rdata",sep="/"))
  #Load interim files
  load(paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsInterim2.Rdata",sep="/"))
  b=Sys.time()
  b-a
  
  #Needs a big minute. 
  ImplicitCatch = PopValues %>% # For 11 years, takes  1.30"
    mutate(CumDiscards=CumDiscards/1000,CumCatches=CumCatches/1000) %>%  # Convert to tons
    #group_by(TStep,PopId,NodeId) %>% # Duplicates eliminated during pre conditioning
    #filter(row_number() == 1) %>% 
    select(-c(TotalN,TotalW,Impact)) %>% # Convert to tons
    #ungroup() %>%
    group_by(NodeId) %>% 
    nest() %>% 
    mutate() %>% 
    merge(subset(NodesDef, select=c(NodeId,icesrectanglecode,Long,Lat)), by=c("NodeId")) %>% 
    rename(rtirectangle=icesrectanglecode) %>% 
    mutate(icesrectanglecode=as.numeric(sapply(rtirectangle, function(x) substr(x,1,4)))) %>% 
    as_tibble() 
  
  rm(PopValues)
  #1'35" for the 2 next lines
  #a=Sys.time()
  implicitCatchSpatialICES = getImplicitCatchSpatialFast(ImplicitCatch,NodesDef,explicitCatchSpatial,nodes2merge,scale="ICES")
  implicitCatchSpatialRTI = getImplicitCatchSpatialFast(ImplicitCatch,NodesDef,explicitCatchSpatial,nodes2merge,scale="RTI")
  # b=Sys.time()
  # print(b-a)
  
  #a=Sys.time() # 3'20" for 11 years
  explicitCatchFortnight = getExplicitCatch(VesselLogLike,VesselLogLikeCatches,months,fortnights,nodes2merge,"fortnight") 
  explicitCatchSpatialFortnight = getExplicitCatchSpatial(catchPertripFortnight,"fortnight") # LIMITED TO ONE YEAR SO FAR BECAUSE OF DISPLACE HARD CODING
  # b=Sys.time()
  # print(b-a)
  
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
    summarize(value=sum(value)) %>%
    rename(layer=icesrectanglecode) %>%
    merge(icesquarterrectangle,by=c("layer"))
  
  explicitCatchSpatialRTI = explicitCatchSpatial %>%
    group_by(PopId,month,metierId,rtirectangle,year,Fraction) %>%
    summarize(value=sum(value)) %>%
    rename(layer=rtirectangle) %>%
    merge(RTIrectangle,by=c("layer"))
  
  explicitCatchSpatialRTIFortnight = explicitCatchSpatialFortnight %>%
    group_by(PopId,fortnight,metierId,rtirectangle,Fraction) %>%
    summarize(value=sum(value)) %>%
    rename(layer=rtirectangle) %>%
    merge(RTIrectangle,by=c("layer"))
  
  implicitCatchSpatialICES = implicitCatchSpatialICES %>%
    group_by(PopId,month,icesrectanglecode,year,Fraction) %>%
    summarize(value=sum(value)) %>%
    rename(layer=icesrectanglecode) %>%
    merge(icesquarterrectangle,by=c("layer"))
  
  implicitCatchSpatialRTI = implicitCatchSpatialRTI %>%
    group_by(PopId,month,rtirectangle,year,Fraction) %>%
    summarize(value=sum(value)) %>%
    rename(layer=rtirectangle) %>%
    merge(RTIrectangle,by=c("layer"))
  
  # #a=Sys.time() # 10 mis for that single step!
  # allCatchSpatial = explicitCatchSpatial %>%
  #   mutate(year=year-min(year)) %>%
  #   select(-c(metierId)) %>%
  #   bind_rows(implicitCatchSpatial)
  # 
  # interimDerivationChunk = function(data2process){
  #   data2process = data2process %>%
  #     group_by(PopId,month,NodeId,icesrectanglecode,rtirectangle,Long,Lat,year,Fraction) %>%
  #     summarize(value=sum(value,na.rm=T)) %>%
  #     ungroup()
  #   return(data2process)
  # }
  # allCatchSpatial = lapply(sort(unique(allCatchSpatial$month)), function(x) interimDerivationChunk(subset(allCatchSpatial, month==x)))
  # allCatchSpatial=plyr::ldply(allCatchSpatial)
  # # b=Sys.time()
  # # b-a
  
  allCatchSpatialICES = explicitCatchSpatialICES %>%
    mutate(year=year-min(year)) %>%
    select(-c(metierId)) %>%
    bind_rows(implicitCatchSpatialICES)
  
  interimDerivationChunk = function(data2process){
    data2process = data2process %>%
      group_by(layer,PopId,month,year,Fraction,x,y) %>%
      summarize(value=sum(value,na.rm=T)) %>%
      ungroup()
    return(data2process)
  }
  allCatchSpatialICES = lapply(sort(unique(allCatchSpatialICES$month)), function(x) interimDerivationChunk(subset(allCatchSpatialICES, month==x)))
  allCatchSpatialICES=plyr::ldply(allCatchSpatialICES)
  
  allCatchSpatialRTI = explicitCatchSpatialRTI %>%
    mutate(year=year-min(year)) %>%
    select(-c(metierId)) %>%
    bind_rows(implicitCatchSpatialRTI)
  
  allCatchSpatialRTI = lapply(sort(unique(allCatchSpatialRTI$month)), function(x) interimDerivationChunk(subset(allCatchSpatialRTI, month==x)))
  allCatchSpatialRTI=plyr::ldply(allCatchSpatialRTI)
  
  save(explicitCatch,explicitCatchFortnight,explicitCatchSpatial,explicitCatchSpatialFortnight,explicitCatchSpatialICES,explicitCatchSpatialRTI,explicitCatchSpatialRTIFortnight,file=paste(general$main.path,general$case_study,sce,"output/forExplicitCatchsPlots.Rdata",sep="/"))
  #save(implicitCatch,implicitCatchSpatial,implicitCatchSpatialICES,implicitCatchSpatialRTI,file=paste(general$main.path,general$case_study,sce,"output/forImplicitCatchsPlots.Rdata",sep="/"))
  save(implicitCatch,implicitCatchSpatialICES,implicitCatchSpatialRTI,file=paste(general$main.path,general$case_study,sce,"output/forImplicitCatchsPlots.Rdata",sep="/"))
  #save(allCatchSpatialRTI,allCatchSpatialICES,allCatchSpatial,file=paste(general$main.path,general$case_study,sce,"output/forAllCatchsPlots.Rdata",sep="/"))
  save(allCatchSpatialRTI,allCatchSpatialICES,file=paste(general$main.path,general$case_study,sce,"output/forAllCatchsPlots.Rdata",sep="/"))
  # b=Sys.time()
  # print(b-a)
  endTime=Sys.time()
  print(endTime-startTime)
}

##################
###
###LPUE AND DPUE PLOTS
###
##################

for(sce in general$namefolderoutput){
  
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
  
  explicitCatchSpatialICESFortnight = explicitCatchSpatialFortnight %>%
    group_by(PopId,fortnight,metierId,icesrectanglecode,Fraction) %>%
    summarize(value=sum(value),effort=sum(effort)) %>%
    rename(layer=icesrectanglecode) %>%
    merge(icesquarterrectangle,by=c("layer"))
  
  explicitCatchSpatialRTIFortnight = explicitCatchSpatialFortnight %>%
    group_by(PopId,fortnight,metierId,rtirectangle,Fraction) %>%
    summarize(value=sum(value),effort=sum(effort)) %>%
    rename(layer=rtirectangle) %>%
    merge(RTIrectangle,by=c("layer"))
  
  save(explicitCatchSpatial,explicitCatchSpatialFortnight,explicitCatchSpatialICES,explicitCatchSpatialICESFortnight,explicitCatchSpatialRTI,explicitCatchSpatialRTIFortnight,file=paste(general$main.path,general$case_study,sce,"output/forExplicitCPUEPlots.Rdata",sep="/"))
}

##################
###
###F PLOTS
###
##################

for(sce in general$namefolderoutput){
  
  if(calib) FestimatesYear = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_annual_indic_",sce,length(general$namesimu[[1]]),".dat",sep=""))
  if(!calib) FestimatesYear = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_annual_indic_",general$namesimu[[which(general$namefolderoutput==sce)]],".dat",sep=""))
  names(FestimatesYear)=c("TStep","pop","multi","multi2","Fbar","totland_kg","totdisc_kg","SSB_kg","tac","N0","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","W0","W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","M0","M1","M2","M3","M4","M5","M6","M7","M8","M9","M10")
  
  
  FestimatesYear = FestimatesYear %>% 
    select(c(TStep,pop,Fbar,paste("F",0:10,sep=""))) %>% 
    rename(PopId=pop) %>% 
    melt(id.vars=c("TStep","Fbar","PopId")) %>% 
    rename(age=variable) %>% 
    mutate(age=as.character(age)) %>%
    mutate(age=as.numeric(sapply(age,function(x) strsplit(x, split="F")[[1]][2]))) %>% 
    # group_by(TStep,PopId) %>% 
    # mutate(Fbar2=mean(value[age%in%c(Fbarage$amin[Fbarage$PopId==PopId]:Fbarage$amax[Fbarage$PopId==PopId])])) %>% 
    # ungroup() %>% 
    # mutate(diff=abs(Fbar-Fbar2)) %>% # Does not match Fbar definition: I cannot derive F bar like that
    mutate(year = factor(TStep,labels=0:(length(unique(TStep))-1))) %>% 
    mutate(year = as.numeric(levels(year))[year]) %>% 
    merge(stockNames,by=c("PopId"))
  
  save(FestimatesYear,file=paste(general$main.path,general$case_study,sce,"output/forFPlots.Rdata",sep="/"))
}

#getFTimeSeries(FestimatesYear,F)

##################
###
###ECONOMIC VARIABLES PLOTS
###
##################

for(sce in general$namefolderoutput){
  if(calib) EconomicsPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
  if(!calib) EconomicsPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",general$namesimu[[which(general$namefolderoutput==sce)]],".dat",sep=""))
  names(EconomicsPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips") 
  EconomicsPertrip = EconomicsPertrip %>% 
    select(c(TStepDep,TStep,cumsteaming,Id,VE_REF,fuelcons,freq_metiers,rev_from_av_prices,rev_explicit_from_av_prices,fuelcost,vpuf,gradva,GVA,LabourSurplus,GrossProfit,NetProfit,NetProfitMargin,NetPresentValue,numTrips))#Get non-fuel variable costs (and other stuff useful for sanity checks)
  economicFeatures=read.table(file=file.path(general$main.path.ibm, paste("vesselsspe_", general$case_study, sep=''),paste("vesselsspe_economic_features.dat",sep='')),sep="|")
  names(economicFeatures)=c("VE_REF","Nb_crew","Annual_other_income","Landing_costs_percent","Crewshare_and_unpaid_labour_costs_percent","Other_variable_costs_per_unit_effort","Annual_insurance_costs_per_crew","Standard_labour_hour_opportunity_costs","Standard_annual_full_time_employment_hours","Other_annual_fixed_costs","Vessel_value","Annual_depreciation_rate","Opportunity_interest_rate","Annual_discount_rate")
  economicFeatures = economicFeatures%>% 
    mutate(VE_REF=as.character(VE_REF))
  
  EconomicsPertrip = EconomicsPertrip %>% 
    merge(economicFeatures, by=c("VE_REF")) %>% 
    mutate(nonFuelvariableCost=Other_variable_costs_per_unit_effort*cumsteaming) %>% 
    mutate(freq_metiers = sapply(as.character(freq_metiers), function(x) strsplit(x, split="\\(")[[1]][2]))%>% 
    mutate(freq_metiers = as.numeric(sapply(as.character(freq_metiers), function(x) strsplit(x, split=")")[[1]][1])))
  
  save(EconomicsPertrip,file=paste(general$main.path,general$case_study,sce,"output/forEconomicsPlots.Rdata",sep="/"))
}

#getEconomicTimeSeries(data4plot = EconomicsPertrip,variable="revenue",cumulTime=T,metChoice=0, facet=F)

##################
###
###ANNUAL INDICATORS
###
##################

for(sce in general$namefolderoutput){
  if(calib) annualIndicators = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_annual_indic_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
  if(!calib) annualIndicators = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_annual_indic_",general$namesimu[[which(general$namefolderoutput==sce)]],".dat",sep=""))
  colnames(annualIndicators)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))
  annualIndicators = annualIndicators[,1:9] %>% 
    reshape2::melt(id.vars=c("tstep","stk")) %>% 
    reshape2::dcast(stk+variable~tstep,value.var="value")
  
  annualIndicators$ratios = annualIndicators[,dim(annualIndicators)[2]]/annualIndicators[,3]
  
  annualIndicators = annualIndicators %>% 
    select(stk,variable,ratios) %>% 
    mutate(sce=sce) %>% 
    filter(variable%in%c("Fbar","totland_kg","totdisc_kg","SSB_kg","tac" )) %>% 
    mutate(variable=droplevels(variable)) %>% 
    mutate(variable=fct_recode(variable,"F/Finit"="Fbar","TLand/TLandinit"="totland_kg","TDisc/TDiscinit"="totdisc_kg","SSB/SSBinit"="SSB_kg","Tac/Tacinit" ="tac"))
  
  save(annualIndicators,file=paste(general$main.path,general$case_study,sce,"output/forAnnualIndicatorsPlots.Rdata",sep="/"))
}


##################
###
###RTI MAPS
###
##################

for(sce in general$namefolderoutput){
  if(calib){
    RTImaps = read_lines(paste(general$main.path,"/",general$case_study,"/",sce,"/popnodes_tariffs_",length(general$namesimu[2][[1]]),".dat",sep=""))
    if(length(RTImaps)>0){
      RTImaps = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popnodes_tariffs_",length(general$namesimu[2][[1]]),".dat",sep=""))
    }
  }
  if(!calib){
    RTImaps = read_lines(paste(general$main.path,"/",general$case_study,"/",sce,"/popnodes_tariffs_simu1.dat",sep=""))
    if(length(RTImaps)>0){
      RTImaps = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/popnodes_tariffs_simu1.dat",sep=""))
    }
  }
  if(length(RTImaps)>0){
    
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
    
    if(calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",sce,length(general$namesimu[2][[1]]),"_out.db",sep=""))
    if(! calib) myConn <- dbConnect(drv = SQLite(), dbname= paste(general$main.path,"/",general$case_study,"/",sce,"/",general$case_study,"_",general$namesimu[[which(general$namefolderoutput==sce)]],"_out.db",sep=""))
    dbListTables(myConn)
    
    NodesDef = dbGetQuery(myConn,"SELECT * FROM NodesDef") %>%  # Get nodes coordinates, ICES rectangle and RTI rectangle (all coded in icesrectanglecode)
      filter(NodeName=="at_sea")
    dbDisconnect(myConn)
    
    names(RTImaps)=c("TStep","NodeId","Long","Lat",seq(0:(dim(RTImaps)[2]-5))) # one tariff per metier
    RTImaps = RTImaps %>% 
      merge(months, by=c("TStep")) %>% 
      merge(subset(NodesDef, select=c(NodeId,icesrectanglecode)), by=c("NodeId")) %>% 
      select(-c("Long","Lat","NodeId","TStep")) %>% 
      melt(id.vars=c("month","icesrectanglecode")) %>% 
      rename(layer=icesrectanglecode) %>% 
      group_by(month,layer,variable) %>% 
      summarize(value=mean(value)) %>% # TO BE CHANGED ONCE FRANCOIS ALTERS HIS CODE, AS IT SHOULD BE UNIQUE, NOT MEAN
      ungroup() %>% 
      merge(RTIrectangle, by=c("layer"))
    levels(RTImaps$variable)=metierNames$name
    
  
  }else{
    RTImaps=NULL
  }
  save(RTImaps,file=paste(general$main.path,general$case_study,sce,"output/forRTITariffsPlots.Rdata",sep="/"))
}
