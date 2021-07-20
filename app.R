
library(cowplot)
library(data.table)
library(gganimate)
library(grid)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(viridis)

setwd("D:/work/Displace/DISPLACE_RShiny_plots")
shinyOptions(shiny.autoreload = TRUE)
shinyOptions(shiny.launch.browser = TRUE)

## Source R scripts
source("R-scripts/setGeneralVariable.R", local = TRUE)
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
                                      # theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
                                      # nbSimus=20,
                                      theScenarios= c("baseline0","baseline1"),
                                      nbSimus=1,
                                      useSQLite=FALSE)

source("R-scripts/getAnnualIndicatorsPlots.R", local = TRUE)
source("R-scripts/getBiomassPlots.R", local = TRUE)
source("R-scripts/getCatchPlots.R", local = TRUE)
source("R-scripts/getCPUEPlots.R", local = TRUE)
source("R-scripts/getEconomicsPlots.R", local = TRUE)
source("R-scripts/getEffortPlots.R", local = TRUE)
source("R-scripts/getFPlots.R", local = TRUE)
source("R-scripts/mapAverageLayerFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromAggLoglikeFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromPopDynFiles.R", local = TRUE)
source("R-scripts/helperFunctions.R", local = TRUE)
source("R-scripts/barplotLanDisPerScenario.R", local = TRUE)
source("R-scripts/responseCurvesSBBandF.R", local = TRUE)



sbox <- shinydashboard::box
outputLocation <- "D:/DISPLACE_outputs/CelticSea/data"
ybeg=2010
yend=2020
## Find available RData files and pick out scenarios
loglikefns <- dir(outputLocation, "loglike.*RData", full.names = TRUE)
loglikefns = loglikefns[as.numeric(unlist(sapply(general$namefolderoutput, function(x) grep(x,loglikefns))))]
loglikescenarios <- unique(gsub("^.*agg_|[.]RData", "", loglikefns))
popdynfns <- dir(outputLocation, "popdyn.*RData", full.names = TRUE)
popdynfns = popdynfns[as.numeric(unlist(sapply(general$namefolderoutput, function(x) grep(x,popdynfns))))]
popdynscenarios <- unique(gsub("^.*popdyn_|[.]RData", "", popdynfns))
# scenames=c("Multipliers","Multipliers SCE")
scenames=c("Baseline 0","Baseline 1")
what2="weight"
selected="_all_"
# a_baseline="calib_multipliers_"
a_baseline="baseline0"

getStockNames = function(){
  codes=read.table(file=paste(general$main.path.ibm, "/pop_names_CelticSea.txt",sep=""),header=T)
  stockNames=read.table(file=paste(general$main.path.param,"/POPULATIONS/Stock_biological_traits.csv",sep=""),header=T,sep=",") %>% 
    select(c(stock,species)) %>% 
    rename(spp=stock) %>% 
    merge(codes,by=c("spp")) %>% 
    rename(PopId=idx)
}

getMetierNames = function(){
  codes=read.table(file=paste(general$main.path.ibm, "/metiersspe_CelticSea/metier_names.dat",sep=""),header=T) %>% 
    rename(metierId=idx)
}

stockNames = getStockNames() %>% 
  arrange(PopId)
metierNames = getMetierNames() %>% 
  arrange(metierId)
getMonths = function(){
  months=read.table(file=paste(general$main.path.ibm, "/simusspe_CelticSea/tstep_months_2010_2020.dat",sep=""),header=F) %>%
    rename(TStep=V1) %>%
    mutate(month=1:n())# Hours give the end of the month
}
months=getMonths()

# annualindicfns <- dir(outputLocation, "lst_annualindic.*RData", full.names = TRUE)
# annualindicscenarios <- gsub("^.*lst_annualindic_|[.]RData", "", popdynfns)
## Load all loglike and popdyn files
#for (f in c(loglikefns, popdynfns, annualindicfns)) load(f, envir = .GlobalEnv)
for (f in c(loglikefns, popdynfns)) load(f, envir = .GlobalEnv)

biomassMaps = effortMaps = allCatchMaps = implicitCatchMaps = explicitCatchMaps = explicitCPUEMaps = explicitCPUEMapsFortnight =list()
biomassTimeSeries = effortTimeSeries = economicsTimeSeries = FTimeSeries = implicitCatchTimeSeries = explicitCatchTimeSeries = explicitCPUETimeSeries = explicitCPUETimeSeriesFortnight = annualIndicatorsList=list()

for (sce in popdynscenarios){
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forEffortPlots.Rdata",sep=""))
  effortMaps[[sce]]$polygonsICES=polygonsICES
  effortMaps[[sce]]$polygonsRTI=polygonsRTI
  effortMaps[[sce]]$VesselVmsLikeCond=effortPertrip
  effortTimeSeries[[sce]]=effortPertrip %>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forBiomassPlots.Rdata",sep=""))
  biomassMaps[[sce]]$interimMap=interimMap
  biomassMaps[[sce]]$interimMapRTI=interimMapRTI
  biomassMaps[[sce]]$interimMapICES=interimMapICES
  biomassTimeSeries[[sce]]=PopDyn%>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forAllCatchsPlots.Rdata",sep=""))
  allCatchMaps[[sce]]$interimMap=allCatchSpatial
  allCatchMaps[[sce]]$interimMapRTI=allCatchSpatialRTI
  allCatchMaps[[sce]]$interimMapICES=allCatchSpatialICES
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forImplicitCatchsPlots.Rdata",sep=""))
  implicitCatchMaps[[sce]]$interimMap=implicitCatchSpatial
  implicitCatchMaps[[sce]]$interimMapRTI=implicitCatchSpatialRTI
  implicitCatchMaps[[sce]]$interimMapICES=implicitCatchSpatialICES
  implicitCatchTimeSeries[[sce]]=implicitCatch %>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forExplicitCatchsPlots.Rdata",sep=""))
  explicitCatchMaps[[sce]]$interimMap=explicitCatchSpatial
  explicitCatchMaps[[sce]]$interimMapRTI=explicitCatchSpatialRTI
  explicitCatchMaps[[sce]]$interimMapICES=explicitCatchSpatialICES
  explicitCatchTimeSeries[[sce]]=explicitCatch %>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forExplicitCPUEPlots.Rdata",sep=""))
  explicitCPUEMaps[[sce]]$interimMap=explicitCatchSpatial
  explicitCPUETimeSeries[[sce]]=explicitCatchSpatial
  explicitCPUETimeSeriesFortnight[[sce]]=explicitCatchSpatialFortnight
  explicitCPUETimeSeries[[sce]]=explicitCatchSpatial %>% 
    mutate(scename=sce)
  explicitCPUETimeSeriesFortnight[[sce]]=explicitCatchSpatialFortnight %>% 
    mutate(scename=sce)
  explicitCPUEMaps[[sce]]$interimMapRTI=explicitCatchSpatialRTI
  explicitCPUEMaps[[sce]]$interimMapICES=explicitCatchSpatialICES
  explicitCPUEMapsFortnight[[sce]]$interimMap=explicitCatchSpatialFortnight
  explicitCPUEMapsFortnight[[sce]]$interimMapRTI=explicitCatchSpatialRTIFortnight
  explicitCPUEMapsFortnight[[sce]]$interimMapICES=explicitCatchSpatialICESFortnight
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forFPlots.Rdata",sep=""))
  FTimeSeries[[sce]]=FestimatesYear %>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forEconomicsPlots.Rdata",sep=""))
  economicsTimeSeries[[sce]]=EconomicsPertrip %>% 
    mutate(scename=sce)
  load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forAnnualIndicatorsPlots.Rdata",sep=""))
  annualIndicatorsList[[sce]]=annualIndicators 
}

biomassTimeSeries = plyr::ldply(biomassTimeSeries)
FTimeSeries = plyr::ldply(FTimeSeries)
effortTimeSeries = plyr::ldply(effortTimeSeries)
explicitCatchTimeSeries = plyr::ldply(explicitCatchTimeSeries)
implicitCatchTimeSeries = plyr::ldply(implicitCatchTimeSeries)
explicitCPUETimeSeries = plyr::ldply(explicitCPUETimeSeries)
explicitCPUETimeSeriesFortnight = plyr::ldply(explicitCPUETimeSeriesFortnight)
economicsTimeSeries = plyr::ldply(economicsTimeSeries)
annualIndicatorsList = plyr::ldply(annualIndicatorsList)

## and read some tables
fleetindicfns <- dir(outputLocation, "outcomes_all_simus_relative_to_baseline_sce_*", full.names = TRUE)
fleetindicnames <- gsub("^.*outcomes_all_simus_|[.]txt", "", fleetindicfns)
for (fi in 1: length(fleetindicfns)) assign(fleetindicnames[fi], read.table(fleetindicfns[fi], header=TRUE, sep=";"))


## Read population names
#popnames <- read.table("data/CelticSea44/pop_names_CelticSea.txt", header = TRUE); save("popnames", file = "data/popnames.Rdata")
load(file = paste(outputLocation,"/popnames.Rdata",sep=""))

convertMenuItem <- function(tabName, mi) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

## User interface ----

ui <- dashboardPage(
  dashboardHeader(title = "DISPLACE output viewer"),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenu(id = "menu",
                  convertMenuItem("intro",
                                  menuItem("Model info", tabName = "intro", icon = icon("info"), startExpanded = TRUE)),
                  #convertMenuItem("map", menuItem("Maps", tabName = "map", icon = icon("map"),selectInput("sel.mapquantity", "Select quantity", choices = selquantity(), multiple = FALSE, selectize = FALSE))), # cumulativeCatch, discards, ftime, swept area
                  convertMenuItem("map",
                                  menuItem("Maps", tabName = "map", icon = icon("map"),
                                           selectInput("selmap.variable", "Variable", choices=c("Effort","Biomass","Landings","Discards","LPUE","DPUE"),selected="Effort"),                         
                                           selectInput("selmap.scale", "Aggregation scale (space)", choices=c("Node","ICES rectangle","RTI rectangle"),selected="Node"),
                                           conditionalPanel(
                                             condition = "input['selmap.variable']=='Landings' || input['selmap.variable']=='Discards'",
                                             selectInput("selmap.catchVariable", "Catch variable", choices=c("Explicit","Implicit","All"),selected="Explicit")),
                                          conditionalPanel(
                                             condition = "input['selmap.variable'] =='LPUE' || input['selmap.variable'] =='DPUE'",
                                             selectInput("selmap.rtilike", "RTI-like maps?", choices=c("No","Yes"),selected="No")),
                                          
                                            selectInput("selmap.timescale", "Aggregation scale (time)", choices=c("month","year"),selected="month"),
                                           conditionalPanel(
                                             condition = "input['selmap.timescale'] =='fortnight'",
                                             sliderInput("selmap.fortnight", "Time step (fortnight)", min = 1, max = ((yend-ybeg+1)*26),value=1, step=1, ticks = T, animate = T)),
                                           conditionalPanel(
                                             condition = "input['selmap.timescale'] =='month'",
                                             sliderInput("selmap.month", "Time step (month)", min = 1, max = ((yend-ybeg+1)*12),value=1, step=1, ticks = T, animate = T)),
                                           conditionalPanel(
                                             condition = "input['selmap.timescale'] =='year'",
                                             sliderInput("selmap.year", "Time step (year)", min = ybeg, max = yend,value=ybeg, step=1, ticks = T, animate = T)),
                                          conditionalPanel(
                                            condition = "input['selmap.variable'] !='Biomass'",
                                            selectInput("selmap.metier", "Métier", choices=c("All",0:16),selected="All")),
                                          conditionalPanel(
                                              condition = "input['selmap.variable'] !='Effort'",
                                              selectInput("selmap.pop", "Population", choices=c(0:(general$nbpops-1)),selected=7)))), # cumulativeCatch, discards, ftime, swept area
                  convertMenuItem("ts",
                                  menuItem("Time series", tabName = "ts", icon = icon("chart-line"),
                                           selectInput("sel.sce", "Select scenarios", choices = selsce(popdynscenarios,scenames), selected = selsce(popdynscenarios,scenames), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.var", "Select a variable", choices = c("Landings","Discards","Effort","F","LPUE","DPUE","Populations","Economics"), selected = "Landings", multiple = FALSE), # choices = selvar()
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Populations'",
                                             selectInput("sel.varPop", "Population variable", choices=c("SSB","B","N"),selected="SSB"),
                                             selectInput("sel.varPopSize", "Display size bins?", choices=c("No","x-axis","colour"),selected="No")),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Economics'",
                                             selectInput("sel.varEco", "Economics variable", choices=c("fuelcost","GrossProfit","GVA","LabourSurplus","NetProfit","NetProfitMargin","nonFuelvariableCost","profit","revenue"),selected="revenue"),
                                             selectInput("sel.facet", "Facet", choices=c("métier","scenario"),selected="métier")),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Landings' || input['sel.var'] =='Discards'|| input['sel.var'] =='LPUE'|| input['sel.var'] =='DPUE'",
                                             selectInput("sel.bothFractions", "Full catch? (landings + discards)", choices=c("Yes","No"),selected="No")),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Landings' || input['sel.var'] =='Discards'",
                                             selectInput("sel.expimp", "Catch from...", choices=c("Explicit vessels","Unmodelled vessels","All vessels"),selected="Explicit vessels")),
                                           conditionalPanel(
                                             condition = "((input['sel.var'] =='Landings' || input['sel.var'] =='Discards')  && input['sel.expimp'] =='Explicit vessels')|| ( input['sel.var'] =='LPUE'|| input['sel.var'] =='DPUE')",
                                             selectInput("sel.facet", "Facet", choices=c("métier","scenario"),selected="métier")),
                                           conditionalPanel(
                                             condition = "((input['sel.var'] =='Landings' || input['sel.var'] =='Discards') && input['sel.expimp'] =='Explicit vessels')|| input['sel.var'] =='Effort' || input['sel.var'] =='LPUE'|| input['sel.var'] =='DPUE'|| input['sel.var'] =='Economics'",
                                             selectInput("sel.metier", "Métier", choices=c("All",0:16),selected="All",multiple=T)),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Landings' || input['sel.var'] =='Discards' || input['sel.var'] =='LPUE'|| input['sel.var'] =='DPUE'",
                                             selectInput("sel.pop", "Population", choices=c("All",0:(general$nbpops-1)),selected="All",multiple=T)),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Landings' || input['sel.var'] =='Discards'|| input['sel.var'] =='Effort' || input['sel.var'] =='LPUE'|| input['sel.var'] =='DPUE' || input['sel.var'] =='Populations' || input['sel.var'] =='Economics'",
                                             selectInput("sel.aggTime", "Time scale", choices=c("year","month","fortnight"),selected="year")),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='F'",
                                             selectInput("sel.fbar", "F...", choices=c("bar","per age class"),selected="bar")),
                                           conditionalPanel(
                                             condition = "input['sel.var'] =='Economics' || input['sel.var'] =='Effort'|| input['sel.var'] =='Landings' || input['sel.var'] =='Discards'",
                                             checkboxInput("quantCumSum", label = "Cumulative sum", value = TRUE))
                                           )) ,
                  convertMenuItem("indicators",
                                  menuItem("Indicators", tabName = "indicators", icon = icon("chart-bar"),
                                           selectInput("indic.sce", "Scenarios", choices = selsce(popdynscenarios,scenames), selected = selsce(popdynscenarios,scenames), multiple = TRUE, selectize = FALSE),
                                           selectInput("indic.pop", "Population", choices=c("All",0:(general$nbpops-1)),selected="All",multiple=T),
                                           selectInput("indic.facet", "Facetting", choices = c("pop","variable","scenario","no"), selected = c("pop"), multiple = FALSE)))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("intro",
              h2("DISPLACE model for the Irish demersal Celtic Sea fisheries"),
              sbox(width = 12, title = "Background", status = "primary", solidHeader = FALSE, collapsible = FALSE,
                   div("We present the outcomes of the DISPLACE agent-based modelling platform for simulating bio-economic fisheries dynamics and clarifying options for sustainable and viable fisheries in the Celtic Sea.
                       In the present case study, we address the issue of quota underutilization from choked species
                    that would arise from annual decisions on TACs not matching the opportunities of individual fishers.
                   We, therefore, explore the potential benefits of spatial avoidance (or spatial selectivity)
                   by displacing the fishing to other areas but also measuring the change of pressure on other ecosystem components,
                    i.e., including i) effects on other species via trophic interactions effects ii) effects on benthic habitats.
                     An innovative fisheries management measure, a fishing credits system
                      is used in this context to help to incentivize displacing the fishing towards areas minimizing
                      the final net effects and is further contrasted against testing spatial management with closed areas.")),
              sbox(width = 6, title = "Study area", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(img(src = "studyAreaMap.png", width = "95%"))),
              sbox(width = 6, title = "Species", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(tableOutput("speciesTable"))),
              sbox(width = 6, title = "Gear categories", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(tableOutput("gearTable"))),
              sbox(width = 6, title = "Conditioning fisheries", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   div("Fishing vessels of the Irish demersal fishing fleet are considered (DAFM, 2017; EC, 2017). "), collapsed = FALSE)),

      tabItem("map",
              plotOutput("cumulativeMap", height = "1000px")
      ),
      tabItem("ts",
              # fluidRow(
              #   sbox(width = 6, plotOutput("linePlot"), title = "", status = "primary", solidHeader = FALSE)
              # ))
              plotOutput("linePlot", height = "1000px")),
      tabItem("indicators",
              plotOutput("indicatorPlot", height = "1000px"))
      # ,
      # tabItem("tab_plotlymap",
      #         plotlyOutput("cumulativeMaps"))
    )
  )
)

## Server side logic ----

server <- function(input, output, session) {
  output$speciesTable <- renderTable(read.csv(paste(outputLocation,"/species.csv",sep="")))
  output$gearTable <- renderTable({
    tbl <- read.csv(paste(outputLocation,"/gears.csv",sep=""))
    names(tbl) <- c("Gear", "Code")
    tbl
  })
  
  outVar <- reactive({
    updatedItems=list()
    updatedItems[[1]] = c("month","year")
    updatedItems[[2]]=c("Node","ICES rectangle","RTI rectangle")
    updatedItems[[3]] = c("month","year")
    #if (!input$selmap.variable%in%c("LPUE","DPUE")) updatedItems[[1]] = c("month","year")
    if (input$selmap.variable%in%c("LPUE","DPUE") & input$selmap.rtilike=="No") updatedItems[[1]] = c("month","year","fortnight")
    if (input$sel.var%in%c("LPUE","DPUE")) updatedItems[[3]] = c("month","year","fortnight")
    if (input$selmap.variable%in%c("LPUE","DPUE") & input$selmap.rtilike=="Yes"){
      updatedItems[[1]] = c("fortnight")
      updatedItems[[2]]=c("RTI rectangle")
    }
    return(updatedItems)
  })
  
  observe({
    updateSelectInput(session, "selmap.timescale",choices = outVar()[[1]], selected =outVar()[[1]][1])
  })
  
  observe({
    updateSelectInput(session, "selmap.scale",choices = outVar()[[2]], selected =outVar()[[2]][1]) 
  })
  
  observe({
    updateSelectInput(session, "sel.aggTime",choices = outVar()[[3]], selected =outVar()[[3]][1]) 
  })
  
  mapsOnAgridEffort = function(effortMaps,scale,metierNum,monthNum,scenames){
    # scale="Node" #input$selmap.scale
    # monthNum=2# input$selmap.month
    # metierNum=NA #metierNum
    # scenames=c("A","B")
    plotList=list()
    i=0
    for(dataset in effortMaps){
      i=i+1
      if(scale=="Node"){
        plotList[[i]]=as_grob(getmapEffortNodeAll(dataset$VesselVmsLikeCond,gif=FALSE,idMetier=metierNum,monthNum,scenames[i]))
      }
      if(scale=="ICES rectangle"){
        plotList[[i]]=as_grob(getmapEffortICESAll(dataset$polygonsICES,gif=FALSE,idMetier=metierNum,monthNum,scenames[i]))
      }
      if(scale=="RTI rectangle"){
        plotList[[i]]=as_grob(getmapEffortRTIAll(dataset$polygonsRTI,gif=FALSE,idMetier=metierNum,monthNum,scenames[i]))
      }
    }
    numCols = 4
    if(length(plotList)<4) numCols = length(plotList)
    return(grid.arrange(grobs=plotList,ncol=numCols))
  }
  
  mapsOnAgridBiomass = function(biomassMaps,scale,popNum,monthNum,scenames){
    # scale="Node" #input$selmap.scale
    # monthNum=2# input$selmap.month
    # metierNum=NA #metierNum
    # scenames=c("A","B")
    plotList=list()
    i=0
    for(dataset in biomassMaps){
      i=i+1
      if(scale=="Node"){
        plotList[[i]]=as_grob(getBiomassMapNode(dataset$interimMap,popNum,timeStep=monthNum,gif=F,scename=scenames[i],scale="Node"))
      }
      if(scale=="ICES rectangle"){
        plotList[[i]]=as_grob(getBiomassMapNode(dataset$interimMapICES,popNum,timeStep=monthNum,gif=F,scename=scenames[i],scale="ICES rectangle"))
      }
      if(scale=="RTI rectangle"){
        plotList[[i]]=as_grob(getBiomassMapNode(dataset$interimMapRTI,popNum,timeStep=monthNum,gif=F,scename=scenames[i],scale="RTI rectangle"))
      }
    }
    numCols = 4
    if(length(plotList)<4) numCols = length(plotList)
    return(grid.arrange(grobs=plotList,ncol=numCols))
  }
  
  mapsOnAgridCatch = function(dataset,catchType,fractionName,metierNum,scaleSpace,scaleTime,popNum,timeStep,scenames){
    # catchMaps=explicitCatchMaps
    # catchType="Explicit"
    # scaleSpace="Node" #input$selmap.scale
    # scaleTime="month"
    # popNum=7
    # timeStep=2# input$selmap.month
    # metierNum=3 #metierNum
    # scenames=c("A","B")
    # fractionName="Landings"
    # plotList=list()
    # data2plot=NULL
    
    if(scaleSpace=="Node") scaleSpace = "All"
    if(scaleSpace=="RTI rectangle") scaleSpace = "RTI"
    if(scaleSpace=="ICES rectangle") scaleSpace = "ICES"
    if (scaleTime=="year" & catchType=="Explicit") timeStep = timeStep -1
    plotList=list()
    i=0
    for(data2map in dataset){
      if(scaleSpace=="All") data2plot = data2map$interimMap
      if(scaleSpace=="ICES") data2plot = data2map$interimMapICES
      if(scaleSpace=="RTI") data2plot = data2map$interimMapRTI
      i=i+1
      if(catchType=="Explicit"){
        plotList[[i]]=as_grob(getExplicitCatchMap(data2plot,popNum,timeStep,metierNum,fractionName,sce=scenames[i],scaleTime,resScale=scaleSpace,gif=F))
      }
      if(catchType=="Implicit"){
        plotList[[i]]=as_grob(getImplicitCatchMap(data2plot,popNum,timeStep,fractionName,sce=scenames[i],scaleTime,resScale=scaleSpace,gif=F))
      }
      if(catchType=="All"){
        plotList[[i]]=as_grob(getAllCatchMap(data2plot,popNum,timeStep,fractionName,sce=scenames[i],scaleTime,resScale=scaleSpace,gif=F))
      }
    }
    numCols = 4
    if(length(plotList)<4) numCols = length(plotList)
    return(grid.arrange(grobs=plotList,ncol=numCols))
  }
  
  mapsOnAgridCPUE = function(dataset,fractionName,metierNum,scaleSpace,scaleTime,popNum,timeStep,scenames,rtilike){
    # catchMaps=explicitCatchMaps
    # scaleSpace="Node" #input$selmap.scale
    # scaleTime="month"
    # popNum=7
    # timeStep=2# input$selmap.month
    # metierNum=3 #metierNum
    # scenames=c("A","B")
    # fractionName="Landings"
    # plotList=list()
    # data2plot=NULL
    
    if(scaleSpace=="Node") scaleSpace = "All"
    if(scaleSpace=="RTI rectangle") scaleSpace = "RTI"
    if(scaleSpace=="ICES rectangle") scaleSpace = "ICES"
    if (scaleTime=="year") timeStep = timeStep -1
    plotList=list()
    i=0
    for(data2map in dataset){
      if(scaleSpace=="All") data2plot = data2map$interimMap
      if(scaleSpace=="ICES") data2plot = data2map$interimMapICES
      if(scaleSpace=="RTI") data2plot = data2map$interimMapRTI
      i=i+1
      if (rtilike=="No") plotList[[i]]=as_grob(getExplicitCPUEMap(data2plot,popNum,timeStep,metierNum,fractionName,sce=scenames[i],scaleTime,resScale=scaleSpace,gif=F))
      if (rtilike=="Yes") plotList[[i]]=as_grob(getExplicitCPUERTILikeMap(data2plot,popNum,timeStep,metierNum,fractionName,sce=scenames[i],scaleTime,gif=F))
    }
    numCols = 4
    if(length(plotList)<4) numCols = length(plotList)
    return(grid.arrange(grobs=plotList,ncol=numCols))
  }
  
  output$cumulativeMap <- renderPlot({
    # # scedir <- "data/CelticSea44/"
    #  scedir <- ""
    # # scenarios <- dir(scedir, "^sce[^_]*")
    # # m <- regexpr("sce[^_]*", scenarios)
    # # scenarios <- unique(regmatches(scenarios, m))
    # first <- function(x) x[1]
    # scenarios <- unique(sapply(strsplit(dir("output", ".*Rds"), split = "_"), first))
    # outdir <- "output"
    # 
    # makeCumulativeMap(scedir = scedir, outdir = outdir, scenarios = scenarios,a_type = input$sel.mapquantity, in_relative = FALSE)
    plot2render = NULL
    
    if(input$selmap.variable=="Effort"){
      metierNum=input$selmap.metier
      if(metierNum=="All") metierNum = NA
      plot2render=mapsOnAgridEffort(effortMaps,scale=input$selmap.scale,metierNum,monthNum=input$selmap.month,attr(effortMaps,"names"))
    }
    
    if(input$selmap.variable=="Biomass"){
      numOfPop=input$selmap.pop
      plot2render=mapsOnAgridBiomass(biomassMaps,scale=input$selmap.scale,popNum=numOfPop,monthNum=input$selmap.month,attr(biomassMaps,"names"))
    }
    
    if(input$selmap.variable%in%c("Discards","Landings")){
      metierNum=input$selmap.metier
      numOfPop=input$selmap.pop
      catchType=input$selmap.catchVariable

      if(input$selmap.timescale=="month") timeStep = input$selmap.month
      if(input$selmap.timescale=="year") timeStep = input$selmap.year-ybeg+1
      
      if(catchType == "Explicit") plot2render=mapsOnAgridCatch(dataset=explicitCatchMaps,catchType,fractionName=input$selmap.variable,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,attr(biomassMaps,"names"))
      if(catchType == "Implicit") plot2render=mapsOnAgridCatch(dataset=implicitCatchMaps,catchType,fractionName=input$selmap.variable,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,attr(biomassMaps,"names"))
      if(catchType == "All") plot2render=mapsOnAgridCatch(dataset=allCatchMaps,catchType,fractionName=input$selmap.variable,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,attr(biomassMaps,"names"))
    }
    
    if(input$selmap.variable%in%c("DPUE","LPUE")){
      metierNum=input$selmap.metier
      numOfPop=input$selmap.pop
      fractionLabel="Landings"
      if(input$selmap.variable=="DPUE") fractionLabel= "Discards"
      
      if(input$selmap.timescale=="fortnight") timeStep = input$selmap.fortnight
      if(input$selmap.timescale=="month") timeStep = input$selmap.month
      if(input$selmap.timescale=="year") timeStep = input$selmap.year-ybeg+1
      
      
      if(input$selmap.rtilike=="No"){
        if(input$selmap.timescale!="fortnight") plot2render=mapsOnAgridCPUE(explicitCPUEMaps,fractionName=fractionLabel,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,scenames=attr(explicitCPUEMaps,"names"),rtilike=input$selmap.rtilike)
        if(input$selmap.timescale=="fortnight") plot2render=mapsOnAgridCPUE(explicitCPUEMapsFortnight,fractionName=fractionLabel,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,scenames=attr(explicitCPUEMaps,"names"),rtilike=input$selmap.rtilike)
      }else{
        plot2render=mapsOnAgridCPUE(explicitCPUEMapsFortnight,fractionName=fractionLabel,metierNum,scaleSpace=input$selmap.scale,scaleTime=input$selmap.timescale,popNum=numOfPop,timeStep,scenames=attr(explicitCPUEMaps,"names"),rtilike=input$selmap.rtilike)
      }
    }
    
    return(plot2render)
    
  })

  output$linePlot <- renderPlot({
    if(input$sel.var%in%c("Landings","Discards")){
      cumulTime=input$quantCumSum
      scenamesSel = input$sel.sce
      chosenFraction=input$sel.var
      metierSel="All"
      if(input$sel.expimp=="Explicit vessels") metierSel=input$sel.metier
      popSel=input$sel.pop
      aggScale=input$sel.aggTime
      facet="métier"
      if(input$sel.facet=="scenario") facet="scenario"
      if(input$sel.bothFractions=="Yes") chosenFraction=c("Landings","Discards")
      Whichscenames=which(scenamesSel%in%popdynscenarios)
      
      plotList=list()
      j=0
      if(facet=="scenario"){
        for(i in Whichscenames){
          j=j+1
          #plotList[[j]]=as_grob(getExplicitCatchTimeSeries(explicitCatchTimeSeries[[i]],aggScale,metierSel,popSel,chosenFraction,sce=scenamesSel[i]))
          if(input$sel.expimp=="Explicit vessels") plotList[[j]]=as_grob(getExplicitCatchTimeSeries(explicitCatchTimeSeries,aggScale,metierSel,popSel,chosenFraction,sce=scenamesSel[i],facet,cumulTime))
        }
      }
      if(facet=="métier"){ 
        for(metName in metierSel){
          j=j+1
          if(input$sel.expimp=="Explicit vessels") plotList[[j]]=as_grob(getExplicitCatchTimeSeries(explicitCatchTimeSeries,aggScale,metName,popSel,chosenFraction,sce=scenamesSel[Whichscenames],facet,cumulTime))
          if(input$sel.expimp=="Unmodelled vessels") plotList[[j]]=as_grob(getImplicitCatchTimeSeries(implicitCatchTimeSeries,aggScale,popSel,chosenFraction,sce=scenamesSel[Whichscenames],cumulTime))
          if(input$sel.expimp=="All vessels") plotList[[j]]=as_grob(getAllCatchTimeSeries(explicitCatchTimeSeries,implicitCatchTimeSeries,aggScale,popSel,chosenFraction,sce=scenamesSel[Whichscenames],cumulTime))
        } 
      }
      
      numCols = 2
      if(length(plotList)<2) numCols = length(plotList)
      plot2render=grid.arrange(grobs=plotList,ncol=numCols)
    }
    
    if(input$sel.var=="F"){
      scenamesSel = input$sel.sce
      fbar=T
      if(input$sel.fbar=="per age class") fbar=F
      plot2render = getFTimeSeries(subset(FTimeSeries), fbar)
    }
    
    if(input$sel.var=="Effort"){
      scenamesSel = input$sel.sce
      aggScale=input$sel.aggTime
      metNum=input$sel.metier
      cumulTime=input$quantCumSum
      plot2render = getEffortTimeSeries (effortTimeSeries,aggScale,metNum,ybeg,cumulTime)
    }
    
    if(input$sel.var%in%c("LPUE","DPUE")){
      scenamesSel = input$sel.sce
      chosenFraction=input$sel.var
      metierSel=input$sel.metier
      popSel=input$sel.pop
      aggScale=input$sel.aggTime
      facet=input$sel.facet
      if(input$sel.var=="LPUE") chosenFraction=c("Landings")
      if(input$sel.var=="DPUE") chosenFraction=c("Discards")
      if(input$sel.bothFractions=="Yes") chosenFraction=c("Landings","Discards")
      Whichscenames=which(scenamesSel%in%popdynscenarios)
      
      if(aggScale %in% c("year","month")) data2plot=explicitCPUETimeSeries
      if(aggScale == "fortnight") data2plot=explicitCPUETimeSeriesFortnight
      
      plotList=list()
      j=0
      if(facet=="scenario"){
        for(i in Whichscenames){
          j=j+1
          plotList[[j]]=as_grob(getExplicitCPUETimeSeries(data2plot,aggScale,metierSel,popSel,chosenFraction,sce=scenamesSel[i],facet))
        }
      }
      if(facet=="métier"){ 
          for(metName in metierSel){
            j=j+1
            plotList[[j]]=as_grob(getExplicitCPUETimeSeries(data2plot,aggScale,metName,popSel,chosenFraction,sce=scenamesSel[Whichscenames],facet))
          } 
      }
      numCols = 2
      if(length(plotList)<2) numCols = length(plotList)
      plot2render=grid.arrange(grobs=plotList,ncol=numCols)
    }
    
    if(input$sel.var=="Populations"){
      scenamesSel = input$sel.sce
      aggScale=input$sel.aggTime
      variable=input$sel.varPop
      colourVar=input$sel.varPopSize
      
      if (colourVar=="No") plot2render = getNBSSBTimeSeries(biomassTimeSeries,aggScale,variable)
      if (colourVar!="No"){
        if (colourVar=="x-axis") colourVar ="TStep"
        if (colourVar=="colour") colourVar ="sizeBin"
        
        plotList=list()
        i=0
        for(sce in scenamesSel){
          i=i+1
          plotList[[i]] = getNBSSBLengthBin(subset(biomassTimeSeries,scename==sce),aggScale,variable,colourVar)
        }
        numCols = 2
        if(length(plotList)<2) numCols = length(plotList)
        plot2render=grid.arrange(grobs=plotList,ncol=numCols)
      }
    }
    
    if(input$sel.var=="Economics"){
      scenamesSel = input$sel.sce
      facet=input$sel.facet
      metChoice=input$sel.metier
      variable=input$sel.varEco
      colourVar=input$sel.varPopSize
      cumulTime=input$quantCumSum
      aggScale=input$sel.aggTime
      Whichscenames=which(scenamesSel%in%popdynscenarios)
      
      if("All" %in% metChoice) metChoice=NA
      
      if (facet=="métier") plot2render= getEconomicTimeSeries(economicsTimeSeries,variable,cumulTime,metChoice, facet,sce=scenamesSel[Whichscenames],aggScale)
      if (facet=="scenario"){
        plotList=list()
        i=0
        for(sce in scenamesSel){
          i=i+1
          plotList[[i]] = getEconomicTimeSeries(subset(economicsTimeSeries,scename==sce),variable,cumulTime,metChoice, facet,sce=scenamesSel[i],aggScale)
        }
        numCols = 2
        if(length(plotList)<2) numCols = length(plotList)
        plot2render=grid.arrange(grobs=plotList,ncol=numCols)
      }
    }
    
    return(plot2render)
    
    # req(input$sel.var, input$sel.sce)
    # par(mar = c(4, 5, 1, 1))
    # do_polygon_plot(
    #   a_variable = input$sel.var,
    #   ybeg = ybeg,
    #   yend = yend,
    #   documsum = input$quantCumSum,
    #   a_set_of_scenarios = input$sel.sce,
    #   the_scenario_names =names(selsce(popdynscenarios,scenames)),
    #   selected = selected,
    #   export = FALSE,
    #   a_ylab = switch(input$sel.var,
    #                   gradva = "Accumulated Gross Value Added (million EUR)",
    #                   rev_from_av_prices = "Income from landings (million EUR)",
    #                   effort = "Effort",
    #                   nbtrip = "Number of trips",
    #                   totland = "Total landings",
    #                   ""),
    #   add_legend = TRUE,
    #   color_legend = c(rgb(94/255,79/255,162/255,0.5), rgb(158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
    #                    rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
    #   a_width = 3500,
    #   a_height = 1000
    # 
    # )
  })
  
  output$indicatorPlot <- renderPlot({
    
      scenamesSel = input$indic.sce
      popVect=input$indic.pop
      facet=input$indic.facet
      if("All"%in%popVect) popVect = 0:(general$nbpops-1)
      Whichscenames=which(scenamesSel%in%popdynscenarios)
      
      plot2render=indicatorsPlot(annualIndicatorsList, popVect,stockNames,scenamesSel[Whichscenames],facet)
    
      return(plot2render)
  })

  # output$catchTimeSeriesPlot <- renderPlot({
  #   ## ColorBrewer: paired
  #   ##cols <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
  #   ## ColorBrewer: set3
  #   ##cols <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd")
  #   cols <-  c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666','#1B9E77','#D95F02','#7570B3',
  #              '#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C',
  #              '#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928','#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6',
  #              '#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2','#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC',
  #              '#CCCCCC','#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999','#66C2A5',
  #              '#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3','#8DD3C7','#FFFFB3','#BEBADA','#FB8072',
  #              '#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F')
  #   par(mar = c(6,4,3.5,0.9), xpd = TRUE)
  #   #onesim <- lst_loglike_agg_weight_all_scebaseline[[1]]
  #   add <- FALSE
  #   for (s in input$sel.sce2) {
  #     onesim <- get(paste0("lst_loglike_agg_weight_all_", s))[[1]]
  #     onesim <- onesim[onesim$year.month != "NA.NA", ]
  #     ym <- ym2date(onesim$year.month)
  #     nms <- names(onesim)
  #     selected <- onesim[, nms %in% input$sel.pop, drop = FALSE] / 1000
  #     maxima <- apply(selected, 2, max)
  #     limits <- c(0, 10, 100, 1000, Inf)
  #     labels <- paste("Max catches: ", paste(limits[-5], limits[-1], sep = "-"), "tonnes")
  #     labels[length(labels)] <- "Max catches: > 1000 tonnes"
  #     lvls <- droplevels(cut(maxima, limits, include.lowest = TRUE,
  #                            labels = labels))
  #     switch(length(levels(lvls)),
  #            "1" = par(mfrow = c(1,1)),
  #            "2" = par(mfrow = c(2,1)),
  #            "3" = par(mfrow = c(2,2)),
  #            "4" = par(mfrow = c(2,2)))
  #     for (l in levels(lvls)) {
  #       pops <- selected[, lvls == l, drop = FALSE]
  #       nms <- as.vector(sapply(names(pops), function(x) popnames$spp[paste0("pop.", popnames$idx) == x]))
  #       cls <- cols[as.integer(sub("pop.", "", names(pops))) + 1]
  #       matplot(ym, pops,
  #               type = "l", ylab = "Catch (tonnes)", xlab = "", add = add, lty = 1, lwd = 3,
  #               col = cls)
  #       mtext(l, line = 0.5, cex = 1.3)
  #       legend("bottomleft", bty = "n", legend = nms, col = cls, inset = c(0, -0.8),
  #              lty = 1, seg.len = 1, lwd = 3, box.col = "#00000022", ncol = 3, cex = 0.8)
  #     }
  #   }
  # })

#   output$fleetIndicatorsPlot <- renderPlot({
#     ## par(mar = c(6,4,3.5,0.9), xpd = TRUE)
#     
#     selOutcome=substr(selected,1,str_length(selected)-1)
#     outcomes <- get(paste0("relative_to_baseline_sce", selOutcome))
#     ## CAUTION: (not the same levels when reading or when using directly the obj in the env)
#     # levels(outcomes$scenario) <-  c("sceavchok","sceavchokpszpctrastopifchok",
#     #                                "sceavchokpszpectra",
#     #                                "sceavhtariffspszpctratariffs",   "scebaseline",
#     #                                "scesizespectrastopifchok", "scetrgthtariffspszpctratariffs")
# 
# 
#     # add baseline at 0,0,0, etc.
#     baseline <- outcomes[outcomes$scenario == outcomes$scenario[1],]  # init
#     baseline$ratio_percent <-0
#     baseline$scenario <- "baseline"
#     outcomes <- rbind.data.frame(baseline, outcomes)
#     outcomes$scenario <- factor(outcomes$scenario)
# 
#     selected_variables <- c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit","totland_explicit","sweptarea", "npv", "av_vpuf_month", "hoover")
#     outcomes           <- outcomes[outcomes$variable %in% selected_variables,]
# 
#     outcomes$variable <- factor(outcomes$variable)
#     outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c( "F. effort", "S. effort", "Nb. of trips","Trip duration",  "CPUE at fishing","Tot landings","Swept Area","NPV", "VPUF", "Income inequality"))
# 
#     
#     selected_scenarios <- input$sel.sce2
#     nms <- names(selsce(popdynscenarios,scenames))[selsce(popdynscenarios,scenames) == input$sel.sce2]
# 
#     outcomes <- outcomes[outcomes$scenario %in% selected_scenarios,]
#     #outcomes$scenario <- factor(outcomes$scenario)
#     outcomes$scenario <- factor(outcomes$scenario, levels=selected_scenarios, labels=  nms)
# 
#     outcomes[outcomes$ratio_percent< -25, "ratio_percent"] <- -25
#     outcomes[outcomes$ratio_percent>25, "ratio_percent"] <- 25
#     p <- ggplot(outcomes[outcomes$ratio_percent>=-25 & outcomes$ratio_percent<=25,], aes(factor(variable), ratio_percent))  + geom_boxplot(outlier.shape=NA)  +
#              labs(x = "Indicators", y = "% ratio over the baseline") # + ylim(-20, 20)
# 
#     p + facet_wrap( ~ scenario, ncol=2, scales="free_y")    + theme_bw() +
#        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
#              strip.background = element_blank(),
#              panel.border = element_rect(colour = "black"))  +
#        geom_abline(intercept=0, slope=0, color="grey", lty=2)
# 
# }, height = function() {((length(input$sel.sce2) + 1) %/% 2 ) * 300 })

  # output$populationSizePlot <- renderPlot({
  #   req(input$sel.pop, input$sel.sce2, input$sel.sum.szgroups)
  #   plot_popdyn(sces = input$sel.sce2,
  #               scenarios_names= names(selsce(popdynscenarios,scenames))[selsce(popdynscenarios,scenames) %in% input$sel.sce2],
  #               explicit_pops = input$sel.pop,
  #               sum_all = input$sel.sum.szgroups)
  # }, height = function() {((length(input$sel.pop) + 1) %/% 2 ) * 300 })

  # output$annualIndicPlot <- renderPlot({
  #   req(input$sel.pop, input$sel.sce2, input$sel.indic)
  #   plot_annualindic(sces = input$sel.sce2,
  #                scenarios_names = names(selsce(popdynscenarios,scenames))[selsce(popdynscenarios,scenames) %in% input$sel.sce2],
  #               explicit_pops = input$sel.pop,
  #               indic = input$sel.indic)
  # }, height = function() {length(input$sel.indic) * 150 + 150 })


  # output$barplot_landis_perpop <- renderPlot({
  #   #warningPlot("Not implemented yet")
  #   barplotTotLandingsPerSce(general=general,type_of_column="pop", selected="_all_",selected_scenarios = input$sel.sce2, scenarios_names = names(selsce(popdynscenarios,scenames))[selsce(popdynscenarios,scenames)%in%input$sel.sce2],selected_pops = sub("pop.", "", input$sel.pop), firsty="2010", lasty="2012")
  # })

  output$cumulativeMaps <- renderPlotly({

  })
}

shinyApp(ui = ui, server = server, options = list("shiny.autoload.r" = FALSE))
