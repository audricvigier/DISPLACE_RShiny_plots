# ######### READ DISPLACE EFFORT OUTPUTS (area, metier, time)
# #
# # DATE ..........................: Mon May 17 14:50:00 2021
# # AUTHOR.........................: Audric Vigier
# #
# #----------------------------------------------------------------
# #
# # R version......................: R version 3.6.0 (2019-04-26)
# #----------------------------------------------------------------
# 
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
# source("D:/work/Displace/DISPLACE_RShiny_plots/R-scripts/setGeneralVariable.R", local = TRUE)
# 
# ##################
# ###
# ###INPUTS
# ###
# ##################
# 
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
#                                       theScenarios= c("baseline2","baseline3","baseline4"),
#                                       nbSimus=1,
#                                       useSQLite=FALSE)
# explicit_pops = 0:(general$nbpops-1)
# 
# ##################
# ###
# ###END INPUTS
# ###
# ##################
# 
# ##################
# ###
# ###LOAD TARIFF MAPS
# ###
# ##################
# 
# sce=general$namefolderoutput[1]
# 
# tariffsMaps = read.table(file=paste(general$main.path,"/",general$case_study,"/",sce,"/popnodes_tariffs_simu1.dat",sep=""))
# names(tariffsMaps)=c("TStep","NodeId","Long","Lat",seq(0:(dim(tariffsMaps)[2]-5))) # one tariff per metier
# ##################
# ###
# ###CATCH TIME SERIES: FRACTION, POP, TIME STEP (MONTH), METIER, SIZE, IMPLICIT/EXPLICIT. AREA INFO ONLY FOR THE MAPS. I IGNORE SIZE OF COMPOSITION OF CTACH AT THE MOMENT.
# ###
# ##################
# 
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
# getMonths = function(){
#   months=read.table(file=paste(general$main.path.ibm, "/simusspe_CelticSea/tstep_months_2010_2020.dat",sep=""),header=F) %>%
#     rename(TStep=V1) %>%
#     mutate(month=1:n())# Hours give the end of the month
# }
# months=getMonths()

##################
###
###CONDITION DATASET
###
##################

# tariffsMaps = merge(tariffsMaps, months, by=c("TStep"))
# tariffsMaps=melt(tariffsMaps,id.vars=c("TStep","NodeId","Long","Lat","month"))
# levels(tariffsMaps$variable)=metierNames$name
# 
# dataset2plot = tariffsMaps
# timeStep = 1
# metierSel=c("largeA","largeBOT")

# dataset2plot=RTImaps %>% 
#   mutate(scename="baseline2")
# timeStep=10
# metierSel="largeBOT"
# sce="baseline2"

getTariffMap = function(dataset2plot,timeStep,metierSel,sce){
  dataset2plot %>% 
  filter(month==timeStep & variable %in% metierSel & scename==sce) %>% 
  ggplot(aes(x,y,fill=value))+
    geom_tile(colour="black",height=0.25,width=0.5)+
    facet_wrap(~variable)+
    labs(x="Longitude",y="Latitude",colour="RTI\ntariff", title = paste("RTI tariff (without métier multiplicator) month ",timeStep,"\nscenario ",sce,sep=""))+
    scale_fill_gradientn(colours = viridis(7),limits=c(0,5))+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5))
}

#getTariffMap(tariffsMaps,timeStep)
