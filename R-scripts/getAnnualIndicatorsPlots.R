# ######### READ DISPLACE ANNUAL INDICATORS (F, landings, discards, SSB, TAC)
# #
# # DATE ..........................: Tue Jun 08 11:37:50 2021
# # AUTHOR.........................: Audric Vigier
# #
# #----------------------------------------------------------------
# #
# # R version......................: R version 3.6.0 (2019-04-26)
# #----------------------------------------------------------------
# 
# rm(list=ls())
# library(reshape2)
# library(tidyverse)
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
#                                       theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
#                                       nbSimus=20,
#                                       useSQLite=FALSE)
# 
# #################
# ##
# ##END INPUTS
# ##
# #################
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
# annualIndicatorsList=list()
# for (sce in general$namefolderoutput){
#   load(file=paste("D:/DISPLACE_outputs/CelticSea/",sce,"/output/forAnnualIndicatorsPlots.Rdata",sep=""))
#   annualIndicatorsList[[sce]]=annualIndicators 
# }
# 
# annualIndicatorsList = plyr::ldply(annualIndicatorsList)

##################
###
###BAR PLOT
###
##################

#Options: facetting per pops : YES OR NO???
#Options: populations subsetting, scenario subsetting

# stockNames
# facet="no"
# popVect = c(1,5,8,9,11,20,21)
# scenames = general$namefolderoutput
indicatorsPlot = function(annualIndicatorsList, popVect,stockNames,scenames,facet){

  plot2return = annualIndicatorsList %>% 
    rename(PopId=stk) %>% 
    filter(PopId %in% popVect & sce%in%scenames) %>% 
    merge(stockNames, by=c("PopId")) %>% 
    ggplot()
  
  if(facet=="pop"){
    plot2return = plot2return+
      geom_point(size=3,aes(x=variable,y=ratios,colour=sce,shape=sce))+
      facet_wrap(~spp,scales="free_y")+
      labs(x=paste("Indicator"),y="Value",colour="Scenario",shape="Scenario")
  }
  if(facet=="variable"){
    plot2return = plot2return+
      geom_point(size=3,aes(x=spp,y=ratios,colour=sce,shape=sce))+
      facet_wrap(~variable,scales="free_y")+
      labs(x=paste("Population"),y="Value",colour="Scenario",shape="Scenario")
  }
  if(facet=="scenario"){
    plot2return = plot2return+
      geom_point(size=3,aes(x=variable,y=ratios,colour=spp,shape=spp))+
      facet_wrap(~sce,scales="free_y")+
      labs(x=paste("Population"),y="Value",colour="Scenario",shape="Scenario")+
      scale_colour_manual(values=rep(scales::hue_pal()(5),6)[1:length(popVect)])+
      scale_shape_manual(values=rep(c(13:18),each=5)[1:length(popVect)])
  }
  if(facet=="no"){
    plot2return = plot2return+
      geom_point(size=3,aes(x=variable,y=ratios,colour=sce,shape=sce))+
      labs(x=paste("Indicator"),y="Value",colour="Scenario",shape="Scenario")
  }
  plot2return = plot2return +
    geom_hline(yintercept=1,colour="black")+
    scale_y_log10()+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5),axis.text.x = element_text(angle=45,vjust=1,hjust=1),text=element_text (size=20))
  
  return(plot2return)
}
