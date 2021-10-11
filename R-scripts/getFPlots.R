# ######### READ DISPLACE F OUTPUTS (pop, age, time)
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
# #load(file=paste(general$main.path,general$case_study,general$namefolderoutput[1],"output/forEffortPlots.Rdata",sep="/"))
# ##################
# ###
# ###END INPUTS
# ###
# ##################
# 
# sce = general$namefolderoutput[1]
# # FestimatesMonth = read.table(file=paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_F_calib_multipliers_",length(general$namesimu[[1]]),".dat",sep=""),header=F)
# # names(FestimatesMonth)=c("TStep","PopId",0:10)
# FestimatesYear = read.table(file=paste(general$main.path,"/",general$case_study,"/",sce,"/popdyn_annual_indic_calib_multipliers_",length(general$namesimu[[1]]),".dat",sep=""),header=F)
# names(FestimatesYear)=c("TStep","pop","multi","multi2","Fbar","totland_kg","totdisc_kg","SSB_kg","tac","N0","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","F0","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","W0","W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","M0","M1","M2","M3","M4","M5","M6","M7","M8","M9","M10")
# 
# # Fbarage=as.data.frame(t(sapply(0:(general$nbpops-1),function(numPop) as.numeric(read.table(file=paste(general$main.path.ibm,"/popsspe_",general$case_study,"/",numPop,"spe_fbar_amin_amax_ftarget_Fpercent_TACpercent.dat",sep=""))[1:2])))) %>% 
#   # mutate(PopId=0:26) %>% 
#   # rename(amin=V1,amax=V2)
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
# ##################
# ###
# ###GET F BAR AND AND PER EACH FOR YEARS ONLY
# ###
# ##################
# 
# # ##################
# # #F month
# # 
# # FestimatesMonth = FestimatesMonth %>% 
# #   melt(id.vars=c("TStep","PopId")) %>% 
# #   rename(age=variable) %>% 
# #   mutate(age=as.numeric(levels(age))[age]) %>% 
# #   # group_by(TStep,PopId) %>% 
# #   # mutate(Fbar=mean(value[age%in%c(Fbarage$amin[Fbarage$PopId==PopId]:Fbarage$amax[Fbarage$PopId==PopId])])) %>% 
# #   # ungroup() %>% # Not matching F bar definition
# #   mutate(timeScale="month")
# # 
# # # not really per month: it is a cumulative F through a year, hence F increses from Jan to Dec.... keep only last time step of each year then! Do not use these files, useless.
# 
# ##################
# #F year
# 
# FestimatesYear = FestimatesYear %>% 
#   select(c(TStep,pop,Fbar,paste("F",0:10,sep=""))) %>% 
#   rename(PopId=pop) %>% 
#   melt(id.vars=c("TStep","Fbar","PopId")) %>% 
#   rename(age=variable) %>% 
#   mutate(age=as.character(age)) %>%
#   mutate(age=as.numeric(sapply(age,function(x) strsplit(x, split="F")[[1]][2]))) %>% 
#   # group_by(TStep,PopId) %>% 
#   # mutate(Fbar2=mean(value[age%in%c(Fbarage$amin[Fbarage$PopId==PopId]:Fbarage$amax[Fbarage$PopId==PopId])])) %>% 
#   # ungroup() %>% 
#   # mutate(diff=abs(Fbar-Fbar2)) %>% # Does not match Fbar definition: I cannot derive F bar like that
#   mutate(year = factor(TStep,labels=0:(length(unique(TStep))-1))) %>% 
#   mutate(year = as.numeric(levels(year))[year]) %>% 
#   merge(stockNames,by=c("PopId"))
# 
# save(FestimatesYear,file=paste(general$main.path,general$case_study,sce,"output/forFPlots.Rdata",sep="/"))

##################
###
###TIME SERIES PLOTS
###
##################

getFTimeSeries = function(FestimatesYear, fbar=TRUE){
  
  if(!fbar){
  plotList=list()
  for(sce in unique(FestimatesYear$scename)){
  plotList[[sce]] = FestimatesYear %>% 
	  filter(scename==sce)%>%
      mutate(age=factor(age)) %>% 
      ggplot(aes(x=year,y=value,colour=age,linetype=age,shape=age))+
        geom_line(size=1)+
        geom_point(size=2)+
        facet_wrap(~spp,scales="free_y")+
        labs(x="Time step (year)",y="Fishing\nmortality",colour="Age",linetype="Age",shape="Age", title = paste("Fishing mortality per age class\n",sce,sep=""))+
        scale_colour_manual(name="Age", values=rep(scales::hue_pal()(5),3)[1:11])+
        scale_linetype_manual(name="Age", values=c(rep(1:2,each=5),3))+
        scale_shape_manual(name="Age", values=c(rep(c(15:16),each=5),17))+
        expand_limits(y=0)+
        theme_minimal()+
        theme(axis.title.y = element_text(angle=0,vjust=0.5),text=element_text (size=20))
		
	plotList[[sce]]=as_grob(plotList[[sce]])
	}
		
	numCols = 4
    if(length(plotList)<4) numCols = length(plotList)
    plot2return=grid.arrange(grobs=plotList,ncol=numCols)
  }
  
  if(fbar){
    plot2return = FestimatesYear %>% 
      ggplot(aes(x=year,y=Fbar,colour=scename,linetype=scename,shape=scename))+
        geom_line(size=1)+
        geom_point(size=2)+
        facet_wrap(~spp,scales="free_y")+
        labs(x="Time step (year)",y="Fishing\nmortality", title ="Fbar")+
        scale_colour_manual(name="Scenario", values=rep(scales::hue_pal()(5),3)[1:length(unique(FestimatesYear$scename))])+
        scale_linetype_manual(name="Scenario", values=c(rep(1:2,each=5),3)[1:length(unique(FestimatesYear$scename))])+
        scale_shape_manual(name="Scenario", values=c(rep(c(15:16),each=5),17)[1:length(unique(FestimatesYear$scename))])+
        expand_limits(y=0)+
        theme_minimal()+
        theme(axis.title.y = element_text(angle=0,vjust=0.5),text=element_text (size=20))
  }
  
  return(plot2return)
}
