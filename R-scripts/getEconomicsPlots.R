# ######### READ DISPLACE ECONOMIC OUTPUTS (time/metier/explicit-implicit/indicator (revenue,profit,fuel cost, other variable costs, GVA, gross profit, labour surplus, net profit margin))
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
# 
# months=getMonths()
# 
# sce = general$namefolderoutput[1]
# explicit_pops = 0:(general$nbpops-1)
# 
# EconomicsPertrip = read.table(paste(general$main.path,"/",general$case_study,"/",sce,"/loglike_",sce,length(general$namesimu[2][[1]]),".dat",sep=""))
# names(EconomicsPertrip)= c('TStepDep', 'TStep', 'reason_back','cumsteaming', 'idx_node',  'Id', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist',  paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", "fuelcost", "vpuf", "gav", "gradva","sweptr", "revpersweptarea",  paste('disc_',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit",  "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips") 
# EconomicsPertrip = EconomicsPertrip %>% 
#   select(c(TStepDep,TStep,cumsteaming,Id,VE_REF,fuelcons,freq_metiers,rev_from_av_prices,rev_explicit_from_av_prices,fuelcost,vpuf,gradva,GVA,LabourSurplus,GrossProfit,NetProfit,NetProfitMargin,NetPresentValue,numTrips))
# 
# #revenue = rev_from_av_prices
# #revenue (modelled only, not accounted for unmodelled stocks) = rev_explicit_from_av_prices
# #Fuel cost (steam+fish) = fuelcost
# #VPUF = vpuf
# #contribution margin = revenue - fuelcost ("Profit") = gradva
# 
# 
# #Get non-fuel variable costs (and other stuff useful for sanity checks)
# economicFeatures=read.table(file=file.path(general$main.path.ibm, paste("vesselsspe_", general$case_study, sep=''),paste("vesselsspe_economic_features.dat",sep='')),sep="|")
# names(economicFeatures)=c("VE_REF","Nb_crew","Annual_other_income","Landing_costs_percent","Crewshare_and_unpaid_labour_costs_percent","Other_variable_costs_per_unit_effort","Annual_insurance_costs_per_crew","Standard_labour_hour_opportunity_costs","Standard_annual_full_time_employment_hours","Other_annual_fixed_costs","Vessel_value","Annual_depreciation_rate","Opportunity_interest_rate","Annual_discount_rate")
# economicFeatures = economicFeatures%>% 
#   mutate(VE_REF=as.character(VE_REF))
# 
# EconomicsPertrip = EconomicsPertrip %>% 
#   merge(economicFeatures, by=c("VE_REF")) %>% 
#   mutate(nonFuelvariableCost=Other_variable_costs_per_unit_effort*cumsteaming) %>% 
#   mutate(freq_metiers = sapply(as.character(freq_metiers), function(x) strsplit(x, split="\\(")[[1]][2]))%>% 
#   mutate(freq_metiers = as.numeric(sapply(as.character(freq_metiers), function(x) strsplit(x, split=")")[[1]][1])))
# 
# 
# #Explore the variables to understand them (which are cumulative through the whole time series?)
# 
# # EconomicsPertrip %>% 
# #   filter(VE_REF==levels(VE_REF)[100]) %>% 
# #   ggplot(aes(x=TStep,y=NetProfit))+
# #     geom_line()+
# #     geom_point()+
# #     geom_vline(xintercept=c(1,months$TStep[months$TStep<=26285]),linetype="dotted")+
# #     theme_minimal()
# 
# # WARNING : GVA is cumulative
# # WARNING : LabourSurplus is cumulative
# # WARNING : GrossProfit is cumulative
# # WARNING : NetProfit is cumulative
# # WARNING : NetProfitMargin LOOKS cumulative
# # WARNING : NetPresentValue LOOKS cumulative
# # WARNING : numTrips is cumulative
#                                  
# ##Sanity checks
# #
# ## rev_from_av_prices == rev_explicit_from_av_prices except for pelagic metiers OK
# #unique(EconomicsPertrip$freq_metiers[which(EconomicsPertrip$rev_from_av_prices != EconomicsPertrip$rev_explicit_from_av_prices)])
# ## Metiers 5, 16 and 11 : all PT. What is the ratio between both revenues? ALL GOOD
# #range(1-(EconomicsPertrip$rev_from_av_prices[which(EconomicsPertrip$rev_from_av_prices != EconomicsPertrip$rev_explicit_from_av_prices)]-EconomicsPertrip$rev_explicit_from_av_prices[which(EconomicsPertrip$rev_from_av_prices != EconomicsPertrip$rev_explicit_from_av_prices)])/EconomicsPertrip$rev_from_av_prices[which(EconomicsPertrip$rev_from_av_prices != EconomicsPertrip$rev_explicit_from_av_prices)])
# 
# # revenue = income
# # revenue - fuelcosts = gradva . Sometimes, a difference of 1 EUR. Why? Rounding error?
# # check = EconomicsPertrip %>%
# #   mutate(SanityCheck = abs(gradva - (rev_from_av_prices - fuelcost))) %>%
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
# 
# # GVA = revenue - fuelcost - var costs - fixed costs. WARNING! GVA is cumulative. There is very little difference (50EUR difference after 3 years)
# # check = EconomicsPertrip %>%
# #   arrange(VE_REF,TStepDep,TStep) %>%
# #   group_by(VE_REF) %>%
# #   mutate(cumRevenue = cumsum(rev_from_av_prices), cumFuelCost = cumsum(fuelcost), cumNonFuelvariableCost = cumsum(nonFuelvariableCost)) %>%
# #   ungroup() %>%
# #   mutate(SanityCheck = abs(cumRevenue + Annual_other_income*TStep/8761 - cumFuelCost - cumNonFuelvariableCost - Other_annual_fixed_costs*TStep/8761 - GVA)) %>%
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
# 
# # GVA = rgradva- var costs - fixed costs. There is very little difference (25EUR difference after 3years)
# # check = EconomicsPertrip %>%
# #   arrange(VE_REF,TStepDep,TStep) %>%
# #   group_by(VE_REF) %>%
# #   mutate(cumGradva = cumsum(gradva), cumFuelCost = cumsum(fuelcost), cumNonFuelvariableCost = cumsum(nonFuelvariableCost)) %>%
# #   ungroup() %>%
# #   mutate(SanityCheck = abs(cumGradva - cumNonFuelvariableCost - Other_annual_fixed_costs*TStep/8761 - GVA)) %>%
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
# 
# # GrossProfit = GVA - crew share. OK
# # check = EconomicsPertrip %>%
# #   mutate(SanityCheck = abs(GVA*((100-Crewshare_and_unpaid_labour_costs_percent)/100) - GrossProfit))%>%
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
# #   range(check$SanityCheck)
# 
# # GrossProfit = GVA - Labour surplus. OK, except for a few 1 EUR differences
# # check = EconomicsPertrip %>%
# #   mutate(SanityCheck = abs(GVA - LabourSurplus - GrossProfit))%>% 
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
# #   range(check$SanityCheck)
#   
# # GrossProfit = NetProfit + depreciated value vessel. ALL GOOD
# # check = EconomicsPertrip %>%
# #   arrange(VE_REF,TStepDep,TStep) %>%
# #   mutate(SanityCheck = abs(NetProfit - GrossProfit + Vessel_value*TStep/8761)) %>%
# #   ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #     geom_line(alpha=0.1)+
# #     theme_minimal()
#   
# # Net profit Margin = Net Profit / revenue. Discrepances up to 1500 EUR for very small vessels, due to discrepancies of a few EUR in cum revenue. Probably due to rounding errors when DISPLACE exports stuff.
# # check = EconomicsPertrip %>%
# #     arrange(VE_REF,TStepDep,TStep) %>%
# #     group_by(VE_REF) %>%
# #     mutate(cumRevenue = cumsum(rev_from_av_prices)) %>%
# #     ungroup() %>% 
# #     mutate(NetProfitMarginEstim = 100*NetProfit/cumRevenue) %>%
# #     mutate(NetProfitMarginEstim = replace(NetProfitMarginEstim,cumRevenue<=0,NA)) %>% # Should be 0, but buggy, so NA.
# #     mutate(SanityCheck = NetProfitMarginEstim/NetProfitMargin)%>%
# #     #filter(SanityCheck=1500) %>% 
# #     ggplot(aes(x=TStep,y=SanityCheck,group=VE_REF))+
# #       geom_line(alpha=0.1)+
# #       theme_minimal()+
# #       expand_limits(y=0)
# 
# save(EconomicsPertrip,file=paste(general$main.path,general$case_study,sce,"output/forEconomicsPlots.Rdata",sep="/"))

##################
###
###TIME SERIES PLOTS
###
##################

returnLastNonNAValue <- function(values,pos){
  lastValues=values[1:pos]
  mostRecent=max(which(!is.na(lastValues)))
  return(lastValues[mostRecent])
}

getEconomicTimeSeries = function(data4plot = EconomicsPertrip,variable="revenue",cumulTime=T,metChoice=0, facet=F){

  legendY = paste(variable,"\n","(EUR)",sep="")
  legendTitle =  paste(variable,"\n",sce,sep="")
  #if(averageTrip)legendY = paste("Average\n",variable,"\nper trip\n(EUR)",sep="")
  if(cumulTime) legendTitle = paste("Cumul of ",variable,"\n",sce,sep="")
  #if(cumulTime & averageTrip) legendY = paste("Cumulative\naverage\n",variable,"\nper trip\n(EUR)",sep="")
  
  if(variable=="revenue") data4plot$value = data4plot$rev_from_av_prices
  if(variable=="profit") data4plot$value = data4plot$gradva
  if(variable=="fuelcost") data4plot$value = data4plot$fuelcost
  if(variable=="nonFuelvariableCost") data4plot$value = data4plot$nonFuelvariableCost
  if(variable=="GVA") data4plot$value = data4plot$GVA
  if(variable=="GrossProfit") data4plot$value = data4plot$GrossProfit
  if(variable=="LabourSurplus") data4plot$value = data4plot$LabourSurplus
  if(variable=="NetProfit") data4plot$value = data4plot$NetProfit
  if(variable=="NetProfitMargin") data4plot$value = data4plot$NetProfitMargin
  
  if (!is.na(metChoice)) data4plot = subset(data4plot,freq_metiers%in%metChoice)
  if (is.na(metChoice)) data4plot$freq_metiers="All"

  if(variable %in%c("revenue","profit","fuelcost","nonFuelvariableCost")){
    data4plot = data4plot %>% 
      select(value,TStep,freq_metiers) %>% 
      group_by(TStep,freq_metiers) %>% 
      summarize(value=sum(value))%>% 
      ungroup()
    
    if (!is.na(metChoice)) data4plot= mutate(data4plot, freq_metiers = sapply(freq_metiers, function(x) metierNames$name[metierNames$metierId==x] ))
    
    if(cumulTime){
      data4plot = data4plot %>% 
        arrange(TStep,freq_metiers) %>% 
        group_by(freq_metiers) %>% 
        mutate(value=cumsum(value)) %>% 
        ungroup()
    }
  }
  
  if(!variable %in%c("revenue","profit","fuelcost","nonFuelvariableCost")){
    data4plot = data4plot %>% 
      select(value,TStep,freq_metiers,VE_REF) %>% 
      complete(nesting(freq_metiers, VE_REF),TStep=full_seq(TStep,1)) %>% 
      group_by(VE_REF) %>% #If for the first time step, GVA is NA, then replacce by 0; do not change it otherwise
      mutate(value=replace(value,TStep==min(TStep) & is.na(value),0)) %>% 
      ungroup() %>% 
      arrange(freq_metiers,VE_REF,TStep) %>% 
      fill(value,.direction="down") %>% 
      group_by(freq_metiers,TStep) %>% 
      summarize(value=sum(value))%>% 
      ungroup()
    
      if (!is.na(metChoice)) data4plot= mutate(data4plot, freq_metiers = sapply(freq_metiers, function(x) metierNames$name[metierNames$metierId==x] ))
    
    if(!cumulTime){
      data4plot = data4plot %>% 
        arrange(freq_metiers,TStep) %>% 
        group_by(freq_metiers) %>% 
        mutate(value=c(min(value[TStep==min(TStep)]),diff(value))) %>% 
        ungroup()
    }
  }

  plot2return = ggplot(data4plot,aes(x=TStep,y=value))+#,colour=freq_metiers,linetype=freq_metiers,shape=freq_metiers))+
    geom_vline(xintercept=months$TStep[months$TStep<max(data4plot$TStep)],linetype="dotted")+
    labs(x="Time step (hour)",y=legendY,colour="Métier",linetype="Métier",shape="Métier", title = legendTitle)+
    expand_limits(y=0)+
    theme_minimal()+
    theme(axis.title.y = element_text(angle=0,vjust=0.5))
  
  if(facet){
    plot2return = plot2return + facet_wrap(~freq_metiers,scales="free_y")+
                                        geom_line(size=1)+
                                        geom_point(size=2)
  }
  if(!facet){
    plot2return = plot2return +  scale_colour_manual(name="Métier", values=rep(scales::hue_pal()(5),4)[1:length(metChoice)])+
                                  scale_linetype_manual(name="Métier", values=c(rep(1:4,each=5),3)[1:length(metChoice)])+
                                  scale_shape_manual(name="Métier", values=c(rep(c(15:18),each=5),17)[1:length(metChoice)])+
                                  geom_line(size=1,aes(colour=freq_metiers,linetype=freq_metiers))+
                                  geom_point(size=2,aes(colour=freq_metiers,shape=freq_metiers))
  }
  return(plot2return)
}

#getEconomicTimeSeries(EconomicsPertrip,variable,cumulTime,metChoice,facet)
