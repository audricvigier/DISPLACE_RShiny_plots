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
library(lattice)         
library(plyr)
library(tidyverse)
displaceplotLib="D:/work/Displace/displaceplot/R"
for (file in list.files(displaceplotLib)) source(paste(displaceplotLib,file,sep="/"))
shinyLib="D:/work/Displace/DISPLACE_RShiny_plots/R-scripts"
for (file in list.files(shinyLib,pattern=".R")[-c(4)]) source(paste(shinyLib,file,sep="/")) # Issue with "makeStudyAreaMap.R" 
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
                                      useSQLite=FALSE
)

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
###TEST FUNCTIONS USED FOR THE APP
###
##################
  
  

