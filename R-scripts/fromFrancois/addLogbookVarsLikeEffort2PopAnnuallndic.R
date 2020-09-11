
#' Produce polygon plots of time series for aggregated loglike indicators
#'
#' This function produces all accumulated time series over month to compare scenario outcomes
#' All the plots are being stored in a polygons_plots folder that can further be found in the output folder.
#' (compare up to 5 scenarios simultaneously)
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
#'                                       case_study="DanishFleet",
#'                                       igraph=41,
#'                                       a.year="2015",
#'                                       a.country="DEN",
#'                                       nbpops=39,
#'                                       nbszgroup=14,
#'                                       namefolderinput="DanishFleet",
#'                                       the_scenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbsimus=20
#'                                       )
#'
#'
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#' 
#' 
#' 
#' polygonPlotsFromAggLoglikeFiles (general=general,
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3]
#'                                            )
#'
#'   }


                                                                                     
if(TRUE){
 # GENERAL SETTINGS
  general <- list()

  general$case_study <- "AdriaticSea"

 if(.Platform$OS.type == "unix") {}
  general$main.path         <- file.path("~","ibm_vessels","DISPLACE_outputs")                                                                                                   
  general$main.path.igraph  <- file.path("~","ibm_vessels","DISPLACE_input_raw", "igraph")
  general$main.path.param   <- file.path("~","ibm_vessels", paste("DISPLACE_input_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~","ibm_vessels", paste("DISPLACE_input_", general$case_study, sep='')) 
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
  
 if(.Platform$OS.type == "windows") {
  general$main.path         <- file.path("C:","DISPLACE_outputs")                                                                                                   
  general$main.path.igraph  <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
  general$main.path.param   <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_gis_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$case_study, sep='')) 
 }

 if(general$case_study=="AdriaticSea"){
    general$igraph            <- 20
    general$a.year            <- "2015"
    general$a.country         <- c("ITA", "HRV")
    general$nbpops            <- 6  
    general$nbszgroup         <- 14
    general$namefolderinput   <- "AdriaticSea"
    general$use_sqlite        <- FALSE
  
       general$namefolderoutput   <- c(
                                 "scebaseline", 
                                 "sceallyear4nm", 
                                 "sceallyear6nm",
                                 #"sceeffred", 
                                 #"sceally6nmpeffred", 
                                 "scepomopit", 
                                 "scesolesanctuary", 
                                 "scesoleselectivity",
                                 "scecatchlimits",
                                 "sceeffortcut10percent" 
                                 )
     general$namesimu           <- list(
                                 "scebaseline"=   paste("simu", c(1:20), sep=''),
                                 "sceallyear4nm"=   paste("simu", c(1:20), sep=''),
                                 "sceallyear6nm"=   paste("simu", c(1:20), sep=''),
                                 #"sceeffred"=   paste("simu", c(1:20), sep=''),
                                 #"sceally6nmpeffred"=   paste("simu", c(1:20), sep=''),
                                 "scepomopit"=   paste("simu", c(1:20), sep=''),
                                 "scesolesanctuary"=   paste("simu", c(1:20), sep=''),
                                 "scesoleselectivity"=   paste("simu", c(1:20), sep=''),
                                 "scecatchlimits"=   paste("simu", c(1:20), sep=''),
                                 "sceeffortcut10percent"=   paste("simu", c(1:20), sep='')
                                 ) 
     the_scenarios1 <-  c("Baseline", 
                          "All_Year_4nm", 
                          "All_Year_6nm", 
                          #"Effort_Reduction",
                          #"6nm_plus_Effort_Reduction",
                          "Pomopit_Ban",
                          "Sole_Sanctuary",
                          "Sole_Selectivity",
                          "catch_limits",
                          "Effort_cut_10%"
                          )
 


   }


} # end FALSE


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


# load Displace loglike output files
source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_AdriaticSea","RoutinesInR_output","loadAggLoglikeFiles.R"))
loadLoglikeFiles(general=general, use_port_info=FALSE) 




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
mat_allsce_y <- NULL
for (sce in c(general$namefolderoutput)){


# get simulated effort   
a_variable <- "effort"

nby        <- 6
   
obj        <- get(paste("lst_loglike_agg_weight", "_all_", sce, sep=''), env=.GlobalEnv) 

# caution: complete missing records if 0s in given year.month
   complete_all_year_month <- function (x, years_span=2015:(2015+nby-1)){ 
      allcombi              <- expand.grid(month=sprintf("%02d", 1:12), year=years_span)
      allcombi$year.month   <- paste0(allcombi$year,".",allcombi$month)
      allcombi              <- cbind.data.frame(year.month=allcombi$year.month,  matrix(0, ncol=ncol(x)-1))
      colnames(allcombi)    <- colnames(x)
      allmissingcombi       <- allcombi[!allcombi$year.month %in% x$year.month,]
      dd <- rbind.data.frame(x, allmissingcombi)
      rownames(dd) <- dd$year.month
      dd <- dd[as.character(allcombi$year.month),] # get the right order...
   return(dd)
  } 
obj  <- lapply(obj, complete_all_year_month)


# per month
simu_names <-  names(obj)    
mat_sce <- matrix(NA, nrow=length(simu_names),  ncol=nby*12) 
rownames(mat_sce) <- simu_names
colnames(mat_sce) <- paste0(rep(paste0("y",1:nby), each=12), "_", 1:12)
  for (sim in simu_names){
    mat_sce[sim, ] <- as.numeric(obj[[sim]][,a_variable]) [1:dim(mat_sce)[2]]
    }
    

# per year
mat_sce_y <- cbind.data.frame(
   y1 =apply(mat_sce[,c(1:12)], 1, sum),  
   y2 =apply(mat_sce[,c(13:24)], 1, sum),  
   y3 =apply(mat_sce[,c(25:36)], 1, sum),  
   y4 =apply(mat_sce[,c(37:48)], 1, sum),  
   y5 =apply(mat_sce[,c(49:60)], 1, sum),  
   y6 =apply(mat_sce[,c(61:72)], 1, sum)  
   )

# reshape in long
mat_sce_y <- reshape(mat_sce_y, direction="long", varying=list(names(mat_sce_y)), v.names="effort", idvar="simu", timevar="year", times=1:6)
mat_sce_y$simu <- paste0("simu",mat_sce_y$simu)        

# bind sce
mat_allsce_y <- rbind.data.frame(
                                       mat_allsce_y,
                                       cbind.data.frame(mat_sce_y, sce=sce)
                                       )  

} # end sce
 

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# ON THE OTHER SIDE, get the pop variables outcomes
mat_allsce_pop_y <- NULL
for (sce in c(general$namefolderoutput)){

  for(sim in simu_names) {
    print(paste(sim))
    # merge all infos
               annual_indics              <-  read.table (file=file.path(general$main.path, 
                                                 general$namefolderinput, sce, paste('popdyn_annual_indic_',sim,".dat", sep='')))
               colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg",
                                                 "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))

               mat_allsce_pop_y <- rbind (mat_allsce_pop_y, cbind(annual_indics, sce=sce, simu=sim))
     }
   } # end sce

# add a y code
mat_allsce_pop_y$year <- factor(mat_allsce_pop_y$tstep) # init
levels(mat_allsce_pop_y$year) <- 1:6




  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# bind both data for this sce
displace_annual_indic_per_pop_year <- merge( mat_allsce_pop_y, mat_allsce_y)

# export
write.table(displace_annual_indic_per_pop_year, 
                                 file=file.path(general$main.path, general$namefolderinput, 
                                                "displace_annual_indic_per_pop_year.txt"), sep=";", row.names=TRUE, col.names=TRUE)
  

  