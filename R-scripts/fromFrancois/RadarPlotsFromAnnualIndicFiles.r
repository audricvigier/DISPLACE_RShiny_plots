
#' Produce polygon plots of time series for annual indicators
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
#' polygonPlotsFromAnnualIndicFiles (general=general,
#'                                          a_variable="Fbar",  # could be "Fbar" or "totland" or "tac"
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3],
#'                                            explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
#'                                            is_individual_export=TRUE
#'                                            )
#'
#'   }



radarPlotsFromAnnualIndicFiles <- function(general=general,
                                            the_baseline="svana_baseline",
                                            a_width=3500,
                                            a_height=1000,
                                            selected_scenarios=general$namefolderoutput[1:3],
                                            the_scenario_names=general$namefolderoutput[1:3], 
                                            )
{

                                                
  
    # look at annual indics such as the TACs...
    res <- NULL
    for(sce in c(the_baseline,selected_scenarios)) {
            print(paste("sce ", sce))
     

         for(i in the_sims) {
            print(paste("sim ", i))
         # merge all infos
               annual_indics              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popdyn_annual_indic_simu',  i,".dat", sep='')))
               colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))

               annual_indics <- annual_indics [, 1:9]   # FOR NOW...
             res <- rbind (res, cbind(annual_indics, sce=sce, simu=paste("simu", i, sep="_")))
             }
      }
       


   dir.create(file.path(general$main.path, general$namefolderinput,"radar_plots"))
   
   
   
   mintstep <- min(res$tstep)
   maxtstep <- max(res$tstep)
   
   indic <- NULL
   #for (a_stock in unique(res$stk)){
   for (a_stock in 1:10){
  
      res_this_stk <- res[res$stk==a_stock,]
      res_this_stk_init <- res_this_stk[res_this_stk$tstep==mintstep,]
      res_this_stk_final <- res_this_stk[res_this_stk$tstep==maxtstep,]
      res_this_stk_indic <- res_this_stk_final

      res_this_stk_indic$FoverFinit             <-  log(res_this_stk_final$Fbar/res_this_stk_init$Fbar)
      res_this_stk_indic$SSBoverSSBinit         <-  log(res_this_stk_final$SSB_kg/res_this_stk_init$SSB_kg)
      res_this_stk_indic$totlandovertotlandinit <-  log(res_this_stk_final$totland_kg/res_this_stk_init$totland_kg)
      
      res_this_stk_agg <- aggregate(res_this_stk_indic[,c("FoverFinit", "SSBoverSSBinit", "totlandovertotlandinit")], list(res_this_stk_indic$sce), mean)
      colnames(res_this_stk_agg) <- c("Sce", "F/Fi", "SSB/SSBi", "Landings/Landingsi")
      levels(res_this_stk_agg$Sce) <- c("Baseline", the_scenario_names)
      
      res_this_stk_agg$Stock <- a_stock
      indic <- rbind(indic, res_this_stk_agg)
      }
      
      # debug
      indic[,2] <- replace(indic[,2], is.infinite(indic[,2]) | is.na(indic[,2]), 0)
      indic[,3] <- replace(indic[,3], is.infinite(indic[,3]) | is.na(indic[,3]), 0)
      indic[,4] <- replace(indic[,4], is.infinite(indic[,4]) | is.na(indic[,4]), 0)
      
     #library(fmsb)
     #rownames(res_this_stk_agg) <- res_this_stk_agg[,1]
     #res_this_stk_agg <- rbind.data.frame(rep(3,3),rep(-3,3), res_this_stk_agg)
     #radarchart(res_this_stk_agg[,-1], axistype=1,
     #             pcol=c("white","white","white","white"), pfcol=c(rgb(0.2,0.5,0.5,0.3),rgb(0.2,0.2,0.5,0.3),rgb(0.2,0.2,0.2,0.3),rgb(0.5,0.2,0.2,0.3)),
     #             plwd=1, plty=1
     #             )
     
   graphics.off()
   tiff(file=file.path(general$main.path, general$namefolderinput, "radar_plots", paste("radarplot.tiff", sep="")), 
                                  width = a_width, height = a_height,   compression="lzw",
                                   units = "px", pointsize = 12,  res=300)
     library(ggplot2)
     #library(ggradar) #devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
     #dat <- reshape(indic, direction="long", varying=list(colnames(res_this_stk_agg)[-1]), v.names = "val", times=colnames(res_this_stk_agg)[-1])
     
     # reshape in long:
     ind1 <- indic[,c(1,2,5)]
     colnames(ind1)[2] <- "val" 
     ind2 <- indic[,c(1,3,5)]
     colnames(ind2)[2] <- "val" 
     ind3 <- indic[,c(1,4,5)]
     colnames(ind3)[2] <- "val" 
     dat <- rbind.data.frame(cbind(ind1, Indic="F/Fi"),
                             cbind(ind2, Indic="SSB/SSBi"),
                             cbind(ind3, Indic="La/Lai")
                             )
     dat$color <- dat$Sce
     levels(dat$color) <- c(rgb(0.8,0.5,0.5,0.3), rgb(0.2,0.5,0.5,0.3),rgb(0.2,0.2,0.5,0.3),rgb(0.2,0.2,0.2,0.3),rgb(0.5,0.2,0.2,0.3))
     ggplot(data=dat, aes(x=Indic, y=val, group=Sce, colour=Sce, fill=Sce) ) +  
        geom_polygon (size=1, alpha=0.2) + 
        #ggtitle(paste("Stock", a_stock)) + 
        theme_light()+ 
        theme(panel.border = element_blank(),
        #panel.grid  = element_blank(),
        legend.key = element_blank(),
        axis.title.x=element_blank(),  
        axis.title.y=element_blank(),
        strip.background = element_blank()) + 
       # scale_color_manual(values=levels(dat$sce)) +
       # scale_fill_manual(values=levels(dat$sce)) +
        coord_polar() + 
        facet_wrap( ~ Stock, nrow = 3)
  
dev.off()
  
  
return()
}  
  
  
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!CALLS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 if(TRUE){
# GENERAL SETTINGS
  general <- list()

  general$case_study <- "CelticSea"

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

 if(general$case_study=="CelticSea"){
    general$igraph            <- 3
    general$a.year            <- "2015"
    general$a.country         <- c("IRL", "BEL", "FRA", "GBR", "NLD")
    general$nbpops            <- 28  
    general$nbszgroup         <- 14
    general$namefolderinput   <- "CelticSea"
    general$use_sqlite        <- FALSE
  
    general$namefolderoutput   <- c(
                                 "scesizespectra", 
                                 "scebaseline", 
                                 "sceavchok",
                                 "sceavchokpszpectra",
                                 "scesizespectrastopifchok",
                                 "sceavchokpszpctrastopifchok",
                                 "sceavhtariffspszpctratariffs",
                                 "scetrgthtariffspszpctratariffs"                               
                                 #"sceavchokpszpctrafmsyrange",
                                 #"sceavchokpszpctrafmsyrangstopifchok",
                                                                 
                                 )
     general$namesimu           <- list(
                                "scesizespectra"=   paste("simu", c(1:10), sep=''), 
                                 "scebaseline"=   paste("simu", c(1:10), sep=''),  
                                 "sceavchok"=   paste("simu", c(1:10), sep=''),  
                                 "sceavchokpszpectra"=   paste("simu", c(1:10), sep=''),  
                                 "scesizespectrastopifchok"=   paste("simu", c(1:10), sep=''),  
                                 "sceavchokpszpctrastopifchok"=   paste("simu", c(1:10), sep=''),  
                                 "sceavhtariffspszpctratariffs"=   paste("simu", c(1:10), sep=''),  
                                 "scetrgthtariffspszpctratariffs"=   paste("simu", c(1:10), sep='')                               
                                 #"sceavchokpszpctrafmsyrange"=   paste("simu", c(1:10), sep=''),  
                                 #"sceavchokpszpctrafmsyrangstopifchok"=   paste("simu", c(1:10), sep='')
                                 ) 
     the_scenarios1 <-  c("Size spectra Baseline", 
                          " - Predation", 
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          )

  }

} # end FALSE

source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_CelticSea","DISPLACE_R_outputs_ForCelticSea","loadAggLoglikeFiles.R"))
loadLoglikeFiles(general=general, use_port_info=FALSE) 


radarPlotsFromAnnualIndicFiles (general=general,
                                          nby=4,
                                            the_baseline="scebaseline",
                                            a_width=3500,
                                            a_height=2000,
                                             selected_scenarios=c(
                                           #"scesizespectra",
                                           #"scesizespectrastopifchok",
                                           "sceavchokpszpctrastopifchok",
                                           "sceavhtariffspszpctratariffs",
                                           "scetrgthtariffspszpctratariffs"
                                                           ),
                                            the_scenario_names=c(
                                           #"Baseline Size Spectra",
                                           #"+ Stop if choked",
                                           "+ Avoidance + Stop if choked",
                                           "+ Avoid High Tariffs",
                                           "+ Focus on High Tariffs"
                                              ),
                                            explicit_pops=c(0:27),
                                            is_individual_export=TRUE,
                                            add_legend=TRUE
                                            )

  