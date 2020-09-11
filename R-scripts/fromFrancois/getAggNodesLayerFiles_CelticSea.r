

            
#' Produce an average spatial layer as first step to do a map 
#'
#' This function generates an average layer as a first step to generate maps from popnodes files 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' 
#' \dontrun{
#' general <- setGeneralOverallVariable (main_path_outputs =file.path("C:","DISPLACE_outputs"),
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
#' getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
#'  #=> produce files in output folders....
#'  }








getAggNodeLayerFiles <- function(general, a_type="cumcatches", a_tstep="34321", a_pop=""){


  for (sce in general$namefolderoutput){



 

 
    ## catches-------------------------------
    alllayers <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            obj <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("popnodes_",a_type,"_", sim, ".dat", sep='')))
            if(a_type=="cumulcatches_per_pop"){
               colnames (obj) <- c('pop', 'tstep', 'idx_node', 'long', 'lat', a_type)
               if(!a_pop %in% unique(obj$pop)){
                print(paste("no catch field for ", a_pop))          
                return()
                }                                   
               obj    <- obj[obj$pop== a_pop & obj$tstep==a_tstep,] 
               obj    <- obj[c("idx_node",'lat','long', a_type)]
               a_popname <- paste0("_pop", a_pop)
             } else{
               colnames (obj) <- c('tstep', 'idx_node', 'long', 'lat', a_type)
               obj    <- obj[obj$tstep==a_tstep,] # e.g. if "34321" then cumul at 1st of Dec 4th year
               obj    <- obj[c("idx_node",'lat','long', a_type)]
               a_popname <- ""
            }
            obj    <- cbind(obj, simu=sim)
            print(head(obj))
            alllayers <- rbind(alllayers, obj)
         }, silent=TRUE)

        if(class(er)=="try-error"){
           print(paste("no simu", sim))

        }

       }
  
     # CAUTION:
     # read graph coord and complete DISPLACE output files with all coords for image() to work properly
     coord <- read.table(file=file.path(paste(general$main.path.ibm, sep=""),
                          "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
     # hereafter:
     allgraphpts <-  data.frame(idx_node=0, lat=coord[,2], long=coord[,1], a_type=NA, simu="simu2") 
      colnames(allgraphpts) [colnames(allgraphpts)%in%  "a_type"] <- a_type
     alllayers <- rbind(alllayers, allgraphpts)
  

    alllayersav <- tapply(as.numeric(as.character(alllayers[,a_type])), list(paste(alllayers$idx_node, alllayers$lat, alllayers$long))
                                                              , mean, na.rm=TRUE) # average over simus
    alllayersav <- cbind.data.frame(node=names(alllayersav), avcum=alllayersav)

    write.table(alllayersav, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type,"_layer",a_popname,".txt", sep='')), row.names=FALSE, quote=FALSE)

   
 
    }


return()
}


##-------------------------------------------------------------------##
##-------------------CALLS-------------------------------------------##

                                                                                     
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

 
#----------
for(pop in c(0:( general$nbpops -1 ))){
   getAggNodeLayerFiles (general, a_type="cumulcatches_per_pop", a_tstep="34321", a_pop = pop)
   cat(paste("cumcatches for pop",pop,"..ok/n"))
}

getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
cat("cumcatches...ok/n")
getAggNodeLayerFiles (general, a_type="cumsweptarea", a_tstep="34321")
cat("cumsweptarea...ok/n")
getAggNodeLayerFiles (general, a_type="cumftime", a_tstep="34321")
cat("cumftime...ok/n")
getAggNodeLayerFiles (general, a_type="cumdiscards", a_tstep="34321")
cat("cumdiscards...ok/n")
