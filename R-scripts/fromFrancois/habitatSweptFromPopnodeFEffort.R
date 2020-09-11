
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


  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ## FISHING EFFORT

 compute_percent_change_in_effort_per_habitat <- function (general, pop=0, 
                                                         fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code"){

  data_this_month <- NULL
  for (names_sce in general$namefolderoutput){

   
     this <- read.table(file=file.path(general$main.path, general$namefolderinput, names_sce,
                              paste("average_cumftime_layer.txt", sep='')), header=FALSE, skip = 1)
     colnames(this) <- c("node","lat",  "long", "cumftime")

     data_this_month <- rbind(data_this_month, cbind.data.frame(this, sce=names_sce))
     }
   
   
   
   
   anf <- function(x) as.character(as.numeric(x))
   
   coord           <- cbind.data.frame(x=anf(data_this_month$long), y=anf(data_this_month$lat), sce=data_this_month$sce)
  
  # convert to UTM
  library(sp)
  library(rgdal)
  SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
  utm_zone <- 29 # CelticSea                     
  coord <- cbind.data.frame(coord,
                 spTransform(SP, CRS(paste("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


   library(maptools)
   anf                  <- function(x) as.numeric(as.character(x))
   sh_coastlines        <- readShapePoly(file.path(general$main.path.param, "MANAGEMENT", "shapes","celtic_land_proj"))

   ## use point-raster overlay.......
   landscapes       <- readShapePoly(fullPolPath,
                                       proj4string=CRS("+proj=longlat +datum=WGS84")    )    # probably need an update of rgdal here....
  
  
   
  #Use the magic 'over' function to see in which polygon it is located
  idx                           <- over(SP, landscapes); #print(idx)
  coord                         <- cbind.data.frame(as.data.frame(coord), landscapes_code=NA)
  coord$landscapes_code         <- as.character(idx[,name_data_attribute])

   sapply(slot(landscapes, "polygons"), slot, "area")
  
   # assign 0 to the NA code
   coord[is.na(coord$landscapes_code), 'landscapes_code'] <- 0
  
   plot(anf(coord[,1]), anf(coord[,2]), col=anf(coord[,6])+1)

 
  
  percent_habitats <- round(table(coord[coord$sce=="scesizespectra",]$landscapes_code)/
                           sum(table(coord[coord$sce=="scesizespectra",]$landscapes_code))*100, 2) # get an idea of the importance of a given landscape in terms of number of nodes over the total number of nodes
  #sum(percent_habitats) # should be 100
 
  percent_habitats[order(percent_habitats, decreasing=TRUE)]
 
 
  data_this_month <- cbind(data_this_month, coord)

   #  do the aggregation  (effort per unit of area)
  feffort_per_habitat <- sweep( tapply(data_this_month$cumftime, list(data_this_month$landscapes_code, data_this_month$sce), sum, na.rm=TRUE),  1,  percent_habitats)
   # round( feffort_per_habitat/sum(feffort_per_habitat[,"scesizespectra"])*100, 1) - 100
  
  
  #marginal            <- apply(feffort_per_habitat, 2, sum)/ apply(feffort_per_habitat, 2, sum)["scesizespectra"] 
  #feffort_per_habitat <- feffort_per_habitat + sweep(feffort_per_habitat, 2, (1 - marginal), FUN="*") # correct for the absolute difference in effort
  
  RES_EFF_relative_to_the_baseline_sce <- sweep(feffort_per_habitat, c(1), feffort_per_habitat[,"scesizespectra"], FUN="/")

  
  # percent change is:
  cat(paste0("percent change is:" ,"\n"))
  print(sweep(feffort_per_habitat, 1, feffort_per_habitat[,"scesizespectra"] , FUN="/"))  

  return(RES_EFF_relative_to_the_baseline_sce)
}
   

 # calls
 RES_EFFORT <- compute_percent_change_in_effort_per_habitat (general, pop=0, 
                                                             fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                             name_data_attribute="hab_code")
 
  
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ##--------------------------------------------##
  ## CATCH-------------------------
  
 
 compute_percent_change_in_catch_per_habitat <- function (pop=0, 
                                                          fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code"){
 
 
  data_this_month <- NULL
  for (names_sce in general$namefolderoutput){

  
     this <- read.table(file=file.path(general$main.path, general$namefolderinput, names_sce,
                              paste("average_cumulcatches_per_pop_pop",pop,"_layer_",names_sce,".txt", sep='')), header=FALSE, skip = 1)
     colnames(this) <- c("node","lat",  "long", "cumcatches")

     data_this_month <- rbind(data_this_month, cbind.data.frame(this, sce=names_sce))
     }
   
 
   
   anf <- function(x) as.character(as.numeric(x))
   
   coord           <- cbind.data.frame(x=anf(data_this_month$long), y=anf(data_this_month$lat), sce=data_this_month$sce)
  
  # convert to UTM
  library(sp)
  library(rgdal)
  SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
  utm_zone <- 29 # CelticSea                     
  coord <- cbind.data.frame(coord,
                 spTransform(SP, CRS(paste("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


  library(maptools)
  anf                  <- function(x) as.numeric(as.character(x))
  sh_coastlines        <- readShapePoly(file.path(general$main.path.param, "MANAGEMENT", "shapes","celtic_land_proj"))



  ## use point-raster overlay.......
  landscapes       <- readShapePoly(file.path(fullPolPath),
                                       proj4string=CRS("+proj=longlat +datum=WGS84")    )    # probably need an update of rgdal here....
  
  
   
  #Use the magic 'over' function to see in which polygon it is located
  idx                           <- over(SP, landscapes); #print(idx)
  coord                         <- cbind.data.frame(as.data.frame(coord), landscapes_code=NA)
  coord$landscapes_code         <- as.character(idx[,name_data_attribute])

  
   # assign 0 to the NA code
   coord[is.na(coord$landscapes_code), 'landscapes_code'] <- 0
  
   plot(anf(coord[,1]), anf(coord[,2]), col=anf(coord[,6]))
 
 
  
  percent_habitats <- round(table(coord[coord$sce=="scesizespectra",]$landscapes_code)/sum(table(coord[coord$sce=="scesizespectra",]$landscapes_code))*100, 2) # get an idea of the importance of a given landscape in terms of number of nodes over the total number of nodes
  sum(percent_habitats) # should be 100
 
  percent_habitats[order(percent_habitats, decreasing=TRUE)]
 
 
  data_this_month <- cbind(data_this_month, coord)
  
   #  do the aggregation
  
   #  do the aggregation  (effort per unit of area)
  cumcatches0_per_habitat <- sweep( tapply(data_this_month$cumcatches, list(data_this_month$landscapes_code, data_this_month$sce), sum, na.rm=TRUE),  1,  percent_habitats)
  RES_CATCH0 <- round( cumcatches0_per_habitat/sum(cumcatches0_per_habitat[,"scesizespectra"])*100, 1) - 100
  
 
 
  #sweep(cumcatches0_per_habitat, 1, cumcatches0_per_habitat[,"scesizespectra"] , FUN="/")  
 
  return(RES_CATCH0)
}


# call---------------
RES_CATCH0  <-  compute_percent_change_in_catch_per_habitat(pop=0, 
                                                          fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code")
RES_CATCH1  <-  compute_percent_change_in_catch_per_habitat(pop=1, 
                                                          fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code")
RES_CATCH2  <-  compute_percent_change_in_catch_per_habitat(pop=2, 
                                                          fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code")
RES_CATCH3  <-  compute_percent_change_in_catch_per_habitat(pop=3, 
                                                          fullPolPath= file.path("C:","Users","fbas","Documents","GitHub", 
                                                                        paste("DISPLACE_input_gis_", general$case_study, sep=''), "HABITATS", 
                                                                        "EunisMapsGreaterNS","ModelledSeabedHabitats_clipped_on_CelticSea"),
                                                         name_data_attribute="hab_code")




   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
  # export the table 
  feffort_and_catches_per_habitat <- rbind.data.frame(cbind.data.frame(var="feffort", RES_EFF), cbind.data.frame(var="catch0", RES_CATCH0),
                                            cbind.data.frame(var="catch1", RES_CATCH1), cbind.data.frame(var="catch2", RES_CATCH2), cbind.data.frame(var="catch3", RES_CATCH3))
  write.table(feffort_and_catches_per_habitat, file=file.path(general$main.path, general$namefolderinput, "change_in_feffort_and_catches_per_habitat.txt"), sep=";", row.names=TRUE, col.names=TRUE)
  # useful to copy/paste into Excel!
  write.table(feffort_and_catches_per_habitat, "clipboard", sep="\t", row.names=TRUE)   # export to excel

   

   
   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
   ###------------------------------------------
   namefile <- "relative_cumul_feffort_per_habitat_and_scenario"
  tiff(filename=file.path(general$main.path, general$namefolderinput, paste(namefile, "_lzw.tiff", sep="" )),
                                   width = 2600, height = 1800, 
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))

 
 
  the_mat <- t(as.matrix( RES_EFF_relative_to_the_baseline_sce [,c(   "scesizespectra",
                                                                      "sceavchokpszpectra",
                                                                      "scesizespectrastopifchok",
                                                                      "sceavchokpszpctrastopifchok",
                                                                      "sceavhtariffspszpctratariffs",
                                                                      "scetrgthtariffspszpctratariffs"
                                                                      )]))
 the_mat <- the_mat[,colnames(the_mat)!=0]
 percent_habitats <- percent_habitats[names(percent_habitats)!=0]
  
par(mfrow=c(1,2))
par(oma=c(1,2,1,1))
par(mar=c(6,1,4,3))


nbhabs <-  length(percent_habitats)
nbsces <-  nrow(the_mat)
widthsbarplot1 <- 6
spacesbarplot1 <- c(0, rep(0.1, nbhabs-1))

    barplot(-percent_habitats, horiz=TRUE, axes=FALSE,beside=TRUE, xlim=c(-30,0), col=grey(0.8),
            width = widthsbarplot1, space = spacesbarplot1,  names.arg=names(percent_habitats), las=2)
    axis(3, at=c(0,-5,-10,-15,-20), labels=c(0,5,10,15,20))                                                                                                              
    mtext(side=3,"% of the habitat", line=3.0, cex=1)
  
widthsbarplot2              <- rep(widthsbarplot1/nbsces, each = nbsces*nbhabs)
spacesbetweengroupsbarplot2 <- mean(widthsbarplot2)
allspacesbarplot2           <- c(rep(0,nbsces), rep(c(spacesbetweengroupsbarplot2, rep(0,nbsces-1)), nbhabs-1))

 # LOG TRANSFORMED
   the_mat_in_log <- log(the_mat)
   the_mat_in_log  <-replace(the_mat_in_log, is.nan(the_mat_in_log), 0.0) 
   the_mat_in_log <- replace(the_mat_in_log, the_mat_in_log< -0.8, -0.8) 
   the_mat_in_log <- replace(the_mat_in_log, the_mat_in_log> 0.8, 0.8) 
   the_mat_in_log <- ifelse(the_mat_in_log< -0.7, the_mat_in_log+0.1, the_mat_in_log)
   bar <- barplot(c(the_mat_in_log), axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE, las=2,  
                  width = widthsbarplot2, space = allspacesbarplot2, col=rainbow(nbsces),  xlim=c(-0.8,0.8))
   mtext(side=1,"Relative fishing effort (log scale)",line=3.0, cex=1)
 
    xat <- pretty((-1) : (1))
    the_xlab <- ifelse(xat< -0.6, xat -0.1, xat)
    dd <- rownames(the_mat)
    axis(1,at=xat, labels=the_xlab)
    library(plotrix)
    box()
    axis.break(1,-0.6,style="slash") 
    
    
    #legend("topright", legend=rownames(the_mat[,]), 
    legend("topright", legend=the_scenarios1 [-c(2:3)], 
     bty="n", fill=rainbow(nbsces), cex=0.6)
  
  dev.off()


  
   ###------------------------------------------
   ###------------------------------------------
   namefile <- "cumul_feffort_per_habitat_and_scenario"
  tiff(filename=file.path(general$main.path, general$namefolderinput, paste(namefile, "_lzw.tiff", sep="" )),
                                   width = 2600, height = 1800, 
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))

  par(mar=c(5,15,5,1))

 
  the_mat <- t(as.matrix( RES_EFF [,c("sceavchokpszpectra",
                                                                      "scesizespectrastopifchok",
                                                                      "sceavchokpszpctrastopifchok",
                                                                      "sceavhtariffspszpctratariffs",
                                                                      "scetrgthtariffspszpctratariffs"
                                                                      )]))
  


   the_mat <- ifelse(the_mat>160000, the_mat-100000, the_mat)
   bar <- barplot(the_mat[,-ncol(the_mat)], axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE,space=c(0.1, 3), las=2)
    mtext(side=1,"cumulated fishing effort (hours)",line=3.0, cex=1)
 
    xat <- pretty(1:300000)
    the_xlab <- ifelse(xat>160000, xat+100000, xat)
     dd <- rownames(the_mat)
    axis(1,at=xat, labels=the_xlab)
    library(plotrix)
    box()
    axis.break(1,160000,style="slash") 
   
    
    par(new=TRUE)
    barplot(-feffort_per_habitat[  colnames(the_mat[,-ncol(the_mat)])   ,"percent_habitat"], axisnames = FALSE, horiz=TRUE, axes=FALSE, xlim=c(-20,0), col=grey(0.8) )
    axis(3, at=c(0,-5,-10,-15,-20), labels=c(0,5,10,15,20))                                                                                                              
    mtext(side=3,"% of the habitat", line=3.0, cex=1)

    par(new=TRUE)
   bar <- barplot(the_mat[,-ncol(the_mat)], axisnames = TRUE, beside=TRUE,  horiz=TRUE,  axes=FALSE,space=c(0.1, 3), las=2)
    mtext(side=1,"cumulated fishing effort (hours)",line=3.0, cex=1)

    legend("bottom", legend=c("Baseline", "Wind", "NAT2000", "Wind+NAT2000", "NAT2000+LowProd", "Wind+NAT2000+LowProd", "NAT2000+20%FuelPrice"), 
     bty="n", fill=grey.colors(7))
  
  dev.off()
    