

#' Produce pie plots centered on ports with proportion affected vessels in classes of stress
#'
#' This function produces pieplots centered on ports of %  of vessel affected per bin for various indicators facing baseline sce
#'
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
#'   library(maptools)
#'   NSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub1_mx20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub1_Mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   NSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                       'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx_20_wgs84'), proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                         'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_20LongTailedD_wgs84'), proj4string=CRS("+proj=longlat #'+ellps=WGS84"))
#'
#'
#'  
#'  
#' loadLoglikeFiles (general, use_port_info=TRUE)
#'
#'
#' comparePieplotStressPerPort (general,
#'                                       nbsim=20,
#'                                       the_baseline="svana_baseline",
#'                                       a_polygon_including_interesting_ports= list(x=c(-2.850909,  16.955191,  18.233003, -12.434505),
#'                                                                                   y=c(46.47831, 52.22793, 63.08831, 62.44946)),
#'                                       selected_scenarios=  c("svana_sub1mx20","svana_sub4mx20"), 
#'                                       gis_shape=list(
#'                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20)),
#'                                           a_width= 3200, a_height =2100, xlims =  c(7, 16), ylims = c(53.5,59)
#'                                           ) 
#'  }



 


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##




comparePieplotStressPerPort <- function(general,
                                       a_variable= "rev_from_av_prices",
                                       nbsim=20,
                                       the_baseline="svana_baseline",
                                       a_polygon_including_interesting_ports= list(x=c(-2.850909,  16.955191,  18.233003, -12.434505),
                                                                                   y=c(46.47831, 52.22793, 63.08831, 62.44946)),
                                       selected_scenarios=  c("svana_sub1mx20","svana_sub4mx20"), 
                                       name_scenarios=  c("svana_sub1mx20","svana_sub4mx20"), 
                                       gis_shape=list(
                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20)),
                                           a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60)
                                                               
                                     ){

 
 
  #-------------------
  compare_per_port <- function(
          sim=1,                       
          a_polygon=list(x=c(13.27,9.08,11.25,15.15, 15.00,55.67),
                                  y=c(56.00,54.82,53.50,53.89,55.11,14.15)),
          sce1   =lst_loglike_agg_weight_vid_port_high_profit_grounds,
          sce2   =lst_loglike_agg_weight_vid_port_high_profit_grounds_biolsce_Linfs_M_mig_weight,
          a_variable= "rev_from_av_prices"
          )  {

   an <- function(x)as.numeric(as.character(x))
   library(sp)
   obj1 <- sce1[[sim]][ point.in.polygon( an(sce1[[sim]]$ld_port_x), an(sce1[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1 ,]
   obj2 <- sce2[[sim]][ point.in.polygon( an(sce2[[sim]]$ld_port_x), an(sce2[[sim]]$ld_port_y), a_polygon$x, a_polygon$y )==1,]




   dd <-  tapply(apply(obj1[,a_variable], 1, sum, na.rm=TRUE),
        obj1$land_port, sum, na.rm=TRUE)
   dd2 <-  tapply(apply(obj2[,a_variable], 1, sum, na.rm=TRUE),
        obj2$land_port, sum, na.rm=TRUE)
   a_diff       <-  round(an(dd2[names(dd2)])/  an(dd[names(dd)]) *100  )    -100
  names(a_diff) <- names(dd)

  dd3 <-   tapply(obj1[,a_variable],
        obj1$land_port, sum, na.rm=TRUE)
  

  nm_ports <- names(a_diff[!is.na(a_diff)])
  print(dd[nm_ports])
  print(dd2[nm_ports])

  return(cbind(a_diff[!is.na(a_diff) & !is.infinite(a_diff)], dd3[!is.na(a_diff) & !is.infinite(a_diff)]))
  }



  ##-----------
  ##-----------
  ##---utils---
  compare_per_port_vessel <- function(
          sim=1,
          a_polygon=list(x=c(13.27,9.08,11.25,15.15, 15.00,55.67),
                                  y=c(56.00,54.82,53.50,53.89,55.11,14.15)),
          sce1   =lst_loglike_agg_weight_vid_port_high_profit_grounds,
          sce2   =lst_loglike_agg_weight_vid_port_high_profit_grounds_biolsce_Linfs_M_mig_weight,
          a_variable  ="rev_from_av_prices"
          )  {

   an <- function(x)as.numeric(as.character(x))
   library(sp)                  
   obj1 <- sce1[[paste0("simu",sim)]][ point.in.polygon( an(sce1[[paste0("simu",sim)]]$ld_port_x), an(sce1[[paste0("simu",sim)]]$ld_port_y), a_polygon$x, a_polygon$y )==1 ,]
   obj2 <- sce2[[paste0("simu",sim)]][ point.in.polygon( an(sce2[[paste0("simu",sim)]]$ld_port_x), an(sce2[[paste0("simu",sim)]]$ld_port_y), a_polygon$x, a_polygon$y )==1,]




    dd <-  aggregate(apply(obj1[,a_variable], 1, sum, na.rm=TRUE),
        list(obj1$land_port, obj1$VE_REF), sum, na.rm=TRUE)
    colnames(dd)[3] <- "ref" 
    dd2 <-  aggregate(apply(obj2[,a_variable], 1, sum, na.rm=TRUE),
         list(obj2$land_port, obj2$VE_REF), sum, na.rm=TRUE)
    colnames(dd2)[3] <- "sce" 

    dd3 <- merge(dd, dd2)
  
    dd3$a_diff       <-  round(an(dd3[,"sce"])/  an(dd3[,"ref"]) *100  )    -100
   
    dd4 <-   tapply(obj1[, a_variable[1]],
         list(obj1$land_port), sum, na.rm=TRUE)
  
    dd3$totrevenue <- dd4 [as.character(dd3[,1])]
  
    dd3[is.infinite(dd3$a_diff), "a_diff"]  <- - 100

 
    return(dd3[!is.na(dd3$a_diff)  & !is.infinite(dd3$a_diff),])
    }


 
  #-------------------------------- 
  these_ports <- NULL
  what <- "weight"
  loglike_baseline <- get( paste("lst_loglike_agg_",what,"_vid_port_", the_baseline , sep=''), env=.GlobalEnv)
  
  for(sce in general$namefolderoutput[!general$namefolderoutput %in% c(the_baseline)]) {


  for(sim in 1:nbsim) {

   
      

       loglike <- get( paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''), env=.GlobalEnv)
       
       if(paste0("simu",sim) %in% names(loglike) && paste0("simu",sim) %in% names(loglike_baseline))  # bc sometime missing some simu...
        { 
        res     <- compare_per_port_vessel(sim=sim,
                      a_polygon=a_polygon_including_interesting_ports,
                      sce1=loglike_baseline,
                      sce2=loglike,
                      a_variable  =c(a_variable, a_variable)) # tips: repeat twice to avoid triggering apply() on a single dim

      these_ports <- rbind.data.frame(these_ports,
                                  cbind(sce, sim, res)
                                  )
       }                           
   }
  } # end sce



  
  these_ports <- these_ports[these_ports$sce %in% selected_scenarios,]
  these_ports$sce <- factor(these_ports$sce)

  colnames(these_ports) <- c( "sce", "sim", "port", "vid", "ref", "sce", "percent_change", "totrevenue")



  ## get back the port name
  port_names <- read.table(file=file.path(general$main.path.ibm,
                                paste("harboursspe_",general$namefolderinput,sep=''),
                                  paste("names_harbours.dat", sep='')), sep=";", header=TRUE)
  port_names           <- cbind(port_names, port=rownames(port_names))
  coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')

  nodeports <- as.numeric(sapply(port_names$node.name, function(x) strsplit(as.character(x), " ")[[1]])[1,])
  these_ports$land_port    <- port_names[   as.numeric(as.character(these_ports$port))  , 'node.name']
  
  an <- function(x)as.numeric(as.character(x))
  these_ports$x    <- coord[  nodeports[an(these_ports$port)+1 ], "x"]  
  these_ports$y    <- coord[  nodeports[an(these_ports$port)+1 ], "y"]  
 

  sauv <- these_ports

                 
  library(maptools)                              
 
 # plot
 namefile       <- paste(paste("pie_chart_per_harbour_", sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 tiff(filename=file.path(output.folder, paste(namefile, a_variable, ".tiff", sep="" )),
                                   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 if(length(selected_scenarios)==2){  par(mar=c(2.5,3,1,1)); par(mfrow=c(1,2)) }
 if(length(selected_scenarios)==3){  par(mar=c(2.5,3,1,1)); par(mfrow=c(1,3)) }
 if(length(selected_scenarios)==4){  par(mar=c(2,1,2,1)); par(mfrow=c(2,2)) }
 if(length(selected_scenarios)==5){  par(mar=c(2,2,2,1)); par(mfrow=c(3,2)) }
 if(length(selected_scenarios)==6){  par(mar=c(2,2,2,1)); par(mfrow=c(3,2)) }
 if(length(selected_scenarios)==7){  par(mar=c(2,2,2,1)); par(mfrow=c(4,2)) }
 if(length(selected_scenarios)==8){  par(mar=c(2,2,2,1)); par(mfrow=c(4,2)) }
 par(oma=c(1,2,1,1))
 
 
 # PLOT!
 count <- 0
 res <- NULL
 for(sce in selected_scenarios) {
  count <- count+1
  
  name_sce <- name_scenarios[count]
  cat(paste(name_sce, "\n"))
 
  library(mapdata)
  map("worldHires",   xlim=xlims, ylim=ylims, add=FALSE, col=grey(0.8), border=grey(0.8), fill=TRUE, asp= 2.587761)
  mtext(side=1, text="Longitude", line=1.9)
  mtext(side=2, text="Latitude", line=2.7)
 
  # add the pie chart
  library(mapplots)
  library(scales)
  box()
 
  if (!is.null(gis_shape)) 
    if(length(gis_shape[[sce]])>0) 
      for (i in 1:length(gis_shape[[sce]])) 
       plot(gis_shape[[sce]][[i]], add=TRUE, fill=FALSE, border=grey(0.8))
 
 maxrev <- max(these_ports$totrevenue)  
 
 dd <-  these_ports[these_ports$sce==sce,]  

 
 make.xyz<- function (x, y, z, group, FUN = sum, ...) 
{
    Z <- tapply(z, list(paste(x, y, sep = ", "), group), FUN, 
        ...)
    Z <- ifelse(is.na(Z), 0, Z)
    XY <- rownames(Z)
    tempfun <- function(XY, i) {
        as.numeric(unlist(lapply(strsplit(XY, ", "), function(x) x[i])))
    }
    X <- tempfun(XY, 1)
    Y <- tempfun(XY, 2)
    return(list(x = X, y = Y, z = Z))
}

 dd$stressclass <- cut (as.numeric(as.character(dd$percent_change)), breaks=c(-1000,-25,0, +25,+1000))
 dd$totrevenue2 <- dd$totrevenue / table(dd$port) [dd$port] # caution, important correction to account for the nb of time a port has been used => average revenue per trip over 5 years over XX replicates
 
 print(head(dd))
 
 res <- rbind(res,dd)
 xyz <- make.xyz(as.numeric(as.character(dd$x)), as.numeric(as.character(dd$y)), as.numeric(as.character(dd$totrevenue2)) , dd$stressclass)
 library(RColorBrewer)
 draw.pie (z=xyz$z, x=xyz$x, y=xyz$y, radius=0.5, col=alpha(brewer.pal(4, "RdYlGn"), 0.8), labels="")
 legend.bubble(xlims[1]+0.5,ylims[1]+0.5, z=max(dd$totrevenue),round=0, maxradius=0.5, bty="n",txt.cex=0.6)
  axis(1)
  axis(2, las=2)
  box()
  mtext(side=3, adj=0, text=name_sce, line=1)

 
  #head(dd[dd$land_port=="35084 Hundested",])

 
 # add the shapefile of closures
 library(maptools)
  
  
 #plot(excludeInBuffer6nm, add=TRUE, border="red")

  ports <- dd[!duplicated(dd$port), c('x', 'y', 'port')]
 # text (as.numeric(as.character(ports$x))+0.5, as.numeric(as.character(ports$y))+0.05,
 #       labels=ports$port, cex=0.5, col=1)
  legend("bottomright", legend=c('<-25%', '-25,0%', '0,+25%', '>25%'), fill=alpha(brewer.pal(4, "RdYlGn"), 0.8), bty="n")
                                                                                             
  cc <- these_ports[!duplicated(data.frame(these_ports$port, these_ports$sce)),]
 
 } # end sce
 
 

dev.off()



return(res)
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




   library(maptools)
   
   sh_coastlines               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_AdriaticSea','MANAGEMENT','francois_EU'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))

  
   
source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_CelticSea","DISPLACE_R_outputs_ForCelticSea","loadAggLoglikeFiles.R"))
loadLoglikeFiles(general=general, use_port_info=TRUE) 


res <- comparePieplotStressPerPort (general,
                                       a_variable= "rev_from_av_prices",
                                       nbsim= length(general$namesimu[[1]]),
                                       the_baseline="scesizespectra",
                                       a_polygon_including_interesting_ports= list(x=c(-5.326565, -12.063789, -12.208364,  -3.851894),
                                                                                   y=c(54.97820, 54.95070, 48.03010, 48.06676)),
                                        selected_scenarios=c(
                                           "sceavchokpszpectra",
                                           "sceavchokpszpctrastopifchok",
                                           "sceavhtariffspszpctratariffs",
                                           "scetrgthtariffspszpctratariffs"
                                                           ),
                                       name_scenarios=c(
                                           "+ Avoidance",
                                           "+ Avoidance + Stop if choked",
                                           "+ Avoid High Tariffs",
                                           "+ Focus on High Tariffs"
                                        ), 
                                       gis_shape=list(
                                           scesizespectra=   list(sh_coastlines),
                                           scesizespectrastopifchok=   list(sh_coastlines),
                                           sceavchokpszpctrastopifchok=   list(sh_coastlines), # ices_areas),
                                           sceavhtariffspszpctratariffs=   list(sh_coastlines),
                                           scetrgthtariffspszpctratariffs=   list(sh_coastlines)
                                          ),
                                           a_width= 2400, a_height =2400, xlims = c(-12, -2.0), ylims=c(51,56)
                                           ) 

head(res[res$sce=="scesizespectra",])
# caution: for sceeffred we expect a large loss when comparing the same same set of vessels 
# (while the exited vessels should be better removed from the pie...)
# we want better to know how large the remaining vessels are gaining from the exit of others....



#choose a variable among:
#  names(loglike_baseline[[1]])
#  "pop.0"                      
# "pop.1"                       "pop.2"                       "pop.3"                       "pop.4"                       "pop.5"                       "effort"                     
# "cumsteaming"                 "nbtrip"                      "bwtrip"                      "fuelcons"                    "revenue"                     "rev_from_av_prices"         
# "sweptr"                      "revpersweptarea"             "fuelcost"                    "gav"                         "gradva"                      "totland"                    
#"totland_explicit"            "totland_implicit"            "pop.0"                       "pop.1"                       "pop.2"                       "pop.3"                      
# "pop.4"                       "pop.5"                       "rev_explicit_from_av_prices" "av_effort"                   "av_bwtrip"                   "traveled_dist"              
# "vpuf"                        "vapuf"                       "disc_rate_0"                 "disc_rate_1"                 "disc_rate_2"                 "disc_rate_3"                
# "disc_rate_4"                 "disc_rate_5"                 "av_vapuf_month"


## BUT DON`T DO IT ON GRADAV BECAUSE POSSIBLE NEGATIVE VALUES THEN PERCENTAGE GAIN/LOSS OF A NEGATIVE VALUE IS TRICKY...(the pie code will fails anyway)
res <- comparePieplotStressPerPort (general,
                                       a_variable= "fuelcons",
                                       nbsim= length(general$namesimu[[1]]),
                                       the_baseline="scesizespectra",
                                         a_polygon_including_interesting_ports= list(x=c(-5.326565, -12.063789, -12.208364,  -3.851894),
                                                                                   y=c(54.97820, 54.95070, 48.03010, 48.06676)),
                                           selected_scenarios=c(
                                           "sceavchokpszpectra",
                                           "sceavchokpszpctrastopifchok",
                                           "sceavhtariffspszpctratariffs",
                                           "scetrgthtariffspszpctratariffs"
                                                           ),
                                       name_scenarios=c(
                                           "+ Avoidance",
                                           "+ Avoidance + Stop if choked",
                                           "+ Avoid High Tariffs",
                                           "+ Focus on High Tariffs"
                                        ), 
                                       gis_shape=list(
                                           scesizespectra=   list(sh_coastlines),
                                           scesizespectrastopifchok=   list(sh_coastlines),
                                           sceavchokpszpctrastopifchok=   list(sh_coastlines), # ices_areas),
                                           sceavhtariffspszpctratariffs=   list(sh_coastlines),
                                           scetrgthtariffspszpctratariffs=   list(sh_coastlines)
                                          ),
                                           a_width= 2400, a_height =2400, xlims = c(-12, -2.0), ylims=c(51,56)
                                           ) 


res <- comparePieplotStressPerPort (general,
                                       a_variable= "pop.2", 
                                       nbsim= length(general$namesimu[[1]]),
                                       the_baseline="scesizespectra",
                                          a_polygon_including_interesting_ports= list(x=c(-5.326565, -12.063789, -12.208364,  -3.851894),
                                                                                   y=c(54.97820, 54.95070, 48.03010, 48.06676)),
                                      selected_scenarios=c(
                                           "scesizespectrastopifchok",
                                           "sceavchokpszpctrastopifchok",
                                           "sceavhtariffspszpctratariffs",
                                           "scetrgthtariffspszpctratariffs"
                                                           ),
                                       name_scenarios=c(
                                           "+ Stop if choked",
                                           "+ Avoidance + Stop if choked",
                                           "+ Avoid High Tariffs",
                                           "+ Focus on High Tariffs"
                                        ), 
                                       gis_shape=list(
                                           scesizespectra=   list(sh_coastlines),
                                           scesizespectrastopifchok=   list(sh_coastlines),
                                           sceavchokpszpctrastopifchok=   list(sh_coastlines), # ices_areas),
                                           sceavhtariffspszpctratariffs=   list(sh_coastlines),
                                           scetrgthtariffspszpctratariffs=   list(sh_coastlines)
                                          ),
                                           a_width= 2400, a_height =2400, xlims = c(-12, -2.0), ylims=c(51,56)
                                           ) 






