            
                                                                                     
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
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 getAggLoglikeFiles <- function(general=general, 
                                              what="weight", 
                                              explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
                                              implicit_pops=c(0,1,2,4,5,6,8,9,11:30),
                                              selected_vessels_set1=selected_vessels_set1,
                                              selected_vessels_set2=selected_vessels_set2,
                                              selected_vessels_set3=selected_vessels_set3
                                              ){
 
 
 
  c.listquote <- function (...){
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }
   
  ICESarea2 <- function (tacsat, string = TRUE){
    library(sp)
    ICES.area <- rep(NA, dim(tacsat)[1])
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653,
            57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(string, 'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0] <-ifelse(string,'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
        pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,
            58.4)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
            9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) >
        0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,
            53.5)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
            55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
            56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
            63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
            63.5, 63.5, 67, 67)) > 0] <-ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
        pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) >
        0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
            56.6)) > 0] <- ifelse(string,'2224', '2')
    return(ICES.area)
   }

 ICESrectangle <- function (dF) {
    rectChar1n2 <- as.integer(2 * (dF[, "SI_LATI"] - 35.5))
    rectChar3 <- ifelse(dF[, "SI_LONG"] <= -40, "A", ifelse(dF[, 
        "SI_LONG"] <= -30, "B", ifelse(dF[, "SI_LONG"] <= -20, 
        "C", ifelse(dF[, "SI_LONG"] <= -10, "D", ifelse(dF[, 
            "SI_LONG"] <= 0, "E", ifelse(dF[, "SI_LONG"] <= 10, 
            "F", ifelse(dF[, "SI_LONG"] <= 20, "G", ifelse(dF[, 
                "SI_LONG"] <= 30, "H", "I"))))))))
    rectChar4 <- as.integer(dF[, "SI_LONG"]%%10)
    rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
    return(rectID)
   }
 
                        
 ICESrectangle2LonLat <- function (statsq, midpoint = F) {
    part1 <- substr(statsq, 1, 2)
    part2 <- substr(statsq, 3, 4)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
        as.character(labels))
    latvalues <- seq(35.5, 80.5, 0.5) + 0.25
    lonlabels <- paste(rep(LETTERS[2:8], rep(10, 7)), rep(0:9,
        7), sep = "")
    lonvalues <- (-40:29) + 0.5
    indx <- match(part1, latlabels)
    lat <- latvalues[indx]
    indx <- match(part2, lonlabels)
    lon <- lonvalues[indx]
    if (any(is.na(lat)) | any(is.na(lon)))
        warning("Some stat squares have not been recognised.")
    if (midpoint == F) {
        lat <- lat - 0.25
        lon <- lon - 0.5
    }
    return(data.frame(SI_LATI = lat, SI_LONG = lon))
   }





   loadGraph <- function(){
    # load the graph
     #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
     coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('x', 'y', 'harb')
     plot(coord[,1], coord[,2])

     graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
     graph <- as.matrix(as.vector(graph))
     graph <- matrix(graph, ncol=3)
     segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R

     return(graph)
     }
     

   loadCoord <- function(){
    # load the graph
    #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
    coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
    coord <- as.matrix(as.vector(coord))
    coord <- matrix(coord, ncol=3)
    colnames(coord) <- c('x', 'y', 'harb')
    plot(coord[,1], coord[,2])

    graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
    graph <- as.matrix(as.vector(graph))
    graph <- matrix(graph, ncol=3)
    segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R

    return(coord)
    }

 
 
 
 
 
 
  #--------------------------------
  explicit_pops_params <- explicit_pops   # save
  implicit_pops_params <- implicit_pops   # save
  
  ## tstep
   t.seq <- seq(as.POSIXct(paste(general$a.year,"-01-01 00:00:00",sep='')),
        as.POSIXct(paste(as.numeric(as.character(general$a.year))+4,"-12-31 00:00:00",sep='')), by="hours")  # 5 years including the start y
 
   for (sce in general$namefolderoutput){
     lst_loglike_agg_all <- list()
     lst_loglike_agg_den <- list()
     lst_loglike_agg_selected_set1 <- list()
     lst_loglike_agg_selected_set2 <- list()
     lst_loglike_agg_selected_set3 <- list()
     lst_loglike_agg_deu <- list()
     lst_loglike_agg_swe <- list()
     lst_loglike_agg_ita <- list()
     lst_loglike_agg_hrv <- list()
     lst_loglike_agg_irl <- list()
     lst_loglike_agg_fra <- list()
     lst_loglike_agg_gbr <- list()
     lst_loglike_agg_nld <- list()
     lst_loglike_agg_bel <- list()
     lst_loglike_agg_vid <- list()
     lst_loglike_agg_vid_port <- list()
     lst_loglike_agg_met <- list()
   
  
     explicit_pops <- explicit_pops_params
     implicit_pops <- implicit_pops_params
  
  
   
     for (sim in general$namesimu[[sce]]){


     if(!general$use_sqlite){
       ## robust read for the 'loglike' output
       er <- try(   {
        filename <- file.path(general$main.path, general$namefolderinput,  sce, 
                                         paste("loglike_", sim, ".dat", sep=''))
        
        #loglike <- read.table(file=filename)
        # because not same number of elements per line, replaced by:
        
        cnts<- count.fields(filename) 
        lf<-readLines(filename)
        lf<-cbind(lf,cnts)
        print(nrow(lf))
        most_freq_nb_of_field <- names(table(cnts)) [table(cnts)==max(table(cnts))]
        #!#!#!#!#!#!#!#!#!
        lf <- lf[lf[,2]==most_freq_nb_of_field,][,-2] # filter out if != most_freq_nb_of_field
        #!#!#!#!#!#!#!#!#!
        print(length(lf))
        # Write data out and read it back in (temporarily)
        filename2 <-  file.path(general$main.path, general$namefolderinput,  sce, 
                                         paste("loglike_", sim, "_corr", ".dat", sep=''))
        write(lf,file=paste(filename2))
        loglike <-read.table(filename2)
        gc(reset=T)

  
   }, silent=TRUE)
  
   } else{
    # Use sqlite
    library(DBI)
    library(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), file.path(general$main.path, general$namefolderinput,  sce, 
                                         paste(general$namefolderinput, "_", sim, "_out", ".db", sep='')))
    head(dbReadTable(con, "VesselLogLike"))
    head(dbReadTable(con, "VesselDef"))
    head(dbReadTable(con, "VesselLogLikeCatches"))

    res <- dbSendQuery(con, "SELECT TStep,SUM(Catches) FROM VesselLogLike JOIN VesselDef ON Id = VesselId JOIN VesselLogLikeCatches ON RowId = LoglikeId WHERE Nationality = 'DNK' GROUP BY TStep")
    dbFetch(res, n= -1)
    dbClearResult(res)
   
    res <- dbSendQuery(con, "SELECT TStep,AVG(vpuf) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
    dd <- dbFetch(res, n= -1)
    dbClearResult(res)
    
    res <- dbSendQuery(con, "SELECT TStep,SUM(gav) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
    dd <- dbFetch(res, n= -1)
    dbClearResult(res)
   sum(dd[,2])
    
    plot(dd)
    dd$runningaverage <- cumsum(dd[,2])/(1:length(dd[,2]))
    plot(dd[,1], dd$runningaverage)
   
    
   # todo....
   }


   if(class(er)!="try-error"){                                       

                           
   colnames (loglike) <- c('tstep_dep', 'tstep_arr', 'reason_back','cumsteaming', 'idx_node', 
                           'idx_vessel', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist', 
                             paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", 
                             "fuelcost", "vpuf", "gav", "gradva", 
                              "sweptr", "revpersweptarea",
                               paste('disc.',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit", 
                                "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
                              
                              

  
  
  
  # discard rate (caution: bi-modal where a lots of 1 i.e. all discarded!) 
  disc_rates <- matrix(0, ncol=length(explicit_pops), nrow=nrow(loglike))
  colnames(disc_rates)  <- paste("disc_rate_", explicit_pops, sep="")
  loglike <- cbind.data.frame(loglike,  disc_rates)
  for (a_st in explicit_pops){
    loglike[, paste("disc_rate_", a_st, sep="")]   <-  loglike   [, paste("disc.", a_st, sep="")]  / (loglike   [, paste("disc.", a_st, sep="")]  + loglike [, paste("pop.", a_st, sep="")]) 
  }

  
  # add an energy efficiency calculation
  loglike$vpuf <- as.numeric(as.character(loglike$revenue))/ 
                          as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
  loglike$vapuf <- as.numeric(as.character(loglike$rev_from_av_prices))/ 
                          as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
  
 
  
 ##...and get back the time info from this...
 loglike$time       <-   t.seq[as.numeric(as.character(loglike$tstep_arr))]
 loglike$month      <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%m")
 loglike$year       <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%Y")
 loglike$year.month <-   paste(loglike$year, '.', loglike$month, sep='')
 #!#!#!#!#!#
 ##=> caution: we are taking the time at the ARRIVAL date to assign a month...
 #(TO DO: better to assign repsective part to each month)
 # so possibly the physical limit (around 600 hours) can be overshoot 
 # if a given trip of the last month end up to the current month(it is just an artefact then...)
 #!#!#!#!#!#
 
 print("reading loglike----OK")

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

 #coord                <- cbind(coord, idx=0:(nrow(coord)-1))
 #head(coord[coord[,"harb"]==303,])
 loglike$land_port    <- port_names[   coord[loglike$idx_node +1 , "harb"]   , 'port']
  # => caution to this +1 because offset by 1 between loglike and coord row index!
 loglike$ld_port_x    <- coord[loglike$idx_node +1, "x"]  
 loglike$ld_port_y    <- coord[loglike$idx_node +1, "y"]  
 
 
 

 print("ports----OK")

 ## sort per vessel and compute effort
 library(doBy)
 loglike        <- orderBy(~VE_REF+tstep_dep, data=loglike)
 loglike$effort <- loglike$tstep_arr - loglike$tstep_dep # trip duration in hours because hourly time step...
 
 if(what=="cpue"){
    loglike$feffort <- (loglike$tstep_arr - loglike$tstep_dep) -loglike$cumsteaming
    loglike[,grep("pop.", colnames(loglike))] <-  loglike[,grep("pop.", colnames(loglike))] / loglike$feffort
    loglike <- loglike[,-grep("feffort", colnames(loglike))] # remove the col now it has been used.
 }
 
 ## add the total landings
 nm <- colnames(loglike)
 idx.col.c <- grep('pop.', nm)
 loglike$totland <- apply(loglike[,idx.col.c], 1, sum, na.rm=TRUE)

 ## add the total landings explicit
 nm <- colnames(loglike)
 #idx.col.c <- grep('pop.', nm)
 loglike$totland_explicit <- apply(loglike[, paste("pop.", explicit_pops, sep='')], 1, sum, na.rm=TRUE)
 if(!is.null(implicit_pops)) {
   loglike$totland_implicit <- apply(loglike[, paste("pop.", implicit_pops, sep='')], 1, sum, na.rm=TRUE)
   } else{
   loglike$totland_implicit <- 0
   }
 
 
 ##=> note that we have now the duration of each trip,
 ## and the duration between trips (i.e. stay on quayside) is given by time between two successive records
 ## and nb of trips per month can also be deduced here......
 loglike$nbtrip <- 1
 loglike$bwtrip <- 0
 loglike$bwtrip[1:(nrow(loglike)-1)] <- 
          loglike$tstep_dep[2:nrow(loglike)] - loglike$tstep_arr[1:(nrow(loglike)-1)] # trip duration in hours because hourly time step...
 loglike[diff(as.numeric(loglike$VE_REF))==1, "bwtrip"] <- 0 # correct when change of vid
 
 
 ##-------------
 ## agg utils ##
 ##-------------
 aggregateLoglike <- function(loglike, agg_by=c("year.month"), what="weight"){
    library(data.table)
    nm <- names(loglike)
    idx.col.c   <- grep('pop.', nm)
    idx.col.d   <- grep('disc.', nm, fixed=TRUE)
    idx.col.e   <- grep('effort', nm)
    idx.col.s   <- grep('cumsteaming', nm)
    idx.col.t   <- grep('nbtrip', nm)
    idx.col.b   <- grep('bwtrip', nm)
    idx.col.f   <- grep('fuelcons', nm)
    idx.col.r   <- grep('revenue', nm, fixed = TRUE)
    idx.col.sa   <- grep('sweptr', nm, fixed = TRUE)
    idx.col.rsa <- grep('revpersweptarea', nm, fixed = TRUE)
    idx.col.ra  <- grep('rev_from_av_prices', nm)
    idx.col.fc  <- grep('fuelcost', nm)
    idx.col.g   <- grep('gav', nm, fixed = TRUE)
    idx.col.g2  <- grep('gradva', nm)
    idx.col.l   <-  which(nm== 'totland')
    idx.col.l2  <- which(nm== 'totland_explicit')
    idx.col.l3  <- which(nm== 'totland_implicit')
    #idx.col.pops  <- which(nm %in% paste('pop.', explicit_pops, sep=''))
    idx.col.g3  <- which(nm== 'rev_explicit_from_av_prices') 
    idx.col <- c(idx.col.c,  idx.col.d, idx.col.e, idx.col.s, idx.col.t, idx.col.b, idx.col.f, idx.col.r, 
              idx.col.ra,  idx.col.sa, idx.col.rsa, idx.col.fc, idx.col.g,  idx.col.g2, idx.col.l, idx.col.l2, idx.col.l3,
              #idx.col.pops,
               idx.col.g3)
    colnames(loglike)[idx.col] # check             
    DT  <- data.table(loglike) # library data.table for fast grouping replacing aggregate()
    # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
    a_mean <- function(x, na.rm) mean(x[x!=0], na.rm=na.rm) # modify the mean() so that 0 are first removed....
    if(what=="weight") eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    if(what=="cpue") eq1  <- c.listquote( paste ("a_mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
    a_by <- c.listquote(  agg_by ) 
    loglike.agg <- DT[,eval(eq1), by= eval(a_by)]
    loglike.agg <- data.frame( loglike.agg)
    colnames(loglike.agg) <- c(agg_by, colnames(loglike)[idx.col] )
    library(doBy)
    loglike.agg <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg) # order to ensure same order when collating....
    # AGGREGATE PER SPECIES -----> MEAN
    idx.col.e  <- grep('effort', nm)
    idx.col.b  <- grep('bwtrip', nm)
    idx.col.t  <- grep('traveled_dist', nm)
    idx.col.v  <- grep('vpuf', nm)
    idx.col.v2  <- grep('vapuf', nm)
    idx.col.disc_rate  <- which(nm %in% paste('disc_rate_', explicit_pops, sep=''))
    idx.col <- c(idx.col.e, idx.col.b, idx.col.t,  idx.col.v, idx.col.v2, idx.col.disc_rate)
    eq1  <- c.listquote( paste ("mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
    a_by <- c.listquote(  agg_by ) 
    loglike.agg2 <- DT[,eval(eq1),by=eval(a_by)]
    loglike.agg2 <- data.frame( loglike.agg2)
    some_col_names           <- colnames(loglike)[idx.col]
    some_col_names_redundant <-  some_col_names [some_col_names %in%  colnames(loglike.agg[,-c(1:length(agg_by))])] 
    some_col_names [some_col_names %in% some_col_names_redundant] <- paste0("av_",some_col_names_redundant)  # a fix
    colnames(loglike.agg2)   <- c(agg_by, some_col_names )
    library(doBy)
    loglike.agg2 <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg2) # order to ensure same order when collating....
    # collate
    loglike.agg <- cbind(loglike.agg, loglike.agg2[,-c(1:length(agg_by))])

    # av_vpuf per trip can be biased by large outliers, so instead look at:
    loglike.agg$av_vapuf_month <- loglike.agg$rev_from_av_prices /  loglike.agg$fuelcons

 return(loglike.agg)
 }


 ##-------------
  ## aggregate by year.month ALL COUNTRIES
 loglike.agg <-  aggregateLoglike(loglike, agg_by=c("year.month"))
 
  # split per country
 loglike.den     <- loglike[grep('DNK',loglike$VE_REF),]  # caution 
 loglike.agg.den <-  aggregateLoglike(loglike.den, agg_by=c("year.month"))
 
  # split per country
 loglike.ita     <- loglike[grep('ITA',loglike$VE_REF),]  # caution 
 loglike.agg.ita <-  aggregateLoglike(loglike.ita, agg_by=c("year.month"))
 
  # split per country
 loglike.hrv     <- loglike[grep('HRV',loglike$VE_REF),]  # caution 
 loglike.agg.hrv <-  aggregateLoglike(loglike.hrv, agg_by=c("year.month"))

  # split per country
 loglike.irl     <- loglike[grep('IRL',loglike$VE_REF),]  # caution 
 loglike.agg.irl <-  aggregateLoglike(loglike.irl, agg_by=c("year.month"))
 
  # split per country
 loglike.fra     <- loglike[grep('FRA',loglike$VE_REF),]  # caution 
 loglike.agg.fra <-  aggregateLoglike(loglike.fra, agg_by=c("year.month"))
 
  # split per country
 loglike.gbr     <- loglike[grep('GBR',loglike$VE_REF),]  # caution 
 loglike.agg.gbr <-  aggregateLoglike(loglike.gbr, agg_by=c("year.month"))
 
  # split per country
 loglike.nld     <- loglike[grep('NLD',loglike$VE_REF),]  # caution 
 loglike.agg.nld <-  aggregateLoglike(loglike.nld, agg_by=c("year.month"))
 
 # split per country
 loglike.bel     <- loglike[grep('BEL',loglike$VE_REF),]  # caution 
 loglike.agg.bel <-  aggregateLoglike(loglike.bel, agg_by=c("year.month"))
 
 # split per selected set of vessels
 if(!is.null(selected_vessels_set1)){
    loglike_selected_set1        <- loglike[loglike$VE_REF %in% selected_vessels_set1,]  # caution
    loglike_selected_set1$VE_REF <- factor(loglike_selected_set1$VE_REF)
    loglike.agg.selected.set1    <-  aggregateLoglike(loglike_selected_set1, agg_by=c("year.month"))
 }
 
 # split per selected set of vessels
 if(!is.null(selected_vessels_set2)){
    loglike_selected_set2        <- loglike[loglike$VE_REF %in% selected_vessels_set2,]  # caution
    loglike_selected_set2$VE_REF <- factor(loglike_selected_set2$VE_REF)
    loglike.agg.selected.set2    <-  aggregateLoglike(loglike_selected_set2, agg_by=c("year.month"))
 }
 
 # split per selected set of vessels
 if(!is.null(selected_vessels_set3)){
    loglike_selected_set3        <- loglike[loglike$VE_REF %in% selected_vessels_set3,]  # caution
    loglike_selected_set3$VE_REF <- factor(loglike_selected_set3$VE_REF)
    loglike.agg.selected.set3    <-  aggregateLoglike(loglike_selected_set3, agg_by=c("year.month"))
 }
 


 ## aggregate by year.month, PER VESSEL
 loglike.agg.vid <-  aggregateLoglike(loglike, agg_by=c( "VE_REF", "year.month"))

  
 # CONTINGENCY TABLE -----> SPECIAL CASE FOR REASON_BACK     
  loglike$VE_REF     <- factor(loglike$VE_REF)
  loglike.agg3.vid   <- data.frame(xtabs(~year.month+VE_REF+reason_back, data=loglike))
  loglike.agg3.vid   <- orderBy (~year.month+VE_REF, data=loglike.agg3.vid)

  loglike.agg3.vid_1 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="1",]

  print(sce)
  print(head(loglike.agg3.vid))
  print(dim(loglike.agg3.vid ))
  print(dim(loglike.agg3.vid_1 ))
  print(head(loglike.agg3.vid_1$year.month))
  print(head(loglike.agg3.vid_1$VE_REF))
  print(dim(paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')))

  loglike.agg3.vid_2 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="2",]
  loglike.agg3.vid_3 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="3",]
  if(nrow(loglike.agg3.vid_1)>0) rownames(loglike.agg3.vid_1) <- paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')
  if(nrow(loglike.agg3.vid_2)>0) rownames(loglike.agg3.vid_2) <- paste (loglike.agg3.vid_2$year.month,".",loglike.agg3.vid_2$VE_REF, sep='')
  if(nrow(loglike.agg3.vid_3)>0) rownames(loglike.agg3.vid_3) <- paste (loglike.agg3.vid_3$year.month,".",loglike.agg3.vid_3$VE_REF, sep='')
  if(nrow(loglike.agg3.vid)>0)rownames(loglike.agg.vid) <- paste (loglike.agg.vid$year.month,".",loglike.agg.vid$VE_REF, sep='')
 # collate
 if(nrow(loglike.agg3.vid)>0) loglike.agg.vid <- cbind(loglike.agg.vid, 
                        reason_1= loglike.agg3.vid_1[rownames(loglike.agg.vid),c(4)],
                         reason_2= loglike.agg3.vid_2[rownames(loglike.agg.vid),c(4)],
                          reason_3= loglike.agg3.vid_3[rownames(loglike.agg.vid),c(4)] )
  
 


  ## aggregate by year.month, PER VESSEL AND PORT
  loglike.agg.vid.port <-  aggregateLoglike(loglike, agg_by=c("VE_REF", "year.month", "land_port","ld_port_x", "ld_port_y"))

  
  #loglike.agg3.vid.port_0 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="0",]
  #loglike.agg3.vid.port_1 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="1",]
  #loglike.agg3.vid.port_2 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="2",]
  #loglike.agg3.vid.port_3 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="3",]
  #rownames(loglike.agg3.vid.port_0) <- paste (loglike.agg3.vid.port_0$year.month,".",loglike.agg3.vid.port_0$VE_REF,".",loglike.agg3.vid.port_0$land_port, sep='')
  #rownames(loglike.agg3.vid.port_1) <- paste (loglike.agg3.vid.port_1$year.month,".",loglike.agg3.vid.port_1$VE_REF,".",loglike.agg3.vid.port_1$land_port, sep='')
  #rownames(loglike.agg3.vid.port_2) <- paste (loglike.agg3.vid.port_2$year.month,".",loglike.agg3.vid.port_2$VE_REF,".",loglike.agg3.vid.port_2$land_port, sep='')
  #rownames(loglike.agg3.vid.port_3) <- paste (loglike.agg3.vid.port_3$year.month,".",loglike.agg3.vid.port_3$VE_REF,".",loglike.agg3.vid.port_3$land_port, sep='')
  #rownames(loglike.agg.vid.port) <- paste (loglike.agg.vid.port$year.month,".",loglike.agg.vid.port$VE_REF,".",loglike.agg.vid.port$land_port, sep='')
 # collate
 
 #loglike.agg.vid.port <- cbind(loglike.agg.vid.port, loglike.agg2.vid.port[,-(1:5)] #,  
   #                   reason_0= loglike.agg3.vid.port_0[rownames(loglike.agg.vid.port),c(4)],
   #                     reason_1= loglike.agg3.vid.port_1[rownames(loglike.agg.vid.port),c(4)],
   #                      reason_2= loglike.agg3.vid.port_2[rownames(loglike.agg.vid.port),c(4)],
   #                       reason_3= loglike.agg3.vid.port_3[rownames(loglike.agg.vid.port),c(4)] 
   #                            )







print("Aggregations from loglike----OK")


if(.Platform$OS.type == "windows") {
## plot landings per species
 X11()
  matplot( loglike.agg[,paste('pop.', 0:(general$nbpops-1), sep='')]/1e3, ylim=c(1,1e4), type="b", main="Simulated landings per species",
           pch=as.character(1:general$nbpops), xlab="Month", ylab="Landings [tons]", lwd=2, lty=1)
   
 ## plot total effort
 X11()
 matplot( loglike.agg[,'effort'], ylim=c(1,max(loglike.agg[,'effort'])), type="b", main="simulated total effort",
            xlab="Month", ylab="Effort [hours]", lwd=2, lty=1)
 
 graphics.off()
} # end windows

 lst_loglike_agg_all[[sim]] <- loglike.agg
 if(!is.null(selected_vessels_set1)){ lst_loglike_agg_selected_set1[[sim]] <- loglike.agg.selected.set1  }
 if(!is.null(selected_vessels_set2)){ lst_loglike_agg_selected_set2[[sim]] <- loglike.agg.selected.set2  }
 if(!is.null(selected_vessels_set3)){ lst_loglike_agg_selected_set3[[sim]] <- loglike.agg.selected.set3  }
 if('GER' %in% general$a.country){ lst_loglike_agg_deu[[sim]] <- loglike.agg.deu }
 if('SWN' %in% general$a.country){ lst_loglike_agg_swe[[sim]] <- loglike.agg.swe }
 if('DEN' %in% general$a.country){ lst_loglike_agg_den[[sim]] <- loglike.agg.den }
 if('ITA' %in% general$a.country){ lst_loglike_agg_ita[[sim]] <- loglike.agg.ita }
 if('HRV' %in% general$a.country){ lst_loglike_agg_hrv[[sim]] <- loglike.agg.hrv }
 if('IRL' %in% general$a.country){ lst_loglike_agg_irl[[sim]] <- loglike.agg.irl }
 if('GBR' %in% general$a.country){ lst_loglike_agg_gbr[[sim]] <- loglike.agg.gbr }
 if('BEL' %in% general$a.country){ lst_loglike_agg_bel[[sim]] <- loglike.agg.bel }
 if('NLD' %in% general$a.country){ lst_loglike_agg_nld[[sim]] <- loglike.agg.nld }
 if('FRA' %in% general$a.country){ lst_loglike_agg_fra[[sim]] <- loglike.agg.fra }
 lst_loglike_agg_vid[[sim]] <- loglike.agg.vid
 lst_loglike_agg_vid_port[[sim]] <- loglike.agg.vid.port
 ## if('ITA' %in% general$a.country){ lst_loglike_agg_met[[sim]] <- loglike.agg.met}


 cat(paste(sce,"...OK\n"))
 cat(paste(sim,"...OK\n"))
 } else{
 cat(paste("error detected for this", sim, ":remove from the list of simus"))
 general$namesimu[[sce]] <- general$namesimu[[sce]][!general$namesimu[[sce]] %in% sim]
 }
 

                     
 } # end for sim
 
 
 assign(paste("lst_loglike_agg_",what,"_all_", sce, sep=''), lst_loglike_agg_all)
 if(!is.null(selected_vessels_set1)){ assign(paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep=''), lst_loglike_agg_selected_set1)  }
 if(!is.null(selected_vessels_set2)){ assign(paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep=''), lst_loglike_agg_selected_set2)  }
 if(!is.null(selected_vessels_set3)){ assign(paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep=''), lst_loglike_agg_selected_set3)  }
 if('GER' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_deu_", sce, sep=''), lst_loglike_agg_deu) }
 if('SWN' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_swe_", sce, sep=''), lst_loglike_agg_swe) }
 if('DEN' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_den_", sce, sep=''), lst_loglike_agg_den) }
 if('ITA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_ita_", sce, sep=''), lst_loglike_agg_ita) }
 if('HRV' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_hrv_", sce, sep=''), lst_loglike_agg_hrv) }
 if('IRL' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_irl_", sce, sep=''), lst_loglike_agg_irl) }
 if('GBR' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_gbr_", sce, sep=''), lst_loglike_agg_gbr) }
 if('FRA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_fra_", sce, sep=''), lst_loglike_agg_fra) }
 if('BEL' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_bel_", sce, sep=''), lst_loglike_agg_bel) }
 if('NLD' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_nld_", sce, sep=''), lst_loglike_agg_nld) }
 assign(paste("lst_loglike_agg_",what,"_vid_", sce, sep=''), lst_loglike_agg_vid)
 assign(paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''), lst_loglike_agg_vid_port)
 ## if('ITA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_met_", sce, sep=''), lst_loglike_agg_met) }

 # save for later use....
 save(list=c(paste("lst_loglike_agg_",what,"_all_", sce, sep=''),
       
       if('GER' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
       if('DEN' %in% general$a.country){paste("lst_loglike_agg_",what,"_den_", sce, sep='')},
       if('DEU' %in% general$a.country){paste("lst_loglike_agg_",what,"_deu_", sce, sep='')},
       if('SWE' %in% general$a.country){paste("lst_loglike_agg_",what,"_swe_", sce, sep='')},
       if('ITA' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
       if('HRV' %in% general$a.country){paste("lst_loglike_agg_",what,"_hrv_", sce, sep='')},
       if('IRL' %in% general$a.country){paste("lst_loglike_agg_",what,"_irl_", sce, sep='')},
       if('GBR' %in% general$a.country){paste("lst_loglike_agg_",what,"_gbr_", sce, sep='')},
       if('FRA' %in% general$a.country){paste("lst_loglike_agg_",what,"_fra_", sce, sep='')},
       if('BEL' %in% general$a.country){paste("lst_loglike_agg_",what,"_bel_", sce, sep='')},
       if('NLD' %in% general$a.country){paste("lst_loglike_agg_",what,"_nld_", sce, sep='')},
         
       if(!is.null(selected_vessels_set1)){ paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep='')},
       if(!is.null(selected_vessels_set2)){ paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep='')},
       if(!is.null(selected_vessels_set3)){ paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep='')},
         
          paste("lst_loglike_agg_",what,"_vid_", sce, sep=''),
          paste("lst_loglike_agg_",what,"_vid_port_", sce, sep='') #,
          #if('ITA' %in% general$a.country){ paste("lst_loglike_agg_",what,"_met_", sce, sep='')}
          
          ),
          file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_loglike_",what,"_agg_", sce,".RData", sep='') )   
     )

 

 } # end for sce

 
 
return()
}




 ## CALLS-----------------------------------------------------------------------
 ## CALLS-----------------------------------------------------------------------
 ## CALLS-----------------------------------------------------------------------
  # first of all, figure out the list of vessels we want to keep for _selected...
   
   if(general$namefolderinput=="CelticSea" && .Platform$OS.type == "unix"){
     explicit_pops <- c(0:27) 
  
   
   
     filename2 <- file.path(general$main.path, general$namefolderinput, "scebaseline", 
                                         paste("loglike_", "simu1", ".dat", sep=''))
     loglike <-read.table(filename2)
     all_vids <- levels(head(loglike[,"V7"]))
    
   
     selected_vessels_set1   <- all_vids
     selected_vessels_set2  <-  all_vids[grep("vsmall",all_vids)]
     selected_vessels_set3  <-  all_vids[-grep("vsmall",all_vids)]
   
   
if (TRUE)  getAggLoglikeFiles(general=general, what="weight",
             explicit_pops=explicit_pops,
             implicit_pops=NULL,
             selected_vessels_set1=selected_vessels_set1,
             selected_vessels_set2=selected_vessels_set2,
             selected_vessels_set3=selected_vessels_set3)  
}





























##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##

 # reload scenarios for what="weight" 
 if(general$case_study=="CelticSea"){
 
source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_CelticSea","DISPLACE_R_outputs_ForCelticSea","loadAggLoglikeFiles.R"))
loadLoglikeFiles(general=general, use_port_info=FALSE) 

  
}



  
##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##

if(general$case_study=="CelticSea"){
   selected <- "_selected_set1_"     # all
   selected <- "_selected_set2_"      
   selected <- "_selected_set3_"     
   sets <- c("_selected_set1_", "_selected_set2_", "_selected_set3_")

   explicit_pops <- c(0:27) 
 
   #*****************************#
   #a_baseline <- "scebaseline"
   a_baseline <- "scesizespectra"    ## CAUTION!
   #*****************************#
   }
   
   

for (selected in sets){
     
outcomes <- NULL

 
what2 <- "weight"
lst_loglike_w_agg_all_1     <- get(paste("lst_loglike_agg_",what2, selected, a_baseline, sep=''))
    dd                      <- table(unlist(lapply(lst_loglike_w_agg_all_1, nrow)))
    expected_nb_rows        <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
    idx                     <- unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows))
    namesimu1               <- names(unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows)))[idx]
    lst_loglike_w_agg_all_1 <- lst_loglike_w_agg_all_1[namesimu1]
     
lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", a_baseline, sep=''))

  
 #*****************************#
 others_than_baseline        <- general$namefolderoutput[!general$namefolderoutput %in% a_baseline]
 #*****************************#

  # quick check at the vessel id level
  plot(density(lst_loglike_w_agg_vid_1[[1]]$gradva))
  lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", others_than_baseline[1], sep=''))
  for (ff in namesimu1){
  lines(density(lst_loglike_w_agg_vid_1[[ff]]$gradva),  col=1)
  lines(density(lst_loglike_w_agg_vid_2[[ff]]$gradva),  col=2)
  }



 

 # fishing effort
  feffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
 
   cat (paste(sce, "------------------------feffort--------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))- sum(as.numeric(x[,'cumsteaming'])))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming'])))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      feffort[sce,"mean"] <- signif(a_mean,5)
     feffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="feffort"))
  }
  print(feffort)

  # note that 
  #boxplot(dd1/dd2, dd1/rev(dd2)) almost identical so we don\B4t care comparing sim with sim whatever the sim

 # steaming effort
  seffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------seffort------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'cumsteaming']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      seffort[sce,"mean"] <- signif(a_mean,5)
     seffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="seffort"))
  }
  print(seffort)

 # percent fishing effort compared to total trip effort
  propfeffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "-------------------------propfeffort-------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
           dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      propfeffort[sce,"mean"] <- signif(a_mean,5)
     propfeffort[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="propfeffort"))
  }
  print(propfeffort)

  # nbtrip
  nbtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "----------------------------nbtrip----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
        namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
        dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'nbtrip']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      nbtrip[sce,"mean"] <- signif(a_mean,5)
     nbtrip[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="nbtrip"))
  }
  print(nbtrip)

 # av_bwtrip
  av_bwtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------av_bwtrip------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
         nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_bwtrip']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_bwtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_bwtrip'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_bwtrip[sce,"mean"] <- signif(a_mean,5)
     av_bwtrip[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_bwtrip")) 
  }
  print(av_bwtrip)

 #  trip_duration
  av_trip_duration <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------av_effort------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
         dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_effort']))))
       ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_trip_duration[sce,"mean"] <- signif(a_mean,5)
     av_trip_duration[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_trip_duration")) 
  }
  print(av_trip_duration)



 # totland_implicit
if(any(lst_loglike_w_agg_all_1[[1]]$totland_implicit!=0)) {
  totland <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "------------------------------totland_implicit--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
        nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland_implicit']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<600]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland[sce,"mean"] <- signif(a_mean,5)
     totland[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland")) 
  }
  print(totland)
  }

 # totland_explicit
  totland_explicit <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------totland_explicit------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
        namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland_explicit']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland_explicit[sce,"mean"] <- signif(a_mean,5)
     totland_explicit[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland_explicit")) 
  }
  print(totland_explicit)

 
 # totland_
  for(sp in explicit_pops){ 
  print(sp)
  totland_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------totland_------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='')]))))[namesimu1] *100) -100
       if(all(is.na(ratio_percent))) ratio_percent[] <- 0
       if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      # ratio_percent <- ratio_percent[ratio_percent<2000]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland_thissp[sce,"mean"] <- signif(a_mean,5)
     totland_thissp[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('totland_', sp, sep='') )) 
  }
  print(totland_thissp)
  }
 

   
 # revenue
  revenue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------revenue------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
           nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      revenue[sce,"mean"] <- signif(a_mean,5)
     revenue[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenue")) 
  }
  print(revenue)


  
 # swept area
  sweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------sweptarea------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] *100) -100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      sweptarea[sce,"mean"] <- signif(a_mean,5)
     sweptarea[sce,"a_CI"] <- signif(a_CI,3)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="sweptarea")) 
  }
  print(sweptarea)

 ## revenue per swept area
 # revenuepersweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
 # for (sce in others_than_baseline){
 #  cat (paste(sce, "--------------------------revpersweptarea------------------------------------\n"))
 #      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
 #      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea'])))))
 #      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
 #      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
 #      print(t.test(dd1,dd2))
 #      ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] *100) -100
 #      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
 #      ratio_percent <- ratio_percent[ratio_percent<500]
 #      a_mean        <- mean(ratio_percent) 
 #      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
 #     revenuepersweptarea[sce,"mean"] <- signif(a_mean,5)
 #    revenuepersweptarea[sce,"a_CI"] <- signif(a_CI,3)
 #    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenuepersweptarea")) 
 # }
 # print(revenuepersweptarea)


  # fuel cost
  fuelcost <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------fuelcost----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost'])))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      fuelcost[sce,"mean"] <- signif(a_mean,4)
     fuelcost[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fuelcost")) 
  }
  print(fuelcost)

   
  #  gradva
  gav <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "-------------------------gradva-------------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      gav[sce,"mean"] <- signif(a_mean,4)
     gav[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="gav")) 
  }
  print(gav)

 
   #  npv net present value NPV assuming 4% discount rate i.e. NPV= Ry1/(1+0.04)^1 + Ry2/(1+0.04)^2 +... 
   nbyears         <- nrow(lst_loglike_w_agg_all_1[[1]])/12 # because monthly data
   discount_factor <- sapply(rep(paste("1/((1+0.04)^",1:nbyears,")"), each=12), function(x) eval(parse(text=x)))
  
  npv <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------npv----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
       print(t.test(dd1,dd2))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      npv[sce,"mean"] <- signif(a_mean,4)
     npv[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="npv")) 
  }
  print(npv)


    # av_vapuf
  av_vpuf <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------av_vapuf--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf'], na.rm=TRUE))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_vpuf[sce,"mean"] <- signif(a_mean,4)
     av_vpuf[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf")) 
  }
  print(av_vpuf)

      # av_vapuf_month
  av_vapuf_month <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------av_vapuf_month----------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      av_vapuf_month[sce,"mean"] <- signif(a_mean,4)
     av_vapuf_month[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf_month")) 
  }
  print(av_vapuf_month)

     # trip-based CPUEs (useless because somehow redundant with vpuf)
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------trip_based_cpue--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort']))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<200]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="trip_based_cpue")) 
  }
  print(cpue)


       # fishing-based CPUEs
 if(any(lst_loglike_w_agg_all_1[[1]]$totland_implicit!=0)) {
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "------------------------------cpue_implicit--------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue")) 
  }
  print(cpue)
 }

       # fishing-based CPUEs (explicit only)
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "-----------------------------cpue_explicit---------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / 
                              unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       #ratio_percent <- ratio_percent[ratio_percent<600]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue_explicit")) 
  }
  print(cpue)



        # fishing-based CPUEs 
  for(sp in explicit_pops){ 
  print(sp)
 cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------------cpue sp------------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,paste('pop.',sp,sep='') ])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
       print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
       if(all(is.na(ratio_percent))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
        if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
        #ratio_percent <- ratio_percent[ratio_percent<400]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
     cpue[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('fishing_based_cpue_',sp,sep='') )) 
  }
  print(cpue)
  } 

 
 
  # discard rate disc_rate_
  for(sp in explicit_pops){ 
  print(sp)
  disc_rate_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "----------------------------------disc_rate_----------------------------\n"))
       lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,paste('disc_rate_', sp, sep='') ], na.rm=TRUE)))[namesimu1])
       dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1])
       if(all(is.na(dd1)))  dd1 [] <-0
       if(all(is.na(dd2)))  dd2 [] <-0
       # print(t.test(dd1,dd2))
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE))))
       ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] *100 )-100
       if(all(is.na(ratio_percent)))  ratio_percent [] <-0
           if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<500]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      disc_rate_thissp[sce,"mean"] <- signif(a_mean,4)
     disc_rate_thissp[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('disc_rate_', sp, sep=''))) 
  }
 print(disc_rate_thissp)
 }
 

   # measure of equity, diversity or distributional profit
   # Shannon index is sum from i=1 to nbVessels of p_i*ln (p_i) with p_i is  the proportion of vessels belonging to the ith class of revenue in the dataset of interest. 
   # nb of classes of revenue to decide upon?
   # but better to use the ginin index or 20:20 Ratio, ...
   #or the the Robin Hood index H (because bwteen 0 to 1 then can be useful for a ratio)
   # http://en.wikipedia.org/wiki/Income_inequality_metrics
  HooverIndex <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
   cat (paste(sce, "--------------------------------------------------------------\n"))
       lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
       namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
       nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gav']))))
       dd1 <- (unlist(lapply(lst_loglike_w_agg_vid_1, function (x) {                              
                              ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
                              ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
                              ai <- table(cut(x[,'gradva'], breaks=ei))
                              ei <- ei[-1]
                              hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
                              }
             ))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_vid_2, function (x) {   
                              ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
                              ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
                              ai <- table(cut(x[,'gradva'], breaks=ei))
                              ei <- ei[-1]
                              hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
                             
                              }
             ))[namesimu1])
      
       print(t.test(dd1,dd2))
       ratio_percent <- (dd2 /
                          dd1 *100 )-100
       ratio_percent <- ratio_percent[!is.na(ratio_percent)]
       ratio_percent <- ratio_percent[ratio_percent<300]
       a_mean        <- mean(ratio_percent) 
       a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      HooverIndex[sce,"mean"] <- signif(a_mean,4)
     HooverIndex[sce,"a_CI"] <- signif(a_CI,4)
     outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="hoover")) 
  }
  print(HooverIndex)





# look at the reason to go back.....
#idx <- rownames(lst_loglike_agg_weight_vid_high_profit_grounds[[3]])
#lst_loglike_agg_weight_vid_high_profit_grounds_biolsce_mig[[3]] [,c('reason_1', 'reason_2', 'reason_3')] - lst_loglike_agg_weight_vid_high_profit_grounds[[3]][idx, c('reason_1', 'reason_2', 'reason_3')]  

#boxplot(cbind(as.matrix(lst_loglike_agg_weight_vid_area_closure_WMF_NAT2000_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')]),
# as.matrix(lst_loglike_agg_weight_vid_high_profit_grounds[[3]][idx, c('reason_1', 'reason_2', 'reason_3')])))


#dd <- as.matrix(lst_loglike_agg_weight_vid_area_closure_WMF_NAT2000_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')])
#dd2 <- as.matrix(lst_loglike_agg_weight_vid_high_profit_grounds_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')])

#common <- unique(c(rownames(dd)[rownames(dd)%in% rownames(dd2)], rownames(dd2)[rownames(dd2)%in% rownames(dd)]))
#comp <- dd[common,]-dd2[common,]
#comp <- replace(comp, comp==0 ,NA)
#boxplot(comp)


# export
write.table(outcomes,
                   file=file.path(general$main.path, general$namefolderinput, 
                     paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')),
              sep=";", quote=FALSE, row.names=FALSE) 

} # end for-loop on sets


##--------------------------##
##--------------------------##
##--------------------------##

if(general$namefolderinput=="CelticSea" && TRUE){

 
   selected <- "_selected_set1_"     
   selected <- "_selected_set2_"     
   selected <- "_selected_set3_"     

sets <- c("_selected_set1_", "_selected_set2_", "_selected_set3_")
for (selected in sets){
 
 outcomes <- read.table(file.path(general$main.path, general$namefolderinput, 
                    paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')), header=TRUE, sep=";")

 ## CAUTION: (not the same levels when reading or when using directly the obj in the env)
 levels (outcomes$scenario) <-  c("sceavchok","sceavchokpszpctrastopifchok",
                                   "sceavchokpszpectra",
                                   "sceavhtariffspszpctratariffs",   "scebaseline",                   
                                   "scesizespectrastopifchok", "scetrgthtariffspszpctratariffs")
                                      

 # add baseline at 0,0,0, etc.
 baseline <- outcomes[outcomes$scenario == "scebaseline",]  # init
 baseline$ratio_percent <-0
 baseline$scenario <- "scesizespectra"
 outcomes <- rbind.data.frame(baseline, outcomes)
 outcomes$scenario <- factor(outcomes$scenario)

 # select because too much info
 select_some <- TRUE
 if(select_some){
 selected_variables <- c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit",
                                       "totland_explicit", 
                                       #"totland_0", 
                                       #"totland_1",
                                       #"totland_2", "totland_3", "totland_4", "totland_5",
                                        "sweptarea", "npv", "av_vpuf_month", "hoover")
 outcomes           <- outcomes[outcomes$variable %in% selected_variables,]
 
 outcomes$variable <- factor(outcomes$variable)
 outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c( "F. effort", "S. effort", "Nb. of trips", "Trip duration",  "CPUE at fishing",
                                                                                       "Tot landings",
                                                                                       # "Landings HKE", 
                                                                                       #"Landings SOL",
                                                                                       # "Landings MUT", "Landings MTS", "Landings NEP", "Landings CTC",  
                                                                                        "Swept Area",  
                                                                                        "NPV", "VPUF", "Income inequality"))
 
 selected_scenarios <-    c("scesizespectra",
                                    "sceavchokpszpectra",
                                 "scesizespectrastopifchok",
                                 "sceavchokpszpctrastopifchok",
                                 "sceavhtariffspszpctratariffs",
                                 "scetrgthtariffspszpctratariffs"                               
                             )
                            
                                 
 outcomes <- outcomes[outcomes$scenario %in%selected_scenarios,]
 outcomes$scenario <- factor(outcomes$scenario)
 outcomes$scenario <- factor(outcomes$scenario, levels=selected_scenarios, labels=  selected_scenarios
                                )

 library(lattice)                            
 bwplot(ratio_percent~variable| scenario, data=outcomes)

 # a better plot
 namefile       <- paste(paste("indicators_boxplot_persce_",selected, sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 the_dim        <- c(2400, 2400)
  

 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2], 
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 library(ggplot2)
 outcomes[outcomes$ratio_percent< -25, "ratio_percent"] <- -25
 outcomes[outcomes$ratio_percent>25, "ratio_percent"] <- 25
 p <- ggplot(outcomes[outcomes$ratio_percent>=-25 & outcomes$ratio_percent<=25,], aes(factor(variable), ratio_percent))  + geom_boxplot(outlier.shape=1)  + 
             labs(x = "Indicators", y = "% ratio over the baseline") # + ylim(-20, 20) 
 print(
       p   + facet_wrap( ~ scenario, ncol=2, scales="free_y")    + theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  + geom_boxplot(outlier.shape=NA)
       )
 
 dev.off()

 } else{
 #keep all
 namefile       <- paste(paste("indicators_ALL_boxplot_persce_", sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 the_dim        <- c(3100, 1800)
  

 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2], 
                                   units = "px", pointsize = 12,  res=300, compression="lzw")

 library(ggplot2)
 p <- ggplot(outcomes, aes(factor(variable), ratio_percent))   + geom_boxplot(outlier.shape=NA)  +
 p   + facet_wrap( ~ scenario, ncol=2, scales="free_y")   + coord_cartesian(ylim = range(boxplot(outcomes$ratio_percent, plot=FALSE)$stats)*c(.9, 1.1)) + theme(axis.text.x = element_text(angle = 90)) + geom_abline(intercept=0, slope=0, color="grey", lty=2) 

 dev.off()
 }
 
 } # end FOR-loop over sets
 
 } # end FALSE


##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##
##--------------------------##


        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
        ##!!!!!!!!!!!!!!OBSERVED VS. SIMULATED PLOTS!!!!!!!!!!!!!!!!!!!##
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


             
compare_obs_sim_landings <- function (lst_loglike_agg, sce="baseline", a.comment, what="per_vessel", count=0)  {  
     
   
      # filter lst_loglike_agg to trash away the failed (i.e. non-complete) simus:
    # detection according to the number of rows...
    dd                 <- table(unlist(lapply(lst_loglike_agg, nrow)))
    expected_nb_rows   <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
    idx                <- unlist(lapply(lst_loglike_agg, function(x) nrow(x)==expected_nb_rows))
    namesimu1          <- c(names(unlist(lapply(lst_loglike_agg, function(x) nrow(x)==expected_nb_rows)))[idx], 'obs')
    lst_loglike_agg <- lst_loglike_agg[namesimu1]
 

     if (what %in% c('per_country')) {
        output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots")
        segment.names <- colnames( lst_loglike_agg[[1]] )[-1]
           par(mfrow=c(3,3))
        } else{
         if (what %in% c('per_vessel')) {
           output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots", "per_vessel")
           segment.names <- colnames( lst_loglike_agg[[1]] )[-(1:2)]
             par(mfrow=c(3,3))
           } else{
            if (what %in% c('per_pop')) {
              output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots", "per_pop")
              segment.names <-  a.comment
             }else{}
           }
        }
        
          
         
           outputfile <- file.path(general$main.path, output.folder,
                                        paste("loglike_panel",count,"_",gsub("\\.","",a.comment),".pdf",sep="" ))
           pdf(file = outputfile)
           par(mfrow=c(3,3))
  
           namesimu <- namesimu1[namesimu1!="obs"]
         if(length(namesimu)!=0){
  
           for(seg in segment.names ){  # for each col
              cat (paste(seg, "\n"))

              count <- count +1
          
              mat.sim1 <- matrix(unlist(lapply(lst_loglike_agg[namesimu], function(x){
                  res <- try(x[,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_loglike_agg[[1]])); res
                  })), nrow=nrow(lst_loglike_agg[[1]]), byrow=FALSE)
              colnames(mat.sim1) <- c(paste(seg, ".", namesimu, sep=''))


             if (length(grep('pop.', seg))!=0) a.unit <- 1e3 else a.unit <-1
             a.xlab <- "month"
             a.ylab <- ""

             plot(0,0, type='n', axes=FALSE, xlim=c(1,nrow(lst_loglike_agg[[1]])),
                 ylim=c(0, (max(c(mat.sim1,mat.sim1), na.rm=TRUE)/a.unit)*1.2),
                     ylab="", xlab=a.xlab)
             if(what=="per_pop") title(paste(seg, lst_loglike_agg[[1]][1,1]))
             if(what=="per_vessel") title(paste(seg))
             if(what=="per_country") title(paste(seg))

             mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
             mtext(side=1 , a.xlab, outer=TRUE, line=-1)

             axis(1, labels= lst_loglike_agg[[1]]$year.month, at=1:nrow(lst_loglike_agg[[1]]), las=2, cex.axis=0.5)
             axis(2, las=2)
             box()

             # polygon 5-95% for simus
             mat.sim1 <- replace(mat.sim1, is.na(mat.sim1),0)
             polygon(c(1:nrow(mat.sim1), rev(1:nrow(mat.sim1))  ),
                  c(apply(mat.sim1, 1, quantile, 0.05)/a.unit,
                    rev(apply(mat.sim1, 1, quantile, 0.95)/  a.unit)) ,
              col=  rgb(0,1,0,0.5), border=FALSE)

             # add obs. data
             dd <- lst_loglike_agg[['obs']]
             dd <- dd[order(dd$year.month),]
             if(seg %in% colnames(dd)) lines(1:12, dd[1:12,seg]/a.unit, col=1, lwd=2)

             if(count%%9 ==0){
                   # save the current one
                  dev.off()
                  # and start a new one...
                  outputfile <- file.path(general$main.path, output.folder,
                                        paste("loglike_panel",count,"_",gsub("\\.","",a.comment),".pdf",sep="" ))
                  pdf(file = outputfile)
                  par(mfrow=c(3,3))
                  }
    
       }   # end for column in lst_loglike_agg[[1]]
       
         }else{print("no similar simus for this vessel")} # end namesimu!=0
         
       # save the last one
       dev.off()
       
             
 return()
}

#------------
#------------
compare_obs_sim_landings_regression_plot <- function (lst_loglike_agg, sce="baseline", 
                                                     a.comment, what="per_vessel", count=0)  {  
     
   
      # filter lst_loglike_agg to trash away the failed (i.e. non-complete) simus:
    # detection according to the number of rows...
    dd                 <- table(unlist(lapply(lst_loglike_agg, nrow)))
    expected_nb_rows   <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
    idx                <- unlist(lapply(lst_loglike_agg, function(x) nrow(x)==expected_nb_rows))
    namesimu1          <- c(names(unlist(lapply(lst_loglike_agg, function(x) nrow(x)==expected_nb_rows)))[idx], 'obs')
    lst_loglike_agg <- lst_loglike_agg[namesimu1]
 

     if (what %in% c('per_country')) {
        output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots")
        segment.names <- colnames( lst_loglike_agg[[1]] )[-1]
           par(mfrow=c(3,3))
        } else{
         if (what %in% c('per_vessel')) {
           output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots", "per_vessel")
           segment.names <- colnames( lst_loglike_agg[[1]] )[-(1:2)]
             par(mfrow=c(3,3))
           } else{
            if (what %in% c('per_pop')) {
              output.folder <-  file.path (general$namefolderinput, sce, "jpeg_plots", "per_pop")
              segment.names <-  a.comment
             }else{}
           }
        }
        
          
         
           a.unit <- 1e6 
           a.xlab <- "Observed landings (4000 tons)"
           a.ylab <- "Simulated landings (4000 tons)"

           outputfile <- file.path(general$main.path, general$namefolderinput,
                                          paste("loglike_regression_plot_",gsub("\\.","",a.comment),".tiff",sep="" )
                                        )
           tiff(file = outputfile, width = 1500, height = 1500,
                                   units = "px", pointsize = 12,  res=300)
           #windows(4,4)
           par(mar=c(4,4,1.5,1))
           par(mfrow=c(1,1))
           plot(0,0, type='n', axes=FALSE, xlim=c(0.01, (40000000/a.unit)*1.2),
                 ylim=c(0.01, (40000000/a.unit)*1.2),    log="xy",
                     ylab="", xlab="")
           axis(1)
           axis(2, las =2)          
           box()
       
         
           namesimu <- namesimu1[namesimu1!="obs"]
  
           segment.names <- segment.names [grep("pop.",segment.names)]
           res <- matrix(0, ncol=2, nrow=length(segment.names))
           colnames(res) <- c("obs","sim")
           
           for(seg in segment.names ){  # for each col
              cat (paste(seg, "\n"))

              count <- count +1
       
              mat.sim1 <- matrix(unlist(lapply(lst_loglike_agg[namesimu], function(x){
                  res <- try(x[,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_loglike_agg[[1]])); res
                  })), nrow=nrow(lst_loglike_agg[[1]]), byrow=FALSE)
              colnames(mat.sim1) <- c(paste(seg, ".", namesimu, sep=''))


              #mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
             #mtext(side=1 , a.xlab, outer=TRUE, line=-1)

   
            # sim
             the_sim_lower <- apply(mat.sim1[1:12,], 1, quantile, 0.05)/a.unit # keep only the first year i.e. 2010
             the_sim_median <- apply(mat.sim1[1:12,], 1, quantile, 0.5)/a.unit # keep only the first year i.e. 2010
             the_sim_upper <- apply(mat.sim1[1:12,], 1, quantile, 0.95)/a.unit # keep only the first year i.e. 2010
            # obs
             dd <- lst_loglike_agg[['obs']]
             dd <- dd[order(dd$year.month),]
             the_obs_median <- dd[1:12,seg]/a.unit

             cat(paste("obs: ",sum(the_obs_median), " sim: ",sum(the_sim_median),"\n"))
         
            ## color if explicit pop or not
            # explicit_pops <-  c(2,3,4,5,7,9,10,11,12,13,14,15,20,21,26,29,30) # canadian  
            # explicit_pops <-  c(3,7,10) # canadian  
             explicit_pops <-  c(10, 11) # myfish 
            #highlight_pops <- c(3,7,10)
             if(unlist(lapply(strsplit(seg, split="\\."),function(x) x[2])) %in% explicit_pops) a_col<-"black" else a_col<-grey(0.7)
            # if(unlist(lapply(strsplit(seg, split="\\."),function(x) x[2])) %in% highlight_pops) a_pch<-1 else a_pch<-16
            a_pch <- 16
              if(sce=="implicit")  a_col<-grey(0.7)
  
             res[count,] <- c(sum(the_obs_median), sum(the_sim_median)) 
             points(sum(the_obs_median), sum(the_sim_median), col="white", cex=2, lwd=1.2)
             points(sum(the_obs_median), sum(the_sim_median), pch=a_pch, col=a_col, cex=2)
  
  # put the name of the stock 
    #        text(sum(the_obs_median), sum(the_sim_median), unlist(lapply(strsplit(seg, split="\\."),function(x) x[2])))
  #print(seg)
  #browser()
  
             segments(sum(the_obs_median), sum(the_sim_lower), sum(the_obs_median), sum(the_sim_upper), col=a_col)
             #text(sum(the_obs_median), sum(the_sim_median), seg)
             lines (seq(0.001,80,by=1),seq(0.001,80,by=1), lty=2)

            
       }   # end for column in lst_loglike_agg[[1]]

       a_lm <- lm(res[-c(8,16,21:22),"sim"]~res[-c(8,16,21:22),"obs"]-1)
       a_lm <- lm(res[-c(16,21:22),"sim"]~res[-c(16,21:22),"obs"]-1)
       a_lm <- lm(res[,"sim"]~res[,"obs"]-1)
       a_lm <- lm(res[,"sim"]~res[,"obs"]-1)
       #abline(abline(a_lm)) # remove 7:SPR.2232; 16:NOP.nsea, 20:SAN.nsea and 21:MAC.nsea
       print(summary(a_lm))

       mtext(side=1, a.xlab, outer=FALSE, line=2 )
       mtext(side=2, a.ylab, outer=FALSE, line=3 )
       #if(sce=="baseline") mtext(side=3, "(b)", cex=1.2, adj=0, line=0.2)
       #if(sce=="implicit") mtext(side=3, "(a)", cex=1.2, adj=0, line=0.2)
        
       # save the last one
       dev.off()
       #savePlot(file=outputfile, type="wmf")
             
 return()
}
   
if(FALSE){
   #--------------
   # calls
   for (sce in general$namefolderoutput){
     lst_loglike_agg_all <- get(paste("lst_loglike_agg_weight_all_", sce, sep=''))
     lst_loglike_agg_den <- get(paste("lst_loglike_agg_weight_den_", sce, sep=''))
     lst_loglike_agg_vid <- get(paste("lst_loglike_agg_weight_vid_", sce, sep=''))

     lst_loglike_agg_den[['obs']] <-   cbind(den_obs_land_2012,
                                           effort=den_obs_effort_2012,
                                           nbtrip=den_obs_nbtrips_2012, 
                                           bwtrip=den_obs_bwtrip_2012,
                                           av_trip_duration=den_obs_av_trip_duration_2012,  
                                           av_bwtrip=den_obs_av_bwtrip_2012 ) # add the obs data in the same object
     
   
   # call per country
   cat("if it still doesn't exist, 'jpeg_plots' folder is created in ",
                      file.path(general$main.path, general$namefolderinput, sce),"\n")
   dir.create(file.path(general$main.path, general$namefolderinput, sce, 'jpeg_plots'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
   compare_obs_sim_landings(lst_loglike_agg_den, sce=sce, a.comment="den", what="per_country", count=0)   #den  
   graphics.off()
  
   # regression plot
   #if(sce=="baseline") compare_obs_sim_landings_regression_plot(lst_loglike_agg_den, sce=sce, 
   if(sce=="stecf_baseline") compare_obs_sim_landings_regression_plot(lst_loglike_agg_den, sce=sce, 
                                      a.comment="for_baseline_den", what="per_country", count=0)   #den  
   
   
   # calls per vessel
   par(mfrow=c(3,3))
   count <- 0
   for(vid in unique(lst_loglike_agg_vid[[1]]$VE_REF)){
        count <- count+1
        cat (paste(vid, "\n"))
          lst_loglike_agg_this <- lapply(lst_loglike_agg_vid, function(x) x[x$VE_REF==vid,]) 
          lst_loglike_agg_this <- lapply(lst_loglike_agg_this, function(x) merge(x, expand.grid(VE_REF=x[1,"VE_REF"], year.month=levels(x$year.month)), all=T) )  # all.combi to fill in the gap
          lst_loglike_agg_this <- lapply(lst_loglike_agg_this, function(x) replace(x, is.na(x), 0) )
          
          if(length(grep("DNK", vid))!=0) {
                  dd <- den_obs_land_2012_vid[as.character(den_obs_land_2012_vid$VE_REF)==vid,]
                  colnames(dd) <- gsub("KG", "pop",colnames(dd))
                  dd <-  merge(dd, expand.grid(VE_REF=vid, year.month=levels(dd$year.month)), all=T)
                  dd <- replace(dd, is.na(dd), 0)
                  obs<-  cbind(effort=den_obs_effort_2012_vid[vid,],
                                       nbtrip=den_obs_nbtrips_2012_vid[vid,],
                                       bwtrip=den_obs_bwtrip_2012_vid[vid,],
                                       av_trip_duration=den_obs_av_trip_duration_2012_vid[vid,],  
                                       av_bwtrip=den_obs_av_bwtrip_2012_vid[vid,]
                                       )
                 lst_loglike_agg_this[['obs']] <- 
                                    cbind(dd, obs[dd$year.month,]) # add the obs data in the same object
                 }
          if(length(grep("GER", vid))!=0) {
                  dd <- deu_obs_land_2012_vid[as.character(deu_obs_land_2012_vid$VE_REF)==vid,]
                  colnames(dd) <- gsub("KG", "pop",colnames(dd))
                  dd <-  merge(dd, expand.grid(VE_REF=vid, year.month=levels(dd$year.month)), all=T)
                  dd <- replace(dd, is.na(dd), 0)
                  obs<- cbind(effort=deu_obs_effort_2012_vid[vid,],
                                       nbtrip=deu_obs_nbtrips_2012_vid[vid,],
                                       bwtrip=deu_obs_bwtrip_2012_vid[vid,],
                                       av_trip_duration=deu_obs_av_trip_duration_2012_vid[vid,],  
                                       av_bwtrip=deu_obs_av_bwtrip_2012_vid[vid,]
                                       )
                 lst_loglike_agg_this[['obs']] <- 
                                    cbind(dd, obs[dd$year.month,]) # add the obs data in the same object
                 }
          if(length(grep("SWN", vid))!=0) {
                  dd <- swe_obs_land_2012_vid[as.character(swe_obs_land_2012_vid$VE_REF)==vid,]
                  colnames(dd) <- gsub("KG", "pop",colnames(dd))
                  dd <-  merge(dd, expand.grid(VE_REF=vid, year.month=levels(dd$year.month)), all=T)
                  dd <- replace(dd, is.na(dd), 0)
                  obs<-  cbind(effort=swe_obs_effort_2012_vid[vid,],
                                       nbtrip=swe_obs_nbtrips_2012_vid[vid,],
                                       bwtrip=swe_obs_bwtrip_2012_vid[vid,],
                                       av_trip_duration=swe_obs_av_trip_duration_2012_vid[vid,],  
                                       av_bwtrip=swe_obs_av_bwtrip_2012_vid[vid,]
                                       )
                 lst_loglike_agg_this[['obs']] <- 
                                    cbind(dd, obs[dd$year.month,]) # add the obs data in the same object
                 }
            per_vessel <- TRUE
            per_pop    <- FALSE # per pop per vessel: to fix: imcompatibble with per_vessel
            if(per_vessel &&  sce=="baseline") {  # PER VESSEL
              cat("if it still doesn't exist, 'per_vessel' folder is created in ",
                      file.path(general$main.path, general$namefolderinput, sce, "jpeg_plots"),"\n")
              dir.create(file.path(general$main.path, general$namefolderinput, sce, 'jpeg_plots', 'per_vessel'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
              compare_obs_sim_landings(lst_loglike_agg_this,  sce=sce, a.comment=vid, what="per_vessel")     
              graphics.off()
             }
             if(per_pop) {        # PER POP
              cat("if it still doesn't exist, 'per_pop' folder is created in ",
                      file.path(general$main.path, general$namefolderinput, sce, "jpeg_plots"),"\n")
              dir.create(file.path(general$main.path, general$namefolderinput, sce, 'jpeg_plots', 'per_pop'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
              compare_obs_sim_landings(lst_loglike_agg_this,  sce=sce, a.comment="pop.1", what="per_pop", count=count)
              graphics.off()
              }
        
       
        }

      } # end for sce 
   
 


}  # end TRUE


        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
        ##!!!!!!!!!!!!!!SIMULATED VS. SIMULATED PLOTS!!!!!!!!!!!!!!!!!!!##
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


        
             
              
compare_sim_sim_landings <- function (lst_loglike_agg_1, lst_loglike_agg_2, ...,  plot_obs=TRUE, 
                                       idx.sim=list(sce1=c(1), sce2=c(1)), combined_name=c("baseline_vs_implicit"), 
                                         a.comment="", what="per_vessel", what2="weight", count=0,
                                          a.xlab="", a.ylab="", a.unit=1, do_mtext=FALSE)  {  
      
     lstargs <- list(...)
  
  
    ## CAUTION: DEBUG to get the same number of rows......
    #tstep                               <- paste(rep(2012:2016, each=12), c("01","02","03","04","05","06","07","08","09","10","11","12"), sep=".")
    tstep                               <- paste(rep(2015:2019, each=12), c("01","02","03","04","05","06","07","08","09","10","11","12"), sep=".")
    names.for.change                    <- names(lst_loglike_agg_1)[!names(lst_loglike_agg_1)%in%"obs"]
    lst_loglike_agg_1[names.for.change] <- lapply(lst_loglike_agg_1[names.for.change], function(x) {merge(data.frame(year.month=tstep), x, all.x=TRUE)})
    lst_loglike_agg_2                   <- lapply(lst_loglike_agg_2, function(x) {merge(data.frame(year.month=tstep), x, all.x=TRUE)})

  
    # filter lst_popdyn to trash away the failed (i.e. non-complete) simus:
    # detection according to the number of rows...
    # for 1.
    dd                <- table(unlist(lapply(lst_loglike_agg_1, nrow)))
    expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
    idx               <- unlist(lapply(lst_loglike_agg_1, function(x) nrow(x)==expected_nb_rows))
    namesimu1          <- names(unlist(lapply(lst_loglike_agg_1, function(x) nrow(x)==expected_nb_rows)))[idx]
    lst_loglike_agg_1 <- lst_loglike_agg_1[namesimu1]
    # for 2.
    dd                <- table(unlist(lapply(lst_loglike_agg_2, nrow)))
    expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)])) [1] # most common number of rows
    idx               <- unlist(lapply(lst_loglike_agg_2, function(x) nrow(x)==expected_nb_rows))
    namesimu2          <- names(unlist(lapply(lst_loglike_agg_2, function(x) nrow(x)==expected_nb_rows)))[idx]
    lst_loglike_agg_2 <- lst_loglike_agg_2[namesimu2]

    if(length(namesimu1)>2 && length(namesimu2)>2){
    
    # folder creations
    cat("if it still doesn't exist, an output folder is created in ",
                    file.path(general$main.path, general$namefolderinput),"\n")
    dir.create(file.path(general$main.path, general$namefolderinput, combined_name), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
    dir.create(file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
    dir.create(file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots', 'per_vessel'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
    dir.create(file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots', 'per_pop'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
      
      
      
    
    # small adjustements.... 
     if (what %in% c('per_country')) {
        output.folder <-  file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots')
        segment.names <- colnames( lst_loglike_agg_1[[1]] )[-1]
        outputfile <- file.path(output.folder,
                        paste("loglike_",what2,"_panel",count,"_",gsub("\\.","",a.comment),'.pdf',sep="" ))
        pdf(file = outputfile)
        par(mfrow=c(3,3))
        a.cex.axis <-0.5
         
         } else{
         if (what %in% c('per_vessel')) {
           output.folder <- file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots', 'per_vessel')
           segment.names <- colnames( lst_loglike_agg_1[[1]] )[-(1:2)]
           outputfile <- file.path(output.folder,
                      paste("loglike_",what2,"_panel",count,"_",gsub("\\.","",a.comment),'.pdf',sep="" ))
           pdf(file = outputfile)
           par(mfrow=c(3,3))
              a.cex.axis <-0.5
           } else{
            if (what %in% c('per_pop')) {
              output.folder <-  file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots', 'per_pop')
              segment.names <-  a.comment
              outputfile <- file.path(output.folder,
                     paste("loglike_",what2,"_panel",count,"_",gsub("\\.","",a.comment),'.pdf',sep="" ))
              pdf(file = outputfile)
              par(mfrow=c(3,3))
              a.cex.axis <-0.5
               }else{
                output.folder <-  file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots')
                segment.names <-  what
                a.cex.axis <-1
                a.comment <- what
                outputfile <- file.path(output.folder,
                     paste("loglike_panel",count,"_",gsub("\\.","",a.comment),'.pdf',sep="" ))
                pdf(file = outputfile)
                par(mfrow=c(1,1))
                par(mar=c(5,5,1,1))
             
             }
           }
        }
    
   
              print(lapply(lst_loglike_agg_1[ namesimu1 ], nrow))  # check if same number of rows between simu!!
              print(lapply(lst_loglike_agg_2[ namesimu2 ], nrow))  # check if same number of rows between simu!!
              refsimu1 <- namesimu1[1] 
              refsimu2 <- namesimu2[1] 
         
           # init for pie chart
           #segment.names <- c(segment.names, "fcpue_all", "fcpue_explicit", "fcpue_implicit", "fcpue_pop3", "fcpue_pop7", "fcpue_pop10"   )
           #segment.names <- c(segment.names, "fcpue_all", "fcpue_explicit", "fcpue_implicit", "fcpue_pop10",  "fcpue_pop11"   )
           segment.names <- c(segment.names, "fcpue_all", "fcpue_explicit", "fcpue_implicit", "fcpue_pop0",  "fcpue_pop1",  "fcpue_pop2",  "fcpue_pop3"   )
         
           sum_all_months_and_mean_across_runs1 <- matrix(0, nrow=1, ncol=length(segment.names))
           colnames(sum_all_months_and_mean_across_runs1) <- segment.names
           sum_all_months_and_mean_across_runs2 <- matrix(0, nrow=1, ncol=length(segment.names))
           colnames(sum_all_months_and_mean_across_runs2) <- segment.names
          
           segment.names <- segment.names[!is.na(segment.names)]
           
           for(seg in segment.names ){  # for each col
              cat (paste(seg, "\n"))

              count <- count +1
              
              # for sce1
              mat.sim1 <- matrix(unlist(lapply(lst_loglike_agg_1[namesimu1], function(x){
                  res <- try(x[,seg], silent=TRUE)
                  if(seg=="fcpue_all")        res <- x[,'totland']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_explicit")   res <- x[,'totland_explicit']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_implicit")   res <- x[,'totland_implicit']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop0")       res <- x[,'pop.0']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop1")       res <- x[,'pop.1']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop2")       res <- x[,'pop.2']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop3")       res <- x[,'pop.3']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop7")       res <- x[,'pop7']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop10")      res <- x[,'pop10']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop11")      res <- x[,'pop11']/(x[,'effort']- x[,'cumsteaming'])  
                  if(class(res)=="try-error") res <- rep(NA, ncol(lst_loglike_agg_1[[refsimu1]]))
                  res
                  })), nrow=nrow(lst_loglike_agg_1[[refsimu1]]), byrow=FALSE)
              colnames(mat.sim1) <- c(paste(seg, namesimu1 , sep=''))
              
              sum_all_months_and_mean_across_runs1[,seg] <- mean(apply(mat.sim1, 2, function(x) sum (as.numeric(x), na.rm=TRUE)))

              # for sce2
              mat.sim2 <- matrix(unlist(lapply(lst_loglike_agg_2[namesimu2 ], function(x){
                  res <- try(x[,seg], silent=TRUE)
                  if(seg=="fcpue_all")        res <- x[,'totland']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_explicit")   res <- x[,'totland_explicit']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_implicit")   res <- x[,'totland_implicit']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop0")       res <- x[,'pop.0']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop1")       res <- x[,'pop.1']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop2")       res <- x[,'pop.2']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop3")       res <- x[,'pop.3']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop7")       res <- x[,'pop7']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop10")      res <- x[,'pop10']/(x[,'effort']- x[,'cumsteaming'])  
                  if(seg=="fcpue_pop11")      res <- x[,'pop11']/(x[,'effort']- x[,'cumsteaming'])  
                  if(class(res)=="try-error") res <- rep(NA, ncol(lst_loglike_agg_2[[refsimu2]]))
                  res
                  })), nrow=nrow(lst_loglike_agg_2[[refsimu2]]), byrow=FALSE)
              colnames(mat.sim2) <- c(paste(seg, namesimu2 , sep=''))

              sum_all_months_and_mean_across_runs2[,seg] <- mean(apply(mat.sim2, 2, function(x) sum (as.numeric(x), na.rm=TRUE)))
  
     
             # debug
             mat.sim1 <- replace(mat.sim1,  is.na(mat.sim1), 0)
             mat.sim2 <- replace(mat.sim2,  is.na(mat.sim2), 0)
   
   
   
             plot(0,0, type='n', axes=FALSE, xlim=c(1,nrow(lst_loglike_agg_1[[refsimu1]])),
                 ylim=c(0, (max(c(mat.sim1,mat.sim2), na.rm=TRUE)/a.unit)*1.2),
                     ylab=a.ylab, xlab=a.xlab)
             if(what=="per_pop") title(paste(seg, lst_loglike_agg_1[[refsimu1]][1,1]))
             if(what=="per_vessel") title(paste(seg))
             if(what=="per_country") title(paste(seg))

             if(do_mtext){
               mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
               mtext(side=1 , a.xlab, outer=TRUE, line=-1)
               }
               
             axis(1, labels= lst_loglike_agg_1[[refsimu1]]$year.month,
                           at=1:nrow(lst_loglike_agg_1[[refsimu1]]), las=2, cex.axis=a.cex.axis)
             axis(2, las=2)
             box()

             # polygon 5-95% for simus SCE 1
             mat.sim1 <- replace(mat.sim1, is.na(mat.sim1),0)
             polygon(c(1:nrow(mat.sim1), rev(1:nrow(mat.sim1))  ),
                  c(apply(mat.sim1, 1, quantile, 0.05, na.rm=TRUE)/a.unit,
                    rev(apply(mat.sim1, 1, quantile, 0.95 , na.rm=TRUE)/  a.unit)) ,
              col=  rgb(0,1,0,0.5), border=FALSE)   # 1=> GREEN

             # polygon 5-95% for simus SCE 2
             mat.sim2 <- replace(mat.sim2, is.na(mat.sim2),0)
             polygon(c(1:nrow(mat.sim2), rev(1:nrow(mat.sim2))  ),
                  c(apply(mat.sim2, 1, quantile, 0.05, na.rm=TRUE)/a.unit,
                    rev(apply(mat.sim2, 1, quantile, 0.95, na.rm=TRUE)/  a.unit)) ,
              col=  rgb(0,0.5,1,0.5), border=FALSE)  # 2=> BLUE
           # add obs. data
             if( plot_obs && seg %in% names(lst_loglike_agg_1[['obs']]) ) {
               lines(1:nrow(mat.sim1), 
               lst_loglike_agg_1[['obs']][  lst_loglike_agg_1[[refsimu1]]$year.month   ,seg]/a.unit,
                 col=1, lwd=2)
             }
           
     # an indicator PER VESSEL of gain compared to baseline  (over the entire period)
     # in terms of landings and in terms of vpuf 
     # i.e. an unique summarizing number per vessel for this scenario        
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="effort"){
        effort_1       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        effort_2       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        effort_mat1    <- mean( effort_1,  na.rm=TRUE)  # baseline
        effort_mat2    <- mean( effort_2,  na.rm=TRUE) 
        gain_effort    <- effort_mat2/effort_mat1 
        gain_effort_per_simu   <- effort_2/effort_1 
        effort_sce         <- effort_mat2
        effort_base        <- effort_mat1
     }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="cumsteaming"){
        seffort1       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        seffort2       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        seffort_mat1    <- mean( seffort1,  na.rm=TRUE)  # baseline
        seffort_mat2    <- mean( seffort2,  na.rm=TRUE) 
        gain_seffort    <- seffort_mat2/seffort_mat1 
        gain_seffort_per_simu   <- seffort2/seffort1 
      }   
   if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fuelcost"){
        fuelcost1       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fuelcost2       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fuelcost_mat1    <- mean( fuelcost1,  na.rm=TRUE)  # baseline
        fuelcost_mat2    <- mean( fuelcost2,  na.rm=TRUE) 
        gain_fuelcost    <- fuelcost_mat2/fuelcost_mat1 
        gain_fuelcost_per_simu   <- fuelcost2/fuelcost1 
      }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="totland"){
        totland1       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        totland2       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        totland_mat1   <- mean( totland1,  na.rm=TRUE)  # baseline
        totland_mat2   <- mean( totland2,  na.rm=TRUE) 
        gain_totland   <- totland_mat2/totland_mat1 
        gain_totland_per_simu   <- totland2/totland1 
    }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="totland_explicit"){
        totlandav1explicit       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        totlandav2explicit       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        totland_av_mat1_explicit <- mean( totlandav1explicit,  na.rm=TRUE)  # baseline
        totland_av_mat2_explicit <- mean( totlandav2explicit,  na.rm=TRUE) 
        gain_totland_explicit    <- totland_av_mat2_explicit/totland_av_mat1_explicit 
        gain_totland_explicit_per_simu   <- totlandav2explicit/totlandav1explicit 
     }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="totland_implicit"){
        totlandav1implicit       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        totlandav2implicit       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        totland_av_mat1_implicit <- mean( totlandav1implicit,  na.rm=TRUE)  # baseline
        totland_av_mat2_implicit <- mean( totlandav2implicit,  na.rm=TRUE) 
        gain_totland_implicit   <- totland_av_mat2_implicit/totland_av_mat1_implicit 
        gain_totland_implicit_per_simu   <- totlandav2implicit/totlandav1implicit 
     }   
        
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="av_vapuf_month"){
        avvapuf1                 <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avvapuf2                 <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_vapuf_mat1            <- mean( avvapuf1,  na.rm=TRUE) # baseline
        av_vapuf_mat2            <-  mean( avvapuf2,  na.rm=TRUE) #
        gain_av_vapuf            <- av_vapuf_mat2/av_vapuf_mat1       
        gain_av_vapuf_per_simu   <- avvapuf2/avvapuf1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="revenue"){
        avrevenue1         <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avrevenue2         <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_revenue_mat1    <- mean( avrevenue1,  na.rm=TRUE) # baseline
        av_revenue_mat2    <- mean( avrevenue2,  na.rm=TRUE) #
        gain_av_revenue   <- av_revenue_mat2/av_revenue_mat1       
        gain_av_revenue_per_simu   <- avrevenue2/avrevenue1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="rev_explicit_from_av_prices"){
        avrevavprices1           <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avrevavprices2           <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_rev_av_prices_mat1    <- mean( avrevavprices1,  na.rm=TRUE) # baseline
        av_rev_av_prices_mat2    <- mean( avrevavprices2,  na.rm=TRUE) #
        gain_av_rev_av_prices    <- av_rev_av_prices_mat2/av_rev_av_prices_mat1       
        gain_av_rev_av_prices_per_simu    <- avrevavprices2/avrevavprices1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="av_trip_duration"){
        tripdur1                       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        tripdur2                       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_trip_duration_mat1          <- mean(tripdur1,  na.rm=TRUE) # baseline
        av_trip_duration_mat2          <- mean(tripdur2,  na.rm=TRUE) #
        gain_av_trip_duration          <- av_trip_duration_mat2/av_trip_duration_mat1       
        gain_av_trip_duration_per_simu <- tripdur1/tripdur2       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="traveled_dist"){
        traveleddistav1         <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        traveleddistav2         <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        traveled_dist_av_mat1   <- mean( traveleddistav1,  na.rm=TRUE) # baseline
        traveled_dist_av_mat2   <- mean( traveleddistav2,  na.rm=TRUE) #
        gain_av_traveled_dist   <- traveled_dist_av_mat2/traveled_dist_av_mat1       
        gain_av_traveled_dist_per_simu   <- traveleddistav2/traveleddistav1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="gav"){
        avgav1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avgav2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_gav_mat1   <- mean( avgav1,  na.rm=TRUE) # baseline
        av_gav_mat2   <- mean( avgav2,  na.rm=TRUE) #
        gain_av_gav   <- av_gav_mat2/av_gav_mat1       
        gain_av_gav_per_simu   <- avgav2/avgav1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="gradva"){
        avgradva1       <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avgradva2       <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_gradva_mat1  <- mean( avgradva1,  na.rm=TRUE) # baseline
        av_gradva_mat2  <-  mean( avgradva2,  na.rm=TRUE) #

       ## CAUTION THE LOG RATIO WHEN POTENTIAL NEGATIVE VALUES MAKES IT TRICKY:      
       if(av_gradva_mat1 >=0 && av_gradva_mat2>=0 && abs(av_gradva_mat1)>= abs(av_gradva_mat2)){
         gain_av_gradva   <- log(av_gradva_mat2/av_gradva_mat1) # this is in the positive interval
        }      
       if(av_gradva_mat1 >=0 && av_gradva_mat2>=0 && abs(av_gradva_mat1)<= abs(av_gradva_mat2)){
         gain_av_gradva   <- log(av_gradva_mat2/av_gradva_mat1) # this is in the positive interval
       }             
       if(av_gradva_mat1 <0 && av_gradva_mat2 <0 && abs(av_gradva_mat1)>= abs(av_gradva_mat2)) {
         gain_av_gradva   <- -log(av_gradva_mat2/av_gradva_mat1) # this is worsening (in the negative interval)
       }
       if(av_gradva_mat1 <0 && av_gradva_mat2 <0 && abs(av_gradva_mat1)<= abs(av_gradva_mat2)) {
         gain_av_gradva   <- log(av_gradva_mat2/av_gradva_mat1) # this is an improvement (in the negative interval)
       }
 
       if(av_gradva_mat1 <0 && av_gradva_mat2 >=0 ) {
         gain_av_gradva   <- log((abs(av_gradva_mat1)+av_gradva_mat2)) # this is an improvement 
       }
           if(av_gradva_mat1 >0 && av_gradva_mat2 <=0 ) {
         gain_av_gradva   <- -log((abs(av_gradva_mat2)+av_gradva_mat1)) # this is an worsening 
       }
            
    
       gain_av_gradva_per_simu         <- rep(0, length(avgradva1))
       idx1                            <- avgradva1 >=0 & avgradva2>=0  
       gain_av_gradva_per_simu[idx1]   <- log(avgradva2[idx1]/avgradva1[idx1])
       idx2                            <- avgradva1 <0 & avgradva2 <0 & abs(avgradva1)> abs(avgradva2)
       gain_av_gradva_per_simu[idx2]   <- -log(avgradva2[idx2]/avgradva1[idx2]) 
       idx3                            <- avgradva1 <0 & avgradva2 <0 & abs(avgradva1)< abs(avgradva2)
       gain_av_gradva_per_simu[idx3]   <- log(avgradva2[idx3]/avgradva1[idx3]) 
       idx4                            <- avgradva1 <0 & avgradva2 >0
       gain_av_gradva_per_simu[idx4]   <- log((abs(avgradva1[idx4])+avgradva2[idx4])/avgradva1[idx4]) 
       idx5                            <- avgradva1 >0 & avgradva2 <0
       gain_av_gradva_per_simu[idx5]   <- -log((abs(avgradva2[idx5])+avgradva1[idx5])/abs(avgradva2[idx5]))
       
       
       
 
     }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="nbtrip"){
        avnbtrip1        <-  apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        avnbtrip2        <-  apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        av_nbtrip_mat1   <-  mean( avnbtrip1,  na.rm=TRUE) # baseline
        av_nbtrip_mat2   <-  mean( avnbtrip2,  na.rm=TRUE) #
        gain_av_nbtrip   <- av_nbtrip_mat2/av_nbtrip_mat1       
        gain_av_nbtrip_per_simu   <- avnbtrip2/avnbtrip1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_all"){
        fcpueall1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpueall2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_all_mat1   <- mean(fcpueall1,  na.rm=TRUE) # baseline
        fcpue_all_mat2   <- mean(fcpueall2,  na.rm=TRUE) #
        gain_fcpue_all   <- fcpue_all_mat2/fcpue_all_mat1       
        gain_fcpue_all_per_simu   <- fcpueall2/fcpueall1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_explicit"){
        fcpueexplicit1         <-  apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpueexplicit2         <-  apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_explicit_mat1    <-  mean( fcpueexplicit1,  na.rm=TRUE) # baseline
        fcpue_explicit_mat2    <-  mean( fcpueexplicit2,  na.rm=TRUE) #
        gain_fcpue_explicit    <-  fcpue_explicit_mat2/fcpue_explicit_mat1       
        gain_fcpue_explicit_per_simu   <- fcpueexplicit2/fcpueexplicit1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_implicit"){
        fcpueimplicit1         <-  apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpueimplicit2         <-  apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_implicit_mat1    <-  mean( fcpueimplicit1,  na.rm=TRUE) # baseline
        fcpue_implicit_mat2    <-  mean( fcpueimplicit2,  na.rm=TRUE) #
        gain_fcpue_implicit    <-  fcpue_implicit_mat2/fcpue_implicit_mat1       
        gain_fcpue_implicit_per_simu   <- fcpueimplicit2/fcpueimplicit1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop0"){
        fcpuepop0_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop0_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop0_mat1    <- mean(fcpuepop0_1,  na.rm=TRUE) # baseline
        fcpue_pop0_mat2    <- mean(fcpuepop0_2,  na.rm=TRUE) #
        gain_fcpue_pop0    <- fcpue_pop0_mat2/fcpue_pop0_mat1       
        gain_fcpue_pop0_per_simu    <- fcpuepop0_2/fcpuepop0_1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop1"){
        fcpuepop1_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop1_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop1_mat1    <- mean(fcpuepop1_1,  na.rm=TRUE) # baseline
        fcpue_pop1_mat2    <- mean(fcpuepop1_2,  na.rm=TRUE) #
        gain_fcpue_pop1    <- fcpue_pop1_mat2/fcpue_pop1_mat1       
        gain_fcpue_pop1_per_simu    <- fcpuepop1_2/fcpuepop1_1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop2"){
        fcpuepop2_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop2_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop2_mat1    <- mean(fcpuepop2_1,  na.rm=TRUE) # baseline
        fcpue_pop2_mat2    <- mean(fcpuepop2_2,  na.rm=TRUE) #
        gain_fcpue_pop2    <- fcpue_pop2_mat2/fcpue_pop2_mat1       
        gain_fcpue_pop2_per_simu    <- fcpuepop2_2/fcpuepop2_1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop3"){
        fcpuepop3_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop3_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop3_mat1    <- mean(fcpuepop3_1,  na.rm=TRUE) # baseline
        fcpue_pop3_mat2    <- mean(fcpuepop3_2,  na.rm=TRUE) #
        gain_fcpue_pop3    <- fcpue_pop3_mat2/fcpue_pop3_mat1       
        gain_fcpue_pop3_per_simu    <- fcpuepop3_2/fcpuepop3_1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop7"){
        fcpuepop7_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop7_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop7_mat1    <- mean(fcpuepop7_1,  na.rm=TRUE) # baseline
        fcpue_pop7_mat2    <- mean(fcpuepop7_2,  na.rm=TRUE) #
        gain_fcpue_pop7    <- fcpue_pop7_mat2/fcpue_pop7_mat1       
        gain_fcpue_pop7_per_simu    <- fcpuepop7_2/fcpuepop7_1       
         }   
     if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop10"){
        fcpuepop10_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop10_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop10_mat1    <- mean(fcpuepop10_1,  na.rm=TRUE) # baseline
        fcpue_pop10_mat2    <- mean(fcpuepop10_2,  na.rm=TRUE) #
        gain_fcpue_pop10    <- fcpue_pop10_mat2/fcpue_pop10_mat1       
        gain_fcpue_pop10_per_simu    <- fcpuepop10_2/fcpuepop10_1       
        }   
   if(what %in% c('per_vessel') &&  what2 %in% c("weight") && seg=="fcpue_pop11"){
        fcpuepop11_1        <- apply(mat.sim1, 2, function(x) sum(x[x!=0]))
        fcpuepop11_2        <- apply(mat.sim2, 2, function(x) sum(x[x!=0]))
        fcpue_pop11_mat1    <- mean(fcpuepop11_1,  na.rm=TRUE) # baseline
        fcpue_pop11_mat2    <- mean(fcpuepop11_2,  na.rm=TRUE) #
        gain_fcpue_pop11    <- fcpue_pop11_mat2/fcpue_pop11_mat1       
        gain_fcpue_pop11_per_simu    <- fcpuepop11_2/fcpuepop11_1       
        }   
             
             
             if(count%%9 ==0){
                 # save the current one
                 dev.off()
                   # to be converted in wmf afterward (because wmf do not handle transparency directly in R)
                 #...and open a new one:
                 outputfile <- file.path(output.folder,
                                        paste("loglike_",what2,"_panel",count,"_",gsub("\\.","",a.comment),'.pdf',sep="" ))
                 pdf(file = outputfile)
                 par(mfrow=c(3,3))
                }
    
       }   # end for column in lst_loglike_agg_1[[1]]
           
       # save the last one
      dev.off()

      # after all....
      # draw a barplot
      graphics.off()
      if(what %in% c("per_pop", "per_vessel", "per_country")){

      outputfile <- file.path(output.folder, paste("loglike_",what2,"_barchart_land_composition_",
                                                gsub("\\.","",a.comment),'.png',sep="" ))
      #pdf(file = outputfile)
       png(filename=file.path(outputfile),
                                   width = 1500, height = 1500, 
                                   units = "px", pointsize = 12,  res=300)

      par(mfrow=c(2,1))
      idx_pop <- grep('pop', colnames(sum_all_months_and_mean_across_runs1))
      idx_col <- intersect(
                   idx_pop,
                   which(sum_all_months_and_mean_across_runs1 < 
                           0.10* max(sum_all_months_and_mean_across_runs1[idx_pop], na.rm=TRUE))
                        )
      idx_col2 <- intersect(
                   idx_pop,
                   which(sum_all_months_and_mean_across_runs1 > 
                           0.10* max(sum_all_months_and_mean_across_runs1[idx_pop], na.rm=TRUE))
                        )
     if(length(idx_col)!=0){
      barplot(t(cbind(sum_all_months_and_mean_across_runs1[,idx_col],
                    sum_all_months_and_mean_across_runs2[,idx_col])/1e6),
                      col = rep(rainbow(24), each=2), density= c(10,100), 
                       axes = TRUE, beside=TRUE, cex.names=1.0,las=2,
                        ylab= "mean over runs of total landings (thousand tons)")       
      barplot(t(cbind(sum_all_months_and_mean_across_runs1[,idx_col2],
                    sum_all_months_and_mean_across_runs2[,idx_col2])/1e6),
                      col = rep(rainbow(24), each=2), density= c(10,100), 
                       axes = TRUE, beside=TRUE, cex.names=1.0,las=2,
                        ylab= "mean over runs of total landings (thousand tons)")       
      }
     dev.off() 
     }
     
     
     
       
 graphics.off()
    
    } else{
    cat(paste("not enough (equivalent) simus for this subset....\n"))
    }
    
                                          
                                          
      
    # write the output for the individual indicator
      if(what %in% c('per_vessel') &&  what2 %in% c("weight")){
         write(c(format(Sys.time(), "%H:%M:%S"), combined_name, lstargs$vid, 
                      effort_base, effort_sce, gain_effort, gain_seffort, 
                      totland_mat1, totland_av_mat1_explicit, 
                      gain_totland, gain_totland_explicit, gain_totland_implicit, gain_av_vapuf, gain_av_revenue, gain_av_rev_av_prices,  
                       gain_av_gav, gain_av_gradva, 
                        gain_fcpue_all, gain_fcpue_explicit,gain_fcpue_implicit,
                        #gain_fcpue_pop3, gain_fcpue_pop7,gain_fcpue_pop10,
                        #gain_fcpue_pop10, gain_fcpue_pop11,
                        gain_fcpue_pop0, gain_fcpue_pop1,gain_fcpue_pop2,gain_fcpue_pop3,
                        gain_av_traveled_dist, gain_av_nbtrip), ncol=28, ## CAUTION NCOL HERE ##
                    file=file.path(lstargs$general$main.path, lstargs$general$namefolderinput, 
                     paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,".txt", sep='')),
                     append = TRUE, sep = " ")

        write.table(cbind.data.frame(time=format(Sys.time(), "%H:%M:%S"), combined_name, lstargs$vid, simu=gsub("effort","", names(gain_effort_per_simu)),
                      effort_base, effort_sce, gain_effort_per_simu, gain_seffort_per_simu, 
                      totland_mat1, totland_av_mat1_explicit, 
                      gain_totland_per_simu, gain_totland_explicit_per_simu, gain_totland_implicit_per_simu,
                        gain_av_vapuf_per_simu, gain_av_revenue_per_simu, gain_av_rev_av_prices_per_simu,  
                       gain_av_gav_per_simu, gain_av_gradva_per_simu,  gain_fuelcost_per_simu, 
                        gain_fcpue_all_per_simu, gain_fcpue_explicit_per_simu,gain_fcpue_implicit_per_simu,
                        #gain_fcpue_pop3_per_simu, gain_fcpue_pop7_per_simu,gain_fcpue_pop10_per_simu,
                        #gain_fcpue_pop10_per_simu, gain_fcpue_pop11_per_simu,
                        gain_fcpue_pop0_per_simu, gain_fcpue_pop1_per_simu,  gain_fcpue_pop2_per_simu,  gain_fcpue_pop3_per_simu,
                         gain_av_traveled_dist_per_simu, gain_av_nbtrip_per_simu),
                    file=file.path(lstargs$general$main.path, lstargs$general$namefolderinput, 
                     paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,"_per_simu.txt", sep='')),
                     append = TRUE, sep = " ", col.names = FALSE, row.names=FALSE, quote=FALSE)

       }
       
            
 return()
}
   
   #--------------
   #--------------
   # calls
   #--------------
   #--------------
if(TRUE){ 
 
 
if(general$namefolderinput=="CelticSea"){


  
  
  
   #!#!#!#!#!#!#!#!#!#
     the_baseline <<- "scesizespectra"
 #!#!#!#!#!#!#!#!#!#

 #!#!#!#!#!#!#!#!#!#
  others_than_baseline  <- general$namefolderoutput[!general$namefolderoutput %in% the_baseline]   ## CAUTION
 #!#!#!#!#!#!#!#!#!#

 if(TRUE) write(c("id","sce","vid", "effort_base", "effort_sce", "gain_effort", "gain_seffort", 
                     "baseline_totland_av",  "baseline_totland_explicit_av","gain_totland","gain_totland_explicit", "gain_totland_implicit",
                      "gain_av_vapuf", "gain_av_revenue", "gain_av_rev_av_prices", "gain_av_gav", "gain_av_gradva",
                      "gain_fcpue_all",  "gain_fcpue_explicit", "gain_fcpue_implicit", "gain_fcpue_pop10", "gain_fcpue_pop11", 
                       "gain_av_traveled_dist", "gain_av_nbtrip"), ncol=26,  ## CAUTION NCOL HERE ##
                   file=file.path(general$main.path, general$namefolderinput, 
                     paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,".txt", sep='')),
                     append = FALSE, sep = " ") # init
 if(TRUE) write(c("id","sce","vid", "simu", "effort_base", "effort_sce", "gain_effort", "gain_seffort", 
                     "baseline_totland_av",  "baseline_totland_explicit_av","gain_totland","gain_totland_explicit", "gain_totland_implicit",
                      "gain_av_vapuf", "gain_av_revenue", "gain_av_rev_av_prices", "gain_av_gav", "gain_av_gradva",  "gain_fuelcost",
                      "gain_fcpue_all",  "gain_fcpue_explicit", "gain_fcpue_implicit", "gain_fcpue_pop10", "gain_fcpue_pop11",
                     "gain_av_traveled_dist", "gain_av_nbtrip"), ncol=28,  ## CAUTION NCOL HERE ##
                   file=file.path(general$main.path, general$namefolderinput, 
                     paste("vid_indicators_gain_in_totland_and_vpuf_",the_baseline,"_per_simu.txt", sep='')),
                     append = FALSE, sep = " ") # init

    # selected vessels
    selected                <- "_selected_set1_"

     filename2 <- file.path(general$main.path, general$namefolderinput, the_baseline, 
                                         paste("loglike_", "simu1", ".dat", sep=''))
     loglike <-read.table(filename2)
     all_vids <- levels(head(loglike[,"V7"]))
    
   
     selected_vessels_set1   <- all_vids
     selected_vessels_set_1 <-   all_vids
 }
 
 


   ## OR PER COUNTRY---------------------
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------------------------------------------\n"))


   
     
     what2 <- "weight"
     lst_loglike_w_agg_all_1 <- get(paste("lst_loglike_agg_",what2, selected, the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", the_baseline, sep=''))
  


     what2 <- "weight"
     lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
     lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
  
     sce1                  <- the_baseline
     sce2                  <- sce
 
     combined_name         <- paste(the_baseline,"_vs_", sce, sep='')
    
   
  if(FALSE){
  # a quick check....
  for (sp in 0:37){
  plot(cumsum(as.numeric(lst_loglike_w_agg_all_1[[1]][, paste('pop.',sp,sep="")])), type="l")
   for(i in 1:50) lines(cumsum(as.numeric(lst_loglike_w_agg_all_1[[i]][, paste('pop.',sp,sep="")])), type="l", col=1)
 #plot(cumsum(as.numeric(lst_loglike_w_agg_den_2[[1]]$pop.11)), type="l")
   for(i in 1:50) lines(cumsum(as.numeric(lst_loglike_w_agg_all_2[[i]][, paste('pop.',sp,sep="")])), type="l", col=2)
    browser()
   }
    plot(cumsum(as.numeric(lst_loglike_w_agg_all_1[[1]][,  "totland_implicit"])), type="l")
   for(i in 1:50) lines(cumsum(as.numeric(lst_loglike_w_agg_all_1[[i]][,"totland_implicit"])), type="l", col=1)
 #plot(cumsum(as.numeric(lst_loglike_w_agg_den_2[[1]]$pop.11)), type="l")
   for(i in 1:50) lines(cumsum(as.numeric(lst_loglike_w_agg_all_2[[i]][, "totland_implicit"])), type="l", col=2)
  
  
  
  }


   
   
    ## PER COUNTRY --- WEIGHT
    compare_sim_sim_landings(
                      lst_loglike_w_agg_all_1,
                      lst_loglike_w_agg_all_2, 
                      idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_all_1)[-length(names(lst_loglike_w_agg_all_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_all_2)),
                      combined_name=combined_name,
                      a.comment="den",
                      what="per_country",
                      what2="weight",
                      a.unit=1,
                      count=0,
                      plot_obs=FALSE,
                      general=general
                      )     

   
   }
   
          
   
  
   ## OR PER VESSELS---------------------
 
 
 
  
  for (sce in others_than_baseline){
     cat (paste(sce, "--------------------------------------------------------------\n"))

  #!#!#!#!#!#!#!#!#!#
  if(general$namefolderinput=="CelticSea"){   the_baseline <<- "scebaseline" }
  #!#!#!#!#!#!#!#!#!#

     what2 <- "weight"
     lst_loglike_w_agg_all_1 <- get(paste("lst_loglike_agg_",what2, selected, the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", the_baseline, sep=''))
     lst_loglike_w_agg_vid_1 <- lapply(lst_loglike_w_agg_vid_1, function(x) x[x$VE_REF %in% selected_vessels_set_1,])   # subset for the relevant vessels (cod fisheries)

     what2 <- "weight"
     lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
     lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
     lst_loglike_w_agg_vid_2 <- lapply(lst_loglike_w_agg_vid_2, function(x) x[x$VE_REF %in% selected_vessels_set_1,])   # subset for the relevant vessels (cod fisheries)

    sce1                  <- the_baseline
    sce2                  <- sce
 
    combined_name         <- paste(the_baseline,"_vs_", sce, sep='')
    
      
    # calls per vessel  (set per_vessel at FALSE if you want to calls per pop instead...)
    par(mfrow=c(3,3))
    count <- 0
    for(vid in unique(lst_loglike_w_agg_vid_1[[1]]$VE_REF)){
        count <- count+1
        cat (paste(vid, "...", count/length(unique(lst_loglike_w_agg_vid_1[[1]]$VE_REF) *100,  "% \n"))  )
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_vid_1, function(x) x[x$VE_REF==vid,]) 
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_this_1, function(x) merge(x, expand.grid(VE_REF=x[1,"VE_REF"], year.month=levels(x$year.month)), all=T) )  # all.combi to fill in the gap
          lst_loglike_w_agg_this_1 <- lapply(lst_loglike_w_agg_this_1, function(x) replace(x, is.na(x), 0) )
 
          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_vid_2, function(x) x[x$VE_REF==vid,]) 
          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_this_2, function(x) merge(x, expand.grid(VE_REF=x[1,"VE_REF"], year.month=levels(x$year.month)), all=T) )  # all.combi to fill in the gap
          lst_loglike_w_agg_this_2 <- lapply(lst_loglike_w_agg_this_2, function(x) replace(x, is.na(x), 0) )
          
           
          
          
          if(length(grep("IRL", vid))!=0 || length(grep("GBR", vid))!=0 || length(grep("NLD", vid))!=0 || length(grep("FRA", vid))!=0 ){
            per_vessel <- TRUE
            per_pop    <- FALSE # per pop per vessel: to fix: imcompatibble with per_vessel
            if(per_vessel) {  # PER VESSEL
              # weight
              compare_sim_sim_landings(
                      lst_loglike_w_agg_this_1,
                      lst_loglike_w_agg_this_2, 
                     idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_all_1)[-length(names(lst_loglike_w_agg_all_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_all_2)),
                           combined_name=combined_name,
                      a.comment=vid,
                      what="per_vessel",
                      what2="weight",
                       plot_obs=TRUE,
                       general=general,
                       vid=vid
                      ) 
             cat (paste('...done', "\n"))  
   
             graphics.off()
             } 
             if(per_pop)  {        # PER POP
              compare_sim_sim_landings(
                      lst_loglike_w_agg_this_1,
                      lst_loglike_w_agg_this_2, 
                    idx.sim=list(idx.sim.1= names(lst_loglike_w_agg_all_1)[-length(names(lst_loglike_w_agg_all_1))],
                                    idx.sim.2= names(lst_loglike_w_agg_all_2)),
                         combined_name=combined_name,
                      a.comment="pop.1",
                      what="per_pop",
                      what2="weight",
                      count=count
                      )   
              }
            }
            
            
            
        
            
            
            
            
       
        }   # end for vid

  
   
  } # end for sce


} # end TRUE





