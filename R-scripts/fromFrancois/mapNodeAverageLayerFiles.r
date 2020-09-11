


#' Generate maps from averaging stochastic DISPLACE spatial layera
#'
#' This function generates maps from an average layer as a second step after call to getAggNodesLayerFiles()
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
#'   # caution: could take a  while...
#'   getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
#'   getAggNodeLayerFiles (general, a_type="cumsweptarea", a_tstep="34321")
#'
#'   library(maptools)
#'   sh_coastlines               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_myfish','graphsspe', 'shp', 'francois_EU'),
#'                                                       proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   ices_areas                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub','DISPLACE_input_raw','ices_areas','ices_areas'),
#'                                                      proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'
#'   NSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub1_mx20_wgs84'),
#'                                                      proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub1mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                     'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub1_Mx_20_wgs84'),
#'                                                        proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   NSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                       'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx_20_wgs84'),
#'                                                        proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx20                  <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                         'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_20LongTailedD_wgs84'),
#'                                                           proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   NSsub4mx5                   <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                          'DISPLACE_SVANAProject', 'Input for DISPLACE', 'NST2_sub4_mx05_wgs84'),
#'                                                            proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'   BSsub4mx5                   <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
#'                                                           'DISPLACE_SVANAProject', 'Input for DISPLACE', 'BHT2_Sub4_Mx_5LgtailedD_wgs84'),
#'                                                              proj4string=CRS("+proj=longlat +ellps=WGS84"))
#'
#'
#'
#'   mapNodeAverageLayerFiles (general, a_type="cumcatches", a_type2="",  field_pos=4,  the_baseline= "svana_baseline",
#'                            selected_scenarios_for_plot=general$namefolderoutput,
#'                            selected_scenarios_for_table=general$namefolderoutput,
#'                            selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
#'                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000),
#'                            the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
#'                            gis_shape=list(svana_baseline=   list(sh_coastlines), # ices_areas),
#'                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20),
#'                                           svana_sub4mx5ns20bt=   list(NSsub4mx5, BSsub4mx20),
#'                                           svana_sub4mx20ns5bt=   list(NSsub4mx20, BSsub4mx5),
#'                                           svana_sub4mx5ns5bt=    list(NSsub4mx5, BSsub4mx5)),
#'                                           a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17,
#'                                           legend_text1="Total Catches kg per "
#'                                           )
#'
#'
#'   mapNodeAverageLayerFiles (general, a_type="cumcatches", a_type2="cumsweptarea",  field_pos=4,  the_baseline= "svana_baseline",
#'                            selected_scenarios_for_plot=general$namefolderoutput,
#'                            selected_scenarios_for_table=general$namefolderoutput,
#'                            selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
#'                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000),
#'                           the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
#'                            gis_shape=list(svana_baseline=   list(sh_coastlines), # ices_areas),
#'                                           svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                           svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20),
#'                                           svana_sub4mx5ns20bt=   list(NSsub4mx5, BSsub4mx20),
#'                                           svana_sub4mx20ns5bt=   list(NSsub4mx20, BSsub4mx5),
#'                                           svana_sub4mx5ns5bt=    list(NSsub4mx5, BSsub4mx5)),
#'                                           a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17,
#'                                           legend_text1="Total Catches kg per Swept Area km2 per "
#'                                           )
#'
#'
#'   mapNodeAverageLayerFiles (general, a_type="cumdiscards",  a_type2="cumcatches", func="rate", field_pos=4,  the_baseline= "svana_baseline",
#'                           selected_scenarios_for_plot=general$namefolderoutput,
#'                           selected_scenarios_for_table=general$namefolderoutput,
#'                           selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
#'                           the_breaks_baseline= c(round((exp(seq(0.00, 0.6, by=0.04))-1),3), 1.1),
#'                           the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
#'                           gis_shape=list(svana_baseline=   list(sh_coastlines), # ices_areas),
#'                                          svana_sub1mx20=   list(NSsub1mx20, BSsub1mx20),
#'                                          svana_sub4mx20=   list(NSsub4mx20, BSsub4mx20),
#'                                          svana_sub4mx5ns20bt=   list(NSsub4mx5, BSsub4mx20),
#'                                          svana_sub4mx20ns5bt=   list(NSsub4mx20, BSsub4mx5),
#'                                          svana_sub4mx5ns5bt=    list(NSsub4mx5, BSsub4mx5)),
#'                                          a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17,
#'                                          legend_text1="Discarded proportion"
#'                                          )
#' }







mapNodeAverageLayerFiles <- function(general, in_relative=FALSE, a_type="cumcatches", a_type2="", func="ratio",    # or func="rate",
                            field_pos=4, a_pop="", the_baseline= "svana_baseline",
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                            namesce=general$namefolderoutput,
                            selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.2))), 1000000),
                            the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
                            gis_shape=list(),
                            a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), xcell=12, ycell=17,
                            legend_text1="Total Catches kg per "
                            ){


 distance <- function (lon, lat, lonRef, latRef)  # vmstools::distance()
{
    pd <- pi/180
    a1 <- sin(((latRef - lat) * pd)/2)
    a2 <- cos(lat * pd)
    a3 <- cos(latRef * pd)
    a4 <- sin(((lonRef - lon) * pd)/2)
    a <- a1 * a1 + a2 * a3 * a4 * a4
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(6371 * c)
}

    legend.gradient2 <-
function (pnts, cols = heat.colors(100), limits = c(0, 1), title = "Legend", legend="",
    ...)
{
    pnts = try(as.matrix(pnts), silent = T)
    if (!is.matrix(pnts))
        stop("you must have a 4x2 matrix")
    if (dim(pnts)[1] != 4 || dim(pnts)[2] != 2)
        stop("Matrix must have dimensions of 4 rows and 2 columms")
    if (length(cols) < 2)
        stop("You must have 2 or more colors")
    yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(cols) +
        1)
    for (i in 1:length(cols)) {
        polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i +
            1], yvals[i + 1]), col = cols[i], border = F)
    }
    text(max(pnts[, 1]), min(pnts[, 2]), labels = limits[1],
        pos = 4, ...)
    text(max(pnts[, 1]), max(pnts[, 2]), labels = limits[2],
        pos = 4, ...)
    start_pos <- (min(pnts[, 2])+((max(pnts[, 2])-min(pnts[, 2]))/length(legend))/2)
    for (i in 1: length(legend)){
    text(max(pnts[, 1])-0, start_pos + ((i-1) * ((max(pnts[, 2])-min(pnts[, 2]))/length(legend)) ), labels = legend[i],
        pos = 4, ...)
    #browser()
    }
    text(min(pnts[, 1])-0.1, max(pnts[, 2])-0, labels = title, adj = c(0,
        -1), ...)
}


# export raster file for GIS engine
 exportGTiff <- function(
                           a_raster=rst,
                           namefile_gtiff= file.path(general$main.path, general$namefolderinput, namefile, paste0("map_averaged_",nametype,"_", plotid)),
                           a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
                            ) {
           require(raster)
           crs(a_raster) <- "+proj=longlat +datum=WGS84"
           rstr_proj       <- projectRaster(a_raster, crs=a_crs)  # e.g. European EEA projection
           rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
           #rstr_proj[rstr_proj<0.001]  <- -999
           # SHUT DOWN THE R CONSOLE IF A ERROR POPPING UP HERE:
           writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)
  return()
  }



   library(maptools)



   table_obj <- matrix(0, nrow=length(selected_scenarios_for_table), ncol=length(selected_areas_for_table)+1)
   rownames(table_obj) <- c(selected_scenarios_for_table)
   colnames(table_obj) <- c(selected_areas_for_table, "Other")



    if(a_type2!="") nametype <- paste0(paste0(a_type, a_pop),"over",a_type2) else nametype <- paste0(a_type, a_pop)
    namefile  <- file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_selected_in_relative", in_relative, ".tiff") )
    namefile2 <- file.path(general$main.path, general$namefolderinput, paste0("table_",nametype,".txt") )


    plotid <- 0
    tiff(filename=namefile,   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))

    if(length(selected_scenarios_for_plot)==3) m <- rbind(c(1, 1), c(1, 1),c(2, 3))
    if(length(selected_scenarios_for_plot)==5) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5))
    if(length(selected_scenarios_for_plot)==7) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5),  c(6, 7))
    if(length(selected_scenarios_for_plot)==2) m <- rbind(c(1, 2))
    if(length(selected_scenarios_for_plot)==4) m <- rbind(c(1, 2), c(3,4))
    if(length(selected_scenarios_for_plot)==6) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6))
    if(length(selected_scenarios_for_plot)==8) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6), c(7, 8))
    layout(m)
    par(mar=c(2,2,3,1))
    par(oma=c(4,4,1,1))
    #table_obj <- NULL

   count <-0
   for(sce in   selected_scenarios_for_table)
     {
     count <- count+1

     plotid <- plotid +1

    this <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
    colnames(this) <- c("node","lat",  "long")
    colnames(this) [field_pos] <- paste0(a_type, a_pop)
    nametype <- paste0(a_type, a_pop)

    # filter out close to 0 values
    this[,nametype]  <- replace(this[,nametype], this[,nametype]<1e-1, 0)

    if(a_type2!=""){
       this  <- replace(this, is.na(this), 0)
       this[,a_type]  <- replace(this[,a_type], is.infinite(this[,a_type]), 0)
       this2 <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",a_type2,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
       colnames(this2) <- c("node","lat",  "long")
       colnames(this2) [field_pos] <- a_type2
       this2  <- replace(this2, is.na(this2), 0)
       this2[,a_type2]  <- replace(this2[,a_type2], is.infinite(this2[,a_type2]), 0)

       # filter out close to 0 values
       this2[,a_type2] <- replace(this2[,a_type2], this2[,a_type2]<1e-1, 0)

       this <- merge(this, this2)
       if(func=="ratio") this[,paste0(nametype,"over",a_type2)] <- this [,nametype]/this [,a_type2]  # assuming a ratio
       if(func=="rate") this[,paste0(nametype,"over",a_type2)] <- (this [,nametype])/(this [,nametype]+this [,a_type2])  # assuming a rate
       nametype <- paste0(paste0(nametype,a_pop),"over",a_type2) # rename
    }



    this_for_gis <- this
    this_for_gis[,4] <- ceiling(this_for_gis[,4]) # because weird bug when importing XY data in GIS if not an integer!!!
    write.table(this_for_gis, file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",nametype,"_layer_",sce,".txt", sep='')), col.names=TRUE, row.names=FALSE)


     # get an idea per area
     er <- require(vmstools)
     if(!er){
        this$SI_LATI <- this$lat
        this$SI_LONG <- this$long
        data(ICESareas)
        this$area <- ICESarea(this, ICESareas, fast=TRUE)
        this$area <- factor(this$area)
        levels(this$area)[! levels(this$area) %in% selected_areas_for_table] <- "Other"
     } else{
        if (is.null(this$area)) {
           this$area <- NA
           warning("No area code found here. Try to install vmstools if within ICES area and re-run, otherwise add an area field by hand to the input file", call. = FALSE)
        }
     }
     table_obj[sce, ] <-  tapply(this [, nametype], this$area, sum, na.rm=TRUE)[colnames(table_obj)]




     this$round_long <- round(this$long*xcell)  # 15
     this$round_lat  <- round(this$lat*ycell)   # 20

     # find out the grid res
     lo <- sort(this$round_long, decreasing=TRUE)
     la <- sort(this$round_lat, decreasing=TRUE)
     most_freq_in_long <- as.numeric(names(sort(table(diff(lo/xcell)), decreasing=TRUE)[2]))
     most_freq_in_lat  <- as.numeric(names(sort(table(diff(la/ycell)), decreasing=TRUE)[2]))
     xcellkm <- distance(this$round_long[1]/xcell, mean(this$round_lat)/ycell, (this$round_long[1]/xcell) + most_freq_in_long, mean(this$round_lat)/ycell)
     ycellkm <- distance(mean(this$round_long)/xcell, this$round_lat[2]/ycell , mean(this$round_long)/xcell, (this$round_lat[2]/ycell) + most_freq_in_lat)

     if(func!="rate") this[,nametype]  <- round(this[,nametype])  /(xcellkm * ycellkm) # (5.576564*6.540486)  # if 15 and 20 then divide by cell area 8.925*5.561km  check??

     this$cell_id <-  paste(this$round_long, this$round_lat, sep="_")
     if(sce == the_baseline) {
        the_baseline_layer <- this
        the_baseline_layer <- aggregate(the_baseline_layer[,nametype],
                                list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id), sum, na.rm=TRUE)
        colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", nametype)


       Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
       #the_breaks_baseline <-   c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000)

       the_baseline_layer[,nametype] <- replace (the_baseline_layer[,nametype],
                                                 the_baseline_layer[,nametype]>the_breaks_baseline[length(the_breaks_baseline)],
                                                 the_breaks_baseline[length(the_breaks_baseline)])

       the_points <- tapply(the_baseline_layer[,nametype],
                  list(the_baseline_layer$round_lat, the_baseline_layer$round_long), sum, na.rm=TRUE)

       #the_points <- replace (the_points, the_points>the_breaks_baseline[length(the_breaks_baseline)], the_breaks_baseline[length(the_breaks_baseline)])

      image(
        x=as.numeric(as.character(colnames(the_points)))/xcell,     # 8.925km  at 53 degree N
        y=as.numeric(as.character(rownames(the_points)))/ycell,     # 5.561km at 53 degree N
        z= t(the_points),  # convert in tons
        breaks=c(the_breaks_baseline),
        col =  Satellite.Palette.baseline(length(the_breaks_baseline[-1]))  ,
        useRaster=FALSE,
        xlab="",
        ylab="",
        axes=FALSE,
        xlim=xlims, ylim=ylims
        )
       library(maps)
       if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[the_baseline]])) plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=TRUE)
       #text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")

     xrange <- range(the_baseline_layer$round_long/xcell)
     yrange <- range(the_baseline_layer$round_lat/ycell)
     r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(abs(most_freq_in_long), abs(most_freq_in_lat)), crs=CRS("+proj=longlat +datum=WGS84"))
     some_coords <- SpatialPoints(cbind(lon=the_baseline_layer$round_long/xcell, lat=the_baseline_layer$round_lat/ycell))
     rstr        <- rasterize(x=some_coords, y=r, field=the_baseline_layer[,nametype], fun="sum")
     #plot(rstr, breaks=the_breaks_baseline, col =  Satellite.Palette.baseline(length(the_breaks_baseline[-1])) )


 #    exportGTiff(
 #                  a_raster= rstr,
 #                  namefile_gtiff= file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce)),
 #                  a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
 #                  )

 # replace by: force shape classification
 require(raster)
           crs(rstr) <- "+proj=longlat +datum=WGS84"
           a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
           rstr_proj       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
           rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
           #rstr_proj[rstr_proj<0.001]  <- -999
       rstr_proj[rstr_proj< the_breaks_baseline[1]] <- the_breaks_baseline[1]
     for(int in 1: length(the_breaks_baseline[-1])) {
            rstr_proj[rstr_proj>the_breaks_baseline[int] & rstr_proj<the_breaks_baseline[int+1]]  <- the_breaks_baseline[int+1]
     }
      namefile_gtiff= file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce))
      writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)



 # make a shape with symbology?
 # addColorTable <- function(inRstName, outRstName, rat.df){
 #      library(rgdal)
 #      r<- readGDAL(inRstName)
 #      rat.df$color<- as.character(rat.df$color)
 #      rat.df$attribute<- as.character(rat.df$attribute)
 #      outRst <- writeGDAL(r, outRstName, type="Byte",
 #      colorTable=list(rat.df$color),
 #      catNames=list(rat.df$attribute), mvFlag=11L)
 #      return(raster(outRst))
 #    }
 #    # This defines the values, the color and the attribute
 #  valT <- the_breaks_baseline
 #  colT <-  Satellite.Palette.baseline(length(the_breaks_baseline))
 #  attT <- the_breaks_baseline
 #  rat.df <- data.frame(value=valT,color=colT,attribute=attT)
 #
 #  # apply the magic function
 #  rnew <- addColorTable(inRstName=file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce, ".tif")),
 #                       outRstName=file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce, "_sym.tif")),
 #                            rat.df)

    box()
    mtext(side=3, namesce[count], cex=1.2, line=0.5)
    axis(1, cex.axis=1.2)
    axis(2, las=2, cex.axis=1.2)


    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+3, ylims[1]+3, ylims[1]+0.5)
    the_breaks_leg <-NULL
    a_title <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1=legend_text1))
    if(func=="rate") a_title <- legend_text1  # overwrite
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i])} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
       legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
         limits="", title=eval(a_title),
         legend= the_breaks_leg,
         cex=1, col="black")


        }   else{



      this <- aggregate(this[,nametype], list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", nametype)

      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")

      # filter for close to 0 values
      this[,paste0(nametype,".x")] <- replace(this[,paste0(nametype,".x")], this[,paste0(nametype,".x")]<1e-1, 0)
      this[,paste0(nametype,".y")] <- replace(this[,paste0(nametype,".y")], this[,paste0(nametype,".y")]<1e-1, 0)

      # percent
      this[,nametype]  <- (100* as.numeric(as.character(this[,paste0(nametype,".y")])) / as.numeric(as.character(this[,paste0(nametype,".x")])) )  -100


      # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
      this[,nametype] [ this[,paste0(nametype,".x")] <quantile(this[,paste0(nametype,".x")] [ this[,paste0(nametype,".x")] !=0], prob=0.05)]  <- 0


    if(in_relative){
        the_points <- tapply( this[,nametype],
                  list(this$round_lat.y, this$round_long.y), sum)
        Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))
        } else{
        the_points <- tapply(this[,paste0(nametype,".y")],
                  list(this$round_lat.y, this$round_long.y), sum)
        the_breaks <-  the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline

        }


    the_points <- replace (the_points, the_points>the_breaks[length(the_breaks)], the_breaks[length(the_breaks)])


     # in ?
     sum(as.numeric(as.character(the_points)), na.rm=TRUE)


  if(sce %in% selected_scenarios_for_plot){
    image(
     x=as.numeric(as.character(colnames(the_points)))/xcell,   #15
     y=as.numeric(as.character(rownames(the_points)))/ycell,   # 20
     z= t(the_points),  # convert in tons
     breaks=c(the_breaks),
     col = Satellite.Palette(length(the_breaks[-1])),
     useRaster=FALSE,
     xlab="",
     ylab="",
     axes=FALSE,
     xlim=xlims, ylim=ylims
     )
    library(maps)
     if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[the_baseline]])) plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=FALSE)
    #text(coordinates(ices_areas), labels=ices_areas$ICES_area, cex=1.4, col="black")


     #xrange <- range(this$round_long.y/xcell)
     #yrange <- range(this$round_lat.y/ycell)
     r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(abs(most_freq_in_long), abs(most_freq_in_lat)), crs=CRS("+proj=longlat +datum=WGS84"))
     some_coords <- SpatialPoints(cbind(lon=this$round_long.x/xcell, lat=this$round_lat.y/ycell))
     rstr        <- rasterize(x=some_coords, y=r, field=this[,nametype], fun="sum")
     exportGTiff(
                   a_raster= rstr,
                   namefile_gtiff= file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce)),
                    a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
                   )



    box()
    mtext(side=3, namesce[count], cex=1.2, line=0.5)
    axis(1, cex.axis=1.2)
    axis(2, las=2, cex.axis=1.2)


    x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
    y = c(ylims[1]+0.5, ylims[1]+3, ylims[1]+3, ylims[1]+0.5)
    if(in_relative) a_title_leg <- substitute( expression(paste("% difference per cell")))
    if(!in_relative) a_title_leg <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1=legend_text1))
    the_breaks_leg <-NULL
    #for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
    the_breaks_leg <- the_breaks
    legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
    limits="", title=eval(a_title_leg),
    legend= the_breaks_leg,
     cex=1.0, col="black")


    # add closure polygons:
     if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[sce]])) plot(gis_shape[[sce]][[i]], add=TRUE,  border=grey(0.2), col=NA)




     } # end selected sce for plot
    } # end  Baseline
 } # end sce

  mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Longitude",line=2, cex=1.5, outer=TRUE)

dev.off()


table_obj <- cbind(table_obj, Total= apply(table_obj, 1, sum, na.rm=TRUE) ) # marginal value

table_obj_relative_to_baseline <- cbind(round(sweep(table_obj, 2, table_obj[1,], FUN="/")*100, 1)- 100)
table_obj_relative_to_baseline[1,] <- table_obj[1,]
write.table(table_obj_relative_to_baseline,   file=namefile2, col.names=TRUE, row.names=TRUE, sep=";", quote=FALSE)
print(namefile2)

# useful to copy/paste into Excel!
write.table(table_obj_relative_to_baseline, "clipboard", sep="\t", row.names=TRUE)   # export to excel

# check in absolute numbers:
# sum(table_obj_relative_to_baseline["svana_sub1mx20",]/100*table_obj_relative_to_baseline["svana_baseline",])



 return(table_obj_relative_to_baseline)
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

   library(maptools)
   bbox               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_CelticSea','MANAGEMENT', 'shapes', 'bounding_box_proj'),
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   summary(bbox)

   sh_coastlines               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_AdriaticSea','MANAGEMENT','francois_EU'),
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))

   #pomoC                      <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
   #                                                        'DISPLACE_input_gis_AdriaticSea','MANAGEMENT','Pomo closure', 'Pomo closure', 'Pomo Zone C'),
   #                                                           proj4string=CRS("+proj=longlat +ellps=WGS84"))



   #!#!#!#!#!#
   #!#!#!#!#!#
   #!#!#!#!#!#
   # CAUTION, THE BASELINE IS ACTUALLY:
   the_baseline <- "scesizespectra"
   #!#!#!#!#!#
   #!#!#!#!#!#
   #!#!#!#!#!#
   library(raster)
   mapNodeAverageLayerFiles (general, in_relative=TRUE, a_type="cumftime", a_type2="",  field_pos=4,  the_baseline= the_baseline,
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                            namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                            selected_areas_for_table=c("27.7.g"),
                            the_breaks_baseline=  c(0.5, 1, round(exp(seq(0.5, 4, by=0.5))), 10000),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                            gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                           a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                           legend_text1="Fishing hours per "
                         )


   mapNodeAverageLayerFiles (general, in_relative=FALSE, a_type="cumcatches", a_type2="",  field_pos=4,  the_baseline= the_baseline,
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                             namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                           selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                            the_breaks_baseline=  c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                              gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                           a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                              legend_text1="Total Catches kg per "
                                           )

  mapNodeAverageLayerFiles (general, in_relative=FALSE, a_type="cumdiscards", a_type2="",  field_pos=4,  the_baseline= the_baseline,
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                             namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                           selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                            the_breaks_baseline=  c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                              gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                           a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                              legend_text1="Total Discards kg per "
                                           )




   mapNodeAverageLayerFiles (general,  in_relative=FALSE, a_type="cumcatches", a_type2="cumsweptarea",   func="ratio", field_pos=4,  the_baseline= the_baseline,
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                                 namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                         selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                             the_breaks_baseline=  c(0.5, 1, round(exp(seq(0.5, 4, by=0.5))), 10000),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                              gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                          a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                           legend_text1="Total Catches kg per Swept Area km2 per "
                                           )


   mapNodeAverageLayerFiles (general,  in_relative=FALSE, a_type="cumdiscards",  a_type2="cumcatches", func="rate", field_pos=4,  the_baseline= the_baseline,
                           selected_scenarios_for_plot=general$namefolderoutput,
                           selected_scenarios_for_table=general$namefolderoutput,
                                 namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                        selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                            #the_breaks_baseline=  c(sqrt(seq(0.001, 0.05, by=0.005)),1),
                             the_breaks_baseline= c(sqrt(seq(0.01, 0.2, by=0.02)),1),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                              gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                            a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                          legend_text1="Discarded proportion"
                                          )



  # pop0 to ...

  for (a_pop in  c(0,  2,  5,  7,  8,  9, 11, 12, 13, 14, 15, 17, 19, 20, 21, 22, 23, 24, 25))
   {

   mapNodeAverageLayerFiles (general,  in_relative=FALSE, a_type="cumulcatches_per_pop", a_pop=paste0("_pop",a_pop), a_type2="",  field_pos=4,  the_baseline= the_baseline,
                            selected_scenarios_for_plot=general$namefolderoutput,
                            selected_scenarios_for_table=general$namefolderoutput,
                                 namesce=c("Size spectra Baseline",
                          " - Predation",
                          " - Predation + Avoiding choke spp.",
                          "+ Avoidance",
                          "+ Stop if choked",
                          "+ Avoidance + Stop if choked",
                          "+ Avoid High Tariffs",
                          "+ Focus on High Tariffs"
                          ),
                           selected_areas_for_table=c("22",    "23",    "24",    "25",    "IIIa",  "IVa",   "IVb",   "IVc"),
                             the_breaks_baseline=  c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                            gis_shape=list(scesizespectra = list(sh_coastlines),
                                 scebaseline = list(sh_coastlines),
                                 sceavchok = list(sh_coastlines),
                                 sceavchokpszpectra = list(sh_coastlines),
                                 scesizespectrastopifchok = list(sh_coastlines),
                                 sceavchokpszpctrastopifchok = list(sh_coastlines),
                                 sceavhtariffspszpctratariffs = list(sh_coastlines),
                                 scetrgthtariffspszpctratariffs = list(sh_coastlines)
                                            ),
                                           a_width= 2400, a_height =4500, xlims = c(-12, -5.0), ylims=c(48,53.5),    xcell=10, ycell=15,
                                              legend_text1="Total Catches kg per "
                                           )

  }

