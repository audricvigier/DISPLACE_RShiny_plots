

  general <- list()
  general$case_study <- "BalticSea"

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

 if(general$case_study=="BalticSea"){
    general$igraph            <- 100
    general$a.year            <- "2016"
    general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
    general$nbpops            <- 37
    general$nbszgroup         <- 14
    general$namefolderinput   <- "BalticSea"
    general$use_sqlite        <- FALSE

    general$namefolderoutput   <- c(
                                 "scebaseline",
                                 "scerestrictionsonnets",
                                 "scerestrictionontrawling1eez",
                                 "scerestrictionontrawling5eez",
                                 "scerestrictionontrawling10eez",
                                 "scerestrictionontrawling15eez",
                                 "scerestrictionontrawling20eez",
                                 "scerestrictionontrawling25eez",
                                 "scerestrictionontrawling30eez",
                                 "scerestrictionontrawling50eez",
                                 "scerestrictionontrawling30eezH",                                
                                 "scerestrictionontrawling50eezH",                                
                                 "scerestrictionontrawling10eez10lesstrip",                                
                                 #"scerestrictionontrawling20eez20lesstrip",                                
                                 #"scerestrictionontrawling30eez30lesstrip",                                
                                 #"scerestrictionontrawling1hab",
                                 #"scerestrictionontrawling5hab",
                                 #"scerestrictionontrawling10hab",
                                 #"scerestrictionontrawling15hab",
                                 #"scerestrictionontrawling20hab",
                                 #"scerestrictionontrawling25hab",
                                 "scerestrictionontrawling30hab",
                                 #"scerestrictionontrawling50hab"
                                 #"scerestrictionsonnetsandtrawl15hab",
                                 #"scerestrictionsonnetsandtrawl15eez",
                                 #"scerestrictionsonnetsandtrawl20hab",
                                 #"scerestrictionsonnetsandtrawl20eez",
                                 #"scerestrictionsonnetsandtrawl25eez",
                                 #"scerestrictionsonnetsandtrawl25hab",
                                 "scerestrictionsonnetsandtrawl30eez"
                                 #"scerestrictionsonnetsandtrawl30hab"
                                 )
     general$namesimu           <- list(
                                 "scebaseline"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnets"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling1eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling5eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling15eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling25eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eezH"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50eezH"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10eez10lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20eez20lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eez30lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling1hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling5hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling15hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling25hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl15hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl15eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl20hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl20eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl25eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl25hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl30eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl30hab"=   paste("simu", c(1:10), sep='')
                                 )
     the_scenarios1 <-  c("Baseline",
                          "Restriction on Nets",
                          "1% EEZ cut on trawl",
                          "5% EEZ",
                          "10% EEZ",
                          "15% EEZ",
                          "20% EEZ",
                          "25% EEZ",
                          "30% EEZ",
                          "50% EEZ",
                          "30% EEZ on High effort",
                          "50% EEZ on High effort",
                          "10% EEZ and 10% less trip",
                          "20% EEZ and 20% less trip",
                          "30% EEZ and 30% less trip",
                          "1% HAB cut on trawl",
                          "5% HAB",
                          "10% HAB",
                          "15% HAB",
                          "20% HAB",
                          "25% HAB",
                          "30% HAB",
                          "50% HAB",
                          "Nets restriction & Trawling 15% HAB",
                          "Nets restriction & Trawling 15% EEZ",
                          "Nets restriction & Trawling 20% HAB",
                          "Nets restriction & Trawling 20% EEZ",
                          "Nets restriction & Trawling 25% EEZ",
                          "Nets restriction & Trawling 25% HAB",
                          "Nets restriction & Trawling 30% EEZ",
                          "Nets restriction & Trawling 30% HAB"
                          )


    general$igraph   <- c(
                                 "scebaseline"=100,
                                 "scerestrictionsonnets"=101,
                                 "scerestrictionontrawling5eez"=102,
                                 "scerestrictionontrawling10eez"=103,
                                 "scerestrictionontrawling15eez"=104,
                                 "scerestrictionontrawling20eez"=105,
                                 "scerestrictionontrawling5hab"=106,
                                 "scerestrictionontrawling10hab"=107,
                                 "scerestrictionontrawling15hab"=108,
                                 "scerestrictionontrawling20hab"=109,
                                 #"scerestrictionsonnetsandtrawl15hab"=112,
                                 #"scerestrictionsonnetsandtrawl15eez"=113,
                                 #"scerestrictionsonnetsandtrawl20hab"=110,
                                 #"scerestrictionsonnetsandtrawl20eez"=111,
                                 "scerestrictionontrawling25eez"=114,
                                 "scerestrictionontrawling25hab"=115,
                                 "scerestrictionontrawling30eez"=116,
                                 "scerestrictionontrawling30hab"=117,
                                 "scerestrictionsonnetsandtrawl25eez"=118,
                                 "scerestrictionsonnetsandtrawl25hab"=119,
                                 "scerestrictionsonnetsandtrawl30eez"=120,
                                 "scerestrictionsonnetsandtrawl30hab"=121,
                                 "scerestrictionontrawling50eez"=122,
                                 "scerestrictionontrawling50hab"=123,
                                 "scerestrictionontrawling1eez"=124,
                                 "scerestrictionontrawling1hab"=125,
                                 "scerestrictionontrawling30eezH"=126,
                                 "scerestrictionontrawling50eezH"=127,
                                 "scerestrictionontrawling10eez10lesstrip"=103,                                
                                 "scerestrictionontrawling20eez20lesstrip"=105,                                
                                 "scerestrictionontrawling30eez30lesstrip"=116
                                 )
 

   }





    if(general$case_study=="BalticSea"){
        implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
        explicit_pops <- c(0:36)[-(implicit_pops+1)] 
    }


 selected_scenarios <-  c("scebaseline",
                           "scerestrictionsonnets",
                           "scerestrictionontrawling1eez",
                           "scerestrictionontrawling5eez",
                           "scerestrictionontrawling10eez",
                           "scerestrictionontrawling15eez",
                           "scerestrictionontrawling20eez",
                           "scerestrictionontrawling25eez",
                           "scerestrictionontrawling30eez",
                           "scerestrictionontrawling50eez",
                           "scerestrictionontrawling30eezH",
                           "scerestrictionontrawling50eezH",
                           "scerestrictionontrawling10eez10lesstrip",
                           "scerestrictionontrawling30hab",
                           "scerestrictionsonnetsandtrawl30eez"
                           )
                                 
labels_selected_scenarios <- c("Baseline",
                           "Restrictions on Nets",
                           "Ban of 1% EEZ",
                           "Ban of 5% EEZ",
                           "Ban of 10% EEZ",
                           "Ban of 15% EEZ",
                           "Ban of 20% EEZ",
                           "Ban of 25% EEZ",
                           "Ban of 30% EEZ",
                           "Ban of 50% EEZ",
                           "Ban of 30% EEZ core",
                           "Ban of 50% EEZ core",
                           "Ban of 10% EEZ + 10% less trip",
                           "Ban of 30% HAB",
                           "Ban of 30% EEZ + restr.nets"
                           )                                 
                                 


    # look at annual indics such as the TACs...
    res <- NULL
    for(sce in selected_scenarios) {
            print(paste("sce ", sce))
       for(simu in general$namesimu[[sce]]) {
            print(paste("sim ", simu))
         # merge all infos
               annual_indics              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popdyn_annual_indic_',  simu,".dat", sep='')))
               colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))

               annual_indics <- annual_indics [, 1:9]   # FOR NOW...
             res <- rbind (res, cbind(annual_indics, sce=sce, simu=paste(simu, sep="_")))
       }
    }
    
  
  
   outcome_firsty <- res[res$tstep==8761,]  
   outcome_lasty <- res[res$tstep==35065,]  
   outcome <- merge(outcome_firsty, outcome_lasty, by.x=c('stk', 'sce', 'simu'), by.y=c('stk', 'sce', 'simu'))
   outcome$"FFinit" <- outcome$Fbar.y/outcome$Fbar.x
   outcome$"SSBSSBinit" <- outcome$SSB_kg.y/outcome$SSB_kg.x
   
   

   outcome$sce <- factor(outcome$sce)
   outcome$sce <- factor(outcome$sce, levels=selected_scenarios, labels=  labels_selected_scenarios)


   # put in long format
   df1 <- cbind.data.frame(outcome[,c('stk','sce','simu','FFinit')], var="FFinit")
   df2 <- cbind.data.frame(outcome[,c('stk','sce','simu','SSBSSBinit')] , var="SSBSSBinit")
   colnames(df1) <- colnames(df2) <- c('stk','sce','simu','value','var')
   out <- rbind.data.frame(df1,df2)
   


 # SSB, F and whatever 
 the_dim        <- c(2400, 2400)
 namefile       <- paste0("responsecurves_bio_laty_",selected)
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 
 library(ggplot2)
   p <- ggplot(out[out$stk==2,], aes(x=sce, y=value, color=var))  + geom_boxplot(outlier.shape=NA)  +
             labs(x = "Scenario", y = "Value")  + facet_wrap( ~ stk, ncol=1, scales="fixed")     + ylim(0, 1)
 print(
       p   + 
       theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  + geom_boxplot(outlier.shape=NA)
       )

 
dev.off()

# if necessary to add a second y-axis, play with the below:

# adding the relative humidity data, transformed to match roughly the range of the temperature
#  p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))
  
#  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#  # and, very important, reverting the above transformation
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

 
 
 
 
 
 

  # for a traffic lights table
 #ggplot_build(p)$dat
 
 head(dat)
 dd<- aggregate(outcome_lasty[outcome_lasty$var=="gradva","value"], 
               by=list(
                        #area=outcome_lasty[outcome_lasty$var=="gradva","ICES_area"], 
                       sce=outcome_lasty[outcome_lasty$var=="gradva","sce"]),
                  mean, na.rm=TRUE)
 head(dd)
