
                                                                                     
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

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!AGGREGATE (UTILS)!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   c.listquote <- function (...)
   {
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



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!POPDYN.DAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
                                                                 
  
 if(.Platform$OS.type == "unix") {
  ## GENERATE THE LST_POPDYN OBJETS... (USUALLY DONE ON THE HPC...)
  lst_popdyn <- list()
  for (sce in general$namefolderoutput){
    print(sce)
    for (sim in general$namesimu[[sce]]){
   


  ## read the 'popdyn' output (N in thousand individuals here)
  er <- try(read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                      paste("popdyn_", sim, ".dat", sep=''))), silent=TRUE)
  if(class(er)!="try-error"){
    lst_popdyn[[sim]] <- er
    colnames (lst_popdyn[[sim]]) <- c('tstep', 'pop', paste('N_at_szgroup.',0:(general$nbszgroup-1), sep=''))
    }

 
    } # end for sim
    assign(paste("lst_popdyn_", sce, sep=''), lst_popdyn)
  
   # save for later use....
   save(list=c(paste("lst_popdyn_", sce, sep='')),
          file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_popdyn_", sce,".RData", sep='') )  )

  } # end for sce

}                      

 if(.Platform$OS.type != "unix") {                      
    # (USUALLY DONE ON NORMAL COMPUTER...)
   # reload scenarios
   
   
  if('scebaseline' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'scebaseline', paste("lst_popdyn_scebaseline.RData", sep='')))
 
  if('scesizespectra' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'scesizespectra', paste("lst_popdyn_scesizespectra.RData", sep='')))
 
  if('sceavchok' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavchok', paste("lst_popdyn_sceavchok.RData", sep='')))
 
 
  if('sceavchokpszpectra' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavchokpszpectra', paste("lst_popdyn_sceavchokpszpectra.RData", sep='')))
 
  if('scesizespectrastopifchok' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'scesizespectrastopifchok', paste("lst_popdyn_scesizespectrastopifchok.RData", sep='')))
 
  if('sceavchokpszpctrastopifchok' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavchokpszpctrastopifchok', paste("lst_popdyn_sceavchokpszpctrastopifchok.RData", sep='')))
 
  if('sceavchokpszpctrafmsyrange' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavchokpszpctrafmsyrange', paste("lst_popdyn_sceavchokpszpctrafmsyrange.RData", sep='')))
 
 if('sceavchokpszpctrafmsyrangstopifchok' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavchokpszpctrafmsyrangstopifchok', paste("lst_popdyn_sceavchokpszpctrafmsyrangstopifchok.RData", sep='')))

 if('sceavhtariffspszpctratariffs' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'sceavhtariffspszpctratariffs', paste("lst_popdyn_sceavhtariffspszpctratariffs.RData", sep='')))
 
 
 if('scetrgthtariffspszpctratariffs' %in% general$namefolderoutput) load(file=file.path(general$main.path, general$namefolderinput,
                 'scetrgthtariffspszpctratariffs', paste("lst_popdyn_scetrgthtariffspszpctratariffs.RData", sep='')))
  
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
        ##!!!!!!!!!!!!!!PLOT PER SIMULATED SPECIES !!!!!!!!! !!!!!!!!!!##
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

      
    plot_popdyn <- function(lst_popdyn1=lst_popdyn, ..., namesimu=list(),  add_legend=TRUE,
                     sce=sce,  explicit_pops= explicit_pops, combined_name=c("baseline_vs_implicit"), sum_all=FALSE, general=general){
       
       lstargs <- list(...)
       if(!is.null(lstargs$lst_popdyn2)) lst_popdyn2 <- lstargs$lst_popdyn2 
      
       # filter lst_loglike_agg to trash away the failed (i.e. non-complete) simus:
       # detection according to the number of rows...
       dd                <- table(unlist(lapply(lst_popdyn1, nrow)))
       expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
       idx               <- unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows))
       namesimu1          <- c(names(unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows)))[idx])
       lst_popdyn1 <- lst_popdyn1[namesimu1]
       
       if(!is.null(lstargs$lst_popdyn2)){
       dd                <- table(unlist(lapply(lst_popdyn2, nrow)))
       expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
       idx               <- unlist(lapply(lst_popdyn2, function(x) nrow(x)==expected_nb_rows))
       namesimu2          <- c(names(unlist(lapply(lst_popdyn2, function(x) nrow(x)==expected_nb_rows)))[idx])
       lst_popdyn2 <- lst_popdyn2[namesimu2]
       } 

       # sum over sizegroup?
       if(sum_all) {
            lst_popdyn1 <- 
           lapply(lst_popdyn1, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3] }
                  )
           if(!is.null(lstargs$lst_popdyn2)){
            lst_popdyn2 <- 
           lapply(lst_popdyn2, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3] }
                  )
           
           }
           
           
           a_comment <- "totN"
           } else{
           a_comment <- "per_szgroup"
           }
           
           
           
       # 1. nb of induviduals per (explicit) pop in each size bin
       # from a bunch of simus   
   
        count <-0
        
        outputfile <- file.path(general$main.path, 
                       general$namefolderinput, sce,"jpeg_plots",
                         paste("popdyn_panel1_", a_comment,'.pdf',sep="" ))
        pdf(file = outputfile)
        par(mfrow=c(3,3))
   
        for(pop in explicit_pops){  # for each (explicit) pop
              cat (paste(pop, "\n"))
              count <- count +1
                
              if(count ==10){
                   dev.off()
                   # to be converted in wmf afterward (because wmf do not handle transparency directly in R)
                   #...and open a new window
                   outputfile <- file.path(general$main.path, 
                                    general$namefolderinput, sce,"jpeg_plots",
                                        paste("popdyn_panel2_", a_comment,'.pdf',sep="" ))
                   pdf(file = outputfile)
                   par(mfrow=c(3,3))
                }
              if(count ==19){
                   dev.off()
                   # to be converted in wmf afterward (because wmf do not handle transparency directly in R)
                   #...and open a new window
                   outputfile <- file.path(general$main.path, 
                                    general$namefolderinput, sce,"jpeg_plots",
                                        paste("popdyn_panel3_", a_comment,'.pdf',sep="" ))
                   pdf(file = outputfile)
                   par(mfrow=c(3,3))
                }
              if(count ==28){
                   dev.off()
                   # to be converted in wmf afterward (because wmf do not handle transparency directly in R)
                   #...and open a new window
                   outputfile <- file.path(general$main.path, 
                                    general$namefolderinput, sce,"jpeg_plots",
                                        paste("popdyn_panel4_", a_comment,'.pdf',sep="" ))
                   pdf(file = outputfile)
                   par(mfrow=c(3,3))
                }

               

              this_pop <- lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop, -c(1:2)]
              
              this_pop <- replace(this_pop, is.na(this_pop), 0)
              if(any(this_pop<0)) cat(paste("negative numbers! check this pop ",pop,"\n"))
              
              a.unit <- 1e3  # if divide N by 1e3 then converting in millions because already in thosuand
              a.xlab <- "month"
              a.ylab <- "millions individuals"

              plot(0,0, type='n', axes=FALSE, xlim=c(1,nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,])),
                 ylim=c(min(this_pop, na.rm=TRUE)/a.unit, (max(this_pop, na.rm=TRUE)/a.unit)*1.2),  
                     ylab="", xlab=a.xlab)
              title(pop)

              mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
              mtext(side=1 , a.xlab, outer=TRUE, line=-1)

              axis(1, labels= 1:nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]),
                          at=1:nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]))
              axis(2, las=2)
              box()

              a.count <-0
              for(seg in colnames( lst_popdyn1[[1]] )[-c(1:2)] ){  # for each col
                    cat (paste(seg, "\n"))
                 a.count <- a.count+1

                  mat.sim1 <- matrix(unlist(lapply(lst_popdyn1[ namesimu1 ], function(x){
                     res <- try(x[x$pop==pop,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_popdyn1[[1]])); res
                       })), nrow=nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]) , byrow=FALSE)
                  colnames(mat.sim1) <- c(paste(seg,"_", namesimu1 , sep=''))

                  mat.sim1 <- replace(mat.sim1, is.na(mat.sim1), 0)

                  ramp1 <- colorRamp(c("red", "white"))
                  rgb( ramp1(seq(0, 1, length = 5)), max = 255)
                  per_szgroup <- TRUE # default

                
                if(!is.null(lstargs$lst_popdyn2)){
                  mat.sim2 <- matrix(unlist(lapply(lst_popdyn2[ namesimu2 ], function(x){
                     res <- try(x[x$pop==pop,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_popdyn2[[1]])); res
                       })), nrow=nrow(lst_popdyn2[[1]][lst_popdyn2[[1]]$pop==pop,]) , byrow=FALSE)
                  colnames(mat.sim2) <- c(paste(seg,"_", namesimu2 , sep=''))

                  mat.sim2 <- replace(mat.sim2, is.na(mat.sim2), 0)

                  ramp2 <- colorRamp(c("blue", "white"))
                  rgb( ramp2(seq(0, 1, length = 5)), max = 255)
                   per_szgroup <- FALSE 
                 }
   
 
     # gain in N computation
     if(!is.null(lstargs$lst_popdyn2)){
        assign(paste(seg,"_1", sep=''), apply(mat.sim1, 2, function(x) sum(x[x!=0]/1e6)) ) # change unit to avoid roundoff error
        assign(paste(seg,"_2", sep=''), apply(mat.sim2, 2, function(x) sum(x[x!=0]/1e6)) )
        assign(paste("gain_", seg, pop,"_per_simu",sep=""),  get(paste(seg,"_2", sep=''))/get(paste(seg,"_1", sep='')) )
      }   
  
              
                 # polygon 5-95% for simus
                 mat.sim1 <- replace(mat.sim1, is.na(mat.sim1),0)
                 polygon(c(1:nrow(mat.sim1), rev(1:nrow(mat.sim1))  ),
                     c(apply(mat.sim1, 1, quantile, 0.05)/a.unit,
                       rev(apply(mat.sim1, 1, quantile, 0.95)/  a.unit)) ,
                          col=   rgb( ramp1(seq(0, 1, length = general$nbszgroup)), max = 255, alpha=100)[a.count], border= rgb( ramp1(seq(0, 1, length = general$nbszgroup)), max = 255)[a.count])

                abline(h=0, lty=3)

                if(!is.null(lstargs$lst_popdyn2)){
                 # polygon 5-95% for simus
                  mat.sim2 <- replace(mat.sim2, is.na(mat.sim2),0)
                  polygon(c(1:nrow(mat.sim2), rev(1:nrow(mat.sim2))  ),
                     c(apply(mat.sim2, 1, quantile, 0.05)/a.unit,
                       rev(apply(mat.sim2, 1, quantile, 0.95)/  a.unit)) ,
                          col=   rgb( ramp2(seq(0, 1, length = general$nbszgroup)), max = 255, alpha=100)[a.count], border= rgb( ramp2(seq(0, 1, length = general$nbszgroup)), max = 255)[a.count])
                 }
                
                }   # end for seg
             
            if (add_legend && per_szgroup) legend("topright", legend=1:general$nbszgroup, 
                fill=  rgb( ramp1(seq(0, 1, length =general$nbszgroup)), max = 255)[1:general$nbszgroup],
                    cex=0.8, ncol=2, bty="n")
           if (add_legend && !per_szgroup) legend("topright", legend=1:2, 
                fill=  c("blue",
                         "red"
                         ),
                   cex=0.8, ncol=2, bty="n")

             
             if(count >=length(explicit_pops) ){
               dev.off()
               }


   
           } # end for pop
       
           # export the "gain compared to baseline" variables
           if(!is.null(lstargs$lst_popdyn2) & a_comment=="totN"){
               variables <- paste("gain_totN", explicit_pops,"_per_simu", sep="")
               for(i in 1: length(variables)) {
                   if(i==1) {cc <- get(variables[i])} else {cc <- cbind.data.frame(cc, get(variables[i]))}
                   }
               write.table(cbind.data.frame(time=format(Sys.time(), "%H:%M:%S"), combined_name, pop, 
                      simu=gsub("totN_","", rownames(cc)), cc),
                    file=file.path(general$main.path, general$namefolderinput, 
                     paste("pop_indicators_gain_in_numbers_",the_baseline,"_per_simu.txt", sep='')),
                     append = TRUE, sep = " ", col.names = FALSE, row.names=FALSE, quote=FALSE)
                }

  
   graphics.off()
   
   
   
  
   return()
   }
    
  #----------
  #----------
  plot_popdyn_ssb_obs_sim_for_validation <- function(lst_popdyn1=lst_popdyn, ..., namesimu=list(), 
                     sce=sce,  explicit_pops= explicit_pops, general=general){
       
       lstargs <- list(...)
      
       # filter lst_loglike_agg to trash away the failed (i.e. non-complete) simus:
       # detection according to the number of rows...
       dd                <- table(unlist(lapply(lst_popdyn1, nrow)))
       expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
       idx               <- unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows))
       namesimu1          <- c(names(unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows)))[idx])
       lst_popdyn1 <- lst_popdyn1[namesimu1]
       

       MAT  <- read.table(file=file.path(general$main.path.ibm, paste("popsspe_",general$namefolderinput,sep=''), paste("init_maturity_per_szgroup_biolsce1.dat",sep='')), sep="", dec=".", header=TRUE)
       W    <- read.table(file=file.path(general$main.path.ibm, paste("popsspe_",general$namefolderinput,sep=''), paste("init_weight_per_szgroup_biolsce1.dat",sep='')), sep="", dec=".", header=TRUE)
       MAT  <- MAT[MAT$stock %in% explicit_pops,]
       W    <- W[W$stock %in% explicit_pops,]
       MAT  <- cbind(MAT, szgroup=rep(0:13, length=nrow(MAT)))  # 14 szgroups
       W    <- cbind(W, szgroup=rep(0:13, length=nrow(W)))
       MAT  <- reshape(MAT, timevar = "szgroup", idvar = "stock", direction = "wide")
       W    <- reshape(W, timevar = "szgroup", idvar = "stock", direction = "wide")
       nb_tstep <- length(unique(lst_popdyn1[[1]]$tstep)) 
       MATS <- NULL
       for(i in 1 : (nb_tstep-1)) MATS <- rbind(MATS, MAT)
       WS <- NULL
       for(i in 1 : (nb_tstep-1)) WS <- rbind(WS, W)
    
       # remove tstep 0 because all pop are there while other tstep have only explicit pops
            lst_popdyn1 <- 
           lapply(lst_popdyn1, function(x, explicit_pops) {
                    x <- x[x$tstep != "0" & x$pop %in% explicit_pops,]
                    }, explicit_pops)
 
      # compute ssb per szgroup
            lst_popdyn1 <- 
           lapply(lst_popdyn1, function(x, MATS, WS) {
               cbind( 
                 x[,c(1:2)], 
                     x[,-c(1:2)]*MATS[,-c(1)]*WS[,-c(1)]*1000/2  /1e6  #=> SSB per stock in thousands tons   #only females!! divide by 2
                    )
                    }, MATS, WS)
  
       # sum over sizegroup
            lst_popdyn1 <- 
           lapply(lst_popdyn1, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "ssb" ; x[,1:3] })
           a_comment <- "ssb"
           
       # for the obs ssb
       N     <- read.table(file=file.path(general$main.path.ibm,  paste("popsspe_",general$namefolderinput,sep=''), paste("init_pops_per_szgroup_biolsce1.dat",sep='')), sep="", dec=".", header=TRUE)
       MAT   <- read.table(file=file.path(general$main.path.ibm,  paste("popsspe_",general$namefolderinput,sep=''), paste("init_maturity_per_szgroup_biolsce1.dat",sep='')), sep="", dec=".", header=TRUE)
       W     <- read.table(file=file.path(general$main.path.ibm,  paste("popsspe_",general$namefolderinput,sep=''), paste("init_weight_per_szgroup_biolsce1.dat",sep='')), sep="", dec=".", header=TRUE)
       mat   <- cbind(N, MAT, W)
       mat [,"ssb"] <- mat[,'init_maturity_per_szgroup']*mat[,'init_weight_per_szgroup']*mat[,'init_pops_per_szgroup']*1000/2     #only females!! divide by 2
       mat   <- mat[mat$stock%in% explicit_pops, c(1,7)]
       obs   <- aggregate(mat$ssb, by= list(stock=mat$stock), FUN=sum)
       obs$x <- obs$x /1e6  #=> SSB per stock in thousands tons

       
       # ssb per (explicit) pop 
       # from a bunch of simus    
        count <-0 
        
       outputfile <- file.path(general$main.path, 
                       general$namefolderinput, sce, "jpeg_plots",
                         paste('popdyn_ssb_obs_sim.pdf',sep="" ))
        pdf(file = outputfile, width=4, height=4)
        #windows(4.0,4.0)
        par(mar=c(4,4,1.5,1))
        par(mfrow=c(1,1))
         
        a.xlab <- "Observed SSB ('000 tons)"
        a.ylab <- "Simulated SSB ('000 tons)"
      
        plot(0,0, type='n', axes=FALSE, xlim=c(1, (3500)*1.2),
                ylim=c(1, (3500)*1.2), log="xy", 
                     ylab="", xlab="")
        
        #mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
        #mtext(side=1 , a.xlab, outer=TRUE, line=-1)

        axis(1)
        axis(2, las=2)
        box()
   
        res <- matrix(0, ncol=2, nrow=length(explicit_pops))
        colnames(res) <- c("obs","sim")
        
        for(pop in explicit_pops){  # for each (explicit) pop
              cat (paste(pop, "\n"))
              count <- count +1
                   
              
                  mat.sim1 <- matrix(unlist(lapply(lst_popdyn1[ namesimu1 ], function(x){
                     res <- try(x[x$pop==pop,"ssb"], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_popdyn1[[1]])); res
                       })), nrow=nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]) , byrow=FALSE)
                  colnames(mat.sim1) <- c(paste("ssb","_", namesimu1 , sep=''))

                  mat.sim1 <- replace(mat.sim1, is.na(mat.sim1), 0)
  
                  # sim
                  the_sim_upper <- apply(mat.sim1[1:12,], 1, quantile, 0.95) # keep only the first year i.e. 2010
                  the_sim_median <- apply(mat.sim1[1:12,], 1, quantile, 0.5) # keep only the first year i.e. 2010
                  the_sim_lower <- apply(mat.sim1[1:12,], 1, quantile, 0.05) # keep only the first year i.e. 2010
                  #=> the_sim_median[11] is the SSB at the end of the 2010 year....
                  
                  # obs
                  the_obs_median <- obs[obs$stock==pop,"x"] # pop number per age group at start 2011
   

             cat(paste("obs: ",the_obs_median, " sim: ",the_sim_median[11],"\n"))  
             a_col="black"
             
             res[count,] <- c(the_obs_median, the_sim_median[11]) 
             points(the_obs_median, the_sim_median[11], col="white", cex=2, lwd=1.2)
             points(the_obs_median, the_sim_median[11], pch=16, col=a_col, cex=2)
             segments(the_obs_median, the_sim_upper[11], the_obs_median, the_sim_lower[11],  col=a_col)
             #text(the_obs_median, the_sim_median[11], pop)
             lines (0:3500,0:3500, lty=2)
  
     
             
     
           } # end for pop

           a_lm <- lm(res[,"sim"]~res[,"obs"]-1)
           #abline(abline(a_lm)) 
           #print(summary(a_lm))


       mtext(side=1, a.xlab, outer=FALSE, line=2 )
       mtext(side=2, a.ylab, outer=FALSE, line=2.8 )
       mtext(side=3, "(c)", cex=1.2, adj=0, line=0.2)
 

       # save the last one
       #savePlot(file = outputfile,  type="wmf")
       dev.off()
 


   return()
   }
    
 
 
 
 
      
 
   ##-----------------------------------------------------------------------
   # CALLS
   #!#!#!#!#!#!#!#!#!#!#
   #!#!#!#!#!#!#!#!#!#!#
   #!#!#!#!#!#!#!#!#!#!#
   the_baseline <- "scesizespectra"
   #!#!#!#!#!#!#!#!#!#!#
   #!#!#!#!#!#!#!#!#!#!#
   #!#!#!#!#!#!#!#!#!#!#
   
   lst_popdyn_baseline <- get(paste("lst_popdyn_", the_baseline, sep=''))

   ### then...
   ## keep only the explicit pop
       if(general$namefolderinput=="CelticSea"){
          explicit_pops <- c(0:27)   
      } else{
         stop(" please update the explicit_pop for this case study... ")
      }

   # to export the gains
   write(c("id","sce","pop", "simu", paste("gain_totN", explicit_pops, sep="")), ncol=4+length(explicit_pops),  ## CAUTION NCOL HERE ##
                   file=file.path(general$main.path, general$namefolderinput, 
                     paste("pop_indicators_gain_in_numbers_baseline_per_simu.txt", sep='')),
                     append = FALSE, sep = " ") # init

     
   for (sce in general$namefolderoutput){      
    
        combined_name         <- paste(the_baseline,"_vs_", sce, sep='')
     
       cat("if it still doesn't exist, 'jpeg_plots' folder is created in ",
                      file.path(general$main.path, general$namefolderinput, sce),"\n")
       dir.create(file.path(general$main.path, general$namefolderinput, sce, 'jpeg_plots'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
       dir.create(file.path(general$main.path, general$namefolderinput, combined_name), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
       dir.create(file.path(general$main.path, general$namefolderinput, combined_name, 'jpeg_plots'), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")

       
       # get the object for this sce
       lst_popdyn <- get(paste("lst_popdyn_", sce, sep=''))
     
      # explicit_pops <- unique(lst_popdyn[[1]][lst_popdyn[[1]][,'tstep']==0 &
      #                             lst_popdyn[[1]][,'N_at_szgroup.0']!=100000, 'pop'])
           
      ## for validation------
    if(sce==the_baseline) plot_popdyn_ssb_obs_sim_for_validation (lst_popdyn1=lst_popdyn, 
                     sce=sce,
                      namesimu=list(names(lst_popdyn)),  
                     explicit_pops= explicit_pops,  
                     combined_name= combined_name,
                       general=general) 

    
      ## 1. call for one sce., scenario per scenario------
  
       plot_popdyn (lst_popdyn1=lst_popdyn, 
                     sce=sce,
                      namesimu=list(names(lst_popdyn)),  
                     explicit_pops= explicit_pops,
                      add_legend=TRUE,
                      sum_all=FALSE,
                      combined_name= combined_name,
                       general=general) 
        #=> if sum_all = FALSE then plot per size group
   
     ## 2. call for one sce., scenario per scenario PER SZGROUP------
       plot_popdyn (lst_popdyn1=lst_popdyn, 
                     sce=sce,
                      namesimu=list(names(lst_popdyn)),  
                     explicit_pops= explicit_pops,
                      add_legend=TRUE,
                      sum_all=TRUE,
                      combined_name= combined_name,
                       general=general) 
        #=> if sum_all = FALSE then plot per size group
   
 
      ## 3. call plotting 2 sce on the same plot------
      if(sce!=the_baseline ) {
                        
       plot_popdyn (lst_popdyn1=lst_popdyn_baseline, 
                    lst_popdyn2=lst_popdyn,
                     sce=paste(the_baseline, '_vs_', sce, sep=''),
                      namesimu=list(names(lst_popdyn_baseline), names(lst_popdyn)), 
                     explicit_pops= explicit_pops,
                      add_legend=TRUE,
                      sum_all=TRUE,
                      combined_name= combined_name,
                       general=general) 
       }

  } # end sce
   graphics.off()    
 
 } # end FALSE        
      
      ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
      ##!!!!!!!!!!!!!!PLOT PER SIMULATED SPECIES (ALL ON ONE PLOT)!!!##
      ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 if(FALSE){
      # plot all stocks totN on one plot
       for (sce in general$namefolderoutput){
       
       # get the object for this sce
       lst_popdyn <- get(paste("lst_popdyn_", sce, sep=''))

       outputfile <- file.path(general$main.path, 
                                 general$namefolderinput, sce, "jpeg_plots",
                                        paste("popdyn_", "all_in_one.pdf",sep="" ))
        pdf(file = outputfile)
        par(mfrow=c(1,1))
    
       explicit_pops <- unique(lst_popdyn[[1]][lst_popdyn[[1]][,'tstep']==0 &
                                   lst_popdyn[[1]][,'N_at_szgroup.0']!=100000, 'pop'])
       lst_popdyn <- 
           lapply(lst_popdyn, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3] }
                  )
       
       a.unit <- 1e6  # in millions
       a.xlab <- "Month"
       a.ylab <- "Millions individuals"

       nbtstep   <- max(lst_popdyn[[1]]$tstep)            

       
       plot(0,0, type='n', axes=FALSE, xlim=c(1, nbtstep),
                 ylim=c(  (max(min(unlist(lst_popdyn), na.rm=TRUE), -1e7))/a.unit, 
                         ((min(max(unlist(lst_popdyn), na.rm=TRUE),1e9))/a.unit)*1.2  ),  
                     ylab="", xlab=a.xlab)
       axis(1, labels= unique(lst_popdyn[[1]]$tstep) , at= unique(lst_popdyn[[1]]$tstep) )
       axis(2, las=2)
       box()
       
       for(pop in explicit_pops){  # for each (explicit) pop
              this_pop <- lst_popdyn[[1]][lst_popdyn[[1]]$pop==pop,]        
              this_pop <- replace(this_pop, is.na(this_pop), 0)
              if(any(this_pop<0)) cat(paste("negative numbers! check this pop ",pop,"\n"))
              
       
              mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
        
              a.count <-0
              for(seg in colnames( lst_popdyn[[1]] )[-c(1:2)] ){  # for each col
                    cat (paste(seg, "\n"))
    
                  mat.sim1 <- matrix(unlist(lapply(lst_popdyn[ general$namesimu[[sce]] ], function(x){
                     res <- try(x[x$pop==pop,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_popdyn[[1]])); res
                       })), nrow=nrow(lst_popdyn[[1]][lst_popdyn[[1]]$pop==pop,]), byrow=FALSE)
                  colnames(mat.sim1) <- c(paste(seg,"_", general$namesimu[[sce]] , sep=''))

                  mat.sim1 <- replace(mat.sim1, is.na(mat.sim1), 0)


                 # polygon 5-95% for simus
                 mat.sim1 <- replace(mat.sim1, is.na(mat.sim1),0)
                 polygon(c(unique(lst_popdyn[[1]]$tstep), rev(unique(lst_popdyn[[1]]$tstep))  ),
                     c(apply(mat.sim1, 1, quantile, 0.05)/a.unit,
                       rev(apply(mat.sim1, 1, quantile, 0.95)/  a.unit)) ,
                          col=   rgb(seq(0.01,1,0.02)[pop],seq(1,0.01,-0.03)[pop],seq(1,0.01,-0.03)[pop],0.5), border= TRUE)

                abline(h=0, lty=3)
                }   # end for seg
           
           } # end for pop
           # save the current one
         
          legend("top", legend=explicit_pops, 
             fill=  rgb(seq(0.01,1,0.1),seq(1,0.01,-0.1)^2,seq(1,0.01,-0.1),0.5) [1:length(explicit_pops)], 
                cex=0.8, ncol=2, bty="n")
 
        dev.off()
        } # end sce
        graphics.off()
        
        
 } # end FALSE       
        
   
