

plot_popdyn <- function(sces=sces,
                        explicit_pops= explicit_pops,
                        sum_all=FALSE){

       pops <- sapply( strsplit(explicit_pops, split="\\."), function(x) x[2])


       lst_popdyn1        <- get(paste("lst_popdyn_", sces[1], sep=''), env=.GlobalEnv)
       lst_popdyn2        <- get(paste("lst_popdyn_", sces[2], sep=''), env=.GlobalEnv)


       # filter lst_loglike_agg to trash away the failed (i.e. non-complete) simus:
       # detection according to the number of rows...
       dd                <- table(unlist(lapply(lst_popdyn1, nrow)))
       expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
       idx               <- unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows))
       namesimu1          <- c(names(unlist(lapply(lst_popdyn1, function(x) nrow(x)==expected_nb_rows)))[idx])
       lst_popdyn1 <- lst_popdyn1[namesimu1]

       dd                <- table(unlist(lapply(lst_popdyn2, nrow)))
       expected_nb_rows  <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
       idx               <- unlist(lapply(lst_popdyn2, function(x) nrow(x)==expected_nb_rows))
       namesimu2          <- c(names(unlist(lapply(lst_popdyn2, function(x) nrow(x)==expected_nb_rows)))[idx])
       lst_popdyn2 <- lst_popdyn2[namesimu2]

       # sum over sizegroup?
       if(sum_all) {
            lst_popdyn1 <-
           lapply(lst_popdyn1, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3] }
                  )
            lst_popdyn2 <-
           lapply(lst_popdyn2, function(x) {
                   x[,3] <- apply(x[,-c(1:2)], 1, sum, na.rm=TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3] }
                  )

           }

       # 1. nb of induviduals per (explicit) pop in each size bin
       # from a bunch of simus

        count <-0


        for(pop in pops){  # for each (explicit) pop
              this_pop <- lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop, -c(1:2)]

              this_pop <- replace(this_pop, is.na(this_pop), 0)
              if(any(this_pop<0)) cat(paste("negative numbers! check this pop ",pop,"\n"))

              a.unit <- 1e3  # if divide N by 1e3 then converting in millions because already in thosuand
              a.xlab <- "month"
              a.ylab <- "millions individuals"

              plot(0,0, type='n', axes=FALSE, xlim=c(1,nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,])),
                 ylim=c(min(this_pop, na.rm=TRUE)/a.unit, (max(this_pop, na.rm=TRUE)/a.unit)*1.2),
                     ylab="", xlab="")
              title(pop)

              mtext(side=2 , a.ylab, outer=TRUE, line=-1.2)
              mtext(side=1 , a.xlab, outer=TRUE, line=-1)

              axis(1, labels= 1:nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]),
                          at=1:nrow(lst_popdyn1[[1]][lst_popdyn1[[1]]$pop==pop,]))
              axis(2, las=2)
              graphics::box()

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
                  ##rgb( ramp1(seq(0, 1, length = 5)), max = 255)
                  per_szgroup <- TRUE # default


                  mat.sim2 <- matrix(unlist(lapply(lst_popdyn2[ namesimu2 ], function(x){
                     res <- try(x[x$pop==pop,seg], silent=TRUE); if(class(res)=="try-error") res <- rep(NA, ncol(lst_popdyn2[[1]])); res
                       })), nrow=nrow(lst_popdyn2[[1]][lst_popdyn2[[1]]$pop==pop,]) , byrow=FALSE)
                  colnames(mat.sim2) <- c(paste(seg,"_", namesimu2 , sep=''))

                  mat.sim2 <- replace(mat.sim2, is.na(mat.sim2), 0)

                  ramp2 <- colorRamp(c("blue", "white"))
                  rgb( ramp2(seq(0, 1, length = 5)), max = 255)
                   per_szgroup <- FALSE



                 # polygon 5-95% for simus
                 mat.sim1 <- replace(mat.sim1, is.na(mat.sim1),0)
                 polygon(c(1:nrow(mat.sim1), rev(1:nrow(mat.sim1))  ),
                     c(apply(mat.sim1, 1, quantile, 0.05)/a.unit,
                       rev(apply(mat.sim1, 1, quantile, 0.95)/  a.unit)) ,
                          col=   rgb( ramp1(seq(0, 1, length = 14)), max = 255, alpha=100)[a.count], border= rgb( ramp1(seq(0, 1, length = 14)), max = 255)[a.count])

                abline(h=0, lty=3)

                 # polygon 5-95% for simus
                  mat.sim2 <- replace(mat.sim2, is.na(mat.sim2),0)
                  polygon(c(1:nrow(mat.sim2), rev(1:nrow(mat.sim2))  ),
                     c(apply(mat.sim2, 1, quantile, 0.05)/a.unit,
                       rev(apply(mat.sim2, 1, quantile, 0.95)/  a.unit)) ,
                          col=   rgb( ramp2(seq(0, 1, length = 14)), max = 255, alpha=100)[a.count], border= rgb( ramp2(seq(0, 1, length = 14)), max = 255)[a.count])

                }   # end for seg


           } # end for pop

   legend("topleft", box.lty=0, legend=sces, fill= c(rgb( ramp1(seq(0, 1, length = 14)), max = 255, alpha=100)[1], rgb( ramp2(seq(0, 1, length = 14)), max = 255)[1]))
   graphics::box()

   return()
   }


