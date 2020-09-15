plot_annualindic <- function(sces=sces,
                        explicit_pops= explicit_pops,
                        indic="F"){



    # look at annual indics such as the TACs...
    res <- NULL
    for(sce in sces) {
       print(paste("sce ", sce))
       lst <- get(paste("lst_annualindic_", sce, sep = ''), env = .GlobalEnv)
       for(simu in length(lst)) {
          print(paste("sim ", simu))
          # merge all infos
          annual_indics <- lst[[simu]] 
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
   outcome$sce <- factor(outcome$sce, levels=sces, labels=  sces)


   # put in long format
   df1 <- cbind.data.frame(outcome[,c('stk','sce','simu','FFinit')], var="FFinit")
   df2 <- cbind.data.frame(outcome[,c('stk','sce','simu','SSBSSBinit')] , var="SSBSSBinit")
   colnames(df1) <- colnames(df2) <- c('stk','sce','simu','value','var')
   out <- rbind.data.frame(df1,df2)
   


 # SSB, F and whatever  
 library(ggplot2)
   pops <- gsub("pop.","", explicit_pops)
   p <- ggplot(out[out$stk==pops,], aes(x=sce, y=value, color=var))  + geom_boxplot(outlier.shape=NA)  +
             labs(x = "Scenario", y = "Value")  + facet_wrap( ~ stk, ncol=1, scales="fixed")     + ylim(0, 1)
 print(
       p   + 
       theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  + geom_boxplot(outlier.shape=NA)
       )

 

# if necessary to add a second y-axis, play with the below:

# adding the relative humidity data, transformed to match roughly the range of the temperature
#  p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))
  
#  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#  # and, very important, reverting the above transformation
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

 
 
 
return()
} 
 
 

 