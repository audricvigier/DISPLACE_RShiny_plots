reshape_per_pop_for_one_sce <- function(lst_loglike1=lst_loglike,
                                        namesimu=paste("simu", 1:5, sep=''),
                                        selected_pops=explicit_pops,
                                        type_of_column="pop",
                                        nby=11){
  
  # filter lst_loglike to trash away the failed (i.e. non-complete) simus:
  # detection according to the number of rows...
  dd                <- table(unlist(lapply(lst_loglike1, nrow)))
  namesimu          <- names(lst_loglike1)
  
  # Drop buggy col names
  for (i in 1:length(lst_loglike1)){
    lst_loglike1[[i]]=lst_loglike1[[i]][,-c(which(is.na(colnames(lst_loglike1[[i]]))))]
  }
  
  # subset
  refsimu     <- namesimu[1]
  lst_loglike <- lst_loglike1#[ namesimu ] # ??? Useless
  
  # explicit pops??
  pops <- selected_pops
  idx_col <- grep(type_of_column, colnames(lst_loglike[[refsimu]]))
  
  
  # check
  #print("check if same numbers of row across the simus")
  #print( lapply(lst_loglike1[ namesimu ], nrow) )
  
  
  # first, reshape for pop
  for (i in seq_along(lst_loglike)){
    lst_loglike[[ i ]] <- lst_loglike[[ i ]][1:nby, colnames(lst_loglike[[ i ]]) %in% c("year",paste0(type_of_column,".",pops)) ]
    lst_loglike[[ i ]] = subset(lst_loglike[[ i ]], !is.na(year))
    
    lst_loglike[[ i ]] <- cbind( reshape(lst_loglike[[ i ]], idvar="year", varying=list(2:ncol(lst_loglike[[ i ]])),
                                         v.names=type_of_column, direction="long"), a_pop = rep(pops, each=nrow(lst_loglike[[ i ]])) )
  }
  
  # reshape...
  res <- NULL
  for(pop in pops){  # for each (explicit) pop
    mat.sim1 <- matrix(unlist(lapply(lst_loglike[ namesimu ], function(x){
      res <- try(x[x$a_pop==pop, type_of_column], silent=TRUE)
      if(class(res)=="try-error") res <- rep(NA, ncol(lst_loglike[[refsimu]]))
      res
    })), nrow=nrow(lst_loglike[[refsimu]][lst_loglike[[refsimu]]$a_pop==pop,]) , byrow=FALSE)
    colnames(mat.sim1) <- c(paste(type_of_column,"_", namesimu , sep=''))
    
    
    
    mat.sim1 <- replace(mat.sim1, is.na(mat.sim1), 0)
    
    mat.sim1 <- cbind.data.frame(lst_loglike[[refsimu]] [ lst_loglike[[1]]$a_pop==pop, c("year","a_pop") ], mat.sim1)
    
    if(!is.null(res)) res <- rbind.data.frame(res, mat.sim1) else res <- mat.sim1
    
  }
  
  
  res <- res[!is.na(res$year),]
  
  # get the median....
  res <- cbind (res,
                median=apply(res[,-c(1,2)], 1, quantile, probs=0.5),
                mean= apply(res[,-c(1,2)], 1, mean)
  )
  
  return(res)
}


barplotTotLandingsPerSce <- function(general=general,type_of_column="pop", # or "disc"
                                     selected="_all_",
                                     selected_pops=0:26,
                                     selected_scenarios=selsce(popdynscenarios,scenames),
                                     scenarios_names=names(selsce(popdynscenarios,scenames)), firsty="2010", lasty="2012",
                                     a_width=3500, a_height=2000, black_and_white = FALSE, ylims = c(0,30)){


  
  nby = as.numeric(lasty) - as.numeric(firsty)+1


  #------------------------
  ### 1- calls for reshaping the lst_popdyn1----------------
  count <- 0
  for (sce in selected_scenarios){
    count <- count+1
    lst_loglike <- get(paste("lst_loglike_agg_weight", selected, sce, sep=''), env=.GlobalEnv)


    # aggregate landings weight PER YEAR
    for (i in 1:length(lst_loglike)){
      loglike       <- lst_loglike[[i]] # input
      if(selected=="_met_") loglike <- loglike[loglike$metier==met,]
        loglike$year  <- unlist(lapply(strsplit(as.character(loglike$year.month), split="\\."), function(x) x[1]))
        nm            <- colnames(loglike)
        idx.col       <- grep(paste0(type_of_column,'.'), nm) # 40: DEBUG
        DT            <- data.table(loglike)
        eq1           <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
        loglike.agg   <- DT[,eval(eq1),by=list(year)]
        loglike.agg   <- data.frame( loglike.agg)
        colnames(loglike.agg) <- c("year", paste(paste0(type_of_column,'.'), 0:(general$nbpops-1), sep=''))
        loglike.agg   <- loglike.agg[order(loglike.agg$year),] # order
        lst_loglike[[i]] <- loglike.agg # output
    }

    # keep complete simu i.e. 5 years
     #lst_loglike <- lst_loglike [ names(lst_loglike) [lapply(lst_loglike, nrow)>=nby] ]

    loglike_reshaped <- reshape_per_pop_for_one_sce(
      lst_loglike1=lst_loglike,
      namesimu=general$namesimu[[sce]],
      selected_pops=selected_pops,
      type_of_column=type_of_column,
      nby=nby)
    assign(paste("loglike_", sce, sep=''), loglike_reshaped)
  }

  ### the code for the plot
  all_sces_first_y  <- data.frame(NULL)
  all_sces_last_y   <- data.frame(NULL)
  count    <- 0
  for (sce in selected_scenarios){
    count <- count+1

    loglike <- get(paste("loglike_", sce, sep=''))


    loglike_last_tstep   <- loglike[loglike$year %in%  lasty ,]
    loglike_first_tstep  <- loglike[loglike$year %in%  firsty ,]
    ##pop_names            <-  read.table(file.path(general$main.path.ibm, paste("pop_names_", general$namefolderinput, ".txt", sep='')), header=TRUE)
    pop_names <- paste0("pop.", selected_pops) ## ALEX

    loglike_last_tstep$a_pop          <- factor(loglike_last_tstep$a_pop)
    #levels(loglike_last_tstep$a_pop)  <- pop_names[ pop_names[,1] %in%   levels(loglike_last_tstep$a_pop) ,2]
    loglike_first_tstep$a_pop         <- factor(loglike_first_tstep$a_pop)
    #levels(loglike_first_tstep$a_pop) <- pop_names[ pop_names[,1] %in%   levels(loglike_first_tstep$a_pop) ,2]

    if(count==1)  all_sces_first_y <- cbind.data.frame(a_pop=as.character(loglike_first_tstep[,"a_pop"]))
    all_sces_first_y <- cbind.data.frame(all_sces_first_y, loglike_first_tstep[,"mean"])
    colnames(all_sces_first_y)[ncol(all_sces_first_y)] <- scenarios_names[count]

    if(count==1)  all_sces_last_y <- cbind.data.frame(a_pop=as.character(loglike_last_tstep[,"a_pop"]))
    all_sces_last_y <- cbind.data.frame(all_sces_last_y, loglike_last_tstep[,"mean"])
    colnames(all_sces_last_y)[ncol(all_sces_last_y)] <- scenarios_names[count]
  } # end sce


  # reorder stocks
  all_sces_first_y$a_pop <- factor(all_sces_first_y$a_pop)
 
  some_colors <-  c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666','#1B9E77','#D95F02','#7570B3',
                    '#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C',
                    '#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928','#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6',
                    '#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2','#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC',
                    '#CCCCCC','#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999','#66C2A5',
                    '#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3','#8DD3C7','#FFFFB3','#BEBADA','#FB8072',
                    '#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F')
  the_density <- rep(500, length(some_colors))



  ##-------A PLOT-----------
  if(type_of_column=="pop") namefileplot <- paste("landings_per_stock_per_scenario", selected, sep='')
  if(type_of_column=="disc") namefileplot <- paste("discards_per_stock_per_scenario", selected, sep='')

  # tiff(file=file.path(general$main.path, general$namefolderinput, paste(namefileplot, ".tiff", sep="")),
  #      width = a_width, height = a_height,   compression="lzw",
  #      units = "px", pointsize = 12,  res=300)
  par(mfrow=c(1,2))
  par(mar=c(9,3,2,2))
  par(oma=c(3,2,1,1))


  rownames(all_sces_first_y) <- all_sces_first_y[,1]
  rownames(all_sces_last_y) <- all_sces_last_y[,1]

  ylims <- c(0, max(as.matrix(all_sces_first_y[,-1])/1e6, as.matrix(all_sces_last_y[,-1])/1e6, na.rm=TRUE))

  mp_first_y <-  barplot(as.matrix(all_sces_first_y[,-1])/1e6, las=2,  ylim=ylims, xlab="", ylab="",
                         col =some_colors[popnames$idx %in% selected_pops] , density=the_density, legend.text=popnames$spp[popnames$idx %in% selected_pops], axes = FALSE,axisnames = FALSE,
                         args.legend = list(x = "topright", bty = "o", ncol=2, cex=0.9))
  text(mp_first_y, par("usr")[3] - 0.5, labels = colnames(all_sces_first_y[-1]), srt = 45, adj = 1, xpd = TRUE, cex = 1)
  axis(2, las=2)
  title ("First year", adj=0)

  mtext("Scenario", 1, line=0, cex=1.5, outer=TRUE)
  if(type_of_column=="pop") mtext(side=2, "Annual landings [*000 tons]", line=0, cex=1, outer=TRUE)
  if(type_of_column=="disc") mtext(side=2, "Annual discards [*000 tons]", line=0, cex=1, outer=TRUE)

  mp_last_y <- barplot(as.matrix(all_sces_last_y[,-1])/1e6, las=2, ylim=ylims, xlab="", ylab="",col =some_colors[popnames$idx %in% selected_pops] , density=the_density,  legend=FALSE, axes = FALSE,axisnames = FALSE,args.legend = list(x = "topright", bty = "n", ncol=2))
  text(mp_last_y, par("usr")[3] - 0.5, labels = colnames(all_sces_last_y[-1]), srt = 45, adj = 1, xpd = TRUE, cex = 1)
  axis(2, las=2)
  title ("Last year", adj=0)

  mtext("Scenario", 1, line=0, cex=1.5, outer=TRUE)
  if(type_of_column=="pop") mtext(side=2,"Annual landings [*000 tons]",line=0, cex=1., outer=TRUE)
  if(type_of_column=="disc") mtext(side=2,"Annual discards [*000 tons]",line=0, cex=1., outer=TRUE)

  ##dev.off()


  return()
}


