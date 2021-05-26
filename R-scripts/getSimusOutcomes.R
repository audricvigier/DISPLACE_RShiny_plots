getOutcome <-  function(variableName="cumsteaming",outcomes,lst_loglike_w_agg_all_1,a_baseline,others_than_baseline,nbyears,discount_factor,what2,selected,explicit_pops){
  outcomes2return <- NULL
  variable2process <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    if(variableName%in%c("cumsteaming","nbtrip","av_bwtrip","av_effort","totland_implicit","totland_explicit",paste('pop.', explicit_pops, sep=''),"rev_from_av_prices","sweptr","fuelcost","gradva","vapuf",paste('disc_rate_', explicit_pops, sep=''))){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,variableName]))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,variableName]))))
    }
    if(variableName=="feffort"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming']))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming']))))
    }
    if(variableName=="propfeffort"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))
    }
    if(variableName=="npv"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      ))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      ))))
    }
    if(variableName=="av_vapuf_month"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE))))
    }
    if(variableName=="trip_based_cpue"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort']))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort']))))
    }
    if(variableName=="fishing_based_cpue_implicit"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
    }
    if(variableName=="fishing_based_cpue_explicit"){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
    }
    if(variableName%in%paste('fishing_based_cpue_explicit_',explicit_pops,sep='')){
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,paste('pop.',explicit_pops,sep='') ])/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,paste('pop.',explicit_pops,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
    }
    # measure of equity, diversity or distributional profit
    # Shannon index is sum from i=1 to nbVessels of p_i*ln (p_i) with p_i is  the proportion of vessels belonging to the ith class of revenue in the dataset of interest. 
    # nb of classes of revenue to decide upon?
    # but better to use the ginin index or 20:20 Ratio, ...
    #or the the Robin Hood index H (because bwteen 0 to 1 then can be useful for a ratio)
    # http://en.wikipedia.org/wiki/Income_inequality_metrics
    if(variableName=="hoover"){
      lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_",a_baseline, sep=''))
      lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
      dd1 <- (unlist(lapply(lst_loglike_w_agg_vid_1, function (x) {                              
        ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
        ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
        ai <- table(cut(x[,'gradva'], breaks=ei))
        ei <- ei[-1]
        hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
      }
      )))
      dd2 <- (unlist(lapply(lst_loglike_w_agg_vid_2, function (x) {   
        ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
        ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
        ai <- table(cut(x[,'gradva'], breaks=ei))
        ei <- ei[-1]
        hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
        
      }
      )))
    }
    
    if(sum(dd1)!=0 & !is.nan(sum(dd1))){
      ratio_percent <- ( dd2/ dd1 *100) -100
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      ratio_percent <- ratio_percent#[ratio_percent<400]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(dd2))
      variable2process[sce,"mean"] <- signif(a_mean,5)
      variable2process[sce,"a_CI"] <- signif(a_CI,3)
    }
    if(sum(dd1)==0 | is.nan(sum(dd1))){
      ratio_percent = rep(NA,length(dd1))
      names(ratio_percent)=names(dd2)
    }
    outcomes2return <- rbind.data.frame(outcomes2return, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=variableName))
  }
  return(outcomes2return)
}

getSimusOutcomes <- function(general,a_baseline="calib_multipliers_",explicit_pops=0:26,selected="_met_"){
  
  if(general$case_study=="CelticSea") loadLoglikeFiles(general=general, use_port_info=FALSE,what="weight")
  what2 <- "weight"
  lst_loglike_w_agg_all_1     <- get(paste("lst_loglike_agg_",what2, selected, a_baseline, sep=''))
  # I don't get why some simulations are taken off
  # dd                      <- table(unlist(lapply(lst_loglike_w_agg_all_1, nrow)))
  # expected_nb_rows        <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
  # idx                     <- unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows))
  # namesimu1               <- names(unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows)))[idx]
  # lst_loglike_w_agg_all_1 <- lst_loglike_w_agg_all_1[namesimu1]
  
  lst_loglike_w_agg_vid_1 <- get(paste("lst_loglike_agg_",what2,"_vid_", a_baseline, sep=''))
  others_than_baseline        <- general$namefolderoutput[!general$namefolderoutput %in% a_baseline]
  nbyears         <- nrow(lst_loglike_w_agg_all_1[[1]])/12 # because monthly data
  discount_factor <- sapply(rep(paste("1/((1+0.04)^",1:nbyears,")"), each=12), function(x) eval(parse(text=x)))
  
  variableVectors = c("cumsteaming","nbtrip","av_bwtrip","av_effort","totland_explicit",paste('pop.', explicit_pops, sep=''),"rev_from_av_prices","sweptr","fuelcost","gradva","vapuf",paste('disc_rate_', explicit_pops, sep=''),"feffort","propfeffort","npv","av_vapuf_month","trip_based_cpue","fishing_based_cpue_explicit",paste('fishing_based_cpue_explicit_',explicit_pops,sep=''),"hoover")# Ignoring "totland_implicit" and "fishing_based_cpue_implicit" as I don't have any implicit populations
  
  # I can define my own outcomes. If needed, work on the metier version to get metier-wise indicators. (create a getOutcomeMetier function)
  outcomes=ldply(lapply(variableVectors,getOutcome,outcomes,lst_loglike_w_agg_all_1,a_baseline,others_than_baseline,nbyears,discount_factor,what2,selected,explicit_pops))
  
  #Rename some levels
  outcomes$variable = recode_factor(outcomes$variable, "seffort" = "cumsteaming" ,"av_trip_duration" = "aveffort" ,"rev_from_av_prices" = "revenue" ,"sweptarea" = "sweptr" ,"gav" = "gradva" ,"av_vpuf" = "vapuf")
  for(pop in explicit_pops)  levels(outcomes$variable)[levels(outcomes$variable)==paste('pop.', pop, sep='')] =  paste('totland_', pop, sep='')
  
  # export
  write.table(outcomes,
              file=file.path(general$main.path, general$namefolderinput, 
                             paste("data/outcomes_all_simus_relative_to_baseline_sce_all.txt", sep='')),
              sep=";", quote=FALSE, row.names=FALSE) 
}

doOutcomesbarPlot <-  function(selected="all",selected_variables = c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit", "totland_explicit", "sweptarea", "npv", "av_vpuf_month", "hoover"),selected_scenarios= c("baseline","calib_multipliers_SCE_")){
  outcomes <- read.table(file.path(general$main.path, general$namefolderinput, 
                                   paste("data/outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')), header=TRUE, sep=";")
  
  # add baseline at 0,0,0, etc.
  baseline <- outcomes[outcomes$scenario == outcomes$scenario[1],]  # init
  baseline$ratio_percent <-0
  baseline$scenario <- "baseline"
  outcomes <- rbind.data.frame(baseline, outcomes)
  outcomes$scenario <- factor(outcomes$scenario)
  
  outcomes           <- outcomes[outcomes$variable %in% selected_variables,]
  
  outcomes <- outcomes[outcomes$scenario %in%selected_scenarios,]
  outcomes$scenario <- factor(outcomes$scenario)
  outcomes$scenario <- factor(outcomes$scenario, levels=selected_scenarios, labels=  selected_scenarios)
  
  bwplot(ratio_percent~variable| scenario, data=outcomes)
  
  # a better plot
  namefile       <- paste(paste("indicators_boxplot_persce_",selected, sep=""))
  output.folder  <- file.path(general$main.path, general$namefolderinput)
  the_dim        <- c(2400, 2400)
  
  
  png(filename=file.path(output.folder, paste(namefile, ".png", sep="" )),
      width = the_dim[1], height = the_dim[2], 
      units = "px", pointsize = 12,  res=300)
  
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
}