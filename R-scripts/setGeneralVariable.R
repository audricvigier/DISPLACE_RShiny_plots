            
#' Setting the general variable to use across other routines
#'
#' This function sets the general variable useful for relative file paths etc. 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' general <- setGeneralOverallVariable()



setGeneralOverallVariable <- function( pathToRawInputs =file.path("D:/work/Displace/", paste0("DISPLACE_input_gis_","CelticSea")),
                                       pathToDisplaceInputs = file.path("D:/work/Displace/", paste0("DISPLACE_input_","CelticSea")),
                                       pathToOutputs =file.path("D:","DISPLACE_outputs"),
                                       caseStudy="CelticSea",
                                       iGraph=3,
                                       iYear="2010",
                                       iYearEnd="2020",
                                       iCountry=NULL,
                                       nbPops=27,
                                       nbSzgroup=14,
                                       theScenarios= c("calib_multipliers_","calib_multipliers_SCE_"),
                                       nbSimus=20,
                                       useSQLite=FALSE    
                                       ){

 # GENERAL SETTINGS
  general <- list()

  general$case_study <- caseStudy

  general$main.path         <- pathToOutputs                                                                                                   
  general$main.path.param   <- pathToRawInputs
  general$main.path.ibm     <- pathToDisplaceInputs
 

    general$igraph            <- iGraph
    general$a.year            <- iYear
    general$a.yearEnd         <- iYearEnd
    general$a.country         <- iCountry
    general$nbpops            <- nbPops  
    general$nbszgroup         <- nbSzgroup
    general$namefolderinput   <- caseStudy
  
    general$namefolderoutput   <- theScenarios
    general$namesimu           <- list()
    for (i in 1:length(theScenarios)){general$namesimu[[i]] = paste(theScenarios[i], c(1:nbSimus), sep='')}
    attr(general$namesimu,"names")=theScenarios
 
    general$use_sqlite <- useSQLite

   

return(general)
} 


