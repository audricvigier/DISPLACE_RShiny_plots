

#' Re-load outcome files of getAggLoglikeFiles() in the global env.
#'
#' This function re-load the files produced by getAggLoglikeFiles()
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#'  \dontrun{
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#' }

loadLoglikeFiles <- function (general=general, use_port_info=FALSE){

 # reload scenarios for what="weight"
 for (sce in general$namefolderoutput){
  if(sce %in% general$namefolderoutput) {
     load(file=file.path(general$main.path, general$namefolderinput,
                 sce, paste("lst_loglike_weight_agg_",sce,".RData", sep='')), envir = .GlobalEnv)
     if(!use_port_info) rm( list=paste("lst_loglike_agg_weight_vid_port_",sce, sep=''), envir = .GlobalEnv) ; gc(TRUE)  # remove bc too memory demanding
    }
               
  }



return()
}




