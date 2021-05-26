ym2date <- function(x) {
  sapply(strsplit(x, "[.]"),
         function(y) {
           as.numeric(y[1]) + as.numeric(y[2]) / 12 - 1/12
         })
}

selpop <- function(what2, selected, a_baseline) {
  nms <- names(get(paste("lst_loglike_agg_",what2, selected, a_baseline, sep=''))[[1]])
  pops <- nms[startsWith(nms, "pop.")]
  pops = (unique(pops))
  if (length(pops) == length(popnames$spp)) {
    names(pops) <- popnames$spp
  }
  pops[order(names(pops)) ]
}

selindic <- function() {
c("F/Finit", "SSB/SSBinit", "TLand/TLandinit", "TDisc/TDiscinit", "Tac/Tacinit")
}

selsce <- function(popdynscenarios,scenames) {
  attr(popdynscenarios,"names")=scenames
 
  return(popdynscenarios)
}

selvar <- function() {
  #nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
  #nms[-1]
  c(GVA = "gradva", Revenue = "rev_from_av_prices", "Total landings" = "totland", "Effort" = "effort", "Number of trips" = "nbtrip")
}

selsumoverszgrp <- function(){
  dd <- c(FALSE, TRUE)
  dd
}

selquantity <- function() {
  tablefns <- dir("output", pattern = "average")
  matches <- regexpr(pattern = "[^_]*cum[^u._]+", tablefns)
  res <- unique(regmatches(tablefns, matches))
  cumul <- function(x) {sub("cum", "Cumulative ", x)}
  over <- function(x) paste(cumul(x[1]), cumul(x[2]), sep = " over ")
  res <- setNames(res, ifelse(grepl("over", res), sapply(strsplit(res, "over"), over), cumul(res) ))
  res[!grepl("over", res)]
}

warningPlot <- function(text = "No data", cex = 1.3, ...) {
  plot(1, type="n", xlab="", ylab="", axes = FALSE)
  text(1, 1, text, cex = cex, ...)
}