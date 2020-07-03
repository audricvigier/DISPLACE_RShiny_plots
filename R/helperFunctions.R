ym2date <- function(x) {
  sapply(strsplit(x, "[.]"),
         function(y) {
           as.numeric(y[1]) + as.numeric(y[2]) / 12 - 1/12
         })
}

selpop <- function() {
  nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
  pops <- nms[startsWith(nms, "pop.")]
  pops
}

selsce <- function() {
  loglikefns <- dir("data", "loglike.*RData", full.names = TRUE)
  gsub("^.*lst_loglike_weight_agg_|[.]RData", "", loglikefns)
}

selvar <- function() {
  nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
  nms[-1]
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