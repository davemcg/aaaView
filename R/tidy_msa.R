##' Convert msa file/object to tidy data frame.
##'
##'
##' @title tidy_msa from Guangchuang Yu's ggmsa
##' @param msa multiple sequence alignment file or sequence object in
##' DNAStringSet, RNAStringSet, AAStringSet, BStringSet, DNAMultipleAlignment,
##' RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @return tibble data frame
##' @export
##' @author Guangchuang Yu
tidy_msa <- function(msa, start = NULL, end = NULL) {
  if(inherits(msa, "character") && length(msa) > 1) {
    aln <- msa
  }else {
    aln <- prepare_msa(msa)
  }
  alnmat <- lapply(seq_along(aln), function(i) {
    ##Preventing function collisions
    base::strsplit(as.character(aln[[i]]), '')[[1]]
  }) %>% do.call('rbind', .)
  ## for DNAbin and AAbin
  alndf <- as.data.frame(alnmat, stringsAsFactors = FALSE)

  if(unique(names(aln)) %>% length == length(aln)) {
    alndf$name = names(aln)
  }else{
    stop("Sequences must have unique names")
  }
  cn = colnames(alndf)
  cn <- cn[!cn %in% "name"]
  df <- gather(alndf, "position", "character", cn)

  y <- df
  y$position = as.numeric(sub("V", "", y$position))
  y$character = toupper(y$character)

  y$name = factor(y$name, levels=rev(names(aln)))


  if (is.null(start)) start <- min(y$position)
  if (is.null(end)) end <- max(y$position)

  y <- y[y$position >=start & y$position <= end, ]

  return(y)
}
