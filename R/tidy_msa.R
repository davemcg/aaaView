##' Convert msa file/object to tidy data frame.
##'
##'
##' @title tidy_msa from Guangchuang Yu's ggmsa
##' @param msa multiple sequence alignment file or sequence object in
##' DNAStringSet, RNAStringSet, AAStringSet, BStringSet, DNAMultipleAlignment,
##' RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @importFrom tidyr gather
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


##' preparing multiple sequence alignment
##'
##' This function supports both NT or AA sequences; It supports multiple
##' input formats such as "DNAStringSet", "BStringSet", "AAStringSet",
##' DNAbin", "AAbin" and a filepath.
##' @title prepare_msa
##' @param msa a multiple sequence alignment file or object
##' @return BStringSet based object
##' @importFrom Biostrings DNAStringSet
##' @importFrom Biostrings RNAStringSet
##' @importFrom Biostrings AAStringSet
##' @importFrom methods missingArg
##' @importFrom seqmagick fa_read
## @export
##' @author Lang Zhou and Guangchuang Yu
##' @noRd
prepare_msa <- function(msa) {
  supported_msa_class <- c("DNAStringSet",
                           "RNAStringSet",
                           "AAStringSet",
                           "BStringSet",
                           "DNAMultipleAlignment",
                           "RNAMultipleAlignment",
                           "AAMultipleAlignment",
                           "DNAbin",
                           "AAbin")


  if (missingArg(msa)) {
    stop("no input...")
  } else if (inherits(msa, "character")) {
    msa <- fa_read(msa)
  } else if (!class(msa) %in% supported_msa_class) {
    stop("multiple sequence alignment object no supported...")
  }

  res <- switch(class(msa),
                DNAbin = DNAbin2DNAStringSet(msa),
                AAbin = AAbin2AAStringSet(msa),
                DNAMultipleAlignment = DNAStringSet(msa),
                RNAMultipleAlignment = RNAStringSet(msa),
                AAMultipleAlignment = AAStringSet(msa),
                msa ## DNAstringSet, RNAStringSet, AAString, BStringSet
  )
  return(res)
}

