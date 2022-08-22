#' Download aaaView UniProt (Swiss Pro) data
#'
#' @return Puts peviz_uniprot_data.Rdata in app folder wtihin aaaView R library
#' @examples
#' \dontrun{create_uniprot_data()}
#' @importFrom Biostrings readAAStringSet
#' @importFrom dplyr as_tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @param local Whether to use a local uniprot fasta
#' @param uniprot_sprot Path to the uniprot_sprot file which can be downloaded from https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz
#' @export
create_uniprot_data <- function(local = FALSE, uniprot_sprot = 'uniprot_sprot.fasta.gz'){
  library(dplyr)
  options(timeout = max(300, getOption("timeout")))
  if (local){
    uniprotDB <- readAAStringSet(uniprot_sprot)
  } else {
    download.file('https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz', destfile = 'uniprot_sprot.fasta.gz')
    uniprotDB <- readAAStringSet('uniprot_sprot.fasta.gz')
  }
  #extract protein names
  proteins <- uniprotDB@ranges %>% data.frame() %>% as_tibble(rownames = 'index') %>%
    mutate(org = str_extract(names, 'OS.*OX') %>% gsub('OS\\=| OX','',.),
           gene = gsub('\\w+\\|\\w+\\|','', names) %>% gsub('_.*','',.)) %>%
    rowwise() %>%
    mutate(id = str_split(names, '\\|| ')[[1]][2]) %>%
    ungroup()
  dir.create(paste0(system.file('app', package = "aaaView"), '/data/'), showWarnings = FALSE)
  save(proteins, uniprotDB, file = paste0(system.file('app', package = "aaaView"), '/data/peviz_uniprot_data.Rdata'), compress = FALSE)
}
