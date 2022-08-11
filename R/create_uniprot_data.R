#' Download aaaView UniProt (Swiss Pro) data
#'
#' @return Puts peviz_uniprot_data.Rdata in app folder
#' @examples
#' \dontrun{create_uniprot_data()}
#' @export
create_uniprot_data <- function(){
  library(dplyr)
  download.file('https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz', destfile = 'uniprot_sprot.fasta.gz')
  uniprotDB <- Biostrings::readAAStringSet('uniprot_sprot.fasta.gz')
  #extract protein names
  proteins <- uniprotDB@ranges %>% data.frame() %>% as_tibble(rownames = 'index') %>%
    mutate(org = stringr::str_extract(names, 'OS.*OX') %>% gsub('OS\\=| OX','',.),
           gene = gsub('\\w+\\|\\w+\\|','', names) %>% gsub('_.*','',.)) %>%
    rowwise() %>%
    mutate(id = stringr::str_split(names, '\\|| ')[[1]][2]) %>%
    ungroup()
  dir.create(paste0(system.file('app', package = "aaaView"), '/data/'), showWarnings = FALSE)
  save(proteins, uniprotDB, file = paste0(system.file('app', package = "aaaView"), '/data/peviz_uniprot_data.Rdata'), compress = FALSE)
}
