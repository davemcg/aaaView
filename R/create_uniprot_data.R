#' Download aaaView UniProt (Swiss Pro) data
#'
#' @return Puts peviz_uniprot_data.Rdata in app folder
#' @examples
#' \dontrun{create_uniprot_data()}
#' @export
create_uniprot_data <- function(){
  library(dplyr)
  system('wget -q https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz')
  uniprotDB <- Biostrings::readAAStringSet('uniprot_sprot.fasta.gz')
  #extract protein names
  proteins <- uniprotDB@ranges %>% data.frame() %>% as_tibble(rownames = 'index') %>%
    mutate(org = stringr::str_extract(names, 'OS.*OX') %>% gsub('OS\\=| OX','',.),
           gene = gsub('\\w+\\|\\w+\\|','', names) %>% gsub('_.*','',.)) %>%
    rowwise() %>%
    mutate(id = stringr::str_split(names, '\\|| ')[[1]][2]) %>%
    ungroup()
  system(paste0('mkdir -p ', system.file('app', package = "aaaView"), '/data/'))
  save(proteins, uniprotDB, file = paste0(system.file('app', package = "aaaView"), '/data/peviz_uniprot_data.Rdata'), compress = FALSE)
}
