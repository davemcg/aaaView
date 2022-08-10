library(shiny)
library(msa)
library(dplyr)
library(httr)
library(jsonlite)
library(xml2)
library(ggplot2)
# input in uniprot db
#load('inst/app/data/peviz_uniprot_data.Rdata')


server <- function(input, output, session) {

  # load database ------
  db <- reactive({
    req(input$Database)
    if (input$Database == 'Swiss-Prot'){
      #cat('sp')
      load('./data/peviz_uniprot_data.Rdata')
    } else {
      req(input$local_fasta)
      ext <- tools::file_ext(input$local_fasta$datapath)
      if (ext == 'gz'){
        system(paste('gunzip', input$local_fasta$datapath))
        datapath <- gsub('.gz','',input$local_fasta$datapath)
      } else {
        datapath <- input$local_fasta$datapath
      }
      uniprotDB <- Biostrings::readAAStringSet(datapath)

      #extract protein names
      proteins <- uniprotDB@ranges %>% data.frame() %>% as_tibble(rownames = 'index') %>%
        mutate(org = stringr::str_extract(names, 'OS.*OX') %>% gsub('OS\\=| OX','',.),
               gene = gsub('\\w+\\|\\w+\\|','', names) %>% gsub('_.*','',.)) %>%
        rowwise() %>%
        mutate(id = stringr::str_split(names, '\\|| ')[[1]][2]) %>%
        ungroup()
    }
    output <- list()
    output$uniprotDB <- uniprotDB
    output$proteins <- proteins
    output
  })


  observe({
    query <- parseQueryString(session$clientData$url_search)
    # selectize for protein names ------
    if (is.null(query[['uniprot']])){
      req(input$Database)
      updateSelectizeInput(session, 'uniprot',
                           choices = db()$proteins$names,
                           options = list(placeholder = 'Type to search'),
                           selected = '',
                           server = TRUE)
    }
    # selectize for protein annotation
    observeEvent(input$uniprot, {
      updateSelectizeInput(session, 'primary_protein',
                           choices = input$uniprot,
                           options = list(placeholder = 'Optional'),
                           selected = '',
                           server = TRUE)
    })
    # selectize for color
    if (is.null(query[['color']])){
      updateSelectizeInput(session, 'color',
                        choices = colnames(ggmsa:::scheme_AA)[colnames(ggmsa:::scheme_AA) != 'CN6'],
                        selected = 'Chemistry_AA',
                        server = TRUE)

    }
  })



  draw_msa <- eventReactive(input$Draw, {

    # testing
    # input <- list()
    # input$primary_protein <- 'sp|P26367|PAX6_HUMAN Paired box protein Pax-6 OS=Homo sapiens OX=9606 GN=PAX6 PE=1 SV=2'
    # input$uniprot <- proteins %>% filter(grepl('PAX6_', names)) %>% pull(names)
    # input$color <- 'Chemistry_AA'
    # select proteins
    indi <- db()$proteins %>%  filter(names %in% input$uniprot) %>% pull(index)
    msa_align <- msa(db()$uniprotDB[as.integer(indi)], method = input$method)

    # build color
    color_pick <- ggmsa:::scheme_AA[, input$color]
    names(color_pick) <- row.names(ggmsa:::scheme_AA)

    # extract msa order
    order <- unmasked(msa_align) %>% data.frame() %>% row.names()
    org_order <- stringr::str_extract(order, 'OS.*OX') %>% gsub('OS\\=| OX','',.)
    gene_order <- stringr::str_split(order, '\\|| ') %>% purrr::map(3) %>% unlist() %>% gsub('_.*','',.)
    id_order <- stringr::str_split(order, '\\|| ')%>% purrr::map(2) %>% unlist()
    order_full <- c('Consensus', paste(org_order, gene_order, id_order, sep = ' | '))

    # add consensus sequence
    consensus <- AAStringSet(x=msaConsensusSequence(msa_align))
    names(consensus) <- 'Consensus'
    tidy_msa <- ggmsa::tidy_msa(c(msa_align@unmasked, consensus) )

    # if uniprot annotations desired ------
    if (input$primary_protein != ''){
      #cat(input$primary_protein)
      selected_protein <- stringr::str_split(input$primary_protein,
                                             '\\|')[[1]][2]

      requestURL <- paste0("https://www.ebi.ac.uk/proteins/api/features?offset=0&size=-1&accession=", selected_protein)
      r <- GET(requestURL, accept("application/json"))

      #stop_for_status(r)

      json <- toJSON(content(r))
      uniprot_domains <- fromJSON(json)$features[[1]]  %>% filter(category == 'DOMAINS_AND_SITES')
      # expand sequence range
      expand_seq <- uniprot_domains %>%
        dplyr::select(evidences, begin, end) %>%
        transmute(evidences, seq = purrr::map2(begin, end, seq, by = 1)) %>%
        tidyr::unnest(cols = seq) %>%
        distinct() %>%
        data.frame() %>%
        left_join(uniprot_domains %>% dplyr::select(type, description, evidences), by = 'evidences')
      # create new data frame with UniProt annotation
      select_protein_aa <- ggmsa::tidy_msa(msa_align@unmasked ) %>% filter(name == input$primary_protein)
      select_protein_aa <- select_protein_aa %>% filter(character != '-') %>%
        mutate(seq = row_number()) %>% left_join(expand_seq, by = 'seq') %>%
        filter(evidences != 'NULL') %>%
        mutate(Protein = paste0(type, ': ', description)) %>%
        dplyr::select(Protein, position) %>%
        mutate(character = '^')

      # set order
      order_full <- c(order_full, select_protein_aa$Protein %>% unique())
      plot_data <- tidy_msa %>%
        mutate(Type = 'Amino Acid') %>%
        bind_rows(select_protein_aa %>% mutate(Type = 'UniProt Annotation')) %>%
        mutate(group =
                 substr(formatC(position, width = 5, format = "d", flag = "0"), 1, 3),
               Position =
                 substr(formatC(position, width = 5, format = "d", flag = "0"), 4, 5)) %>%
        left_join(., db()$proteins[indi,], by = c('name' = 'names')) %>%
        mutate(Protein = case_when(name == 'Consensus' ~ 'Consensus',
                                   Type == 'Amino Acid' ~ paste(org, gene, id, sep = ' | '),
                                   TRUE ~ Protein),
               Protein = factor(Protein, levels = rev(order_full))) %>%
        rename(AA = character)
      plot_data$group <- factor(plot_data$group)
      levels(plot_data$group) = paste0(plot_data$group, " (Hundreds Position)") %>% unique()
      plot <- plot_data %>%
        ggplot(aes(x=Position,y=Protein, label = AA, fill = AA)) +
        ggforce::facet_col(~ group + Type, scales = 'free_y', space = 'free')
      height <- plot_data %>% select(group, Protein, Type) %>% unique() %>% nrow() * 35
    } else { # no annotation plot
      plot_data <- tidy_msa %>%
        mutate(group =
                 substr(formatC(position, width = 5, format = "d", flag = "0"), 1, 3),
               Position =
                 substr(formatC(position, width = 5, format = "d", flag = "0"), 4, 5)) %>%
        left_join(., db()$proteins[indi,], by = c('name' = 'names')) %>%
        mutate(Protein = case_when(name != 'Consensus' ~ paste(org, gene, id, sep = ' | '),
                                   TRUE ~ 'Consensus'),
               Protein = factor(Protein, levels = rev(order_full))) %>%
        rename(AA = character)
      plot_data$group <- factor(plot_data$group)
      levels(plot_data$group) = paste0(plot_data$group, " (Hundreds Position)") %>% unique()
      plot <- plot_data %>%
        ggplot(aes(x=Position,y=Protein, label = AA, fill = AA)) +
        facet_wrap(~group, scales = 'free_y', ncol = 1)
      height <- plot_data %>% select(group, Protein) %>% unique() %>% nrow() * 25
    }


    output <- list()
    #output$msa <- msa_align
    #output$plot <- plot
    #output
    output$plot <-  plot +
      geom_raster() +
      geom_text() +
      #cowplot::theme_cowplot() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())  +
      scale_fill_manual(values = color_pick) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      xlab("Position (tens)")
    output$height <- height
    output$fasta <- c(msa_align@unmasked, consensus) %>% data.frame() %>% as_tibble(rownames = 'names')
    colnames(output$fasta)[2] <- 'fasta'
    output
  })

  draw_tree <- eventReactive(input$Draw, {
    indi <- db()$proteins %>%  filter(names %in% input$uniprot) %>% pull(index)
    msa_align <- msa(db()$uniprotDB[as.integer(indi)], method = input$method)
    options(ignore.negative.edge=TRUE)
    msa_align2 <- msaConvert(msa_align, type="seqinr::alignment")

    org <- stringr::str_extract(msa_align2$nam, 'OS.*OX') %>% gsub('OS\\=| OX','',.)
    gene <- stringr::str_split(msa_align2$nam, '\\|| ') %>% purrr::map(3) %>% unlist() %>% gsub('_.*','',.)
    id <- stringr::str_split(msa_align2$nam, '\\|| ')%>% purrr::map(2) %>% unlist()
    msa_align2$nam <- paste(org, gene, id, sep = ' | ')

    d <- seqinr::dist.alignment(msa_align2, "identity")
    Tree <- ape::nj(d)
    ggtree::ggtree(Tree) + ggtree::geom_tiplab() +
      coord_cartesian(clip="off") +
      theme(plot.margin = margin(1,8,1,1, "cm"))
  })


  output$msa <- renderPlot({
    draw_msa()$plot
  }, height =
    eventReactive(input$Draw, {draw_msa()$height})
  )

  output$tree <- renderPlot({
    draw_tree()
  })


  output$fasta_table <- DT::renderDataTable({
    fa <- draw_msa()$fasta
    fa <- fa %>% mutate(fasta = gsub('-','',fasta))
    fa %>% DT::datatable()
  })

  output$fasta_aligned_download <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '_aligned.fasta', sep='')
       },
       content = function(con) {
         fa <- draw_msa()$fasta
         fa %>% filter(names != 'Consensus')
         seqinr::write.fasta(as.list(draw_msa()$fasta %>% pull(2)), draw_msa()$fasta %>% pull(1), file.out = con)
       }
     )

  output$fasta_download <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.fasta', sep='')
    },
    content = function(con) {
      fa <- draw_msa()$fasta
      fa %>% filter(names != 'Consensus')
      seqinr::write.fasta(as.list(fa %>% pull(2) %>% gsub('-','',.)), fa %>% pull(1), file.out = con)
    }
  )
}
