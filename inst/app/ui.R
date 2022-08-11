library(shiny)

ui <- fluidPage(

  titlePanel("Aminio Acid Alignment Viewer"),

  fluidPage(

    fluidRow(
      column(8,
             selectInput(inputId = 'Database',
                         label = 'Database:',
                         selected = 'Swiss-Prot',
                         choices = c('Swiss-Prot', 'Local UniProt Fasta')),
             conditionalPanel(condition = "input.Database == 'Local UniProt Fasta'",
                              fileInput('local_fasta', 'Local UniProt Fasta', multiple = FALSE)),
             selectizeInput(inputId = "uniprot",
                            label = "Select proteins:",
                            choices=NULL,
                            multiple=TRUE,
                            width = '100%'
             ))),
    fluidRow(
      column(8,
             selectizeInput(inputId = "primary_protein",
                            label = "Add UniProt features from:",
                            choices=NULL,
                            multiple=FALSE,
                            options = list(placeholder = 'Optional'),
                            width = '100%'
             ))),
    fluidRow(
      column(3,
             selectizeInput(inputId = 'color',
                            label ='Color Scheme',
                            choices= NULL,
                            multiple = FALSE,
                            options = list(placeholder = 'Select Color Scheme'))),
      column(3,
             selectizeInput(inputId = 'method',
                            label ='MSA Method',
                            choices= c('ClustalW','ClustalOmega','Muscle'),
                            multiple = FALSE,
                            options = list(placeholder = 'Select MSA Method')))),
    fluidRow(
      column(8,
             actionButton('Draw','Draw', alt =  'Draw Plots',
                          style='background-color: #3269FF; color: #ffffff'))

    ),
    br(), br(),

    fluidRow(width = 11,
             plotOutput("msa", height = "auto"),
             plotOutput("tree")
    ),
    br(), br(),
    fluidRow(width = 8,
             div(DT::DTOutput('fasta_table'), style='font-size:75%'))
  ),
  br(),
  fluidRow(column(4,
                  downloadButton('fasta_download', 'Download Fasta',
                                 style='background-color: #3269FF; color: #ffffff')),
           column(4,
                  downloadButton('fasta_aligned_download', 'Download Aligned Fasta',
                                 style='background-color: #3269FF; color: #ffffff'))
  ),
  br(), br()
)
