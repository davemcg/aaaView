# aaaView
Amino Acid Alignment Viewer

# Quick Start

in R (to install needed packages):
devtools::install_github('davemcg/aaaView')

on command line (to clone package):
git clone https://github.com/davemcg/aaaView.git
(yes I should really wrap this up to avoid this step, but maybe later)

# What it does
For a set of proteins visualize [MSA](https://en.wikipedia.org/wiki/Multiple_sequence_alignment) and build a tree across user specified proteins. aaaView is built around UniProt data, but can theoretically use generic protein fasta. Well, it will probably explode as the app parses out information from the fasta to use in the visualization....but in theory that could be worked around with some annoying coding. Which I haven't done yet. 

The app can also overlay known protein domains (via uniprot?) over the MSA to help with quick(er) interpretation of the substance of evolutionary differences.

# Some related tools

  - https://github.com/plotly/react-msa-viewer
    - plotly based viewer
  - http://yulab-smu.top/ggmsa/index.html
    - "gg" like visualization
    - Can't use standard geoms? 
    - Slow and odd plot result (vertically squished) with large proteins (Abca4; over 2000 AA)
    - can set start/stop to print subsets of AA
    - also includes several custom geoms
  - http://www.bioinf.jku.at/software/msa/
    - does MSA with ClustalW, ClustalOmega, or Muscle
    - MSA visualization done with LaTex
    - Challenging to use their "viz" in a Shiny / reactive environment as output is either pdf or Tex code
  - https://github.com/mhahsler/rBLAST/
    - R interface for blast
    - blast itself needs to be installed
  - https://github.com/vragh/seqvisr
    - MSA visualization package
    - Doesn't show AA - so more of a "graphic"
  - https://github.com/GMOD/jbrowse-plugin-msaview 
    - jbrowse2 plugin....and jbrowseR package can display in Shiny
    - not certain if I can get msaview into Shiny via jbrowseR....
    - nope for now: https://github.com/GMOD/jbrowse-components/discussions/3125
  - http://bioconductor.org/packages/release/bioc/html/DECIPHER.html
    - http://www2.decipher.codes/Gallery.html
  - https://github.com/pachterlab/gget
    - python / command line tool suite that, among MANY things, does MSA from a fasta file via muscle  
    



