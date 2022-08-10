# aaaView
<b>A</b>mino <b>A</b>cid <b>A</b>lignment <b>View</b>er

# Two visualizations
## MSA (with UniProt feature underlaid)
<img width="800" alt="Screen Shot 2022-08-10 at 4 34 45 PM" src="https://user-images.githubusercontent.com/10225430/184014939-86cf3c0b-5c20-41f3-9d28-18485fbe18b6.png">

## Tree
<img width="800" alt="Screen Shot 2022-08-10 at 4 34 51 PM" src="https://user-images.githubusercontent.com/10225430/184014951-7347a509-dcb2-4ffa-b77a-59a358b654c1.png">

# In action
https://user-images.githubusercontent.com/10225430/183989784-67965cf0-9bec-4628-8072-785dbe87edd3.mp4




# Quick Start

in R:
```
devtools::install_github('davemcg/aaaView')
library(aaaView)
# install latest uniprot swiss-prot database for app
create_uniprot_data() # downloads latest swiss prot db and turns them into two R objects for the app. Takes a few minutes to run. Go get a snack.
# ALTERNATIVELY download pre-made data file (made on August 10th, 2022)
# and then unzip and move to proper dir for app
system("wget -q https://hpc.nih.gov/~mcgaugheyd/aaaView/peviz_uniprot_data.Rdata.gz")
system("gunzip peviz_uniprot_data.Rdata.gz"); system(paste0("mkdir -p ", system.file('app', package = "aaaView"), '/data/'))
system(paste0("mv peviz_uniprot_data.Rdata ", system.file('app', package = "aaaView"), '/data/'))
# OK
launch() # should launch the app!
```

# What it does
For a user-defined set of proteins visualize [MSA](https://en.wikipedia.org/wiki/Multiple_sequence_alignment) and build a relationahip tree. aaaView is built around UniProt data, but can theoretically use generic protein fasta. Well, it will probably explode as the app parses out information from the fasta to use in the visualization....but in theory that could be worked around with some annoying coding. Which I haven't done yet. 

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
    



