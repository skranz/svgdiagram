example.mermaid = function() {
  library(rmdtools)
  setwd("D:/libraries/svgdiagram/")
  mmd = text = readLines("intervention.mmd")
  svg = mermaid.svg(text)
  ui = tagList(
    tags$style('
      g.label {
        color: black;
      }
      .edgeLabel {
        background-color: white;
      }
    '),
    HTML(svg)
  )

  view.html(ui=ui)
}

mermaid.svg = function(text=NULL, width=400) {
  restore.point("mermaid.svg.org")

  dir = tempdir()
  file = tempfile(tmpdir=dir, fileext=".mmd")
  writeLines(text, file)
  com = paste0("mermaid -s -w ", width," -o ",dir," ", file)
  system(com,ignore.stdout = TRUE)
  svg.file = paste0(file,".svg")
  svg = paste0(readLines(svg.file, warn = FALSE), collapse="\n")
  svg
}


