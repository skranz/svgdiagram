example.gv.to.svg = function(){
  gv = "
digraph boxes {
c [label='c'];
p [label='p'];
S [label='S'];
eta;
eps [label='epsilon'];
q [label='q = beta0 + beta1 p + eps'];

c->p;
S->p;
p->q;
S->eps;
eta->eps;
eps -> q;
}
"
  svg = gv.to.svg(gv)
  setwd("D:/libraries/svgdiagram")
  gv = "iv.gv"
  svg = gv.to.svg(gv)
  svg

  app = eventsApp()
  app$ui = bootstrapPage(
    HTML(svg)
  )
  viewApp(app)
}

.ViZGloB = new.env()

#' @export
get.viz.v8 = function() {
  if (is.null(.ViZGloB$v8)) {
    ctx = .ViZGloB$v8 = v8()
    file = paste0(path.package("svgdiagram"),"/www/viz.js")
    ctx$source(file)
  }
  .ViZGloB$v8
}

sep.lines = function (txt, collapse = "\n") {
  if (length(txt) > 1)
      txt = merge.lines(txt, collapse)
  stringr::str_split(txt, collapse)[[1]]
}

make.viz.svg.org = function(dot, ctx = get.viz.v8()) {
  restore.point("make.viz.svg.org")

  str = paste0(sep.lines(dot), collapse="\n")
  str = gsub(x = str, "'", "\"")

  ctx$call("Viz",str)
}


make.viz.mem = memoise(make.viz.svg.org)

#' Create a svg file from a gv (Graphiviz) file or text
#' @param gv filename or source code of graphiviz diagram
#' @param use.memoise if TRUE memoise results
#' @param to.clipboard copy SVG source code to cliboard
#' @param out.file if not NULL save to outfile
#' @export
gv.to.svg = function(gv, use.memoise=TRUE, to.clipboard=is.null(out.file), out.file = NULL, adapt.latex=TRUE) {
  restore.point("gv.to.svg")

  # Check for a connection or file
  if (inherits(gv, "connection") || file.exists(gv)){
    gv <- readLines(gv, warn = FALSE)
    gv <- paste0(gv, collapse = "\n")
  } else {
    # Check for vector with length > 1 and concatenate
    if (length(gv) > 1){
      gv <- paste0(gv, collapse = "\n")
    }
  }
  if (use.memoise) {
    svg = make.viz.mem(gv)
  } else {
    svg = make.viz.org(gv)
  }
  Encoding(svg) = "UTF-8"
  if (adapt.latex)
    svg = adapt.svg.gv.latex(svg)
  if (!is.null(out.file))
    writeLines(svg, out.file,useBytes = TRUE)
  if (to.clipboard)
    writeClipboard(svg)
  if (!is.null(out.file)) return(invisible(svg))
  svg
}

adapt.svg.gv.latex = function(svg) {
  restore.point("adapat.svg.gv.latex")
  library(stringtools)
  svg = sep.lines(svg)
  lines = str.starts.with(svg,"<text ")
  txt = svg[lines]
  right = str.left.of(str.right.of(txt,">"),"</text>")
  left = str.left.of(txt,">")
  right =sapply(right,latex.to.textspan)
  svg[lines] = paste0(left,">", right,"</text>")
  merge.lines(svg)
}

gvToSvgAddin = function() {

  library(svgdiagram)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("gvToSvgAddin")
  file = basename(doc$path)
  dir = dirname(doc$path)

  ext = tools::file_ext(file)
  if (!identical(ext,"gv")) {
    cat("\nRStudio has not a .gv file in the current tab.")
    return()
  }
  out.file = paste0(dir,"/", tools::file_path_sans_ext(file),".svg")
  cat("\nCreate svg, this can take a while...")
  gv.to.svg(gv=doc$path,out.file=out.file,to.clipboard = TRUE)
  cat(paste0("\nsvg code copied to clipboard and saved as \n", out.file))
}

