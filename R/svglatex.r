

latex.to.textspan = function(str) {
  #str = "x_{5ab} y_{1} z_3"
  restore.point("latex.to.textspan")

  #str = "x_{5ab}\\alpha * \\beta"

  #str = "E^{*}"
  str = replace.latex.with.unicode(str)

  txt = str
  txt = svg.change.subscripts(txt,super = FALSE)
  txt = svg.change.subscripts(txt,super = TRUE)

  if (!identical(txt,str)) {
    txt = paste0("<tspan>",txt,"</tspan>")
  }

  # remove curley braces
  txt = gsub("{{","jJj",txt, fixed=TRUE)
  txt = gsub("}}","hHh",txt, fixed=TRUE)
  txt = gsub("{","",txt, fixed=TRUE)
  txt = gsub("}"," ",txt, fixed=TRUE)
  txt = gsub("  "," ",txt, fixed=TRUE)
  txt = gsub("jJj","{{",txt, fixed=TRUE)
  txt = gsub("hHh","}}",txt, fixed=TRUE)

  txt
}

svg.change.subscripts = function(str, add.tspan = FALSE, super=FALSE) {
  restore.point("svg.change.subscripts")

  if (!super) {
    char = "_"
    class = "label_subscript"
    sign = 0.75
  } else {
    char = "\\^"
    class = "label_superscript"
    sign = -0.75
  }

  li = find.subscripts(str,char=char)$s
  if (length(li)==1) {
    txt = li
  } else {
    #if (length(li) %% 2 ==1) li = c(li,"")
    sub = seq(2, length(li),by=2)
    li[sub] = paste0('<tspan dy="',5*sign,'" style="font-size: 75%;" class="', class,'">', li[sub],'</tspan>')
    li[-sub] = paste0('<tspan dy="',-5*sign,'">', li[-sub],'</tspan>')

    txt = paste0(li,collapse="")
    if (add.tspan) {
      txt = paste0("<tspan>",txt,"</tspan>")
    }
  }
  txt
}

find.subscripts = function(str, char = "_") {
  restore.point("find.subscripts")


  # find subscripts
  pos1 = str.find(str,paste0(char,'[0-9a-zA-Z|.=]+'),fixed=FALSE)
  pos2 = str.find(str,paste0(char,'\\{[0-9a-zA-Z_|.=,*+-°]+\\}'),fixed=FALSE)
  pos = rbind(pos1,pos2)
  if (NROW(pos)==0) {
    return(list(s=str,is.sub=FALSE))
  }

  spl = str.split.at.pos(str,pos,keep.pos = TRUE)
  first = pos[1,1]==1
  if (first) {
    is.sub = rep(c(TRUE,FALSE),length.out=length(spl))
  } else {
    is.sub = rep(c(FALSE,TRUE),length.out=length(spl))
  }
  spl[is.sub] = substring(spl[is.sub],2)



  list(s=spl, is.sub=is.sub)

}

replace.latex.with.unicode = function(str) {

  latex = c( "\\alpha","\\beta","\\gamma","\\delta","\\epsilon","\\zeta","\\eta","\\theta","\\iota","\\kappa","\\lambda","\\mu","\\nu","\\xi","\\pi","\\rho","\\varsigma","\\sigma","\\tau","\\upsilon","\\phi","\\chi","\\psi","\\omega","\\Gamma","\\Delta","\\Theta","\\Lambda","\\Xi","\\Pi","\\Sigma","\\Upsilon","\\Phi","\\Psi","\\Omega","\\neg","\\pm","\\cdot","\\to","\\Rightarrow","\\Leftrightarrow","\\forall","\\partial","\\exists","\\emptyset","\\nabla","\\in","\\notin","\\prod","\\sum","\\surd","\\infty","\\wedge","\\vee","\\cap","\\cup","\\int","\\approx","\\neq","\\equiv","\\leq","\\geq","\\subset","\\supset","\\^circ","\\times","\\lfloor","\\rfloor","\\lceil","\\rceil" )

uc = c( "\U3B1","\U3B2","\U3B3","\U3B4","\U3B5","\U3B6","\U3B7","\U3B8","\U3B9","\U3BA","\U3BB","\U3BC","\U3BD","\U3BE","\U3C0","\U3C1","\U3C2","\U3C3","\U3C4","\U3C5","\U3C6","\U3C7","\U3C8","\U3C9","\U393","\U394","\U398","\U39B","\U39E","\U3A0","\U3A3","\U3A5","\U3A6","\U3A8","\U3A9","\U00AC","\U00B1","\U00B7","\U2192","\U21D2","\U21D4","\U2200","\U2202","\U2203","\U2205","\U2207","\U2208","\U2209","\U220F","\U2211","\U221A","\U221E","\U2227","\U2228","\U2229","\U222A","\U222B","\U2248","\U2260","\U2261","\U2264","\U2265","\U2282","\U2283","\U00B0","\U00D7","\U230A","\U230B","\U2308","\U2309" )

  pos = str.find(str,'\\\\[0-9a-zA-Z]+',fixed=FALSE)
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)
  ind = match(spl, latex)
  rows = !is.na(ind)
  spl[rows] = uc[ind[rows]]

  res = paste0(spl,collapse="")
  Encoding(res) = "UTF-8"
  res
}
