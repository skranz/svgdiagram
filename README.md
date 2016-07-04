## svgdiagram

Author: Sebastian Kranz, Ulm University

Create svg files from graphviz .gv files.


### Usage 

First install the package github:

```r
devtools::install_github("skranz/svgdiagram")
```


Create a graph file with extension ".gv" and open it in RStudio. See the corresponding documentation in the DiagrammeR package:

[http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html](http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html)

Now in your RStudio Addins, run the addin gv to svg. This will create an svg file with the same name in the same folder as your .gv file.

You can also call the function `gv.to.svg` in your R code.



