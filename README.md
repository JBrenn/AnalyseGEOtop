
======
# AnalyseGEOtop
R package for GEOtop simulation analysis including functionality for
* creating GEOtop input maps for a specific basin (using SAGA GIS library - RSAGA)
* read single / multiple point output from GEOtop simulation 
* GEOtop output map animation (using ImageMagick)
* diagnostic plots on hydrological budget in GEOtop 3d simulation


# How to start

First install the package with:

```R
install.packages("devtools")
library(devtools)
install_github("JBrenn/AnalyseGEOtop")
```

and then import the library with:

```R
library(AnalyseGeotop)
```
