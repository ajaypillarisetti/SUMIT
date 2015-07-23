# SUMIT
## An R and Shiny-based tool for analyzing iButton Stove Use Monitor Data

SUMIT is an easy-to-use and extremely simple tool for preliminary analysis of SUMs data from iButtons. It is made for rapid visualization and analysis of a small number of files; it is not suited for analysis of large amounts of SUMs data. It requires R, some libraries, and a modern web browser (Chrome recommended) to work. It currently accepts *only* csv files from Maxim's OneWireViewer, the free software used to download data from iButtons.

To run SUMIT from R:

```R
# install missing packages.
list.of.packages <- c("shiny","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","shinyBS","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

# Use runGitHub
runGitHub("sumit", "ajaypillarisetti")

# Run a zip file using runUrl
runUrl("https://github.com/ajaypillarisetti/SUMIT/archive/master.zip")
```

Clone the repository and use `runApp()`:

```R
# Clone with git. If you have cloned it into
# ~/sumit, cd to that directory and use runApp().
setwd("~/sumit")
runApp()
```

A public beta of SUMIT is available at [householdenergy.shinyapps.io/sumit](http://householdenergy.shinyapps.io/sumit). For analysis of a large number of iButton SUMs files, we recommend [SUMSarizer](https://github.com/SUMSarizer/SUMSarizer), a machine-learning based tool that can analyze large volumes of SUMs data. 