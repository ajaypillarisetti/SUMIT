# SUMIT
## An R and Shiny-based tool for analyzing iButton Stove Use Monitor Data

SUMIT is an easy-to-use, simple tool for analyzing SUMs data from iButtons. It requires R, some libraries, and a modern web browser (Chrome recommended) to work. It currently accepts *only* csv files from Maxim's OneWireViewer, the free software used to download data from iButtons.

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

We will also make a public version of SUMIT available on the web. Stay tuned!