# SUMIT
### An R and Shiny-based tool for analyzing iButton Stove Use Monitor Data

###Overview
SUMIT is an easy-to-use and simple tool for preliminary analysis of SUMs data from iButtons. It is made for rapid visualization and analysis of a small number of files; it is not suited for analysis of large amounts of SUMs data. SUMIT requires R, some visualization and analysis libraries, and a modern web browser (Chrome recommended) to work. SUMIT currently accepts only csv files from Maxim's OneWireViewer, the free software used to download data from iButtons.

###Installing SUMIT

SUMIT can be run locally or on the web. To run SUMIT on your own computer, you must first download and install R (version 3.0 or later), available here:

[https://cran.rstudio.com](https://cran.rstudio.com)

Choose the file appropriate for your platform (Linux, Mac OS X, or Windows) and follow the onscreen instructions. After R is installed, run the following code to install required packages:


```R
# install missing packages.
list.of.packages <- c("shiny","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","shinyBS","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
```

SUMIT can be run in three ways after R and the appropriate packages are run:

```R
# 1. Use runGitHub
runGitHub("sumit", "ajaypillarisetti")

# 2. Run a zip file using runUrl
runUrl("https://github.com/ajaypillarisetti/SUMIT/archive/master.zip")
```

Finally, you can clone the source code of SUMIT from 

[https://github.com/ajaypillarisetti/SUMIT/archive/master.zip](https://github.com/ajaypillarisetti/SUMIT/archive/master.zip)
 

Clone the repository and use `runApp()`:

```R
# Clone with git. If you have cloned it into
# ~/sumit, cd to that directory and use runApp().
setwd("~/sumit")
runApp()
```

###Using SUMIT
#####Uploading Files
Use of SUMIT begins with temporarily uploading a file using its File Browser interface. Click the “Choose File” button to select a File for use in SUMIT. SUMIT accepts any iButton csv downloaded using Maxim’s free One Wire Viewer[^1].

![](https://raw.githubusercontent.com/ajaypillarisetti/SUMIT/master/documentation_images/Welcome_to_SUMIT.png)

After successful upload, a graph and a number of info boxes will be displayed in the Main SUMIT window.

![](https://raw.githubusercontent.com/ajaypillarisetti/SUMIT/master/documentation_images/Success_upload.png)

All graphs in SUMIT are fully interactive; click and drag to zoom in on specific portions of the graph that may be of interest (see below). Double-clicking on the graph zooms out. To the right of the graph, SUMIT displays summary information about the uploaded file. THis includes the total days of sampling, the number of samples logged, the logging interval, and two intelligent diagnostics to assess the health of the iButton. These diagnostics determine hte type of iButton used to collect data and indicate if the max or min temperatures have been exceeded. If so, as is the case in the example above, a warning symbol is displayed.[^2] 


![](https://raw.githubusercontent.com/ajaypillarisetti/SUMIT/master/documentation_images/Interact_with_%20Viewer.gif)

#####Analyzing Files
Click the SUMs Analysis Tab to select algorithms you can use to analyze your data. SUMIT currently contains 3 algorithms.

*Threshold Algorithm*
This algorithm looks at the time above a specific, user-set temperature. The slider on the page can be used to adjust the threshold. Like elsewhere, click and drag over the graph to zoom in; to zoom out, double click on the graph.

When you zoom and pan the graph, the information boxes above refresh reflecting the currently visible data. 

[^1]: To download One Wire Viewer, visit [Maxim](http://www.maximintegrated.com/en/products/ibutton/software/1wire/OneWireViewer.cfm). OWV works on Windows only. 
[^2]: We strongly advise not using any iButtons that show negative values or exceed their maximum temperature regularly. This buttons are error prone and may fail with data loss during a deployment.
