# SUMIT
## An R and Shiny-based tool for analyzing iButton Stove Use Monitor Data

SUMIT is an easy-to-use, simple tool for analyzing SUMs data from iButtons. It requires R, some libraries, and a modern web browser (Chrome recommended) to work. It currently accepts *only* csv files from Maxim's OneWireViewer, the free software used to download data from iButtons.

There are many ways to download and run SUMIT:

```R
library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(xts)
library(shinydashboard)
library(scales)
library(devtools)
library(dygraphs)

# The simplest way is to use runGitHub
runGitHub("sumit", "ajaypillarisetti")

# Run a zip file directly
runUrl("https://github.com/ajaypillarisetti/SUMIT/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:

```R
# First clone the repository with git. If you have cloned it into
# ~/sumit, first go to that directory, then use runApp().
setwd("~/sumit")
runApp()
```

We will also make a public version of SUMIT available on the web. Stay tuned!