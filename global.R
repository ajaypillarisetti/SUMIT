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

### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

# install missing packages.
# list.of.packages <- c("shiny","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","shinyBS","scales")
# 	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

read.sum <- function(file, fname,tzone="GMT"){
	fileCheck <- file.info(file)$size>0
	if(fileCheck){
		#get device type from SUMS header; clean/format
	#get device type from SUMS header; clean/format
	notes <- head(read.csv(file, header=F),21)[,1]
	notes <- as.character(notes[1:21])
	device <- substring(strsplit(notes[1],":")[[1]][2],2,9)
	if(device=="DS1921G-"){device <- "DS1921G"}
	if(device=="DS1922/D"){device <- "DS1922X"}
 
	#find line at which to begin import
	startimport <- grep("Value",notes)
 
	#import SUMs file
	sums <- read.csv(file,skip=startimport, header=F, stringsAsFactors=F)
	sums$device <- device
	sums$sums_serial_no <- substring(notes[2],nchar(notes[2])-15,nchar(notes[2]))
	names(sums)[1:5]  <- c('datetime','unit','temp','sumstype','serial')
 
	#correct for fahrenheit files
	tempunit <- unique(as.character(sums$unit))
	if(tempunit==FALSE){sums$temp <- round((5/9)*(sums$temp - 32),3)}
	if(tempunit==FALSE){sums$unit <- "F"}
	#use cat to write alert to file -- remove grep(file,filelist), as is frame specific
	if(tempunit==FALSE){cat(Sys.time(), "   ALERT: ", file, " is in Fahrenheit \n")}
  
	#extract month, day, and year from first timestamp of actual data
	fulldate <- strsplit(sums[1,1]," ")[[1]][1]
 
	#determine the date delimiter
	if(grepl('/',fulldate)){delimiter <- "/"}
	if(grepl('-',fulldate)){delimiter <- '-'}	
 
	#separate date into three components
	firsts <- as.numeric(unique(sapply(strsplit(sums$datetime,delimiter),'[[',1)))
	seconds <-as.numeric(unique(sapply(strsplit(sums$datetime,delimiter),'[[',2)))
	thirds <- unique(sapply(strsplit(sums$datetime,delimiter),'[[',3))
	thirds <- unique(as.numeric(sapply(strsplit(thirds, " "),'[[',1)))
	#for the rare 2012 or 2013 in a SUMs file, reduce to two digit year
	if(all(nchar(thirds))==4){thirds <- as.numeric(substring(thirds,3,4))}
 
	#figure out sample duration in days
	numsamples <- as.numeric(strsplit(notes[grep("Mission Samples", notes)],":")[[1]][2])
	sampleinterval <- strsplit(notes[grep("Sample Rate", notes)],":")[[1]][2]
	#some the sums record 10 minute intervals; other say 600 seconds
	#catch the minutes and convert to seconds
	samplescale <- grepl('minute',sampleinterval)
	sampleinterval <- as.numeric(strsplit(sampleinterval," ")[[1]][4])
	if(samplescale){sampleinterval <- sampleinterval*60}
	sampletime <- (numsamples*sampleinterval)/(60*60*24)

	
	sampletimesec <- (numsamples*sampleinterval)
 
	#extract startdate from SUMS header - this is recorded by the device.
	startdate <- strsplit(notes[grep("Mission Start", notes)],":  ")[[1]][2]
	reform <- strsplit(startdate," ")
	startdate <- ymd_hms(paste(reform[[1]][6],reform[[1]][2],reform[[1]][3],reform[[1]][4]))
	enddate <- startdate + sampletimesec
	monthmatch <- month(startdate) == month(enddate)
 
	#from the reconstructed start date
	#extract the month, day, and year of the first sample
	#as logged in the SUMS header
	startmonth <- month(startdate)
	startday <- day(startdate)
	startyear <- as.numeric(substr(year(startdate),3,4))
 
	# 1. Multiday sample within a month
	# First, we need to confirm (1) the duration of the sample and (2) that samples occur within the same month.
	if(
		#if monthmatch == true, then all sampling is happening in the same month.
		monthmatch & 
		#if the number of unique values in the first and third position are equal
		length(unique(firsts)) == length(unique(thirds)) & 
		#and not equal to the second position
		length(unique(seconds))!= length(unique(firsts)) & 
		#and the third position matches startyear
		all(thirds==startyear)){
			sums$datetime <- mdy_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'mdy'
		}else if(
		monthmatch & 
		length(unique(firsts)) == length(unique(seconds)) & 
		length(unique(firsts))!= length(unique(thirds)) & 
		all(firsts==startyear)){
			sums$datetime <- ymd_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'ymd'
		}else if(
		monthmatch & 
		length(unique(seconds)) == length(unique(thirds)) & 
		length(unique(firsts))!= length(unique(thirds)) & 
		all(thirds==startyear)){
			sums$datetime <- dmy_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'dmy'
		}else if(
 
	# 2. multiday sample across a new year
	# month match == false -- more than one month
	# any value in the second field greater than 12
		#longer than a month
		monthmatch==F & 
		#second > 12 == day
		any(seconds>12) &
		#firsts in 12 or 1 -- dec or jan 
		all(firsts %in% c('12','1')) &
		#2012 and 2013 in thirds
		all(c(startyear,startyear+1) %in% thirds)
		){
			sums$datetime <- mdy_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'mdy'
		}else if(
	
	# 3. multiday sample throughout the year
		#longer than a month, ymd
		monthmatch==F & 
		all(firsts %in% startyear) &
		all(seconds %in% startmonth:month(enddate))&
		any(thirds>12 | length(thirds)>length(seconds))		
		){
			sums$datetime <- ymd_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'ymd'
		}else if(
		#longer than a month, dmy
		monthmatch==F & 
		all(thirds %in% startyear) &
		all(seconds %in% startmonth:month(enddate))&
		any(firsts>12 | length(firsts)>length(seconds))		
		){
			sums$datetime <- dmy_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'dmy'
		}else if(		
		#longer than a month, mdy
		monthmatch==F & 
		all(thirds %in% startyear) &
		all(firsts %in% startmonth:month(enddate))&
		any(seconds>12 | length(seconds)>length(firsts))		
		){
			sums$datetime <- mdy_hms(paste(as.character(sums[,1])),tz=tzone,truncated=1)
			format <- 'mdy'
		}

		sums <- as.data.table(sums)
		sums[,c('unit','serial'):=NULL]
		setnames(sums, 'sumstype', 'device_id')

	}else{warning(paste("File", file, "does not contain valid iButton data", sep=" "))}
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
