### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N



Sys.setenv(TZ="GMT")

shinyServer(function(input, output) {

	#read in data
	datasetInput <- reactive({
	    inFile <- input$files
    	if (is.null(inFile)){return(NULL)} 
		dta<- read.sum(inFile$datapath[1], inFile$name[1])
	})

	datasetName <- reactive({
	    inFile <- input$files
    	if (is.null(inFile)){return(NULL)} 
		inFile$name[1]
	})

	ambInput <- reactive({
	    inFile <- input$amb
    	if (is.null(inFile)){return(NULL)} 
		dta<- read.sum(inFile$datapath[1], inFile$name[1])
	})

	procInput <- reactive({
	    inFile <- input$processedfiles
    	if (is.null(inFile)){return(NULL)} 
    	all <- NULL
    	for(i in inFile$datapath){
    		a <- fread(i)
    		a[,filename:=inFile$name[inFile$datapath==i]]
    		print(a)
    		all <- rbind(all,a)
    	}
    	all
	})

	data_cleaned <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- datasetInput()[,with=F]
	})

	amb_cleaned <- reactive({
		if (is.null(ambInput())) return(NULL)
		data_d <- ambInput()[,with=F]
	})

	####################
	##### datasets ##### 
	####################
	dataXTS.plainplot <- reactive({
		dta<-data_cleaned()
		as.xts(dta[,c('temp'), with=F], order.by=dta$datetime, tz="GMT")
	})

	data.threshold <- reactive({
		dta<-as.data.frame(data_cleaned())
		dta$threshold <- input$tempThreshold
		dta$event[dta$temp>dta$threshold] <- 1000
		dta <- as.data.table(dta)
		dta[,yday:=yday(datetime)]
		dta[,rnum:=1:nrow(dta)]
		dta[is.na(event), rnum:=0]
		dta[,rnum.diff:=c(diff(rnum),NA)]
		dta[rnum.diff<0, rnum.diff:=1]
	})

	data.sdthreshold <- reactive({
		dta <- data_cleaned()
		dta[,day:=as.Date(datetime)]
		dta[,dailyMean:=mean(temp),by='day']
		dta[,dailySD:=sd(temp),by='day']
		dta<-as.data.frame(dta)
		dta$threshold <- (input$sdThreshold*dta$dailySD) + dta$dailyMean
		dta$event[dta$temp>dta$threshold] <- 1000
		dta <- as.data.table(dta)
		dta[,yday:=yday(datetime)]
		dta[,rnum:=1:nrow(dta)]
		dta[is.na(event), rnum:=0]
		dta[,rnum.diff:=c(diff(rnum),NA)]
		dta[rnum.diff<0, rnum.diff:=1]
	})

	data.ambthreshold <- reactive({
		dta2 <- copy(data_cleaned()[,c('datetime','temp'), with=F])
		amb <- copy(amb_cleaned()[,c('datetime','temp'), with=F])
		setnames(amb, 'temp', 'amb')
		dta2[,device_id:=NULL]
		amb[,device_id:=NULL]
		dta2[,datetime:=round.minutes(datetime,fileSamplingInterval())]
		amb[,datetime:=round.minutes(datetime,fileSamplingInterval())]
		dta_amb <- merge(dta2, amb, by='datetime', all=T)
		dta_amb[,day:=as.Date(datetime)]
		dta_amb[,hday:=hour(datetime)]
		dta_amb[,mean.amb:=mean(amb, na.rm=T),by='day,hday']
		dta_amb[,sd.amb:=sd(amb, na.rm=T),by='day,hday']
		dta_amb[,slope:=c(NA,diff(temp,1, na.rm=T))]
		dta<-as.data.frame(dta_amb)
		dta$threshold <- (input$ambThreshold*dta$sd.amb) + dta$amb
		dta$event[dta$temp>(dta$threshold) & dta$temp>input$ambThresholdplus & dta$slope>-10] <- 1000
		dta <- as.data.table(dta)
		dta[,yday:=yday(datetime)]
		dta[,rnum:=1:nrow(dta)]
		dta[is.na(event), rnum:=0]
		dta[,rnum.diff:=c(diff(rnum),NA)]
		dta[rnum.diff<0, rnum.diff:=1]
	})

	################################
	##### XTS-ify for Dygraphs ##### 
	################################
	dataXTS.threshold <- reactive({
		dta <- data.threshold()
		as.xts(dta[,c('temp','threshold','event'), with=F], order.by=dta$datetime, tz="GMT")
	})

	dataXTS.sdthreshold <- reactive({
		dta <- data.sdthreshold()
		as.xts(dta[,c('temp','threshold','event'), with=F], order.by=dta$datetime, tz="GMT")
	})

	dataXTS.ambthreshold <- reactive({
		dta <- data.ambthreshold()
		as.xts(dta[,c('amb','temp','threshold','event'), with=F], order.by=dta$datetime, tz="GMT")
	})

	####################
	##### dygraphs ##### interactivity - to subset data to user-selected range
	####################
	#threshold plots
	from <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"))
	})
  
	to <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"))
	})  
	output$from <- renderText({from()})
  	output$to <- renderText({to()})
	output$graphTitle <- renderText({paste("Time Series Graph:", paste(from(), to(), sep=" to ")," ")}) 

	#sd plots
	fromSD <- reactive({
		if (!is.null(input$sdthresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$sdthresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"))
	})
  
	toSD <- reactive({
		if (!is.null(input$sdthresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$sdthresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"))
	})  
	output$fromSD <- renderText({fromSD()})
  	output$toSD <- renderText({toSD()})
	output$graphTitleSD <- renderText({paste("Time Series Graph:", paste(fromSD(), toSD(), sep=" to ")," ")}) 

	#amb plots
	fromAMB <- reactive({
		if (!is.null(input$ambthresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$ambthresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"))
	})
  
	toAMB <- reactive({
		if (!is.null(input$ambthresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$ambthresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"))
	})  
	output$fromAMB <- renderText({fromAMB()})
  	output$toAMB <- renderText({toAMB()})
	output$graphTitleAMB <- renderText({paste("Time Series Graph:", paste(fromAMB(), toAMB(), sep=" to ")," ")}) 
	

	#UI OUTPUTS
	#Temperature Threshold Algorithm
	fileMin <- reactive({data_cleaned()[,min(temp)]})

	fileMax <- reactive({data_cleaned()[,max(temp)]})

	fileSamplingInterval <- reactive({as.numeric(data_cleaned()[10,'datetime',with=F]-data_cleaned()[9,'datetime',with=F])})

	output$tempThreshold <- renderUI({
        sliderInput("tempThreshold", "Temperature Threshold",
            min = fileMin()-10, max = fileMax()+10, value = fileMax()-20, step = 0.1
        )
	})

	output$sdThreshold <- renderUI({
        sliderInput("sdThreshold", "SD Threshold",
            min = 0, max = 12, value = 1, step = 0.25
        )
	})

	output$ambThreshold <- renderUI({
        sliderInput("ambThreshold", "Ambient SD Threshold",
            min = 0, max = 18, value = 9, step = 0.25
        )
	})

	output$ambThresholdplus <- renderUI({
        sliderInput("ambThresholdplus", "Minimum Cooking Temperature",
            min = fileMin(), max = fileMax()-20, value = fileMin()+10, step = 0.5
        )
	})

	####################
	####### Boxes ###### 
	####################
	# Overview Page 	
	output$dataDays <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		days <- data_cleaned()[,round(difftime(max(datetime),min(datetime), units='days'),2)]
		infoBox(
			value = paste(days, ' days',sep=""),
			title = "Sampling Duration",
			icon = icon("calendar"),
			color = "aqua"
		)
	})

	output$nSamples <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		samples <- nrow(data_cleaned())
		infoBox(
			value = paste(samples, ' samples',sep=""),
			title = "Number of Samples",
			icon = icon("calculator"),
			color = "aqua"
		)
	})

	output$sampleInterval <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		infoBox(
			value = paste(as.character(fileSamplingInterval()), ' minutes',sep=""),
			title = "Sampling Interval",
			icon = icon("clock-o"),
			color = "aqua"
		)
	})

	output$maxTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		maxTemp <- data_cleaned()[,max(temp)]
		deviceID <- data_cleaned()[,unique(device_id)]
		if(deviceID=='DS1922T'){maxThreshold <- 125}else
		if(deviceID=='DS1921G'){maxThreshold <- 85}else
		if(deviceID=='DS1922E'){maxThreshold <- 140}else
		if(deviceID=='DS1922L'){maxThreshold <- 85}else{maxThreshold <- 85}
		infoBox(
			value = formatC(maxTemp, digits = 2, format = "f"),
			title = if (maxTemp >= maxThreshold) "Warning: Max Temp Exceeded" else "Max Temp",
			icon = if (maxTemp >= maxThreshold)icon("warning") else icon("chevron-circle-up"),
			color = if (maxTemp >= maxThreshold) "red" else "green"
		)
	})

	output$minTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		minTemp <- data_cleaned()[,min(temp)]
		deviceID <- data_cleaned()[,unique(device_id)]
		minThreshold <- 0
		infoBox(
			value = formatC(minTemp, digits = 2, format = "f"),
			title = if (minTemp <= minThreshold) "Warning: Values Below 0" else "Min Temp",
			icon = if (minTemp <= minThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (minTemp <= minThreshold) "red" else "green"
		)
	})

	#Straight threshold algorithm
	output$totaluseTHRS <- renderValueBox({
		days <- data_cleaned()[datetime>=from() & datetime<=to(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.threshold()[datetime>=from() & datetime<=to(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse,0),
			subtitle = "Total Use Minutes for Selection",
			icon = icon("time", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$dataDaysTHRS <- renderValueBox({
		days <- data_cleaned()[datetime>=from() & datetime<=to(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		valueBox(
			value = days,
			subtitle = "Days of Sampling for Selection",
			icon = icon("calendar"),
			color = "aqua"
		)
	})

	output$useperdayTHRS <- renderValueBox({
		days <- data_cleaned()[,round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.threshold()[, sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse/as.numeric(days),0),
			subtitle = "Average Daily Use Minutes for File",
			icon = icon("fire", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$useperdayuseddaysTHRS <- renderValueBox({
		days <- data_cleaned()[datetime>=from() & datetime<=to(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.threshold()[datetime>=from() & datetime<=to(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		num_nonuse_days <- data.threshold()[is.na(event) & datetime>=from() & datetime<=to(),length(event),by=yday][V1==144, length(V1)]
		valueBox(
			# intentionally throw error
			value = if(days>=1){round(totaluse/(as.numeric(days) - num_nonuse_days),0)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg Use (mins) on Days with Use for Selection",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})

	output$eventsperdayuseddaysTHRS <- renderValueBox({
		days <- data_cleaned()[datetime>=from() & datetime<=to(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		num_nonuse_days <- data.threshold()[is.na(event) & datetime>=from() & datetime<=to(),length(event),by=yday][V1==144, length(V1)]
		events <- length(data.threshold()[datetime>=from() & datetime<=to() & rnum.diff>1, rnum.diff])
		grouped <- length(which(diff(events)<=3))
		events <- events - grouped

		valueBox(
			# intentionally throw error
			value = if(days>=1){round(events/(as.numeric(days) - num_nonuse_days),1)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg # of Uses on Selected Days with Use",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})

	#Daily Mean Algorithm
	output$totaluseSD <- renderValueBox({
		days <- data_cleaned()[datetime>=fromSD() & datetime<=toSD(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.sdthreshold()[datetime>=fromSD() & datetime<=toSD(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse,0),
			subtitle = "Total Use Minutes for Selection",
			icon = icon("time", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$dataDaysSD <- renderValueBox({
		days <- data_cleaned()[datetime>=fromSD() & datetime<=toSD(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		valueBox(
			value = days,
			subtitle = "Days of Sampling for Selection",
			icon = icon("calendar"),
			color = "aqua"
		)
	})

	output$useperdaySD <- renderValueBox({
		days <- data_cleaned()[datetime>=fromSD() & datetime<=toSD(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.sdthreshold()[datetime>=fromSD() & datetime<=toSD(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse/as.numeric(days),0),
			subtitle = "Average Daily Use Minutes for File",
			icon = icon("fire", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$useperdayuseddaysSD <- renderValueBox({
		days <- data_cleaned()[datetime>=fromSD() & datetime<=toSD(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.sdthreshold()[datetime>=fromSD() & datetime<=toSD(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		num_nonuse_days <- data.sdthreshold()[is.na(event) & datetime>=fromSD() & datetime<=toSD(),length(event),by=yday][V1==144, length(V1)]
		valueBox(
			# intentionally throw error
			value = if(days>=1){round(totaluse/(as.numeric(days) - num_nonuse_days),0)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg Use (mins) on Days with Use for Selection",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})

	output$eventsperdayuseddaysSD <- renderValueBox({
		days <- data_cleaned()[datetime>=fromSD() & datetime<=toSD(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		num_nonuse_days <- data.sdthreshold()[is.na(event) & datetime>=fromSD() & datetime<=toSD(),length(event),by=yday][V1==144, length(V1)]
		events <- length(data.threshold()[datetime>=fromSD() & datetime<=toSD() & rnum.diff>1, rnum.diff])
		grouped <- length(which(diff(events)<=3))
		events <- events - grouped

		valueBox(
			# intentionally throw error
			value = if(days>=1){round(events/(as.numeric(days) - num_nonuse_days),1)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg # of Uses on Selected Days with Use",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})


	#Ambient-corrected
	output$totaluseAMB <- renderValueBox({
		days <- data_cleaned()[datetime>=fromAMB() & datetime<=toAMB(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse,0),
			subtitle = "Total Use Minutes for Selection",
			icon = icon("time", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$dataDaysAMB <- renderValueBox({
		days <- data_cleaned()[datetime>=fromAMB() & datetime<=toAMB(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		valueBox(
			value = days,
			subtitle = "Days of Sampling for Selection",
			icon = icon("calendar"),
			color = "aqua"
		)
	})

	output$useperdayAMB <- renderValueBox({
		days <- data_cleaned()[datetime>=fromAMB() & datetime<=toAMB(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		valueBox(
			value = round(totaluse/as.numeric(days),0),
			subtitle = "Average Daily Use Minutes for File",
			icon = icon("fire", lib='glyphicon'),
			color = "aqua"
		)
	})

	output$useperdayuseddaysAMB <- renderValueBox({
		days <- data_cleaned()[datetime>=fromAMB() & datetime<=toAMB(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		totaluse <- data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB(), sum(event-(event - fileSamplingInterval()), na.rm=T)]
		num_nonuse_days <- data.ambthreshold()[is.na(event) & datetime>=fromAMB() & datetime<=toAMB(),length(event),by=yday][V1==144, length(V1)]
		valueBox(
			# intentionally throw error
			value = if(days>=1){round(totaluse/(as.numeric(days) - num_nonuse_days),0)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg Use (mins) on Days with Use for Selection",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})

	output$eventsperdayuseddaysAMB <- renderValueBox({
		days <- data_cleaned()[datetime>=fromAMB() & datetime<=toAMB(),round(difftime(max(datetime),min(datetime), units='days'),2)]
		num_nonuse_days <- data.ambthreshold()[is.na(event) & datetime>=fromAMB() & datetime<=toAMB(),length(event),by=yday][V1==144, length(V1)]
		events <- length(data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB() & rnum.diff>1, rnum.diff])
		grouped <- length(which(diff(events)<=3))
		events <- events - grouped

		valueBox(
			# intentionally throw error
			value = if(days>=1){round(events/(as.numeric(days) - num_nonuse_days),1)}else{round(events*as,numeric(days),1)},
			subtitle = "Avg # of Uses on Selected Days with Use",
			icon = icon("fire", lib='glyphicon'),
			color = "green"
		)
	})
	####################
	###### Tables ###### 
	####################
	output$filetable <- renderTable({
		if (is.null(input$processedfiles)) {return(NULL)}
		as.data.table(input$processedfiles)
		# as.data.table(procInput())
	})

	output$allDataTable<-renderPrint({
		orig <- options(width = 1000)
		nrows <- nrow(melt(data_cleaned(), id.var=c('datetime','device_id')))
		print(melt(data_cleaned(), id.var=c('datetime','device_id')), nrows)
		options(orig)
	})

	thresholdData <- reactive({
		data.threshold()[,Date:=as.character(as.Date(datetime))]
		events <- data.threshold()[datetime>=from() & datetime<=to() & rnum.diff>1, list(Events=length(rnum.diff)), by='Date']
		summary <- data.threshold()[datetime>=from() & datetime<=to(),list(
			`Use (minutes)`=sum(event-(event - fileSamplingInterval()), na.rm=T),
			`Sampling time (minutes)`=length(temp)*fileSamplingInterval(),
			`Algorithm`=paste("Threshold: ",unique(threshold)," Deg C",sep="")
			),
			by='Date']
		summary[`Use (minutes)`>0,`Any Use`:='Yes']	
		summary[`Use (minutes)`<=0,`Any Use`:='No']
		summary <- merge(summary, events, by='Date')	
		setcolorder(summary,c(1,2,3,5,6,4))
	})

	output$thresholdOutput <- renderDataTable(thresholdData(), options=list(searchable = FALSE, searching = FALSE, pageLength = 7))
	# ,columnDefs = list(list(width = '100px', targets = c(0:4)))

	sdData <- reactive({
		data.sdthreshold()[,Date:=as.character(as.Date(datetime))]
		events <- data.sdthreshold()[datetime>=fromSD() & datetime<=toSD() & rnum.diff>1, list(Events=length(rnum.diff)), by='Date']		
		summary <- data.sdthreshold()[datetime>=fromSD() & datetime<=toSD(),list(
			`Use (minutes)`=sum(event-(event - fileSamplingInterval()), na.rm=T),
			`Sampling time (minutes)`=length(temp)*fileSamplingInterval(),
			`Algorithm`="Deviation from Daily Mean"
			),
			by='Date']
		summary[`Use (minutes)`>0,`Any Use`:='Yes']	
		summary[`Use (minutes)`<=0,`Any Use`:='No']	
		summary <- merge(summary, events, by='Date')	
		setcolorder(summary,c(1,2,3,5,6,4))
	})

	output$sdOutput <- renderDataTable(sdData(), options=list(searchable = FALSE, searching = FALSE, pageLength = 7))
# ,columnDefs = list(list(swidth = '100px', targets = c(0:4)))

	ambData <- reactive({
		data.ambthreshold()[,Date:=as.character(as.Date(datetime))]
		events <- data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB() & rnum.diff>1, list(Events=length(rnum.diff)), by='Date']			
		summary <- data.ambthreshold()[datetime>=fromAMB() & datetime<=toAMB(),list(
			`Use (minutes)`=sum(event-(event - fileSamplingInterval()), na.rm=T),
			`Sampling time (minutes)`=length(temp)*fileSamplingInterval(),
			`Algorithm`="Ambient-corrected"
			),
			by='Date']
		summary[`Use (minutes)`>0,`Any Use`:='Yes']	
		summary[`Use (minutes)`<=0,`Any Use`:='No']	
		summary <- merge(summary, events, by='Date')	
		setcolorder(summary,c(1,2,3,5,6,4))
	})

	output$ambOutput <- renderDataTable(ambData(), options=list(searchable = FALSE, searching = FALSE, pageLength = 7))
	# ,columnDefs = list(list(width = '100px', targets = c(0:4)))

	####################
	####### PLOTS ###### 
	####################
	output$plainPlot<- 
	renderDygraph({
		# if (is.null(datasetInput())) return(NULL)
		dygraph(dataXTS.plainplot()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
		dySeries("temp", label = "SUMs Temp Deg C", strokeWidth=1)
	})

	output$thresholdPlot<- 
	renderDygraph({
		dygraph(dataXTS.threshold()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T) %>%
	    dyAxis("y", label = "Temp C", valueRange = c(0,145)) %>%
	    dyAxis("x", label = "Date & Time") %>%
		dySeries("temp", label = "SUMs Temp Deg C", strokeWidth=1) %>%
		dySeries("threshold", label = "Threshold", strokePattern = "dashed", strokeWidth=1) %>%
		dySeries("event", label = "Usage Events", strokeWidth=3, fillGraph=TRUE) %>%
		dyCallbacks(drawCallback = "function(dygraph){$('#thresholdPlotDL').attr('href', Dygraph.Export.asCanvas(dygraph).toDataURL());}")
	})


	output$sdthresholdPlot<- 
	renderDygraph({
		dygraph(dataXTS.sdthreshold()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T) %>%
	    dyAxis("y", label = "Temp C", valueRange = c(0,145)) %>%
	    dyAxis("x", label = "Date & Time") %>%
		dySeries("temp", label = "SUMs Temp Deg C", strokeWidth=1) %>%
		dySeries("threshold", label = "Threshold", strokePattern = "dashed", strokeWidth=1) %>%
		dySeries("event", label = "Usage Events", strokeWidth=3, fillGraph=TRUE)%>%
		dyCallbacks(drawCallback = "function(dygraph){$('#sdThresholdPlotDL').attr('href', Dygraph.Export.asCanvas(dygraph).toDataURL());}")
	})

	output$ambthresholdPlot<- 
	renderDygraph({
		dygraph(dataXTS.ambthreshold()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T) %>%
	    dyAxis("y", label = "Temp C", valueRange = c(0,145)) %>%
	    dyAxis("x", label = "Date & Time") %>%
		dySeries("temp", label = "SUMs Temp Deg C", strokeWidth=1) %>%
		dySeries("threshold", label = "Threshold", strokePattern = "dashed", strokeWidth=1) %>%
		dySeries("event", label = "Usage Events", strokeWidth=3, fillGraph=TRUE) %>%
		dySeries("amb", label = "Ambient Temp Deg C", strokeWidth=1, strokePattern="dotted", fillGraph=F)%>%
		dyCallbacks(drawCallback = "function(dygraph){$('#ambthresholdPlotDL').attr('href', Dygraph.Export.asCanvas(dygraph).toDataURL());}")
	})

	##########################
	####### DL HANDLERS ###### 
	##########################
	output$downloadCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.cleaned.csv', sep='') },
		content = function(file) {
			write.csv(melt(data_cleaned(), id.var=c('datetime','device_id')), file, row.names=F)
		}
	)

	output$downloadThresholdCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.threshold.output.csv', sep='') },
		content = function(file) {
			write.csv(thresholdData(), file, row.names=F)
		}
	)		

	output$downloadsdCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.dailymean.output.csv', sep='') },
		content = function(file) {
			write.csv(sdData(), file, row.names=F)
		}
	)	

	output$downloadambCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.ambient.output.csv', sep='') },
		content = function(file) {
			write.csv(ambData(), file, row.names=F)
		}
	)	

	output$downloadMerged <- downloadHandler(
		filename = function() {paste(gsub("[-: ]","_",as.character(Sys.time())), '.sumit.output.csv', sep='') },
		content = function(file) {
			write.csv(procInput(), file, row.names=F)
		}
	)	

})