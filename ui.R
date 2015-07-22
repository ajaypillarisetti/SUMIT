dashboardPage(
  dashboardHeader(title = "SUMIT"),
  dashboardSidebar(
    sidebarMenu(
	    menuItem("Overview", tabName = "overview",  icon = icon("list-alt")),
	    menuItem("SUMs Analysis", tabName = "graphs",  icon = icon("area-chart"),
            menuSubItem("Threshold Algorithm", tabName = "threshold"),
            menuSubItem("Daily Mean Algorithm", tabName = "dailymean"),
            menuSubItem("Ambient-corrected", tabName = "dailymeanamb")
        ),
	    # menuItem("Dashboard", tabName = "dashboard"),
    	menuItem("Raw data", tabName = "rawdata",  icon = icon("tasks")),
        menuItem("Merge Files", tabName = "merge",  icon = icon("files-o"))

    )
  ),
	dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(type='text/javascript', src='dygraph-extra.js'),
            tags$script(type='text/javascript', src='scripties.js')
            ),

    	tabItems(
			tabItem("overview",
                fluidRow(
                    box(width=12,  collapsible = TRUE,status="info", solidHeader=TRUE, title="Welcome to SUMIT: the SUMs-iButton Tool",
                        h5('SUMIT is a simple tool to visualize, analyize, and output basic usage data from iButton Stove Use Monitors. SUMIT was designed by Ajay Pillarisetti at the University of California, Berkeley, with support from Winrock International and Berkeley Air Monitoring Group.'),
                        p("To get started, upload an iButton file using the button below."),
                        fileInput('files', 'Select an iButton csv file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple=F)
                    )
                ),
                fluidRow(
                    box(
                        width = 8, height='510px',
                        status = "info", solidHeader = TRUE,
                        title = "Time-Series Plot",
                        dygraphOutput('plainPlot', height='445px')
                    ),
                    infoBoxOutput('dataDays'),
                    infoBoxOutput('nSamples'),
                    infoBoxOutput('sampleInterval'),
                    infoBoxOutput('maxTemp'),
                    infoBoxOutput('minTemp')
                )

			),      
    		tabItem("threshold",
                fluidRow(
                    valueBoxOutput('totaluseTHRS'),                    
                    valueBoxOutput('dataDaysTHRS'),
                    valueBoxOutput('useperdayTHRS'),
                    valueBoxOutput('useperdayuseddaysTHRS'),
                    valueBoxOutput('eventsperdayuseddaysTHRS')
                ),
		        fluidRow(                    
                    box(
                        width = 9, height='450px',
                        status = "info", solidHeader = TRUE,
                        title = textOutput("graphTitle"),
                        dygraphOutput('thresholdPlot', height='390px'),
                        tags$a(id = "thresholdPlotDL", class = "shiny-download-link", icon("download"), "Save", download = "thresholdPlot.png")
                    ),
                    box(
                        width = 3, 
                        status="info", solidHeader = TRUE,
                        title = 'Algorithm Info',
                        p("This algorithm looks at the time above a specific, user-set temperature. The slider at right can be used to adjust the threshold. Click and drag over the graph to zoom in; to zoom out, double click on the graph.")
                    ),
                    box(
                        width = 3, 
                        status="info", solidHeader = TRUE,
                        title = 'Analysis Parameters',
                        uiOutput("tempThreshold")
                    )
                ),
                fluidRow(

                    box(title = "Daily Summary",
                        width = 12,
                        status="info", solidHeader = TRUE,
                        downloadButton("downloadThresholdCSV", "Download CSV"),
                        dataTableOutput('thresholdOutput')
                    )
	      	    )
            ),
            tabItem("dailymean",
                fluidRow(
                    valueBoxOutput('totaluseSD'),                    
                    valueBoxOutput('dataDaysSD'),
                    valueBoxOutput('useperdaySD'),                    
                    valueBoxOutput('useperdayuseddaysSD'),
                    valueBoxOutput('eventsperdayuseddaysSD')
                ),
                fluidRow(
                    box(
                        width = 9,
                        status = "info", solidHeader = TRUE,
                        title = textOutput("graphTitleSD"),
                        dygraphOutput('sdthresholdPlot', height='390px'),
                        tags$a(id = "sdThresholdPlotDL", class = "shiny-download-link", icon("download"), "Save", download = "sdthresholdPlot.png")
                    ),
                    box(
                        width = 3,
                        status="info", solidHeader = TRUE,
                        title = 'Algorithm Info',
                        p("This algorithm calculates the daily average temperature. Events are defined as temperatures above the daily average temperature plus X * daily standard deviation of the temperature. The slider at right can be used to adjust the standard deviation threshold (X). Click and drag over the graph to zoom in; to zoom out, double click on the graph.")
                    ),
                    box(
                        width = 3,
                        status="info", solidHeader = TRUE,
                        title = 'Analysis Parameters',
                        uiOutput("sdThreshold")
                    )                    
                ),
                fluidRow(
                    box(title = "Daily Summary",
                        width = 12,
                        status="info", solidHeader = TRUE,
                        downloadButton("downloadsdCSV", "Download CSV"),
                        dataTableOutput('sdOutput') 
                    )
                )
            ),
            tabItem("dailymeanamb",
                fluidRow(
                    box(
                        width=4,
                        status='info',
                        solidHeader=TRUE,
                        title="Ambient Upload",
                        fileInput('amb', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple=F),
                        height='103px'
                    ),
                    valueBoxOutput('totaluseAMB'),
                    valueBoxOutput('dataDaysAMB'),
                    valueBoxOutput('useperdayAMB'),
                    valueBoxOutput('useperdayuseddaysAMB'),
                    valueBoxOutput('eventsperdayuseddaysAMB')
                ),
                fluidRow(
                    box(
                        width = 9,
                        status = "info", solidHeader = TRUE,
                        title = textOutput("graphTitleAMB"),
                        dygraphOutput('ambthresholdPlot', height='390px'),
                        tags$a(id = "ambthresholdPlotDL", class = "shiny-download-link", icon("download"), "Save", download = "ambthresholdPlot.png")
                    ),
                    box(
                        width = 3, 
                        status="info", solidHeader = TRUE,
                        title = 'Algorithm Info',
                        HTML("<p>This algorithm looks for deviations from mean hourly ambient temperature to determine cooking. The sliders at right can be used to adjust parameters. Click and drag over the graph to zoom in; to zoom out, double click on the graph.</p>")
                    ),
                    box(
                        width = 3,
                        status="info", solidHeader = TRUE,
                        title = 'Analysis Parameters',
                        uiOutput('ambThreshold'),
                        uiOutput('ambThresholdplus')
                    )                    
                ),
                fluidRow(
                    box(title = "Daily Summary",
                        width = 12,
                        status="info", solidHeader = TRUE,
                        downloadButton("downloadambCSV", "Download CSV"),
                        dataTableOutput('ambOutput') 
                    )
                )
            ),
	    	tabItem("rawdata",
    	  		downloadButton("downloadCSV", "Download as CSV"),
    	  		HTML("<BR><BR>"),
				verbatimTextOutput("allDataTable")
	      	),
            tabItem("merge",
                fluidRow(
                    box(width=12,  collapsible = TRUE,status="info", solidHeader=TRUE, title="Merge SUMIT Output",
                        h5('This simple tool allows you to merge many files output by SUMIT into one large comma-separated text file, enabling editing in commercial spreadsheet software or using the statistical package of your choice. It combines all files into one long document and adds an additional column with the original filename.'),
                        p("To get started, upload SUMIT files using the button below."),
                        fileInput('processedfiles', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple=T)
                    )
                ),
                fluidRow(
                    box(width=12, status='info', solidHeader=TRUE, title="Uploaded Files to Combine",
                        downloadButton("downloadMerged", "Download CSV"),
                        tableOutput('filetable')
                    )
                )
            )               
    	)	
	)
)