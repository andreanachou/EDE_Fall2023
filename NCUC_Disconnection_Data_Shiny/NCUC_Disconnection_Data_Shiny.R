#load libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(here)
library(yaml)
library(trend)
library(zoo)
library(Kendall)
library(tseries)
library(conflicted)
library(rsconnect)

#set theme for plots
my_theme <- theme_classic(base_size = 12) + 
  theme(panel.background = element_rect(color = "lightblue", fill = "white"),
        legend.title = element_text(color = "black", size=10),
        legend.position = "right")
theme_set(my_theme)

#load raw data set
disconnections <- read.csv(here("./NCUC_Disconnection_1996_2022.csv"), 
                           stringsAsFactors = TRUE,
                           header = TRUE)

#wrangle data set
disconnections.int <- disconnections %>% 
  mutate(Number_Disconnections_Res = na.approx(Number_Disconnections_Res),
         Date = make_date(month = Month, year = Year))


ui <- fluidPage(

    # Application title
    titlePanel("North Carolina Electric Utilities - Disconnection Dashboard"),

    # Sidebar with selection widgets 
    sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:30%",
          tags$h2("User Selection"),
          selectInput(inputId = "company",
                      label = h3("Select electric utility (main)"), 
                      choices = list("Duke Energy Carolinas" = "Duke Energy Carolinas", 
                                     "Duke Energy Progress" = "Duke Energy Progress", 
                                     "Dominion" = "Dominion",
                                     "New River Light & Power" = "New River Light & Power",
                                     "Nantahala" = "Nantahala"), 
                      selected = "N/A"),
         
          selectInput(inputId = "company2",
                      label = h3("Select electric utility (optional, for two-utility comparison)"), 
                      choices = list("Duke Energy Carolinas" = "Duke Energy Carolinas", 
                                     "Duke Energy Progress" = "Duke Energy Progress", 
                                     "Dominion" = "Dominion",
                                     "New River Light & Power" = "New River Light & Power",
                                     "Nantahala" = "Nantahala"), 
                      selected = "N/A"),
          
          selectInput(inputId = "month",
                      label = h3("Select month"), 
                      choices = list("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, 
                                     "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, 
                                     "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12), 
                      selected = "N/A"),
          
          selectInput(inputId = "year",
                      label = h3("Select year"), 
                      choices = list(2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014,
                                     2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005,
                                     2004, 2003, 2002, 2001, 2000, 1999, 1998, 1997, 1996), 
                      selected = "N/A")
        ),

        # Provide disconnection value
        mainPanel(
          tags$text("This dashboard contains data on North Carolina electric utility 
                    disconnections from the year 1996 to 2022. To find out how many 
                    disconnections occurred for a particular utility and time frame, 
                    toggle the drop-down boxes on the side panel."),
          tags$br(),
          tags$h3("Disconnection Value"),
          textOutput("disconnections.value"), #disconnection value 
          tags$br(),
          tags$h3("Annual disconnection comparisons"),
          tags$text("This graph depicts the number of disconnections for the selected 
                    utility for the entire selected year. Missing data entries were 
                    interpolated using na.approx()."),
          tags$br(),
          plotOutput("yearlyplot"), #yearly disconnections for selected utility and year
          tags$br(),
          tags$text("This graph depicts the total number of disconnnections for each year
                    in North Carolina."),
          tags$br(),
          plotOutput("NC.total.plot"), #scatter plot depicting total disconnections by year
          tags$br(),
          tags$h3("Multi-utility comparisons"),
          tags$text("This graph compares the yearly disconnections between two selected 
                    utilities for a selected year. Please select for the second utility in
                    the side panel."),
          tags$br(),
          plotOutput("yearly.comparison"), #scatter plot comparing two companies
          tags$br(),
          tags$text("This graph depicts the yearly disconnections across all utilities
                    for the selected year."),
          tags$br(),
          plotOutput("facetwrap.month"), #yearly disconnections across all utilities for selected year
          tags$br(),
          tags$h3("Monthly disconnection comparisons"),
          tags$text("This graph depicts the disconnections across all utilities for
                    the selected month and year."),
          tags$br(),
          plotOutput("barplot"), #monthly disconnections across all utilities for selected year and month
          tags$br(),
          tags$h3("Time Series Analysis"),
          tags$text("During the COVID-19 pandemic, electric utilities suspended residential 
                    energy disconnections, resulting in a period of time (4/2020-12/2021) 
                    where utility disconnections were at '0' across all electric utilities. 
                    Utilities resumed disconnections in January 2022. The time-series 
                    analysis performed below examines pre-pandemic disconnection data 
                    for the selected utility."),
          tags$br(),
          plotOutput("timeseries.graph"), #time series analysis plot
          tags$br(),
          tags$text("The Mann-Kendall test results are:"),
          verbatimTextOutput("timeseries.mk"), #mann-kendall results output
          tags$br(),
          tags$text("Time series analysis is also performed for the selected utility, by
                    averaging its yearly disconnections."),
          tags$br(),
          plotOutput("timeseries.graph.yearly"), #time series analysis plot, yearly averages
          tags$br(),
          tags$text("The Mann-Kendall test results are:"),
          verbatimTextOutput("timeseries.mk.yearly"), #mann-kendall results output for yearly averages
          tags$br(),
          tags$h3("Seasonal disconnection comparisons"),
          tags$text("This graph compares summer and winter disconnections for the selected
                    utility by comparing disconnections from the month of July and 
                    February."),
          tags$br(),
          plotOutput("seasonal.comparison") #scatter plot comparing seasonal values for selected company

        )
    )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
  #filter data set into singular line, extract disconnection value
  value <- function(df, company, month, year) {
    filtered.line <- df %>% 
      dplyr::filter(Company == company, Month == month, Year == year)
    return(filtered.line$Number_Disconnections_Res)}
  
  #print disconnection value as an output
    output$disconnections.value <- 
      renderText({paste("The number of disconnections from",
                        input$company,"during",input$month,"-",input$year,"is",
                        value(disconnections.int, input$company, input$month, input$year))})

  #reassign disconnections as a numeric data type  
  disconnections.int$Number_Disconnections_Res <- 
    as.numeric(disconnections.int$Number_Disconnections_Res)  
  
  #create reactive sub-data set for yearly plot
  yeardf <- reactive({
    dplyr::filter(disconnections.int, Year == input$year, Company == input$company)
  })
  
  #create yearly plot of disconnections for selected utility
  yearlyplot.int <- function(df, year, company) {
    yearplot <- yeardf() %>%
      ggplot(aes(x=factor(Month, levels=1:12, labels=month.abb),
                 y=Number_Disconnections_Res,
                 label=Number_Disconnections_Res)) + 
      labs(x = "Month", y = "Number of Residential Disconnections", 
           title = paste("Number of Residential Disconnections for",company,"during",
                         year),
           subtitle = "N/A's interpolated") +
      geom_point() +
      geom_text(vjust=-0.25, hjust=-0.25) +
      geom_line(group=1, alpha=0.2)
    
    return(yearplot)
  }
  
  #produce plot of yearly disconnections for selected utility
    output$yearlyplot <- 
      renderPlot(yearlyplot.int(disconnections.int, input$year, input$company))
  
  #create reactive sub-data set for bar plot
  barchartdf <- reactive({
    dplyr::filter(disconnections.int, Year == input$year, Month == input$month)
  })  
  
  #create bar chart of utility disconnections for selected date
  barplot.func <- function(df, year, month) {
    barplot <- barchartdf() %>%
      ggplot(aes(x=Company, y=Number_Disconnections_Res,
                 label=Number_Disconnections_Res)) +
      geom_col(aes(fill=Company)) +
      labs(x ="Company", y ="Number of Residential Disconnections",
           title = paste("Disconnections for", 
                       month,"/",year, "across all existing utilities"),
           subtitle ="N/A's interpolated") +
    geom_text(vjust=-0.25, hjust=-0.25)
  
  return(barplot) }
  
  #produce bar plot of utility disconnections for selected date
    output$barplot <- 
      renderPlot(barplot.func(disconnections.int, input$month, input$year))  
    
  #create reactive sub-data set for facet wrap plots
  facetwrap.df <- reactive({
    dplyr::filter(disconnections.int, Year == input$year)})
    
  #create facet wrap plot of all utilities' disconnections for the selected year
  facetwrap.func <- function(df, year) {
    facetwrap.plot <- facetwrap.df() %>%
      ggplot(aes(x=factor(Month, levels=1:12, labels=month.abb),
                 y=Number_Disconnections_Res,
                 label=Number_Disconnections_Res)) +
      labs(x = "Month", y = "Number of Residential Disconnections", 
           title = paste("Number of Residential Disconnections during",year),
           subtitle = "N/A's interpolated") +
      geom_point() +
      geom_text(vjust=-0.1, hjust=-0.25) +
      facet_wrap(vars(Company)) +
      geom_line(group=4, alpha=0.2)
    
    return(facetwrap.plot)}
  
  #produce facet wrap plots for selected year
  output$facetwrap.month <- 
    renderPlot(facetwrap.func(disconnections.int, input$year))

  #create static data frame of pre-pandemic values
  disconnections.int.prepan <- disconnections.int %>%
    dplyr::filter(Date < as.Date("2020-04-01"))
  
  #create time series plot for selected utility
  timeseries.int <- function(df, company) {
    disconnections.company.ts <- df %>%
      dplyr::filter(Company == company)
    
    disconnections.plot.ts <- disconnections.company.ts %>%
      ggplot(aes(x=Date, y=Number_Disconnections_Res)) +
      geom_line() +
      labs(x="Date", y="Number of Residential Disconnections",
           title=paste("Historical Disconnections for",input$company))
    
    return(disconnections.plot.ts)}
  
  #produce time series plot for selected utility
  output$timeseries.graph <-
    renderPlot(timeseries.int(disconnections.int.prepan, input$company))
  
  #Mann-Kendall analysis on basic time-series data
  disconnections.mk.int <- function(df, company) {
    disconnections.company.ts <- df %>%
      dplyr::filter(Company == company)
    
    disconnections.ts <- ts(disconnections.company.ts$Number_Disconnections_Res,
                            start=c(1996, 01, 01), frequency=12)
    
    mktrends <- MannKendall(disconnections.ts)
    
    return(summary(mktrends))}
  
  #print Mann-Kendall results
  output$timeseries.intro <- 
    renderText({paste("The Mann-Kendall test results are:")})
  
  output$timeseries.mk <- 
    renderPrint(disconnections.mk.int(disconnections.int.prepan, input$company))
  
  #create static data frame of pre-pandemic yearly average disconnection values
  disconnections.yearly <- disconnections.int.prepan %>%
    group_by(Company, Year) %>%
    dplyr::summarise(Yearly_Average_Disconnections_Res = mean(Number_Disconnections_Res)) %>%
    mutate(across(Yearly_Average_Disconnections_Res, round, digits=2))
  
  disconnections.yearly <- as.data.frame(disconnections.yearly)
  
  #create yearly average time series plot for selected utility
  timeseries.yearly <- function(df, company) {
    disconnections.company.ts <- df %>%
      dplyr::filter(Company == company)
    
    disconnections.plot.ts <- disconnections.company.ts %>%
      ggplot(aes(x=Year, y=Yearly_Average_Disconnections_Res,
                 label=Yearly_Average_Disconnections_Res)) +
      geom_line(alpha=0.2) +
      geom_text(vjust=-0.1, hjust=-0.25) +
      labs(x="Year", y="Average Number of Residential Disconnections",
           title=paste(input$company,"Yearly Average Disconnections"))
    
    return(disconnections.plot.ts)}
  
  #produce yearly average time series plot for selected utility
  output$timeseries.graph.yearly <- 
    renderPlot(timeseries.yearly(disconnections.yearly, input$company))
  
  #Mann-Kendall analysis on yearly average time series data
  disconnections.mk.yearly <- function(df, company) {
    disconnections.company.ts <- df %>%
      dplyr::filter(Company == company)
    
    disconnections.ts <- ts(disconnections.company.ts$Yearly_Average_Disconnections_Res,
                            start=c(1996, 01, 01))
    
    mk.values <- MannKendall(disconnections.ts)
    
    return(summary(mk.values))}
  
  #produce yearly averages time series values
  output$timeseries.mk.yearly <- 
    renderPrint(disconnections.mk.yearly(disconnections.yearly, input$company))
  
  #create reactive sub-data set for two utility comparison
  yearplot.df <- reactive({
    disconnections.int %>% 
      dplyr::filter(Year == input$year, 
                    Company %in% c(input$company, input$company2)) %>%
      mutate(Company=as.factor(Company))})
  
  #create scatter plot comparing two utilities for selected year
  yearlyplot.compare <- function(df, year, company, company2) {
    yearplot <- yearplot.df() %>%
      ggplot(aes(x=factor(Month, levels=1:12, labels=month.abb),
                 y=Number_Disconnections_Res,
                 label=Number_Disconnections_Res,
                 group=Company)) + 
      labs(x = "Month", y = "Number of Residential Disconnections", 
           title = paste("Number of Residential Disconnections for",company,"and",
                         company2,"during",year),
           subtitle = "N/A's interpolated") +
      geom_point(aes(color=Company)) +
      geom_text(aes(color=Company), vjust=-0.1, hjust=-0.25) +
      geom_line(aes(group=Company, color=Company), alpha=0.5)
    
    return(yearplot)}
  
  #produce scatter plot comparing two utilities for selected year as output
  output$yearly.comparison <-
    renderPlot(yearlyplot.compare(disconnections.int, input$year, 
                                  input$company, input$company2))
  
  #create reactive sub-data set for seasonal comparison
  seasoncombo.df <- reactive ({
    disconnections.int %>% dplyr::filter(Company == input$company, 
                                         Month %in% c(2, 7)) %>% 
      mutate(Month = as.factor(Month))})
  
  #create scatter plot comparing seasonal disconnections for selected utility
  seasoncombo.int <- function(df, company) {
    seasoncombo.int.plot <- seasoncombo.df() %>%
      ggplot(aes(x=Date, y=Number_Disconnections_Res,
                 label=Number_Disconnections_Res)) +
      geom_point(aes(color=Month)) +
      geom_line(aes(group=Month, color=Month)) +
      geom_text(aes(color=Month), vjust=-0.25, hjust=-0.25) +
      labs(x = "Year", y = "Number of Residential Disconnections", 
           title = paste("Seasonal Residential Disconnections for",company),
           subtitle = "N/A's interpolated")
    return(seasoncombo.int.plot)}
  
  #produce seasonal scatter plot
  output$seasonal.comparison <-
    renderPlot(seasoncombo.int(disconnections.int, input$company))
  
  #group disconnections by year and sum - data frame
  disconnections.nc.yearly <- disconnections.int %>%
    group_by(Year) %>%
    mutate(Total_Disconnections = sum(Number_Disconnections_Res),
           Company = NULL, Number_Disconnections_Res = NULL, Outliers = NULL,
           Date = NULL, Month = NULL) %>%
    distinct()
  
  disconnections.nc.yearly.plot <- disconnections.nc.yearly %>%
    ggplot(aes(x=Year, y=Total_Disconnections,
               label=Total_Disconnections)) +
    geom_point() +
    geom_line(alpha=0.5) +
    geom_text(vjust=-0.25, hjust=-0.25) +
    labs(x = "Year", y = "Total Residential Disconnections", 
         title = "Total North Carolina Residential Electric Disconnections",
         subtitle = "N/A's interpolated")
  
  output$NC.total.plot <- renderPlot(disconnections.nc.yearly.plot)
  
  
    } #server bracket



# Run the application 
shinyApp(ui = ui, server = server)
