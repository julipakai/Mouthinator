#load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(phonR)
library(shiny)
#ui
ui <- shinyUI(fluidPage(
#add navbar and diphtongs
  titlePanel("Mouthinator 1.0.0"),
  fluidRow(
    column(2,
           wellPanel(
             h3("File Upload"),
             fileInput("file1", "Choose .csv file",
                       accept=c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
             helpText(em("Please do not upload any file that could contain confidential data")
             )),
           wellPanel(
             h3("Select your columns"),
             fluidRow(
               column(width=6,
                      selectInput("f1", "F1", "", selected="")
               ),
               column(width=6,
                      selectInput("f2", "F2", "", selected="")
               )
             ),
             selectInput("vowel", "Vowel", "", selected=""),
             checkboxInput(inputId="spkrs", "Are there multiple speakers in your data?", FALSE),
             #conditional panel selectbox does not show up!!!
             conditionalPanel(
               condition="input.spkrs==true",
               selectInput("speaker", strong("Speakers"), "", selected = "")),
           ),
           wellPanel(
             h3("Plot settings"),
             tags$br(),
             h5(strong("Ellipse settings")),
             checkboxInput(inputId="ellipseline", "Ellipse line", TRUE),
             checkboxInput("ellipsefill", " Ellipse fill", FALSE),
             tags$hr(),
             h5(strong("Means settings")),
             checkboxInput(inputId="plotmeans", "Plot means", FALSE),
             tags$hr(),
             h5(strong("Token settings")),
             checkboxInput(inputId="plottokens", "Plot tokens", TRUE),
             tags$hr(),
             h5(strong("Legend Placement")),
             selectInput("legendplc", "", c("bottomleft", "bottomright", "topleft", "topright"), selected="bottomleft")
           )
           ),
    column(10,
           wellPanel(
             tabsetPanel(type="tabs",
                         tabPanel("Table", tableOutput("contents")),
                         tabPanel("Summary", tableOutput("stats")),
                         tabPanel("Formant Plot", plotOutput("plot",
                                                             #change to height == width
                                                             height="600px",
                                                             width="600px")))
  )

  )
    )
)
)



#server
server <- shinyServer(function(input, output, session) {
  #preload data
  data <- reactive({ 
    req(input$file1)
    inFile <- input$file1 
    df <- read.csv(inFile$datapath, header=TRUE, sep=",",
                   quote='"')
    updateSelectInput(session, inputId="f1", label="F1",
                      choices=names(df), selected=names(df)[1]
                      [sapply(df, is.numeric)])
    updateSelectInput(session, inputId="f2", label="F2",
                      choices=names(df), selected=names(df)[2]
                      [sapply(df, is.numeric)])
    updateSelectInput(session, inputId="vowel", label="Vowel",
                      choices=names(df), selected=names(df)[3]
                      [sapply(df, is.character)])
    updateSelectInput(session, inputId="speaker", label="Speakers",
                      choices=names(df), selected=names(df)[4])
    

    return(df)
  })
  output$contents <- renderTable({
    data()
  })
  output$plot <- renderPlot({
    req(data)
    plot <- with(data(),
                 plotVowels(f1=get(input$f1), f2=get(input$f2), vowel=get(input$vowel),
                            var.sty.by=get(input$vowel), var.col.by=get(input$vowel),
                            ellipse.line=input$ellipseline, ellipse.fill=input$ellipsefill,
                            plot.tokens=input$plottokens, cex.tokens=NULL,
                            plot.means=input$plotmeans, cex.means=4,
                            xlim=c(3100, 500),
                            ylim=c(1000, 250),
                            legend.kwd=input$legendplc,
                            pretty=TRUE))
    #multiple speakers
    if(input$spkrs==TRUE){
      plot <- with(data(),
                   plotVowels(f1=get(input$f1), f2=get(input$f2), vowel=get(input$vowel), group=get(input$speaker),
                              var.sty.by=get(input$vowel), var.col.by=get(input$speaker),
                              ellipse.line=input$ellipseline, ellipse.fill=input$ellipsefill,
                              plot.tokens=input$plottokens, cex.tokens=NULL,
                              plot.means=input$plotmeans, cex.means=4,
                              xlim=c(3100, 500),
                              ylim=c(1000, 250),
                              legend.kwd=input$legendplc,
                              pretty=TRUE))
    }
  })
 
  output$stats <- renderTable({
    req(data)
    stats <- data() %>%
      group_by(get(input$vowel)) %>%
      summarise(meanf1=mean(get(input$f1)),
                sdf1=sd(get(input$f1)),
                meanf2=mean(get(input$f2)),
                sdf2=(sd(get(input$f2))))
    colnames(stats) <- c("", "Mean F1", "SD for F1", "Mean F2", "SD for F2")
    if(input$spkrs==TRUE){
      stats <- data() %>%
        group_by(get(input$vowel), get(input$speaker)) %>%
        summarise(meanf1=mean(get(input$f1)),
                  sdf1=sd(get(input$f1)),
                  meanf2=mean(get(input$f2)),
                  sdf2=(sd(get(input$f2))))
      colnames(stats) <- c("", "","Mean F1", "SD for F1", "Mean F2", "SD for F2")
    }
    return(stats)
  })
})

shinyApp(ui, server)