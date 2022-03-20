#load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(phonR)
library(shiny)
#ui
ui <- shinyUI(fluidPage(
#add navbar and diphtongs
  titlePanel("Mouthinator 1.1.3"),
  fluidRow(
    column(2,
           wellPanel(
             h3("File Upload"),
             tags$br(),
             fileInput("file1", "Choose .csv file",
                       accept=c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
             helpText(em("Please do not upload any file that could contain confidential data")
             )),
           wellPanel(
             h3("Harrison-support"),
             tags$br(),
             checkboxInput(inputId="harrison", "Was your file created using Harrison (2021)?", FALSE),
             helpText(em("Please remember to convert your Harrison .Table file into a .csv file before uploading, for example using Excel"))
           ),
           wellPanel(
             h3("Select your columns"),
             tags$br(),
             fluidRow(
               column(width=6,
                      selectInput("f1", "F1", "", selected="")
               ),
               column(width=6,
                      selectInput("f2", "F2", "", selected="")
               )
             ),
             tags$hr(),
             selectInput("vowel", "Vowel", "", selected=""),
             helpText(em("If your file was created using Harrison (2021), you can also select the lexical set (lexset), or IPA (ipa)")),
             checkboxInput(inputId="spkrs", "Are there multiple speakers in your data?", FALSE),
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
             helpText(em("Displays ellipse line of ±1 standard deviation from mean")),
             tags$hr(),
             h5(strong("Means settings")),
             checkboxInput(inputId="plotmeans", "Plot means", FALSE),
             tags$hr(),
             h5(strong("Token settings")),
             checkboxInput(inputId="plottokens", "Plot tokens", TRUE),
             tags$hr(),
             h5(strong("Legend Placement")),
             selectInput("legendplc", "", c("bottomleft", "bottomright", "topleft", "topright"), selected="bottomleft"),
             checkboxInput(inputId="plotipa", "Plot symbols as IPA", FALSE),
             helpText(em("Only possible if you have a column containing IPA symbols in your file or if youused Harrison (2021) to create your file")),
             conditionalPanel(
               condition="input.plotipa==true",
               selectInput(inputId="ipasel", "IPA", "", selected="")
               
             )
           )
           ),
    column(10,
           wellPanel(
             tabsetPanel(type="tabs",
                         tabPanel("Table", tableOutput("contents")),
                         tabPanel("Summary", tableOutput("stats")),
                         tabPanel("Formant Plot", plotOutput("plot",
                                                             #change to height == width
                                                             height="700px",
                                                             width="700px")))
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
    if(input$harrison==TRUE) {
      remapping <- c("i:" = "FLEECE", "I" = "KIT", "e" = "DRESS", "a" = "TRAP", "a:" = "START/BATH",
                     "o" = "LOT", "or" = "NORTH/FORCE", "u" = "FOOT", "v" = "STRUT", "u:" = "GOOSE",
                     "c" = "SCHWA", "c:" = "NURSE",
                     "aI" = "PRICE", "eI" = "FACE", "cu" = "GOAT", "au" = "MOUTH",
                     "HES" = "HESITATION",
                     "e:" = "SQUARE", "es" = "horsEs")
      remappingipa <- c("i:" = "iː", "I" = "ɪ", "e" = "e", "a" = "æ", "a:" = "ɑː",
                        "o" = "ɒ", "or" = "ɔː", "u" = "ʊ", "v" = "ʌ", "u:" = "uː",
                        "c" = "ə", "c:" = "ɜː",
                        "aI" = "aɪ", "eI" = "eɪ", "cu" = "əʊ", "au" = "aʊ",
                        "HES" = "h",
                        "e:" = "eə", "es" = "horsEs")
      #fix horses
      df$lexset <-  remapping[as.character(df$Vowel)]
      df$ipa <-  remappingipa[as.character(df$Vowel)]
    }
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
    updateSelectInput(session, inputId="ipasel", label="IPA",
                      choices=names(df), selected=names(df)[5])
    return(df)
  })
  output$contents <- renderTable({
    data()
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
      #add number of each token
    }
    return(stats)
  })
  
  output$plot <- renderPlot({
    req(data)
    plot <- with(data(),
                 plotVowels(f1=get(input$f1), f2=get(input$f2), vowel=get(input$vowel),
                            var.sty.by=get(input$vowel), var.col.by=get(input$vowel),
                            ellipse.line=input$ellipseline, ellipse.fill=input$ellipsefill,
                            plot.tokens=input$plottokens, cex.tokens=NULL,
                            plot.means=input$plotmeans, cex.means=4,
                            xlim=c(3100, 250),
                            ylim=c(1000, 250),
                            ylab=c("F1 (Hz)"), xlab=c("F2 (Hz)"),
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
                              xlim=c(3100, 250),
                              ylim=c(1000, 250),
                              ylab=c("F1 (Hz)"), xlab=c("F2 (Hz)"),
                              legend.kwd=input$legendplc,
                              pretty=TRUE))
    }
    #plot tokens as ipa
    if(input$plotipa==TRUE){
      plot <- with(data(),
                   plotVowels(f1=get(input$f1), f2=get(input$f2), vowel=get(input$vowel),
                              var.sty.by=get(input$vowel), var.col.by=get(input$vowel),
                              ellipse.line=input$ellipseline, ellipse.fill=input$ellipsefill,
                              plot.tokens=input$plottokens, cex.tokens=NULL, pch.tokens=get(input$ipasel),
                              plot.means=input$plotmeans, cex.means=4, pch.means=get(input$ipasel),
                              xlim=c(3100, 250),
                              ylim=c(1000, 250),
                              ylab=c("F1 (Hz)"), xlab=c("F2 (Hz)"),
                              legend.kwd=input$legendplc,
                              pretty=TRUE))
    }
    #multiple speakers AND plot tokens as ipa
    if(input$spkrs==TRUE & input$plotipa==TRUE){
        plot <- with(data(),
                     plotVowels(f1=get(input$f1), f2=get(input$f2), vowel=get(input$vowel), group=get(input$speaker),
                                var.sty.by=get(input$vowel), var.col.by=get(input$speaker),
                                ellipse.line=input$ellipseline, ellipse.fill=input$ellipsefill,
                                plot.tokens=input$plottokens, cex.tokens=NULL, pch.tokens=get(input$ipasel),
                                plot.means=input$plotmeans, cex.means=4, pch.means=get(input$ipasel),
                                xlim=c(3100, 250),
                                ylim=c(1000, 250),
                                ylab=c("F1 (Hz)"), xlab=c("F2 (Hz)"),
                                legend.kwd=input$legendplc,
                                pretty=TRUE))
    }
  })
})

shinyApp(ui, server)
