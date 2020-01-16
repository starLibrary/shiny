#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    http://tanmoy.shinyapps.io/capstone/
#    http://rpubs.com/Tanmoy/564975

stme <- Sys.time()
starttime <- Sys.time()
library(shiny)
library(data.table)
library(ggplot2)
library(ggdark)
library(gridExtra)
endtime <- Sys.time()
L <- as.numeric(endtime)-as.numeric(starttime)

n <- 30



starttime <- Sys.time()
grams2 <- fread("2-gram.final.txt", showProgress=FALSE, encoding="UTF-8")
#grams2 <- fread("2-gram.final2.txt", showProgress=FALSE, encoding="UTF-8")
endtime <- Sys.time()
L <- c(L, as.numeric(endtime)-as.numeric(starttime))
starttime <- Sys.time()
#setkey(grams2,base)
endtime <- Sys.time()
#L <- c(L, as.numeric(endtime)-as.numeric(starttime))



starttime <- Sys.time()
grams3 <- fread("3-gram.final.txt", showProgress=FALSE, encoding="UTF-8")
#grams3 <- fread("3-gram.final2.txt", showProgress=FALSE, encoding="UTF-8")
endtime <- Sys.time()
L <- c(L, as.numeric(endtime)-as.numeric(starttime))
starttime <- Sys.time()
#setkey(grams3,base)
endtime <- Sys.time()
#L <- c(L, as.numeric(endtime)-as.numeric(starttime))



ndtm <- Sys.time()
L <- c(L, as.numeric(ndtm)-as.numeric(stme))

#L <- data.frame("Tasks"=c("load Libraries","load 2-grams","setting keys","load 3-grams","setting keys","all TOTAL"), "LoadTimes"=as.character(L))
L <- data.frame("Tasks"=c("load Libraries","load 2-grams","load 3-grams","all TOTAL"), "LoadTimes"=as.character(L))









# Define UI for application that draws a histogram
ui <- fluidPage(
        
        #theme = shinytheme("darkly"),
        theme = "mystyle3.css",
        
        #shinythemes::themeSelector(),
        #tags$script(src = "myscript.js"),
        
        # Application title
        titlePanel( "Next Word Predictor (NWP) App" ),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(width = 3,
                        tags$div(class="info"),
                        
                        tags$em(
                        tags$div(class="info2",
                                 h5("A simple NWP Shiny app of mine."),
                                 h5("Takes an input phrase (multiple words) in a text box and outputs a prediction of the next word with their frequencies."),
                                 h5("Type out your phrase in Text input field , then hit space , to get predicted words.")
                        ),
                        h5( tags$span(class="info2","e.g:"),
                            HTML("&nbsp;"),
                            tags$span(class="info4","as real as it gets"),
                            tags$span(class="info5", "<SPACE>") ),
                        #h5("Each panel shows 500  words with frequency ( i.e. 1000 values, R's limit ), all 3 totalling to 1500."),
                        #h5("Each panel shows upto 500 words with their frequencies. This amounts to showing 1000 values, which is the maximum limit of R. The 3 panels alltogether show 1500 words with frequencies (i.e. 3000 values)."),
                        #h5("Each panel shows 500  words with frequency ( i.e. 1000 values, R's limit ), all 3 totalling to 1500 words with frequencies (i.e. 3000 values)."),
                        hr(class="design"),
                        tags$div(class="info2", h5("In Plots tab, the slider below changes the number of results plotted."))
                        ),
                        #chooseSliderSkin("Nice"),
                        #setSliderColor("#ED5565", 1),
                        sliderInput(inputId="bins",
                                    label="Number of n-grams plotted :",
                                    min=1,
                                    max=250,
                                    value=n,
                                    width="100%"
                        )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(width = 9,
                        tabsetPanel(type = "tabs",
                                    tabPanel("Predictor",
                                             br(),
                                             textInput("text1",
                                                       "Text input field : type your phrases below...",# also check out the Phrases tab.",
                                                       width="100%",
                                                       placeholder="type some phrases here...",
                                                       value=""
                                             ),
                                             tags$em(
                                                     tags$span(class="info4",  h6("Remember to hit ", tags$span(class="info5", "<SPACE>"), " after typing")  )
                                             ),
                                             h4(htmlOutput("res")),
                                             fixedRow(
                                                     column(width=4,
                                                            verbatimTextOutput("textoutput1")
                                                     )
                                                     #column(width=4, verbatimTextOutput("textoutput2") ),
                                                     #column(width=4, verbatimTextOutput("textoutput3") )
                                             )
                                             #tags$div(class="info3", htmlOutput("textoutput1"))
                                             #tags$div(class="info3", verbatimTextOutput("textoutput1"), verbatimTextOutput("textoutput2")),

                                    ),
                                    #tabPanel("Phrases",  "\'started telling me about his \'"  ),
                                    tabPanel("Plots",
                                             h4("List of top 2-grams & 3-grams. Pls wait a bit for plots to display."),
                                             tags$em(
                                                     tags$span(class="info4",  h6(htmlOutput("tip"))  )
                                             ),
                                             plotOutput("frequencyplots", height = "600px")
                                    ),
                                    tabPanel("Diagnostics",
                                             h4("Pls wait a bit for results to display."),
                                             actionButton(inputId="clicks", label="Re-check Memory Stats"),
                                             verbatimTextOutput("memstat"),
                                             h4("Time taken (seconds) by various processing tasks."),
                                             verbatimTextOutput("loadtimes")
                                    )#,tabPanel("Credits",h5("I'm extremely thankful to ", HTML("<a href='https://wallpapersafari.com/w/wAEJCh' target='_blank'><b>https://wallpapersafari.com/w/wAEJCh</b></a>"), " for their background image."))
                        )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        
        nhits <- reactive({ input$bins })
        plothits <- reactive({ (600/30)*nhits() })
        
        solfinal <- reactive({
                
                phr <- input$text1
                #n <- input$bins
                
                if(phr==""){
                        sol <- NULL#rep("",n)
                }else{
                        phr <- tolower(phr)
                        Ts <- unlist(gregexpr(" ",phr))
                        
                        if(length(Ts)==1){
                                if(Ts==-1){  s <- 0  }else{  s <- 1  }
                        }else{  s <- length(Ts)  }
                        
                        sol <- NULL
                        if(s==1){
                                
                                phrTmp <- substring(phr, 1, Ts-1)
                                #sol <- grams2[ phrTmp, on="base" ]
                                sol <- grams2[ phrTmp==base ]
                                if(sol[,.N]){
                                        sol[,base:=NULL]
                                        #sol[,freq:=NULL]
                                        colnames(sol) <- c("WORDS","FREQ")
                                        #updateSliderInput(session, "bins",  max=sol[,.N])
                                        if(sol[,.N]>=30){
                                                #updateSliderInput(session, "bins",  value=sol[,.N])
                                        }
                                }else{
                                        sol <- NULL#rep("",nhits())
                                        #updateSliderInput(session, "bins",  max=1)
                                        #sol <- paste0(sol,"<br/>")
                                }
                        }
                        if(s==2){
                                phrTmp <- substring(phr, 1, Ts[length(Ts)]-1)
                                #sol <- grams3[ phrTmp, on="base" ]
                                sol <- grams3[ phrTmp==base ]
                                if(sol[,.N]){
                                        sol[,base:=NULL]
                                        #sol[,freq:=NULL]
                                        colnames(sol) <- c("WORDS","FREQ")
                                        #updateSliderInput(session, "bins",  max=sol[,.N])
                                        if(sol[,.N]>=30){
                                                #updateSliderInput(session, "bins",  value=sol[,.N])
                                        }
                                }else{
                                        phrTmp <- substring(phr, Ts[length(Ts)-1]+1, Ts[length(Ts)]-1)
                                        #sol <- grams2[ phrTmp, on="base" ]
                                        sol <- grams2[ phrTmp==base ]
                                        if(sol[,.N]){
                                                sol[,base:=NULL]
                                                #sol[,freq:=NULL]
                                                colnames(sol) <- c("WORDS","FREQ")
                                                #updateSliderInput(session, "bins",  max=sol[,.N])
                                                if(sol[,.N]>=30){
                                                        #updateSliderInput(session, "bins",  value=sol[,.N])
                                                }
                                        }else{
                                                sol <- NULL#rep("",nhits())
                                                #updateSliderInput(session, "bins",  max=1)
                                                #sol <- paste0(sol,"<br/>")
                                        }
                                }
                        }
                        if(s>2){
                                phrTmp <- substring(phr, Ts[length(Ts)-2]+1, Ts[length(Ts)]-1)
                                #sol <- grams3[ phrTmp, on="base" ]
                                sol <- grams3[ phrTmp==base ]
                                if(sol[,.N]){
                                        sol[,base:=NULL]
                                        #sol[,freq:=NULL]
                                        colnames(sol) <- c("WORDS","FREQ")
                                        #updateSliderInput(session, "bins",  max=sol[,.N])
                                        if(sol[,.N]>=30){
                                                #updateSliderInput(session, "bins",  value=sol[,.N])
                                        }
                                }else{
                                        phrTmp <- substring(phr, Ts[length(Ts)-1]+1, Ts[length(Ts)]-1)
                                        #sol <- grams2[ phrTmp, on="base" ]
                                        sol <- grams2[ phrTmp==base ]
                                        if(sol[,.N]){
                                                sol[,base:=NULL]
                                                #sol[,freq:=NULL]
                                                colnames(sol) <- c("WORDS","FREQ")
                                                #updateSliderInput(session, "bins",  max=sol[,.N])
                                                if(sol[,.N]>=30){
                                                        #updateSliderInput(session, "bins",  value=sol[,.N])
                                                }
                                        }else{
                                                sol <- NULL#rep("",nhits())
                                                #updateSliderInput(session, "bins",  max=1)
                                                #sol <- paste0(sol,"<br/>")
                                        }
                                }
                        }
                }
                sol
        })
        
        
        output$res <- renderText({
                if( is.null(solfinal()) ){
                        return(  paste0("Predicted Results : ",0)  )
                }else{  paste0("Predicted Results : ",solfinal()[,.N])  }
        })
        
        #output$textoutput1 <- renderText({
        output$textoutput1 <- renderPrint({
                #paste0( solfinal()[1:nhits()], "<br/>" )
                if( is.null(solfinal()) ){
                        return("No matches found !!...")
                }else{
                        Kn <- solfinal()[,.N]
                        return( as.data.frame(solfinal())[1:Kn,] )
                }
        })
        
#        output$textoutput2 <- renderPrint({
#                if( is.null(solfinal()) ){
#                        return("No matches found !!...")
#                }else{
#                        Kn <- solfinal()[,.N]
#                        if(  Kn > 500  ){
#                                return( as.data.frame(solfinal())[501:Kn,] )
#                        }else{
#                                return("No more matches found !!...")
#                        }
#                }
#        })
        
#        output$textoutput3 <- renderPrint({
#                if( is.null(solfinal()) ){
#                        return("No matches found !!...")
#                }else{
#                        Kn <- solfinal()[,.N]
#                        if(  Kn > 1000  ){
#                                return( as.data.frame(solfinal())[1001:Kn,] )
#                        }else{
#                                return("No more matches found !!...")
#                        }
#                }
#        })
        
        
        #output$frequencyplots <- renderUI({plotOutput("freqplots", height = paste0((600/30)*nhits(), "px") )})
        
        output$frequencyplots <- renderPlot({
                
                x <- grams2[order(-freq)][1:nhits()][order(freq)]
                x[ , base:=paste0(base, " ", ends," : ",freq) ]
                x$base <- factor(x$base, levels = x$base)
                
                g1 <- ggplot(data = x) +
                        geom_bar(mapping = aes(x=base, y=freq, fill=base), stat = "identity") +
                        xlab(  paste0("2-grams : ", grams2[,.N])  ) +
                        ylab("frequency") +
                        suppressMessages(dark_theme_gray()) +
                        theme(text = element_text(size=18)) +
                        scale_y_continuous(position="right") + coord_flip() +
                        #theme_light() +
                        theme(legend.position = "none")
                
                
                x <- grams3[order(-freq)][1:nhits()][order(freq)]
                x[ , base:=paste0(base, " ", ends," : ",freq) ]
                x$base <- factor(x$base, levels = x$base)
                
                g2 <- ggplot(data = x) +
                        geom_bar(mapping = aes(x=base, y=freq, fill=base), stat = "identity") +
                        xlab(  paste0("3-grams : ", grams3[,.N])  ) +
                        ylab("frequency") +
                        suppressMessages(dark_theme_gray()) +
                        theme(text = element_text(size=18)) +
                        scale_y_continuous(position="right") + coord_flip() +
                        #theme_light() +
                        theme(legend.position = "none")
                
                grid.arrange(g1,g2,ncol=2)
        })
        
        output$tip <- renderText({
                "Move the slider to change the plot results"
        })
        
        
        
        output$memstat <- renderPrint({
                input$clicks
                gc()
        })
        
        output$loadtimes <- renderPrint({
                L
        })
        
        
        
}

# Run the application
shinyApp(ui = ui, server = server)
