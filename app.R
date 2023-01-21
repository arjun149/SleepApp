# R packages
library(shiny)
library(shinythemes)
library(suncalc)
library(ggplot2)
library(maptools)
library(lubridate)
library(lutz)
library(tidygeocoder)
library(tibble)
library(dplyr, warn.conflicts = FALSE)
library(deSolve)
library(sf)
library(tidyverse)
library(scales)
library(plotly)
#library(thematic)
#library(shinymaterial)


#times <- data.frame("Time" = c("Sunset", "Sunrise))

# Notes: UI to Server
#        UI = User Interface 

#theme = shinytheme("default")

# sunlight/sunset data = []
  
  ui <- fluidPage(
   # shinythemes::themeSelector(),
   # theme = bslib::bs_theme(bootswatch = "darkly"),
   navbarPage(
    #theme = "darkly",
   # OR
  # theme = "superhero"
  # OR
  # theme = default  
      
      
      "School Light App",
      tabPanel("Calculator",
               
               sidebarPanel(
                 tags$h3("Enter Information:"),
                 # YYYY-MM-DD format for date
                 textInput("txt1", "Date:"),
                 
                 textInput("txt2", "Location Information (Enter a valid address):"),
                 
                 textOutput("text1")
                 
                 # button to show graph or not maybe?
                 
               ), 
               
               mainPanel(
                            h1("Sunrise and Sunset Calculator"),
                            
                            h4("Sunrise and Sunset Times"),
                            verbatimTextOutput("txtout"),
                            
                      #      plotOutput("plot", click = "plot_click"),              
                 
                            actionButton("submitbutton", "Calculate"),      
                            plotOutput("plot")

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
     
      tabPanel("Attributions", "Arcascope @ https://arcascope.com",
              textOutput("text0"),
              imageOutput("photo"),
              textOutput("text1"),
              textOutput("text2")),
      
      tabPanel("Important Data: Index vs Homeostat", 
               plotOutput("plot3"), 
               textOutput("text2")),
      
      tabPanel("Important Data: Lights vs Times",
               plotOutput("plot4"),
               textOutput("text3")),
      
      tabPanel("Settings (To be updated): ",
               
               shinythemes::themeSelector(),
               
               sliderInput("rating", "App Rating (0 being the worst and 20 being the best):", 
                           min = 0, max = 20, value = 10)),
               
               # Data Visualization: One bar represents sunrise
               # and another bar represents sunset, each based on data from the
               # input of the user
               # CHANGE HISTOGRAM TO BAR GRAPH

               # Note: Watch for commas and related errors
               
         
     
               
      
      
      # 'Important Data: Timings' tab could have data
      #  ^ maybe?
  
    ) # navbarPage
   
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {
    
    #thematic::thematic_shiny()
    
    
    source(file.path("sample.r"), local = TRUE)$value
   
# Note: Replace "sleep" with "sampleR" function when ready
    })
   
  
    
    output$plot3 <- renderPlot({
      sample2(sunR, sunS)
    output$photo <- renderImage({
      
      list(
        src = file.path("aphoto", paste0("yOIT88xWkbg", ".jpg")),
        contentType = "image/jpeg", 
        width = 418,
        height = 625
      )
      }, deleteFile = FALSE)
    
    
    output$text0 <- renderText({
      "Credits to the following papers for inspiring the research used in this application:"
      
    })
    
    output$text1 <- renderText({
      "Phillips, Andrew J K, et al. “Modeling the Adenosine System as a Modulator of Cognitive Performance and Sleep 
      Patterns during Sleep Restriction and Recovery.” PLoS Computational Biology, U.S. National Library of Medicine, 
      26 Oct. 2017, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5675465/. "
    })
    
    output$text2 <- renderText({
      "Hannay, Kevin, et al. “Macroscopic Models for Human Circadian Rhythms - Sage Journals.” Sage Journals, 16 Oct. 
      2019, https://journals.sagepub.com/doi/10.1177/0748730419878298."
    })
    
    output$text1 <- renderText({
      "Note: The x-axis is the Wake Times (AM) and the y-axis is the Average Sleep (Hours)"
      
    })
    
    output$text2 <- renderText({
      "Graph of Index vs Homeostat"
      
    })
    
    output$text3 <- renderText({
      "Graph of Lights vs Times"
      
      
    })
     
    output$plot4 <- renderPlot({
      sample3(sunR, sunS)
    })
    
    
    
    output$txtout <- renderText({
       
       
       
       # *ymd* func: can transform integers or numeric data into Date objects
       # *strtoi* func: can convert strings to integers
       
       # getSunlightTimes outputs things in dataframe type (basically a list/vector)
       # CONVERT LAT AND LON INPUTS TO FLOATS/DECIMALS NOT INTEGERS
       # *as.numeric* func: can convert strings into decimals or numeric types
       
       someVar <- geo_cascade(input$txt2)
       
       tb1 <- tibble(lat = 1:1)
       
       str(someVar)
       
       
       lat1 <- as.numeric(someVar$lat[1])
       
       lon1 <- as.numeric(someVar$long[1])
       
       sun <-  getSunlightTimes(date = ymd(input$txt1),
                                           keep = c("sunrise", "sunset"), 
                                           lat = lat1,
                                           lon = lon1, 
                                           tz = tz_lookup_coords(lat1, 
                                                                 lon1, method="fast"))
       
       
       
       sunR2 <- str(sun[1,4])
       
       sunS2 <- str(sun[1,5])
       
       if (input$submitbutton == 0)
         return()
       else
         isolate(paste("Average sleep in hours: ",  sep = "   "))
         #isolate(paste(sampleR(sunR, sunS)))
       
      
       
     }) 
     
     
    output$plot <- renderPlot({
      avgsleepHours <- sampleR(sunR2, sunS2)
      wakeAM <- c("10", "5", "6", "7", "8", "9")
      ca.df <- data.frame(wakeAM, avgsleepHours)
      if (input$submitbutton == 0)
        return()
      else
        isolate(ggplot(ca.df, aes(x=wakeAM, y=avgsleepHours)) +
                  geom_bar(stat="identity", fill="lightblue") + 
                  theme(
                    panel.grid.minor = element_blank(), 
                    panel.grid.major = element_blank(),
                    panel.background = element_blank(),
                    plot.background = element_blank()) 
        )
      # sampleR(sunR, sunS)
      
    }, bg = "transparent")   
    
     
  
   
  
    
     # paste(sampleR(sunR, sunS))      
      
      
   # Maybe an image here would be nice? Possible future developement    
      
  
   # images/bagrounds for 'Attribution' page of app
   # #1: moon and sky pic || https://unsplash.com/photos/yOIT88xWkbg?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink
   # #2: galaxy and shooting stars || https://unsplash.com/photos/RmoWqDCqN2E?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink   
      
   # output$txtout <- renderImage ({})
      
    
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
