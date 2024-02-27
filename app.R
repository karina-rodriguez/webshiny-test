#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html
# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc#:~:text=Shinylive%20code%20editor.-,Deploy%20the%20App%20on%20Github%20Pages,up%20the%20Github%20Pages%20website.
# shinylive::export(appdir = "/Users/kre/workspace/research/websites/2024-paper-visualisation-ml/2024-visualisation-website", destdir = "docs")
library(shiny)
library(colorspace)
library(plyr)
library (dplyr)

# library(ggplot2)

getMyData <- function() { # create a function with the name my_function
  # data <- read.csv("www/testdata_encoding_sample_forlarger_normalised.csv")
  # data <- read.csv("www/allmodel.csv")
  # data <- read.csv("www/images_9K_data.csv",
  #                  header = TRUE, sep = ",")[6:7]
  # print(nrow(data))
  # print(data)
  # print(mtcars)
  data <- read.csv("www/images_9K_data.csv", header = TRUE,
           # nrows=all,
           sep = ",",
           colClasses=c("x"="numeric","y"="numeric","z"="numeric"))
  data$colour = data$predicted1 #create column 'Rate2' and make it equal to 'Rate' (duplicate).

  print(data)

  uniquevalues <- unique(data$predicted1)
  print(uniquevalues)
  #https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
  dc <- rainbow_hcl(length(uniquevalues),  c = 50, l = 70, start = 0, end = 360*(length(uniquevalues)-1)/length(uniquevalues))
  # print(data$colour)
  data$colour <- mapvalues(data$colour,from=uniquevalues,to=dc)
  print(data)


  # print(unique(data$predicted1));
  # categories <- c();
  # c=0
  # for(var in unique(data$predicted1))
  # {
  #
  #   categories.append(var,c)
  #   c=c+1
  # }
  # values = unique(data$predicted1)
  return(data);
}
mydata <- getMyData();
ui <- fluidPage(
  title = "Test Page: Plotting Embeddings",
  tags$style("#mycheck {font-size:8px;}
             }"
  ),

  titlePanel(h1("ML Embeddings for Design Archives
                Dataset")),
  sidebarLayout(position = "left",

      sidebarPanel(
                   h3("Fliter by"),
                   width = 2,
                   uiOutput("selnodes"),
                   #*****REACTIVE SOURCE******
                   # actionButton("sourceUpdate", "Update"),

                   checkboxGroupInput('sourceChecked',h5("Categories"),
                                      choices = unique(mydata$predicted1),
                                      selected = unique(mydata$predicted1)
                                      )
                  # checkboxInput("somevalue", "Some value", FALSE),

      ),
      mainPanel(
        # adding the new div tag to the sidebar
        # tags$div(class="FirstDIV", checked=NA,
        #          list(
        #            tags$p("Ready to take the Shiny tutorial? If so"),
        #            tags$a(href="shiny.posit.co/tutorial", "Click Here!"),
        #            "Thank you"
        #          )
        # ),
        # h2("Second level title"),

        fluidRow(style = 'width: 100%; margin: auto;',
          column(width = 8,
                 style = 'width: 80%; margin: auto;',
                 #*****REACTIVE SOURCE******
                 plotOutput("plot", height=500,
                            click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                            hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                            brush = brushOpts(id = "plot_brush")
                 ),
                 h4("Brushed points"),
                 #*****REACTIVE SOURCE******
                 tableOutput("plot_brushedpoints"),
                 h4("Clicked points"),
                 #*****REACTIVE SOURCE******
                 tableOutput("plot_clickedpoints"),
          ),
        ),
        fluidRow(
          column(width = 4,

          ),
          # column(width = 4,
          #        wellPanel(actionButton("newplot", "New plot")),
          #        verbatimTextOutput("plot_brushinfo")
          # )
        ),
        # bbl = "aa",
        # h1("Vermeer Painting", align = "center"),
        # p("Johannes Vermeer was a Dutch Baroque Period painter who specialized in domestic interior scenes of middle-class life. He is considered one of the greatest painters of the Dutch Golden Age. During his lifetime, he was a moderately successful provincial genre painter, recognized in Delft and The Hague."),
        # strong("strong() makes bold text."),
        # p(),
        # br(),
        # img(src = "https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/Johannes_Vermeer_Het_melkmeisje/full/300,/00/color.jpg", width=500),
        # br(),
        #
        # em("em() creates italicized (i.e, emphasized) text."),
        # br(),
        # code("code displays your text similar to computer code"),
        # div("div creates segments of text with a similar style.
        #     This division of text is all blue because I passed the argument
        #     'style = color:blue' to div", style = "color:blue"),
        #
        # h3("Third level title"),
        # h4("Fourth level title"),
        # h5("Fifth level title"),
        # h6("Sixth level title"),
        #

      )
)

)

server <- function(input, output, session) {
  # output$selnodes <- renderUI({
  #   checkboxGroupInput('mycheck',h5("Categories"),
  #                      choices = unique(mydata$predicted1),
  #                      selected = unique(mydata$predicted1)
  #   )
  # })


  output$plot <- renderPlot({
    # Take a dependency on input$update so this only happens when an update is pressed
    print(input$sourceUpdate)
    print(input$sourceChecked)
    # mydata[mydata$predicted1 == input$sourceChecked]
    # Apply filters
    mydata <- filter(mydata,predicted1 %in% input$sourceChecked)
    print(mydata)
    plot(mydata$x, mydata$y,
         #https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/points.html
         pch = 19,
         bg = mydata$colour,
         col = mydata$colour)

    # output$plot_clickinfo <- renderPrint({
    #     cat("Click:\n")
    #     str(input$plot_click)
    #   })
    # output$plot_hoverinfo <- renderPrint({
    #   cat("Hover (throttled):\n")
    #   str(input$plot_hover)
    # })
    # output$plot_brushinfo <- renderPrint({
    #   cat("Brush (debounced):\n")
    #   str(input$plot_brush)
    # })
    output$plot_clickedpoints <- renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(mydata, input$plot_click, "x", "y")
      if (nrow(res) == 0)
        return()
      res
    })
    output$plot_brushedpoints <- renderTable({
      res <- brushedPoints(mydata, input$plot_brush, "x", "y")
      if (nrow(res) == 0)
        return()
      res
    })
  })
  # print("hello function")
  # getData()
  #here is a funciton to get the data
  # observe({
  #   req(input$update)
  #   print("here"+input$mycheck)
  #
  #   rng <- reactive({
  #     print("I am here")
  #     input$mycheck
  #     print(input$selnodes)
  #
  #     })
  #   values <-rng()
  #
  # })
  # values <- reactiveValues(mydata=c())


  # data <- reactive({
  #   input$newplot
  #   # Add a little noise to the cars data so the points move
  #   cars + rnorm(nrow(cars))
  # })

  # data <- reactive({
  #   # input$newplot
  #   thedata <- read.csv("www/images_9K_data.csv", header = TRUE,
  #                    nrows=5000, sep = ",",
  #                    colClasses=c("x"="numeric","y"="numeric","z"="numeric"))
  #   thedata$colour = thedata$predicted1 #create column 'colour' and make it equal to 'pedicted1' (duplicate).
  #
  #   uniquevalues <- unique(thedata$predicted1)
  #   #https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
  #   dc <- rainbow_hcl(length(uniquevalues),  c = 50, l = 70, start = 0, end = 360*(length(uniquevalues)-1)/length(uniquevalues))
  #   thedata$colour <- mapvalues(thedata$colour,from=uniquevalues,to=dc)
  #   thedata
  # })

# COL= mydata$colour
  # output$plot <- renderPlot({
  #   # d <- mydata
  #   d <- data()
  #   plot(d$x, d$y,
  #     #https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/points.html
  #        pch = 19,
  #        bg = d$colour,
  #        col = d$colour)
  # })

  # output$plot <- renderPlot({
  #   d <- data()
  #   plot(d$x, d$y)
  # })
  # output$plot <- renderPlot({
  #   # tt <- rng()
  #   plot(mydata$x, mydata$y,
  #     #https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/points.html
  #        pch = 19,
  #        bg = mydata$colour,
  #        col = mydata$colour)
  # })
  # output$plot_clickinfo <- renderPrint({
  #   cat("Click:\n")
  #   str(input$plot_click)
  # })
  # output$plot_hoverinfo <- renderPrint({
  #   cat("Hover (throttled):\n")
  #   str(input$plot_hover)
  # })
  # output$plot_brushinfo <- renderPrint({
  #   cat("Brush (debounced):\n")
  #   str(input$plot_brush)
  # })
  # output$plot_clickedpoints <- renderTable({
  #   # For base graphics, we need to specify columns, though for ggplot2,
  #   # it's usually not necessary.
  #   res <- nearPoints(mydata, input$plot_click, "x", "y")
  #   if (nrow(res) == 0)
  #     return()
  #   res
  # })
  # output$plot_brushedpoints <- renderTable({
  #   res <- brushedPoints(mydata, input$plot_brush, "x", "y")
  #   if (nrow(res) == 0)
  #     return()
  #   res
  # })

}

shinyApp(ui, server)

