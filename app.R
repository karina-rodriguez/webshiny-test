#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# run from CommandLine
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html
# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc#:~:text=Shinylive%20code%20editor.-,Deploy%20the%20App%20on%20Github%20Pages,up%20the%20Github%20Pages%20website.
# shinylive::export(appdir = "/Users/kre/workspace/research/websites/2024-paper-visualisation-ml/2024-visualisation-website", destdir = "docs")
# then git upload all files
# git add docs/*
#git commit -m "new file"
#git push origin main
library(shiny)
library(colorspace)
library(plyr)
library (dplyr)
library(stringr)

# library(ggplot2)

getMyData <- function() { # create a function with the name my_function
  # data <- read.csv("www/testdata_encoding_sample_forlarger_normalised.csv")
  # data <- read.csv("www/allmodel.csv")
  # data <- read.csv("www/images_9K_data.csv",
  #                  header = TRUE, sep = ",")[6:7]
  # print(nrow(data))
  # print(data)
  # print(mtcars)
  data <- read.csv("www/images_9K_data_wcorrectnames.csv", header = TRUE,
           # nrows=all,
           sep = ",",
           colClasses=c("x"="numeric","y"="numeric","z"="numeric"))

  data$colour = data$predicted1 #create column 'Rate2' and make it equal to 'Rate' (duplicate).
  filename <- str_sub(data$file,0,-5)
  print(filename)
  filenamewithoutplus <- str_replace(filename, "[+]", "%2B")
  mystring  = paste("https://kuybs3qucnn2f6djohgb2cscbm0bppme.lambda-url.eu-west-2.on.aws/iiif/2/designarchives%2F",filenamewithoutplus,"/full/90,/0/color.jpg", sep = "")
  #mystringwithurl = paste(HTML("<img src=",mystring," width='80px'>"))
  #data$image = paste(HTML("<img src='https://kuybs3qucnn2f6djohgb2cscbm0bppme.lambda-url.eu-west-2.on.aws/iiif/2/designarchives%2F",str_sub(data$file,0,-5),"/full/90,/0/color.jpg' width='80px'>", sep = ""))
  #mystringwithurl = paste("a(\"Google Homepage\", href=\"",mystring,"\")", sep = "")
  #print(mystring)
  data$urldata <- mystring
  #data$image = paste(HTML("<h1>YEIII</h1><img src=",mystring," width='80px'>"))
  #print(data$image)
  #paste(HTML("<img src=",example2," width='80px'>"))
  #print(res$urldata)
  #data$urldata <- mystringwithurl
  # urldata="https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/designarchives%2F"+data$file+"/full/200,/0/color.jpg"
  # data$urldata = urldata
  # print(data$urldata)

  uniquevalues <- unique(data$predicted1)
  # print(uniquevalues)
  #https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
  dc <- rainbow_hcl(length(uniquevalues),  c = 50, l = 70, start = 0, end = 360*(length(uniquevalues)-1)/length(uniquevalues))
  # print(data$colour)

  data$colour <- mapvalues(data$colour,from=uniquevalues,to=dc)
  # print(data)


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
  # overflow: hidden;
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: #FFF;

      }

      .bubble {
        width: 150px;
        height: 70px;
        line-height: 70px;
        font-weight: bold;
        position: absolute;
        left: 150px;
        top: 150px;
        text-align: center;
        background-color: #ff0000;
        background-color: #d50000;
        background-color: #b71c1c;
        color: rgba(255,255,255,0.9);
        border-radius: 10px;
        opacity: 0;
        will-change: transform;
      }
      ")
    )
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
                                      selected = unique(mydata$predicted1)[1]
                                        #unique(mydata$predicted1)
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
                            hover = hoverOpts(id = "plot_hover", delay = 0, delayType = "debounce"),
                            brush = brushOpts(id = "plot_brush")
                 ),
                 h4("Brushed points"),
                 #*****REACTIVE SOURCE******
                 tableOutput("plot_brushedpoints"),
                 h4("Clicked points"),
                 #*****REACTIVE SOURCE******
                 tableOutput("plot_clickedpoints"),
          ),
          column(width = 4,
                 imageOutput("dimage", width = 60, height = 60),
                 htmlOutput("picture"),

                 verbatimTextOutput("plot_clickinfo"),
                 verbatimTextOutput("plot_hoverinfo"),
                 verbatimTextOutput("plot_brushinfo"),

          )
        ),
        fluidRow(
          column(width = 4,
          )
        ),

      )
)

)

process_images_when_clicked_or_brushed <- function(list) {
  if (nrow(list) >= 1){

    mylist <- unlist(list[10],",")
    #print(mylist)
    mynewlist <- NULL
    i <- 1
    for (urls in mylist)
    {
      newurl <- paste(HTML("<a href=",urls,"><img crossorigin='anonymous' src=",urls," width='80px'></a>"))

      #addbutton <- paste0(newurl, actionButton("go", "Enlarge"))

      addbutton <-paste0(newurl)
      mynewlist[[i]] <- addbutton
      i <- i + 1
    }
    list[10] <- replace(list[10], 1, list(mynewlist))

  }
  list <- list[-c(9)]
  list <- list[c("urldata","predicted1", "predicted2","predicted3","x","y","z","file")]
  return(list)
}
server <- function(input, output, session) {
  reval <- reactiveValues(clickurl=c())
  selectedRow <- reactiveVal(1)

  output$plot <- renderPlot({
    # Take a dependency on input$update so this only happens when an update is pressed
    # print(input$sourceUpdate)
    # print(input$sourceChecked)
    # mydata[mydata$predicted1 == input$sourceChecked]
    # Apply filters
    mydata <- filter(mydata,(predicted1  %in% input$sourceChecked))
                     # (predicted1  %in% input$sourceChecked) |
                     #   (predicted2  %in% input$sourceChecked)  |
                     #   (predicted3  %in% input$sourceChecked) )
    # print(mydata)

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
    # output$plot_clickinfo <- renderPrint({
    #   cat("Click:\n")
    #   str(input$plot_click)
    # })
    # output$dimage <- renderImage({
    #   list(src = "https://culturedigitalskills.org/wp-content/uploads/Logo-Banner-4-2048x470.png",
    #        contentType = 'image/png',
    #        alt = "This is alternate text")
    # }, deleteFile = FALSE)
    output$picture <- renderText({
      if(!is.null(input$plot_hover)){
          hover <- input$plot_hover
          if (!is.null(hover)){
            #print(hover$x)
            #print(hover$y)
            thevalues <- nearPoints(mydata, hover,
                                    xvar = "x", yvar = "y",
                                    threshold = 2,
                                    maxpoints = 1)
              if (nrow(thevalues) == 0)
                return()
              else{
              mylist <- unlist(thevalues["urldata"],",")
              # print(mylist)
              # c('<div class="bubble"><img src="',mylist,'"/></div>')
               c('<div class="bubble"><img src="https://culturedigitalskills.org/wp-content/uploads/Logo-Banner-4-2048x470.png"/></div>')

              }
          }
      }
      })
    # output$dimage <- renderImage({
    # #  cat("Hover (throttled):\n")
    # #  str(input$plot_hover)
    #   print(mydata)
    #   list(src = "https://culturedigitalskills.org/wp-content/uploads/Logo-Banner-4-2048x470.png",
    #          contentType = 'image/png',
    #          alt = "This is alternate text")
    #   }, deleteFile = TRUE)
    #   # if(!is.null(input$plot_hover)){
    #   #   #hover=input$plot_hover
    #   #   thevalues <- nearPoints(mydata, input$plot_hover, "x", "y")
    #   #
    #   #   print(nrow(thevalues))
    #   #   #dist=sqrt((hover$x-mtcars$mpg)^2+(hover$y-mtcars$disp)^2)
    #   #   #x=hover$x
    #   #   #y=hover$y
    #   #   # thevalues <- nearPoints(mydata,input$plot_hover, threshold = 10, maxpoints = 1,addDist = TRUE)
    #   #   if (nrow(thevalues) == 0)
    #   #     return()
    #   #   #print(thevalues)
    #   #   #res <- process_images_when_clicked_or_brushed(res)
    #   #   #print(thevalues)
    #   #   #cat("Weight (lb/1000)\n")
    #   #   #str(thevalues)
    #   #   # str(hover$y)
    #   #
    #   #   # if(min(dist) < 3)
    #   #   #   brushedPoints(mydata, input$plot_brush, "x", "y")[which.min(dist)]
    #   #    }
    # # })
    output$plot_hoverinfo <- renderPrint({
      cat("Hover (debounced):\n")
      str(input$plot_hover)
    })
    output$plot_brushinfo <- renderPrint({
      cat("Brush (debounced):\n")
      str(input$plot_brush)
    })
    # output$hoverplot <- renderImage(expr, env = parent.frame(), quoted = FALSE, deleteFile = TRUE)

    output$plot_clickedpoints <-
      renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(mydata,input$plot_click, "x", "y")
      if (nrow(res) == 0)
        return()
      res <- process_images_when_clicked_or_brushed(res)

    }, sanitize.text.function = function(x) x)
    output$plot_brushedpoints <-
      renderTable({
      res <- brushedPoints(mydata, input$plot_brush, "x", "y")
      if (nrow(res) == 0)
        return()
      res <- process_images_when_clicked_or_brushed(res)

    }, sanitize.text.function = function(x) x)

    # output$dimage <- renderImage({
    #   list(src = "https://culturedigitalskills.org/wp-content/uploads/Logo-Banner-4-2048x470.png",
    #        contentType = 'image/png',
    #        alt = "This is alternate text")
    # }, deleteFile = FALSE)
    #closes output render plot
  })

  #event to show the image in larger size

  observeEvent(input$go, {
    showModal(modalDialog(
      title = "Your Image",
      "Here is the selected image!",
      HTML('<img src="https://culturedigitalskills.org/wp-content/uploads/Logo-Banner-4-2048x470.png" width="550" >'),
      size="l",
      fade=F,
      easyClose = TRUE
    )

    )
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')

    input$go
    # Use isolate() to avoid dependency on input$n
    isolate({
      print(selectedRow)
    })

  })


}

shinyApp(ui, server)

