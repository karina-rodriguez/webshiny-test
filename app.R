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
  #URL
  #https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/designarchives%2F101and102-O/full/200,/0/color.jpg
  #https://kuybs3qucnn2f6djohgb2cscbm0bppme.lambda-url.eu-west-2.on.aws/iiif/2/designarchives%2F101and102-O/full/200,/0/color.jpg
  #full/90,/0/color.jpg
  # string1 <- data$file
  # string2 <- "https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/"
  # string2 <- "https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/"

  mystring  = paste("https://kuybs3qucnn2f6djohgb2cscbm0bppme.lambda-url.eu-west-2.on.aws/iiif/2/designarchives%2F",str_sub(data$file,0,-5),"/full/90,/0/color.jpg", sep = "")
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

          )
        ),

      )
)

)

server <- function(input, output, session) {

  output$plot <- renderPlot({
    # Take a dependency on input$update so this only happens when an update is pressed
    # print(input$sourceUpdate)
    # print(input$sourceChecked)
    # mydata[mydata$predicted1 == input$sourceChecked]
    # Apply filters
    mydata <- filter(mydata,predicted1 %in% input$sourceChecked)
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
    # output$plot_brushinfo <- renderPrint({
    #   cat("Brush (debounced):\n")
    #   str(input$plot_brush)
    # })
    output$plot_clickedpoints <-
      renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(mydata,input$plot_click, "x", "y")
      if (nrow(res) == 0)
        return()
      if (nrow(res) >= 1){
        mylist <- unlist(res[10],",")
        print(mylist)
        mynewlist <- NULL
        i <- 1
        for (urls in mylist)
        {
          newurl <- paste(HTML("<img src=",urls," width='80px'>"))
          mynewlist[[i]] <- newurl
          i <- i + 1
        }
        res[10] <- replace(res[10], 1, list(mynewlist))
      }
      res
    }, sanitize.text.function = function(x) x)


    output$plot_brushedpoints <-
      renderTable({
      res <- brushedPoints(mydata, input$plot_brush, "x", "y")
      if (nrow(res) == 0)
        return()
      if (nrow(res) >= 1){
        mylist <- unlist(res[10],",")
        print(mylist)
        mynewlist <- NULL
        i <- 1
        for (urls in mylist)
        {
          newurl <- paste(HTML("<img src=",urls," width='80px'>"))
          mynewlist[[i]] <- newurl
          i <- i + 1
        }
        res[10] <- replace(res[10], 1, list(mynewlist))

      }
      res
    }, sanitize.text.function = function(x) x)
  #closes output render plot
  })


}

shinyApp(ui, server)

