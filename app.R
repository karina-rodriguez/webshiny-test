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
# scp -P 25088 -r docs/. qn58tc6y@lhcp4025.webapps.net:/home/qn58tc6y/designarchives.culturedigitalskills.org/.
library(shiny)
# library(ggplot2)


ui <- fluidPage(
  title = "Test Page: Plotting Embeddings",

  titlePanel(h1("ML Embeddings for Design Archives
                Dataset")),

  sidebarLayout(position = "left",
      sidebarPanel(h3("Sidebar")
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

        # fluidRow(
        #   column(width = 4,
        #          plotOutput("plot", height=300,
        #                     click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
        #                     hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
        #                     brush = brushOpts(id = "plot_brush")
        #          ),
        #          h4("Brushed points"),
        #          tableOutput("plot_brushedpoints"),
        #          h4("Clicked points"),
        #          tableOutput("plot_clickedpoints"),
        #   ),
        #   column(width = 4,
        #          verbatimTextOutput("plot_hoverinfo"),
        #          verbatimTextOutput("plot_clickinfo")
        #   ),
        #   column(width = 4,
        #          wellPanel(actionButton("newplot", "New plot")),
        #          verbatimTextOutput("plot_brushinfo")
        #   )
        # ),

        bbl = "aa",
        h1("Vermeer Painting", align = "center"),
        p("Johannes Vermeer was a Dutch Baroque Period painter who specialized in domestic interior scenes of middle-class life. He is considered one of the greatest painters of the Dutch Golden Age. During his lifetime, he was a moderately successful provincial genre painter, recognized in Delft and The Hague."),
        strong("strong() makes bold text."),
        p(),
        br(),
        img(src = "https://6fzwqjk2sg.execute-api.eu-west-2.amazonaws.com/latest/iiif/2/Johannes_Vermeer_Het_melkmeisje/full/300,/00/color.jpg", width=500),
        br(),

        em("em() creates italicized (i.e, emphasized) text."),
        br(),
        code("code displays your text similar to computer code"),
        div("div creates segments of text with a similar style.
            This division of text is all blue because I passed the argument
            'style = color:blue' to div", style = "color:blue"),

        h2("Second level title"),
        h3("Third level title"),
        h4("Fourth level title"),
        h5("Fifth level title"),
        h6("Sixth level title"),


      )

  )
)

getData <- function() { # create a function with the name my_function
  # data <- read.csv("www/testdata_encoding_sample_forlarger_normalised.csv")
  # data <- read.csv("www/allmodel.csv")
  data <- read.csv("www/images_9K_data.csv")
  print(nrow(data))
  print(mtcars)
}

server <- function(input, output) {
#
#   print("hello function")
#   getData()
#   #here is a funciton to get the data
#   data <- reactive({
#     input$newplot
#     # Add a little noise to the cars data so the points move
#     cars + rnorm(nrow(cars))
#   })
#   print(rnorm(nrow(cars)))
#
#
#   output$plot <- renderPlot({
#     d <- data()
#     plot(d$speed, d$dist)
#   })
#   output$plot_clickinfo <- renderPrint({
#     cat("Click:\n")
#     str(input$plot_click)
#   })
#   output$plot_hoverinfo <- renderPrint({
#     cat("Hover (throttled):\n")
#     str(input$plot_hover)
#   })
#   output$plot_brushinfo <- renderPrint({
#     cat("Brush (debounced):\n")
#     str(input$plot_brush)
#   })
#   output$plot_clickedpoints <- renderTable({
#     # For base graphics, we need to specify columns, though for ggplot2,
#     # it's usually not necessary.
#     res <- nearPoints(data(), input$plot_click, "speed", "dist")
#     if (nrow(res) == 0)
#       return()
#     res
#   })
#   output$plot_brushedpoints <- renderTable({
#     res <- brushedPoints(data(), input$plot_brush, "speed", "dist")
#     if (nrow(res) == 0)
#       return()
#     res
#   })

}

shinyApp(ui, server)
