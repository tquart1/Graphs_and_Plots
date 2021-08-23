library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard", titleWidth = 300,
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "DMAR",
                                 message = "We create visual dashboards"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "What is your request?",
                                 icon = icon("question")
                               ),
                               messageItem(
                                 from = "Response Inquiry",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
                  ),
  dashboardSidebar(
    sidebarMenu(
    id = "sidebar",
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
    menuItem("Graphs", tabName = 'graphs', icon = icon("chart-bar")),
    menuItem("Table", tabName = 'table', icon = icon("signal")),
    menuItem("Source Code for app", tabName = 'code', icon = icon("pencil"))
  )
  ),
  dashboardBody(
    tabItems(
    tabItem("graphs",
            fluidRow(
              box(plotOutput("lineg")),
              box(sidebarLayout(
                selectInput("carDisplay", label = h5("Favorite category"),
                            choices = c("mpg", "disp", "hp", "wt", "qsec"), selected = "mpg"),
                selectInput("carOther", label = h5("Secondary category"),
                            choices = c("cyl", "drat", "vs"), selected = "cyl")
              ),
                mainPanel(plotOutput("barg"))
                ),
              box(plotOutput("donutg"))
            )),
    tabItem("table",
            fluidPage(
              h3("The cars dataset"),
              dataTableOutput("mtcarsdisplay")
            )),
    tabItem("code")
  )
  )
)

server <- function(input, output) { 
  output$lineg <- renderPlot({
    ggplot(mtcars) + geom_line(aes(x = hp, y = mpg, color = factor(gear))) + ggtitle("Lineplot Mtcars")
  })
  output$barg <- renderPlot({
    ggplot(mtcars, aes_string(x = input$carDisplay)) +
      geom_bar(aes_string(fill= input$carOther), position = position_stack(reverse = TRUE)) +
      coord_flip() + 
      theme(legend.position = "top")
  })
  output$donutg <- renderPlot({
   ggplot(mtcars2, aes(ymax = mtcars2$ymax, ymin = mtcars2$ymin, xmax = 4, xmin=3, fill=category)) + geom_rect()+
     coord_polar(theta = "y") + xlim(c(2,4))
  })
  
  output$mtcarsdisplay <- renderDataTable(mtcars)
  }

shinyApp(ui, server)
