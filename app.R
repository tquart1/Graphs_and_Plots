library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(DT)

mtcars2 <- read_csv("mtcars2.csv")
colors <- c("#EE4B2B","#FF6E33","#F9FF33","#A3FF33",
            "#65FF33","#94FF33","#50FF33", "#3388FF",
            "#335DFF", "#3385FF", "#F9FF33", "#FFA833")
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
    menuItem("Source code for app", icon = icon("pencil"), 
             href = "https://github.com/tquart1/Graphs_and_Plots/blob/main/app.R")
  )
  ),
  #took away main panels to decrease graph whitespace
  dashboardBody(
    tabItems(
    tabItem("graphs",
            fluidRow(
              box(
                selectInput(inputId = "carType", label = "Choose a CarType",
                           "carType"),
                plotOutput("lineg")
                ),
              box(
                selectInput(inputId = "carSpeed",
                            label = "Choose a Speed",
                           "carSpeed"),
                #sliderInput("disp", "Disp",min = 100, max = 400, value =100),
                plotOutput("barg")
                ),
              box(
                radioButtons("plot_type", "Plot type",
                             c("base", "ggplot2")),
                plotOutput("plot1")
              ),
              box(
                selectInput("carGo", "Speed:", choices = unique(mtcars2$carSpeed)),
                selectInput("carCountry", "Country: ", choices = unique(mtcars2$original)),
                #selectInput("carAm", "Am: ", choices = unique(mtcars2$am)),
                
                  plotOutput("car")
                
              ),
              
              fluidPage(
                h3("Mtcars2 data"),
                fluidRow(
                  column(4,
                         selectInput("mpg",
                                     "Mpg:",
                                     c("All",
                                       unique(as.character(mtcars2$mpg))))
                  ),
                  column(4,
                         selectInput("hp",
                                     "Horsepower:",
                                     c("All",
                                       unique(as.character(mtcars2$hp))))
                  ),
                  column(4,
                         selectInput("cyl",
                                     "Cylinders:",
                                     c("All",
                                       unique(as.character(mtcars2$cyl))))
                  )
                ),
                dataTableOutput("mtcars2display")
              )
              
              #box(plotOutput("donutg"))
            )),
    tabItem("table",
            fluidPage(
              h3("The cars dataset"),
              fluidRow(
                column(4,
                       selectInput("mpg",
                                   "Mpg:",
                                   c("All",
                                     unique(as.character(mtcars2$mpg))))
                ),
                column(4,
                       selectInput("hp",
                                   "Horsepower:",
                                   c("All",
                                     unique(as.character(mtcars2$hp))))
                ),
                column(4,
                       selectInput("cyl",
                                   "Cylinders:",
                                   c("All",
                                     unique(as.character(mtcars2$cyl))))
                )
              ),
              dataTableOutput("mtcarsdisplay")
            )),
    tabItem("code")
  )
  )
)

server <- function(input, output, session) { 
  #Line chart -- Interactive - 1 param
 linedat <- reactive({
   req(input$carType)
   df <- mtcars2 %>% filter(carType == input$carType) 
 })
  observe({
     updateSelectInput(session, "carType", choices = unique(mtcars2$carType))
   })

   output$lineg <- renderPlot({
      ggplot(linedat(), aes(x = X1, y = wt, color = wt, group = 1)) + geom_line()
   })
  #Bar Chart -- Interactive - 1 param
  observe({
    updateSelectInput(session, "carSpeed", choices = mtcars2$carSpeed)
  })
  data <- reactive({
   req(input$carSpeed)
    df <- mtcars2 %>% filter(carSpeed %in% input$carSpeed) %>%  group_by(X1) %>% summarize(dispMedian = median(disp))
  })
  
  output$barg <- renderPlot({
   ggplot(data(), aes(x = X1, y = dispMedian)) + geom_bar(stat = "identity", fill = "steelblue")  
    })
  
  #Donut Chart -- non-Interactive
  tidonut <- reactive({
    donut <- mtcars2 %>% group_by(X1) %>% summarize(counts = n(), percentage = n()/nrow(mtcars2))
  })
 
  output$donutg <- renderPlot({
    # donut <- mtcars2 %>%
    #   group_by(X1) %>%
    #   summarize(counts = n(), percentage = n()/nrow(mtcars2))
    
   ggplot(tidonut(), aes(x = 2, y = percentage, fill = X1)) + geom_col(color = "black")+
     coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(percentage*100),"%")),
                                             position = position_stack(vjust = 0.5)) +
     theme(panel.background = element_blank(),
           axis.line = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(), 
           plot.title = element_text(hjust = 0.5, size = 18)) +
     ggtitle("Donut Chart of Subset Mtcars(ggplot2)") +
     scale_fill_manual(values = colors) +
     xlim(0.5, 2.5)
  })
  
  #Bar Plot -- Interactive, 2 params
 thedata <- reactive({
   req(input$carSpeed)
   #req(input$original)
   
   df <- mtcars2 %>% filter(carSpeed == input$carGo) %>% filter(original == input$carCountry)
 })
  observe({
    updateSelectInput(session, "carSpeed", choices = unique(mtcars2$carSpeed))
  })
 #  observe({
 #    updateSelectInput(session, "carWt", choices = unique(mtcars2$carWt))
 # })
output$car <- renderPlot({
  
  ggplot(thedata(), aes(x = X1, y = mpg, color = "carType", group = 4)) + geom_line(size = 1)
  
   })

# mtcars2 %>% filter( carSpeed == 'M') %>% filter(original == 'America') %>%
# ggplot(mtcars2, mapping = aes(x = X1, y = mpg, color = "carType", group = 2)) + geom_line()

#Check Box bar -- Interactive 1 param
  checkdata <- reactive({
    mtcars2
  })
  observe({
    choices <- c("base", "ggplot2")
    updateCheckboxInput(session, "plot_type", value = choices)
  })
  
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(mtcars2$wt, mtcars2$mpg)
    } else if (input$plot_type == "ggplot2") {
      ggplot(checkdata(), aes(wt, mpg)) + geom_point()
    }
  })
  
  output$mtcars2display <- DT::renderDataTable(DT::datatable({
    data <- mtcars2
    if (input$mpg != "All") {
      data <- data[data$mpg == input$mpg,]
    }
    if (input$hp != "All") {
      data <- data[data$hp == input$hp,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    data
  }))
  
  
  #Table Page
  output$mtcarsdisplay <- DT::renderDataTable(DT::datatable({
    data <- mtcars
    if (input$mpg != "All") {
      data <- data[data$mpg == input$mpg,]
    }
    if (input$hp != "All") {
      data <- data[data$hp == input$hp,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    data
  }))
  
}
shinyApp(ui, server)
