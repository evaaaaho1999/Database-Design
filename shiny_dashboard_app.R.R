# Load required libraries
#install.packages("DT")
# library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(rsconnect)
library(tidyr)
library(htmltools)
library(dashboardthemes)
library(shinyWidgets)
library(htmlwidgets)
library(ggplot2)
library(leafgl)
library(bslib)
library(ggrepel)
library(tidyverse)
# install.packages('shinyWidgets')
# rsconnect::setAccountInfo(name='evaho1999',
#                           token='01588B5484004E729736AE149D572F9E',
#                           secret='HQsk7v35SB7OrJN6wqVLqed/8hpZmpHvrff9kXMv')
# # Read database credentials
# getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("credentials_v4.R")
# C:/Users/user/Desktop/courses (SMU)/Mod b semester 1/database design/Database design project IHuaHo/04_shiny_HW1_solution/
# For running the app type in 
# runApp('./03_shiny_HW1/shiny_dashboard_example.R')

# It gets published to my account at https://kbabu.shinyapps.io/01_zomato_v2/


ui <- dashboardPage(
  
  dashboardHeader(title = span("Restaurant ", style="color: #FFE4E1; font-size: 20px; font-family:Rockwell, serif",
                               span("dashboard", 
                                    style = "color: #BC8F8F; font-size: 18px; font-family:Rockwell, serif"))),
  # skin = "purple",
  
  #Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # tags$head(
        # tags$style(HTML(
        # 
        #   ".main-sidebar { color: red; font-size: 20px; }")),
      # ),
      tags$style(type = 'text/css',".badge{min-width: 20px;}"),
      menuItem("HW Summary", tabName = "HWSummary",
               badgeLabel   = "Go!", badgeColor = "green"),
      menuItem("Q1-DB Query", tabName = "dbquery", 
               badgeLabel   = "Check!", badgeColor = "red"),
      menuItem("Q2-Maps", tabName = "leaflet", 
               badgeLabel   = "New!", badgeColor = "yellow")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "HWSummary",
              tags$h3("This HW was submitted by I Hua Ho of ITOM6265", style="color:#FFFFFF; font-size:150%; font-family:Goudy Old Style, serif;"),
              tags$p("In this APP, you can see the overview of the restaurants, also can search the restaurant by keywords and find it on map.", style="color:#FFFFFF; font-size:120%; font-family:Goudy Old Style, serif;"),
              tags$p("- In the first tab, I use info box to show the overview of the data.", style="color:#FFFFFF; font-size:120%; font-family:Goudy Old Style, serif;"),
              tags$p("- In the second tab, users can enter the keywords and use the slider bar to filter the restaurant they want. Then I output the data table and use the renderplot to show the pie-chart of the city distribution.", style="color:#FFFFFF; font-size:120%; font-family:Goudy Old Style, serif;"),
              tags$p("- In the third tab, users can use the slider bar to filter the votes of the restaurant, then the map will show the information and the urls of that restaurant.", style="color:#FFFFFF; font-size:120%; font-family:Goudy Old Style, serif;"),
              tags$p("- Some other changes are the UI interfaces and the words color and fonts.", style="color:#FFFFFF; font-size:120%; font-family:Goudy Old Style, serif;"),
              fluidRow(
                # A static infoBox
                infoBox("Total restaurants", 73, icon = icon("list"),color = "red"),
                infoBox("Total Cities", 15, icon = icon("city"),color = "yellow"),
                infoBox("Average Cost", 15, icon = icon("dollar-sign"),color = "teal"),
                HTML('<center><img src="https://dmn-dallas-news-prod.cdn.arcpublishing.com/resizer/zTdFQmj-zfRvtpmbG1XPIjDhFn4=/1660x934/smart/filters:no_upscale()/cloudfront-us-east-1.images.arcpublishing.com/dmn/ZFXIDNRNSVDR7HJ3HXUOJNQPM4.jpg" width="976"></center>')
                # img(src="https://dmn-dallas-news-prod.cdn.arcpublishing.com/resizer/zTdFQmj-zfRvtpmbG1XPIjDhFn4=/1660x934/smart/filters:no_upscale()/cloudfront-us-east-1.images.arcpublishing.com/dmn/ZFXIDNRNSVDR7HJ3HXUOJNQPM4.jpg",align="center", height = 500, width = 976)
              )
      ),
      
      # tabItem(tabName = "dbquery",
      #         fluidRow(
      #           titlePanel("Searching for restaurant!"),
      #           
      #           column(width = 6,
      #           box(width = NULL, solidHeader = TRUE, background = "black",
      #             textInput("rest_names", h3("Pattern of Name:")),
      #             # Copy the line below to make a slider range 
      #             sliderInput("rest_votes_slider", label = h3("Range of votes to search for:"), min = 0, 
      #                         max = 1500, value = c(0, 100)),
      #             actionButton("Go", "Get results"),
      #             h2("The is your search result"),
      #             DT::dataTableOutput("mytable")
      #           )
      #         )),
      # ),
      #Second tab content
      tabItem(tabName = "dbquery",
              fluidPage(
              titlePanel(h2("Basic Information of Restaurants", style="color:#FFFFFF; font-size:180%; font-family:Goudy Old Style, serif;")),
              sidebarLayout(
                sidebarPanel(
                  textInput("rest_names", tags$h3("Pattern of Name:", style="color:#696969; font-size:150%; font-family:Goudy Old Style, serif;")),
                  chooseSliderSkin("Shiny", color="#B0C4DE"),
                  sliderInput("rest_votes_slider", label = tags$h3("Range of votes to search for:", style="color:#696969; font-size:150%; font-family:Goudy Old Style, serif;"), 
                              min = 0, max = 1500, value = c(0, 100)),
                  actionButton("Go", "Get results", icon = icon("paper-plane"), style="color: #fff; background-color: #B0C4DE; border-color: #708090")),
                mainPanel(
                  tabsetPanel(
                    tabPanel(tags$h5("Basic information of restaurants", style="color:#696969; font-size:120%; font-family:Goudy Old Style, serif;"), DT::dataTableOutput("mytable")),
                    tabPanel(tags$h5("Distribution of City", style="color:#696969; font-size:120%; font-family:Goudy Old Style, serif;"), plotOutput("output1_1"))
                  )
                )
              ))),
      
      
      
      # Third tab content
      tabItem(tabName = "leaflet",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("rest_votes", label = tags$h3("Range of votes:", style="color:#696969; font-size:180%; font-family:Goudy Old Style, serif;"), 
                                min = 0, max = 1500, value = c(0, 100))),
                  mainPanel(
                    column(width = 9,
                               tags$h2("Map of restaurants in London.", style="color:#FFFFFF; font-size:180%; font-family:Goudy Old Style, serif;"),
                               leafletOutput("mymap", width = "125%", height = 400)
                           
                    )
                  )
                )
                
              )
              
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$Go, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #browser()
    query <- paste("select name,Votes,city from zomato_rest where name like '%",input$rest_names,"%' AND  votes  between ", input$rest_votes_slider[1]," AND ",input$rest_votes_slider[2],  ";", sep="")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
    output$mytable = DT::renderDataTable({
      data
    })
    query2 <- paste("select city, Votes, Average_Cost from zomato_rest where name like '%",input$rest_names,"%' AND  votes  between ", input$rest_votes_slider[1]," AND ",input$rest_votes_slider[2], ";", sep="")
    # query2 <- paste("select Average_Cost from zomato_rest where Average_Cost is NOT NULL",  ";", sep="")
    print(query2)
    data2 <- dbGetQuery(db, query2)
    output$output1_1 = renderPlot({
      df<-as.data.frame(data2)
      # ggplot(df, aes(x=Average_Cost , color=city))+
      #   geom_histogram(fill = "white")+
      #   labs(title = "Average_Cost vs City")
      ggplot(df, aes(x="", y=city, fill= fct_inorder(city))) +
        geom_col(width = 1, color = 1)+
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        guides(fill = guide_legend(title = "city")) +
        theme_void()
      # hist(df$Average_Cost, main="The average cost Plot", col="purple", ltw=2, lwd=2)
    })
    
  })
  
  
  output$mymap <- renderLeaflet({
    
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #browser()
    query <- paste("select name, city, address, latitude,Longitude, Average_Cost, Votes from zomato_rest where votes between ", input$rest_votes[1]," AND ",input$rest_votes[2],  "AND latitude is NOT NULL;", sep="")
    # query <- paste("select * from zomato_rest where latitude is NOT NULL;", sep="")
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    data<-as.data.frame(data)
    new=NULL
    
    # data2<-reactive({data%>%
    #       filter(data$Votes>=input$rest_votes[1])%>%
    #       filter(data$Votes<=input$rest_votes[2])%>%
    #       filter(data$Average_Cost>=input$Avg_cost[1])%>%
    #       filter(data$Average_Cost<=input$Avg_cost[2])}
    #       return(data))
      
    
    web<-"https://www.google.com/search?q="
    add=NULL
    for(i in data$name){
      add<-append(add,paste(web,i, sep = ""))
    }
    print(add)
    data<-cbind(data, add)
    
    # content <- paste(sep = "<br/>",
    #                  "<b><a href = add >data$name</a></b>",
    #                  data$city,
    #                  data$address
    # )
    # leaflet(data = data) %>%
    #     addTiles() %>%
    #     addMarkers(lng = ~Longitude,
    #                lat = ~latitude,
    #                popup = paste("Name:", data$name))  
      # addProviderTiles(providers$Stamen.Toner)
    #m
    leaflet(data = data) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude,
                lat = ~latitude,
                popup = paste(sep = "<br/>",
                      data$name,
                      data$city,
                      data$address,
                      paste("$",data$Average_Cost),
                      data$add)) 
      
    
  })

}

shinyApp(ui, server)


