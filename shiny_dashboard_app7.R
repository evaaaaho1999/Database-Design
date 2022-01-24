# Load required libraries
# library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(treemap)

# Read database credentials
source("./credentials_v4.R")

card <- function(.name, .date, .id) {
  HTML(
    paste0(
      '<div class="card" id="card-',
      .id,
      '">
      <div class="container">
      <h2><b>',
      .name,
      '</b></h3>
      <hr>
      <p>Date Created: ',
      .date,
      '</p>
      </div>
      </div>'
    )
  )
}

company_card <- function(.ticker, .name, .sector) {
  HTML(
    paste0(
      '<div class="card" id="card-',
      .ticker,
      '">
      <div class="container">
      <h2><b>',
      .ticker,
      '</b></h3>
      <hr>
      <p class="company-name"><b>',
      .name,
      '</b></p>
      <p>',
      .sector,
      '</p>
      </div>
      </div>'
    )
  )
}

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Portfolio Manager"),
  #Sidebar content
  dashboardSidebar(
    tags$head(tags$link(rel="shortcut icon", href="https://img.favpng.com/5/17/20/computer-icons-economic-growth-favicon-economy-green-growth-png-favpng-GH4ui06SrREb9kqgeh7mYvbWk.jpg")),
    
    #Add sidebar menus here
    sidebarMenu(
      menuItem(
        "Manage Portfolios",
        tabName = "manage_portfolio",
        icon = icon("plus")
      ),
      menuItem(
        "Trade Records",
        tabName = "trade_records",
        icon = icon("dollar-sign")
      ),
      menuItem(
        "Manage Trade Records",
        tabName = "manage_trade_record",
        icon = icon("plus")
      ),
      menuItem(
        "Analytics",
        tabName = "analytics",
        icon = icon("chart-bar")
      ),
      menuItem("Explore", tabName = "explore", icon = icon("search"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "manage_portfolio",
        fluidRow(column(
          12,
          h2("Manage Portfolios"),
          column(
            6,
            textInput(
              "portfolio_add",
              label = h3("Name of Portfolio"),
              value = "",
              width = "100%"
            ),
            
            actionButton(
              "createPortfolio",
              label = "Create Portfolio",
              class = "btn btn-success",
              style = "color: white;"
            )
          ),
          
          column(
            6,
            textInput(
              "portfolio_delete",
              label = h3("Name of Portfolio"),
              value = "",
              width = "100%"
            ),
            
            actionButton(
              "deletePortfolio",
              label = "Delete Portfolio",
              class = "btn btn-danger",
              style = "color: white;"
            )
          )
        )),
        tabPanel("diamonds",
                 useShinyjs(),
                 fluidRow(column(
                   12,
                   hr(),
                   column(12,
                          DT::dataTableOutput("portfolio_list"))
                 )))
      ),
      tabItem(tabName = "manage_trade_record",
              fluidRow(
                column(12,
                       h2("Manage Trade Records")),
                fluidRow(column(12,
                                column(
                                  12,
                                  uiOutput("portfolio_names3")
                                )),
                         column(
                           12,
                           column(12,
                                  tabsetPanel(
                                    tabPanel(
                                      h5("Add"),
                                      column(
                                        12,
                                        dateInput("add_date", label =
                                                    h3("Trade Date"), width = "100%"),
                                        uiOutput("add_tickers"),
                                        textInput(
                                          "add_price",
                                          label = h3("Price per Share"),
                                          width = "100%"
                                        ),
                                        textInput("add_qty", label =
                                                    h3("Quantity"), width = "100%"),
                                        selectInput(
                                          "add_direction",
                                          label = h3("Direction"),
                                          choices = c("Buy", "Sell"),
                                          width = "100%"
                                        ),
                                        actionButton(
                                          "addTrade",
                                          label = "Add Trade",
                                          class = "btn btn-success",
                                          style = "color: white;"
                                        ),
                                      ),
                                      
                                    ),
                                    tabPanel(
                                      h5("Edit"),
                                      column(
                                        12,
                                        uiOutput("edit_record"),
                                        dateInput("edit_date", label =
                                                    h3("Trade Date"), width = "100%"),
                                        uiOutput("edit_tickers"),
                                        textInput(
                                          "edit_price",
                                          label = h3("Price per Share"),
                                          width = "100%"
                                        ),
                                        textInput("edit_qty", label =
                                                    h3("Quantity"), width = "100%"),
                                        selectInput(
                                          "edit_direction",
                                          label = h3("Direction"),
                                          choices = c("Buy", "Sell"),
                                          width = "100%"
                                        ),
                                        actionButton(
                                          "editTrade",
                                          label = "Edit Trade",
                                          class = "btn btn-warning",
                                          style = "color: white;"
                                        ),
                                      ),
                                    ),
                                  
                                    tabPanel(h5("Delete"),
                                             column(
                                               12,
                                               uiOutput("delete_record"),
                                               actionButton(
                                                 "deleteTrade",
                                                 label = "Delete Trade",
                                                 class = "btn btn-danger",
                                                 style = "color: white;"
                                               ),
                                             ),)
                                  ), style = "margin-top: 20px;")
                         )),
              )),
      tabItem(tabName = "trade_records",
              fluidRow(column(
                12,
                h2("Trade Records"),
                fluidRow(column(
                  12,
                  column(6,
                         uiOutput("portfolio_names")),
                  column(
                    6,
                    uiOutput("date_range")
                  ),
                  column(
                    12,
                    actionButton(
                      "Go",
                      "Get Results",
                      icon = icon("paper-plane"),
                      class = "btn btn-success",
                      style = "color: white;"
                    )
                  )
                )),
                fluidRow(column(12,
                                DT::dataTableOutput("trade_list")
                                ), style = "margin-top: 20px;")
              ))),
      tabItem(tabName = "analytics",
              fluidRow(column(
                12,
                h2("Portfolio Analysis"),
                fluidRow(column(12,
                                column(
                                  12,
                                  uiOutput("portfolio_names2"),
                                  actionButton(
                                    "Go1",
                                    "Get Results",
                                    icon = icon("paper-plane"),
                                    class = "btn btn-success",
                                    style = "color: white; margin-top: 21px"
                                  )
                                )), ),
                fluidRow(column(
                  12,
                  tabsetPanel(
                    tabPanel(
                      tags$h5("Portfolio Overview"),
                      DT::dataTableOutput("mytable1")
                    ),
                    tabPanel(tags$h5("Portfolio Allocation"), plotOutput("output1_1")),
                    tabPanel(tags$h5("Performances"), DT::dataTableOutput("mytable2")),
                    tabPanel(tags$h5("Long & Short"), plotOutput("output1_2"))
                  )
                ), style = "margin-top: 20px;")
              ))),
      
      #  Add contents for second tab
      tabItem(tabName = "explore",
              fluidRow(column(
                12,
                useShinyjs(),
                h2("Explore Companies"),
                tags$head(
                  tags$style(
                    '.card {
                                            background-color: white;
                                            width: 300px;
                                            border-radius: 5px;
                                            clear: both;
                                            /* Add shadows to create the "card" effect */
                                            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                                            transition: 0.3s;
                                          }
                                            /* On mouse-over, add a deeper shadow */
                                          .card:hover {
                                            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                                          }
                                           /* Add some padding inside the card container */
                                          .container {
                                            padding: 2px 16px;
                                          }
                                          .company-name {
                                            color: green;
                                          }
                                          .company_card {
                                                  background-color: white;
                                                  border-radius: 5px;
                                                  clear: both;
                                                  /* Add shadows to create the "card" effect */
                                                  box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                                                  transition: 0.3s;
                                                }
                                                  /* On mouse-over, add a deeper shadow */
                                                .company_card:hover {
                                                  box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                                                }
                                                 /* Add some padding inside the card container */
                                                .container {
                                                  //width: 250px;
                                                  padding: 2px 16px;
                                                  cursor: pointer;
                                                }'
                  )
                ),
                uiOutput("companies")
              )))
    )
  )
)

display_info <- function(id) {
  print(id)
}

display_company_info <- function(id) {
  db <- dbConnector(
    server   = getOption("database_server"),
    database = getOption("database_name"),
    uid      = getOption("database_userid"),
    pwd      = getOption("database_password"),
    port     = getOption("database_port")
  )
  on.exit(dbDisconnect(db), add = TRUE)
  
  data <-
    dbGetQuery(db, paste0("SELECT * FROM Company WHERE ticker = '", id, "';"))
  data2 <-
    dbGetQuery(db, paste0("SELECT * FROM Stocks WHERE ticker = '", id, "';"))
  showModal(modalDialog(
    title = h2(data['company_name']),
    h3(id),
    p(paste0("Sector: " , data['sector'])),
    p(paste0("Industry: ", data['industry'])),
    p(paste0(
      "Market Cap ($M): $",
      format(as.numeric(data2['market_cap']), nsmall = 1, big.mark = ",")
    )),
    p(paste0(
      "Stock Price: $", format(as.numeric(data2['stock_price']), nsmall = 1, big.mark = ",")
    )),
    easyClose = TRUE,
    footer = NULL
  ))
}

server <- function(input, output) {
  db <- dbConnector(
    server   = getOption("database_server"),
    database = getOption("database_name"),
    uid      = getOption("database_userid"),
    pwd      = getOption("database_password"),
    port     = getOption("database_port")
  )
  on.exit(dbDisconnect(db), add = TRUE)
  
  # PORTFOLIOS -----------------------------------------------------------------
  
  portfolios_data_list <-
    dbGetQuery(
      db,
      "SELECT portfolio_id as ID, UPPER(LEFT(portfolio_name,1))+LOWER(SUBSTRING(portfolio_name,2,LEN(portfolio_name))) AS 'Portfolio Name', convert(varchar, creation_date, 107) AS 'Date Created' FROM Portfolio;"
    )
  
  output$portfolio_list <- DT::renderDataTable({
    portfolios_data_list
  })
  
  portfolios_id <-
    dbGetQuery(db, "SELECT portfolio_id AS 'ID' FROM Portfolio")
  
  output$portfolio_names <- renderUI({
    selectInput("Portfolio_ID",
                tags$h3("Portfolio"),
                width = "100%",
                choice = portfolios_id['ID'])
  })
  
  output$portfolio_names2 <- renderUI({
    selectInput("Portfolio_ID2",
                tags$h3("Portfolio"),
                width = "100%",
                choice = portfolios_id['ID'])
  })
  
  output$portfolio_names3 <- renderUI({
    selectInput("Portfolio_ID3",
                tags$h3("Portfolio"),
                width = "100%",
                choice = portfolios_id['ID'])
  })
  
  
  observeEvent(input$createPortfolio, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    names <-
      dbGetQuery(db,
                 "SELECT UPPER(portfolio_name) AS portfolio_name FROM Portfolio;")
    
    if (is.element(toupper(input$portfolio_add), names[, "portfolio_name"])) {
      showModal(modalDialog(
        title = h2("Error"),
        p(
          paste0(
            "You already have a portfolio named '",
            input$portfolio_add,
            "', try creating a portfolio with another name."
          )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      query <-
        paste0(
          "INSERT INTO Portfolio(portfolio_name, creation_date) VALUES ('",
          input$portfolio_add,
          "', GETDATE());"
        )
      
      rs <- dbSendQuery(db, query)
      
      dbClearResult(rs)
      
      portfolios_data_list <-
        dbGetQuery(
          db,
          "SELECT portfolio_id AS 'ID', UPPER(LEFT(portfolio_name,1))+LOWER(SUBSTRING(portfolio_name,2,LEN(portfolio_name))) AS 'Portfolio Name', convert(varchar, creation_date, 107) AS 'Date Created' FROM Portfolio;"
        )
      
      output$portfolio_list <- DT::renderDataTable({
        portfolios_data_list
      })

      portfolios_id <-
        dbGetQuery(db, "SELECT portfolio_id AS 'ID' FROM Portfolio")
      
      output$portfolio_names <- renderUI({
        selectInput(
          "Portfolio_ID",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
      
      output$portfolio_names2 <- renderUI({
        selectInput(
          "Portfolio_ID2",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
      
      output$portfolio_names3 <- renderUI({
        selectInput(
          "Portfolio_ID3",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
    }
  })
  
  observeEvent(input$deletePortfolio, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    names <-
      dbGetQuery(db,
                 "SELECT UPPER(portfolio_name) AS portfolio_name FROM Portfolio;")
    
    if (is.element(toupper(input$portfolio_delete), names[, "portfolio_name"])) {
      query <-
        paste0("DELETE FROM Portfolio WHERE portfolio_name = '",
               input$portfolio_delete,
               "';")
      rs <- dbSendQuery(db, query)
      
      dbClearResult(rs)
      
      portfolios_data_list <-
        dbGetQuery(
          db,
          "SELECT portfolio_id AS 'ID', UPPER(LEFT(portfolio_name,1))+LOWER(SUBSTRING(portfolio_name,2,LEN(portfolio_name))) AS 'Portfolio Name', convert(varchar, creation_date, 107) AS 'Date Created' FROM Portfolio;"
        )
            
      output$portfolio_list <- DT::renderDataTable({
        portfolios_data_list
      })
      
      portfolios_id <-
        dbGetQuery(db, "SELECT portfolio_id AS 'ID' FROM Portfolio")
      
      output$portfolio_names <- renderUI({
        selectInput(
          "Portfolio_ID",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
      
      output$portfolio_names2 <- renderUI({
        selectInput(
          "Portfolio_ID2",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
      
      output$portfolio_names3 <- renderUI({
        selectInput(
          "Portfolio_ID3",
          tags$h3("Portfolio"),
          width = "100%",
          choice = portfolios_id['ID']
        )
      })
    } else {
      showModal(modalDialog(
        title = h2("Error"),
        p(
          paste0(
            "There is no portfolio named '",
            input$portfolio_delete,
            "', try another name."
          )
        ),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  # TRADE RECORDS --------------------------------------------------------------
  observeEvent(input$Portfolio_ID, {
    output$date_range <- renderUI({
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      min_date_query <- paste0("SELECT MIN(trade_date) AS trade_date FROM Trade_Records WHERE portfolio_id = ", input$Portfolio_ID, ";")
      max_date_query <- paste0("SELECT MAX(trade_date) AS trade_date FROM Trade_Records WHERE portfolio_id = ", input$Portfolio_ID, ";")
      
      min_date <- dbGetQuery(db, min_date_query)[1, 'trade_date']
      max_date <- dbGetQuery(db, max_date_query)[1, 'trade_date']
      
      dateRangeInput("date_range", h3("Date Range"), width = "100%", start = as.Date(min_date), end = as.Date(max_date))
    })
  })
  
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
    
    # browser()
    query <-
      paste(
        "select * from Trade_Records where portfolio_id =",
        input$Portfolio_ID,
        " AND trade_date  between ",
        "'",
        input$date_range[1],
        "'",
        " AND ",
        "'",
        input$date_range[2],
        "'",
        ";",
        sep = ""
      )
    
    trade_data_list <- dbGetQuery(db, query)
    
    output$trade_list <- DT::renderDataTable({
      trade_data_list
    })
  })
  
  output$add_tickers <- renderUI({
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <-
      paste("select ticker from Stocks;")
    
    tickers <- dbGetQuery(db, query)
    
    selectInput(
      "add_tickers",
      label = h3("Tickers"),
      choices = tickers,
      width = "100%"
    )
  })
  
  output$edit_tickers <- renderUI({
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <-
      paste("select ticker from Stocks;")
    
    tickers <- dbGetQuery(db, query)
    
    selectInput(
      "edit_tickers",
      label = h3("Tickers"),
      choices = tickers,
      width = "100%"
    )
  })
  
  output$delete_record <- renderUI({
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <-
      paste0(
        "select trade_id from Trade_Records where portfolio_id = ",
        input$Portfolio_ID3,
        ";"
      )
    
    records <- dbGetQuery(db, query)
    
    selectInput(
      "delete_records",
      label = h3("Records"),
      choices = records,
      width = "100%"
    )
  })
  
  output$edit_record <- renderUI({
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    query <-
      paste0(
        "select trade_id from Trade_Records where portfolio_id = ",
        input$Portfolio_ID3,
        ";"
      )
    
    records <- dbGetQuery(db, query)
    
    selectInput(
      "edit_record",
      label = h3("Record"),
      choices = records,
      width = "100%"
    )
  })
  
  observeEvent(input$addTrade, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    if (input$add_direction == "Buy") {
      direction <- 1
    } else {
      direction <- 0
    }
    
    query <-
      paste0(
        "INSERT INTO Trade_Records(portfolio_id, trade_date, ticker, price_per_share, quantity, direction) VALUES (",
        input$Portfolio_ID3,
        ", '",
        input$add_date,
        "', '",
        input$add_tickers,
        "', ",
        input$add_price,
        ", ",
        input$add_qty,
        ", ",
        direction,
        ");"
      )
    dbSendQuery(db, query)
    
    showModal(modalDialog(
      title = h2("Success!"),
      p(paste0("The trade was successfully recorded.")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$deleteTrade, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    on.exit(dbDisconnect(db), add = TRUE)
    
    query <-
      paste0("DELETE FROM Trade_Records WHERE trade_id = ",
             input$delete_records,
             ";")
    
    dbSendQuery(db, query)
    
    showModal(modalDialog(
      title = h2("Success!"),
      p(paste0("The trade was successfully deleted.")),
      easyClose = TRUE,
      footer = NULL
    ))
    
    output$delete_record <- renderUI({
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      on.exit(dbDisconnect(db), add = TRUE)
      
      # browser()
      query <-
        paste0(
          "select trade_id from Trade_Records where portfolio_id = ",
          input$Portfolio_ID3,
          ";"
        )
      
      records <- dbGetQuery(db, query)
      
      selectInput(
        "delete_records",
        label = h3("Records"),
        choices = records,
        width = "100%"
      )
    })
  })
  
  observeEvent(input$editTrade, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    on.exit(dbDisconnect(db), add = TRUE)
    
    if (input$edit_direction == "Buy") {
      direction <- 1
    } else {
      direction <- 0
    }
    
    query <-
      paste0(
        "UPDATE Trade_Records SET",
        " portfolio_id = ",
        input$Portfolio_ID3,
        ", trade_date = '",
        input$edit_date,
        "', ticker = '",
        input$edit_tickers,
        "', price_per_share = ",
        input$edit_price,
        ", quantity = ",
        input$edit_qty,
        ", direction = ",
        direction,
        " WHERE trade_id = ",
        input$edit_record,
        ";"
      )
    
    dbSendQuery(db, query)
    
    showModal(modalDialog(
      title = h2("Success!"),
      p(paste0("The trade was successfully edited.")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$edit_record, {
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    on.exit(dbDisconnect(db), add = TRUE)
    
    query <-
      paste0("SELECT * FROM Trade_Records WHERE trade_id = ",
             input$edit_record)
    row <- dbGetQuery(db, query)
    
    if (row$direction) {
      direction <- "Buy"
    } else {
      direction <- "Sell"
    }
    
    updateDateInput(session = getDefaultReactiveDomain(),
                    "edit_date",
                    value = row$trade_date)
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "edit_tickers",
                      selected = row$ticker)
    updateTextInput(session = getDefaultReactiveDomain(),
                    "edit_price",
                    value = row$price_per_share)
    updateTextInput(session = getDefaultReactiveDomain(),
                    "edit_qty",
                    value = row$quantity)
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "edit_direction",
                      selected = direction)
  })
  
  # ANALYSIS--------------------------------------------------------------------
  observeEvent(input$Go1, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    query <-
      paste(
        "select t.*, s.* from Trade_Records as t join Stocks as s on t.ticker=s.ticker where portfolio_id =",
        input$Portfolio_ID2,
        ";",
        sep = ""
      )
    print(query)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
    output$mytable1 = DT::renderDataTable({
      data
    })
    # Portfolio Allocation
    da <- data
    da$direction[da$direction == TRUE] <- 1
    da$direction[da$direction == FALSE] <- -1
    da$trade_amount <-
      da$price_per_share * da$quantity * da$direction
    ttl <- aggregate(trade_amount ~ ticker, da, sum)
    ttl$trade_amount <- round(abs(ttl$trade_amount), 0)
    
    output$output1_1 = renderPlot({
      treemap(
        ttl,
        index = c("ticker", "trade_amount"),
        vSize = "trade_amount",
        overlap.labels = 1,
        align.labels = list(c("center", "center"),
                            c("right", "bottom")),
        type = "index"
      )
      
    })
    
    # Performances
    t1 <- aggregate(trade_amount ~ ticker, da, sum)
    ind <- which(t1$trade_amount >= 0)
    t2 <- aggregate(quantity ~ ticker, da, sum)
    s_price <- aggregate(stock_price ~ ticker, da, mean)
    cur <- t2$quantity * s_price$stock_price
    cur[ind] <- -cur[ind]
    # o_price<-aggregate(price_per_share  ~ ticker, da, mean)
    diff_amount <- -round((cur + t1$trade_amount), 2)
    return <- round(diff_amount / abs(t1$trade_amount), 4) * 100
    avg_cost <- round(t1$trade_amount / t2$quantity, 2)
    analysis <-
      data.frame(cbind(
        t1$ticker,
        diff_amount,
        return,
        avg_cost,
        s_price$stock_price
      ))
    colnames(analysis) <-
      c(
        "Ticker",
        "Return in Amount",
        "Return in Percentage",
        "Average Cost",
        "Current Price"
      )
    library(DT)
    # datatable(analysis, rownames = FALSE) %>%
    #   formatStyle(columns = c("Return in Amount","Return in Percentage"),
    #               color = styleInterval(c(-1e6, 0, 1e6), c("white", "red","black", "white")))
    output$mytable2 = DT::renderDataTable({
      datatable(analysis, rownames = FALSE) %>%
        formatStyle(
          columns = c("Return in Amount", "Return in Percentage"),
          color = styleInterval(c(-1e6, 0, 1e6), c("white", "red", "black", "white"))
        )
    })
    # Direction
    # da<-data
    output$output1_2 = renderPlot({
      ttl1 <- aggregate(trade_amount ~ ticker, da, sum)
      ttl1$trade_amount <- round(ttl1$trade_amount, 0)
      library(waterfalls)
      library(ggpubr)
      p1 <- waterfall(ttl1, calc_total = TRUE, linetype = 1) +
        ggtitle("Waterfall chart of direction in the portfolio")
      
      dir <- c(sum(ttl1$trade_amount[ttl1$trade_amount < 0]),
               sum(ttl1$trade_amount[ttl1$trade_amount > 0]))
      dir <- cbind(c("Short", "Long"), dir)
      dir <- as.data.frame(dir)
      p2 <- ggplot(dir, aes(x = V1, y = dir)) +
        geom_bar(
          stat = "identity",
          width = 0.5,
          fill = c("#F8766D", "#00BFC4")
        ) +
        coord_flip() +
        ggtitle("Bar chart of total direction in the portfolio") +
        ylab("Amount") +
        xlab("Direction")
      ggarrange(p1, p2,
                ncol = 2, nrow = 1)
    })
    
  })
  
  # COMPANIES ------------------------------------------------------------------
  
  companies_data <-
    dbGetQuery(db, "SELECT ticker, company_name, sector FROM Company;")
  
  output$companies <- renderUI({
    size <- nrow(companies_data)
    
    args <- lapply(1:size, function(.x)
      company_card(
        .ticker = companies_data[.x, "ticker"],
        .name = companies_data[.x, "company_name"],
        .sector = companies_data[.x, "sector"]
      ))
    
    args$cellArgs <- list(style = "
        width: auto;
        height: auto;
        margin: 10px 5px;
        ")
    
    do.call(shiny::flowLayout, args)
  })
  
  observe({
    onclick("card-AAL", display_company_info("AAL"))
    onclick("card-AAPL", display_company_info("AAPL"))
    onclick("card-ABNB", display_company_info("ABNB"))
    onclick("card-AMZN", display_company_info("AMZN"))
    onclick("card-BA", display_company_info("BA"))
    onclick("card-DIS", display_company_info("DIS"))
    onclick("card-F", display_company_info("F"))
    onclick("card-GME", display_company_info("GME"))
    onclick("card-GS", display_company_info("GS"))
    onclick("card-HD", display_company_info("HD"))
    onclick("card-JNJ", display_company_info("JNJ"))
    onclick("card-JPM", display_company_info("JPM"))
    onclick("card-KO", display_company_info("KO"))
    onclick("card-MCD", display_company_info("MCD"))
    onclick("card-MMM", display_company_info("MMM"))
    onclick("card-MRNA", display_company_info("MRNA"))
    onclick("card-NVDA", display_company_info("NVDA"))
    onclick("card-PFE", display_company_info("PFE"))
    onclick("card-PG", display_company_info("PG"))
    onclick("card-RCL", display_company_info("RCL"))
    onclick("card-SQ", display_company_info("SQ"))
    onclick("card-TSLA", display_company_info("TSLA"))
    onclick("card-V", display_company_info("V"))
    onclick("card-VZ", display_company_info("VZ"))
    onclick("card-WMT", display_company_info("WMT"))
  })
  
}

shinyApp(ui, server)