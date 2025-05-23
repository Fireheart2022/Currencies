rm(list=ls())
library(highcharter)
library(timetk)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Currency Exchange Rates"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("currency", "Choose a Currency:", 
                  choices = c("EUR", "USD", "PLN"),
                  selected = "EUR")
    ),
    
    mainPanel(
      highchartOutput("currency_plot"),
      verbatimTextOutput("summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on selected currency
  selected_data <- reactive({
    rm(list=ls())
    
    # Load necessary libraries
    library(httr)
    library(readxl)
    library(dplyr)
    
    # Define the months for the file names
    months <- c("jan", 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    
    # Initialize an empty list to store data frames
    tab <- list()
    
    # Loop through each month to create URLs and download files
    for (i in seq_along(months)) {
      month <- months[i]
      url <- paste0("https://github.com/Fireheart2022/Currencies/raw/refs/heads/main/fx_", month, "24_en.xlsx")
      print(url)
      
      # Create a temporary file for each download
      temp_file <- tempfile(fileext = ".xlsx")
      
      # Download the file
      req <- GET(url, 
                 authenticate(Sys.getenv("GITHUB_PAT"), ""),
                 write_disk(temp_file, overwrite = TRUE))
      
      # Read the Excel file into R and store it in the list
      tab[[i]] <- read_excel(temp_file)
    }
    
    # Combine all data frames into one, handling differing column names
    combined_data <- bind_rows(tab)
    
    # Print the combined data frame
    print(combined_data)
    
    combined_data[,4:14]<- NULL
    df <-combined_data%>%
      slice(-1)%>%
      `colnames<-`(.,value=c("Date", "Currency","Rate"))
    
    combined_data<- df%>%
      transform(
        Date=as.Date(as.numeric(as.character(df$Date)), origin = "1899-12-30"),
        Rate= as.numeric(Rate)
      )
    
    a=list()
    for (i in 1:12){
      a[i]= filter(combined_data,Currency=="US Dollar")%>%list()
    }
    usd=do.call(rbind.data.frame,a)
    
    
    for (i in 1:12){
      a[i]= filter(combined_data,Currency=="Euro")%>%list()
    }
    eur=do.call(rbind.data.frame,a)
    
    
    for (i in 1:12){
      a[i]= filter(combined_data,Currency=="Polish Zloty")%>%list()
    }
    pln=do.call(rbind.data.frame,a)
    
    
    
    
    exchange_rates <- left_join(usd,eur, by="Date")%>%
      left_join(., pln, by="Date")%>%
      transform(Date= as.Date(Date))
    
    rm(list=c("a", 'usd','eur','pln'))
    
    df <- data.frame(exchange_rates)%>%
      transform(
        USD= Rate.x %>%as.numeric(),
        EUR = Rate.y%>%as.numeric(),
        PLN= Rate %>%as.numeric()
      )
    
    df_xts <- timetk::tk_xts(df, date_var="Date")
    
    return(df_xts)
  })
  
  # Render highchart output
  output$currency_plot <- renderHighchart({
    data <- selected_data()
    
    # Create a highchart plot for the selected currency
    highchart(type="stock") %>%
      hc_title(text = paste(input$currency, "Exchange Rates")) %>%
      hc_add_series(name = input$currency%>%as.character(), data =data[,input$currency] , type = "line") %>%
      hc_yAxis(title = list(text = "Exchange Rate")) %>%
      hc_tooltip(pointFormat = '{point.x:%b %d, %Y}: {point.y:.2f}')%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$summary<- renderPrint({
    data <- selected_data()
    summary(data[,input$currency])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

runGitHub(repo="Currencies",username="Fireheart2022", ref='main')
