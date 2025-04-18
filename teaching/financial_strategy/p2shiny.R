
library(downloader)
library(dplyr)
library(GetQuandlData)
library(ggplot2)
library(ggthemes)
library(PerformanceAnalytics)
library(plotly)
library(readxl)
library(roll)
library(tidyr)
library(tidyquant)
library(yfR)
library(shiny)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Start Date:",
                value = '1995-01-01', format = "yyyy-mm-dd"),
      dateInput("end_date", "End Date:",
                value = Sys.Date(), format = "yyyy-mm-dd")
    ),
    mainPanel(
      plotlyOutput("cumulative_plot")
    )
  )
)


server <- function(input, output) {
  
  output$cumulative_plot <- renderPlotly({
    # Define the user-selected start and end dates
    start <- input$start_date
    end <- input$end_date
    
    # Download Ibov data based on selected dates
    stock <- '^BVSP'
    ibov <- yf_get(tickers = stock, first_date = start, last_date = end)
    ibov <- ibov[order(as.numeric(ibov$ref_date)),]
    ibov$Ibov_return <- ibov$cumret_adjusted_prices - 1
    
    # Download Selic data
    selic <- GetBCBData::gbcbd_get_series(
      id = 432,
      first.date = start,
      last.date = end
    )
    names(selic) <- c("ref_date", "selic")
    selic$ref_date <- as.Date(selic$ref_date, format = "%d/%m/%Y")
    selic <- na.omit(selic)
    selic$selic <- selic$selic / (252 * 100)
    
    # Cumulative return Selic
    return_selic <- data.frame(nrow(selic):1)
    colnames(return_selic) <- "selic_return"
    for(i in (2:nrow(selic))) {
      return_selic[i, 1] <- Return.cumulative(selic$selic[1:i])
    }
    
    # Merging dataframes
    selic <- cbind(selic, return_selic)
    df <- merge(ibov, selic, by = c("ref_date"))
    df$selic_return[1] <- NA
    df$Ibov_return[1] <- NA
    
    # Plot the data
    p <- ggplot(df, aes(ref_date)) +
      geom_line(aes(y = Ibov_return, colour = "Ibov")) +
      geom_line(aes(y = selic_return, colour = "Selic")) +
      labs(y = 'Cumulative return (daily)') +
      labs(x = '') +
      theme_solarized() +
      ggtitle("Cumulative Returns for Ibov and Selic")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)


#rsconnect::deployApp('p2shiny.qmd')

