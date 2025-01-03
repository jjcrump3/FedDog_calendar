#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(calendR)
library(ggplot2)
library(fs)
library(glue)
library(shiny)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tracking Method for 'Who Fed the Dog?'"),
    
    # Year and Month of interest ------------------------- 
    fluidRow(
          column(4, 
                 selectInput(inputId = "year_chr",
                                label = "Select Year of Interest",
                                choices = c(year(today()) - 2, 
                                            year(today()) - 1,
                                            year(today()),
                                            year(today()) + 1, 
                                            year(today()) + 2),
                                selected = year(today())
                                )
                 ),
          column(4, 
                 selectInput(inputId = "month_chr",
                      label = "Select Month Calendar",
                      choices = month.name,
                      selected = month.name[month(today())]
                      )
                 )
          ),
      
      # Heartworm Medication Label and day of application ----------------------
      fluidRow(
          column(4,
                 textInput(inputId = "Heartworm_label",
                           label = "Heartworm For? (Insert Dog(s) name)",
                           value = "Dog(s)")
                 ),

          column(4,
                 selectInput(inputId = "heartworm_day",
                             label = "Date for Heartworm Medication",
                             selected = 12,
                             choices = c(seq(from = 1,
                                             to = days_in_month(month(today())))
                                         )
                              )
                 )
          ),
        # Notes and annotation embedded in calendar ------------------------
        # fluidRow(
        #   column(4,
        #          textAreaInput(
        #            "text",
        #            "Text input",
        #            value = "Dog 1 = 1.5 cups of food; Dog 2 = 1.25 cups of food"
        #            )
        #          )
        #   ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 8,
          # Add the download button for printing
          downloadButton("download_pdf", "Download Calendar as PDF"),
          plotOutput("calender",
                     height = "600px"),
          
          # verbatimTextOutput("value"), # Adding notes to the calendar would be nice, but not required
          
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    days_in_selected_month <- days_in_month(as.Date(paste0(input$year_chr, "-", 
                                                           match(input$month_chr, month.name), "-01")))
    
    updateSelectInput(
      session,
      "heartworm_day",
      choices = seq(1, days_in_selected_month),
      selected = min(input$heartworm_day, days_in_selected_month) # Retain current selection or adjust if out of range
    )
  })
  
  # Reactive expression to generate the calendar plot
  generate_calendar_plot <- reactive({
    custom_events <- calendR(year = input$year_chr,
                             month = match(input$month_chr, month.name)
    )
    
    events <- custom_events$data |> 
      mutate(rows = row_number(),
             date = case_when(
               rows == input$heartworm_day ~ glue("Heartworm - {input$Heartworm_label}"),
               .default = NA)
      ) |> 
      pull(date)
    
    custom_cal <- calendR(year = input$year_chr,
                          month = match(input$month_chr, month.name),
                          title = glue("{input$month_chr} {input$year_chr}: Fed Dog?"),
                          special.days = events,
                          special.col = c("lightblue"),
                          legend.pos = "top",
                          text.size = 3,
                          margin = 1,
                          weeknames = c("Mo", "Tu",  # Week names
                                        "We", "Th",
                                        "Fr", "Sa",
                                        "Su"),
                          day.size = 2.25
    )
    
    # Horizontal ------------------
    # Reversed Diagonal -----------------------
    annotate_AmPm <- function(y) {
      annotate(geom = "text",
               x = -1,
               y = y,
               label = "AM\nPM")
    }
    
    custom_cal + 
      geom_segment(aes(x = (dow) - 0.5, xend = (dow) + 0.5,
                       y = y + 0.5, yend = y - 0.5), col = "gray60") +
      annotate_AmPm(1:5)
  })
  
  # Render the plot in the UI
  output$calender <- renderPlot({
    generate_calendar_plot()
  })
  
  output$value <- renderText({input$text})
  
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      glue("Output/{input$year_chr}/FeedDog_calendar_{input$year_chr}_{input$month_chr}.pdf")
    },
    content = function(file) {
      # Save the current calendar plot as a PDF file
      ggsave(filename = file,
             plot = generate_calendar_plot(),
             device = cairo_pdf,
             units = "in",
             height = 8.5,
             width = 11)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
