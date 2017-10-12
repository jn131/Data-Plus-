library(data.table)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(shiny)
library(shinydashboard)
library(sp)

setwd("~/Desktop/Data+/Project Data/Durham_County_Mortalities/")

df_count <- read.csv("External_Count_Data.csv")
df_pop <- read.csv("External_Pop_Data.csv")

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Durham County"),
    dashboardSidebar(
      selectInput(inputId = "options", label = "Filter", choices = c("Title","Scale","Year","Sex","Race","Cause of Death")), 
      conditionalPanel(condition = "input.options == 'Title'", textInput(inputId = "title", label = "Write A Title", value = "Mortality Plot")), 
      conditionalPanel(condition = "input.options == 'Scale'", checkboxInput(inputId = "scale", label = "Set Scale", value = F)),  
      conditionalPanel(condition = "input.options == 'Scale' & input.scale > 0", numericInput(inputId = "min", label = "Minimum", min = 0, max = 800, value = 0)),
      conditionalPanel(condition = "input.options == 'Scale' & input.scale > 0", numericInput(inputId = "max", label = "Maximum", min = 0, max = 800, value = 800)),
      conditionalPanel(condition = "input.options == 'Year'", radioButtons(inputId = "year", label = "Year", 
                                                                           choiceNames = c("All","2005-2009","2010-2014"), choiceValues = c(1:3))), 
      conditionalPanel(condition = "input.options == 'Sex'", radioButtons(inputId = "sex", label = "Sex", choiceNames = c("All","Female","Male"), choiceValues = c(1:3))), 
      conditionalPanel(condition = "input.options == 'Race'", radioButtons(inputId = "race", label = "Race", 
                                                                           choiceNames = c("All","African American","White, Non-Hispanic"), choiceValues = c(1:3))),
      conditionalPanel(condition = "input.options == 'Cause of Death'", radioButtons(inputId = "cause", label = "Any Cause of Death", 
                                                                                     choiceNames = c("All","Cancer","Heart Disease"), choiceValues = c(1:3))),
      actionButton(inputId = "reset_all", label = "Reset All")
    ),
    dashboardBody(selectInput(inputId = "plottype", label = "Plot Type", choices = c("Count","Rate")), 
                  plotOutput(outputId = "plot", height = 500))
  )
)


server <- function(input, output, session) {
  block_groups <- readOGR(dsn = "Durham_Shapefile", layer = "DurhamShapefile", stringsAsFactors = F)
  column_name <- reactive({paste("X",input$year, input$sex, input$race, input$cause, sep = "")})
  
  count <- reactive({df_count[, c("GEOID10",column_name())]})
  data_count <- reactive({setnames(count(), old = colnames(count()[2]), new = "count")})
  
  rate <- reactive({merge(df_count[, c("GEOID10",column_name())], df_pop[, c("GEOID10",column_name())], by = "GEOID10")})
  rate_name <- reactive({setnames(rate(), old = colnames(rate()[2:3]), new = c("count", "population"))})
  data_rate <- reactive({rate_name() %>% dplyr::group_by(GEOID10) %>% summarise(rate = sum(count/population))})
  
  plotinput_count <- reactive({
    spplot(merge(block_groups, data_count(), by = "GEOID10"),
           "count", col.regions = brewer.pal(9, "Blues"), cuts = 8,
           at = {seq(from = (if (input$scale > 0) {as.numeric(input$min)} else {20}), 
                     to = (if (input$scale > 0) {as.numeric(input$max)} 
                           else {max(data_count()$count, na.rm = T) + max(data_count()$count, na.rm = T)*.03}), 
                     by = (((if (input$scale > 0) {input$max} else {max(data_count()$count, na.rm = T) + max(data_count()$count, na.rm = T)*.03}) - 
                              (if (input$scale > 0) {input$min} else {20}))/9))},
           main = list(label = input$title, cex = 1.5)
    )
  })
  plotinput_rate <- reactive({
    spplot(merge(block_groups, data_rate(), by = "GEOID10"),
           "rate", col.regions = brewer.pal(9, "Blues"), cuts = 8,
           at = {seq(from = (if (input$scale > 0) {as.numeric(input$min)} else {0}), 
                     to = (if (input$scale > 0) {as.numeric(input$max)} 
                           else {max(data_rate()$rate, na.rm = T) + max(data_rate()$rate, na.rm = T)*.03}), 
                     by = (((if (input$scale > 0) {input$max} else {max(data_rate()$rate, na.rm = T) + max(data_rate()$rate, na.rm = T)*.03}) - 
                              (if (input$scale > 0) {input$min} else {0}))/9))},
           main = list(label = input$title, cex = 1.5)
    )
  })
  output$plot <- renderPlot({if (input$plottype == "Count") {print(plotinput_count())} else if (input$plottype == "Rate") {print(plotinput_rate())}})
  
  observe({
    if (input$reset_all > 0) {
      updateTextInput(session = session, inputId = "title", value = "Mortality Plot")
      updateCheckboxInput(session = session, inputId = "scale", value = F)
      updateNumericInput(session = session, inputId = "min", min = 0, max = 800, value = 0)
      updateNumericInput(session = session, inputId = "max", min = 0, max = 800, value = 800)
      updateRadioButtons(session = session, inputId = "year", label = "Year", choiceNames = c("All","2005-2009","2010-2014"), choiceValues = c(1:3)) 
      updateRadioButtons(session = session, inputId = "sex", label = "Gender", choiceNames = c("All","Female","Male"), choiceValues = c(1:3)) 
      updateRadioButtons(session = session, inputId = "race", label = "Race", choiceNames = c("All","African American","White, Non-Hispanic"), choiceValues = c(1:3))
      updateRadioButtons(session = session, inputId = "cause", label = "Any Cause of Death",choiceNames = c("All","Cancer","Heart Disease"), choiceValues = c(1:3))
    }
  })
  
}

shinyApp(ui = ui, server = server)


