library(shiny)
library(shinydashboard)

#### Set Working Directory
setwd("~/Desktop/College/Data+/Project Data")

#### Set County Information
county_dsn <- "Durham_Block_Groups"
county_layer <- "DurhamBlockGroups"
problem_blkgr <- c("370639801001") # Removes specified block groups from the rate maps 
# Leave this as c("") if there are no block groups that need to be removed
# If there are multiple block groups, separate them with commas: ex. c("370639801001","370630015031")


# Variables
max_age <- max(as.numeric(deaths_county$AGE)) 
roundUp <- function(x) 10^floor(log10(x)) * ceiling(x/(10^(floor(log10(x)))))
max_blkgrp <- deaths_county %>% dplyr::group_by(GeoIDBlkGr) %>% dplyr::summarize(count = sum(n()))
set_max_blkgrp <- roundUp(max(as.numeric(max_blkgrp$count)))


# Inputs Functions 
title.f <- function(options, title, mortalitytitle) {conditionalPanel(condition = paste("input.", options," == 'Title'", sep = ""),
                                                                      textInput(inputId = title, label = "Write A Title", value = mortalitytitle))}
scale.f <- function(options, scale) {conditionalPanel(condition = paste("input.", options, " == 'Scale'", sep = ""), 
                                                      checkboxInput(inputId = scale, label = "Set Scale", value = F))}  
min.f <- function(options, scale, min, set_max) {conditionalPanel(condition = paste("input.", options, " == 'Scale' & input.", scale, " > 0", sep = ""), 
                                                                  numericInput(inputId = min, label = "Minimum", min = 0, max = set_max, value = 0))}
max.f <- function(options, scale, max, set_max, set_value) {conditionalPanel(condition = paste("input.", options," == 'Scale' & input.", scale, " > 0", sep = ""),
                                                                             numericInput(inputId = max, label = "Maximum", min = 0, max = set_max, value = set_value))}
year.f <- function(options, year) {conditionalPanel(condition = paste("input.", options, " == 'Year'", sep = ""), 
                                                    checkboxGroupInput(inputId = year, label = "Year", choices = c(unique(df$YEAR))))}
causeoptions.f <- function(options,causeoptions) {conditionalPanel(condition = paste("input.",options,"=='Cause of Death'", sep = ""), 
                                                                   selectInput(inputId = causeoptions, label = "Type of Cause", choices = c("None","Primary Cause of Death","Any Cause of Death")))}
primarychoice.f <- function(causeoptions,primarychoice) {conditionalPanel(condition = paste("input.",causeoptions,"=='Primary Cause of Death'", sep = ""),
                                                                          selectInput(inputId = primarychoice, label = "Type of Input", choices = c("","Name","ICD 10 Code")))}
primaryname.f <- function(options,primarychoice,causeoptions,primaryname) {conditionalPanel(condition = paste("input.",options,"=='Cause of Death' & input.", primarychoice,"=='Name' & input.",causeoptions,"!= 'Any Cause of Death'", sep = ""), 
                                                                                            textAreaInput(inputId = primaryname, label = "Enter Name of Cause of Death", value = "", placeholder = "e.g. Alzheimer's Disease, Parkinson's Disease"))}
primaryicd10.f <- function(options,primarychoice,causeoptions,primaryicd10) {conditionalPanel(condition = paste("input.",options,"=='Cause of Death' & input.",primarychoice,"=='ICD 10 Code' & input.",causeoptions,"!='Any Cause of Death'", sep = ""), 
                                                                                              textAreaInput(inputId = primaryicd10, label = "Enter ICD 10 Code", value="", placeholder = "e.g. C349, C509"))}
primarymain.f <- function(options,causeoptions,primarymain,reset_primary) {conditionalPanel(condition = paste("input.",options,"=='Cause of Death' & input.",causeoptions,"=='Primary Cause of Death' & input.",causeoptions,"!='Any Cause of Death'", sep=""),
                                                                                            checkboxGroupInput(inputId = primarymain, label = "Main Causes", choices = c("Cancer","Diabetes","Heart Disease","HIV")))}
anyicd10.f <- function(causeoptions,anyicd10) {conditionalPanel(condition = paste("input.",causeoptions,"=='Any Cause of Death'", sep = ""),
                                                                textAreaInput(inputId = anyicd10, label = "Enter ICD 10 Code", value = "", placeholder = "e.g. C349, C509"))}
anymain.f <- function(options,causeoptions,anymain,reset_any) {conditionalPanel(condition = paste("input.",options,"=='Cause of Death' & input.",causeoptions,"=='Any Cause of Death' & input.",causeoptions,"!='Primary Cause of Death'", sep = ""),
                                                                                checkboxGroupInput(inputId = anymain, label = "Main Causes", choices = c("Cancer","Diabetes","Heart Disease","HIV")))}
reset.f <- function(reset_all) {actionButton(inputId = reset_all, label = "Reset All")}


ui <- fluidPage(navbarPage(
  "Mortality Plots",
  
  # Inputs Counts 
  tabPanel("Counts", 
           dashboardPage(
             dashboardHeader(title = "Mortality Count Plot"),
             dashboardSidebar(
               selectInput(inputId = "options_count", label = "Filter", choices = c("Title","Scale","Year","Sex","Age","Ethnicity","Race","Cause of Death")), title.f("options_count","title_count","Mortality Count Plot"), 
               scale.f("options_count","scale_count"), min.f("options_count","scale_count","min_count", set_max_blkgrp), max.f("options_count","scale_count","max_count", set_max_blkgrp, set_max_blkgrp), year.f("options_count","year_count"), 
               causeoptions.f("options_count","causeoptions_count"), primarychoice.f("causeoptions_count","primarychoice_count"), primaryname.f("options_count","primarychoice_count","causeoptions_count","primaryname_count"), 
               primaryicd10.f("options_count","primarychoice_count","causeoptions_count","primaryicd10_count"), primarymain.f("options_count","causeoptions_count","primarymain_count","reset_primary_count"), 
               anyicd10.f("causeoptions_count","anyicd10_count"), anymain.f("options_count","causeoptions_count","anymain_count","reset_any_count"),
               conditionalPanel(condition = "input.options_count == 'Sex'", checkboxGroupInput(inputId = "sex_count", label = "Sex", choices = c(unique(sort(df$SEX))))),
               conditionalPanel(condition = "input.options_count == 'Age'", sliderInput(inputId = "age_count", label = "Age Range", min = 0, max = max_age, value = c(0, max_age))), 
               conditionalPanel(condition = "input.options_count == 'Ethnicity'", checkboxGroupInput(inputId = "hispanic_count", label = "Ethnicity", choices = c(unique(sort(df$HISPANIC))))),
               conditionalPanel(condition = "input.options_count == 'Race'", checkboxGroupInput(inputId = "race_count", label = "Race", choices = c(unique(sort(df$RACE))))),
               reset.f("reset_all_count")),
             dashboardBody(plotOutput(outputId = "plot_count", height = 450), downloadButton(outputId = "download_count", label = "Download Plot"), downloadButton(outputId = "download_data_count", label = "Download Data"))
           )),
  
  # Inputs Rates 
  tabPanel("Rates",
           dashboardPage(
             dashboardHeader(title = "Mortality Rate Plot"),
             dashboardSidebar(
               selectInput(inputId = "options_rate", label = "Filter", choices = c("Title","Scale","Year","Ethnicity","Race","Cause of Death")), title.f("options_rate","title_rate","Mortality Rate Plot"), 
               scale.f("options_rate","scale_rate"), min.f("options_rate","scale_rate","min_rate", 100), max.f("options_rate","scale_rate","max_rate", 1000, 50), year.f("options_rate","year_rate"), 
               causeoptions.f("options_rate","causeoptions_rate"), primarychoice.f("causeoptions_rate","primarychoice_rate"), primaryname.f("options_rate","primarychoice_rate","causeoptions_rate","primaryname_rate"), 
               primaryicd10.f("options_rate","primarychoice_rate","causeoptions_rate","primaryicd10_rate"), primarymain.f("options_rate","causeoptions_rate","primarymain_rate","reset_primary_rate"), 
               anyicd10.f("causeoptions_rate","anyicd10_rate"), anymain.f("options_rate","causeoptions_rate","anymain_rate","reset_any_rate"),
               conditionalPanel(condition = "input.options_rate == 'Ethnicity'", selectInput(inputId = "hispanic_rate", label = "Ethnicity", choices = c("All","Hispanic","Non-Hispanic"))),
               conditionalPanel(condition = "input.options_rate == 'Race'", checkboxGroupInput(inputId = "race_rate", label = "Race", choices = sort(unique(df$race_shiny)))),
               reset.f("reset_all_rate")),
             dashboardBody(plotOutput(outputId = "plot_rate", height = 450), downloadButton(outputId = "download_rate", label = "Download Plot"), downloadButton(outputId = "download_data_rate", label = "Download Data"))
           )),
  
  # Inputs Age-Adjusted 
  tabPanel("Age-Adjusted",  
           dashboardPage(
             dashboardHeader(title = "Age-Adjusted Mortality Rate Plot"),
             dashboardSidebar(
               selectInput(inputId = "options_ageadj", label = "Filter", choices = c("Title","Scale","Year","Sex","Cause of Death")), title.f("options_ageadj","title_ageadj","Age-Adjusted Mortality Rate Plot"), 
               scale.f("options_ageadj","scale_ageadj"), min.f("options_ageadj","scale_ageadj","min_ageadj", 100), max.f("options_ageadj","scale_ageadj","max_ageadj", 1000, 50), year.f("options_ageadj","year_ageadj"), 
               causeoptions.f("options_ageadj","causeoptions_ageadj"), primarychoice.f("causeoptions_ageadj","primarychoice_ageadj"), primaryname.f("options_ageadj","primarychoice_ageadj","causeoptions_ageadj","primaryname_ageadj"), 
               primaryicd10.f("options_ageadj","primarychoice_ageadj","causeoptions_ageadj","primaryicd10_ageadj"), primarymain.f("options_ageadj","causeoptions_ageadj","primarymain_ageadj","reset_primary_ageadj"), 
               anyicd10.f("causeoptions_ageadj","anyicd10_ageadj"), anymain.f("options_ageadj","causeoptions_ageadj","anymain_ageadj","reset_any_ageadj"),
               conditionalPanel(condition = "input.options_ageadj == 'Sex'", selectInput(inputId = "sex_ageadj", label = "Sex", choices = c("All","Male","Female"))),
               reset.f("reset_all_ageadj")),
             dashboardBody(plotOutput(outputId = "plot_ageadj", height = 450), downloadButton(outputId = "download_ageadj", label = "Download Plot"), downloadButton(outputId = "download_data_ageadj", label = "Download Data"))
           ))
))


server <- function(input, output, session) {
  # Block Groups
  block_groups <- readOGR(dsn = county_dsn, layer = county_layer, stringsAsFactors = F)
  
  # Plot Functions 
  trim.f <- function (x) gsub("^\\s+|\\s+$", "", x)
  text.f <- function(input, category, data) (if (input == "") {category %in% unique(data)} else {category %in% trim.f(c(unlist(strsplit(input, ","))))})
  select.f <- function(input, category, data) (if (is.null(input)) {category %in% unique(data)} else {category %in% c(input)})
  plot.f <- function(data, column, data_column, scale_input, min_input, max_input, title_input) { 
    spplot(merge(block_groups, data, by = "GEOID10"), 
           column, col.regions = brewer.pal(9, "Blues"), cuts = 8,
           at = {seq(from = (if (scale_input > 0) {as.numeric(min_input)} else {0}),
                     to = (if (scale_input > 0) {as.numeric(max_input)} else {max(data_column, na.rm = T) + max(data_column, na.rm = T)*.03}),
                     by = (if (scale_input > 0) {max_input} else {max(data_column, na.rm = T) + max(data_column, na.rm = T)*.03} - 
                             (if (scale_input > 0) {min_input} else {0}))/9)},
           main = list(label = title_input, cex = 1.5))}
  download.f <- function(title_input, plot_input) {downloadHandler(
    filename = function() {paste(title_input, ".png", sep = "")},
    content = function(file) {
      png(file)
      print(plot_input)
      dev.off()},
    contentType = 'image/png')}
  download.data.f <- function(title_input, data_input) {downloadHandler(
    filename = function() {paste(title_input, ".csv", sep = "")},
    content = function(file) {write.csv(data_input, file)},
    contentType = 'text/csv')}
  apply.f <- function(data, input) apply(data[,(c("Zero1","Zero2",c(input)))], 1, sum)
  mutate.f <- function (variable) (if (variable != 0) {variable} else {NA})
  summarize.f <- function(input, total_final, final) {if (is.null(input)) {total_final} else if (!is.null(input)) {final}}
  
  # Plot Counts 
  num_count <- reactive({df %>% dplyr::group_by(GEOID10) %>% dplyr::summarize(Count = sum(
    select.f(input$year_count, YEAR, df$YEAR) & select.f(input$sex_count, SEX, df$SEX) & (AGE >= min(input$age_count) & AGE <= max(input$age_count)) & 
      select.f(input$hispanic_count, HISPANIC, df$HISPANIC) & select.f(input$race_count, RACE, df$RACE) & text.f(input$primaryname_count, CauseOfDea, df$CauseOfDea) &
      text.f(input$primaryicd10_count, ACME, df$ACME) & select.f(input$primarymain_count, causeofdeath_shiny, df$causeofdeath_shiny) & 
      (text.f(input$anyicd10_count, MENT1, df$MENT1) | text.f(input$anyicd10_count, MENT2, df$MENT2) | text.f(input$anyicd10_count, MENT3, df$MENT3) | 
         text.f(input$anyicd10_count, MENT4, df$MENT4) | text.f(input$anyicd10_count, MENT5, df$MENT5)) & 
      (select.f(input$anymain_count, ment1_shiny, df$ment1_shiny) | select.f(input$anymain_count, ment2_shiny, df$ment2_shiny) | 
         select.f(input$anymain_count, ment3_shiny, df$ment3_shiny) | select.f(input$anymain_count, ment4_shiny, df$ment4_shiny) | select.f(input$anymain_count, ment5_shiny, df$ment5_shiny)))) 
  })
  plotinput_count <- reactive({plot.f(num_count(), "Count", num_count()$Count, input$scale_count, input$min_count, input$max_count, input$title_count)})
  output$plot_count <- renderPlot({print(plotinput_count())})
  output$download_count <- download.f(input$title_count, plotinput_count())
  output$download_data_count <- download.data.f(input$title_count, num_count())
  
  
  # Plot Rates
  df_rate <- df[which(!df$GEOID10 %in% problem_blkgr),]
  num_rate <- reactive({df_rate %>% dplyr::group_by(GEOID10) %>% dplyr::summarize(count_rate = sum( 
    (if (input$hispanic_rate == "All") {HISPANIC %in% c("Hispanic","Non-Hispanic")} else {HISPANIC %in% c(input$hispanic_rate)}) & 
      select.f(input$year_rate, YEAR, df$YEAR) & select.f(input$race_rate, race_shiny, df$race_shiny) & text.f(input$primaryname_rate, CauseOfDea, df$CauseOfDea) &
      text.f(input$primaryicd10_rate, ACME, df$ACME) & select.f(input$primarymain_rate, causeofdeath_shiny, df$causeofdeath_shiny) & 
      (text.f(input$anyicd10_rate, MENT1, df$MENT1) | text.f(input$anyicd10_rate, MENT2, df$MENT2) | text.f(input$anyicd10_rate, MENT3, df$MENT3) | 
         text.f(input$anyicd10_rate, MENT4, df$MENT4) | text.f(input$anyicd10_rate, MENT5, df$MENT5)) & 
      (select.f(input$anymain_rate, ment1_shiny, df$ment1_shiny) | select.f(input$anymain_rate, ment2_shiny, df$ment2_shiny) | 
         select.f(input$anymain_rate, ment3_shiny, df$ment3_shiny) | select.f(input$anymain_rate, ment4_shiny, df$ment4_shiny) | 
         select.f(input$anymain_rate, ment5_shiny, df$ment5_shiny))) * (if (is.null(input$year_rate)) {1000/11} else {1000/length(c(input$year_rate))})) 
  }) 
  population_georate <- population_county[,c(1,18)] %>% dplyr::rename(GEOID10 = GeoIDBlkGr)
  population_input <- reactive({cbind(
    population_georate, all = apply.f(pop_shiny_total, input$race_rate), 
    hispanic = apply.f(pop_shiny_hispanic, input$race_rate), hispanic_total = (pop_shiny_hispanic$Total), 
    nonhispanic = apply.f(pop_shiny_nonhispanic, input$race_rate), nonhispanic_total = pop_shiny_nonhispanic$Total
  )})
  total_rate <- reactive({merge(num_rate(), population_input()) %>% dplyr::group_by(GEOID10) %>% 
      dplyr::mutate(Total_Final = mutate.f(Total), all_final = mutate.f(all), hispanic_total_final = mutate.f(hispanic_total),
                    hispanic_final = mutate.f(hispanic), nonhispanic_total_final = mutate.f(nonhispanic_total), nonhispanic_final = mutate.f(nonhispanic)) %>%
      dplyr::summarize(Rate = sum(count_rate/
                                    (if (input$hispanic_rate == "All") summarize.f(input$race_rate, Total_Final, all_final)
                                     else if (input$hispanic_rate == "Hispanic") summarize.f(input$race_rate, hispanic_total_final, hispanic_final)
                                     else if (input$hispanic_rate == "Non-Hispanic") summarize.f(input$race_rate, nonhispanic_total_final, nonhispanic_final)
                                    )
      ))
  })
  plotinput_rate <- reactive({plot.f(total_rate(), "Rate", total_rate()$Rate, input$scale_rate, input$min_rate, input$max_rate, input$title_rate)})
  output$plot_rate <- renderPlot({print(plotinput_rate())})
  output$download_rate <- download.f(input$title_rate, plotinput_rate())
  output$download_data_rate <- download.data.f(input$title_rate, total_rate())
  
  
  # Plot Age-Adjusted
  age.f <- function(y) sub('^0+(?=[0-9])', '', y, perl=TRUE)
  setname.f <- function(x) setnames(x, old = colnames(x), new = c("GEOID10","count","population"))
  ageadj_pop.f <- function(pop, y, z) {
    ageadj_rate = setname.f(cbind(df %>% dplyr::group_by(GEOID10) %>% dplyr::summarize(
      count = sum(na.rm = T, (if (input$sex_ageadj  == "All") {sex_shiny %in% unique(df$sex_shiny)} else {sex_shiny %in% c(input$sex_ageadj)}) &
                    select.f(input$year_ageadj, YEAR, df$YEAR) & text.f(input$primaryname_ageadj, CauseOfDea, df$CauseOfDea) & 
                    text.f(input$primaryicd10_ageadj, ACME, df$ACME) & select.f(input$primarymain_ageadj, causeofdeath_shiny, df$causeofdeath_shiny) & 
                    (text.f(input$anyicd10_ageadj, MENT1, df$MENT1) | text.f(input$anyicd10_ageadj, MENT2, df$MENT2) | text.f(input$anyicd10_ageadj, MENT3, df$MENT3) | 
                       text.f(input$anyicd10_ageadj, MENT4, df$MENT4) | text.f(input$anyicd10_ageadj, MENT5, df$MENT5)) & 
                    (select.f(input$anymain_ageadj, ment1_shiny, df$ment1_shiny) | select.f(input$anymain_ageadj, ment2_shiny, df$ment2_shiny) | 
                       select.f(input$anymain_ageadj, ment3_shiny, df$ment3_shiny) | select.f(input$anymain_ageadj, ment4_shiny, df$ment4_shiny) | 
                       select.f(input$anymain_ageadj, ment5_shiny, df$ment5_shiny)) & 
                    AGE >= as.numeric(age.f(substr(colnames(pop[y]), 1, 3))) & AGE <= as.numeric(age.f(substr(colnames(pop[y]), 5, 7)))) 
      * (if (is.null(input$year_ageadj)) {1000/11} else {1000/length(c(input$year_ageadj))}) * age_distr[1, y-2]), 
      pop[,paste(substr(colnames(pop[y]), 1, 3), "-",substr(colnames(pop[y]), 5, 7), sep = "")])) %>% dplyr::group_by(GEOID10) %>% dplyr::summarize(rate = sum(count/population))
    setnames(ageadj_rate, old = "rate", new = paste(z, sep = ""))
  }
  ageadj.f <- function(y, z) (if (input$sex_ageadj == "All") {ageadj_pop.f(pop_ageadj_total, y, z)}
                              else if (input$sex_ageadj == "Female") {ageadj_pop.f(pop_ageadj_females, y, z)}
                              else if (input$sex_ageadj == "Male") {ageadj_pop.f(pop_ageadj_males, y, z)})
  ageadj <- reactive({plyr::join_all(list(ageadj.f(3,"a"), ageadj.f(4,"b"), ageadj.f(5,"c"), ageadj.f(6,"d"), ageadj.f(7,"e"), ageadj.f(8,"f"), ageadj.f(9,"g"), ageadj.f(10,"h"), ageadj.f(11,"i"), ageadj.f(12,"j"), ageadj.f(13,"k")), by = "GEOID10")})
  reactive({ageadj()[is.na(ageadj())] <- 0})
  total_ageadj <- reactive({ageadj() %>% dplyr::group_by(GEOID10) %>% summarize(Rate = sum(sum(a, b, c, d, e, f, g, h, i, j, k)))})
  
  plotinput_ageadj <- reactive({plot.f(total_ageadj(), "Rate", total_ageadj()$Rate, input$scale_ageadj, input$min_ageadj, input$max_ageadj, input$title_ageadj)})
  output$plot_ageadj <- renderPlot({print(plotinput_ageadj())})
  output$download_ageadj <- download.f(input$title_ageadj, plotinput_ageadj())
  output$download_data_ageadj <- download.data.f(input$title_ageadj, total_ageadj())
  
  
  # Reset Functions
  updateprimary.f <- function(input, primaryname, primaryicd10, primarymain) {
    if (input %in% c("None","Any Cause of Death")) {
      updateTextAreaInput(session = session, inputId = primaryname, value = "")
      updateTextAreaInput(session = session, inputId = primaryicd10, value = "")
      updateCheckboxGroupInput(session = session, inputId = primarymain, choices = c("Cancer","Diabetes","Heart Disease","HIV"))
    }}
  updateany.f <- function(input, anyicd10, anymain) {
    if (input %in% c("None","Primary Cause of Death")) {
      updateTextAreaInput(session = session, inputId = anyicd10, value = "")
      updateCheckboxGroupInput(session = session, inputId = anymain, choices = c("Cancer","Diabetes","Heart Disease","HIV"))
    }}
  updatestandard.f <- function(title, scale, min_input, max_input, max_value, set_value, year, primaryname, primaryicd10, primarymain, anyicd10, anymain) {
    updateTextInput(session = session, inputId = title, value = "Mortality Count Plot")
    updateCheckboxInput(session = session, inputId = scale, value = F)
    updateNumericInput(session = session, inputId = min_input, min = 0, max_value, value = 0)
    updateNumericInput(session = session, inputId = max_input, min = 0, max_value, value = set_value)
    updateCheckboxGroupInput(session = session, inputId = year, choices = c(unique(df$YEAR)))
    updateTextAreaInput(session = session, inputId = primaryname, value = "")
    updateTextAreaInput(session = session, inputId = primaryicd10, value = "")
    updateCheckboxGroupInput(session = session, inputId = primarymain, choices = c("Cancer","Diabetes","Heart Disease","HIV"))
    updateTextAreaInput(session = session, inputId = anyicd10, value = "")
    updateCheckboxGroupInput(session = session, inputId = anymain, choices = c("Cancer","Diabetes","Heart Disease","HIV"))}
  
  # Reset Counts
  observe({
    updateprimary.f(input$causeoptions_count, "primaryname_count", "primaryicd10_count", "primarymain_count")
    updateany.f(input$causeoptions_count, "anyicd10_count", "anymain_count")
    if (input$reset_all_count > 0) {
      updatestandard.f("title_count", "scale_count", "min_count", "max_count", set_max_blkgrp, set_max_blkgrp, "year_count", 
                       "primaryname_count", "primaryicd10_count", "primarymain_count", "anyicd10_count", "anymain_count")
      updateCheckboxGroupInput(session = session, inputId = "sex_count", choices = c(unique(df$SEX)))
      updateSliderInput(session = session, inputId = "age_count", value = c(0, max_age))
      updateCheckboxGroupInput(session = session, inputId = "hispanic_count", choices = c(unique(df$HISPANIC)))
      updateCheckboxGroupInput(session = session, inputId = "race_count", choices = c(unique(df$RACE)))
    }
  })
  
  # Reset Rates
  observe({
    updateprimary.f(input$causeoptions_rate, "primaryname_rate", "primaryicd10_rate", "primarymain_rate")
    updateany.f(input$causeoptions_rate, "anyicd10_rate", "anymain_rate")
    if (input$reset_all_rate > 0) {
      updatestandard.f("title_rate", "scale_rate", "min_rate", "max_rate", 1000, 50, "year_rate", 
                       "primaryname_rate", "primaryicd10_rate", "primarymain_rate", "anyicd10_rate", "anymain_rate")
      updateSelectInput(session = session, inputId = "hispanic_rate", choices = c("All","Hispanic","Non-Hispanic"))
      updateCheckboxGroupInput(session = session, inputId = "race_rate", choices = sort(unique(df$race_shiny)))
    }    
  })
  
  # Reset Age-Adjusted
  observe({
    updateprimary.f(input$causeoptions_ageadj, "primaryname_ageadj", "primaryicd10_ageadj", "primarymain_ageadj")
    updateany.f(input$causeoptions_ageadj, "anyicd10_ageadj", "anymain_ageadj")
    if (input$reset_all_ageadj > 0) {
      updatestandard.f("title_ageadj", "scale_ageadj", "min_ageadj", "max_ageadj", 1000, 50, "year_ageadj",
                       "primaryname_ageadj", "primaryicd10_ageadj", "primarymain_ageadj", "anyicd10_ageadj", "anymain_ageadj")
      updateSelectInput(session = session, inputId = "sex_ageadj", choices = c("All","Male","Female"))
    }    
  })
}
shinyApp(ui = ui, server = server)
