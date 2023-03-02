#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(DBI)
library(bigrquery)
library(ggplot2)
library(datasets)



demo_var1 = c("ethnicity", "language", "insurance", "marital_status", "gender")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ICU cohort data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Choose a variable for graphical summary",
                  choices = c("ethnicity",
                              "language",
                              "insurance",
                              "marital_status",
                              "gender",
                              "Admission year", 
                              "Admission month",
                              "Admission month day", 
                              "Admission week day",
                              "Admission hour",
                              "Admission minute",
                              "Admission duration",
                              "Laboratory measurements",
                              "Vital measurements in charted data"
                  ),
                  selected = "Please Select"),
      sliderInput("bins",
                  "Number of bins (only for admission years):",
                  min = 20,
                  max = 102,
                  value = 102),
      selectInput("itemid",
                  label = "Which measurements do you want to 
                      check (only for Laboratory measurements and Vital 
                      measurements in charted data)?",
                  choices = c(
                    "Please select", 
                    "Creatinine (50912)", "Potassium (50971)",
                    "Sodium (50983)", "Chloride (50902)",
                    "Bicarbonate (50882)", "Hematocrit (51221)",
                    "WBC (51301)", 
                    "Glucose (50931)",
                    "Boxplot for all laboratory measurements",
                    "Heart rate (220045)", 
                    "Mean non-invasive blood pressure (220181)",
                    "Systolic non-invasive blood pressure (220179)", 
                    "Body temperature in Fahrenheit (223761)",
                    "Respiratory rate (220210)", 
                    "Boxplot for all vital measurements"),
                  selected = "Please select")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Numerical and graphical summary of the variable"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  icu = read_rds(
    "icu_cohort.rds")
# I am not able to get to display this table for numerical representation on the app
  output$table <- renderTable({
    if (input$var %in% demo_var1){
        icu %>%
        group_by_("thirty_day_mort", input$var) %>%
        summarize(n = n()) %>%
        mutate(prop = n / sum(n)) %>%
        select(-n) %>%
        spread("thirty_day_mort", prop)
      } else FALSE
  })
  
    
  output$distPlot <- renderPlot({
    if(input$var == "Admission year") {
      icu %>%
        ggplot() +
        geom_histogram(mapping = aes(x = year(admittime)), 
                       bins = input$bins) +
        labs(x = "First admission year")
      
  
    }else if(input$var == "Admission month") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = lubridate::month(admittime, 
                                                    label = TRUE))) +
        labs(x = "First admission month")
    }else if(input$var == "Admission month day") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = mday(admittime))) +
        labs(x = "First admission month day")
    }else if(input$var == "Admission week day") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = lubridate::wday(admittime, label = TRUE, 
                                                   abbr = TRUE))) +
        labs(x = "First admission week day")
    }else if(input$var == "Admission hour") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = hour(admittime))) +
        labs(x = "First admission hour")
    }else if(input$var == "Admission minute") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = minute(admittime))) +
        labs(x = "First admission minute")
    }else if(input$var == "Admission duration") {
      icu %>%
        mutate(dur = as.duration(dischtime - admittime)) %>%
        filter((dur >= 0) & (dur / 86400 <= 15)) %>%
        ggplot() +
        geom_histogram(mapping = aes(x = dur / 86400)) +
        labs(x = "Hospital stay duration (days)")
    }else if(input$var == "gender") {
      icu %>%
        ggplot() + 
        geom_bar(mapping = aes(x = anchor_age, fill = gender))
    }else if(input$var == "ethnicity") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = ethnicity, fill = thirty_day_mort)) +
        labs(x = "Ethnicity")
    }else if(input$var == "language") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = language, fill = thirty_day_mort)) +
        labs(x = "Language")
    }else if(input$var == "insurance") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = insurance, fill = thirty_day_mort)) +
        labs(x = "Insurance")
    }else if(input$var == "marital_status") {
      icu %>%
        ggplot() +
        geom_bar(mapping = aes(x = marital_status, fill = thirty_day_mort)) +
        labs(x = "Marital Status")
    }else if(input$var == "Laboratory measurements") {
      icu = icu %>%
        mutate(icu, '51301' = valuenum51301) %>%
        mutate(icu, '50882' = valuenum50882) %>%
        mutate(icu, '51221' = valuenum51221) %>%
        mutate(icu, '50912' = valuenum50912) %>%
        mutate(icu, '50971' = valuenum50971) %>%
        mutate(icu, '50983' = valuenum50983) %>%
        mutate(icu, '50902' = valuenum50902) %>%
        mutate(icu, '50931' = ifelse(valuenum50931 < 600, valuenum50931, 
                                     NA)) %>%
        pivot_longer(c('51301', '50882', '51221', '50912', '50971', 
                       '50983', '50902',  '50931'), 
                     names_to = "labitemid", values_to = "labvaluenum")
      if(input$itemid == "Boxplot for all laboratory measurements"){
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_boxplot(mapping = aes(x = as.character(labitemid), 
                                     y = labvaluenum))
      }else if(input$itemid == "Creatinine (50912)"){
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50912))
      }else if(input$itemid == "Potassium (50971)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50971))
      }else if(input$itemid == "Sodium (50983)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50983))
      }else if(input$itemid == "Chloride (50902)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50902))
      }else if(input$itemid == "Bicarbonate (50882)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50882))
      }else if(input$itemid == "Hematocrit (51221)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum51221))
      }else if(input$itemid == "WBC (51301)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum51301))
      }else if(input$itemid == "Glucose (50931)") {
        icu %>%
          group_by(labitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum50931))
      } else FALSE
    }else if(input$var == "Vital measurements in charted data") {
      icu = icu %>%
        mutate(icu, '220181' = 
                 ifelse(valuenum220181 < 1000, valuenum220181, NA)) %>%
        mutate(icu, '220179' = 
                 ifelse(valuenum220179 < 1000, valuenum220179, NA)) %>%
        mutate(icu, '223761' = 
                 ifelse(valuenum223761 < 1000, valuenum223761, NA)) %>%
        mutate(icu, '220210' = 
                 ifelse(valuenum220210 < 1000, valuenum220210, NA)) %>%
        mutate(icu, '220045' = 
                 ifelse(valuenum220045 < 1000, valuenum220045, NA)) %>%
        pivot_longer(c('220181', '220179', '223761', '220210', '220045'), 
                     names_to = "chartitemid", values_to = "chartvaluenum")
      if(input$itemid == "Boxplot for all vital measurements"){
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_boxplot(mapping = aes(x = as.character(chartitemid),
                                     y = chartvaluenum))
      }else if(input$itemid == "Heart rate (220045)"){
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum220045))
      }else if(input$itemid == "Mean non-invasive blood pressure (220181)") {
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum220181))
      }else if(input$itemid == 
               "Systolic non-invasive blood pressure (220179)") {
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum220179))
      }else if(input$itemid == "Body temperature in Fahrenheit (223761)") {
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum223761))
      }else if(input$itemid == "Respiratory rate (220210)") {
        icu %>%
          group_by(chartitemid) %>%
          ggplot() + 
          geom_histogram(mapping = aes(x = valuenum220210))
      }else FALSE
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)