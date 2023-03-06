
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinyWidgets)
library(shiny)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(readr)

nutrients_csvfile <- read.csv("nutrients_csvfile.csv")
ui <- fluidPage(
  titlePanel("Nutritional Facts for most common foods"),
  
  tabsetPanel(type = "tabs",
              tabPanel("About", verbatimTextOutput("About"),
                       helpText(h4("This app uses a dataset contains a csv file with", strong("more than 300 foods"), "each with the
                                amount of", strong("Calories, Fats, Proteins, Saturated fats, Carbohydrates, Fibers"), "labelled for each food.")),
                       helpText(h4("The dataset contains", em("329"), "unique food values and", em("10 variables."))),
                       helpText(strong("Below is a small amount of data : ", style = "color:blue")),
                       tableOutput("sample_data")
              ),
              
              tabPanel("Plots",
                       helpText(h4("You are able to see the each Select the aspect you want to 
                             explore more, total cases or active cases, etc.")),
                       br(),
                       helpText(h4("You will see barplots of each category's food with its (food name) and its (calories).")),
                       helpText(h4(em("You can choose the calories range you want to see. This will help you decide how big calories food
                       you want to know", style = "color:green"))),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("category", 
                                              label = "which category to plot: ", 
                                              choices = unique(nutrients_csvfile$Category), 
                                              selected = unique(nutrients_csvfile$Category)), 
                           
                           sliderInput("calories_range", 
                                       "What range of calories to plot: ", 
                                       min = 0, 
                                       value = c(0, 600),
                                       max = 1000 
                                       ),
                           radioButtons(
                             inputId = "palette",
                             label = "Select a palette:",
                             choices = c("Standard", "Set 2"),
                             selected = "Standard"
                           )
                         ),
                         
                         mainPanel(
                           plotOutput("calories_plot")
                         )
                       )
              ),
              
              tabPanel("Tables",
                       helpText(h4("The tables will show the", strong("top 10"), "food for each nutrient.
                                   You can see the nutrient's value for each food"),
                      helpText(h4("The nutrients includes :", strong("protein, Fat, Sat.Fat, Fiber, and Carbs"))),
                      br()
                       ),
                      sidebarPanel(
                        selectInput("nutrient", 
                                    label = "Choose a nutrient you want to see: ",
                                    choices = c("Protein", "Fat", "Sat.Fat", "Fiber", "Carbs"))
                      ),
                      mainPanel(
                        tableOutput("nutrients_table")
                      )
                ),
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$calories_plot <- renderPlot({
    filtered_data <- nutrients_csvfile %>%
      filter(Category == input$category,
             Calories >= input$calories_range[1],
             Calories <= input$calories_range[2])
    if(input$palette == "Standard") {
      my_color <- c("blue")
    } else {
      my_color <- c("red")
    }
    
    
    ggplot(filtered_data, aes(x = Food, y = as.numeric(Calories), fill = Category)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = my_color) +
      labs(title = "Calories for Different Categories",
           x = "Food", y = "Calories") +
      theme(axis.text.x = element_text(size = 9)) +
      coord_flip()
  })
  
  output$nutrients_table <- renderTable({
    nutrients_csvfile %>%
      select(Food, !!sym(input$nutrient)) %>%
      filter(!!sym(input$nutrient) != "t") %>%
      group_by(Food) %>%
      summarize(nutrient_value = mean(as.numeric(!!sym(input$nutrient)), na.rm = TRUE)) %>%
      arrange(desc(nutrient_value)) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 10) %>%
      select(rank, Food, nutrient_value)
  })
  
  
  
  
  output$sample_data <- renderTable({
    nutrients_csvfile %>% 
      select(Food, Measure,Grams,Calories,Protein,Fat, Sat.Fat,Fiber, Carbs, Category) %>% 
      head(10)
  })
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
