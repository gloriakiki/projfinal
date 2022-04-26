library(shiny) 
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(ggthemes)

#load data
raw_data_2016 <- readRDS("data/vote2016.rds")
raw_data_2020 <- readRDS("data/vote2020.rds")

raw_data_2016$CNN <- ifelse(raw_data_2016$cnn==1,"Read News From CNN",
                            "Not Use CNN")
raw_data_2020$CNN <- ifelse(raw_data_2020$cnn==1,"Read News From CNN",
                            "Not Use CNN")



#UI
ui <- fluidPage(sidebarLayout(
  sidebarPanel( 
    
    selectInput("x", "Select a covariate with CNN news", choices = 
                  c("age","income","gender","race")
                  )
  ),
  mainPanel(plotOutput( "plot") ) 
) 
) 

#server 

server <- function(input, output) { 
  x <- reactive({
    input$x
  })
  
  output$plot <- renderPlot({ 
    
    
    
    raw_data_2016 <- raw_data_2016[,c(x(), "Trump","CNN")]
    raw_data_2020 <- raw_data_2020[,c(x(), "Trump","CNN")]
    colnames(raw_data_2016)[1] = colnames(raw_data_2020)[1] = "x"
    t1 <- raw_data_2016 %>%
      group_by(x, CNN) %>%
      summarise(Trump = mean(Trump ==  1)) 
    
    p1 <- ggplot(t1, aes(x, Trump, fill = x)) +
      geom_col(position = "dodge") +
      scale_fill_viridis_d() + 
      facet_wrap(~CNN) +
      labs(x = x(), fill = x(),
           y = "Percentage (%)", title = "2016") +
      scale_y_continuous(label = percent) +
      theme_stata()
    t1 <- raw_data_2020 %>%
      group_by(x, CNN) %>%
      summarise(Trump = mean(Trump ==  1)) 
    
    p2 <- ggplot(t1, aes(x, Trump, fill = x)) +
      geom_col(position = "dodge") +
      scale_fill_viridis_d() + 
      facet_wrap(~CNN) +
      labs(x = x(), fill = x(),
           y = "Percentage (%)", title = "2020") +
      scale_y_continuous(label = percent) +
      theme_stata()
    grid.arrange(p1, p2, nrow = 1)
    
    
  }) 
}

shinyApp(ui=ui,server=server) 