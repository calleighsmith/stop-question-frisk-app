library(shiny)
library(tidyverse)
library(dplyr)
library(sf)
library(stringr)
library(editData)

source("data_wrangling_ec.R")
col.range=c(-.8,.8)

ui <- fluidPage(
    
    tags$head(
        
        tags$link(href = "https://fonts.googleapis.com/css?family=Inconsolata", 
                  rel = "stylesheet"),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ), 
        tags$style(HTML('
        
        *{
          font-family: Inconsolata;
          font-size: 100%;
        }
        #sidebar {
           background-color: #fff;
           border: 0px;
        }
        .rt-th {
          display: none;
        }
        .rt-noData {
          display: none;
        }
        .rt-pagination-nav {
          float: left;
          width: 100%;
        }

      
    '))
    ),
    
    sidebarLayout(
        
        sidebarPanel(
            id = "sidebar",
            tags$h1("NYPD Stop-Question-Frisk Dashboard"),
            tags$br(),
            HTML(paste0(
                "<p> Stop-Question-Frisk (SQF) is a controversial practice implemented by the New York City Police Department since 2002. This method aims to detain, interrogate, and potentially search suspects in an attempt to curb crime. However, this tactic has long been criticized as disproportionately targeting Black and Hispanic youths. In 2011, SQF incidents were at an all time high with 685,724 documented stops, of which 84% of the suspects were Black or Hispanic. This prompted a class-action lawsuit <i>Floyd et. al vs. The City of New York</i> in 2013, where the federal judge ruled that SQF practices were unconstitutional and unfairly targeted Black and Hispanic citizens. Since the ruling, the frequency of SQF incidents has steeply declined. In 2019, there were only 13,459 documented cases of SQF incidents, a decrease of ~98% since the tactic's peak in 2011.",
                br(),
                br(),
                "The implications of SQF events can be severe for innocent individuals who are stopped as a result of racial profiling by the police, especially in light of current events surrounding police brutality and the Black Lives Matter movement. There have been numerous instances of innocent civilians, particularly black men, who have been wrongfully killed as a result of interactions with the police. In May 2020, the murder of George Floyd in Minneapolis brought to light widespread police brutality. A video of Minneapolis police officer Derek Chauvin kneeling on Floyd’s neck for 8 minutes and 46 seconds as Floyd called out \"I can’t breathe\" went viral, igniting protests globally in response to the systemic racism that permeates society 60 years after the civil rights movement in the US.",
                br(),
                br(),
                "In an unbiased world, the population percent by race and the SQF rate by race would be equivalent. Therefore, we calculated the difference in these two proportions by precinct (population rate - SQF rate). A negative difference would indicate that a racial group was stopped more frequently in a given precinct than one would expect based on the population of that racial group in the precinct, and vice versa.",
                br(),
                br(),
                "To the right, select the year and race for which you are interested in exploring discrepancies in SQF events. </p>",
                "<p>(Made by <a href='https://github.com/calleighsmith'>",
                "@calleighsmith</a>. Source code ",
                "<a href='https://github.com/calleighsmith/stop-question-frisk-app'>on GitHub</a>.)</p>"
            )),
            tags$br(),
            

            width = 5
            
        ),
        mainPanel(
            
            tags$br(),
          
            
            selectInput3("year", label = "Year", 
                                 choices = c(2017, 2018, 2019), 
                                 selected = 2019),
            selectInput3("race", label = "Race", 
                                 choices = c("Black" = "BLACK", 
                                             "Hispanic" = "WHITE HISPANIC", 
                                             "White" = "WHITE"), 
                                 selected = "Black"),
            
            plotOutput("plot", width = "700px", height = "700px"),
            
            width = 7
            
        )
    )
)

server <- function(input, output) {
    
    
    
  output$plot <- renderPlot({
    
    
    res <- master_1_group %>%
      filter(SUSPECT_RACE_DESCRIPTION == input$race,
             YEAR2 == input$year)
    print(head(res))

    ggplot(res) + 
      geom_sf(aes(fill = diff_pc, geometry = geometry)) +
      scale_fill_gradientn(colors = c("lightpink", "lightgreen"), limits=col.range) +
      theme_bw() +
      labs(title = paste0("Discrepancies in SQF Rates for the ", str_to_title(input$race), " Population in ", input$year),
        fill = "Difference in Proportions\n(Precinct Population Rate\n- Precinct SQF Rate)")+
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom")
  
    
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)