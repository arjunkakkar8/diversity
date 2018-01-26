library(shiny)
library(leaflet)
library(DT)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(tidyr)

navbarPage(
  "Diversity at Williams",
  id = "nav",
  
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),
      
      #Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto", style = 'opacity: 0.8',

                    h3("Select Level"),

                    selectInput("class", "Classification by:", 
                                c("Neighborhood" = "nbhd",
                                  "Building" = "build",
                                  "Suite" = "suite"),
                                selected = "build"),
                    plotlyOutput("pie", height = 350),
                    plotlyOutput("chi", height = 200)
      ),
      
      tags$div(
        id = "cite",
        tags$em(
          'Data compiled for a study on proportional representation
          of ethnic groups accross residential halls at Williams College.'
        ),
        'Gabriella Carmona, Arjun Kakkar and Karan Tiberwal'
      )
    )
  ),
  
  tabPanel("Data and Analysis",
           h3("Data"),
           p("The data for this project was obtained from various sources that are all
             accessible to Williams students. The building rosters for on-campus
             housing are available", 
             a("here", href = "https://williams.starrezhousing.com/StarRezPortal/Default.aspx?Params=L9ezxPcQnQuRGKTzF%2b4sxeNblvAA%2b26c"),
             "using a Williams College login. This data was then merged with
             class year data using a list of all students obtained from",
             a("WSO.", href = "https://wso.williams.edu/facebook"), "The ethnic/racial
             classifications were done using the 2010 US census data on last names. For
             those ethnic groups for which the last name did not provide a strong signal
             for classification, the assignment was done by hand.
             The combined dataset consists of 1653 students. 197 students were removed
             from the original dataset since no match for their lastname was found.
             Assuming that the number of last names without matches are proportional
             in terms of ethnicity, the analysis conducted here would not have an
             unreasonable amount of error."),
           h3("Analysis"),
           p("The compiled data was analysed by looking at proportional representation
             accross class years and campus locations. As an example the following plot
             visualizes the representation of ethnic groups for all years accross different
             student housing on campus."),
           plotOutput("allbar"),
           p("Statistical significance for a difference between each of these proportions and the
             overall proportions of the college community was calculated using 4-sample test for 
             equality of proportions. The p-values obtained using this test were used to color the
             nodes in the map. 'Super unlikely' corresponds to a p-value between 0-0.05, 'very unlikely'
             corresponds to a p-value between 0.05-0.1, 'unlikely' corresponds to a p-value between
             0.1-0.3 and the rest is designated as 'likely'."),
           h3("Anonymized Raw Data"),
           p("The raw data used to construct the interactive graphics and
             conduct the statistical analysis is presented as follows. It
             can be downloaded by simply copying the table. The names of 
             students have been removed in order to protect sensitive 
             information. For any additional information, questions or 
             concerns about the dataset, please contact the authors."),
           dataTableOutput("raw_data")
           ),
  
  # tabPanel("Data explorer",
  #          fluidRow(
  #            column(3,
  #                   selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
  #            ),
  #            column(3,
  #                   conditionalPanel("input.states",
  #                                    selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
  #                   )
  #            ),
  #            column(3,
  #                   conditionalPanel("input.states",
  #                                    selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
  #                   )
  #            )
  #          ),
  #          fluidRow(
  #            column(1,
  #                   numericInput("minScore", "Min score", min=0, max=100, value=0)
  #            ),
  #            column(1,
  #                   numericInput("maxScore", "Max score", min=0, max=100, value=100)
  #            )
  #          ),
  #          hr(),
  #          DT::dataTableOutput("ziptable")
  # ),
  tabPanel("About",
           column(2, p("")),
           column(
             8,
             p(
               div(
                 em("“The Office of Institutional Diversity and Equity at
                 Williams College dedicates itself to a community where all
                 members can thrive. We work to eliminate harmful bias and discrimination,
                 close opportunity gaps, and advance critical conversations and initiatives
                 that promote inclusion, equity, and social justice on campus and beyond.”"),
                 style = "font-size:18px; text-align:justify; padding-right:60px; padding-left:60px"
               ),
               div("-Williams College Office of Diversity and Equity",
               style = "font-size:14px; text-align:right; padding-right:60px")
             ),
             br(),
             br(),
             p("This project began with one single yet elusive question - ",
               strong("What is diversity?"), "While college admissions brochures and 
               statistics constantly boast a College’s diversity like a badge of honor,
               there seems to be no clear understanding of what diversity should look like
               beyond the numbers. While diversity is generally thought of in terms of admissions,
               this project attempts to investigate diversity as it has physically manifested at
               Williams College.",
               style = "font-size:18px; text-align:justify; padding-right:40px; padding-left:40px"),
             br(),br(),
             p(h3("Contact"),
               div("Gabriella Carmona: ", code("gnc1@williams.edu")),
               div("Arjun Kakkar: ", code("ak23@williams.edu")),
               div("Karan Tibrewal", code("kt3@williams.edu")))
             )),
  
  conditionalPanel("false", icon("crosshair"))
  )