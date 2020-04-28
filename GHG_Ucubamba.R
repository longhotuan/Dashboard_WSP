#### Global R ####

library(knitr)
library(rmarkdown)
library(tidyverse)
library(reshape)
library(plotly)
library(flexdashboard)
library(shiny)
library(data.table)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(tweenr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(rsconnect)
library(DT)
library(rworldmap)
library(feather)

ghg_cuenca <- read_csv("WSP.csv")
ghg_ucubamba <- read_csv("WSP24h.csv")
first_column <- which(colnames(ghg_cuenca) == "Water temperature (oC)")
last_column <- which(colnames(ghg_cuenca) == "CO2 Dissolved Gas (ug/L)")
first_column_2 <- which(colnames(ghg_ucubamba) == "Water temperature (oC)")
last_column_2 <- which(colnames(ghg_ucubamba) == "CO2 Dissolved Gas (ug/L)")
#### ui ####

ui <- dashboardPage(skin = "green",
                    # Dashboard header ####
                    dashboardHeader(title="Greenhouse gas emissions from Ucubamba WSP (Ecuador)"),
                    # Dashboard sidebar #### 
                    dashboardSidebar(
                        sidebarMenu(id="tabs",
                                    menuItem("About", 
                                             tabName = "about",
                                             icon = icon("info")),
                                    menuItem("Spatial variability", 
                                             tabName = "info",
                                             icon = icon("microscope")),
                                    menuItem("Temporal variability", 
                                             tabName = "time",
                                             icon = icon("clock")),
                                    conditionalPanel(condition = "input.tabs == 'info'",
                                                     selectInput(inputId = "water", label = "Select a variable", 
                                                                 choices = c(All = colnames(ghg_cuenca)[first_column:last_column]))),
                                    conditionalPanel(condition = "input.tabs == 'time'",
                                                     selectInput(inputId = "pond", label = "Select a pond", 
                                                                 choices = c(levels(as.factor(ghg_ucubamba$Pond))[1], levels(as.factor(ghg_ucubamba$Pond))[2])),
                                                     selectInput(inputId = "variable", label = "Select a variable", 
                                                                 choices = colnames(ghg_cuenca)[first_column_2:last_column_2]))                                    )
                        
                    ),
                    # Dashboard body #### 
                    dashboardBody(
                        tabItems(
                            # About tab content ####
                            tabItem(tabName = "about",
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Abstract"),
                                            
                                            br(),
                                            h4("..."),
                                          
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Acknowledgment"),
                                            
                                            br(),
                                            h4("This research was performed in the context of the VLIR Ecuador Biodiversity Network project. This project was funded by the Vlaamse Interuniversitaire Raad-Universitaire Ontwikkelingssamenwerking (VLIR-UOS), which supports partnerships between universities and university colleges in Flanders and the South. We thank Carlos Santiago Deluquez, Caio Neves, Paula Avila, Juan Enrique Orellana, and Kate Pesantez for their contributions during the sampling campaign. We are grateful to the Water and Soil Quality Analysis Laboratory of the University of Cuenca for their supports in our analyses."),
                                            
                                        )
                                    ),
                                    fluidRow(
                                        box(width = 12, 
                                            h2("Information"),
                                            
                                            br(),
                                            h4("If you find this tool useful, please cite the reference of our paper (submitted) and help spread the word. If you have questions related to the dataset, please  send us an email to ",
                                               a("Long.TuanHo@UGent.com",
                                                 href = "mailto: Long.TuanHo@UGent.com"))
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               h1("Funded by"),
                                               img(style = "max-width:30%",
                                                   src = "Logo2.jpg")
                                        ),
                                        column(6, 
                                               img(align = "left|bottom",
                                                   style = "max-width:20%",
                                                   src = "Logo.png") 
                                        )
                                    )
                            ), # end of About tabItem
                            # Info tab content ####
                            tabItem(tabName = "info",
                                    fluidRow(
                                        box(title = "Value of the variable at each sampling site", width = 12, height = 500, 
                                            leafletOutput("map", width = "100%", height = 500) # Can be changed
                                        )),
                                    fluidRow(
                                        box(title = "Summary", width = 12, heigh = 200,
                                            verbatimTextOutput("table"))
                                        
                                    )
                            ), # end of Info tab 
                            # Time tab content ####
                            tabItem(tabName = "time",
                                    fluidRow(
                                        box(title = "Temporal variability of the variable", width = 12, height = 500, 
                                            plotlyOutput("line")
                                        )),
                                    fluidRow(
                                        box(title = "Summary", width = 12, heigh = 200,
                                            verbatimTextOutput("table_2"))
                                        
                                    )
                            ) # end of time tab 
                        ) # end tabItems
                    ) # end dashboardbody
) # end dashboardpage

#### server ####

server <- function(input, output,session) {
    # Setting reactivities ####
    df <- reactive({ghg_cuenca})
    variablename <- reactive({
        colnames(df())[first_column:last_column]
    })
    observe({
        updateSelectInput(session, inputId = "water", label = "Select a variable", choices = c(variablename()))
    })
    df_variable <- reactive({
        input$water
    })
    
    df2 <- reactive({ghg_ucubamba})
    
    pondname <- reactive({
        c(levels(as.factor(ghg_ucubamba$Pond))[1], levels(as.factor(ghg_ucubamba$Pond))[2])
    })
    observe({
        updateSelectInput(session, inputId = "pond", label = "Select a pond", choices = c(pondname()))
    })
    df_pond <- reactive({
        input$pond
    })
    
    variablename2 <- reactive({
        if(pondname() == "Facultative Pond"){
            df2_2 <- df2() %>% dplyr::filter(Pond == "Facultative Pond")
            colnames(df2_2)[first_column_2:last_column_2]
        } else if (pondname() == "Maturation Pond"){
            df2_2 <- df2() %>% dplyr:filter(Pond == "Maturation Pond")
            colnames(df2_2)[first_column_2:last_column_2]
        }
    })
    
    observe({
        updateSelectInput(session, inputId = "variable", label = "Select a variable", choices = c(variablename2()))
    })
    df_variable2 <- reactive({
        input$variable
    })

    # Output map in Info tab ####
    output$map <- renderLeaflet({
        selecteddf2 <- df()
        colors <- brewer.pal(n = 7, name = "Dark2")
        tilesURL <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
        leaflet() %>%
            addTiles(tilesURL) %>%
            fitBounds(lng1 = -78.950307, lat1 =  -2.865964, lng2 = -78.934000, lat2 = -2.879165) %>%
            addMinicharts(selecteddf2$Longitude, selecteddf2$Latitude,
                          type = "pie",
                          chartdata = selecteddf2[,which(colnames(selecteddf2) == df_variable())],
                          colorPalette = colors,
                          opacity = 0.6,
                          showLabels = TRUE,
                          transitionTime = 0)
    })
    
    # Output table in Info tab ####
    output$table <- renderPrint(summary(df()[[input$water]]))
     
    # Output line in Time tab #### Dung o day 18:00
    output$line <- renderPlotly({
        if(df_pond() == "Facultative Pond"){
            selecteddata <- df2() %>% dplyr::filter(Pond == "Facultative Pond") %>% dplyr::select(Time, input$variable)
            
        } else {
            selecteddata <- df2() %>%  dplyr::filter(Pond == "Maturation Pond") %>% dplyr::select(Time, input$variable)
        }
        
        ggplotly(ggplot(data = selecteddata, aes(x =  as.POSIXct(Time), y = selecteddata[[input$variable]])) +
                     geom_line() + 
                     geom_point() +
                     theme_bw()+
                     ylab(paste(input$variable)) +
                     scale_x_datetime(date_labels = "%H:%M") +
                     scale_color_brewer(palette = "Dark2") +
                     theme(text=element_text(size=18),
                           strip.text.x = element_text(size=18),
                           axis.text.x = element_text(size=16),
                           axis.title.x = element_blank(),
                           legend.position = "none"))
    })
    # Output table2 in Time tab ####
    output$table_2 <- renderPrint({
        if(df_pond() == "Facultative Pond"){
            selecteddata <- df2() %>% dplyr::filter(Pond == "Facultative Pond")
        } else {
            selecteddata <- df2() %>%  dplyr::filter(Pond == "Maturation Pond")
        }
        summary(selecteddata[[input$variable]])
        
    })
    
}

#### Run the application ####
shinyApp(ui = ui, server = server)