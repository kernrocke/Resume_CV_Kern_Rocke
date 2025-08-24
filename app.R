# app.R

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(fmsb)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggradar)

# Hypothetical data based on website content
home_text <- "My name is Kern Rocke, born in the twin island republic of Trinidad and Tobago and I now currently reside in Barbados. I am a researcher in the Caribbean working at the George Alleyne Chronic Disease Research Centre (GA-CDRC) as a Research Associate. I am also an international consultant focusing on vaccine safety surveillance at the Pan American Health Organization. I studied Human Nutrition and Dietetics at a Bachelor's level and then moved on to a Masters of Epidemiology. Currently I am pursuing my terminal degree in Epidemiology with a focus on Spatial Epidemiology.

I worked as a researcher with Nutrition Research Group at the Department of Agricultural Economics and Extension in Trinidad from 2015-2017 working on a variety of projects centred around population health behaviours and nutritional status. During my time there, I also worked as a part-time lecturer for the BSc Human Nutrition and Dietetics programme teaching in areas of research methods, biostatistics, community nutrition, physiology and health and, nutrition and metabolism. From 2017-2019, I worked as a Statistical Consultant with the CARICOM Secretariat in Guyana analysing data related to employee engagement. From 2019-present I am working in Barbados at the GA-CDRC on the interlinks between built environment, walkability, physical activity and metabolic health which links with my PhD work. Furthermore, at PAHO I worked with countries from the English speaking Caribbean on further strengthening their national ESAVI (Events Supposedly Attributable to Vaccination or Immunization) surveillance."

skills <- data.frame(
  Skill = c("Project Management", "Data Analysis", "Research Design", "Report Writing", "Data Visualization", "Epidemiology"),
  Proficiency = c("Expert", "Advanced", "Advanced", "Advanced", "Expert", "Advanced")
)

professional_qualities <- data.frame(
  Diligence = c(85, 100, 0),
  Task_orientated = c(90, 100, 0),
  Self_motivated = c(95, 100, 0),
  Communication = c(80, 100, 0),
  Leadership = c(85, 100, 0),
  Creativity = c(70, 100, 0),
  Analytical = c(95, 100, 0),
  Technical = c(95, 100, 0),
  Adaptable = c(90, 100, 0),
  Collaborative = c(95, 100, 0),
  row.names = c("Score", "Max", "Min")
)

# New professional qualities data format for ggradar
# Note: The "Max" and "Min" rows are not needed for ggradar
professional_qualities_ggradar <- data.frame(
  Diligence = 85,
  Task_orientated = 90,
  Self_motivated = 95,
  Communication = 80,
  Leadership = 85,
  Creativity = 70,
  Analytical = 95,
  Technical = 95,
  Adaptable = 90,
  Collaborative = 95
)

research_interests <- c("Chronic Non-Communicable Diseases", "Built Environment", "Nutrition", "Statistical Modeling", "Epidemiology", "Public Health Geoinformatics", "Geospatial Modelling", "Vaccine Safety Surveillance", "Digital Health")

phd_text <- "Pursuing PhD in Epidemiology at the University of the West Indies. Focus: Geoinformatics applications to public health. Thesis involves spatial analysis of health data."

phd_timeline <- data.frame(
  Year = c(2022, 2023, 2024, 2025),
  Milestone = c("Coursework Completion", "Proposal Defense", "Data Collection", "Thesis Submission")
)

vaccine_text <- "Research on vaccine safety, including analysis of adverse events and public health impacts. Projects involve statistical modeling of vaccine data."

vaccine_projects <- data.frame(
  Project = c("COVID-19 Vaccine Adverse Events", "MMR Vaccine Safety Study"),
  Status = c("Completed", "Ongoing"),
  Findings = c("Low incidence of severe events", "No link to chronic diseases")
)

gis_text <- "GIS applications in public health, including mapping disease patterns and urban health analysis."

gis_projects <- data.frame(
  Project = c("Geoinformatics in Public Health", "Streetscapes Mapping in Barbados"),
  Tools = c("ArcGIS, QGIS", "Stata, GIS Software"),
  Outcome = c("Improved health policy mapping", "Urban health insights")
)

outreach_text <- "Engaged in community outreach, workshops, and public health education."

outreach_activities <- data.frame(
  Activity = c("Public Health Workshop", "Data Analysis Seminar", "Community Health Talk"),
  Date = c("2023-05", "2024-02", "2025-01"),
  Audience = c("Students", "Professionals", "General Public")
)

support_text <- "Offers data analysis consulting services for health and academic research."

services <- data.frame(
  Service = c("Statistical Analysis", "Data Visualization", "GIS Mapping", "Research Consulting"),
  Description = c("Advanced modeling and inference", "Interactive dashboards and charts", "Spatial data analysis", "Project design and execution")
)

certifications <- data.frame(
  Programme = c("DHIS2", "DHIS2", "DHIS2", "DHIS2", "HL7 FHIR", "World Health Organization (WHO)", "Uppsala Monitoring Centre (UMC)"),
  Course_Name = c("Introduction to DHIS2", "Aggregate Data Capture and Validation Fundamentals", "Planning and Budgeting DHI2 Implementations", "Aggregate Data Analysis Fundamentals", "FHIR Fundamentals", "Vaccine Safety Basics", "Statistical Reasoning and algorithms in signal detection"),
  Certificate = c("certificate1.pdf", "certificate2.pdf", "certificate3.pdf", "certificate4.pdf", "certificate5.pdf", "certificate6.pdf", "certificate7.pdf")
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Kern Rocke Resume"),
  dashboardSidebar(
    tags$img(src = "profile.png", width = "100%", style = "padding: 10px;"),
    tags$div(style = "text-align: center; padding: 10px;",
             tags$h3(strong("Kern Rocke"), style = "margin-top: 10px; margin-bottom: 5px;"),
             tags$p("kernrocke@gmail.com", style = "margin: 5px;"),
             tags$p("kernrocke@icloud.com", style = "margin: 5px;"),
             tags$hr(style = "border: 1px solid #ccc; margin: 10px 20px;"),
             tags$h4(strong("Education"), style = "margin-top: 10px; margin-bottom: 10px;"),
             tags$p("PhD in Epidemiology, 2019-present, The UWI-Cave Hill Campus, BRB, WI", style = "margin: 5px;"),
             tags$p("PhD in Human Ecology, 2017-2018 (transferred), The UWI-St. Augustine Campus, TTO, WI", style = "margin: 5px;"),
             tags$p("Masters in Epidemiology, 2014, The UWI-Mona Campus, JAM, WI", style = "margin: 5px;"),
             tags$p("Bachelors in Human Nutrition and Dietetics, 2013, The UWI-St. Augustine Campus, TTO, WI", style = "margin: 5px;"),
             tags$hr(style = "border: 1px solid #ccc; margin: 10px 20px;"),
             tags$h4(strong("FIND ME"), style = "margin-top: 10px; margin-bottom: 10px;"),
             div(
               class = "social",
               a(href = "https://www.linkedin.com/in/kern-rocke-168b5636/", target = "_blank", icon('linkedin', class = 'fa-3x', lib = "font-awesome"), style = "color: white;"),
               a(href = "https://github.com/kernrocke", target = "_blank", icon("github", class = "fa-3x", lib = "font-awesome"), style = "color: white;"),
               a(href = "mailto:kernrocke@gmail.com", target = "_blank", icon("envelope", class = 'fa-3x', lib = "font-awesome"), style = "color: white;")
             ),
    )
  ),
  dashboardBody(
    # Use tags$head to inject CSS
    tags$head(
      tags$style(
        HTML("
          /* Make the main sidebar static and full-height */
          .main-sidebar {
            position: fixed !important;
            height: 100vh;
            overflow-y: auto;
          }
          
          /* Adjust the body content to prevent it from overlapping the fixed sidebar */
          .content-wrapper, .right-side {
            margin-left: 230px !important; 
          }
          
          /* Adjust the header to prevent it from overlapping the body content */
          .main-header {
            position: fixed !important;
            width: 100%;
            z-index: 1000;
          }

          /* Add a top margin to the body to prevent content from being hidden behind the fixed header */
          .content-wrapper {
            padding-top: 50px; /* Adjust based on your header's height */
          }
        ")
      )
    ),
    tabsetPanel(
      id = "tabs",
      type = "tabs",
      tabPanel("Home", icon = icon("home"),
               fluidRow(
                 box(title = "Professional Summary", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("homeText")
                 )
               ),
               fluidRow(
                 box(title = "Certifications", width = 6, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("certificationsTable")
                 ),
                 box(title = "Professional Qualities", width = 6, solidHeader = TRUE, status = "primary",
                     plotOutput("qualitiesSpiderChart")
                 )
               ),
               fluidRow(
                 box(title = "Software Skills", width = 6, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("softwareSkillsTable")
                 ),
                 box(title = "Professional Skills", width = 6, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("skillsTable")
                 )
               )
      ),
      tabPanel("Research", icon = icon("search"),
               fluidRow(
                 box(title = "Research Interests", width = 6, solidHeader = TRUE, status = "primary",
                     uiOutput("researchInterests")
                 ),
                 box(title = "Publications Over Time", width = 6, solidHeader = TRUE, status = "primary",
                     plotOutput("pubPlot")
                 )
               ),
               fluidRow(
                 box(title = "Publications", width = 12, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("pubTable")
                 )
               )
      ),
      tabPanel("PhD", icon = icon("graduation-cap"),
               fluidRow(
                 box(title = "PhD Overview", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("phdText")
                 )
               ),
               fluidRow(
                 box(title = "PhD Timeline", width = 12, solidHeader = TRUE, status = "primary",
                     plotlyOutput("phdTimeline")
                 )
               )
      ),
      tabPanel("Vaccine Safety", icon = icon("syringe"),
               fluidRow(
                 box(title = "Vaccine Safety Research", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("vaccineText")
                 )
               ),
               fluidRow(
                 box(title = "Vaccine Projects", width = 12, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("vaccineTable")
                 )
               )
      ),
      tabPanel("GIS Work", icon = icon("map"),
               fluidRow(
                 box(title = "GIS Work Overview", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("gisText")
                 )
               ),
               fluidRow(
                 box(title = "GIS Projects", width = 12, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("gisTable")
                 )
               )
      ),
      tabPanel("Outreach", icon = icon("users"),
               fluidRow(
                 box(title = "Outreach Activities", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("outreachText")
                 )
               ),
               fluidRow(
                 box(title = "Activities Table", width = 12, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("outreachTable")
                 )
               )
      ),
      tabPanel("Data Analysis Support", icon = icon("chart-bar"),
               fluidRow(
                 box(title = "Data Analysis Services", width = 12, solidHeader = TRUE, status = "primary",
                     textOutput("supportText")
                 )
               ),
               fluidRow(
                 box(title = "Services Offered", width = 12, solidHeader = TRUE, status = "primary",
                     DT::dataTableOutput("servicesTable")
                 )
               )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load and clean CSV data
  software_skills <- reactive({
    data <- read.csv("data/cv_software_skills.csv", stringsAsFactors = FALSE)
    # Clean data: remove empty rows and handle missing values
    data <- data[complete.cases(data$Software.Name, data$Skill.Level), ]
    data$Category <- ifelse(data$Category == "", "Other", data$Category)
    # Fix typo in Category
    data$Category <- gsub("Spaital Management and Analysis", "Spatial Management and Analysis", data$Category)
    data
  })
  
  # New reactive expression to read publications data from CSV
  publications_data <- reactive({
    data <- read.csv("data/publications.csv", stringsAsFactors = FALSE)
    data
  })
  
  output$homeText <- renderText({ home_text })
  output$skillsTable <- DT::renderDataTable({
    skills }, options = list(pageLength = 6, searching = FALSE, ordering = FALSE))
  
  output$qualitiesSpiderChart <- renderPlot({
    # Reshape the data for ggradar
    data_radar <- professional_qualities_ggradar %>%
      mutate(group = "Score") %>%
      select(group, everything()) %>%
      as_tibble()
    
    # Create the radar chart
    ggradar(
      data_radar,
      grid.min = 0,
      grid.max = 100,
      grid.mid = 50,
      base.size = 1.2,
      axis.label.size = 4,
      legend.position = "none",
      values.radar = c("0", "50", "100"),
      background.circle.colour = "white",
      fill = TRUE,
      fill.alpha = 0.4,
      group.colours = "#2ca25f"
    )
  })
  
  output$certificationsTable <- DT::renderDataTable({
    datatable(
      certifications,
      options = list(pageLength = 7, searching = FALSE, ordering = FALSE),
      escape = FALSE,
      callback = JS("table.on('click.dt', 'a', function(e) {
                     e.preventDefault();
                     window.open($(this).attr('href'), '_blank');
                   });")
    ) %>% formatStyle(
      'Certificate',
      target = 'row',
      backgroundColor = styleEqual(
        certifications$Certificate,
        rep('#f7f7f7', nrow(certifications))
      )
    ) %>% formatString(
      'Certificate',
      prefix = '<a href="',
      suffix = '" class="btn btn-primary btn-sm" download>View</a>'
    )
  })
  
  output$softwareSkillsTable <- DT::renderDataTable({
    datatable(
      software_skills(),
      options = list(pageLength = 30, searching = TRUE, ordering = TRUE),
      colnames = c("Software Name", "Skill Level", "Category")
    )
  })
  
  # Corrected renderUI for research interests
  output$researchInterests <- renderUI({
    tags$ul(
      lapply(research_interests, tags$li)
    )
  })
  
  # Redo the plot using ggplot2
  # Corrected to use renderPlot
  output$pubPlot <- renderPlot({
    pubs <- publications_data()
    # Move the data preparation step inside renderPlot
    pubs_count <- pubs %>% count(Year)
    
    # Create a data frame with all years from 2014 to 2025
    all_years <- data.frame(Year = 2014:2025)
    
    # Join with publications data and replace NA counts with 0
    pubs_full_timeline <- all_years %>%
      left_join(pubs_count, by = "Year") %>%
      replace_na(list(n = 0))
    
    # Create the bar chart using ggplot2
    ggplot(pubs_full_timeline, aes(x = as.factor(Year), y = n)) +
      geom_bar(stat = "identity", fill = "#de2d26") +
      labs(title = "Publications by Year", x = "Year", y = "Count") +
      geom_text(aes(label = round(n, 1), y = n * 1.01), vjust = -0.5, size = 5) + # Corrected variable and placement
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      ) +
      scale_x_discrete(breaks = as.character(2014:2025))
  })
  
  # Update the pubTable to use the reactive data
  output$pubTable <- DT::renderDataTable({ publications_data() }, options = list(pageLength = 25))
  
  output$phdText <- renderText({ phd_text })
  output$phdTimeline <- renderPlotly({
    plot_ly(phd_timeline, x = ~Year, y = ~Milestone, type = "scatter", mode = "lines+markers",
            marker = list(color = "#ff7f0e")) %>%
      layout(title = "PhD Milestones Timeline")
  })
  
  output$vaccineText <- renderText({ vaccine_text })
  output$vaccineTable <- DT::renderDataTable({ vaccine_projects }, options = list(pageLength = 5))
  
  output$gisText <- renderText({ gis_text })
  output$gisTable <- DT::renderDataTable({ gis_projects }, options = list(pageLength = 5))
  
  output$outreachText <- renderText({ outreach_text })
  output$outreachTable <- DT::renderDataTable({ outreach_activities }, options = list(pageLength = 5))
  
  output$supportText <- renderText({ support_text })
  output$servicesTable <- DT::renderDataTable({ services }, options = list(pageLength = 5))
}

# Run the app
shinyApp(ui, server)