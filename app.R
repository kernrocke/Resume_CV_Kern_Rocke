
# Install if not already installed
install.packages("remotes")

remotes::install_github("ricardo-bion/ggradar")

#-------------------------------------------------------------------------------
######################
### Libraries ###
#####################
# Note: Add any new libraries to the list of libraries in libs

#List of libraries needed
libs <- c("shiny", "shinydashboard", "dplyr", "plotly", "DT", "fmsb", "ggplot2",
          "tidyr", "ggradar", "rsconnect")

#Install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

#Load libraries
invisible(lapply(libs, library, character.only = T))
#-------------------------------------------------------------------------------

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

research_interests <- c("Chronic Non-Communicable Diseases", "Built Environment", "Nutrition", "Statistical Modeling", "Epidemiology", "Public Health Geoinformatics", "Geospatial Modelling", "Vaccine Safety Surveillance", "Digital Health", "Sports Competitive Balance")

phd_text <- "Pursuing PhD in Epidemiology at the University of the West Indies. Focus: Geoinformatics applications to public health and cardiovascular disease. Thesis involves spatial analysis of built environment and health data exploring from perspectives from macro -> micro -> individual level"

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
             )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .main-sidebar {
            position: fixed !important;
            height: 100vh;
            overflow-y: auto;
          }
          .content-wrapper, .right-side {
            margin-left: 230px !important; 
          }
          .main-header {
            position: fixed !important;
            width: 100%;
            z-index: 1000;
          }
          .content-wrapper {
            padding-top: 50px;
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
      tabPanel("Experience", icon = icon("briefcase"),
               fluidRow(
                 box(title = "Professional Experience", width = 12, solidHeader = TRUE, status = "primary",
                     htmlOutput("experienceText")
                 )
               )
      ),
      tabPanel("Research", icon = icon("search"),
               fluidRow(
                 box(title = "Research Interests", width = 6, solidHeader = TRUE, status = "primary",
                     uiOutput("researchInterests")
                 ),
                 column(width = 6,
                        valueBoxOutput("totalPublicationsBox", width = 12),
                        box(title = "Publications Over Time", width = 12, solidHeader = TRUE, status = "primary",
                            plotOutput("pubPlot")
                        )
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
    data <- data[complete.cases(data$Software.Name, data$Skill.Level), ]
    data$Category <- ifelse(data$Category == "", "Other", data$Category)
    data$Category <- gsub("Spaital Management and Analysis", "Spatial Management and Analysis", data$Category)
    data
  })
  
  publications_data <- reactive({
    data <- read.csv("data/publications.csv", stringsAsFactors = FALSE)
    data
  })
  
  output$homeText <- renderText({ home_text })
  output$skillsTable <- DT::renderDataTable({
    skills }, options = list(pageLength = 6, searching = FALSE, ordering = FALSE))
  
  output$qualitiesSpiderChart <- renderPlot({
    data_radar <- professional_qualities_ggradar %>%
      mutate(group = "Score") %>%
      select(group, everything()) %>%
      as_tibble()
    
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
  
  output$totalPublicationsBox <- renderValueBox({
    total_pubs <- nrow(publications_data())
    valueBox(
      value = total_pubs,
      subtitle = "Total Publications",
      icon = icon("book"),
      color = "red"
    )
  })
  
  output$researchInterests <- renderUI({
    tags$ul(
      lapply(research_interests, tags$li)
    )
  })
  
  output$pubPlot <- renderPlot({
    pubs <- publications_data()
    pubs_count <- pubs %>% count(Year)
    
    all_years <- data.frame(Year = 2014:2025)
    
    pubs_full_timeline <- all_years %>%
      left_join(pubs_count, by = "Year") %>%
      replace_na(list(n = 0))
    
    ggplot(pubs_full_timeline, aes(x = as.factor(Year), y = n)) +
      geom_bar(stat = "identity", fill = "#de2d26") +
      labs(title = "Publications by Year", x = "Year", y = "Count") +
      geom_text(aes(label = round(n, 1), y = n * 1.01), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      ) +
      scale_x_discrete(breaks = as.character(2014:2025))
  })
  
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
  
  output$experienceText <- renderUI({
    HTML(
      '<h3><img src="bnr_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">Data Manager/Analyst Consultant</h3>
        <p><strong>The Barbados National Registry – The George Alleyne Chronic Disease Research Centre, Bridgetown, Barbados</strong><br>
        August 01, 2023 – Present</p>
        <ul>
        <li>Data cleaning and analysis of national cardiovascular disease registry data (2023)</li>
        <li>Data cleaning and analysis of national cardiovascular disease registry data (2019-2022)</li>
        <li>Developed analysis reports for data analysed from cardiovascular disease and cancer registries</li>
        <li>Developed national HEARTS hypertension dashboard for the automatic analysis and generation of periodic reports</li>
        <li>Lead the training in process for data cleaning and analysis on cardiovascular disease and cancer data</li>
        <li>Support ad-hoc data analysis request from stakeholders on data related to Barbados National registry</li>
        </ul>
      <h3><img src="uwi_global.png" style="vertical-align:middle; margin-right:10px; width:90px;">Online Facilitator and Statistical Advisor (Part-time)</h3>
      <p><strong>Caribbean Institute for Health Research (CAHIR), The University of the West Indies, Mona and Global Campus, Jamaica</strong><br>
      August 01, 2020 – Present</p>
      <ul>
        <li>Support the teaching of postgraduate courses in the MSc Epidemiology and Human Research and Epidemiology postgraduate diploma programmes</li>
        <li>Lead in the supervision of postgraduate student research projects from MSc Epidemiology and Human Research and Epidemiology postgraduate diploma programmes</li>
        <li>Advise students and staff on statistical procedures and analysis related to epidemiology and surveillance projects</li>
      </ul>  
      <h3><img src="paho_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">International Consultant & Epidemiologist</h3>
      <p><strong>Comprehensive Immunization Program (CIM) – Pan American Health Organization, Washington, DC, USA</strong><br>
      August 01, 2021 – July 2024</p>
      <ul>
        <li>Serve as the focal point for all activities related to the strengthening of national vaccine surveillance systems in the 23 countries of English-speaking Caribbean sub-region</li>
        <li>Support of the conduct of national level face-face workshops on vaccine safety with stakeholders from each of the countries of the Caribbean</li>
        <li>Support in the digital transformation and implementation of the data capture, management and analysis mechanisms integral for the national vaccine surveillance system</li>
        <li>Support the conceptualization, development, and implementation of projects aimed at improving the quality of ESAVI and AESI surveillance activities at the regional and national levels</li>
        <li>Support the processes of case investigation, causality analysis and data analysis for the detection of signals</li>
        <li>Lead the planning and implementation of activities related to bringing population awareness to national vaccine safety surveillance systems</li>
        <li>Provide ad-hoc technical support to immunization teams on surveillance activities related to immunization</li>
        <li>Co-lead the validation from Spanish to English on regional tools for strengthening national ESAVI surveillance systems</li>
      </ul>
      <h3><img src="goarn_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">Consultant Trainer</h3>
      <p><strong>World Health Organization – Global Outbreak and Alert Response Network, Geneva, Switzerland</strong><br>
      April 04, 2020 – July 31, 2021</p>
      <ul>
        <li>Lead in the development of training resources related to mapping of COVID-19 data</li>
        <li>Support training of stakeholders from the European; North & Central American; Caribbean and South American region on use of geographical information systems to responding to the COVID-19 pandemic</li>
        <li>Support in the assessment and evaluation of training sessions</li>
      </ul>
      <h3><img src="cdrc_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">Epidemiologist/Research Analyst (Full-time)</h3>
      <p><strong>The George Alleyne Chronic Disease Research Centre, Bridgetown, Barbados</strong><br>
      July 01, 2019 – July 31, 2021</p>
      <ul>
        <li>Lead in the conduct of research projects related to epidemiological burden of chronic non-communicable diseases and their associated risk factors</li>
        <li>Lead in the design of research studies and writing of research publications</li>
        <li>Supported the conduct of evidence reviews related to nutrition interventions</li>
        <li>Supported ad-hoc epidemiological data analysis requests from stakeholders within the Caribbean</li>
      </ul>
      <h3><img src="caricom_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">Research & Statistical Consultant (Part-time)</h3>
      <p><strong>CARICOM Community Secretariat, Turkenyn, Guyana</strong><br>
      February 01, 2017 – August 31, 2019</p>
      <ul>
        <li>Lead the data analysis of employee data on change management interventions</li>
        <li>Lead the development of analysis reports by department on change management results</li>
        <li>Lead in the development of analysis insights to be coupled with employee communication activities</li>
      </ul>
      <h3><img src="uwista_logo.png" style="vertical-align:middle; margin-right:10px; width:90px;">Researcher/Lecturer (Full-time)</h3>
      <p><strong>Nutrition Group, Department of Agricultural Economics and Extension, The University of the West Indies, St. Augustine Campus, St. Augustine, Trinidad and Tobago</strong><br>
      September 1st, 2014 – May 31st, 2017</p>
      <ul>
        <li>Lead in the development of undergraduate and postgraduate research and epidemiology courses for Nutrition and Dietetics</li>
        <li>Lead teaching undergraduate and postgraduate courses related to Human Nutrition, Research Methods and Epidemiology</li>
        <li>Lead in the supervising of undergraduate and postgraduate students in their final programme research projects</li>
        <li>Lead in the design, implementation and analysis of the epidemiological research studies targeting nutritional behaviour and chronic non-communicable disease; environmental hazards and communicable disease in at risk populations</li>
      </ul>'
    )
  })
}

# Run the app
shinyApp(ui, server)