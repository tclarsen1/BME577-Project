library(shiny)
library(shinyWidgets)

# Define UI for application which presents data 
ui <- fluidPage(
  
  # Application title
  titlePanel("Radiotherapy Data Analysis"),
  
  
  #Header row which 
  # contains buttons which can tab between info displays
  fluidRow(
    column(2),
    column(10,
      column(2,
        actionButton("top1", label = "Cancer Overview", class = "btn-block")
      ),
      column(2,
        actionButton("top2", label = "Cancer Over Time", class = "btn-block")
      ),
      column(2,
        actionButton("top3", label = "Treatment Room", class = "btn-block")
      ),
      column(2,
        actionButton("top6", label = "Treatment Options", class = "btn-block")
      ),
      column(2,
        actionButton("top4", label = "Workload", class = "btn-block")
      ),
      column(2,
        actionButton("top5", label = "Analysis", class = "btn-block")
      )
    )
  ),
  
  #This fluid row starts the second row which contains sidebar+main
  fluidRow(
    column(2,
      fileInput(inputId = "file1",label = "Please Select A File"),
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = as.Date("2019-01-02"), end = as.Date("2021-08-23")
      ),
      pickerInput("cancerCheckbox", label = "Select Cancer(s) To View", 
                  choices = setNames(c("Ca Breast", "Ca Cervix", "H&N","Ca Rectum","Ca Lung",
                                     "Ca Esophagus","Ca Prostate","Bone Metastasis","Ca Endometrium","Sarcoma",
                                     "Other"),
                                    c("Breast", "Cervical", "Head & Neck","Rectal","Lung",
                                     "Esophagus","Prostate","Bone Metastasis","Endometrial","Sarcoma",
                                     "Other")),
                  selected = c("Ca Breast", "Ca Cervix", "H&N","Ca Rectum","Ca Lung",
                               "Ca Esophagus","Ca Prostate","Bone Metastasis","Ca Endometrium","Sarcoma",
                               "Other"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      checkboxGroupInput("machineCheckbox", label = "Select Machines To View", 
                         choices = list("COMPACT", "ONCOR", "TOMO", "CK"),
                         selected = c("COMPACT","ONCOR","TOMO", "CK")),
      checkboxGroupInput("techniqueCheckbox", label = "Select Techniques To View", 
                         choices = list("3D", "IMRT", "SRS/SBRT"),
                         selected = c("3D","IMRT", "SRS/SBRT"))
    ),
    
  # Main panel is made up of sub panels controlled by action buttons
  #Each tab is numbered (1,2,3,4). In each panel there are 4 figures in
  #   in a grid of 11, 12 made by fluidrows and columns
  #                21, 22
    column(10,
      tabsetPanel(id = "controller", 
        type="hidden",
        tabPanelBody("panel1",
          fluidRow(
            plotOutput("plot111")
          ),
          fluidRow(
            plotOutput("plot121")
          )
        ),
        tabPanelBody("panel2",
          fluidRow(
            plotOutput("plot211")
          ),
          fluidRow(
            plotOutput("plot221")
          )
        ),
        tabPanelBody("panel3",
          fluidRow(
            column(6,
              plotOutput("plot311")
            ),
            column(6,
              plotOutput("plot312")
            )
          ),
          fluidRow(
            plotOutput("plot321")
          )
        ),
        tabPanelBody("panel4",
          fluidRow(
           column(2,
                  checkboxInput("numberSwitch", label = "Numbers Visible", FALSE)
           )
          ),
          fluidRow(
            column(6,
                   plotOutput("plot411")
            ),
            column(6,
                   plotOutput("plot412")
            )
          ),
          fluidRow(
            column(6,
                   plotOutput("plot421")
            ),
            column(6,
                   plotOutput("plot422")
            )
          )
        ),
        tabPanelBody("panel5",
          fluidRow(
            br(),
            tags$style(type = "text/css", "label{font-size: 18px;}"),
            numericInput("compactWorkload", "Compact Daily Workload:", value = 40, min = 1),
            span(textOutput("compactCapacity"), style="font-size:18px"),
            span(textOutput("compactDays"), style="font-size:18px"),
            br(),
            br()
          ),
          fluidRow(
            numericInput("oncorWorkload", "ONCOR Daily Workload:", value = 40, min = 1),
            span(textOutput("oncorCapacity"), style="font-size:18px"),
            span(textOutput("oncorDays"), style="font-size:18px"),
            br(),
            br()
          ),
          fluidRow(
            numericInput("tomoWorkload", "TOMO Daily Workload:", value = 25, min = 1),
            span(textOutput("tomoCapacity"), style="font-size:18px"),
            span(textOutput("tomoDays"), style="font-size:18px"),
            br(),
            br()
          ),
          fluidRow(
            numericInput("ckWorkload", "CK Daily Workload:", value = 4, min = 1),
            span(textOutput("ckCapacity"), style="font-size:18px"),
            span(textOutput("ckDays"), style="font-size:18px"),
            br(),
            br()
          )          
        ),
        tabPanelBody("panel6",
          fluidRow(
            plotOutput("plot611")
          ),
          fluidRow(
            plotOutput("plot621")
          )
        )
      )
    )
  )
)