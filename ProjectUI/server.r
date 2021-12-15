library(dplyr)

#install.packages("zoo")
library(zoo)

#install.packages("lubridate")
library(lubridate)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("hrbrthemes") # for style
library(hrbrthemes) 

#install.packages("RColorBrewer") # for color
library(RColorBrewer)

library(shinyWidgets)
library(scales)

server <- function(input, output) {
  
  #Reactives pull in data from slides/checkboxes
  #   These are objects, must be called with '()'
  #Checks for file input and returns table of data
  #data <-  read.csv("C:/Users/tclar/Documents/Medical School/PhDSchool/BME577/GroupProject/Radiotherapy_Data.csv")
  data <- reactive({ 
    inFile <- input$file1
    
    if(!is.null(inFile)) {
      data = read.csv(inFile$datapath);
    } else {
      return(NULL);
    }
    
    #Need to sort by selected parameters, most are inclusive and easy
    # "other" in cancer list requires if else and exclusion categorization
    defaultList = c("Ca Breast", "Ca Cervix", "H&N","Ca Rectum","Ca Lung",
        "Ca Esophagus","Ca Prostate","Bone Metastasis","Ca Endometrium","Sarcoma",
        "Other")
    
    if("Other" %in% cancerList()){
      exclude = defaultList[!defaultList %in% cancerList()]
      data <- data[data$Date > dateRange()[1] & 
                     data$Date < dateRange()[2]&
                     data$Technique %in% techniqueList()&
                     data$Room %in% roomList()&
                     !data$Disease %in% exclude,]
    }
    else
      data <- data[data$Date > dateRange()[1] & 
                    data$Date < dateRange()[2]&
                    data$Technique %in% techniqueList()&
                    data$Disease %in% cancerList()&
                    data$Room %in% roomList(),]
  
    data
    
  })
  dateRange <- reactive({
    input$dateRange
  })
  cancerList <-reactive({
    cbind(input$cancerCheckbox)
  })
  techniqueList <-reactive({
    cbind(input$techniqueCheckbox)
  })
  roomList <- reactive({
    cbind(input$machineCheckbox)
  })
  count_case_date <- reactive({
    count_case_date <- data() %>% 
      # Categorized data into group    
      group_by(Date, Room) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    count_case_date <- rename(count_case_date, Number=freq)
  })
  visibleNumbers <- reactive({
    input$numberSwitch
  })
  
  #####
  #
  ##Controllers for buttons to switch infographic tabs
  #
  #####
  observeEvent(input$top1,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel1")
  })
  observeEvent(input$top2,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel2")
  })
  observeEvent(input$top3,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel3")
  })
  observeEvent(input$top4,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel4")
  })
  observeEvent(input$top5,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel5")
  })
  observeEvent(input$top6,{
    updateTabsetPanel(inputId = "controller", 
                      selected = "panel6")
  })
  
  #####
  #
  #Text outputs for debugging/early dev.
  #
  #####
  output$panel1Text <- renderText({
    c("Test: Panel 1")
  })
  output$panel2Text <- renderText({
    c("Test: Panel 2")
  })
  output$panel3Text <- renderText({
    c("Test: panel 3", "Cancer List:", cancerList())
  })
  output$panel4Text <- renderText({
    #c("Test: panel 4", "Date Range:", as.Date(dateRange()[1]))
    paste("Date Range is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  
  #####
  #
  #Panel 1
  #
  #####
  output$plot111 <- renderPlot({
    if(is.null(data())){
      return()
    }
    # Count cancer cases for all
    count_cancer <- count(data(), Disease, sort = TRUE)
    count_cancer <- rename(count_cancer, Number=n) # rename tibble name
    count_cancer <- count_cancer[order(count_cancer$Number),]
    
    ggplot(count_cancer, aes(reorder(Disease, -Number), y=Number, fill=Number)) +
      #ggplot(count_cancer, aes(x=Disease, y=Number, fill=Disease)) + 
      scale_fill_viridis_c(option = 'viridis') +
      geom_bar(stat="identity") +
      #scale_fill_manual(values = getPalette(colourCount)) + 
      
      ggtitle("Number of treatments for each cancer type") +
      xlab("") + ylab("Number") +
      
      theme(legend.position = "none") + 
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      geom_text(aes(label=Number), vjust=-0.3, size=4) +
      guides(color = guide_legend(nrow = 3))
    
  })
  output$plot112 <-renderPlot({
    if(is.null(data())){
      return()
    }
    #NOT CURRENTLY VISIBLE
  })
  
  output$plot121 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Year-Month (All data) ####
    sort_cancer_month <- data() %>%
      # Categorized data into group
      group_by(Disease,month,year) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    sort_cancer_month <- rename(sort_cancer_month, Number=freq)
    
    # Plot graph
    sort_cancer_month %>%
      ggplot( aes(x=month, y=Number, group=Disease, color=Disease)) +
      geom_line() +
      
      ggtitle("Number of treatments for each cancer type over time") +
      xlab("") + ylab("Number") +
      
      theme(legend.title = element_blank()) +
      theme(legend.position = 'bottom') + # should be none legend?
      theme(axis.text.x=element_text(size=7, angle=90,hjust=0.95,vjust=0.2)) +
      guides(color = guide_legend(nrow = 3))
    
  })
  
  output$plot122 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #NOT CURRENTLY VISIBLE
  })
  
  #####
  #
  #Panel 2
  #
  ##### 
  output$plot211 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    count_cancer_year <- data() %>% 
      # Categorized data into group    
      group_by(year) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    count_cancer_year <- rename(count_cancer_year, Number=freq)
    
    # Plot graph - Number of patient with Year
    colourCount = length(unique(mtcars$hp))
    getPalette = colorRampPalette(brewer.pal(3, "Spectral")) # Edit color here
    
    ggplot(count_cancer_year, aes(x=year, y=Number, fill=year)) + 
      geom_bar(stat="identity") +
      
      ggtitle("Number of treatment for all cancers by year") +
      xlab("") + ylab("Number") +
      
      theme(legend.position = "none") + 
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      geom_text(aes(label=Number), vjust=-0.3, size=4)
  })
  output$plot212 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    # #### Treatment Room with cancer type ####
    # sort_cancer_Room <- data() %>% 
    #   # Categorized data into group    
    #   group_by(Disease,Room) %>%
    #   # Summarize data
    #   summarise(freq=n(), .groups = 'rowwise')
    # sort_cancer_Room  <- rename(sort_cancer_Room , Number=freq)
    # 
    # ## Plot Bar Graph ##
    # sort_cancer_Room %>%
    #   #gather("Technique", "Number",-Disease) %>%
    #   ggplot(aes(Disease, Number, fill = Room)) +
    #   geom_bar(position = "dodge", stat = "identity") +
    #   geom_text(aes(Disease, Number,label=Number, group=Room), position=position_dodge(width=1), size=2, vjust=-0.3) +
    #   theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
    #   ggtitle("Number of treatments based on cancer type for each treatment room") +
    #   xlab("") + ylab("Number") +
    #   theme(legend.position = 'bottom')
    
  })
  output$plot221 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Month-year ####
    count_cancer_month <- data() %>% 
      # Categorized data into group    
      group_by(month) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    count_cancer_month <- rename(count_cancer_month, Number=freq)
    
    # Plot graph - Number of patient with month-year
    colourCount = length(unique(mtcars$hp))
    getPalette = colorRampPalette(brewer.pal(7, "Spectral")) # Edit color here
    
    ggplot(count_cancer_month, aes(x=month, y=Number, fill=month)) + 
      geom_bar(stat="identity") +
      
      ggtitle("Number of treatments for all cancers by month") +
      xlab("") + ylab("Number") +
      
      theme(legend.position = "none") + 
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      geom_text(aes(label=Number), vjust=-0.3, size=3) +
      guides(color = guide_legend(nrow = 3))
  })
  output$plot222 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    
  })

  #####
  #
  #Panel 3
  #
  #####
  output$plot311 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    # All case
    # Count cancer cases for all
    count_mac <- count(data(), Room, sort = TRUE)
    
    count_mac <- group_by(count_mac , Room) %>%
      tally(n, sort = TRUE)
    
    #Rename
    count_mac  <- rename(count_mac , Number=n, Machine=Room)
    
    # Plot Bar Chart
    # Number vs Disease
    colourCount = length(unique(mtcars$hp))
    getPalette = colorRampPalette(brewer.pal(5, "Spectral")) # Edit color here
    
    ggplot(count_mac, aes(reorder(Machine, -Number), y=Number, fill=Machine)) + geom_bar(stat="identity") +
      #  scale_fill_manual(values = getPalette(colourCount)) + 
      
      ggtitle("Number of treatments by room") +
      xlab("") + ylab("Number") +
      
      theme(legend.position = "none") + 
      theme(axis.text.x=element_text(size=12, angle=0,hjust=0.95,vjust=0.2)) +
      geom_text(aes(label=Number), vjust=-0.3, size=4) 
    
  })
  output$plot312 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Year (All data) ####
    sort_mac_year <- data() %>% 
      # Categorized data into group    
      group_by(Room,year) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    sort_mac_year  <- rename(sort_mac_year , Number=freq, Machine=Room)
    
    # Plot graph
    sort_mac_year %>%
      ggplot( aes(x=year, y=Number, group=Machine, color=Machine)) +
      geom_text(aes(label=Number), size=3, vjust=-.5, show.legend=FALSE) +
      geom_line()+
      
      ggtitle("Number of treatments by room over time (year)") +
      xlab("") + ylab("Number") +
      
      theme(legend.position = 'bottom')
    
  })
  output$plot321 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Year-Month (All data) ####
    sort_mac_month <- data() %>%
      # Categorized data into group
      group_by(Room,month,year) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    sort_mac_month <- rename(sort_mac_month, Number=freq, Machine=Room)
    
    # Plot graph
    sort_mac_month %>%
      ggplot( aes(x=month, y=Number, group=Machine, color=Machine)) +
      geom_line() +
      
      ggtitle("Number of treatments by room over time (month)") +
      xlab("Month") + ylab("Number") +
      
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      geom_text(aes(label=Number), size=3, vjust=-.5, show.legend=FALSE) + # Adjust text size on line
      theme(legend.position = 'bottom')
  })
  output$plot322 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #CURRENTLY NOT VISIBLE
  })
  
  #####
  #
  #Panel 4
  #
  #####
  #Machine Workload Information
  #ONCOR
  output$plot411 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    num_ONCOR <- filter(count_case_date(), Room == "ONCOR")
    ONCOR_limit=input$oncorWorkload
    num_ONCOR$limit <- num_ONCOR$Number/ ONCOR_limit
    num_ONCOR <- mutate(num_ONCOR, condition=if_else(limit>1, "Full", "Available"))
    num_ONCOR$remain <- abs(num_ONCOR$Number - ONCOR_limit)
    group.colors <- c(Full = "indianred1", Available = "aquamarine2")
    if(visibleNumbers() == TRUE){
      ggplot(num_ONCOR, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("ONCOR Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_text(aes(label=Number), vjust=-0.3, size=2.5)  +
        geom_hline(yintercept=ONCOR_limit, linetype="dashed", color = "red")
    }
    else{
      num_ONCOR$Date <-as.Date(num_ONCOR$Date)
      ggplot(num_ONCOR, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("ONCOR Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_hline(yintercept=ONCOR_limit, linetype="dashed", color = "red") +
        scale_x_date(date_labels = "%Y-%m")
    }
  })
  #COMPACT
  output$plot412 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    num_COMPACT <- filter(count_case_date(), Room == "COMPACT")
    COMPACT_limit=input$compactWorkload
    num_COMPACT$limit <- num_COMPACT$Number/ COMPACT_limit
    num_COMPACT <- mutate(num_COMPACT, condition=if_else(limit>1, "Full", "Available"))
    num_COMPACT$remain <- abs(num_COMPACT$Number - COMPACT_limit)
    group.colors <- c(Full = "indianred1", Available = "aquamarine2")
    if(visibleNumbers() == TRUE){
      ggplot(num_COMPACT, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("COMPACT Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_text(aes(label=Number), vjust=-0.3, size=2.5)  +
        geom_hline(yintercept=COMPACT_limit, linetype="dashed", color = "red")
    }
    else{
      num_COMPACT$Date <-as.Date(num_COMPACT$Date) 
      ggplot(num_COMPACT, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("COMPACT Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_hline(yintercept=COMPACT_limit, linetype="dashed", color = "red") +
        scale_x_date(date_labels = "%Y-%m")
    }
  })
  #TOMO
  output$plot421 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    num_TOMO <- filter(count_case_date(), Room == "TOMO")
    TOMO_limit=input$tomoWorkload
    num_TOMO$limit <- num_TOMO$Number/ TOMO_limit
    num_TOMO <- mutate(num_TOMO, condition=if_else(limit>1, "Full", "Available"))
    num_TOMO$remain <- abs(num_TOMO$Number - TOMO_limit)
    group.colors <- c(Full = "indianred1", Available = "aquamarine2")
    if(visibleNumbers() == TRUE){
      ggplot(num_TOMO, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("TOMO Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_text(aes(label=Number), vjust=-0.3, size=2.5)  +
        geom_hline(yintercept=TOMO_limit, linetype="dashed", color = "red")
    }
    else{
      num_TOMO$Date <-as.Date(num_TOMO$Date)
      ggplot(num_TOMO, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("TOMO Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_hline(yintercept=TOMO_limit, linetype="dashed", color = "red") +
        scale_x_date(date_labels = "%Y-%m")
    }
  })
  #CK
  output$plot422 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    num_CK <- filter(count_case_date(), Room == "CK")
    CK_limit=input$ckWorkload
    num_CK$limit <- num_CK$Number/ CK_limit
    num_CK <- mutate(num_CK, condition=if_else(limit>1, "Full", "Available"))
    num_CK$remain <- abs(num_CK$Number - CK_limit)
    group.colors <- c(Full = "indianred1", Available = "aquamarine2")
    if(visibleNumbers() == TRUE){
      ggplot(num_CK, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("CK Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_text(aes(label=Number), vjust=-0.3, size=2.5)  +
        geom_hline(yintercept=CK_limit, linetype="dashed", color = "red")
    }
    else{
      num_CK$Date <-as.Date(num_CK$Date)
      ggplot(num_CK, aes(x=Date, y=Number, fill=condition)) + 
        geom_bar(stat="identity") +
        ggtitle("CK Workload") +
        #Specify colours
        scale_fill_manual(values=group.colors) +
        theme(legend.position = c(1,1), legend.justification = c(1,1)) + 
        theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
        geom_hline(yintercept=CK_limit, linetype="dashed", color = "red") +
        scale_x_date(date_labels = "%Y-%m")
    }
  })
  #####
  #
  #Panel 5
  #
  #####
  output$compactCapacity <- renderText({
    num_COMPACT <- filter(count_case_date(), Room == "COMPACT")
    num_COMPACT$limit <- num_COMPACT$Number/ input$compactWorkload
    working = sum(num_COMPACT$limit)
    counts = nrow(num_COMPACT)
    c("Workload Capacity of COMPACT:", working/counts)
  })
  output$compactDays <- renderText({
    num_COMPACT <- filter(count_case_date(), Room == "COMPACT")
    num_COMPACT$limit <- num_COMPACT$Number/ input$compactWorkload
    Exceeds = sum(num_COMPACT$limit > 1)
    total = nrow(num_COMPACT)
    c("Workload exceeded capacity", Exceeds, "out of", total, "days.\n",
      "This is ", round(Exceeds/total*100,2), "% of days")
  })
  output$oncorCapacity <- renderText({
    num_ONCOR <- filter(count_case_date(), Room == "ONCOR")
    num_ONCOR$limit <- num_ONCOR$Number/ input$oncorWorkload
    working = sum(num_ONCOR$limit)
    counts = nrow(num_ONCOR)
    c("Workload Capacity of ONCOR:", working/counts)
  })
  output$oncorDays <- renderText({
    num_ONCOR <- filter(count_case_date(), Room == "ONCOR")
    num_ONCOR$limit <- num_ONCOR$Number/ input$oncorWorkload
    Exceeds = sum(num_ONCOR$limit > 1)
    total = nrow(num_ONCOR)
    c("Workload exceeded capacity", Exceeds, "out of", total, "days.\n",
      "This is ", round(Exceeds/total*100,2), "% of days")
  })
  output$tomoCapacity <- renderText({
    num_TOMO <- filter(count_case_date(), Room == "TOMO")
    num_TOMO$limit <- num_TOMO$Number/ input$tomoWorkload
    working = sum(num_TOMO$limit)
    counts = nrow(num_TOMO)
    c("Workload Capacity of TOMO:", working/counts)
  })
  output$tomoDays <- renderText({
    num_TOMO <- filter(count_case_date(), Room == "TOMO")
    num_TOMO$limit <- num_TOMO$Number/ input$tomoWorkload
    Exceeds = sum(num_TOMO$limit > 1)
    total = nrow(num_TOMO)
    c("Workload exceeded capacity", Exceeds, "out of", total, "days.\n",
      "This is ", round(Exceeds/total*100,2), "% of days")
  })
  output$ckCapacity <- renderText({
    num_CK <- filter(count_case_date(), Room == "CK")
    num_CK$limit <- num_CK$Number/ input$ckWorkload
    working = sum(num_CK$limit)
    counts = nrow(num_CK)
    c("Workload Capacity of CK:", working/counts)
  })
  output$ckDays <- renderText({
    num_CK <- filter(count_case_date(), Room == "CK")
    num_CK$limit <- num_CK$Number/ input$ckWorkload
    Exceeds = sum(num_CK$limit > 1)
    total = nrow(num_CK)
    c("Workload exceeded capacity", Exceeds, "out of", total, "days.\n",
      "This is ", round(Exceeds/total*100,2), "% of days")
  })
  #####
  #
  #Panel 6 (4 in app) but due to time added, labeled as 6
  #
  #####
  output$plot611 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Treatment technique with cancer type ####
    sort_technique <- data() %>% 
      # Categorized data into group    
      group_by(Disease,Technique) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    sort_technique  <- rename(sort_technique , Number=freq)
    
    ## Plot Bar Graph ##
    sort_technique %>%
      #gather("Technique", "Number",-Disease) %>%
      ggplot(aes(Disease, Number, fill = Technique)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(aes(Disease, Number,label=Number, group=Technique), position=position_dodge(width=1), size=2.5, vjust=-0.3) +
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      ggtitle("Treatments for each cancer separated by technique") +
      xlab("") + ylab("Number") +
      theme(legend.position = 'bottom')
  })
  output$plot621 <-renderPlot({
    if(is.null(input$file1)){
      return()
    }
    #### Treatment Room with cancer type ####
    sort_cancer_Room <- data() %>% 
      # Categorized data into group    
      group_by(Disease,Room) %>%
      # Summarize data
      summarise(freq=n(), .groups = 'rowwise')
    sort_cancer_Room  <- rename(sort_cancer_Room , Number=freq)
    
    ## Plot Bar Graph ##
    sort_cancer_Room %>%
      #gather("Technique", "Number",-Disease) %>%
      ggplot(aes(Disease, Number, fill = Room)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(aes(Disease, Number,label=Number, group=Room), position=position_dodge(width=1), size=2.5, vjust=-0.3) +
      theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
      ggtitle("Treatments for each cancer separated by room") +
      xlab("") + ylab("Number") +
      theme(legend.position = 'bottom')
  })
}