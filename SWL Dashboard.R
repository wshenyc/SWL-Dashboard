library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(lubridate)
library(shinydashboard)
library(dashboardthemes)
library(shinyalert)
library(tidytext)
library(echarts4r) #to make interactive charts & maps! 
library(parsedate)
library(leaflet)
library(htmltools)

##----DATA-----------------------------------------------------------
#
# Description: Load in data
#____________________________________________________________________________

#General note for myself: 2016-2020 is the time frame so let me just cut down on these filters 

dir <- "R:/POLICY-STRATEGY-HOUSING POLICY/Data Projects/Speculation Watch List/SWL Exploration/Code/SWL Dashboard"

dob_permit_wide <- data.table::fread(glue("{dir}/Data/avg_perms_all_types.csv")) 

dob_permits_details <- data.table::fread(glue("{dir}/Data/dob_permit_swl.csv"))

dob_permits_chart<-data.table::fread(glue("{dir}/Data/dob_permits_chart.csv")) 

dob_subtypes <- data.table::fread(glue("{dir}/Data/dob_subtypes.csv")) 

#citywide_sales_permit <- data.table::fread(glue("{dir}/Data/sales_permits_totals_city.csv"))

#swl_sales_permits <- data.table::fread(glue("{dir}/Data/swl_sales_permits.csv"))

swl_bldgs <- data.table::fread(glue("{dir}/Data/Speculation_Watch_List.csv"))

qt_bldgs <- data.table::fread(glue("{dir}/Data/qual_transactions.csv"))

swl_table <- data.table::fread(glue("{dir}/Data/swl_table.csv"))

swl_elig_table <- data.table::fread(glue("{dir}/Data/swl_elig_table.csv"))

swl_eligible_geo <- data.table::fread(glue("{dir}/Data/swl_eligible_geocoded.csv"))

master_executed <- data.table::fread(glue("{dir}/Data/master_executed.csv"))

master_harassment <- data.table::fread(glue("{dir}/Data/master_harassment.csv"))

oca_harassment_grouped <- data.table::fread(glue("{dir}/Data/oca_harassment_grouped.csv"))

oca_executed_grouped <- data.table::fread(glue("{dir}/Data/oca_executed_grouped.csv"))

##attempting to create a map with a geojson file

#master_executed_zip <- geojsonio::geojson_read(glue("{dir}/Data/master_executed_group_zip.geojson"), what = "sp")

#HPD Complaint yearly data 
comp_details_wide <- data.table::fread(glue("{dir}/Data/comp_per_year_all_types.csv"))

#complaint types
swl_only_complaint_types <- read_csv(glue("{dir}/Data/swl_only_complaint_types.csv"))
swl_eligible_complaint_types <- read_csv(glue("{dir}/Data/swl_eligible_complaint_types.csv"))
qual_trans_complaint_types <- read_csv(glue("{dir}/Data/qual_trans_complaint_types.csv"))

#complaints table
complaint_table <- read_csv(glue("{dir}/Data/complaint_table.csv")) %>% 
  drop_na()

#HPD violations yearly data
hpd_vios <- read_csv(glue("{dir}/Data/vios_per_year_all_types.csv"))

#violations table
hpd_vios_chart <- read_csv(glue("{dir}/Data/hpd_vios_table.csv"))

#DOB violations & work without permit ECB violations
wwp_avg <- read_csv(glue("{dir}/Data/wwp_avg.csv"))
dob_vios_avg <- read_csv(glue("{dir}/Data/dob_vios_avg.csv"))

#DOB vio tables
dob_vios_chart <- read_csv(glue("{dir}/Data/dob_vios_table.csv")) %>% 
  drop_na()

wwp_chart <- read_csv(glue("{dir}/Data/wwp_table.csv")) %>% 
  drop_na()

#useful: https://echarts4r.john-coene.com/articles/get_started.html 

##----UI-----------------------------------------------------------
#
# Description: UI page
#____________________________________________________________________________


header <- dashboardHeader (
  title = "SWL Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("SWL Map", tabName = "swl_map", icon = icon("city")),
    menuItem("DOB Permits", tabName = "dob_permits", icon = icon("toolbox")),
    menuItem("DOB Violations", tabName = "dob_vios", icon = icon("toolbox")),
    menuItem("HPD Complaints & Violations", tabName = "hpd_vios", icon = icon("building")),
    menuItem("Housing Court", tabName = "evictions", icon = icon("landmark"))
  )
)

body <- dashboardBody(
  
  ###SWL Map#### 
  tabItems(
    tabItem(tabName = "swl_map",
            tabBox(
              width = 12, 
              tabPanel(
              "Map of SWL Buildings",
              leafletOutput("swl_leaflet", height = "85vh"),
              width = 12
              ),
              
              
              tabPanel(
                "SWL Buildings Quick Facts",
                
                fluidRow(
                  infoBox(
                    "What is the Speculation Watch List?",
                    "Local Law No. 7 of 2018 requires HPD to evaluate certain 
                    sales of select rent-regulated multiple dwellings for 
                    potential inclusion on the Speculation Watch List. 
                    HPD measures the potential for speculation by comparing a property’s 
                    capitalization rate (defined as a property’s net operating 
                    income divided by its sales price) to the median capitalization 
                    rate of similar buildings sold in the same borough. 
                    HPD places properties sold with capitalization rates 
                    below their borough’s median on the Speculation Watch List.",
                    icon = icon("building"),
                    width = 12
                  )
                ),#fluidrow closer
                
                br(),
                
                fluidRow(
                echarts4rOutput('swl_barchart') #add some quick facts about these buildings
              )#1st fluidrow closer
              ,
             
              fluidRow(
                valueBox("12,096" ,
                         "Residential Units Across the SWL Buildings",
                         icon=icon("house-user"),
                         width = 6),
                valueBox("350" ,
                         "Buildings on the SWL",
                         icon=icon("building"),
                         width = 6),
              )#2nd fluidrow closer 
              ),#tab panel closer
              
            tabPanel(
              "SWL Eligible Building Quick Facts",
              fluidRow(
                infoBox(
                  "What is a Speculation Watch List Eligible building?",
                  "A Speculation Watch List eligible building meets the SWL criteria for evaluation according 
                  to the Multiple Dwelling definition.",
                  icon = icon("building"),
                  width = 12
                )
              ),#fluidrow closer
              
              br(),
              
              fluidRow(
                echarts4rOutput('swl_eligible_barchart')
              ),
              
              fluidRow(
                valueBox("609,319" ,
                         "Residential Units Across the SWL Eligible Buildings",
                         icon=icon("house-user"),
                         width = 6),
                valueBox("14,167" ,
                         "Buildings that are SWL Eligible",
                         icon=icon("building"),
                         width = 6),
              )
            )
              
            )#tab box closer
            ), #tabitem closer
    
    ###DOB Permits #### 
    tabItem(tabName = "dob_permits",
            fluidRow(
              tabBox(
                title = "",
                id = "tabset1",  width = 6,
                tabPanel("Average Number of Issued DOB Permits",
                         echarts4rOutput('dob_permit_chart')), 
                tabPanel("DOF Sales and Average Number of Permits Issued",
                         fluidRow(
                         valueBox("1.51",
                                  "SWL Eligible - Average Number of Permits Isssued Within a Year of A Building's Sale",
                                  icon=icon("hammer"),
                                  width = 12)
                        
                         ),
                         fluidRow(
                           valueBox("0.85",
                                    "Qualified Transactions - Average Number of Permits Isssued Within a Year of A Building's Sale",
                                    icon = icon("hammer"),
                                    width = 12)
                           
                         ),
                         fluidRow(
                           valueBox("0.88",
                                    "SWL Only - Average Number of Permits Isssued Within a Year of A Building's Sale",
                                    icon = icon("hammer"),
                                    width = 12)
                           
                         ),
                )#tab panel closer
              ),#tab box closer 
              
              tabBox(
                title = "",
                id = "tabset2",  width = 6,
                tabPanel("DOB Permit Type Breakdown",
                         echarts4rOutput('dob_common_types')),
                tabPanel("DOB Permit Subtypes Breakdown",
                         echarts4rOutput('permit_subtype_chart'))
              )
            
            ), #1st fluidrow closer
          
            
            fluidRow(

              tabBox(
                title = "",
                id = "tabset3",  width = 12,
                tabPanel("DOB Permits Issued for SWL Eligible Buildings",
                         div(style = "overflow-x: scroll",
                             DTOutput("dob_permit_table"), width = "100%")
                ) 
              )

            )#2nd fluidrow closer
            
    ), #dob permits tab item closer 
    
    ###DOB Violations####
    tabItem(tabName = "dob_vios",
            fluidRow(
              tabBox(
                title = "",
                width = 6,
                tabPanel("DOB Violations",
                         echarts4rOutput('dob_vios_line'))
              ),
              tabBox(
                title = "",
                width = 6,
                tabPanel("Work Without Permit",
                         echarts4rOutput('wwp_line'))
              )
            ),#1st fluid row closer 
            fluidRow(
              tabBox(
                title = "",
                width = 12,
                tabPanel("DOB Violations Table",
                         div(style = "overflow-x: scroll",
                             DTOutput("dob_vios_table"), width = "100%")),
                tabPanel("Work Without Permit Table",
                         div(style = "overflow-x: scroll",
                             DTOutput("wwp_table"), width = "100%"))
              )
            )
            ), #dob violations tab item closer
    
    ###HPD Complaints/Violations#### 
    tabItem(tabName = "hpd_vios",
            fluidRow(
              tabBox(
                title = "",
                width = 6,
                tabPanel("HPD Complaints", 
                         echarts4rOutput('hpd_complaint_line')),
                tabPanel("HPD Violations",
                         echarts4rOutput('hpd_violations_line')),
                tabPanel("Class B & C violations",
                         echarts4rOutput('hpd_class_c_line'))
              
              ),#tab box closer 
              
              tabBox(
                title = "Top 5 Complaint Categories",
                width = 6,
                tabPanel("SWL Eligible",
                         echarts4rOutput('swl_eligible_complaints_sankey')),
                tabPanel("Qualified Transactions",
                         echarts4rOutput('qt_complaints_sankey')),
                tabPanel("SWL Only",
                         echarts4rOutput('swl_only_complaints_sankey'))
              )

            ), #1st fluidrow closer


            fluidRow(
              tabBox(
                title = "",
                width = 12,
                tabPanel("HPD Complaints Table",
                         div(style = "overflow-x: scroll",
                             DTOutput("hpd_comp_table"), width = "100%")
                ),
                tabPanel("HPD Violations Table",
                         div(style = "overflow-x: scroll",
                             DTOutput("hpd_vios_table"), width = "100%"))
              )

            )#2nd fluidrow closer
            
    
    ), #hpd complaints tab item closer  


    tabItem(tabName = "evictions",
            tabBox(
              width = 12, 
              tabPanel(
                "Executed Evictions",
                leafletOutput("oca_leaflet", height = "85vh"),
                width = 12
              ),#tabPanel closer
              
              tabPanel(
                "Possible Harassment Cases",
                leafletOutput("harass_leaflet", height = "85vh"),
                width = 12
              ),
              
              tabPanel(
                "Executed Evictions",
                leafletOutput("executed_zip_leaflet", height = "85vh"),
                width = 12
              ),
              
              tabPanel(
                "Housing Court Quick Facts",
                fluidRow(
                  column(6,
                  echarts4rOutput('harassment_pie')
                  ),
                  column(6,
                  echarts4rOutput('executed_pie')
                  )
                )
              )
              
              
            )#tabBox closer
            
            )#tab item closer 
  )
)

ui <- dashboardPage(
  title = "SWL Dashboard",
  
  header,
  sidebar,
  body
    
  )



##----Server-----------------------------------------------------------
#
# Description: Server
#____________________________________________________________________________


server <- function(input, output, session) {

  
##----Intro Modal-----------------------------------------------------------
#
# Description: Intro Modal
#____________________________________________________________________________
  
  shinyalert(
    title = "Introduction",
    text = 
      '<div align = "left">SWL Dashboard</div>
        </br>
        <div align = "left"><ul>
              <li>Displays information related to buildings on the Speculation Watch List (SWL) and SWL eligible buildings.</li>
              <li>Showcases findings from DOB permits, HPD violations and complaints, OCA filings</li>
              <li>Among other datasets!</li>
            <ul></div>
        </br>
        <div align = "left">To start, select one of the tabs on the left.</div>', 
    html = TRUE,
    type = "info",
    closeOnClickOutside = TRUE
  )
  
##----SWL Map-----------------------------------------------------------
#
# Description: SWL Map leaflet
#____________________________________________________________________________
  
  output$swl_leaflet <- renderLeaflet ({
    leaflet() %>%
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(data = swl_bldgs, lng = ~Longitude, lat = ~Latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "red",
                       popup = ~htmlEscape(paste(hnum_lo,str_name, sep = " ")),
                       group = "SWL Buildings") %>% 
      addCircleMarkers(data = qt_bldgs, lng = ~Longitude, lat = ~Latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "blue",
                       popup = ~htmlEscape(paste(hnum_lo,str_name, sep = " ")),
                       group = "Qualified Transactions") %>% 
      addCircleMarkers(data = swl_eligible_geo, lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "orange",
                       popup = ~htmlEscape(paste(housenum_lo,street_name, sep = " ")),
                       group = "SWL Eligible Buildings",
                       clusterOptions = markerClusterOptions()
                       ) %>% 
      addLayersControl(
        overlayGroups = c("SWL Buildings", "Qualified Transactions", "SWL Eligible Buildings"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend("bottomright", 
                colors =c("red", "blue", "orange"),
                labels= c("SWL", "Qualified Transactions","SWL Eligible"),
                title= "Building Type", 
                opacity = 1)
  })
  
  output$swl_barchart <- renderEcharts4r({
    swl_table %>% 
      e_charts(borough) %>%  
      e_bar(total) %>% 
      e_tooltip() %>% 
      e_title("  SWL Buildings Borough Breakdown") %>% 
      e_legend(FALSE)
  })
  
  output$swl_eligible_barchart <- renderEcharts4r({
    swl_elig_table %>% 
      e_charts(boro) %>% 
      e_bar(total) %>% 
      e_tooltip() %>% 
      e_title("  SWL Eligible Buildings Borough Breakdown") %>% 
      e_legend(FALSE)
  })


##----DOB Permits-----------------------------------------------------------
#
# Description: DOB Permits Charts 
#____________________________________________________________________________
  
  output$dob_permit_chart <- renderEcharts4r({
    dob_permit_wide %>%
      rename(`Qualified Transactions`= QT) %>%  
      filter(issued_year != "2021") %>% 
      mutate(issued_year = as.factor(issued_year)) %>%
      e_chart(issued_year) %>%
      e_line(`SWL Only`) %>%  
      e_line(`Qualified Transactions`) %>%
      e_line(`SWL Eligible`) %>%
      #e_axis_labels(x = "", y = "# of Permits") %>%
      e_title("Avg # Of DOB Permits Issued Per Building") %>%
      e_tooltip() %>%
      e_legend(right = 0)
  })
  
  output$dob_common_types <- renderEcharts4r({
    dob_permits_details %>% 
      filter(issuance_date %within% interval(ymd("2016-01-01"), ymd("2020-12-31"))) %>% 
      group_by(permit_type) %>% 
      summarize(Total = n()) %>% 
      mutate(permit_type = recode(
        permit_type,
        AL = 'Alteration',
        EQ = 'Construction Equipment',
        EW = 'Equipment Work',
        FO = 'Foundation/Earthwork',
        PL = 'Plumbing'
      )) %>% 
      e_charts(permit_type) %>% 
      e_pie(Total, radius = c("50%", "70%")) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />Total: ' + params.value + 
                                        '<br />Percent: ' +  params.percent)  +'%' }  ")) %>% 
      e_title("Breakdown of DOB Permit Types Issued for SWL Eligible Buildings from 2016-2020") %>% 
      e_legend(orient = 'vertical',
               right = '5',
               bottom = '15%')
  })
  


  output$permit_subtype_chart <- renderEcharts4r({
    dob_subtypes %>%
      e_charts() %>%
      e_sankey(source = permit_type, target = permit_subtype ,value = count) %>%
      e_tooltip()
  })

  output$dob_permit_table <- DT::renderDT(dob_permits_chart) 

##----DOB Violations-----------------------------------------------------------
#
# Description: DOB Violations Charts
#____________________________________________________________________________

  output$dob_vios_line <- renderEcharts4r({
    dob_vios_avg %>% 
      mutate(issued_year = as.factor(issued_year)) %>% 
      rename(`SWL Only` = avg_vios_swl,
             `Qualified Transactions` = avg_vios_qt,
             `SWL Eligible` = avg_vios_elig) %>% 
      e_chart(issued_year) %>% 
      e_line(`SWL Only`) %>% 
      e_line(`Qualified Transactions`) %>% 
      e_line(`SWL Eligible`) %>% 
      e_title("Average # of DOB Violations") %>% 
      e_tooltip() %>% 
      e_legend(right = 0)
  })
  
  output$wwp_line <- renderEcharts4r({
    wwp_avg %>% 
      mutate(issued_year = as.factor(issued_year)) %>% 
      rename(`SWL Only` = avg_wwp_swl,
             `Qualified Transactions` = avg_wwp_qt,
             `SWL Eligible` = avg_wwp_elig) %>% 
      e_chart(issued_year) %>% 
      e_line(`SWL Only`) %>% 
      e_line(`Qualified Transactions`) %>% 
      e_line(`SWL Eligible`) %>% 
      e_title("Average # of Work W/Out Permit Violations") %>% 
      e_tooltip() %>% 
      e_legend(right = 0)
  })
  
  output$dob_vios_table <- DT::renderDT(dob_vios_chart) 
  
  output$wwp_table <- DT::renderDT(wwp_chart) 
  
##----HPD Vios & Complaint Charts-----------------------------------------------------------
#
# Description: HPD Violations & Complaints 
#____________________________________________________________________________
  
  output$hpd_complaint_line <- renderEcharts4r({
    comp_details_wide %>% 
      filter(received_year != "2021") %>%  
      mutate(received_year = as.factor(received_year)) %>%
      rename(`Qualified Transactions` = QT) %>%  
      e_chart(received_year) %>%
      e_line(`SWL Only`) %>% 
      e_line(`Qualified Transactions`) %>% 
      e_line(`SWL Eligible`) %>% 
      e_title("Complaints Per DU from 2016-2020") %>% 
      e_tooltip() %>% 
      e_legend(right = 0)
  })
  
  
  output$hpd_violations_line <- renderEcharts4r({
    hpd_vios %>% 
      filter(approved_year != "2021") %>% 
      mutate(approved_year = as.factor(approved_year)) %>% 
      rename(`SWL Eligible` = `avg_vios_SWL Eligible`,
             `Qualified Transactions` = avg_vios_QT,
             `SWL Only` = `avg_vios_SWL Only`) %>% 
      e_chart(approved_year) %>% 
      e_line(`SWL Only`) %>% 
      e_line(`Qualified Transactions`) %>% 
      e_line(`SWL Eligible`) %>% 
      e_title("Violations Per DU from 2016-2020") %>% 
      e_tooltip() %>% 
      e_legend(right = 0)
  })
  
  output$hpd_class_c_line <- renderEcharts4r({
    hpd_vios %>% 
      filter(approved_year != "2021") %>% 
      mutate(approved_year = as.factor(approved_year)) %>% 
      rename(`SWL Eligible` = `avg_class_b_c_SWL Eligible`,
             `Qualified Transactions` = avg_class_b_c_QT,
             `SWL Only` = `avg_class_b_c_SWL Only`) %>% 
      e_chart(approved_year) %>% 
      e_line(`SWL Only`) %>% 
      e_line(`Qualified Transactions`) %>% 
      e_line(`SWL Eligible`) %>% 
      e_title("B & C Violations Per DU from 2016-2020") %>% 
      e_tooltip() %>% 
      e_legend(right = 0)
  })
  
  output$qt_complaints_sankey <- renderEcharts4r({
    qual_trans_complaint_types %>%
      filter(major_category %in% c("HEAT/HOT WATER", "UNSANITARY CONDITION", 
      "PAINT/PLASTER", "PLUMBING", "DOOR/WINDOW")) %>% 
      e_charts() %>%
      e_sankey(source = major_category, target = minor_category, value = count) %>%
      e_tooltip()
  })
  
  output$swl_eligible_complaints_sankey <- renderEcharts4r({
    swl_eligible_complaint_types %>% 
      filter(major_category %in% c("HEAT/HOT WATER", "UNSANITARY CONDITION", 
                                   "PAINT/PLASTER", "PLUMBING", "DOOR/WINDOW")) %>% 
      e_charts() %>%
      e_sankey(source = major_category, target = minor_category, value = count) %>%
      e_tooltip()
  })
  
  output$swl_only_complaints_sankey <- renderEcharts4r({
    swl_only_complaint_types %>% 
      filter(major_category %in% c("HEAT/HOT WATER", "UNSANITARY CONDITION", 
                                   "PAINT/PLASTER", "PLUMBING", "DOOR/WINDOW")) %>% 
      e_charts() %>%
      e_sankey(source = major_category, target = minor_category, value = count) %>%
      e_tooltip()
  })
  
  output$hpd_comp_table <- DT::renderDT(complaint_table) 
  
  output$hpd_vios_table <- DT::renderDT(hpd_vios_chart) 
  
##----OCA Filings-----------------------------------------------------------
#
# Description: Map of OCA filings for SWL Eligible 
#____________________________________________________________________________
  
  #pal <- colorFactor("BuPu", domain = unique(oca_harassment$classification)) 
  #pal2 <- colorFactor("YlOrRd", domain = unique(master_executed$classification))
  
  output$oca_leaflet <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(data = master_executed[type == "SWL Eligible"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "orange",
                       popup = ~htmlEscape(classification),
                       group = "SWL Eligible",
                       clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data = master_executed[type == "Qualified Transactions"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "blue",
                       popup = ~htmlEscape(classification),
                       group = "Qualified Transactions") %>% 
      addCircleMarkers(data = master_executed[type == "SWL"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "red",
                       popup = ~htmlEscape(classification),
                       group = "SWL") %>%
      addLayersControl(
        overlayGroups = c("SWL Eligible", "Qualified Transactions", "SWL"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend("bottomright", 
                colors =c("red", "blue", "orange"),
                labels= c("SWL", "Qualified Transactions","SWL Eligible"),
                title= "Building Type", 
                opacity = 1)
  })
  
  output$harass_leaflet <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers(data =   master_harassment[type == "SWL Eligible"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "orange",
                       popup = ~htmlEscape(classification),
                       group = "SWL Eligible",
                       clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data =   master_harassment[type == "Qualified Transactions"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "blue",
                       popup = ~htmlEscape(classification),
                       group = "Qualified Transactions") %>% 
      addCircleMarkers(data =   master_harassment[type == "SWL"], 
                       lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.5,
                       color = "red",
                       popup = ~htmlEscape(classification),
                       group = "SWL") %>%
      addLayersControl(
        overlayGroups = c("SWL Eligible", "Qualified Transactions", "SWL"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend("bottomright", 
                colors =c("red", "blue", "orange"),
                labels= c("SWL", "Qualified Transactions","SWL Eligible"),
                title= "Building Type", 
                opacity = 1)
  })
  
 
####LOTS OF TESTING WITH NEW MAPPING####
  master_executed_zip <- sf::read_sf(glue("{dir}/Data/master_executed_group_zip_wide.geojson"))
  master_executed_zip_transformed <- sf::st_transform(master_executed_zip, "+init=epsg:4326") #sweet jesus
  master_executed_zip_transformed$Refactored_Qualified.Transactions[master_executed_zip_transformed$Refactored_Qualified.Transactions == "NA"] =  NA
  master_executed_zip_transformed$Refactored_SWL[master_executed_zip_transformed$Refactored_SWL == "NA"] =  NA
  
  
  #swl_eligible_executed <- subset(master_executed_zip_transformed, Refactored_type %in% c("SWL"))
  
  pal <- colorNumeric("viridis", domain = master_executed_zip_transformed$Refactored_SWL.Eligible)
  #pal2 <- colorNumeric("viridis", domain = master_executed_zip_transformed$Refactored_Qualified.Transactions)
  #pal3 <- colorNumeric("viridis", domain = master_executed_zip_transformed$Refactored_SWL)
  
  output$executed_zip_leaflet <- renderLeaflet({
    leaflet(master_executed_zip_transformed) %>% 
      addTiles() %>% 
      setView(-74.00, 40.71, zoom = 12) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity =1.0,
                  fillOpacity = 0.5,
                  fillColor = ~pal(Refactored_SWL.Eligible),
                  label = ~paste0(ZIPCODE, ": ", Refactored_SWL.Eligible),
                  group = "SWL Eligible", #pls god 
                  highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 0.6,
                    color = "white",
                    opacity = 1.0,
                    bringToFront = T,
                    sendToBack = T
                  )) %>%
      # addPolygons(color = "#444444",
      #             weight = 1,
      #             smoothFactor = 0.5,
      #             opacity =1.0,
      #             fillOpacity = 0.5,
      #             fillColor = ~pal2(Refactored_Qualified.Transactions),
      #             label = ~paste0(ZIPCODE, ": ", Refactored_Qualified.Transactions),
      #             group = "SWL Eligible", #pls god 
      #             highlight = highlightOptions(
      #               weight = 3,
      #               fillOpacity = 0.6,
      #               color = "white",
      #               opacity = 1.0,
      #               bringToFront = T,
      #               sendToBack = T
      #             )) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~Refactored_SWL.Eligible,
                title = "# of Executed Evictions")
  })
  ####END TESTING WITH NEW MAPPING####
  
  # output$oca_leaflet <- renderLeaflet ({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(-74.00, 40.71, zoom = 12) %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     addCircleMarkers(data = oca_harassment, lng = ~longitude, lat = ~latitude,
  #                      radius = 5, stroke = FALSE, fillOpacity = 0.5,
  #                      color = ~pal(classification),
  #                      popup = ~htmlEscape(classification),
  #                      group = "Possible Harassment Cases") %>%
  #     addCircleMarkers(data = oca_executed, lng = ~longitude, lat = ~latitude,
  #                      radius = 5, stroke = FALSE, fillOpacity = 0.5,
  #                      color = ~pal2(classification),
  #                      popup = ~htmlEscape(paste(classification, execution_date, sep = " ")),
  #                      group = "Executed Evictions",
  #                      clusterOptions = markerClusterOptions()) %>%
  #     addLayersControl(
  #       overlayGroups = c("Possible Harassment Cases", "Executed Evictions"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     ) %>%
  #     addLegend("bottomright", pal = pal, values = unique(oca_harassment$classification), group = "Possible Harassment Cases",
  #               title = "Possible Harassment Case Types",
  #               opacity = 1) %>%
  #     addLegend("bottomright", pal = pal2, values = unique(oca_executed$classification), group = "Executed Evictions",
  #             title = "Eviction Case Types",
  #             opacity = 1)
  # })
  
  output$harassment_pie <- renderEcharts4r({
    oca_harassment_grouped %>%  
    e_charts(classification) %>% 
      e_pie(count, radius = c("50%", "70%")) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />Total: ' + params.value + 
                                        '<br />Percent: ' +  params.percent)  +'%' }  ")) %>% 
      e_title("Breakdown of Possible Harassment Case Types") %>% 
      e_legend(orient = 'vertical',
               right = '5',
               bottom = '15%')
  })
  
  output$executed_pie <- renderEcharts4r({
    oca_executed_grouped %>% 
      e_charts(classification) %>% 
      e_pie(count, radius = c("50%", "70%")) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />Total: ' + params.value + 
                                        '<br />Percent: ' +  params.percent)  +'%' }  ")) %>% 
      e_title("Breakdown of Executed Eviction Case Types") %>% 
      e_legend(orient = 'vertical',
               right = '5',
               bottom = '15%')
      
  })
  
  
  

} #server closing bracket 

##----App launch-----------------------------------------------------------
#
# Description: Launches app 
#____________________________________________________________________________



shinyApp(ui, server)

