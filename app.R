library(shiny)
library(dplyr)
library(readr)
library(tidyverse)
library(jsonlite)
library(janitor)
library(shinyWidgets)
# library(rlang)

#THIS ONE IS MINE FOR META 33



options(shiny.maxRequestSize = 60*1024^2)

# source("Meta Clean.R")
# source("function - meta.R")

extract_states <- function(json_data) {
  parsed_data <- fromJSON(json_data)
  state_names <- names(parsed_data)
  percentages <- unlist(parsed_data)
  return(data.frame(state_names, percentages))
}

#This one actually works

# ui <- fluidPage(
#   titlePanel("Meta Ads Cleaning App"),
#   fileInput("file", "Choose a CSV file"),
#   actionButton("cleanBtn", "Clean Data"),
#   downloadButton("downloadBtn1", "Impressions by State CSV"),
#   downloadButton("downloadBtn2", "Spending by State CSV"),
#   downloadButton("downloadBtn3", "Impressions by Demographic CSV"),
#   tableOutput("data_table"), 
#   setBackgroundImage(
#     src = "https://images.unsplash.com/photo-1618005182384-a83a8bd57fbe?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=1528&q=80"
#   )
# )

ui <- fluidPage(
  titlePanel(div("Meta Ads Cleaning App", style = "color: white")),
  fileInput("file", "Choose a CSV file",),
  actionButton("cleanBtn", "Clean Data"),
  downloadButton("downloadBtn1", "Impressions by State CSV"),
  downloadButton("downloadBtn2", "Spending by State CSV"),
  downloadButton("downloadBtn3", "Impressions by Demographic CSV"),
  tableOutput("data_table"), 
  setBackgroundImage(
    src = "https://images.unsplash.com/photo-1618760760733-f7ed5d01e561?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=2729&q=80"
  )
)

server <- function(input, output) {
  
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    
    # Read the uploaded CSV file
    data_raw <- read_csv(input$file$datapath)
    
    # Store the data in the reactive value
    data(data_raw)
  })
  
  #START OF THE FIRST CLEANING PROCESS
  
  meta_data1 <- eventReactive(input$cleanBtn, {
    req(data())
    
    # Apply cleaning method 1
    meta_data <- data()
    # Your cleaning process for method 1 here
    # Example: Convert all columns to uppercase
    meta_data$delivery_by_region <- gsub("percentage", "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub("region", "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub('""', "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub(':"', '"', as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub(',:', ': ', as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub("\\},\\{", ', ', as.character(meta_data$delivery_by_region))
    
    meta_data <- meta_data %>%
      filter(delivery_by_region != '') %>%
      mutate(json_data = delivery_by_region)
    
    #works UTH (up to here)
    
    meta_data <- meta_data %>%
      rowwise() %>%
      mutate(parsed_data = list(extract_states(json_data))) %>%
      unnest(parsed_data) %>%
      pivot_wider(names_from = state_names, values_from = percentages)
    
    #UTH
    
    meta_data <- meta_data %>%
      rename(DC = `Washington, District of Columbia`) %>%
      clean_names()
    
    #UTH
    
    meta_data <- meta_data %>%
      select(ad_archive_id, page_id, page_name, ad_creation_time, 
             ad_delivery_start_time, ad_delivery_stop_time, byline, 
             ad_creative_bodies, ad_creative_link_titles, 
             ad_creative_link_captions, ad_creative_link_descriptions, 
             impressions, spend, currency, demographic_distribution, 
             delivery_by_region, publisher_platforms, estimated_audience_size, 
             languages, json_data, alabama, alaska, arizona, arkansas, california, 
             colorado, connecticut, dc, delaware, florida, georgia, hawaii, 
             idaho, illinois, indiana, iowa, kansas, kentucky, louisiana, 
             maine, maryland, massachusetts, michigan, minnesota, mississippi, 
             missouri, montana, nebraska, nevada, new_hampshire, new_jersey, 
             new_mexico, new_york, north_carolina, north_dakota, ohio, oklahoma, 
             oregon, pennsylvania, rhode_island, south_carolina, south_dakota, 
             tennessee, texas, utah, vermont, virginia, washington, west_virginia, 
             wisconsin, wyoming)
    
    meta_data <- meta_data %>%
      separate(col = impressions, into = c("lower_impress", "upper_impress"), sep = ",") %>%
      separate(col = spend, into = c("lower_spend", "upper_spend"), sep = ",")
    
    meta_data$lower_impress <- gsub("lower_bound: ", '', as.character(meta_data$lower_impress))
    meta_data$upper_impress <- gsub("upper_bound: ", '', as.character(meta_data$upper_impress))
    meta_data$lower_spend <- gsub("lower_bound: ", '', as.character(meta_data$lower_spend))
    meta_data$upper_spend <- gsub("upper_bound: ", '', as.character(meta_data$upper_spend))
    
    meta_data <- meta_data %>%
      transform(lower_impress = as.numeric(lower_impress)) %>%
      transform(upper_impress = as.numeric(upper_impress)) %>%
      transform(lower_spend = as.numeric(lower_spend)) %>%
      transform(upper_spend = as.numeric(upper_spend)) %>%
      mutate(avg_impress = (lower_impress + upper_impress)/2) %>%
      mutate(avg_spend = (lower_spend + upper_spend)/2)
    
    
    meta_data <- meta_data %>%
      group_by(page_name) %>%
      summarise(
        total_impressions = sum(avg_impress, na.rm = TRUE),
        alabama = sum(alabama * avg_impress, na.rm = TRUE),
        alaska = sum(alaska * avg_impress, na.rm = TRUE),
        arizona = sum(arizona * avg_impress, na.rm = TRUE),
        arkansas = sum(arkansas * avg_impress, na.rm = TRUE),
        california = sum(california * avg_impress, na.rm = TRUE),
        colorado = sum(colorado * avg_impress, na.rm = TRUE),
        connecticut = sum(connecticut * avg_impress, na.rm = TRUE),
        dc = sum(dc *avg_impress, na.rm = TRUE),
        delaware = sum(delaware * avg_impress, na.rm = TRUE),
        florida = sum(florida * avg_impress, na.rm = TRUE),
        georgia = sum(georgia * avg_impress, na.rm = TRUE),
        hawaii = sum(hawaii * avg_impress, na.rm = TRUE),
        idaho = sum(idaho * avg_impress, na.rm = TRUE),
        illinois = sum(illinois * avg_impress, na.rm = TRUE),
        indiana = sum(indiana * avg_impress, na.rm = TRUE),
        iowa = sum(iowa * avg_impress, na.rm = TRUE),
        kansas = sum(kansas * avg_impress, na.rm = TRUE),
        kentucky = sum(kentucky * avg_impress, na.rm = TRUE),
        louisiana = sum(louisiana * avg_impress, na.rm = TRUE),
        maine = sum(maine * avg_impress, na.rm = TRUE),
        maryland = sum(maryland * avg_impress, na.rm = TRUE),
        massachusetts = sum(massachusetts * avg_impress, na.rm = TRUE),
        michigan = sum(michigan * avg_impress, na.rm = TRUE),
        minnesota = sum(minnesota * avg_impress, na.rm = TRUE),
        mississippi = sum(mississippi * avg_impress, na.rm = TRUE),
        missouri = sum(missouri * avg_impress, na.rm = TRUE),
        montana = sum(montana * avg_impress, na.rm = TRUE),
        nebraska = sum(nebraska * avg_impress, na.rm = TRUE),
        nevada = sum(nevada * avg_impress, na.rm = TRUE),
        new_hampshire = sum(new_hampshire * avg_impress, na.rm = TRUE),
        new_jersey = sum(new_jersey * avg_impress, na.rm = TRUE),
        new_mexico = sum(new_mexico * avg_impress, na.rm = TRUE),
        new_york = sum(new_york * avg_impress, na.rm = TRUE),
        north_carolina = sum(north_carolina * avg_impress, na.rm = TRUE),
        north_dakota = sum(north_dakota * avg_impress, na.rm = TRUE),
        ohio = sum(ohio * avg_impress, na.rm = TRUE),
        oklahoma = sum(oklahoma * avg_impress, na.rm = TRUE),
        oregon = sum(oregon * avg_impress, na.rm = TRUE),
        pennsylvania = sum(pennsylvania * avg_impress, na.rm = TRUE),
        rhode_island = sum(rhode_island * avg_impress, na.rm = TRUE),
        south_carolina = sum(south_carolina * avg_impress, na.rm = TRUE),
        south_dakota = sum(south_dakota * avg_impress, na.rm = TRUE),
        tennessee = sum(tennessee * avg_impress, na.rm = TRUE),
        texas = sum(texas * avg_impress, na.rm = TRUE),
        utah = sum(utah * avg_impress, na.rm = TRUE),
        vermont = sum(vermont * avg_impress, na.rm = TRUE),
        virginia = sum(virginia * avg_impress, na.rm = TRUE),
        washington = sum(washington * avg_impress, na.rm = TRUE),
        west_virginia = sum(west_virginia * avg_impress, na.rm = TRUE),
        wisconsin = sum(wisconsin * avg_impress, na.rm = TRUE),
        wyoming = sum(wyoming * avg_impress, na.rm = TRUE)
      )
    
    meta_data
  })
  
  #START OF SECOND CLEANING PROCESS
  
  meta_data2 <- eventReactive(input$cleanBtn, {
    req(data())
    
    meta_data <- data()
    # Your cleaning process for method 1 here
    # Example: Convert all columns to uppercase
    meta_data$delivery_by_region <- gsub("percentage", "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub("region", "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub('""', "", as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub(':"', '"', as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub(',:', ': ', as.character(meta_data$delivery_by_region))
    meta_data$delivery_by_region <- gsub("\\},\\{", ', ', as.character(meta_data$delivery_by_region))
    
    meta_data <- meta_data %>%
      filter(delivery_by_region != '') %>%
      mutate(json_data = delivery_by_region)
    
    
    meta_data <- meta_data %>%
      rowwise() %>%
      mutate(parsed_data = list(extract_states(json_data))) %>%
      unnest(parsed_data) %>%
      pivot_wider(names_from = state_names, values_from = percentages)
    
    
    meta_data <- meta_data %>%
      rename(DC = `Washington, District of Columbia`) %>%
      clean_names()
    
    
    meta_data <- meta_data %>%
      select(ad_archive_id, page_id, page_name, ad_creation_time, 
             ad_delivery_start_time, ad_delivery_stop_time, byline, 
             ad_creative_bodies, ad_creative_link_titles, 
             ad_creative_link_captions, ad_creative_link_descriptions, 
             impressions, spend, currency, demographic_distribution, 
             delivery_by_region, publisher_platforms, estimated_audience_size, 
             languages, json_data, alabama, alaska, arizona, arkansas, california, 
             colorado, connecticut, dc, delaware, florida, georgia, hawaii, 
             idaho, illinois, indiana, iowa, kansas, kentucky, louisiana, 
             maine, maryland, massachusetts, michigan, minnesota, mississippi, 
             missouri, montana, nebraska, nevada, new_hampshire, new_jersey, 
             new_mexico, new_york, north_carolina, north_dakota, ohio, oklahoma, 
             oregon, pennsylvania, rhode_island, south_carolina, south_dakota, 
             tennessee, texas, utah, vermont, virginia, washington, west_virginia, 
             wisconsin, wyoming)
    
    meta_data <- meta_data %>%
      separate(col = impressions, into = c("lower_impress", "upper_impress"), sep = ",") %>%
      separate(col = spend, into = c("lower_spend", "upper_spend"), sep = ",")
    
    meta_data$lower_impress <- gsub("lower_bound: ", '', as.character(meta_data$lower_impress))
    meta_data$upper_impress <- gsub("upper_bound: ", '', as.character(meta_data$upper_impress))
    meta_data$lower_spend <- gsub("lower_bound: ", '', as.character(meta_data$lower_spend))
    meta_data$upper_spend <- gsub("upper_bound: ", '', as.character(meta_data$upper_spend))
    
    meta_data <- meta_data %>%
      transform(lower_impress = as.numeric(lower_impress)) %>%
      transform(upper_impress = as.numeric(upper_impress)) %>%
      transform(lower_spend = as.numeric(lower_spend)) %>%
      transform(upper_spend = as.numeric(upper_spend)) %>%
      mutate(avg_impress = (lower_impress + upper_impress)/2) %>%
      mutate(avg_spend = (lower_spend + upper_spend)/2)
    
    meta_data <- meta_data %>%
      group_by(page_name) %>%
      summarise(
        total_spend = sum(avg_spend, na.rm = TRUE),
        alabama = sum(alabama * avg_spend,  na.rm = TRUE),
        alaska = sum(alaska * avg_spend, na.rm = TRUE),
        arizona = sum(arizona * avg_spend, na.rm = TRUE),
        arkansas = sum(arkansas * avg_spend, na.rm = TRUE),
        california = sum(california * avg_spend, na.rm = TRUE),
        colorado = sum(colorado * avg_spend, na.rm = TRUE),
        connecticut = sum(connecticut * avg_spend, na.rm = TRUE),
        dc = sum(dc * avg_spend, na.rm = TRUE),
        delaware = sum(delaware * avg_spend, na.rm = TRUE),
        florida = sum(florida * avg_spend, na.rm = TRUE),
        georgia = sum(georgia * avg_spend, na.rm = TRUE),
        hawaii = sum(hawaii * avg_spend, na.rm = TRUE),
        idaho = sum(idaho * avg_spend, na.rm = TRUE),
        illinois = sum(illinois * avg_spend, na.rm = TRUE),
        indiana = sum(indiana * avg_spend, na.rm = TRUE),
        iowa = sum(iowa * avg_spend, na.rm = TRUE),
        kansas = sum(kansas * avg_spend, na.rm = TRUE),
        kentucky = sum(kentucky * avg_spend, na.rm = TRUE),
        louisiana = sum(louisiana * avg_spend, na.rm = TRUE),
        maine = sum(maine * avg_spend, na.rm = TRUE),
        maryland = sum(maryland * avg_spend, na.rm = TRUE),
        massachusetts = sum(massachusetts * avg_spend, na.rm = TRUE),
        michigan = sum(michigan * avg_spend, na.rm = TRUE),
        minnesota = sum(minnesota * avg_spend, na.rm = TRUE),
        mississippi = sum(mississippi * avg_spend, na.rm = TRUE),
        missouri = sum(missouri * avg_spend, na.rm = TRUE),
        montana = sum(montana * avg_spend, na.rm = TRUE),
        nebraska = sum(nebraska * avg_spend, na.rm = TRUE),
        nevada = sum(nevada * avg_spend, na.rm = TRUE),
        new_hampshire = sum(new_hampshire * avg_spend, na.rm = TRUE),
        new_jersey = sum(new_jersey * avg_spend, na.rm = TRUE),
        new_mexico = sum(new_mexico * avg_spend, na.rm = TRUE),
        new_york = sum(new_york * avg_spend, na.rm = TRUE),
        north_carolina = sum(north_carolina * avg_spend, na.rm = TRUE),
        north_dakota = sum(north_dakota * avg_spend, na.rm = TRUE),
        ohio = sum(ohio * avg_spend, na.rm = TRUE),
        oklahoma = sum(oklahoma * avg_spend, na.rm = TRUE),
        oregon = sum(oregon * avg_spend, na.rm = TRUE),
        pennsylvania = sum(pennsylvania * avg_spend, na.rm = TRUE),
        rhode_island = sum(rhode_island * avg_spend, na.rm = TRUE),
        south_carolina = sum(south_carolina * avg_spend, na.rm = TRUE),
        south_dakota = sum(south_dakota * avg_spend, na.rm = TRUE),
        tennessee = sum(tennessee * avg_spend, na.rm = TRUE),
        texas = sum(texas * avg_spend, na.rm = TRUE),
        utah = sum(utah * avg_spend, na.rm = TRUE),
        vermont = sum(vermont * avg_spend, na.rm = TRUE),
        virginia = sum(virginia * avg_spend, na.rm = TRUE),
        washington = sum(washington * avg_spend, na.rm = TRUE),
        west_virginia = sum(west_virginia * avg_spend, na.rm = TRUE),
        wisconsin = sum(wisconsin * avg_spend, na.rm = TRUE),
        wyoming = sum(wyoming * avg_spend, na.rm = TRUE)
      )
    
    
    meta_data
  })
  
  cleaned_data3 <- eventReactive(input$cleanBtn, {
    req(data())
    
    meta_data <- data()
    # Apply cleaning method 3
    # cleaned_data <- data() %>%
    #   # Your cleaning process for method 3 here
    #   # Example: Remove rows where a specific column has a certain value
    #   mutate_all(toupper)
    
    meta_data$demographic_distribution <- gsub("percentage", "", as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub("gender", "", as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub("age", "", as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub('""', "", as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub(':"', '"', as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub(',:', ': ', as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub("\\},\\{", ', ', as.character(meta_data$demographic_distribution))
    meta_data$demographic_distribution <- gsub('","', "", as.character(meta_data$demographic_distribution))
    
    meta_data <- meta_data %>%
      filter(demographic_distribution != '') %>%
      mutate(json_data = demographic_distribution)
    
    meta_data <- meta_data %>%
      rowwise() %>%
      mutate(parsed_data = list(extract_states(json_data))) %>%
      unnest(parsed_data) %>%
      pivot_wider(names_from = state_names, values_from = percentages)
    
    meta_data <- meta_data %>%
      separate(col = impressions, into = c("lower_impress", "upper_impress"), sep = ",") %>%
      separate(col = spend, into = c("lower_spend", "upper_spend"), sep = ",")
    
    meta_data$lower_impress <- gsub("lower_bound: ", '', as.character(meta_data$lower_impress))
    meta_data$upper_impress <- gsub("upper_bound: ", '', as.character(meta_data$upper_impress))
    meta_data$lower_spend <- gsub("lower_bound: ", '', as.character(meta_data$lower_spend))
    meta_data$upper_spend <- gsub("upper_bound: ", '', as.character(meta_data$upper_spend))
    
    meta_data <- meta_data %>%
      rename(`male_45to54` = "45-54male",
             `male_65plus` = "65+male",
             `female_65plus` = "65+female",
             `male_55to64` = "55-64male",
             `male_35to44` = "35-44male",
             `female_55to64` = "55-64female",
             `unknown_55to64` = "55-64unknown",
             `female_45to54` = "45-54female",
             `unknown_25to34` = "25-34unknown",
             `unknown_35to44` = "35-44unknown",
             `unknown_18to24` = "18-24unknown",
             `female_18to24` = "18-24female",
             `unknown_45to54` = "45-54unknown",
             `female_25to34` = "25-34female",
             `male_18to24` = "18-24male",
             `female_35to44` = "35-44female",
             `male_25to34` = "25-34male",
             `unknown_65plus` = "65+unknown",
             `female_13to17` = "13-17female",
             `male_13to17` = "13-17male")
    # `unknown_13to17` = "13-17unknown")
    
    meta_data <- meta_data %>%
      transform(lower_impress = as.numeric(lower_impress)) %>%
      transform(upper_impress = as.numeric(upper_impress)) %>%
      transform(lower_spend = as.numeric(lower_spend)) %>%
      transform(upper_spend = as.numeric(upper_spend)) %>%
      mutate(avg_impress = (lower_impress + upper_impress)/2) %>%
      mutate(avg_spend = (lower_spend + upper_spend)/2)
    
    meta_data <- meta_data %>%
      group_by(page_name) %>%
      summarise(
        male_45_54 = sum(male_45to54*avg_impress, na.rm = TRUE),
        male_65_plus = sum(male_65plus*avg_impress, na.rm = TRUE),
        female_65_plus = sum(female_65plus*avg_impress, na.rm = TRUE),
        male_55_64 = sum(male_55to64*avg_impress, na.rm = TRUE),
        male_35_44 = sum(male_35to44*avg_impress, na.rm = TRUE),
        female_55_64 = sum(female_55to64*avg_impress, na.rm = TRUE),
        unknown_55_64 = sum(unknown_55to64*avg_impress, na.rm = TRUE),
        female_45_54 = sum(female_45to54*avg_impress, na.rm = TRUE),
        unknown_25_34 = sum(unknown_25to34*avg_impress, na.rm = TRUE),
        unknown_35_44 = sum(unknown_35to44*avg_impress, na.rm = TRUE),
        unknown_18_24 = sum(unknown_18to24*avg_impress, na.rm = TRUE),
        female_18_24 = sum(female_18to24*avg_impress, na.rm = TRUE),
        unknown_45_54 = sum(unknown_45to54*avg_impress, na.rm = TRUE),
        female_25_34 = sum(female_25to34*avg_impress, na.rm = TRUE),
        male_18_24 = sum(male_18to24*avg_impress, na.rm = TRUE),
        female_35_44 = sum(female_35to44*avg_impress, na.rm = TRUE),
        male_25_34 = sum(male_25to34*avg_impress, na.rm = TRUE),
        unknown_65_plus = sum(unknown_65plus*avg_impress, na.rm = TRUE),
        female_13_17 = sum(female_13to17*avg_impress, na.rm = TRUE),
        male_13_17 = sum(male_13to17*avg_impress, na.rm = TRUE),
        male_total = sum(male_13_17,male_18_24, male_25_34, 
                         male_35_44, male_45_54,  male_55_64,  
                         male_65_plus, na.rm = TRUE),
        female_total = sum(female_13_17, female_18_24, 
                           female_25_34, female_35_44, 
                           female_45_54, female_55_64,  female_65_plus, na.rm = TRUE),
        unknown_total = sum(unknown_18_24, 
                            unknown_25_34, unknown_35_44, unknown_45_54,  
                            unknown_55_64,  unknown_65_plus, na.rm = TRUE),
        impressions_total = sum(avg_impress, na.rm = TRUE))
    
    meta_data <- meta_data %>%
      select(page_name, male_total, female_total, unknown_total, male_13_17, 
             male_18_24, male_25_34, male_35_44, male_45_54, male_55_64, male_65_plus, 
             female_13_17, female_18_24, female_25_34, female_35_44, female_45_54, female_55_64, 
             female_65_plus, unknown_18_24, unknown_25_34, unknown_35_44, 
             unknown_45_54, unknown_55_64, unknown_65_plus)
    
    meta_data
  })
  
  output$data_table <- renderTable({
    req(data())
    data()
  })
  
  observeEvent(input$cleanBtn, {
    output$downloadBtn1 <- downloadHandler(
      filename = "impressions_by_state.csv",
      content = function(file) {
        write_csv(meta_data1(), file)
      }
    )
    
    output$downloadBtn2 <- downloadHandler(
      filename = "spend_by_state.csv",
      content = function(file) {
        write_csv(meta_data2(), file)
      }
    )
    
    output$downloadBtn3 <- downloadHandler(
      filename = "impressions_by_demographic.csv",
      content = function(file) {
        write_csv(cleaned_data3(), file)
      }
    )
  })
}

# Run the app
shinyApp(ui, server)