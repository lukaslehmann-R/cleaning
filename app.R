library(shiny)
library(tidyverse)
library(jsonlite)
library(janitor)
library(zip)

#Version: Dec 12, 2023

extract_states <- function(json_data) {
  parsed_data <- fromJSON(json_data)
  state_names <- names(parsed_data)
  percentages <- unlist(parsed_data)
  return(data.frame(state_names, percentages))
}

# Define UI
ui <- fluidPage(
  titlePanel("Meta Ads Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      actionButton("processData", "Process Data")
    ),
    mainPanel(
      downloadButton("downloadData", "Download Processed Data as ZIP")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Placeholder for the processed data
  processed_data <- reactiveValues()
  
  observeEvent(input$processData, {
    req(input$fileInput)
    
    # Initialize progress bar
    withProgress(message = 'Processing data...', value = 0, {
      # Increment progress
      incProgress(0.1)
      
      # Load and process the data
      meta_data <- read_csv(input$fileInput$datapath)
      incProgress(0.3) # Increment progress
      
      meta_data$delivery_by_region <- gsub("percentage", "", as.character(meta_data$delivery_by_region))
      meta_data$delivery_by_region <- gsub("region", "", as.character(meta_data$delivery_by_region))
      meta_data$delivery_by_region <- gsub('""', "", as.character(meta_data$delivery_by_region))
      meta_data$delivery_by_region <- gsub(':"', '"', as.character(meta_data$delivery_by_region))
      meta_data$delivery_by_region <- gsub(',:', ': ', as.character(meta_data$delivery_by_region))
      meta_data$delivery_by_region <- gsub("\\},\\{", ', ', as.character(meta_data$delivery_by_region))
      
      # Function to extract state names and percentages
      extract_states <- function(json_data) {
        parsed_data <- fromJSON(json_data)
        state_names <- names(parsed_data)
        percentages <- unlist(parsed_data)
        return(data.frame(state_names, percentages))
      }
      
      meta_data1 <- meta_data %>%
        filter(delivery_by_region != '') %>%
        mutate(json_data = delivery_by_region)
      
      reshaped_meta_data <- meta_data1 %>%
        rowwise() %>%
        mutate(parsed_data = list(extract_states(json_data))) %>%
        unnest(parsed_data)
      
      reshaped_meta_data1 <- reshaped_meta_data %>%
        distinct()
      
      reshaped_meta_data2 <- reshaped_meta_data1 %>%
        pivot_wider(names_from = state_names, values_from = percentages, values_fn = list)
      
      
      meta_data3 <- reshaped_meta_data2 %>%
        rename(DC = `Washington, District of Columbia`) %>%
        clean_names()
      
      reshaped_meta_data3 <- meta_data3 %>%
        mutate(alabama = sapply(alabama, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(alabama = sapply(alabama, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(alaska = sapply(alaska, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(arizona = sapply(arizona, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(arkansas = sapply(arkansas, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(california = sapply(california, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(colorado = sapply(colorado, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(connecticut = sapply(connecticut, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(delaware = sapply(delaware, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(florida = sapply(florida, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(georgia = sapply(georgia, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(hawaii = sapply(hawaii, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(idaho = sapply(idaho, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(illinois = sapply(illinois, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(indiana = sapply(indiana, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(iowa = sapply(iowa, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(kansas = sapply(kansas, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(kentucky = sapply(kentucky, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(louisiana = sapply(louisiana, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(maine = sapply(maine, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(maryland = sapply(maryland, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(massachusetts = sapply(massachusetts, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(michigan = sapply(michigan, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(minnesota = sapply(minnesota, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(mississippi = sapply(mississippi, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(missouri = sapply(missouri, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(montana = sapply(montana, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(nebraska = sapply(nebraska, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(nevada = sapply(nevada, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(new_hampshire = sapply(new_hampshire, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(new_jersey = sapply(new_jersey, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(new_mexico = sapply(new_mexico, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(new_york = sapply(new_york, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(north_carolina = sapply(north_carolina, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(north_dakota = sapply(north_dakota, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(ohio = sapply(ohio, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(oklahoma = sapply(oklahoma, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(oregon = sapply(oregon, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(pennsylvania = sapply(pennsylvania, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(rhode_island = sapply(rhode_island, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(south_carolina = sapply(south_carolina, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(south_dakota = sapply(south_dakota, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(tennessee = sapply(tennessee, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(texas = sapply(texas, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(utah = sapply(utah, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(vermont = sapply(vermont, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(virginia = sapply(virginia, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(washington = sapply(washington, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(west_virginia = sapply(west_virginia, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(wisconsin = sapply(wisconsin, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(wyoming = sapply(wyoming, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(dc = sapply(dc, function(x) ifelse(is.null(x), 0, x)))
      
      meta_data4 <- reshaped_meta_data3 %>%
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
      
      incProgress(0.4)
      
      meta_data5 <- meta_data4 %>%
        separate(col = impressions, into = c("lower_impress", "upper_impress"), sep = ",") %>%
        separate(col = spend, into = c("lower_spend", "upper_spend"), sep = ",")
      
      meta_data5$lower_impress <- gsub("lower_bound: ", '', as.character(meta_data5$lower_impress))
      meta_data5$upper_impress <- gsub("upper_bound: ", '', as.character(meta_data5$upper_impress))
      meta_data5$lower_spend <- gsub("lower_bound: ", '', as.character(meta_data5$lower_spend))
      meta_data5$upper_spend <- gsub("upper_bound: ", '', as.character(meta_data5$upper_spend))
      
      meta_data6 <- meta_data5 %>%
        transform(lower_impress = as.numeric(lower_impress)) %>%
        transform(upper_impress = as.numeric(upper_impress)) %>%
        transform(lower_spend = as.numeric(lower_spend)) %>%
        transform(upper_spend = as.numeric(upper_spend)) %>%
        mutate(avg_impress = (lower_impress + upper_impress)/2) %>%
        mutate(avg_spend = (lower_spend + upper_spend)/2)
      
      impressions_by_state <- meta_data6 %>%
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
      
      spending_by_state <- meta_data6 %>%
        group_by(page_name) %>%
        summarise(
          total_spend = sum(avg_spend, na.rm = TRUE),
          alabama = sum(alabama * avg_spend, na.rm = TRUE),
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
      
      options(scipen = 999)
      
      meta_dem <- meta_data
      
      incProgress(0.5)
      
      meta_dem$demographic_distribution <- gsub("percentage", "", as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub("gender", "", as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub("age", "", as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub('""', "", as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub(':"', '"', as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub(',:', ': ', as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub("\\},\\{", ', ', as.character(meta_dem$demographic_distribution))
      meta_dem$demographic_distribution <- gsub('","', "", as.character(meta_dem$demographic_distribution))
      
      meta_dem1 <- meta_dem %>%
        filter(demographic_distribution != '') %>%
        mutate(json_data = demographic_distribution)
      
      reshaped_meta_dem <- meta_dem1 %>%
        rowwise() %>%
        mutate(parsed_data = list(extract_states(json_data))) %>%
        unnest(parsed_data) %>%
        pivot_wider(names_from = state_names, values_from = percentages, values_fn = list)
      
      reshaped_meta_dem2 <- reshaped_meta_dem %>%
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
      
      
      reshaped_meta_dem3 <- reshaped_meta_dem2 %>%
        mutate(male_45to54 = sapply(male_45to54, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_65plus = sapply(male_65plus, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_65plus = sapply(female_65plus, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_55to64 = sapply(male_55to64, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_35to44 = sapply(male_35to44, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_55to64 = sapply(female_55to64, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_55to64 = sapply(unknown_55to64, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_45to54 = sapply(female_45to54, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_25to34 = sapply(unknown_25to34, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_35to44 = sapply(unknown_35to44, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_18to24 = sapply(unknown_18to24, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_18to24 = sapply(female_18to24, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_45to54 = sapply(unknown_45to54, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_25to34 = sapply(female_25to34, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_18to24 = sapply(male_18to24, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_35to44 = sapply(female_35to44, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_25to34 = sapply(male_25to34, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(unknown_65plus = sapply(unknown_65plus, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(female_13to17 = sapply(female_13to17, function(x) ifelse(is.null(x), 0, x))) %>%
        mutate(male_13to17 = sapply(male_13to17, function(x) ifelse(is.null(x), 0, x)))
      
      meta_dem6 <- reshaped_meta_dem3 %>%
        separate(col = impressions, into = c("lower_impress", "upper_impress"), sep = ",") %>%
        separate(col = spend, into = c("lower_spend", "upper_spend"), sep = ",")
      
      meta_dem6$lower_impress <- gsub("lower_bound: ", '', as.character(meta_dem6$lower_impress))
      meta_dem6$upper_impress <- gsub("upper_bound: ", '', as.character(meta_dem6$upper_impress))
      meta_dem6$lower_spend <- gsub("lower_bound: ", '', as.character(meta_dem6$lower_spend))
      meta_dem6$upper_spend <- gsub("upper_bound: ", '', as.character(meta_dem6$upper_spend))
      
      incProgress(0.7)
      
      meta_dem7 <- meta_dem6 %>%
        transform(lower_impress = as.numeric(lower_impress)) %>%
        transform(upper_impress = as.numeric(upper_impress)) %>%
        transform(lower_spend = as.numeric(lower_spend)) %>%
        transform(upper_spend = as.numeric(upper_spend)) %>%
        mutate(avg_impress = (lower_impress + upper_impress)/2) %>%
        mutate(avg_spend = (lower_spend + upper_spend)/2)
      
      
      meta_dem8 <- meta_dem7 %>%
        group_by(page_name) %>%
        summarise(
          total_impressions = sum(avg_impress, na.rm = TRUE),
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
          # unknown_13_17 = sum(unknown_13to17*avg_impress, na.rm = TRUE),
          male_total = sum(male_13_17,male_18_24, male_25_34, 
                           male_35_44, male_45_54,  male_55_64,  
                           male_65_plus, na.rm = TRUE),
          female_total = sum(female_13_17, female_18_24, 
                             female_25_34, female_35_44, 
                             female_45_54, female_55_64,  female_65_plus, na.rm = TRUE),
          unknown_total = sum(unknown_18_24, 
                              unknown_25_34, unknown_35_44, unknown_45_54,  
                              unknown_55_64,  unknown_65_plus, na.rm = TRUE))
      
      
      impressions_by_dem <- meta_dem8 %>%
        select(page_name, total_impressions, male_total, female_total, unknown_total, male_13_17, 
               male_18_24, male_25_34, male_35_44, male_45_54, male_55_64, male_65_plus, 
               female_13_17, female_18_24, female_25_34, female_35_44, female_45_54, female_55_64, 
               female_65_plus,unknown_18_24, unknown_25_34, 
               unknown_35_44, unknown_45_54, unknown_55_64, unknown_65_plus)
      
      # Your data processing script goes here
      # ...
      # At the end of your script, you should have three dataframes:
      # impressions_by_dem, impressions_by_state, spending_by_state
      # Below are placeholders for these dataframes
      processed_data$impressions_by_dem <- impressions_by_dem # Replace with actual processing result
      processed_data$impressions_by_state <- impressions_by_state # Replace with actual processing result
      processed_data$spending_by_state <- spending_by_state # Replace with actual processing result
      
      incProgress(0.8) # Increment progress
    })
  })
  
  # Create a function to generate a temporary ZIP file with all CSVs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("meta-ads-data-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory
      tempDir <- tempdir()
      # Write dataframes to CSV files in the temporary directory
      write.csv(processed_data$impressions_by_dem, file.path(tempDir, "impressions_by_dem.csv"), row.names = FALSE)
      write.csv(processed_data$impressions_by_state, file.path(tempDir, "impressions_by_state.csv"), row.names = FALSE)
      write.csv(processed_data$spending_by_state, file.path(tempDir, "spending_by_state.csv"), row.names = FALSE)
      # Create a ZIP file containing all the CSVs
      zip::zipr(zipfile = file, files = list.files(tempDir, full.names = TRUE))
    }
  )
}

# Run the App
shinyApp(ui, server)
