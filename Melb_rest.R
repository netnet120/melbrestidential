# call necessary libraries
library(tidyverse)
library(tm)
library(ggplot2)
library(leaflet)
library(viridis)
library(sf)
library(shiny)
library(plotly)
library(htmltools)


# load all required data set

built_permit <- read_csv("building-permits.csv")

res_dwel <- read_csv("residential-dwellings.csv")

built_info <- read_csv("buildings-with-name-age-size-accessibility-and-bicycle-facilities.csv")


### DEP CODE REGION

# Data Wrangling
# only gather what we need 
built_permit <- built_permit%>%
  select(council_ref, issue_date, address, desc_of_works, permit_certificate_type)

# remove non-interested permit certificate type: Certificate of Final Inspection
built_permit <- built_permit%>%
  filter(permit_certificate_type != "Certificate of Final Inspection")

# maybe try tokenization tmr
colnames(built_info)

# clean data on built_info (discard data if no accessibility data available)
built_info <- built_info %>%
  drop_na(`Accessibility type`, `Accessibility type description`)

# remove has_shower column on built_info

built_info <- built_info%>%select(-`Has showers`)


### Things to be done change scale, some naming convention
## remove VIC in address

res_dwel <- res_dwel %>%
  mutate(`Building address` = str_replace(`Building address`, "VIC", ""))

built_permit <- built_permit %>%
  mutate(address = str_replace(address, "VIC", ""))

built_info <- built_info %>%
  mutate(`Street address` = str_replace(`Street address`, "VIC", ""))




# Normalised built_permit text
built_permit <- built_permit%>%mutate(lower_text = tolower(desc_of_works))


# remove unoccupied types
built_info <- built_info %>%
  filter(!str_detect(`Predominant space use`, "^Unoccupied"))

# combined retail into one 
built_info <- built_info %>%
  mutate(`Predominant space use` = ifelse(grepl("^Retail", `Predominant space use`)
                                          , "Retail", `Predominant space use`))

# Combined parking
built_info <- built_info %>%
  mutate(`Predominant space use` = ifelse(grepl("^Parking", `Predominant space use`)
                                          , "Parking", `Predominant space use`))


# Define keywords related to alterations and residential properties
alteration_keywords <- c("alterations", "additions", "conversion", "change of use", "fitout", "alteration",
                         "addition")

# Define keyword related to residential properties
residential_keyword <- "residential"

# Function to label entries
label_entry <- function(entry) {
  if (any(str_detect(tolower(entry), alteration_keywords)) && str_detect(tolower(entry),
                                                                         residential_keyword)) {
    return("Alterations to residential")
  } else {
    return("Other")
  }
}

# Apply function to label entries and create new column
built_permit <- built_permit%>%
  drop_na(issue_date, lower_text)
built_permit$label <- sapply(built_permit$lower_text, label_entry)


# Group by Census year and Predominant space use
year_diff <- built_info %>%
  group_by(`Census year`, `CLUE small area`, `Predominant space use`) %>%
  summarize(average_score = mean(`Accessibility rating`)) %>%
  arrange(`CLUE small area`, `Predominant space use`, `Census year`) %>%
  group_by(`Predominant space use`, `CLUE small area`) %>%
  mutate(
    percent_change_ac = ifelse(row_number() == 1 | `Census year` == min(`Census year`),
                               0,
                               ifelse(is.nan(average_score / lag(average_score)) | 
                                        is.infinite(average_score / lag(average_score)), 
                                      0, 
                                      ((average_score - lag(average_score)) / lag(average_score)) * 100)
    )
  )


# Calculate the percentage change in average score compared to the previous year

year_diff <- year_diff%>%
  filter(!(`Predominant space use` %in% c("Residential Apartment", "House/Townhouse", "Student Accommodation")))


# Calculate the total_dwel for each combination of Census year, CLUE small area, and Dwelling type
res_total <- res_dwel %>%
  group_by(`Census year`, `CLUE small area`, `Dwelling type`) %>%
  summarise(total_dwel = n())%>%
  arrange(`CLUE small area`, `Dwelling type`, `Census year`) %>%
  group_by(`Dwelling type`, `CLUE small area`) %>%
  mutate(
    percent_change_dwel = ifelse(row_number() == 1 | `Census year` == min(`Census year`),
                                 0,
                                 ((total_dwel - lag(total_dwel)) / lag(total_dwel)) * 100)
  ) 

# join data on census year and CLUE small area
corr_data <- inner_join(year_diff, res_total, by = c("Census year", "CLUE small area"))
# aggregate data
correlation_result <- corr_data %>%
  group_by(`CLUE small area`, `Census year`) %>%
  summarise(correlation_amenity_dwelling = cor(average_score, total_dwel),
            correlation_percent_change = cor(percent_change_ac, percent_change_dwel),
            .groups = "drop")

#Correlation scatter plot
cor_plot <- plot(correlation_result$correlation_amenity_dwelling, 
     correlation_result$correlation_percent_change, 
     xlab = "Correlation between amenity and dwelling changes", 
     ylab = "Correlation between percent changes",
     main = "Correlation between amenity and dwelling changes vs. Percent changes")

# total house (Count_house)
total_house <- res_dwel%>%group_by(`Census year`)%>%summarize(count_house =n())

# mutate percent change for dwelling
total_house <- total_house %>%
  mutate(percent_change_dwelling = ifelse(row_number() == 1, 0,
                                          ((count_house - lag(count_house)) / lag(count_house))))

# filter data for "Building permit" with "Alterations to residential" label
alteration_permit <- built_permit %>%
  filter(label == "Alterations to residential")

alteration_permit <- alteration_permit%>%
  filter(permit_certificate_type == "Building Permit")


alteration_permit <- alteration_permit %>%
  distinct(council_ref, .keep_all = TRUE)

# extract year from issue_date in DD/MM/YYYY format
alteration_permit <- alteration_permit %>%mutate(`Census year` = year(issue_date))

# get count for alternation
total_alter <- alteration_permit%>%
  group_by(`Census year`)%>%
  summarise(count_alt = n())%>%
  filter(`Census year` >= 2002 & `Census year` <= 2022)

# mutate percent change for alteration
total_alter <- total_alter%>%
  mutate(percent_change_alter = ifelse(row_number() == 1, 0,
                                       ((count_alt - lag(count_alt)) / lag(count_alt))))


# left join on census year
joined_data <- left_join(total_alter, total_house, by = "Census year")

# Perform linear regression
model <- lm(percent_change_dwelling ~ percent_change_alter, data =joined_data)


# Visualize the regression line
regress <- ggplot(joined_data, aes(x = percent_change_alter, y = percent_change_dwelling)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Regression analysis on Alteration of residential and dwelling",
       x = "Percent Change in Alteration", y = "Percent Change in Dwelling")
### END OF DEP REGION

### DEV REGION
# load additional dataset (geojson)
geojson <- st_read("small-areas-for-census-of-land-use-and-employment-clue.geojson")

# additional wrangling
# find CLUE Small zone for Total Alteration
built_permit <- built_permit%>%
  filter(label == "Alterations to residential")%>%
  mutate(year = year(issue_date))%>%
  select(year, address, label)%>%
  filter(year >= 2002)


# re-load data set
res_lookup <- read_csv("residential-dwellings.csv")
built_lookup <- read_csv("buildings-with-name-age-size-accessibility-and-bicycle-facilities.csv")

# sub select only target columns to look up
res_lookup <- res_lookup%>%select(`Building address`, `CLUE small area`)%>%
  rename(`Street address` = `Building address`)

# sub select only target columns to look up
built_lookup <- built_lookup%>%select(`Street address`, `CLUE small area`)

# append row
lookup_data <- rbind(res_lookup, built_lookup)

# find zone based on postal cod
postal_search<- lookup_data%>%
  mutate(postal_code = str_extract(`Street address`, "\\b\\d{4}\\b"))%>%
  filter(!is.na(postal_code))%>%
  distinct(postal_code, .keep_all = TRUE)

# find zone 
zone_find <- built_info %>%
  distinct(`Street address`, .keep_all = TRUE) %>%
  select(`Street address`, `CLUE small area`)

# Join `built_permit` with `zone_find` based on `address` and `Street address`
built_permit <- built_permit %>%
  left_join(zone_find, by = c("address" = "Street address")) 

# Rename the column to 'zone'
names(built_permit)[names(built_permit) == "CLUE small area"] <- "zone"

# extract postal code
built_permit <- built_permit%>%
  mutate(postal_code = str_extract(address, "(\\d{4}$)"))

# Join `built_permit` with `postal_search` to get CLUE small area for each postal code
built_permit <- built_permit %>%
  left_join(postal_search, by = "postal_code") %>%
  select(-postal_code)  # Remove the 'postal_code' column as it's no longer needed

# Replace NA values in 'zone' with corresponding 'CLUE small area' from 'postal_search'
built_permit <- built_permit %>%
  mutate(zone = ifelse(is.na(zone), `CLUE small area`, zone)) %>%
  select(-`CLUE small area`, -`Street address`, -address)  # Remove the 'CLUE small area' column as it's no longer needed




# final data for this one
alter_map <- built_permit %>%
  group_by(year, zone) %>%
  summarize(count = n())%>% filter(year <= 2022)


# create empty row
alter_df <- built_info%>%distinct(`CLUE small area`)

# define year vectorized element
years <- 2002:2022

# Create a data frame with all combinations of "CLUE small area" and years
all_combinations <- expand.grid(`CLUE small area` = unique(alter_df$`CLUE small area`), year = years)

# Merge the original dataset with the new combinations
alter_df <- merge(alter_df, all_combinations, by = "CLUE small area", all = TRUE)%>%
  rename(zone = `CLUE small area`)



# left join for df
complete_alter <- left_join(alter_df, alter_map, by = c("year" = "year", "zone" = "zone")) %>%
  # Replace missing counts with 0
  mutate(count = ifelse(is.na(count), 0, count))



## further wrangling on residential dwelling
res_extract <- res_dwel %>%
  select(`Census year`, `CLUE small area`,`Dwelling type`,`Dwelling number`) %>%
  group_by(`Census year`, `CLUE small area`, `Dwelling type`) %>%
  summarise(total_dwelling = n())%>%rename(year = `Census year`,
                                           zone = `CLUE small area`,
                                           type = `Dwelling type`,
                                           unit = `total_dwelling`)

# create an empty grid to join data set
complete_res <- expand.grid(
  year = 2002:2022,
  zone = unique(res_extract$zone),
  type = unique(res_extract$type)
)

# Left join with existing data
complete_res <- left_join(complete_res, res_extract, by = c("year", "zone", "type"))

# Replace NA values in the 'unit' column with 0
complete_res$unit[is.na(complete_res$unit)] <- 0


# accessibility transformation
access_extract <- built_info%>%select(`Census year`, `CLUE small area`, `Accessibility rating`)%>%
  group_by(`Census year`, `CLUE small area`)%>%
  summarise(average_accessibility = mean(`Accessibility rating`))%>%
  rename(year = `Census year`, zone = `CLUE small area`)

# create an empty grid to join data set
complete_acc <- expand.grid(
  year = 2002:2022,
  zone = unique(res_extract$zone))

# Left join with existing data
complete_acc <- left_join(complete_acc, access_extract, by = c("year", "zone"))

# Replace NA values in the 'unit' column with 0
complete_acc$average_accessibilityy[is.na(complete_acc$average_accessibility)] <- 0
glimpse(complete_acc)


# Create a static map from geojson
static_map <- st_as_sf(geojson, wkt = "geometry", crs = 4326)

# Preserve original complete_res data
original_complete_res <- complete_res
glimpse(original_complete_res)

# Compute the total units for complete_res
complete_res <- complete_res %>%
  group_by(year, zone) %>%
  summarise(
    total_unit = sum(unit, na.rm = TRUE),
    detail_1 = paste(
      "House/Townhouse: ", unit[type == "House/Townhouse"]),
    detail_2 = paste(
      "Residential Apartments: ", unit[type == "Residential Apartments"]),
    detail_3 = paste(
      "Student Apartments: ", unit[type == "Student Apartments"])
    ,
    .groups = 'drop'
  )



# Define global min and max values for color palettes
global_min_alter <- min(complete_alter$count, na.rm = TRUE)
global_max_alter <- max(complete_alter$count, na.rm = TRUE)
global_min_res <- min(complete_res$total_unit, na.rm = TRUE)
global_max_res <- max(complete_res$total_unit, na.rm = TRUE)
global_min_acc <- min(complete_acc$average_accessibility, na.rm = TRUE)
global_max_acc <- max(complete_acc$average_accessibility, na.rm = TRUE)

# UI
ui <- fluidPage(
  titlePanel("Urban development and how it affects residential dwellings: 
             City of Melbourne municipal case"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Alteration",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year", "Select Year:", 
                             min = min(complete_alter$year), 
                             max = max(complete_alter$year), 
                             value = min(complete_alter$year), 
                             step = 1,
                             animate = TRUE),
                 actionButton("reset_click", "Restore Value")
               ),
               mainPanel(
                 leafletOutput("map_alter"),
                 p("From total cases of building permit,
                   on 626 cases related to alteration and 
                   all of them are changed from other types 
                   of building uses to residential buildings 
                   and it accounted for less than 1% of 
                   all building permit cases from 2002 to 2022."),
                 plotlyOutput("histogram_alter"),
                 p("From visualisation, it shows that there is high alteration 
                   across city of Melbourne in 2002 with 105 and 
                   in 2006 with 86 cases and it become less.  
                   In zone-level, it illustrates that Melbourne (CBD) ,
                   North Melbourne, and Carlton have alteration report 
                   almost every year and with relatively high in volume compared 
                   to other zones. ")
               )
             )
    ),
    tabPanel("Residential Dwelling",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_res", "Select Year:", 
                             min = min(complete_res$year), 
                             max = max(complete_res$year), 
                             value = min(complete_res$year), 
                             step = 1,
                             animate = TRUE),
                 actionButton("reset_click_res", "Restore Value")
               ),
               mainPanel(
                 leafletOutput("map_res"),
                 p("By far, the residential dwellings are increasing overtime 
                   especially on House/Townhouse and residential apartments. "),
                 plotlyOutput("linegraph_res"),
                 p("The visualization illustrates the trends in different zones of the City of Melbourne from 2002 to 2022,
                 including Carlton, Docklands, East Melbourne, Kensington, CBD, 
                 the remainder of Melbourne, North Melbourne, Parkville, Port Melbourne, 
                 South Yarra, Southbank, and West Melbourne. Key trends identified are:"),
                 p("- A rise in the number of residential apartments in the CBD, 
                   the remainder of Melbourne, and Southbank."),
                 p("- Increasing housing trends in Docklands,
                   North Melbourne, West Melbourne, South Yarra, 
                   Kensington, East Melbourne, Carlton, and Parkville."),
                 p("- Increasing housing trends in Docklands, North Melbourne, West Melbourne,
                   South Yarra, Kensington, East Melbourne, Carlton, and Parkville."),
                 p("- Stable but small numbers of student apartments in the CBD, North Melbourne, Carlton, East Melbourne, and Parkville.")
                 
               )
             )
    ),
    tabPanel("Average Accessibility",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_acc", "Select Year:", 
                             min = min(complete_acc$year), 
                             max = max(complete_acc$year), 
                             value = min(complete_acc$year), 
                             step = 1,
                             animate = TRUE),
                 actionButton("reset_click_acc", "Restore Value")
               ),
               mainPanel(
                 leafletOutput("map_acc"),
                 p("By default, the City of Melbourne municipal has 
                   its criteria for accessibility rating, 
                   where 3 is the highest and 0 is the lowest for
                   each category. However, they did not maintain consistency 
                   in evaluation or even assess some categories at 
                   certain points. Consequently, the average accessibility 
                   rating seems to fluctuate and often results in 
                   low scores across the municipal area."),
                 plotlyOutput("linegraph_acc"),
                 p("When examining the zone-level data, 
                   it becomes evident that Southbank, 
                   Port Melbourne, Melbourne (CBD), and Melbourne (Reminder)
                   have significantly higher average scores (almost 2.0) 
                   compared to other zones, which have scores below 1.")
               )
             )
    ),
    tabPanel("Correlation & Regression Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Analysis Summation"), 
                 hr(), # Horizontal line for separation
                 p("The regression tests reveal that architectural alterations 
                   or modifications have no discernible impact on 
                   the functionality or aesthetics of residential dwellings. 
                   Consequently, both correlation and causality tests 
                   demonstrate that differences in accessibility among 
                   various building types do not directly influence residents' 
                   quality of life or correlate with residential dwellings."), 
               ),
               mainPanel(
                 plotOutput("correlation_plot"),
                 p("For the correlation test, two pairs were analyzed: 
                   average accessibility score vs. total dwellings, 
                   and percent change in average accessibility vs. 
                   percent change in residential dwellings. The correlation 
                   graph indicates no clear positive or negative trend, 
                   displaying only a vertical trend line with 
                   scattered residuals across both axes."),
                 plotOutput("regression_plot"),
                 p("The regression plot for the percent change 
                   in dwellings shows some correlation and 
                   a trend line in the left visualization. 
                   However, the statistical report indicates a p-value 
                   higher than the significance level, 
                   making it difficult to reject the null hypothesis. 
                   In simple terms, this suggests that changes 
                   in building use do not significantly impact 
                   residential dwellings.")
               )
             )
    )
  ),
  
  # References
  tags$footer(
    tags$p(HTML("<br><br><br>
           Data information <br>
           1. building-permit.csv retrieved from Building permits. - CoM Open Data Portal.
(2017, December 29). 
https://data.melbourne.vic.gov.au/explore/dataset/building-permits/information/<br>
           Size: 160,000 x 10 <br>
           Type: Tabular <br>
           2. buildings-with-name-age-size-accessibility-and
           -bicycle-facilities.csv retrieved from 
           Building information. - CoM Open Data Portal. (2014a, June 23). 
https://data.melbourne.vic.gov.au/explore/dataset/buildings-with-name-age-size
accessibility-and-bicycle-facilities/information/<br>
           Size: 270,000 x 20 <br>
           Type: Tabular <br>
           3. residential-dwellings.csv retrieved from Residential dwellings. 
- CoM Open Data Portal. (2015, October 12). 
https://data.melbourne.vic.gov.au/explore/dataset/residential-dwellings/information/<br>
           Size: 190,000 x 11 <br>
           Type: Tabular <br>
           4. small-areas-for-census-of-land-use-and-employment-clue.geojson 
           retrieved from
Small areas for census of Land Use and Employment (CLUE). - CoM Open Data 
Portal. (2014b, May 2). https://data.melbourne.vic.gov.au/explore/dataset/small-areas
for-census-of-land-use-and-employment-clue/information/<br>
           Size: 13 x 5 <br>
           Type: Spatial <br>
           "))
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data based on selected year
  filtered_alter_data <- reactive({
    complete_alter %>% filter(year == input$year)
  })
  
  
  filtered_res_data <- reactive({
    complete_res %>% filter(year == input$year_res)
  })
  filtered_acc_data <- reactive({
    complete_acc %>% filter(year == input$year_acc)
  })
  
  # Merge filtered data with geojson
  merged_alter_data <- reactive({
    geojson %>% 
      left_join(filtered_alter_data(), by = c("featurenam" = "zone")) %>%
      mutate(count = ifelse(is.na(count), 0, count))
  })
  
  merged_res_data <- reactive({
    geojson %>% 
      left_join(filtered_res_data(), by = c("featurenam" = "zone")) %>%
      mutate(unit = ifelse(is.na(total_unit), 0, total_unit))
  })
  
  merged_acc_data <- reactive({
    geojson %>% 
      left_join(filtered_acc_data(), by = c("featurenam" = "zone")) %>%
      mutate(average_accessibility = ifelse(is.na(average_accessibility),
                                            0,
                                            average_accessibility))
  })
  
  # Define color palettes
  pal_alter <- reactive({
    colorNumeric(palette = "viridis", domain = c(global_min_alter, 
                                                 global_max_alter))
  })
  
  pal_res <- reactive({
    colorNumeric(palette = "viridis", domain = c(global_min_res, 
                                                 global_max_res))
  })
  
  pal_acc <- reactive({
    colorNumeric(palette = "viridis", domain = c(global_min_acc, 
                                                 global_max_acc))
  })
  
  # Reactive values to store the clicked zones
  clicked_zone_alter <- reactiveVal(NULL)
  clicked_zone_res <- reactiveVal(NULL)
  clicked_zone_acc <- reactiveVal(NULL)
  
  # Render the leaflet map for alteration
  output$map_alter <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = st_as_sf(merged_alter_data(),
                                  wkt = "geometry",
                                  crs = 4326),
                  fillColor = ~pal_alter()(count),
                  fillOpacity = 0.7,
                  color = "#BDBDC3",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~featurenam,
                  label = ~paste(htmlEscape(featurenam)),
                  popup = ~paste("Zone: ", 
                                 htmlEscape(featurenam),
                                 "<br>",
                                 "Year: ",
                                 htmlEscape(year),
                                 "<br>",
                                 "Total Alteration: ",
                                 htmlEscape(count)),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "yellow",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  )) %>%
      addLegend("bottomright", pal = pal_alter(),
                values = c(global_min_alter, global_max_alter),
                title = "Total Alteration",
                opacity = 0.7)
  })
  
  # Render the leaflet map for residential dwelling
  output$map_res <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = st_as_sf(merged_res_data(), 
                                  wkt = "geometry",
                                  crs = 4326),
                  fillColor = ~pal_res()(total_unit),
                  fillOpacity = 0.7,
                  color = "#BDBDC3",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~featurenam,
                  label = ~paste(htmlEscape(featurenam)),
                  popup = ~paste("Zone: ", 
                                 htmlEscape(featurenam),
                                 "<br>",
                                 "Year: ",
                                 htmlEscape(year),
                                 "<br>",
                                 "Total Unit: ", 
                                 htmlEscape(total_unit),
                                 "<br>",
                                 htmlEscape(detail_1),
                                 "<br>",
                                 htmlEscape(detail_2),
                                 "<br>",
                                 htmlEscape(detail_3)),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "yellow",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  )) %>%
      addLegend("bottomright", pal = pal_res(), 
                values = c(global_min_res,
                           global_max_res),
                title = "Total Unit",
                opacity = 0.7)
  })
  
  # Render the leaflet map for accessibility
  output$map_acc <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = st_as_sf(merged_acc_data(),
                                  wkt = "geometry", 
                                  crs = 4326),
                  fillColor = ~pal_acc()(average_accessibility),
                  fillOpacity = 0.7,
                  color = "#BDBDC3",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~featurenam,
                  label = ~paste(htmlEscape(featurenam)),
                  popup = ~paste("Zone: ", 
                                 htmlEscape(featurenam),
                                 "<br>",
                                 "Year: ",
                                 htmlEscape(year),
                                 "<br>",
                                 "Average accessibility: ", 
                                 htmlEscape(average_accessibility)),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "yellow",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  )) %>%
      addLegend("bottomright", pal = pal_acc(), 
                values = c(global_min_acc, 
                           global_max_acc),
                title = "Average accessibility",
                opacity = 0.7)
  })
  
  # Render the histogram for alteration
  output$histogram_alter <- renderPlotly({
    if (is.null(clicked_zone_alter())) {
      # Plot total count by year
      yearly_data <- complete_alter %>%
        group_by(year) %>%
        summarise(total_count = sum(count, 
                                    na.rm = TRUE))
      
      # Create plotly object
      plot_ly(yearly_data, 
              x = ~factor(year),
              y = ~total_count,
              type = 'bar', 
              marker = list(color = 'blue'),
              hovertemplate = "Year: %{x}<br>Alteration: %{y}<extra></extra>") %>%
        layout(title = "Total alteration by Year",
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Total alteration"))
    } else {
      # Plot count for the clicked zone
      zone_data <- complete_alter %>% filter(zone == clicked_zone_alter())
      
      # Create plotly object
      plot_ly(zone_data,
              x = ~factor(year),
              y = ~count, type = 'bar',
              marker = list(color = 'blue'),
              hovertemplate = "Year: %{x}<br>Alteration: %{y}<extra></extra>") %>%
        layout(title = paste("Total alteration by Year in ", 
                             clicked_zone_alter()), 
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Total alteration"))
    }
  })
  
  # Render the line graph for residential dwelling
  output$linegraph_res <- renderPlotly({
    if (is.null(clicked_zone_res())) {
      # Plot total unit by year and type
      yearly_data <- original_complete_res %>%
        group_by(year, type) %>%
        summarise(total_unit = sum(unit, 
                                   na.rm = TRUE),
                  .groups = 'drop')
      
      # Create plotly object
      plot_ly(yearly_data,
              x = ~factor(year),
              y = ~total_unit,
              color = ~type, 
              type = 'scatter', 
              mode = 'lines+markers',
              hovertemplate = "Year: %{x}<br>Unit: %{y}<extra></extra>") %>%
        layout(title = "Total residential dwellings by Year and Type", 
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Total Unit"))
    } else {
      # Plot unit for the clicked zone by year and type
      zone_data <- original_complete_res %>% filter(zone == clicked_zone_res())
      
      # Create plotly object
      plot_ly(zone_data, x = ~factor(year),
              y = ~unit, color = ~type, 
              type = 'scatter',
              mode = 'lines+markers',
              hovertemplate = "Year: %{x}<br>Unit: %{y}<extra></extra>") %>%
        layout(title = paste("Residential dwellings by Year and Type in ",
                             clicked_zone_res()), 
               xaxis = list(title = "Year"), 
               yaxis = list(title = "Total Unit"))
    }
  })
  
  # Render the line graph for accessibility
  output$linegraph_acc <- renderPlotly({
    if (is.null(clicked_zone_acc())) {
      # Plot total accessibility by year
      yearly_data <- complete_acc %>%
        group_by(year) %>%
        summarise(total_accessibility = mean(average_accessibility,
                                             na.rm = TRUE))
      
      # Create plotly object
      plot_ly(yearly_data, 
              x = ~factor(year), 
              y = ~total_accessibility, 
              type = 'scatter',
              mode = 'lines+markers',
              hovertemplate = "Year: %{x}<br>Accessibility: %{y}<extra></extra>") %>%
        layout(title = "Average accessibility by Year",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Average accessibility"))
    } else {
      # Plot accessibility for the clicked zone
      zone_data <- complete_acc %>% filter(zone == clicked_zone_acc())
      
      # Create plotly object
      plot_ly(zone_data,
              x = ~factor(year),
              y = ~average_accessibility, 
              type = 'scatter', 
              mode = 'lines+markers',
              hovertemplate = "Year: %{x}<br>Accessibility: %{y}<extra></extra>") %>%
        layout(title = paste("Average accessibility by Year in ",
                             clicked_zone_acc()),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Average accessibility"))
    }
  })
  
  # Observe click events on the map for alteration
  observeEvent(input$map_alter_shape_click, {
    clicked_zone_alter(input$map_alter_shape_click$id)
  })
  
  # Observe click events on the map for residential dwelling
  observeEvent(input$map_res_shape_click, {
    clicked_zone_res(input$map_res_shape_click$id)
  })
  
  # Observe click events on the map for accessibility
  observeEvent(input$map_acc_shape_click, {
    clicked_zone_acc(input$map_acc_shape_click$id)
  })
  
  # Reset clicked_zone_alter to NULL when Reset Click button is clicked for alteration
  observeEvent(input$reset_click, {
    clicked_zone_alter(NULL)
  })
  
  # Reset clicked_zone_res to NULL when Reset Click button is clicked for residential dwelling
  observeEvent(input$reset_click_res, {
    clicked_zone_res(NULL)
  })
  
  # Reset clicked_zone_acc to NULL when Reset Click button is clicked for accessibility
  observeEvent(input$reset_click_acc, {
    clicked_zone_acc(NULL)
  })
 

  # Render the correlation plot 
  output$correlation_plot <- renderPlot({
      plot(correlation_result$correlation_amenity_dwelling, 
           correlation_result$correlation_percent_change, 
           xlab = "Correlation between amenity and dwelling changes", 
           ylab = "Correlation between percent changes",
           main = "Correlation between amenity and dwelling changes vs. Percent changes")
  })
  
  # Render the regression plot
  output$regression_plot <- renderPlot({
      ggplot(joined_data, aes(x = percent_change_alter, y = percent_change_dwelling)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(title = "Regression analysis on Alteration of residential and dwelling",
             x = "Percent Change in Alteration", y = "Percent Change in Dwelling")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

