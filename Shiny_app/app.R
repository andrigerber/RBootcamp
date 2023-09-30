library(shiny)
library(tidyverse)
library(viridis)
library(gridExtra)
library(ggmap)
library(ggplot2)
library(plotly)


# listings (3 datasets)
listings_dec <- read.csv("listings_december.csv.gz")

listings_dec$period <- 1
listings_mar <-
  read.csv("listings_march.csv.gz")

listings_mar$period <- 2
listings_jun <-
  read.csv("listings_june.csv.gz")

listings_jun$period <- 3

# join
listings <- rbind(listings_dec, listings_mar, listings_jun)

# price taking $ , away for numeric
listings$price <- gsub("\\$", "", listings$price)
listings$price <- gsub(",", "", listings$price)
listings$price <- as.numeric(listings$price)

# changeing to date format
listings$host_since <- as.Date(listings$host_since)
listings$host_since <- as.Date(listings$host_since)

# acceptance taking % away
listings$host_acceptance_rate <-
  gsub("\\%", "", listings$host_acceptance_rate)
listings$host_response_rate <-
  gsub("\\%", "", listings$host_response_rate)

# add swiss franc
listings$price_swiss_franc <- listings$price * 0.88

# add amenities groups
listings <- listings %>%
  mutate(
    showergel_or_shampoo = grepl(
      "([S-s]hower\\s*[-]*[G-g]el)|([S-s]hampoo)",
      amenities,
      ignore.case = T
    )
  ) %>%
  mutate(wifi = grepl("[W-w]ifi", amenities, ignore.case = T)) %>%
  mutate(freeparking = grepl("[F-f]ree\\s*[-]*[P-p]arking", amenities, ignore.case = T)) %>%
  mutate(pool = grepl("([P-p]ool)|([J-j]acuzzi)", amenities, ignore.case = T)) %>%
  mutate(dishwasher = grepl("[D-d]ish\\s*washer", amenities, ignore.case = T)) %>%
  mutate(washer = grepl("[W-w]asher", amenities, ignore.case = T)) %>%
  mutate(selfcheckin = grepl("[S-s]elf\\s*check[-]*\\s*in", amenities, ignore.case = T)) %>%
  mutate(petsallowed = grepl("[P-p]ets\\s*allowed", amenities, ignore.case = T)) %>%
  mutate(refrigerator = grepl("[R-r]efrigerator", amenities, ignore.case = T)) %>%
  mutate(airconditioner = grepl("[A-a]ir\\s*conditioner", amenities, ignore.case = T)) %>%
  ungroup()

# add a column to sum the amenities for each row
listings$row_sums <-
  rowSums(listings[, c(
    "showergel_or_shampoo",
    "wifi",
    "freeparking",
    "pool",
    "dishwasher",
    "washer",
    "selfcheckin",
    "petsallowed",
    "refrigerator",
    "airconditioner"
  )])

# get the map without text on the map:
# Google API Key
api_key <- "AIzaSyB1YEwTEBaMAnHj8nMmuLnvIFwKcjxO9QQ"
register_google(key = "AIzaSyB1YEwTEBaMAnHj8nMmuLnvIFwKcjxO9QQ")

# Setting city to Geneva
city <- "Geneva"

city_location <- geocode(city)

# Getting a map of Geneva (zoom 11, without text)
city_map <-
  get_googlemap(
    center = c(lon = city_location$lon, lat = city_location$lat),
    zoom = 11,
    key = api_key,
    style = "feature:all|element:labels|visibility:off"
  )

# UI Configuration
ui <- fluidPage(titlePanel("Airbnb Data Visualization of Geneva"),
                
                sidebarLayout(
                  sidebarPanel(
                    # Price, room type, neighbourhood and property type filters
                    sliderInput(
                      "price_range",
                      "Price Range:",
                      min = 0,
                      max = 1000,
                      value = c(0, 1000)
                    ),
                    selectInput(
                      "room_type",
                      "Room Type:",
                      choices = unique(listings$room_type),
                      selected = "All",
                      multiple = TRUE
                    ),
                    selectInput(
                      "neighbourhood",
                      "Neighbourhood:",
                      choices = unique(listings$neighbourhood_cleansed),
                      selected = "All",
                      multiple = TRUE
                    ),
                    selectInput(
                      "property_type",
                      "Property Type:",
                      choices = unique(listings$property_type),
                      selected = "All",
                      multiple = TRUE
                    )
                  ),
                  
                  mainPanel(tabsetPanel(
                    # Tab for Plots
                    tabPanel(
                      "Plots",
                      fluidRow(column(6, plotOutput("histPlot")),
                               column(6, plotOutput("plot1"))),
                      fluidRow(column(6, plotOutput("plot2")),
                               column(6, plotOutput("plot3"))),
                      fluidRow(column(6, plotOutput("amenitiesPlot")))
                    ),
                    # Tab for map visualization
                    tabPanel("Location",
                             plotlyOutput("geoPlot"))
                  ))
                ))

# Server configuration
server <- function(input, output) {
  # Filtering data based on user input
  filtered_data <- reactive({
    listings %>%
      filter(
        price_swiss_franc >= input$price_range[1] &
          price_swiss_franc <= input$price_range[2],
        room_type %in% input$room_type,
        neighbourhood_cleansed %in% input$neighbourhood,
        property_type %in% input$property_type
      )
  })
  # Price distribution (histogram)
  output$histPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = price_swiss_franc)) +
      geom_histogram() +
      xlab("Price (CHF)") +
      ylab("Count") +
      ggtitle("Listings Price Distribution")
  })
  # Price by neighbourhoods
  output$plot1 <- renderPlot({
    sorted_neighbourhoods <- filtered_data() %>%
      group_by(neighbourhood_cleansed) %>%
      summarize(median_price_swiss_franc = median(price_swiss_franc, na.rm = TRUE)) %>%
      arrange(desc(median_price_swiss_franc))
    
    sorted_neighbourhood_names <-
      sorted_neighbourhoods$neighbourhood_cleansed
    
    palette <- viridis(length(sorted_neighbourhood_names))
    my_colors <-
      scale_color_manual(values = setNames(palette, sorted_neighbourhood_names))
    
    ggplot(data = filtered_data(),
           aes(
             x = factor(neighbourhood_cleansed, levels = sorted_neighbourhood_names),
             y = price_swiss_franc,
             color = neighbourhood_cleansed
           )) +
      geom_boxplot() +
      my_colors +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  +
      xlab("Neighbourhood") +
      ylab("Price (CHF)") +
      ggtitle("Price by Neighbourhoods")
  })
  # Price by property type
  output$plot2 <- renderPlot({
    sorted_price_swiss_franc <- filtered_data() %>%
      group_by(property_type) %>%
      summarise(median_price_swiss_franc = median(price_swiss_franc, na.rm = TRUE)) %>%
      arrange(desc(median_price_swiss_franc))
    
    sorted_property_type_names <-
      sorted_price_swiss_franc$property_type
    
    palette <- viridis(length(sorted_property_type_names))
    my_colors <-
      scale_color_manual(values = setNames(palette, sorted_property_type_names))
    
    ggplot(data = filtered_data(),
           aes(
             x = factor(property_type, levels = sorted_property_type_names),
             y = price_swiss_franc,
             color = property_type
           )) +
      geom_boxplot() +
      my_colors +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      xlab("Property type") +
      ylab("Price (CHF)") +
      ggtitle("Price by Property Type")
  })
  # Price by room type
  output$plot3 <- renderPlot({
    sorted_price <- filtered_data() %>%
      group_by(room_type) %>%
      summarise(median_price = median(price, na.rm = TRUE)) %>%
      arrange(desc(median_price))
    
    sorted_room_types <- sorted_price$room_type
    
    palette <- viridis(length(sorted_room_types))
    my_colors <-
      scale_color_manual(values = setNames(palette, sorted_room_types))
    
    ggplot(data = filtered_data(),
           aes(
             x = factor(room_type, levels = sorted_room_types),
             y = price,
             color = room_type
           )) +
      geom_boxplot() +
      my_colors +
      geom_jitter(alpha = 0.1, position = position_jitter(width = 0.1)) +
      theme(axis.text.y = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      xlab("Room type") +
      ylab("Price (CHF)") +
      ggtitle("Price by Room Type") +
      coord_flip()
  })
  # Distribution of number of amenities
  output$amenitiesPlot <- renderPlot({
    # Check if 'row_sums' column exists in filtered_data
    req("row_sums" %in% names(filtered_data()))
    
    # Check if the data isn't empty, otherwise it won't show up
    # if we don't do that an error will show up because of missingness
    req(nrow(filtered_data()) > 0)
    
    ggplot(data = filtered_data(), aes(x = "Amenities", y = row_sums)) +
      geom_boxplot() +
      geom_jitter(aes(x = "Amenities"),
                  alpha = 0.1,
                  position = position_jitter(width = 0.1)) +
      xlab("") +
      ylab("Number of amenities") +
      ggtitle("Distribution of Number of Amenities")
  })
  
  # map with listings
  output$geoPlot <- renderPlotly({
    
    map_geneva <- ggmap(city_map) +
      geom_point(
        data = filtered_data(),
        aes(x = longitude, y = latitude),
        color = "blue",
        alpha = 0.3
      ) +
      xlab("Longitude") +
      ylab("Latitude")
    
    ggplotly(map_geneva)
  })
}

shinyApp(ui = ui, server = server)
