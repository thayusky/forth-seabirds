install.packages(c('shiny','bslib','leaflet','DT','dplyr','tidyr','readr','lubridate','ggplot2','knitr','quarto','httr2'))
library(httr2); library(dplyr); library(readr); library(lubridate)

`%||%` <- function(a, b) if (is.null(a)) b else a

key <- "93c8cc03-30cf-474e-8173-dbe5a22a5d28"   # JNCC Seabirds at Sea (ESAS)
wkt <- "POLYGON((-3.4 55.9,-1.8 55.9,-1.8 56.4,-3.4 56.4,-3.4 55.9))"  # Firth of Forth box

out <- list(); offset <- 0
repeat {
  r <- request("https://api.gbif.org/v1/occurrence/search") |>
    req_url_query(datasetKey = key, geometry = wkt, limit = 1000, offset = offset) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()
  
  if (length(r$results) == 0) break
  
  out[[length(out) + 1]] <- bind_rows(lapply(r$results, function(x) tibble(
    date    = as.character(x$eventDate %||% NA),
    lat     = as.numeric(x$decimalLatitude %||% NA),
    lon     = as.numeric(x$decimalLongitude %||% NA),
    species = as.character(x$vernacularName %||% x$species %||% NA),
    latin   = as.character(x$species %||% NA),
    count   = as.numeric(x$individualCount %||% 1)
  )))
  
  offset <- offset + 300
  cat("Downloaded", min(offset, r$count), "of", r$count, "\n")
  if (isTRUE(r$endOfRecords)) break
}

# Tidy: proper dates, year and month, drop rows with no position or species
birds <- bind_rows(out) |>
  mutate(date = as.Date(substr(date, 1, 10)),
         year = year(date), month = month(date)) |>
  filter(!is.na(lat), !is.na(lon), !is.na(species))

dir.create("data", showWarnings = FALSE)
write_csv(birds, "data/forth_birds.csv")
cat("Saved", nrow(birds), "records to data/forth_birds.csv\n")

birds <- readr::read_csv("data/forth_birds.csv")
head(birds)
range(birds$year, na.rm = TRUE)                           # roughly 1979 to 2000
sort(table(birds$species), decreasing = TRUE)[1:10]       # top 10 species
plot(birds$lon, birds$lat, pch = ".", main = "Sightings") # should outline the Forth


library(leaflet); library(readr); library(dplyr)

birds <- read_csv("data/forth_birds.csv")

leaflet(slice_sample(birds, n = 5000),
        options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles("CartoDB.Positron") |>
  addCircleMarkers(~lon, ~lat, radius = 3, stroke = FALSE,
                   fillOpacity = 0.4, popup = ~paste(species, count))

file.exists("data/forth_birds.csv")    # should be TRUE
birds <- readr::read_csv("data/forth_birds.csv")
nrow(birds)                            # should be close to 30,000

birds <- read_csv("data/forth_birds.csv")

leaflet(slice_sample(birds, n = 5000),
        options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles("CartoDB.Positron") |>
  addCircleMarkers(~lon, ~lat, radius = 3, stroke = FALSE,
                   fillOpacity = 0.4, popup = ~paste(species, count))

library(shiny); library(leaflet); library(dplyr); library(ggplot2); library(readr)

birds <- read_csv("data/forth_birds.csv", show_col_types = FALSE) |>
  filter(!is.na(year))
species_list <- names(sort(table(birds$species), decreasing = TRUE))

ui <- fluidPage(
  titlePanel("Seabird sightings in the Firth of Forth"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Species", choices = species_list),
      sliderInput("years", "Years", min = min(birds$year), max = max(birds$year),
                  value = range(birds$year), sep = ""),
      sliderInput("months", "Months", min = 1, max = 12, value = c(1, 12), step = 1),
      strong(textOutput("n"))
    ),
    mainPanel(
      leafletOutput("map", height = 450),
      plotOutput("season", height = 250)
    )
  )
)

server <- function(input, output, session) {
  
  # The filtered data: every output below reacts to this
  chosen <- reactive({
    birds |>
      filter(species == input$species,
             year  >= input$years[1],  year  <= input$years[2],
             month >= input$months[1], month <= input$months[2])
  })
  
  output$n <- renderText({
    paste(nrow(chosen()), "sightings,", sum(chosen()$count), "birds")
  })
  
  # Draw the empty map once
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      # addProviderTiles("CartoDB.Positron") |>
      addTiles() |>
      fitBounds(-3.4, 55.9, -1.8, 56.4)
  })
  
  # Redraw only the points when the filters change
  observe({
    leafletProxy("map", data = chosen()) |>
      clearMarkers() |>
      addCircleMarkers(~lon, ~lat, radius = ~pmin(2 + sqrt(count), 12),
                       stroke = FALSE, fillOpacity = 0.5, fillColor = "#1f6fb2",
                       popup = ~paste0(species, ": ", count, " (", date, ")"))
  })
  
  # Birds recorded per month
  output$season <- renderPlot({
    chosen() |>
      group_by(month) |>
      summarise(birds = sum(count)) |>
      ggplot(aes(factor(month, levels = 1:12, labels = month.abb), birds)) +
      geom_col(fill = "#1f6fb2") +
      scale_x_discrete(drop = FALSE) +
      labs(x = NULL, y = "Birds recorded", title = "Seasonality") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)