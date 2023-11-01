# install.packages("RColorBrewer")
# install.packages("rshiny")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages('tidyverse')
# install.packages('ggcorrplot')
# install.packages("plotly")
# install.packages("igraph")
# install.packages("ggraph")

library(RColorBrewer)
library(shiny)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(sf)
library(plotly)
library(ggcorrplot)
library(igraph)
library(ggraph)



# read listings csv
listings <- read_csv('./data/listings_fixed.csv')

###########################################################
# Logic of wrangling the time series data

calendar_groupedDate <- read_csv('./data/calendar_cleaned.csv')

# reference: http://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html
# Put the day_of_week as factor
calendar_groupedDate$day_of_week <-factor(calendar_groupedDate$weekday,levels=rev(0:6),
                                          labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),
                                          ordered=TRUE) # converting the day no. to factor
# Set the month as factor
calendar_groupedDate$monthf<-factor(month(calendar_groupedDate$date),
                                    levels=as.character(1:12),
                                    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                                    ordered=TRUE) # extract month


###########################################################
# Logic of creating text network and bar chart
# set seed
set.seed(99999999)

# reference from: https://ladal.edu.au/net.html
# read term-frequency data for both highly and badly review text data
va_high <- read_csv('./data/token_high.csv')
va_low <- read_csv('./data/token_low.csv')

# read term-term adjacency data for both highly and badly review text data
ed_high <- read_csv('./data/adjacency_high.csv')
ed_low <- read_csv('./data/adjacency_low.csv')

# remove 0 to NA for visualization purposes
ed_high <- ed_high %>%
  mutate(frequency = ifelse(frequency == 0, NA, frequency))
ed_low <- ed_low %>%
  mutate(frequency = ifelse(frequency == 0, NA, frequency))


# wrangling the dataframe and creating parameters for creating text network
ig_high <- igraph::graph_from_data_frame(d=ed_high, vertices=va_high, directed = FALSE)
ig_low <- igraph::graph_from_data_frame(d=ed_low, vertices=va_low, directed = FALSE)
tg_high <- tidygraph::as_tbl_graph(ig_high) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)
tg_low <- tidygraph::as_tbl_graph(ig_low) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)
E(tg_high)$weight <- E(tg_high)$frequency
E(tg_low)$weight <- E(tg_low)$frequency


# text network graph for highly reviewed properties
tn_high_graph <- tg_high %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "black",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(V(tg_high)$Occurrences)/2,
                  color = "#1ACE38") +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.1, "lines"), 
                 size=log(V(tg_high)$Occurrences)/3, 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)+
  labs(title="Top 40 keywords from the guest reviews of highly reviewed properties")

# text network graph for badly reviewed properties
tn_low_graph <-tg_low %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "black",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight)) +
  geom_node_point(size=log(V(tg_high)$Occurrences)/2,
                  color = "red") +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.1, "lines"), 
                 size=log(V(tg_high)$Occurrences)/3, 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)+
  labs(title="Top 40 keywords from the guest reviews of badly reviewed properties")

# Top 10 keywords from the guest reviews of badly reviewed properties bar chart
bart_low_graph <- va_low[1:10,] %>%
  ggplot(aes(x = reorder(Token, -Occurrences), y = Occurrences)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Top 10 keywords from the guest reviews of badly reviewed properties", 
       x = "Words", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top 10 keywords from the guest reviews of highly reviewed properties bar chart
bart_high_graph <- va_high[1:10,] %>%
  ggplot(aes(x = reorder(Token, -Occurrences), y = Occurrences)) +
  geom_bar(stat = "identity", fill = "#1ACE38") +
  labs(title = "Top 10 keywords from the guest reviews of highly reviewed properties", 
       x = "Words", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########################################################

# Read the geospatial dataframe
gdf <- st_read("./data/choropleth_data.geojson")

server <- function(input, output, session) {
  
############################################################################################################################################################################################################################################################
# Section 1: 2 Choropleth maps
  
  # reference: https://stackoverflow.com/questions/63044609/r-shiny-how-do-i-toggle-between-two-different-plots-using-an-action-button
  # Reference: https://r-graph-gallery.com/183-choropleth-map-with-leaflet.html
  whichplot <- reactiveVal(TRUE)
  
  # Create a color palette for the choropleth map
  pal <- colorBin("YlOrBr", domain = gdf$price, bins = 5)
  
  b_pal <- colorBin("YlOrBr", domain = gdf$price_per_bedroom, bins = 5)
  
  # Create tooltip text for choro map
  cmap_tooltip_texts <- paste(
    "Suburb: ", gdf$suburb,"<br/>", 
    "Average Price: ", round(gdf$price, 1), 
    sep="") %>%
    lapply(htmltools::HTML)
  
  # Create tooltip text for choro map per bedroom
  b_cmap_tooltip_texts <- paste(
    "Suburb: ", gdf$suburb,"<br/>", 
    "Average Price per Bedroom: ", round(gdf$price_per_bedroom, 1), 
    sep="") %>%
    lapply(htmltools::HTML)
  
  # Choropleth map block
  choro_map_1 <- leaflet(gdf) %>%
    setView(145.2,-37.8136,9.4) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(price),
      fillOpacity = 0.7,
      color = "grey",
      smoothFactor = 0.5,
      weight = 1.3,
      layerId = ~suburb,
      label = cmap_tooltip_texts,
      labelOptions = labelOptions( 
        style = list("font-weight" = "bold", padding = "3px 8px", border = "1px solid black"), 
        textsize = "13px", 
        direction = "auto"
      ),
      highlightOptions = highlightOptions(
        weight = 3,
        color = "black",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~price,
      opacity = 0.8,
      title = "Average Price Per night (in AUD)",
      position = "bottomright",
      labFormat = labelFormat(prefix = "$")
    )
  
  # Choropleth map per bedroom map
  choro_map_2 <- leaflet(gdf) %>%
    setView(145.2,-37.8136,9.4) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~b_pal(price_per_bedroom),
      fillOpacity = 0.7,
      color = "grey",
      smoothFactor = 0.5,
      weight = 1.3,
      layerId = ~suburb,
      label = b_cmap_tooltip_texts,
      labelOptions = labelOptions( 
        style = list("font-weight" = "bold", padding = "3px 8px", border = "1px solid black"), 
        textsize = "13px", 
        direction = "auto"
      ),
      highlightOptions = highlightOptions(
        weight = 3,
        color = "black",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = b_pal,
      values = ~price_per_bedroom,
      opacity = 0.8,
      title = "Average Price Per night Per bedroom (in AUD)",
      position = "bottomright",
      labFormat = labelFormat(prefix = "$"),
      na.label = 'No Data Available'
    )
  
  # The logic for choosing different choropleth maps
  observeEvent(input$cmap_button, {
    whichplot(!whichplot())
  })
  
  which_graph <- reactive({
    if (whichplot()) {
      choro_map_1
    } else {
      choro_map_2
    }
  })
  
  
  output$choro_map <- renderLeaflet({
    which_graph()
  })
  
  
  # Logic of getting the suburb name on click
  click_suburb <- reactiveVal("All Suburbs")
  
  # Get the ID of the layer when user clicks on the polygon
  observeEvent(input$choro_map_shape_click, { 
    polygon_click <- input$choro_map_shape_click
    click_suburb(polygon_click$id)
  })
  
  observeEvent(input$resetButton, {
    click_suburb("All Suburbs")  # Reset click_suburb to 'All Suburbs' when the button is clicked
  })
  
  suburb_clicked <- reactive({
    click_value <- click_suburb()
  })
  
  output$suburb_name <- renderText({
    suburb_name <- suburb_clicked()
    paste("Suburb chosen: ", suburb_name)
  })
  
############################################################################################################################################################################################################################################################
# Section 2
  
  # Filter the dataframe base on the checkboxes and suburb from the UI
  filtered_listings <- reactive({
    
    filtered <- listings
    
    # Filter by suburb 
    suburb <- suburb_clicked()  # Get the clicked suburb name
    if (suburb != "All Suburbs") {
      filtered <- filtered[filtered$suburb == suburb,]
    }
    
    # Filter by amenities
    if (input$has_Wifi >= 0) {
      filtered <- filtered %>% filter(has_Wifi == as.numeric(input$has_Wifi))
    }
    if (input$has_kitchen >= 0) {
      filtered <- filtered %>% filter(has_kitchen == as.numeric(input$has_kitchen))
    }
    if (input$has_ACHeat >= 0) {
      filtered <- filtered %>% filter(has_ACHeat == as.numeric(input$has_ACHeat))
    }
    if (input$has_selfcheckin >= 0) {
      filtered <- filtered %>% filter(has_selfcheckin == as.numeric(input$has_selfcheckin))
    }
    
    return(filtered)
  })
  
  # Box plot of the price distribution for different room type
  output$price_rt_box <- renderPlotly({
    ggplotly(ggplot(data = filtered_listings(), aes(x = room_type, y = price, fill = room_type)) +
      geom_boxplot() +
      labs(
        title = "Price Range by Room Type",
        x = "Room Type",
        y = "Price per night (in AUD)",
        fill = "Room Type"
      ) +
      scale_fill_brewer(palette = 'Accent') +
      theme_light() 
      # +
      # theme(
      #   plot.title = element_text(size = 16, face = "bold")
      # )
      )
  })
  
  # Pie chart showing different number of bedrooms
  output$bedrm_pie <- renderPlotly({
    # create a new dataframe for making the pie chart
    bedrm_counts <- filtered_listings() %>%
      filter(!is.na(bedrooms)) %>%
      group_by(bedrooms) %>%
      summarize(count = n()) %>%
      mutate(tooltip_text = paste(bedrooms, "bedrooms,", count, "listings"))
    
    # declaring and getting the parameters in the dataframe for making the pie chart
    max_count_category <- bedrm_counts %>%
      filter(count == max(count)) %>%
      pull(bedrooms)
    # pie_color_pal <- RColorBrewer::brewer.pal(length(unique(bedrm_counts$bedrooms)), "Set3")
    pull_values <- ifelse(bedrm_counts$bedrooms == max_count_category, 0.2, 0)
    marker_colors <- ifelse(bedrm_counts$bedrooms == max_count_category, "red", "white")
    marker_line_width <- ifelse(bedrm_counts$bedrooms == max_count_category, 4, 1)
    
    # plot the pie chart
    plot_ly(bedrm_counts, labels = ~bedrooms, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent+label',
            insidetextfont = list(color = 'white'),
            hoverinfo = 'text',
            text = ~tooltip_text,
            textfont = list(size = 14),
            marker = list(
              # colors = pie_color_pal,
              line = list(color = marker_colors, width = marker_line_width)
            ),
            pull = pull_values
    ) %>%
      layout(title = list(text = 'Proportion of AirBnB listings by Number of Bedrooms', font = list(weight = 'bold')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(title = list(text = 'Number of Bedrooms')))
    
    
  })
  
##############################################################################################################################
# Section 3: Room Types and Property Attributes
  
  # Define a reactive value to store the sampled data
  sampled_data <- reactive({
    sample_size <- input$sample_size
    
    # Sample n rows from the highly reviewed properties
    highly_reviewed_rows <- listings %>%
      filter(review_scores_rating > 3) %>%
      sample_n(sample_size, replace = FALSE)
    
    # Sample n rows from the badly reviewed properties
    badly_reviewed_rows <- listings %>%
      filter(review_scores_rating < 3) %>%
      sample_n(sample_size, replace = FALSE)
    
    # Combine the sampled rows into a single dataframe
    combined_sampled_df <- bind_rows(highly_reviewed_rows, badly_reviewed_rows)
    
    combined_sampled_df
  })

  
  # ref: https://plotly.com/r/3d-scatter-plots/
  # 3d plot for availability
  output$avail_3d_plot <- renderPlotly({
    
    combined_sampled_df <- sampled_data()
    plot_ly(combined_sampled_df, x = ~review_scores_cleanliness, y = ~review_scores_communication, z = ~review_scores_location,
            color = ~`availability days in 60 days`, colors = c("red", "#1ACE38"),
            marker = list(colorbar = list(title = "availability days in 60"))
            ,hoverinfo = "text") %>%
      add_markers(text = ~paste("Cleanliness Score: ", review_scores_cleanliness, "<br>",
                                "Communication Score: ", review_scores_communication, "<br>",
                                "Location Score: ", review_scores_location, "<br>",
                                "Availability in 60 days: ", `availability days in 60 days`
                                ),
                  # colorbar = list(title = "New Colorbar Title")
                  )  %>%
      layout(scene = list(xaxis = list(title = 'Cleanliness Score'),
                          yaxis = list(title = 'Communication Score'),
                          zaxis = list(title = 'Location Score')),
             title = "Review Scores vs. Availability days in 60 days"
             # annotations = list(
             #   x = 1.24,
             #   y = 1.03,
             #   text = '(No. of available days in 60 days)',
             #   xref = 'paper',
             #   yref = 'paper',
             #   showarrow = FALSE
             # )
             
             
      )
  })
  
  # 3d plot for price
  output$price_3d_plot <- renderPlotly({
    
    combined_sampled_df <- sampled_data()
    
    plot_ly(combined_sampled_df, x = ~review_scores_cleanliness, y = ~review_scores_communication, z = ~review_scores_location,
            color = ~price, colors = brewer.pal(9, 'YlOrBr'), 
            # showscale = TRUE,
            hoverinfo = "text") %>% 
      add_markers(text = ~paste("Cleanliness Score: ", review_scores_cleanliness, "<br>",
                                "Communication Score: ", review_scores_communication, "<br>",
                                "Location Score: ", review_scores_location, "<br>",
                                "Price: ", price))  %>%
      layout(scene = list(xaxis = list(title = 'Cleanliness Score'),
                          yaxis = list(title = 'Communication Score'),
                          zaxis = list(title = 'Location Score')),
             annotations = list(
               x = 1.2,
               y = 1.05,
               text = '( in (AUD) )',
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE
             ),
             title = "Review Scores vs. Price in (AUD)"
      )
  })
  
  output$corr_heatmap <- renderPlotly({
    
    # Select numeric rows and drop na values for correlation
    listings_num <- listings %>%
      select(bedrooms, price, num_baths,review_scores_cleanliness, `availability days in 60 days`,
             review_scores_checkin,review_scores_communication,
             review_scores_location,review_scores_rating) %>%
      drop_na()
    
    # make a correlation matrix
    corr<-cor(listings_num)
    
    # Define a list of names to be displayed on the correlation heat map
    corr_labels <- rev(c("No. of Bathrooms", "No. of bedrooms", "Price", "Availabilty in 60Days", 
                         "Communication Score", "Checkin Score", "Review Score",
                         "Cleanliness Score", "Location Score"))
    
    # plot the correlation heatmap
    ggplotly(ggcorrplot(
      corr, hc.order = TRUE, type = "full",legend.title = "Correlation Coefficient",
      outline.color = "white",
      title = "Correlation between Attributes, Price and Occupancy"
    ) + scale_x_discrete(labels = corr_labels) + scale_y_discrete(labels = corr_labels),
    # width = 700, height = 500
    )
    
  }
  )
  
##############################################################################################################################
# Section 4: Seasonality and Price Trends  

  observe({
    year_input <- as.numeric(input$year_slider)
    
    # Filter the data based on the selected year
    filtered_calendar <- calendar_groupedDate %>% filter(year == year_input)

    # Create the price heatmap
    price_heatmap <- filtered_calendar %>%
      ggplot(aes(week_of_month, day_of_week, fill = average_price)) +
      geom_tile(colour = "white") +
      facet_grid(~monthf) +
      scale_fill_gradient(low = "#1ACE38", high = "red") +
      labs(title = paste("Airbnb Price Calendar Heatmap in", year_input), x = "Week of Month", y = "", fill = "Price (in AUD)")
    price_heatmap <- ggplotly(price_heatmap)
    
    # Create the availability heatmap
    availability_heatmap <- filtered_calendar %>%
      ggplot(aes(week_of_month, day_of_week, fill = percentage_available)) +
      geom_tile(colour = "white") +
      facet_grid(~monthf) +
      scale_fill_gradient(low = "red", high = "#1ACE38") +
      labs(title = paste("Airbnb Availability Calendar Heatmap in", year_input), x = "Week of Month", y = "", fill = "% of properties available")
    availability_heatmap <- ggplotly(availability_heatmap)
    
    # Update the plots
    output$price_heatmap <- renderPlotly({ price_heatmap })
    output$availability_heatmap <- renderPlotly({ availability_heatmap })
  })
  
##############################################################################################################################
# Section 5: Guest Review Insights 
  
  # logic for displaying different text network graph based on the filter
  output$text_net_graph <- renderUI({
    if (input$review_option == "Highly reviewed properties") {
      plotOutput("tn_high_graph", 
                 height = "500px"
                 )
    } else {
      plotOutput("tn_low_graph"
                 , height = "500px"
                 )
    }
  })
  
  
  # logic for displaying different bar graph based on the filter
  output$text_bar_graph <- renderUI({
    if (input$review_option == "Highly reviewed properties") {
      plotOutput("bart_high_graph", 
                 height = "500px"
                 )
    } else {
      plotOutput("bart_low_graph", 
                 height = "500px"
                 )
    }
  })
  
  output$tn_high_graph <- renderPlot({
    tn_high_graph
  })
  
  output$tn_low_graph <- renderPlot({
    tn_low_graph
  })
  
  output$bart_high_graph <- renderPlot({
    bart_high_graph
  })
  
  output$bart_low_graph <- renderPlot({
    bart_low_graph
  })
  
}