
# install.packages("rshiny")
# install.packages("scrollrevealR")
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(scrollrevealR)

# list of graph names
graphs_list <- c("#choro_map", "#price_rt_box", "#bedrm_pie", "#avail_3d_plot","#price_3d_plot",
                "#avail_3d_plot", "#price_3d_plot", "#corr_heatmap","#price_heatmap",
                "#availability_heatmap", "#text_net_graph", "#text_bar_graph" )

# list of buttons names
buttons_list <- c('#action-button',"#resetButton","#review_option")


# page starts
fixedPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
##############################################################################################################################
  # VIZ Title
    
    h1("Melbourne AirBnB Insights: Exploring Price, Occupancy,
       and Property Attributes"),
    br(),
    p('Kin Kwan Yim'),
    p('2023 Oct 31'),

    hr(),
  
##############################################################################################################################
# Introduction
  
  br(),
  p("Melbourne, known for its rich culture and diverse neighborhoods,
      has seen a remarkable rise in Airbnb's influence on the local hospitality scene.
      As more travelers opt for unique stays, and homeowners open their doors to global visitors,
      Melbourne's Airbnb landscape is ever-evolving.
      Through our data visualizations,
      we offer a unique lens through which to examine Melbourne's Airbnb ecosystem.
      These visualizations are designed to provide valuable insights into the factors influencing pricing,
      seasonal trends, and guest experiences.
      Whether you're an observer interested in the evolving sharing economy or wish to better understand Melbourne's dynamic Airbnb scene,
      our visualizations aim to offer a comprehensive view of this ever-changing landscape."),

  br(),hr(), br(),
##############################################################################################################################
  # Section 1: Choropleth map
  
  # Title of the Section 
  h4("Melbourne Airbnb Price Distribution Overview", align = 'center'), br(),


  # Description of the map
  p("Explore Melbourne's Airbnb landscape with the map below. 
             Each suburb is color-coded to represent the average price of Airbnb properties. 
             Lighter colors indicate lower prices, while darker colors signify higher prices. 
             This map reveals that the average prices are notably elevated in three key areas: 
             the city center, the east coastal region, and the outskirts, 
             compared to the other suburbs."),
  p("While property size might be expected to significantly influence price,
     especially in larger suburban areas, a deeper exploration reveals a more nuanced picture.
     Click the button below to view the map displaying the price per night per bedroom. 
     You can click to button below to toggle between two maps for comparision.
     You'll notice a striking similarity in the spatial distribution,
     suggesting that factors beyond property size are at play in determining price spatial distribution."),

  br(),

  
  # choropleth maps
  div(
  class = 'choro-container',
  leafletOutput("choro_map", width = 700),
  ),
  
  # button to change maps
  div(
  class = 'action-button',
  actionButton("cmap_button", "Click to toggle between different maps")
  ),

  hr(), br(),
  
##############################################################################################################################
# Section 2: Room Types and Property Attributes 
  
    # title of section 2
    h4('Room Types and Property Attributes', align = 'center'), br(),

    # Description of section 2
    p("Here, we delve into property attributes with a focus on room types and bedroom distribution. 
      Click on a suburb in the map above, and the data below will instantly adapt to provide insights specific to your selected location. 
     The left graph visually depicts the price distribution among different room types: 
     Entire Home/Apartment, Hotel Room, Private Room, and Shared Room. Meanwhile, 
     the right pie chart shows the proportion of bedrooms in properties within your chosen suburb, 
     with the most common type highlighted for your convenience. Simply hover over the pie slice 
     to view the number of listings for that specific room type. To further refine your exploration, 
     utilize the filters on the left to analyze how amenities impact the pricing dynamics of each room type."), 
  br(),
  
  # sidebar filters - suburb and amenities
  fluidRow(
    column(width = 2,
           h4("Suburb"),
           verbatimTextOutput("suburb_name"),
           br(),
           actionButton("resetButton", "Reset Suburb"),
           br(),
           h4("Amenities Filter"),
           selectInput("has_Wifi", "Wifi",
                       c("No Selection" = -1, "Yes" = 1, "No" = 0)
           ),
           selectInput("has_kitchen", "Kitchen",
                       c("No Selection" = -1, "Yes" = 1, "No" = 0)
           ),
           selectInput("has_ACHeat", "Air Conditioning / Heating",
                       c("No Selection" = -1, "Yes" = 1, "No" = 0)
           ),
           selectInput("has_selfcheckin", "Self Check in",
                       c("No Selection" = -1, "Yes" = 1, "No" = 0)
          , )
    ),
    column(width = 5, plotlyOutput("price_rt_box", width = "100%")),
    column(width = 5, plotlyOutput("bedrm_pie", width = "100%")),
    class = "box_pie_container"
  ),
  
  br(),br(),
  
  # explaination of the boxplot and pie chart
  p("Entire homes/apartments and private rooms exhibit the most extensive price distribution, 
             spanning from $50 to $1000, 
             whereas private rooms display a narrower range. 
             Hotel rooms stand out with the highest median price, 
             while shared rooms offer the most budget-friendly option with the lowest median price. 
             Interestingly, the presence of amenities appears to have a minimal impact on prices.
              When it comes to the number of bedrooms, a majority of Airbnb properties in Melbourne offer 1-2 bedrooms, 
             indicating that the the number of bedrooms offered has only a modest influence on pricing dynamics."),
  
  br(), br(),br(),br(),br(),br(),br(),hr(),br(),


##############################################################################################################################
# Section 3: Attributes Impacting Price and Occupancy

  # title of section 3
  h4('Attributes Impacting Price and Occupancy', align='center'),
  br(),

  # Description of section 3
  p("The left 3D plot, showcased below, depicts the interplay between location, 
   host communication, and cleanliness attributes, as reflected in guest review scores, 
   on Airbnb property pricing. Each data point is meticulously color-coded to represent price levels, 
   with lighter shades indicative of more affordable options, while darker hues signify higher prices. 
   On the right, you'll find a similar 3D plot, only this time the data points are color-coded to 
   illustrate the number of available days within a 60-day window. The default number of data points 
   is 100. You have the option to generate new graphs with a specified number of data points, 
   you can actively explore how these attributes impact both pricing and availability. "),

  br(),br(),

  # 2 x 3D plots
  fluidRow(
    column(width = 7,
           # Availability 3d plot
           plotlyOutput("avail_3d_plot"),
           ),

    column(width = 5,
           # price 3d plot
           plotlyOutput("price_3d_plot"),
    ),

  ),

  br(),

  # button for selecting sample size
  div(
  class = 'action-button',
  numericInput("sample_size", "Enter the number of data points to generate new 3D plots: ", 
                value = 100)),
  
  br(),

  # explanation of the 3d plots
   p("When we look at how location, cleanliness, and host communication affect property occupancy, 
     there isn't a simple, direct connection. Surprisingly, even properties with lower scores in 
     these areas can sometimes maintain high occupancy, while highly-rated places might not always 
     be fully booked. However, things become clearer when we consider the price. It turns out that 
     properties with higher prices tend to do better when it comes to location, cleanliness, and 
     communication with hosts. On the flip side, lower-priced properties show a wider range of guest reviews, 
     with some being highly rated and others not so much in these three aspects. What this means 
     is that price may play a big role in how guests perceive and experience a place. 
     Higher-priced properties can invest more in having a great location, cleanliness, 
     and host communication, which often leads to better reviews and potentially more bookings. 
     Meanwhile, lower-priced options may have more mixed guest experiences. "),
  
  br(),br(),br(), br(), br(),

  
  # description of the correlation matrix heatmap
   p("Moving on to our correlation matrix heatmap, it reveals intriguing insights into the interplay of property attributes, 
   pricing, and occupancy in Melbourne's Airbnb market. First and foremost, it's evident that guest review 
   scores in various aspects, including location, cleanliness, ratings, check-in, and communication, are 
   highly correlated. This underscores that when guests are happy with one aspect, they're likely satisfied 
   with others too. Additionally, we find a strong correlation between price and the number of bedrooms and 
   bathrooms, indicating that property size plays a significant role in pricing. Surprisingly, we uncover an 
   intriguing pattern: Availability in 60 days exhibits a weak and inverse correlation with guest review scores. 
   In simple terms, properties with higher availability levels tend to receive less favorable reviews. 
   This suggests that popular properties might receive more scrutiny, potentially leading to a wider range of reviews. 
   In contrast, the correlation with price is relatively weak, emphasizing that pricing alone does not significantly 
   affect a property's popularity. This paints a nuanced picture of how different factors interact to shape the 
   Airbnb experience in Melbourne."),
  
  br(),
  
  # correlation heat map
  div(
    class = 'threed-container',
    plotlyOutput("corr_heatmap", width = "60%", height = "400px")
    ),

    
  br(), br(),br(),br(),br(),br(),br(),hr(),br(),
  
##############################################################################################################################
# Section 4: Seasonality and Price Trends  

  # Title of section 4
   h4('Seasonality and Price Trends', align='center'),
  br(),

  # description of section 4 graphs
   p("The left heatmap unveils how average prices change at different times, 
     while the right one illustrates the percentage of available properties during those times. 
     The animated presentation allows you to witness the ebb and flow of pricing and property availability. 
     Notably, prices peak during the weekends, with Friday to Sunday experiencing the highest rates. 
     When it comes to the monthly perspective, December and February stand out as the months with the highest prices. 
     During weekends, you'll notice a surge in property bookings compared to weekdays, it may suggest 
     that many people plan short getaways during their days off. 
     Similarly, the higher demand for accommodation from December to 
     January aligns with the extended summer holiday season in Australia."),
   br(),
   br(),


  # 2 x heatmaps
  fluidRow(
    column(width = 6,
           # Price Heatmap
           plotlyOutput("price_heatmap"),
    ),
    column(width = 6,
           # availability heatmap
           plotlyOutput("availability_heatmap")
    ),
  ),

  br(),

  # slider for animating the heatmaps
  fluidRow(
  column(width = 5),
  column(width = 2,
         # Numeric input for sample size
         sliderInput("year_slider", "Select a Year:",
                     min = 2022, max = 2024, value = 2022, step = 1,
                     ticks = FALSE, sep = "", animate = animationOptions(interval = 2000, loop = TRUE,
                                                                         playButton = h6('PLAY',style="background-color:green; color: white; font-weight: bold"),
                                                                         pauseButton = h6('STOP',style="background-color:red; color: white; font-weight: bold")))
        )
  ),

  br(), br(),br(),br(),br(),br(),br(),hr(),br(),

##############################################################################################################################
# Section 5: Guest Review Insights 

  # Title of section 5
  h4('Top keywords appearing the guest reviews', align='center'),
  br(),

  # desciption of section 5
   p("In this section, we explore guest reviews and what they reveal about Melbourne's Airbnb scene. 
     On the left, our text network graph highlights the most frequently mentioned keywords in these reviews, 
     giving you a visual sense of what's often talked about. On the right, our bar chart shows the top 10 most used keywords. 
     You can also pick whether you'd like to see keywords from highly-rated or lower-rated 
     properties, giving you the flexibility to explore what interests you the most."),
   br(),

  # explanation of section 5 graphs
   p("We can find intriguing patterns in the way guests describe their Airbnb experiences. 
     For highly-reviewed properties, phrases like 'great' and 'location,' as well as 'clean' and 'comfortable,' 
     frequently appear together in reviews. This suggests that positive impressions often center 
     around aspects like cleanliness, location, and overall comfort. 
     Interestingly, even for poorly-reviewed properties, 'clean' is a keyword that repeatedly comes up,
     indicating its significance in guest satisfaction. Notably, 'clean' and 'host' are common keywords mentioned 
     in both highly-rated and lower-rated properties, highlighting the overarching importance of cleanliness 
     and host interactions in shaping guests' perceptions and experiences."),
   br(),

  # text network and bar chart
  fluidRow(
    column(width = 7,
           uiOutput("text_net_graph")),
    column(width = 5,
           uiOutput("text_bar_graph")),
  ),
  
  br(),

  # select button for review types
  div(
    class = 'action-button',
    selectInput("review_option", "Select Review Type",
                     choices = c("Highly reviewed properties", "Badly Reviewed properties"),
                     selected = "Highly reviewed properties")),




  br(), br(), br(), br(), br(), br(), br(),


##############################################################################################################################
# Scroll effects

  scroll_reveal(target = "h4", duration = 1500, distance = "200px"),
  scroll_reveal(target = "p", duration = 1500, distance = "300px"),
  scroll_reveal(target = graphs_list, duration = 2000, distance = "300px"),
  scroll_reveal(target = buttons_list, duration = 1000, distance = "300px"),

)   
