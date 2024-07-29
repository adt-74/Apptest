
# global functions with all necessary libraries
global = function() {
  library(sf)
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data visualization
  library(tidygraph)
  library(shiny) # main shiny app package
  library(bslib) # easier html construction
  library(plotly) # interactive visuals
}

read_data = function() {
  # Get any helper data you need to starts
  
  # 1) read county data
  read.csv("./County.csv") %>%
    saveRDS("county_names.rds")
  county_names = read_rds("./county_names.rds") %>%
                filter(state == "Florida") #Filtering to Florida data analysis only
  county_names$county <- as.integer(county_names$county)

  from_county_names = county_names %>%
    rename(
      from_countyid = county,
      from_county_name = county_name,
      from_state = state
    )
  
  to_county_names = county_names %>%
    rename(
      to_countyid = county,
      to_county_name = county_name,
      to_state = state
    )
  
  n = read_rds("./dorian.rds") %>%
    # Focus on just the nodes of the network
    activate("nodes")
  # 2) read nodes data
  read_rds("./dorian.rds") %>%
    # Focus on just the nodes of the network
    activate("nodes") %>%
    # Turn it into a tibble/data.frame
    as_tibble() %>%
    # Create a unique node id,
    # which we'll use to join in geoid to the from and to columns of the edges
    mutate(node = 1:n()) %>%
    # Reorder the variables; dropping geometry
    select(node, geoid, svi, pop, county, rainfall_14days) %>%
    # Save the nodes
    saveRDS("nodes.rds")
  nodes = read_rds("./nodes.rds")
  # Convert county column to integer
  nodes$county <- as.integer(nodes$county)
  
  # 3) read edges data
  # Save the edges to file.
  read_rds("./dorian.rds") %>%
    # Focus on just the edges of the network
    activate("edges") %>%
    # Turn it into a tibble/data.frame
    as_tibble() %>%
    # Drop the geometry
    # select(-geometry) %>%
    # Let's join the source (from) geoid in using the shared node id
    left_join(by = c("from" = "node"),
              y = nodes %>% select(node, from_geoid = geoid)) %>%
    # Let's join the destination (to) geoid in using the shared node id
    left_join(by = c("to" = "node"),
              y = nodes %>% select(node, to_geoid = geoid)) %>%
    # Save this to file
    saveRDS("edges.rds")
  edges = read_rds("./edges.rds")
  
  edges %>%
    mutate(from_countyid = as.integer(substr(from_geoid, 1, 5)),
           to_countyid = as.integer(substr(to_geoid, 1, 5))) %>%
    inner_join(county_names, by = c("from_countyid" = "county")) %>%
    rename(from_county_name = county_name) %>%
    inner_join(county_names, by = c("to_countyid" = "county")) %>%
    #Joins the county names table tother to match with the and from county ids
    rename(to_county_name = county_name) %>%
    select(from:from_county_name, to_county_name, state = state.x) %>%
    mutate(evac_shel = if_else(evacuation > 0, "Evacuated", "Sheltered")) %>%
    # join from node
    left_join(
      by = c("from" = "node"),
      y = nodes %>% select(node, from_geoid = geoid, svi, pop, rainfall_14days)
    ) %>%
   
    saveRDS("nodes_n_edges.rds")
  
}
### USER INTERFACE ####

ui = function() {
  read_data()
  # Load the nodes in
  nodes = read_rds("./nodes.rds")
  nodes_n_edges = read_rds("./nodes_n_edges.rds")
  
  nodes_n_edges %>%
    # Get the day of analysis.
    mutate(day = lubridate::day(date_time)) %>%
    # Get the month
    mutate(month = lubridate::month(date_time)) %>%
    # Get the hour
    mutate(hour = lubridate::hour(date_time))
  
  # Get unique counties
  from_counties <- unique(nodes_n_edges$from_county_name[order(nodes_n_edges$from_county_name)])
  to_counties <- unique(nodes_n_edges$to_county_name[order(nodes_n_edges$to_county_name)])
  ev_sh <- unique(nodes_n_edges$evac_shel[order(nodes_n_edges$evac_shel)])
  
  # Make a named vector, so you can select by Names but get back specific ids, eg. "Dec" = 12
  choices_from_county = setNames(object = from_counties, nm = from_counties)
  choices_to_county = setNames(object = to_counties, nm = to_counties)
  choices_min_date = min(nodes_n_edges$date_time)
  choices_max_date = max(nodes_n_edges$date_time)
  choices_ev_sh = setNames(object = ev_sh, nm = ev_sh)
  
 
  # TITLE CARD ###################################
  c1 = card(# Make a card header whose background is the primary color (class = bg-primary)
    card_header(
      class = "bg-primary",
      # Add this title
      card_title("Analysis of Hurricane Dorian Evacuation (2019)")
    ))
  
  # SELECTOR CARD #################################
  c2 = bslib::card(
    # Make a simple card header and title
    card_header(card_title("Filters")),
    # Make a card body section
    card_body(
      sliderInput(
        "dateRange",
        "Select Date Range:",
        min = as.Date(choices_min_date),
        max = as.Date(choices_max_date),
        value = c(as.Date("2019-08-28"), as.Date("2019-09-7")),
        timeFormat = "%Y-%m-%d"
      )
    ),
    card_body(
      selectInput(
        inputId = "evac_shel",
        label = "Select Evacuated / Sheltered",
        choices = choices_ev_sh,
        selected = "Evacuated",
        selectize = FALSE, size = 5
      )
    ),
    card_body(
      selectInput(
        inputId = "to_county_name",
        label = "Select Destination County",
        choices = choices_to_county,
        selected = sample(to_counties, 1),
        selectize = FALSE, size = 5
      )
    )
  )
  
  # PLOT CARD ##########################
  c3 = bslib::layout_column_wrap(card(plotlyOutput(outputId = "plot_one")), card(plotlyOutput(outputId = "plot_two")), width = 0.5)
  c4 = bslib::layout_column_wrap(card(plotlyOutput(outputId = "map_one")), 
                                 card(plotlyOutput(outputId = "plot_three")), 
                                 card(plotlyOutput(outputId = "plot_four")), 
                                 card(plotlyOutput(outputId = "map_two")), 
                                 width = 0.5)
  
  # VALUE BOXES CARD ##########################
  box1 = bslib::value_box(
    title = textOutput("box_one_title"),
    value = textOutput("box_one_measure"),
    class = "bg-primary text-light",
    # add a fontawesome icon to showcase
    showcase = shiny::icon("person-walking-arrow-right")
  )
  box2 = bslib::value_box(
    title = textOutput("box_two_title"),
    value = textOutput("box_two_measure"),
    class = "bg-primary text-light",
    # add a fontawesome icon to showcase
    showcase = shiny::icon("home")
  )
  box3 = bslib::value_box(
    title = textOutput("box_three_title"),
    value = textOutput("box_three_measure"),
    class = "bg-primary text-light",
    # add a fontawesome icon to showcase
    showcase = shiny::icon("plane")
  )
  box4 = bslib::value_box(
    title = textOutput("box_four_title"),
    value = textOutput("box_four_measure"),
    class = "bg-primary text-light",
    # add a fontawesome icon to showcase
    showcase = shiny::icon("cloud-rain")
  )
  # Bundle them together with a header
  c5 = card(
    # Add a header describing the Selections you made
    card_header(class = "bg-primary", card_title(textOutput("text_selection"))),
    # Bundle the value boxes together
    card_body(layout_column_wrap(box1, box2, box3, box4)),
    width = 1 / 4
  ) 

  # SPOTLIGHT BOX
  c6 = bslib::card(
    bslib::card_header("Spotlight", class = "bg-dark"),
    bslib::card_footer(textOutput("page_two_spotlight")))
  
  # Or add a sidebar-main split layout like this...
  bslib::page(
    title = "Hurricane Dorian",
    # add a bootstrap theme to the page
    theme = bslib::bs_theme(preset = "cerulean"),
    # Stack cards
    c1,
    # header
    # Put next cards in a sidebar-main panel split layout
    bslib::layout_sidebar(
      # Sidebar...
      sidebar = bslib::sidebar(c2),
      # main panel
      c5,
      # Make a series of panels we can click between
      bslib::navset_card_pill(
        selected = "plots",
        # page one
        bslib::nav_panel(title = "Overview", value = "plots", c3),
        # page two
        bslib::nav_panel(title = "Spatial Maps", value = "maps", c4, c6)
      )
      # c4 # text
    )
  )
}

#### SERVER DATA #####

server = function(input, output, session) {
  # Load the nodes in
  nodes = read_rds("./nodes.rds")
  nodes_n_edges = read_rds("./nodes_n_edges.rds")
  countygeo = read_sf("./counties.geojson") #Mapping data
  
  #Florida data filter
  florida_stat = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time),
             time = format(as.POSIXct(date_time), format = "%H:%M:%S")) %>%
      # Filter to just rows where county matches the user input
      # filter(county_name %in% input$county_name)
      filter(date > input$dateRange[1] &
               date < input$dateRange[2]) # use side bar filter for date
    # Trigger whenever input$origin changes
  }) %>% bindEvent({
    input$dateRange
  })
  
  #To County data filter
  to_county_stat = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time),
             time = format(as.POSIXct(date_time), format = "%H:%M:%S")) %>%
      # Filter to just rows where county matches the user input
      # filter(county_name %in% input$county_name)
      filter(date > input$dateRange[1] &
               date < input$dateRange[2]) %>% # use side bar filter for date
      filter(to_county_name == input$to_county_name)
    # Trigger whenever input$origin changes
  }) %>% bindEvent({
    list(input$dateRange, input$to_county_name)
  })
  
  #Nodes and Edges data filter
  nodes_n_edges_stat = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time),
             time = format(as.POSIXct(date_time), format = "%H:%M:%S")) %>%
      # Filter to just rows where county matches the user input
      # filter(county_name %in% input$county_name)
      filter(date > input$dateRange[1] &
               date < input$dateRange[2]) %>% # use side bar filter for date
      filter(evac_shel == input$evac_shel) %>%
      filter(to_county_name == input$to_county_name)# use side bar filter for to_county_name
    
    # Trigger whenever input$evac_shel and input$to_county_name changes
  }) %>% bindEvent({
    list(input$dateRange, input$evac_shel, input$to_county_name)
  })
  
  # Value Box Text ########################################
  output$box_one_title = renderText({
    paste("Total Number of Evacuatations to",
          "\n",
          input$to_county_name)
  }) %>% bindEvent({
    input$to_county_name
  })
  output$box_one_measure = renderText({
    t_ev <- to_county_stat() %>%
      filter(evac_shel == "Evacuated") %>%
      summarize(total_evacuations = sum(evacuation, na.rm = TRUE)) %>%
      pull(total_evacuations)
    t_ev
  }) %>% bindEvent({
    to_county_stat()
    input$to_county_name
  })
  output$box_two_title = renderText({
    paste("Total Number of Shelter-In-Place in",
          "\n",
          input$to_county_name)
  }) %>% bindEvent({
    input$to_county_name
  })
  output$box_two_measure = renderText({
    t_sh <- to_county_stat() %>%
      filter(evac_shel == "Sheltered") %>%
      summarize(total_evacuations = sum(evacuation, na.rm = TRUE)) %>%
      pull(total_evacuations)
    t_sh
  }) %>% bindEvent({
    to_county_stat()
    input$to_county_name
  })
  output$box_three_title = renderText({
    paste("Average Distance (km) Traveled to",
          "\n",
          input$to_county_name)
  }) %>% bindEvent({
    input$to_county_name
  })
  output$box_three_measure = renderText({
    t_tr <- to_county_stat() %>%
      filter(evac_shel == "Sheltered") %>%
      summarize(avg_distance_traveled = mean(km, na.rm = TRUE)) %>%
      pull(avg_distance_traveled)
    t_tr
  }) %>% bindEvent({
    to_county_stat()
    input$to_county_name
  })
  output$box_four_title = renderText({
    paste("Average Rainfall in L14 Days", "\n", input$to_county_name)
  }) %>% bindEvent({
    input$to_county_name
  })
  output$box_four_measure = renderText({
    a_rf <- nodes_n_edges_stat() %>%
      summarize(avg_rainfall = mean(rainfall_14days)) %>%
      pull(avg_rainfall)
    a_rf
  }) %>% bindEvent({
    nodes_n_edges_stat()
    input$to_county_name
  })
  output$text_selection = renderText({
    paste(
      "Evacuation Status at",
      input$to_county_name,
      "Between",
      input$dateRange[1],
      "and",
      input$dateRange[2]  
    )
  }) %>% bindEvent({
    input$to_county_name
  })

  # SPOTLIGHT TEXT IN SPATIAL TAB #####
  page_two_spotlight_stat = reactive({
    # Format a number for highlighting
    nodes_n_edges_stat() %>%
      summarize(avg_distance_traveled = mean(km, na.rm = TRUE)) %>%
      mutate(avg_distance_traveled = scales::number(avg_distance_traveled, accuracy = 0.1)) %>%
      mutate(text = paste(
      "Between", input$dateRange[1], "and",input$dateRange[2] ,
      "the average distance traveled to", input$from_county_name, "is", avg_distance_traveled, "km."))
  }) %>% bindEvent({
    nodes_n_edges_stat()
    list(input$dateRange, input$from_county_name)
  })

  ## Render to text output 'text_highlight'
  output$page_two_spotlight = renderText({
    # Output a single text blob value. Must have just length 1.
    page_two_spotlight_stat()$text
    # Trigger whenever stat_highlight() changes
  }) %>% bindEvent({page_two_spotlight_stat()})

  
  ### PLOTS  ########################################
  
  ## PLOT ONE ## 
  #TOP 10 DESTINATIONS FOR EVACUATION WITHIN FLORIDA
  
  output$plot_one = renderPlotly({
    plot_one_data = florida_stat() %>% # use side bar filter for date
      filter(evac_shel == input$evac_shel) %>%   # use side bar filter EVAC OR SHELTERED
      group_by(from_county_name) %>%
      summarise(
        total_evacuations = sum(evacuation, na.rm = TRUE),
        avg_distance_traveled = mean(km, na.rm = TRUE)
      ) %>%
      arrange(desc(total_evacuations)) %>%
      head(10)
    
    gg_plot_one = ggplot(
      data = plot_one_data,
      mapping = aes(
        x = reorder(from_county_name, total_evacuations),
        y = total_evacuations,
        fill = avg_distance_traveled
      )
    ) +
      geom_col(size = 0.5) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "Counties",
        y = "Evacuated",
        fill = "Distance Traveled (km)",
        title = paste(
          "Top 10 Places Individuals",
          input$evac_shel,
          "\n",
          "Between",
          input$dateRange[1],
          "and",
          input$dateRange[2]
        )
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      ) +
      scale_fill_gradient2(
        low = "lightyellow",
        mid = "turquoise",
        high = "blue4",
        midpoint = mean(plot_one_data$avg_distance_traveled, na.rm = TRUE)
      )
    
    # Make it plotly
    pp_plot_one = plotly::ggplotly(gg_plot_one)
    # return the visualization
    pp_plot_one
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({
    florida_stat()
    list(input$evac_shel, input$dateRange)
  })
  
  
  ## PLOT TWO ## 
  # AVERAGE EVACUATION OVER TIME (COUNTY VS STATE)
  
  output$plot_two = renderPlotly({
    ## DateTime Vs. Mean Number of Evacuation
    plot_two_state_data = florida_stat() %>%
      #sorts date range to examine hurricane
      group_by(date) %>%
      summarize(AVG_EVAC = mean(evacuation, na.rm = TRUE))
    
    plot_two_to_cty_data = to_county_stat() %>%
      #sorts date range to examine hurricane
      group_by(date) %>%
      summarize(AVG_EVAC = mean(evacuation, na.rm = TRUE))
    
    gg_plot_two <- ggplot() +
      geom_line(
        data = plot_two_state_data,
        aes(x = date, y = AVG_EVAC, color = "State of Florida"),
        size = 0.5,
        alpha = 0.3,
      ) +
      geom_point(
        data = plot_two_state_data,
        aes(
          x = date,
          y = AVG_EVAC,
          color = ifelse(AVG_EVAC < 0, 'Below Zero', 'Above Zero')
          #size = 0.1,
        ),
        alpha = 0.3
      ) +
      geom_line(
        data = plot_two_to_cty_data,
        aes(
          x = date,
          y = AVG_EVAC,
          color = input$to_county_name
        ),
        size = 0.5
      ) +
      geom_point(data = plot_two_to_cty_data, aes(
        x = date,
        y = AVG_EVAC,
        color = ifelse(AVG_EVAC < 0, 'Below Zero', 'Above Zero')
        #size = 0.1
      )) +
      geom_hline(yintercept = 0,
                 linetype = "solid",
                 color = "black") +
      scale_color_manual(
        values = c(
          "State of Florida" = "coral",
          "Below Zero" = "darkturquoise",
          "Above Zero" = "coral"
        )
      ) +
      scale_x_date(date_breaks = "3 days", date_labels = "%m-%d") +
      scale_y_continuous(
        labels = function(x)
          format(x, scientific = FALSE)
      ) +
      theme_bw() +
      labs(
        x = "Date",
        y = "Evacuations",
        title = paste(
          "Average Evacuations",
          "Between",
          input$dateRange[1],
          "and",
          input$dateRange[2]
        )
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Make it plotly
    pp_plot_two = plotly::ggplotly(gg_plot_two, tooltip = c("Date", "Evacuations"))
    # return the visualization
    pp_plot_two
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({
    florida_stat()
    to_county_stat()
    list(input$to_county_name, input$dateRange)
  })
  
  ## MAP 1 ## 
  # DISTANCE TRAVELED TO CHOSEN COUNTIES (GEO LOCATION)
  
  output$map_one = renderPlotly({
    florida_map <- countygeo %>%
      filter(state == "FL") %>%
      mutate(geoid = as.integer(geoid))
    
    fl_evac_from_raw <- to_county_stat() %>%
      filter(evac_shel == "Evacuated") %>%
      group_by(from_countyid, from_county_name) %>%
      summarise(avg_distance_traveled = mean(km, na.rm = TRUE)) %>%
      arrange(desc(avg_distance_traveled)) %>%
      left_join(florida_map %>% select(geoid, geometry),
                by = c("from_countyid" = "geoid")) %>%
      head(10) %>%
      st_as_sf()
    
    # Let's grab nodes from this network (cities / county-subdivisions)
    data = read_rds("./dorian.rds")
    nodes_data = data %>%
      # Focus on just the nodes of the network
      activate("nodes") %>%
      # Turn it into a tibble/data.frame
      as_tibble() %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    edges_data = nodes_n_edges_stat() %>%
      filter(evac_shel == "Evacuated") %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    valid_fl_geo <- st_is_valid(fl_evac_from_raw)
    empty_fl_geo <- st_is_empty(fl_evac_from_raw)
    fl_evac_from <- fl_evac_from_raw[valid_fl_geo & !empty_fl_geo, ]
    
    centroids <- st_centroid(fl_evac_from)
    
    if (nrow(fl_evac_from) > 0) {
      map_one <- ggplot() +
        geom_sf(data = florida_map,
                fill = "transparent",
                color = "black") +
        geom_sf(
          data = fl_evac_from,
          aes(geometry = geometry, fill = avg_distance_traveled),
          color = "coral"
        ) + # Fill based on sum_evac
        geom_sf(data = edges_data, mapping = aes(alpha = nrow(edges_data))) +
        geom_text(
          data = centroids,
          aes(
            x = st_coordinates(geometry)[, 1],
            y = st_coordinates(geometry)[, 2],
            label = from_county_name
          ),
          size = 3,
          color = "black",
          nudge_y = 0.1
        ) +
        scale_fill_gradient2(
          low = "lightyellow",
          mid = "turquoise",
          high = "blue4",
          midpoint = mean(fl_evac_from$avg_distance_traveled, na.rm = TRUE),
          name = "Avg Distance Traveled"
        ) +
        labs(
          title = paste(
            "Top 10 Origin Counties to",
            input$to_county_name,
            "\n",
            "Between",
            input$dateRange[1],
            "and",
            input$dateRange[2]
          ),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
    } else {
      map_one <- ggplot() +
        geom_sf(data = florida_map,
                fill = "transparent",
                color = "black") +
        geom_sf(data = edges_data, mapping = aes(alpha = nrow(edges_data))) +
        labs(
          title = paste("Geo Data is Not Available for", input$to_county_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
          
        )
    }
  }) %>% bindEvent({
    to_county_stat()
    list(input$evac_shel, input$to_county_name)
  })
  
  ## PLOT 3 ##
  # DISTANCE TRAVELED TO CHOSEN COUNTY (BAR PLOT)
  
  output$plot_three = renderPlotly({
    # for specific county, total evacuation
    plot_three_data = to_county_stat() %>%
      filter(evac_shel == input$evac_shel) %>%  # use side bar filter EVAC OR SHELTERED
      group_by(to_county_name, from_county_name) %>%
      summarise(
        total_evacuations = sum(evacuation, na.rm = TRUE),
        count = n(),
        avg_distance_traveled = mean(km, na.rm = TRUE)
      ) %>%
      arrange(desc(total_evacuations))
    
    gg_plot_three = ggplot(
      data = plot_three_data,
      mapping = aes(
        x = reorder(from_county_name, total_evacuations),
        y = total_evacuations,
        fill = avg_distance_traveled
      )
    ) +
      geom_col(size = 2) +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "From Counties",
        y = "Evacuated",
        fill = "Distance Traveled (km)",
        title = paste(
          "County Individuals",
          input$evac_shel,
          "to:",
          input$to_county_name
        )
      ) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      # legend.position = "none") +
      scale_y_log10(
        labels = function(x)
          format(x, scientific = FALSE)
      ) +
      scale_fill_gradient2(
        low = "lightyellow",
        mid = "turquoise",
        high = "blue4",
        midpoint = mean(plot_three_data$avg_distance_traveled, na.rm = TRUE)
      )
    # Make it plotly
    pp_plot_three = plotly::ggplotly(gg_plot_three)
    # return the visualization
    pp_plot_three
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({
    to_county_stat()
    list(input$evac_shel, input$to_county_name)
  })

  ## MAP 2 ##
  # AVERAGE SVI PER ORIGIN COUNTIES
  
  output$map_two = renderPlotly({
    florida_map <- countygeo %>%
      filter(state == "FL") %>%
      mutate(geoid = as.integer(geoid))
    
    fl_evac_from_raw <- to_county_stat() %>%
      filter(evac_shel == "Evacuated") %>%
      group_by(from_countyid, from_county_name) %>%
      summarise(avg_svi = mean(svi, na.rm = TRUE)) %>%
      arrange(desc(avg_svi)) %>%
      left_join(florida_map %>% select(geoid, geometry),
                by = c("from_countyid" = "geoid")) %>%
      head(10) %>%
      st_as_sf()
    
    # Let's grab nodes from this network (cities / county-subdivisions)
    data = read_rds("./dorian.rds")
    nodes_data = data %>%
      # Focus on just the nodes of the network
      activate("nodes") %>%
      # Turn it into a tibble/data.frame
      as_tibble() %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    edges_data = nodes_n_edges_stat() %>%
      filter(evac_shel == "Evacuated") %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    valid_fl_geo <- st_is_valid(fl_evac_from_raw)
    empty_fl_geo <- st_is_empty(fl_evac_from_raw)
    fl_evac_from <- fl_evac_from_raw[valid_fl_geo & !empty_fl_geo, ]
    
    centroids <- st_centroid(fl_evac_from)
    
    if (nrow(fl_evac_from) > 0) {
      map_one <- ggplot() +
        geom_sf(data = florida_map,
                fill = "transparent",
                color = "black") +
        geom_sf(
          data = fl_evac_from,
          aes(geometry = geometry, fill = avg_svi),
          color = "coral"
        ) + # Fill based on sum_evac
        geom_sf(data = edges_data, mapping = aes(alpha = nrow(edges_data))) +
        geom_text(
          data = centroids,
          aes(
            x = st_coordinates(geometry)[, 1],
            y = st_coordinates(geometry)[, 2],
            label = from_county_name
          ),
          size = 3,
          color = "black",
          nudge_y = 0.1
        ) +
        scale_fill_gradient2(
          low = "lightyellow",
          mid = "orange",
          high = "red3",
          midpoint = mean(fl_evac_from$avg_svi, na.rm = TRUE),
          name = "Avg SVI"
        ) +
        labs(
          title = paste(
            "Avg Social Vulnerability Index of Origin Counties to",
            input$to_county_name,
            "\n",
            "Between",
            input$dateRange[1],
            "and",
            input$dateRange[2]
          ),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
    } else {
      map_one <- ggplot() +
        geom_sf(data = florida_map,
                fill = "transparent",
                color = "black") +
        geom_sf(data = edges_data, mapping = aes(alpha = nrow(edges_data))) +
        labs(
          title = paste("Geo Data is Not Available for", input$to_county_name),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
          
        )
    }
  }) %>% bindEvent({
    to_county_stat()
    list(input$evac_shel, input$to_county_name)
  })

  ## PLOT 4 ##
  # AVERGAE RAINFALL IN ORIGIN COUNTIES
  
  output$plot_four = renderPlotly({
    # for specific county, total evacuation
    plot_four_data = to_county_stat() %>%
      filter(evac_shel == input$evac_shel) %>%  # use side bar filter EVAC OR SHELTERED
      group_by(to_county_name, from_county_name) %>%
      summarise(
        avg_rainfall = mean(rainfall_14days, na.rm = TRUE),
        count = n(),
        avg_distance_traveled = mean(km, na.rm = TRUE)
      ) %>%
      arrange(desc(avg_distance_traveled))
    
    gg_plot_four = ggplot(
      data = plot_four_data,
      mapping = aes(
        x = reorder(from_county_name, avg_distance_traveled),
        y = avg_distance_traveled,
        fill = avg_rainfall
      )
    ) +
      geom_col(size = 2) +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "From Counties",
        y = "Average Distance Traveled (km)",
        fill = "Average Rainfall in Last 14 Days",
        title = paste(
          "County Individuals",
          input$evac_shel,
          "to:",
          input$to_county_name
        )
      ) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      # legend.position = "none") +
      scale_y_log10(
        labels = function(x)
          format(x, scientific = FALSE)
      ) +
      scale_fill_gradient2(
        low = "lightyellow",
        mid = "orange",
        high = "red3",
        midpoint = mean(plot_four_data$avg_rainfall, na.rm = TRUE)
      )
    # Make it plotly
    pp_plot_four = plotly::ggplotly(gg_plot_four)
    # return the visualization
    pp_plot_four
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({
    to_county_stat()
    list(input$evac_shel, input$to_county_name)
  })
  
  
  
  
}

# Run app
shiny::shinyApp(ui = ui,
                server = server,
                onStart = global)