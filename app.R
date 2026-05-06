# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)

# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and 
# how people can use the app. 

ui <-fluidPage(
  theme = bs_theme(
    bg        = "#1a1a2e",
    fg        = "#e0e0e0",
    primary   = "#c41e3a",
    secondary = "#4a4a6a",
    base_font = font_google("Crimson Text")  
  ),
  
  titlePanel(
    div(style = "color: #c41e3a; font-weight: bold;",
        "'Hunger Games: Catching Fire' Rebellion Influence Network")
  ),
  
  page_sidebar(
    sidebar = sidebar(
      title = "Explore the Network",
      bg    = "#16213e",
      "Analyze rebellion influence and gossip structure 
       among characters in Catching Fire."
    ),
    #card1
    card(
      card_header("Introduction"), 
      "This project is focused on gossip network in the Hunger Games movie Catching Fire. 
      This network is described to have connections between characters when characters refrence eachother in conversation. 
      The goal of this project is to analyze who is talked about the most, who is a leading force in rebellion, whose characters are being focused on,etc. "),
    
    #card2 network colored by attribute
    card(card_header("Here's a simple network of relations based on attributes of characters"),
         selectInput("color",
                     "choose characteristic of the network for nodes", 
                     choices = list("Gender" = "gender", 
                                    "District" = "District"), 
                     selected = 1), 
         plotOutput("characters_net"), height = "600px"),
    
    #card3 In and out degree network
    card(
      card_header("In-Degree vs Out-Degree"),
      plotOutput("degree_scatter"),
      height = "800px", 
      "The y-axis represents the in-degree aka of how much a character is talked about by others. 
      So the higher up on the y-axis, the more a character is talked about. The x-axis represents out-degree, 
      so how much a character talks about other characters. So the farther rigth a hcracater is
      the more frequenlty they mention other characters. Characters in 
      the top right talk about others a lot and are talked about a lot.
      Characters in the bottom left don't talk about others and don't get talked about much.
      From this we can conclude that Katniss is the most talked about character and talks about other 
      chracaters the most, which makes sense since she is teh main character and the story follows her.Peeta
      is a character who is talked about frequently by other characters, as he is all the way at the top of the
      in-degree metric, but isn't refrened more then Haymitch aor Katniss, as he is about halfway across the out-degree axis. 
      Someone like Haymitch would be a very influential character because his out-degree is proportionally larger then in-degree, 
      meaning he is spreading a lot of information, but maybe he is more on the down low about his information spread. An interesting 
      observation is president snow talks about others far more frequrnlty then he is talked about. Which suggest he doesn't have as 
      much influence over Panem then he belives he does. This could foreshadow rebellion. ",
    ),
    #card4 w character removal
    card(
      card_header("Network With Character Removed"),
      "Remove a character to see how the network changes without them.",
      selectInput("remove_char",
                  "Select character to remove:",
                  choices = NULL),   # we fill this from the server
      plotOutput("no_char_net"),
      height = "500px"
    ),
    
    #card5
    card(
      card_header("In vs Out Degree by District"),
      "Which districts are being talked about vs which are driving the conversation?",
      plotOutput("district_degree"),
      height = "500px", 
      "District 12 talks about others slightly more then they are talked about. The capital is 
      talked about slightly more then they talk about others. Then there are districts like districts 3 
      and 4 that are talked about way more frequntly then they talk about other districts. It is important 
      to note that this could be due to screen time. So since the main characters are from district 12, their out-degree 
      will be very high because they are ferquenlty conversating on screen about others. "
    ),
    
    #card6 vis network
    card(card_header("Degree Centrality by Character"), 
         "we can use the package VisNetwork to make it happen", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                      "Betweenness Centrality" = "betweenness"), 
         selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
    )
  )


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  
# let's create a simple example network with 10 nodes and calulate the degree centrality

  # CARD 2 
  
network <- reactive({
  data_nodes <-  read.csv("Hungergames_nodes.csv")
  data_edges <-  read.csv("Hungergames_edges (3).csv")
  
  data_edges <- data_edges |>
    rename(from = source, to = target)
  
  characters_net <- tbl_graph(nodes = data_nodes,
                              edges = data_edges,
                              directed = TRUE,
                              node_key = "Characters")
  
  characters_net <- characters_net |>
    activate(nodes) |>
    mutate(degree      = centrality_degree(loops = TRUE),
           indegree    = centrality_degree(mode = "in"),
           outdegree   = centrality_degree(mode = "out"),
           betweenness = centrality_betweenness(normalized = TRUE))
  
  characters_net
  })


# now let's get it visualized and reactive to our choice from above! 
#comes from ui output above
#card 2
output$characters_net <- renderPlot({
  characters_net <- network() 
  
  p <- ggraph(characters_net, layout = "auto") +
    geom_edge_link(aes(width = weight), alpha = 0.3) + 
    scale_edge_width(range = c(0.1, 3)) + 
    geom_node_point(aes(color = .data[[input$color]]), size = 5) + 
    geom_node_text(aes(label = Characters), color = "black") + 
    theme_void() + 
    theme(legend.position = "none")
  
  p
  
})
#card3
output$degree_scatter <- renderPlot({
  characters_df <- network() |>
    activate(nodes) |>
    as_tibble()
  
  ggplot(characters_df, aes(x = outdegree,
                            y = indegree,
                            color = Characters,
                            label = Characters)) +
    geom_point(size = 4) +
    geom_text_repel(aes(label = Characters), size = 4, color = "black") +
    theme_minimal() +
    labs(x     = "Out-Degree (Character talks about others)",
         y     = "In-Degree (Character is talked about by others)",
         color = "Character",
         title = "In-Degree vs Out-Degree")
})

#card4
  observe({
    characters_net <- network()
    char_names <- characters_net |>
      activate(nodes) |>
      as_tibble() |>
      pull(Characters)
    
    updateSelectInput(inputId = "remove_char",
                      choices = char_names,
                      selected = "Katniss")
  })
  
  output$no_char_net <- renderPlot({
    characters_net <- network()
    
    characters_net |>
      activate(nodes) |>
      filter(Characters != input$remove_char) |>
      mutate(betweenness = centrality_betweenness()) |>
      ggraph(layout = "auto") +
      geom_edge_link(aes(width = weight), alpha = 0.3) +
      scale_edge_width(range = c(0.1, 3)) +
      geom_node_point(aes(color = District, size = betweenness)) +
      geom_node_text(aes(label = Characters), repel = TRUE, size = 4) +
      theme_void() +
      labs(title = paste("Betweenness Centrality Without", input$remove_char),
           color = "District",
           size = "Betweenness")
  })
  
#card5 
  output$district_degree <- renderPlot({
    characters_df <- network() |>
      activate(nodes) |>
      as_tibble()
    
    characters_df |>
      group_by(District) |>
      summarise(avg_indegree  = mean(indegree),
                avg_outdegree = mean(outdegree)) |>
      pivot_longer(cols      = c(avg_indegree, avg_outdegree),
                   names_to  = "type",
                   values_to = "value") |>
      ggplot(aes(x    = reorder(District, value),
                 y    = value,
                 fill = type)) +
      geom_col(position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(x     = "District",
           y     = "Average Degree",
           fill  = "Type",
           title = "In vs Out Degree by District")
  })
  
# we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 
#card6
  output$int_network <- renderVisNetwork({
    characters_net <- network()
    
    nodes_df <- characters_net |>
      activate(nodes) |>
      as_tibble() |>
      rowid_to_column("id") |>
      mutate(label = Characters,
             value = if_else(input$size_by == "degree", degree, betweenness))
    
    edges_df <- characters_net |>
      activate(edges) |>
      as_tibble() 
    
    visNetwork(nodes_df, edges_df) |>
    visNodes(borderWidth = 1, 
             color = list(
               background= "pink", 
               border = "red", 
               highlight =  "purple"))|>
    
    visEdges(
      color = list(color = "purple", highlight = "black")) |> 
    
    visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE), 
      nodesIdSelection = FALSE) |>
    
    visInteraction(
      dragNodes = TRUE, 
      dragView = TRUE, 
      zoomView = TRUE) |> 
    
    visPhysics(stabilization = TRUE)
    
})

}

# Run the application 
shinyApp(ui = ui, server = server)



