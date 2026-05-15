# Shiny App 
# Molly Nordquist
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(ggrepel)
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
  
  navlistPanel(
    widths = c(2, 10),  # sidebar width vs content width
    well   = TRUE,
    
    # ── sidebar nav items ──────────────────────────────────────
    nav_panel("Introduction",
              div(style = "color: #c41e3a; font-weight: bold; font-size: 28px; padding: 20px 0px;",
                  "'Hunger Games: Catching Fire' Rebellion Influence Network"),
              card(
                card_header("About This Project"),
                "This project is focused on the gossip network in Catching Fire.
                 Connections exist between characters when they reference each other
                 in conversation. The goal is to analyze who is talked about the most, in the movie to be able to determine which characters are most central to conversations about rebellion and information flow, and who holds the most influence.Centrality measures and in-degree/out-degree
                 measures will be used to allow users to make insights to what they see to be as powerful/influential in rebellion  ", 
                card_header("About the Data and Network"), 
                "The data was collected scene by scene. A scene was defined
by a continuous segment with no cutting. A character’s conversation was classified as dialogue
between at least two characters within one scene. In each scene, dialogue was analyzed to
identify when one character referred to another. A directed edge was created from character A to
character B when character A referred to character B as the subject of dialogue. References were
counted when a character’s name was used or when a clear descriptor of a character was used.
To avoid overcounting, each source-target edge was recorded only once per scene. If a character
referred to the same character multiple times in a scene, it was only counted once. If the same
reference occurred in a different scene, it was counted again. Directly addressing another
character (ex: Katniss yelling Peeta’s name) was not counted unless that character was being
discussed as a topic.", br(),
                "Each node represents all the characters that appear on screen or are clearly
referenced in dialogue. The character is then represented with the district they are originally
from. The character’s role in the movie is attributed to them, such as being a tribute in the games,
a mentor, a stylist, a game maker, part of the Hunger Games production team, the president of
Panem, or just a family member or friend of the main characters. Gender is also noted" , br(),
                "Edges are defined to be references within conversation to the other character." , br(),  
                "The weight represents the number of scenes that
the source character talks about the target. An edge weight of one is added for each new scene in
which the source character refers to the target character. If a character refers to the same
individual multiple times in a scene, it only counts as one weight, as edge weight is only added
once per scene.", br(),
                "This is a weighted, directed network.", 
                card_header("Before jumping into the website, here are some facts to keep in mind:", br(), br(),
                            "Katniss is the main character of this story. She lives in District 12. The capitol is where the president 
                            lives and all the upperclass citizens. The Hunger games is hosted by the capitol where the tributes for the 
                            games are from the 12 districts.", br(),
                            "Now, look at basic network visuals below and play around with the attributes and connections!")
              ),
              card(
                card_header("Character Network"),
                selectInput("color",
                            "Color nodes by:",
                            choices = list("Gender"   = "gender",
                                           "District" = "District", 
                                           "Role in movie" = "Role"),
                            selected = "gender"),
                plotOutput("characters_net"),
                height = "600px"
              ),
              card(
                card_header("Try removing characters to see if this changes the networks!"),
                card_header("Network With Character Removed"),
                selectInput("remove_char",
                            "Select character to remove:",
                            choices = NULL),
                plotOutput("no_char_net"),
                height = "500px"
              )
    ),
    
    
    
    nav_panel("Degree Analysis",
              card(
                card_header("Quick Defn! ", br(), "In-Degree represents a character getting talked about in a scene, out-degree means a character talking about another character in scene."),
                card_header("In-Degree vs Out-Degree"),
                plotOutput("degree_scatter"),
                height = "900px", 
                "Possible takeaways: 
                The y-axis represents the in-degree aka of how much a character is 
                talked about by others. So the higher up on the y-axis, the more a 
                character is talked about. The x-axis represents out-degree, so how 
                much a character talks about other characters. So the farther right a
                character is the more frequently they mention other characters.", br(),
                "Characters in the top right talk about others a lot and are talked about a lot. Characters 
                in the bottom left don't talk about others and don't get talked about much. From 
                this we can conclude that Katniss is the most talked about character and talks 
                about other characters the most, which makes sense since she is the main character 
                and the story follows her. Peeta is a character who is talked about frequently by 
                other characters, as he is all the way at the top of the in-degree metric, but isn't 
                referenced more than Haymitch or Katniss, as he is about halfway across the out-degree
                axis. Someone like Haymitch would be a very influential character because his out-degree 
                is proportionally larger than in-degree, meaning he is spreading a lot of information, 
                but maybe he is more on the down low about his information spread. An interesting observation 
                is President Snow is talked about more then he talks about others, but the amount he is talked about is less than Peeta and Katniss,
                as well as the same amount as characters such as Finnick or Mags or Wirus. Therefore suggesting Snow may be less central to information flow then expected, despite having political authority over Panem."
              ),
              card(
                card_header("In vs Out Degree by District"),
                plotOutput("district_degree"),
                height = "700px",
                "District 12 talks about others slightly more than they are talked about. 
                The Capitol is talked about slightly more than they talk about others. Then 
                there are districts like districts 3 and 4 that are talked about way more frequently 
                than they talk about other districts. It is important to note that this could be due 
                to screen time. So since the main characters are from district 12, their out-degree 
                will be very high because they are frequently conversating on screen about others."
              )
    ),
    
    nav_panel("Character Comparison",
              card(
                card_header("Some more quick defns!", br(), 
                           "Betweeness Centrality measures how often a character is on the shortest paths connecting other characters, meaning they act as though they are a bridge to information flow.", br(),
                           "Closeness Centrality is measured by taking 1 and dividing it by sum of distances. A high closeness centrality means a character is close to other characters in the network so they can therefore spread or retreive information very efficiently. Closer to 0 means a low closeness, so a character is more isolated."),
                card_header("Compare Characters"),
                selectInput("compare_char",
                            "Compare Katniss' centrality measures to:",
                            choices = NULL),
                navset_tab(
                  nav_panel("In vs Out Degree",
                            plotOutput("katniss_degree")),
                  nav_panel("Centrality Summary",
                            verbatimTextOutput("katniss_stats"))
                ),
                height = "800px"
              ),
              nav_panel("President Snow Analysis",
                        card(
                          card_header("Compare President Snow's centrality measures to"),
                          selectInput("compare_snow",
                                      "Compare President Snow to:",
                                      choices = NULL),
                          navset_tab(
                            nav_panel("In vs Out Degree",
                                      plotOutput("snow_degree")),
                            nav_panel("Centrality Summary",
                                      verbatimTextOutput("snow_stats"))
                          ),
                          height = "700px"
                        )
              ),
    ),
    
  )
)
#getting background of graphs match the app background
graph_theme <- theme(
  plot.background = element_rect(fill = "#1a1a2e", color = NA),
  panel.background = element_rect(fill = "#1a1a2e", color = NA),
  legend.background = element_rect(fill = "#1a1a2e", color = NA),
  legend.key = element_rect(fill = "#1a1a2e", color = NA),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  text = element_text(color = "#e0e0e0"),
  plot.title = element_text(color = "#e0e0e0", face = "bold"),
  legend.text = element_text(color = "#e0e0e0"),
  legend.title = element_text(color = "#e0e0e0")
)

chart_theme <- theme(
  plot.background = element_rect(fill = "#1a1a2e", color = NA),
  panel.background = element_rect(fill = "#1a1a2e", color = NA),
  legend.background = element_rect(fill = "#1a1a2e", color = NA),
  legend.key = element_rect(fill = "#1a1a2e", color = NA),
  text = element_text(color = "#e0e0e0"),
  axis.text = element_text(color = "#e0e0e0"),
  axis.title = element_text(color = "#e0e0e0"),
  plot.title = element_text(color = "#e0e0e0", face = "bold"),
  legend.text = element_text(color = "#e0e0e0"),
  legend.title = element_text(color = "#e0e0e0")
)
# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  


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
           betweenness = centrality_betweenness(normalized = TRUE), 
           closeness = centrality_closeness(normalized = TRUE))
  
  characters_net
  })


# now let's get it visualized and reactive to our choice from above! 
#comes from ui output above
#card 2
output$characters_net <- renderPlot({
  characters_net <- network() 
  
  p <- ggraph(characters_net, layout = "auto") +
    geom_edge_link(aes(width = weight), color = "white", alpha = 0.3) + 
    scale_edge_width(range = c(0.1, 3)) + 
    geom_node_point(aes(color = .data[[input$color]]), size = 5) + 
    geom_node_text(aes(label = Characters), color = "lightblue") + 
    theme_void() + 
    graph_theme +
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
    geom_text_repel(aes(label = Characters), size = 4, color = "#e0e0e0") +
    theme_minimal() +
    chart_theme + 
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
      geom_edge_link(aes(width = weight), color="white", alpha = 0.3) +
      scale_edge_width(range = c(0.1, 3)) +
      geom_node_point(aes(color = District, size = betweenness)) +
      geom_node_text(aes(label = Characters), color = "lightblue", size = 4)+
      theme_void() +
      graph_theme +
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
      chart_theme +
      labs(x     = "District",
           y     = "Average Degree",
           fill  = "Type",
           title = "In vs Out Degree by District")
  })
  
  observe({
    char_names <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters != "Katniss") |>
      pull(Characters)
    
    updateSelectInput(inputId = "compare_char",
                      choices  = char_names,
                      selected = "Peeta")
  })
  
  output$katniss_degree <- renderPlot({
    characters_df <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters %in% c("Katniss", input$compare_char)) |>
      select(Characters, indegree, outdegree) |>
      pivot_longer(cols      = c(indegree, outdegree),
                   names_to  = "type",
                   values_to = "value")
    
    ggplot(characters_df, aes(x    = type,
                              y    = value,
                              fill = Characters)) +
      geom_col(position = "dodge", width = 0.5) +
      theme_minimal() +
      chart_theme +
      labs(x     = "Degree Type",
           y     = "Value",
           fill  = "Character",
           title = paste("Katniss vs", input$compare_char, ": In vs Out Degree"))
  })
  #card 
  output$katniss_stats <- renderPrint({
    characters_df <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters %in% c("Katniss", input$compare_char)) |>
      select(Characters, indegree, outdegree, betweenness, closeness)
    
    for (i in 1:nrow(characters_df)) {
      cat("Character:             ", characters_df$Characters[i], "\n")
      cat("In-Degree:             ", characters_df$indegree[i], "\n")
      cat("Out-Degree:            ", characters_df$outdegree[i], "\n")
      cat("Betweenness Centrality:", round(characters_df$betweenness[i], 4), "\n")
      cat("Closeness Centrality:  ", round(characters_df$closeness[i], 4), "\n")
      cat("-----------------------------------\n")
    }
  })

#card
  observe({
    char_names <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters != "Pres Snow") |>
      pull(Characters)
    
    updateSelectInput(inputId = "compare_snow",
                      choices  = char_names,
                      selected = "Katniss")
  })
  
  output$snow_degree <- renderPlot({
    characters_df <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters %in% c("Pres Snow", input$compare_snow)) |>
      select(Characters, indegree, outdegree) |>
      pivot_longer(cols      = c(indegree, outdegree),
                   names_to  = "type",
                   values_to = "value")
    
    ggplot(characters_df, aes(x    = type,
                              y    = value,
                              fill = Characters)) +
      geom_col(position = "dodge", width = 0.5) +
      theme_minimal() +
      chart_theme +
      labs(x     = "Degree Type",
           y     = "Value",
           fill  = "Character",
           title = paste("President Snow vs", input$compare_snow, ": In vs Out Degree"))
  })
  
  output$snow_stats <- renderPrint({
    characters_df <- network() |>
      activate(nodes) |>
      as_tibble() |>
      filter(Characters %in% c("Pres Snow", input$compare_snow)) |>
      select(Characters, indegree, outdegree, betweenness, closeness)
    
    for (i in 1:nrow(characters_df)) {
      cat("Character:             ", characters_df$Characters[i], "\n")
      cat("In-Degree:             ", characters_df$indegree[i], "\n")
      cat("Out-Degree:            ", characters_df$outdegree[i], "\n")
      cat("Betweenness Centrality:", round(characters_df$betweenness[i], 4), "\n")
      cat("Closeness Centrality:  ", round(characters_df$closeness[i], 4), "\n")
      cat("-----------------------------------\n")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



