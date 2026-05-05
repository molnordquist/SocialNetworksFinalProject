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
  
  titlePanel("'Hunger Games: Catching Fire' Rebellion influence Network "),
  
  page_sidebar(
    title = "subtitle here", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Introduction/Background"), "This project is focused on gossip network in the Hunger Games movie Catching Fire. This network is described to have connections between characters when characters refrence eachothe rin conversation. The goal of this project is to analyze who is talked about the most, who is a leading force in rebellion, who people focus on, etc. "),
    
    card(
      card_header("Dynamic Demo 1"), "you could put a caption like so",
      selectInput("select", 
                  "select an option", 
                  choices = list("Option A" = "A", 
                                 "Option B" = "B"),
                  selected =1), 
      textOutput("ourVariable")
      ), 
    
    
    card(card_header("here's a simple network of relations based on the above criteria"),
         selectInput("color",
                     "choose characteristic of the network for nodes", 
                     choices = list("Gender" = "gender", 
                                    "District" = "District"), 
                     selected = 1), 
         plotOutput("characters_net"), height = "400px"),
    
  
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
  
  characters_net
})

# now let's get it visualized and reactive to our choice from above! 
#comes from ui output above 
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

# CARD 3 

# we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 

network2 <- reactive({
  data_nodes <-  read.csv("Hungergames_nodes.csv")
  data_edges <-  read.csv("Hungergames_edges (3).csv")
  
  data_edges <- data_edges |>
    rename(from = source, to = target)
  
  characters_net <- tbl_graph(nodes = data_nodes,
                              edges = data_edges,
                              directed = TRUE,
                              node_key = "Characters")
  
  nodes_df <- characters_net |> 
    activate(nodes) |> 
    as_tibble() |> 
    rowid_to_column("id") |> 
    mutate(value = if (input$size_by == "degree") degree else betweenness) # have to give size based on "value" for visNetwork
  
  edges_df <- characters_net |> 
    activate(edges) |> 
    as_tibble() |> 
    rename(from = 1, to =2 )
  
  list(nodes = nodes_df, edges = edges_df)
  
  characters_net
})

output$int_network <- renderVisNetwork({
   net <- characters_net()
   nodes <- net2$nodes
   edges <- net2$edges 
  
   
  visNetwork(nodes, net$edges) |> 
    
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



