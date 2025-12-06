# libraries
library(shiny)
library(shinydashboard)
library(visNetwork)
library(readxl)
library(dplyr)

# UI ----------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Spruce Budworm"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("System Diagram", tabName = "diagram", icon = icon("project-diagram")),
      menuItem("Quantified Diagram", tabName = "path_analyis_diagram", icon = icon("project-diagram")),
      menuItem("Sources table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # About section ----
      tabItem(tabName = "about",
              h3("About this RShiny Dashboard"),
              p("This Shiny Dashboard contains information related to the Eastern spruce budworm (",em("Choristoneura fumiferana"),
                ") and how their populations interact within the rest of the abiotic, biotic, and socio-economic ecosystem. 
                The objectives set forward by our team are to illustrate and quantify system linkages, to identify research gaps, 
                and to situate our findings in the context of climate change."),
              h4("Systems Diagram"),
              p("In the first systems diagram, all the connections are illustrated, and research gaps are shown in a separate panel. 
                The arrows between the different relationships represent either a positive relationship (green), a negative 
                relationship (red), or one that cannot be assigned a label (black). The user can click on different variables to obtain 
                a definition, move them around, and clearly see which other variables are connected to them."),
              h4("Path Analysis Diagram"),
              p("The second systems diagram, called “Path Analysis Diagram,” displays specifically the biotic and abiotic variables 
                that influence the spruce budworm in some way. These relationships were quantified through a literature review and 
                path analysis. The thickness of each arrow represents the strength of the relationship, which is the weighted z-score.When
                clicking on an arrow, a square indicating the weighted Z scores will appear. A thicker arrow represents a stronger relationship 
                and vice versa. 
                The level of transparency reflects the uncertainty associated with the relationship, as determined by the Tau squared value, 
                for variables where more than one paper was associated with it. For the others, a tau squared of 0.01 was awarded. This is 
                the lowest certainty, given that there was only one paper about the relationship.
                Given that the effect of fire on non-host abundance is very complex and requires mutliple interactions, we were not able 
                to quanitify it in this diagram, but felt that its importance could not be ignored, so this relationship is represented by 
                a grey arrow. 
                The variables on the left represent the exogenous variables, temperature and temperature fluctuations, in order to see
                how their effects flow through the system"),
              h4("Relationships Table"),
              p("A table with the papers we researched for the path analysis diagram, and their respective weighted Z scores and Tau squared values, 
              is available for consultation at the end.")
      ),
      
      # Complete Diagram Section----
      tabItem(tabName = "diagram",
              
              fluidRow(
                
                # Left column
                column(
                  width = 3,
                  
                  # Controls box (top)
                  box(width = 12, title = "Controls", solidHeader = TRUE, status = "primary",
                      checkboxGroupInput("showGroups_sys", "Show Groups:",
                                         choices = NULL, selected = NULL),
                      hr(),
                      h4("Selected Variable:"),
                      uiOutput("node_info_sys"),
                      hr()
                      
                      
                  ),
                  
                  # Gaps box (below Controls)
                  box(width = 12, title = "Gaps", solidHeader = TRUE, status = "warning",
                      tags$div(
                        style = "display:flex; flex-direction:column; align-items:flex-start; gap:14px; margin-top:10px;",
                        
                        lapply(c(
                          "Precipitation",
                          "Humidity",
                          "Wind",
                          "Public participation",
                          "Mi’kmaq Nation"
                        ), function(lbl) {
                          tags$div(
                            style = "display:flex; align-items:center; gap:10px;",
                            tags$div(
                              style = "width:18px; height:18px; border-radius:50%;
                                 background:#DDDDDD; border:1px solid #888888;"
                            ),
                            tags$span(lbl)
                          )
                        })
                      )
                  )
                ),
                
                # Right column
                column(
                  width = 9,
                  box(width = 12, title = "Interactive System Diagram", solidHeader = TRUE, status = "success",
                      visNetworkOutput("network_sys", height = "700px")
                  )
                )
              )
      ),      
      # Path analysis diagram section ----
      tabItem(tabName = "path_analyis_diagram",
              fluidRow(
                box(width = 3, title = "Controls", solidHeader = TRUE, status = "primary",
                    checkboxGroupInput("showGroups_path", "Show Groups:",
                                       choices = NULL, selected = NULL),
                    hr(),
                    h4("Selected Variable:"),
                    uiOutput("node_info_path"),
                    hr()
                ),
                box(width = 9, title = "Interactive System Diagram", solidHeader = TRUE, status = "success",
                    visNetworkOutput("network_path", height = "690px")
                )
              )
      ),
      
     
      
      # Data Table Section ----
      tabItem(tabName = "table",
              fluidRow(
                box(width = 12, title = "Path Analysis Sources", status = "primary",
                    DT::dataTableOutput("node_table"))
              )
      )
    )
  )
)
server <- function(input, output, session) {
    
    #  SYSTEM DIAGRAM ---------------------
    
    nodes_sys <- read_excel("data_sys.xlsx", sheet = "Nodes")
    edges_sys <- read_excel("data_sys.xlsx", sheet = "Edges")
    
    nodes_sys$label <- stringr::str_wrap(nodes_sys$label, width = 20)
    
    # Variable colors
    nodes_sys$color.background <- dplyr::case_when(
      nodes_sys$group == "Abiotic" ~ "#A6CEE3",
      nodes_sys$group == "Biotic" ~ "#C7E9B4",
      nodes_sys$group == "Budworm" ~ "#d9d3ea",
      nodes_sys$group == "Socio-economic" ~ "#fdd29a",
      TRUE ~ "gray"
    )
    
    # Relationship arrows colors
    if ("sign" %in% colnames(edges_sys)) {
      edges_sys$sign <- trimws(as.character(edges_sys$sign))
      edges_sys$color <- dplyr::case_when(
        edges_sys$sign == "+"  ~ "#8fca6d",
        edges_sys$sign == "-"  ~ "#c14040",
        edges_sys$sign == "?"  ~ "black",
        TRUE ~ "gray"
      )
    }
    
    # Controls
    observe({
      updateCheckboxGroupInput(session, "showGroups_sys",
                               choices = unique(nodes_sys$group),
                               selected = unique(nodes_sys$group))
    })
    
    # Filtered data for System Diagram
    filtered_sys <- reactive({
      req(input$showGroups_sys)
      n <- subset(nodes_sys, group %in% input$showGroups_sys)
      e <- subset(edges_sys, from %in% n$id & to %in% n$id)
      list(nodes = n, edges = e)
    })
    
    # Render System Diagram
    output$network_sys <- renderVisNetwork({
      data <- filtered_sys()
      data$nodes$x <- as.numeric(data$nodes$x)
      data$nodes$y <- as.numeric(data$nodes$y)
      
      legend_nodes <- data.frame(
        label = c("Abiotic", "Biotic", "Socio-economic", "Budworm"),
        color = c("#A6CEE3", "#C7E9B4", "#fdd29a", "#d9d3ea")
      )
      
      visNetwork(data$nodes, data$edges,
                 height = "750px", width = "100%") %>%
        visNodes(
          shape = "box",
          font = list(size = 18, multi = TRUE),
          widthConstraint = list(maximum = 250)
        ) %>%
        visEdges(
          arrows = "to",
          smooth = TRUE
        ) %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE
        ) %>%
        visInteraction(dragNodes = TRUE, zoomView = TRUE, dragView = TRUE) %>%
        visPhysics(enabled = FALSE) %>%
        visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 200,
          nodeSpacing = 150
        ) %>%
        visLegend(addNodes = legend_nodes, useGroups = FALSE, position = "left") %>%
        visInteraction(navigationButtons = TRUE)
    })
    
    #Description/definition of variable
    output$node_info_sys <- renderUI({
      req(input$network_sys_selected)
      node <- nodes_sys[nodes_sys$id == input$network_sys_selected, ]
      if (nrow(node) > 0) {
        tagList(
          node$label, "\n",
          h4("Description:"),
          p(node$description)
        )
      }
    })
    
    
    
    #  PATH ANALYSIS DIAGRAM -----------------------
    
    nodes_path <- read_excel("data_path.xlsx", sheet = "Nodes")
    edges_path <- read_excel("data_path.xlsx", sheet = "Edges")
    
    nodes_path$label <- stringr::str_wrap(nodes_path$label, width = 20)
    
    # Variable colors
    nodes_path$color.background <- dplyr::case_when(
      nodes_path$group == "Abiotic" ~ "#A6CEE3",
      nodes_path$group == "Biotic" ~ "#C7E9B4",
      nodes_path$group == "Budworm" ~ "#d9d3ea",
      TRUE ~ "#DDDDDD"
    )
    
    # Relationship arrow colors
    if ("sign" %in% colnames(edges_path)) {
      edges_path$color <- dplyr::case_when(
        edges_path$sign == "+"  ~ "mediumseagreen",
        edges_path$sign == "-"  ~ "red3",
        edges_path$sign == "±"  ~ "grey20",
        TRUE ~ "gray"
      )
    }
    
    # Relationship arrow thickness (strength)
    if ("strength" %in% colnames(edges_path)) {
      edges_path$width <- scales::rescale(edges_path$strength, to = c(2, 10))
    } else {
      edges_path$width <- 2
    }
    
    # Relationship arrow transparency (uncertainty)
    if ("uncertainty" %in% colnames(edges_path)) {
      alpha_vals <- as.integer(scales::rescale(edges_path$uncertainty, to = c(60, 255)))
      edges_path$color <- mapply(function(base_col, a) {
        rgb_val <- grDevices::col2rgb(base_col)
        grDevices::rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = a, maxColorValue = 255)
      }, edges_path$color, alpha_vals)
    }
    
    # Tooltip
    if ("notes" %in% colnames(edges_path)) {
      edges_path$title <- edges_path$notes
    }
    
    # Populate Path Diagram filter
    observe({
      updateCheckboxGroupInput(session, "showGroups_path",
                               choices = unique(nodes_path$group),
                               selected = unique(nodes_path$group))
    })
    
    # Filtered data for Path Diagram
    filtered_path <- reactive({
      req(input$showGroups_path)
      n <- subset(nodes_path, group %in% input$showGroups_path)
      e <- subset(edges_path, from %in% n$id & to %in% n$id)
      list(nodes = n, edges = e)
    })
    
    # Render Path Diagram
    output$network_path <- renderVisNetwork({
      data <- filtered_path()
      data$nodes$x <- as.numeric(data$nodes$x)
      data$nodes$y <- as.numeric(data$nodes$y)
      
      legend_nodes <- data.frame(
        label = c("Abiotic", "Biotic", "Budworm"),
        color = c("#A6CEE3", "#C7E9B4", "#d9d3ea")
      )
      
      visNetwork(data$nodes, data$edges,
                 height = "750px", width = "100%") %>%
        visNodes(
          shape = "box",
          font = list(size = 18, multi = TRUE),
          widthConstraint = list(maximum = 250)
        ) %>%
        visEdges(
          arrows = "to",
          smooth = TRUE
        ) %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE
        ) %>%
        visInteraction(dragNodes = TRUE, zoomView = TRUE, dragView = TRUE) %>%
        visPhysics(enabled = FALSE) %>%
        visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 200,
          nodeSpacing = 300,
          edgeMinimization = TRUE
        ) %>%
        visLegend(addNodes = legend_nodes, useGroups = FALSE)%>% 
        visInteraction(navigationButtons = TRUE)
    })
    
    # Description/definition of selected variable
    output$node_info_path <- renderUI({
      req(input$network_path_selected)
      node <- nodes_path[nodes_path$id == input$network_path_selected, ]
      if (nrow(node) > 0) {
        tagList(
          node$label, "\n",
          h4("Description:"),
          p(node$description)
        )
      }
    })
    
    
  
    #  TABLE TAB --------------------------------

    Relationships_table <- read_excel("Relationships_table.xlsx")
    
    output$node_table <- DT::renderDataTable({
      Relationships_table[, 1:12]
    }, options = list(
      scrollX = TRUE,
      scrollY = "400px",
      paging = TRUE,
      pageLength = 100,     
      lengthChange = FALSE,  
      columnDefs = list(
        list(
          targets = "_all",
          render = DT::JS(
            "function(data, type, row, meta) {",
            "  return type === 'display' && data != null ? '<div style=\"white-space: normal;\">' + data + '</div>' : data;",
            "}"
          )
        )
      )
    ))
    
    
    
}

# Merge ui and server to create dashboard
shinyApp(ui, server)



      