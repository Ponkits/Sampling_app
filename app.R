###################
# app.R
# Pons Alexandre
# 03/12/2025
####################

# ---- PACKAGES GLOBAUX ----
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(patchwork)
library(lubridate)
library(ggforce)
library(rsconnect)

# ---- CHARGEMENT DES FONCTIONS / GRAPHIQUES ----
source("Test.R")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Ponkito gaing Internal visualisation secured"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Visualisation des données d'échantillonnage"),
      helpText("Les graphiques se mettent automatiquement à jour toutes les 5 secondes."),
      
      # ---- Sélection habitat ----
      selectInput("selected_habitat", "Choisir un habitat",
                  choices = unique(JDD_all$Categorie)),
      
      # ---- Sélection code d'espèce dynamique ----
      uiOutput("species_code_ui"),
      
      # ---- Affichage nom complet espèce ----
      uiOutput("species_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General", plotOutput("graph_cat", height = "900px")),
        tabPanel("Côtier",
                 tabsetPanel(
                   tabPanel("Espèce", plotOutput("g_species_coastal", height = "900px")),
                   tabPanel("EEZ", plotOutput("g_eez_coastal", height = "900px")),
                   tabPanel("Taille", plotOutput("g_size_coastal", height = "1200px")),
                   tabPanel("Sexe", plotOutput("g_sex_coastal", height = "1200px")),
                   tabPanel("Mois", plotOutput("g_month_coastal", height = "1200px"))
                 )
        ),
        tabPanel("Pélagique",
                 tabsetPanel(
                   tabPanel("Espèce", plotOutput("g_species_pelagic", height = "900px")),
                   tabPanel("EEZ", plotOutput("g_eez_pelagic", height = "900px")),
                   tabPanel("Taille", plotOutput("g_size_pelagic", height = "1200px")),
                   tabPanel("Sexe", plotOutput("g_sex_pelagic", height = "1200px")),
                   tabPanel("Mois", plotOutput("g_month_pelagic", height = "1200px"))
                 )
        ),
        tabPanel("Deepwater",
                 tabsetPanel(
                   tabPanel("Espèce", plotOutput("g_species_dw", height = "900px")),
                   tabPanel("EEZ", plotOutput("g_eez_dw", height = "900px")),
                   tabPanel("Taille", plotOutput("g_size_dw", height = "1200px")),
                   tabPanel("Sexe", plotOutput("g_sex_dw", height = "1200px")),
                   tabPanel("Mois", plotOutput("g_month_dw", height = "1200px"))
                 )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Réactif : filtrer les espèces selon habitat ----
  filtered_species <- reactive({
    req(input$selected_habitat)
    JDD_all %>%
      filter(Categorie == input$selected_habitat) %>%
      select(`Species key`, Species) %>%
      distinct()
  })
  
  # ---- UI dynamique pour le choix du code d'espèce ----
  output$species_code_ui <- renderUI({
    df <- filtered_species()
    selectInput("selected_code", "Choisir un code d'espèce",
                choices = df$`Species key`)
  })
  
  # ---- Affichage nom complet de l'espèce ----
  output$species_info <- renderUI({
    req(input$selected_code)
    df <- filtered_species()
    full_name <- df %>% filter(`Species key` == input$selected_code) %>% pull(Species)
    HTML(paste0("<b>", input$selected_code, "</b> = ", full_name))
  })
  
  # ---- GLOBAL CATEGORIES ----
  output$graph_cat <- renderPlot({ GSample_cat })
  
  # ---- CÔTIER ----
  output$g_species_coastal <- renderPlot({ nspecies })
  output$g_eez_coastal     <- renderPlot({ sample_eez })
  output$g_size_coastal    <- renderPlot({ size_speciesc })
  output$g_sex_coastal     <- renderPlot({ sex_speciesc })
  output$g_month_coastal   <- renderPlot({ Month_speciesc })
  
  # ---- PÉLAGIQUE ----
  output$g_species_pelagic <- renderPlot({ nspecies_pelagic })
  output$g_eez_pelagic     <- renderPlot({ sample_eez_pelagic })
  output$g_size_pelagic    <- renderPlot({ size_speciesp })
  output$g_sex_pelagic     <- renderPlot({ sex_speciesp })
  output$g_month_pelagic   <- renderPlot({ Month_speciesp })
  
  # ---- DEEPWATER ----
  output$g_species_dw <- renderPlot({ nspecies_deepwater })
  output$g_eez_dw     <- renderPlot({ sample_eez_deepwater })
  output$g_size_dw    <- renderPlot({ size_speciesD })
  output$g_sex_dw     <- renderPlot({ sex_speciesD })  
  output$g_month_dw   <- renderPlot({ Month_speciesD })
}

# ---- LANCEMENT APP ----
shinyApp(ui, server)



