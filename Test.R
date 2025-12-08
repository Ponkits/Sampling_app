# Test.R - graphiques pour Shiny (version modifiée pour axes/titres uniformes)

# ---- LIBRARIES ----
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(ggforce)
library(tidyr)     # pour complete()
library(forcats)   # pour fct_relevel()
library(cowplot)   # pour theme_cowplot si nécessaire (optionnel)
library(here)
wd=getwd()
# ---- THÈME UNIFORME ----
theme_graph <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text   = element_text(face = "bold", size = 12)
  )

# ---- CHARGEMENT DES DONNÉES ----
JDDC <-read_excel(file.path(wd,"Coastal_sampling.xlsx"))%>% 
  mutate(across(everything(), as.character))
JDDP <- read_excel(file.path(wd,"Pelagic_Sampling.xlsx"))%>% 
  mutate(across(everything(), as.character))
JDDD <- read_excel(file.path(wd,"Deepwater_Sampling.xlsx"))%>% 
  mutate(across(everything(), as.character))

JDD_all <- bind_rows(
  JDDC %>% mutate(Categorie = "Coastal"),
  JDDP %>% mutate(Categorie = "Pelagic"),
  JDDD %>% mutate(Categorie = "Deepwater")
)

# ---- GRAPH: NOMBRE D'ÉCHANTILLONS PAR CATÉGORIE ----
GSample_cat <- ggplot(JDD_all, aes(x = Categorie)) +
  geom_bar(fill = "#112446") +
  labs(title = "Number of samples per habitat", x = "Habitat", y = "Number of samples") +
  theme_graph

# ---- UTIL: vecteur mois complet (ordre) ----
month_levels <- month.abb

# ---- FUNCTION: SIZE-SPECIES with constant X and Y axes ----
create_size_hist <- function(df, title_prefix = "", ncol = 5) {
  
  df <- df %>% mutate(FL = as.numeric(`FL (mm)`)) %>% filter(!is.na(FL))
  species_keys <- unique(df$`Species key`)
  
  # ---- GLOBAL RANGES ----
  global_min <- min(df$FL)
  global_max <- max(df$FL)
  bins <- seq(global_min, global_max, length.out = 31)
  
  # ---- FIND GLOBAL MAX COUNT FOR CONSTANT Y AXIS ----
  global_max_count <- max(
    df %>%
      mutate(bin = cut(FL, breaks = bins, include.lowest = TRUE)) %>%
      count(`Species key`, bin) %>%
      pull(n),
    na.rm = TRUE
  )
  
  # ---- PLOT PER SPECIES ----
  plots <- lapply(species_keys, function(sp) {
    dsub <- df %>% filter(`Species key` == sp)
    if (nrow(dsub) == 0) return(NULL)
    
    ggplot(dsub, aes(x = FL)) +
      geom_histogram(breaks = bins, fill = "#112446", color = NA) +
      scale_x_continuous(limits = c(global_min, global_max),
                         breaks = pretty(c(global_min, global_max), n = 5)) +
      scale_y_continuous(limits = c(0, global_max_count)) +
      labs(title = sp, x = "FL (mm)", y = "Number of samples") +
      theme_graph +
      theme(plot.title = element_text(face = "bold", size = 10, hjust = 0.5))
  })
  
  plots <- plots[!sapply(plots, is.null)]
  
  wrap_plots(plots, ncol = ncol) +
    theme_graph +
    plot_annotation(title = paste("Fork length (mm) —", title_prefix))
}

# ---- FUNCTION: COUNT PER SEX (liste de plots + wrap_plots) ----
create_count_sex <- function(df, title_prefix = "", ncol = 5) {
  
  df <- df %>% mutate(FL = as.numeric(`FL (mm)`))  # FL non utile ici mais ok
  species_keys <- unique(df$`Species key`)
  
  sex_levels <- c("F", "M", "I", "U")
  
  # ---- Trouver le maximum global pour fixer l'axe Y ----
  max_count <- df %>%
    group_by(`Species key`, Sex) %>%
    summarise(N = n(), .groups = "drop") %>%
    pull(N) %>%
    max(na.rm = TRUE)
  
  plots <- lapply(species_keys, function(sp) {
    
    dsub <- df %>% filter(`Species key` == sp)
    
    stats <- dsub %>%
      group_by(Sex) %>%
      summarise(N = n(), .groups = "drop") %>%
      complete(Sex = sex_levels, fill = list(N = 0)) %>%
      mutate(Sex = factor(Sex, levels = sex_levels))
    
    ggplot(stats, aes(x = Sex, y = N, fill = Sex)) +
      geom_col() +
      scale_y_continuous(limits = c(0, max_count)) +
      scale_fill_manual(values = c("F"="firebrick2","M"="dodgerblue2",
                                   "I"="forestgreen","U"="grey")) +
      labs(title = sp, x = "Sex", y = "Number of individuals") +
      theme_graph +
      theme(
        plot.title = element_text(face="bold", size=10, hjust=0.5),
        legend.position = "none"
      )
  })
  
  wrapped <- wrap_plots(plots, ncol = ncol, guides = "collect")
  wrapped <- wrapped & theme(legend.position = "right")
  
  wrapped + plot_annotation(
    title = paste("Number of individuals by sex —", title_prefix),
    theme = theme(plot.title = element_text(face="bold", size=14, hjust=0.5))
  )
}

# ---- FUNCTION: SAMPLES PER MONTH (axe Y constant, dates reconnues, titre centré et bold) ----
library(lubridate)  # <---- IMPORTANT
create_monthly <- function(df, title_prefix = "", ncol = 5) {
  
  # Correspondance français -> anglais abrégé
  month_translation <- c(
    "janvier"   = "Jan",
    "février"   = "Feb",
    "mars"      = "Mar",
    "avril"     = "Apr",
    "mai"       = "May",
    "juin"      = "Jun",
    "juillet"   = "Jul",
    "août"      = "Aug",
    "septembre" = "Sep",
    "octobre"   = "Oct",
    "novembre"  = "Nov",
    "décembre"  = "Dec"
  )
  
  # Ordre des mois anglais
  month_levels <- month.abb
  
  df2 <- df %>%
    mutate(
      # Traduction du mois
      Month = month_translation[tolower(trimws(Month))],
      Month = factor(Month, levels = month_levels, ordered = TRUE),
      Species = `Species key`
    ) %>%
    group_by(Species, Month) %>%
    summarise(N = n(), .groups = "drop")
  
  species_keys <- unique(df2$Species)
  max_N <- max(df2$N, na.rm = TRUE)
  
  plots <- lapply(species_keys, function(sp) {
    
    dsub <- df2 %>%
      filter(Species == sp) %>%
      complete(
        Month = factor(month_levels, levels = month_levels, ordered = TRUE),
        fill = list(N = 0)
      )
    
    ggplot(dsub, aes(x = Month, y = N)) +
      geom_col(fill = "#112446") +
      scale_x_discrete(drop = FALSE) +
      labs(title = sp, x = "Month", y = "Number of samples") +
      theme_minimal() +
      theme(
        plot.title = element_text(face="bold", size=12, hjust=0.5),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )
  })
  
  wrapped <- wrap_plots(plots, ncol = ncol, guides = "collect")
  wrapped & 
    scale_y_continuous(limits = c(0, max_N)) &
    plot_annotation(
      title = paste("Number of samples per month —", title_prefix),
      theme = theme(plot.title = element_text(face="bold", size=14, hjust=0.5))
    )
}

# ---- COASTAL GRAPHS ----
nspecies <- ggplot(JDDC, aes(y = `Species key`)) +
  geom_bar(fill="#112446") +
  labs(title="Number of samples per species — Coastal", y="Species", x="Number of samples") +
  theme_graph

sample_eez <- ggplot(JDDC, aes(x = EEZ, y = `Species key`)) +
  geom_tile() +
  labs(title="EEZ per species — Coastal", x="EEZ", y="Species") +
  theme_graph

size_speciesc  <- create_size_hist(JDDC, "Coastal", ncol = 5)
sex_speciesc   <- create_count_sex(JDDC, "Coastal", ncol = 5)
Month_speciesc <- create_monthly(JDDC, "Coastal", ncol = 5)

# ---- PELAGIC GRAPHS ----
nspecies_pelagic <- ggplot(JDDP, aes(y = `Species key`)) +
  geom_bar(fill="#112446") +
  labs(title="Number of samples per species — Pelagic", y="Species", x="Number of samples") +
  theme_graph

sample_eez_pelagic <- ggplot(JDDP, aes(x = EEZ, y = `Species key`)) +
  geom_tile() +
  labs(title="EEZ per species — Pelagic", x="EEZ", y="Species") +
  theme_graph

size_speciesp  <- create_size_hist(JDDP, "Pelagic", ncol = 5)
sex_speciesp   <- create_count_sex(JDDP, "Pelagic", ncol = 5)
Month_speciesp <- create_monthly(JDDP, "Pelagic", ncol = 5)

# ---- DEEPWATER GRAPHS ----
nspecies_deepwater <- ggplot(JDDD, aes(y = `Species key`)) +
  geom_bar(fill="#112446") +
  labs(title="Number of samples per species — Deepwater", y="Species", x="Number of samples") +
  theme_graph

sample_eez_deepwater <- ggplot(JDDD, aes(x = EEZ, y = `Species key`)) +
  geom_tile() +
  labs(title="EEZ per species — Deepwater", x="EEZ", y="Species") +
  theme_graph

size_speciesD  <- create_size_hist(JDDD, "Deepwater", ncol = 5)
sex_speciesD   <- create_count_sex(JDDD, "Deepwater", ncol = 5)
Month_speciesD <- create_monthly(JDDD, "Deepwater", ncol = 5)

