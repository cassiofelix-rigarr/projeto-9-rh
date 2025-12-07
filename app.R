# app.R — Projeto Recursos Humano Projeto 9 -------------------------------
library(shiny)
library(tidyverse)
library(highcharter)
library(ggplot2)
library(reactable)
library(lubridate)
library(scales)
library(fontawesome)

source("R/mod_login.R")
source("R/mod_dashboard.R")
source("R/helpers_supabase.R")
source("R/helpers_theme.R")

ui <- fluidPage(
  tags$head(
    # ================== GOOGLE FONTS ==================
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel        = "preconnect",
      href       = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Montserrat:wght@400;500;600;700;800&display=swap"
    ),
    # ================== CSS DO PROJETO =================
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  uiOutput("app_main")
)

server <- function(input, output, session) {
  
  user_rv <- reactiveValues(
    logged_in = FALSE,
    email     = NULL
  )
  
  callModule(mod_login_server,     "login",     user_rv = user_rv)
  callModule(mod_dashboard_server, "dashboard", user_rv = user_rv)
  
  output$app_main <- renderUI({
    if (!isTRUE(user_rv$logged_in)) {
      mod_login_ui("login")
    } else {
      mod_dashboard_ui("dashboard")
    }
  })
}

shinyApp(ui, server)
