# ============================================================
# Shiny App — Eligibility Analysis
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(shinymanager)
library(binom)

credentials <- data.frame(
  user     = c("admin"),
  password = c("courgette"),
  stringsAsFactors = FALSE
)

# ---- Load data & fit model ONCE at startup ------------------
#data_raw glm + SNR_numeric min + SNR_numeric mx
#n_trials = 31
SNR_min = -18
SNR_max= 6
glm_fit=readRDS("model.rds") 

snr_grid <- data.frame(
  SNR_numeric = seq(SNR_min,
                    SNR_max ,
                    length.out = 300)
)
snr_grid$fit     <- predict(glm_fit, newdata = snr_grid, type = "response")
snr_grid$fit_3dB <- predict(glm_fit,
                            newdata = data.frame(SNR_numeric = snr_grid$SNR_numeric - 3),
                            type = "response")

snr_levels  = c(-18, -15, -12,  -9,  -6,  -3,   0,   3,   6)

# ---- CI helper ----------------------------------------------
compute_ci <- function(p, n, level, method) {
  z <- qnorm(1 - (1 - level) / 2)
  if (method == "wilson") {
    ci_wilson = binom.confint(p * n, n, conf.level = level, methods = "wilson")
    #centre <- (p + z^2 / (2*n)) / (1 + z^2 / n)
    #margin <- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / (1 + z^2/n)
    #lwr <- centre - margin
    #upr <- centre + margin
    lwr = ci_wilson$lower
    upr = ci_wilson$upper 
  } else {  # wald
    #margin <- z * sqrt(p * (1 - p) / n)
    #lwr <- p - margin
    #upr <- p + margin
    ci_wald = binom.confint(p * n, n, conf.level = level, methods = "asymptotic")
    lwr = ci_wald$lower
    upr = ci_wald$upper 
  }
  list(lwr = max(0, lwr), upr = min(1, upr))
}

# ---- UI -----------------------------------------------------
ui_content <- fluidPage(
  titlePanel("Éligibilité au remboursement"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Participant"),
      sliderInput("snr", "SNR testé (dB)",
                  min = SNR_min ,
                  max = SNR_max ,
                  value = -9, step = 1),
      numericInput("perf", "Performance (%)",
                   value = 50, min = 0, max = 100, step = 1),
      numericInput("ntrials","Nombre d'essais", value = 32, min = 0, max=250, step=1),
      
      hr(),
      h4("Intervalle de confiance"),
      radioButtons("ci_method", "Méthode",
                   choices = c("Wilson" = "wilson", "Wald" = "wald"),
                   selected = "wilson", inline = TRUE),
      sliderInput("ci_level", "Niveau de confiance (%)",
                  min = 80, max = 99, value = 95, step = 1),
      
      hr(),
      h4("Résultat"),
      uiOutput("verdict")
    ),
    
    mainPanel(
      plotOutput("plot",height="500px"),
      tableOutput("ci_table")
    )
  )
)

ui = secure_app(ui_content)
# ---- Server -------------------------------------------------
server <- function(input, output, session) {
  #shinymanager
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  result <- reactive({
    p   <- input$perf / 100
    snr <- input$snr
    lvl <- input$ci_level / 100
    n_trials <- input$ntrials
    
    ci        <- compute_ci(p, n_trials, lvl, input$ci_method)
    threshold <- predict(glm_fit,
                         newdata = data.frame(SNR_numeric = snr - 3),
                         type = "response")
    eligible  <- ci$upr < threshold
    
    list(p = p, snr = snr, lwr = ci$lwr, upr = ci$upr,
         threshold = threshold, eligible = eligible)
  })
  
  output$verdict <- renderUI({
    r <- result()
    if (r$eligible) {
      tags$div(style = "font-size:1.4em; font-weight:bold; color:#548a37;",
               "✓ ÉLIGIBLE")
    } else {
      tags$div(style = "font-size:1.4em; font-weight:bold; color:#94262e;",
               "✗ NON ÉLIGIBLE")
    }
  })
  
  output$ci_table <- renderTable({
    r <- result()
    data.frame(
      `Info` = c("Performance", paste0("IC borne basse"), paste0("IC borne haute"), "Seuil (courbe +3 dB)"),
      `Pourcentage` = round(c(r$p, r$lwr, r$upr, r$threshold) * 100, 1)
    )
  }, digits = 1)
  
  output$plot <- renderPlot({
    r <- result()
    col <- ifelse(r$eligible, "#548a37", "#94262e")
    
    ggplot() +
      geom_ribbon(data = snr_grid,
                  aes(x = SNR_numeric, ymin = 0, ymax = fit_3dB),
                  fill = "#548a37", alpha = 0.15) +
      geom_line(data = snr_grid,
                aes(x = SNR_numeric, y = fit, colour = "Courbe normalisée"),
                linewidth = 1.1) +
      geom_line(data = snr_grid,
                aes(x = SNR_numeric, y = fit_3dB, colour = "+3 dB"),
                linewidth = 1.1, linetype = "dashed") +
      #stat_summary(data = data, aes(x = SNR_numeric, y = prop),
       #            geom="point",fun="mean",size=3,show.legend=F,color = "#94262e")+
      geom_errorbar(aes(x = r$snr, ymin = r$lwr, ymax = r$upr),
                    colour = col, width = 0.3, linewidth = 1.3) +
      geom_point(aes(x = r$snr, y = r$p),
                 colour = col, size = 5, shape = 18) +
      annotate("text",
               x = -15 , y = 0.05,
               label = "Zone éligible", colour = "#548a37",
               hjust = 0, size = 4, fontface = "italic") +
      annotate("text",
               x = r$snr + 0.3, y = r$p + 0.06,
               label = ifelse(r$eligible, "ÉLIGIBLE ✓", "NON ÉLIGIBLE ✗"),
               colour = col, hjust = 0, size = 4.5, fontface = "bold") +
      scale_colour_manual(values = c("Courbe normalisée" = "#94262e", "+3 dB" = "#548a37")) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_x_continuous(breaks = snr_levels, labels = snr_levels) +
      labs(title = "Courbe psychométrique — Zone d'éligibilité",
           x = "SNR (dB)", y = "Pourcentage de Réponses correctes", colour = NULL) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
