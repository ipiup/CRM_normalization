# Shiny App — CRM normalization and reimbursment eligibility  

library(shiny)
library(dplyr)
library(ggplot2)
library(shinymanager)
library(binom)

credentials <- data.frame(
  user = c("admin"), password = c("courgette"),
  stringsAsFactors = FALSE
)


# ---- CONFIG ----------

SNR_min <- -18
SNR_max  <-  6
glm_fit  <- readRDS("model.rds")

# SIB50 normatif : SNR pour lequel la courbe normative prédit p = 50 %
# Calculé depuis glm_fit (logit link) : SNR = -b0 / b1
b0_norm    <- coef(glm_fit)["(Intercept)"]
b1_norm    <- coef(glm_fit)["SNR_numeric"]
SIB50_REEL <- as.numeric(-b0_norm / b1_norm) #-11.1

snr_grid <- data.frame(SNR_numeric = seq(SNR_min, SNR_max, length.out = 300))
snr_grid$fit     <- predict(glm_fit, newdata = snr_grid, type = "response")
snr_grid$fit_3dB <- predict(glm_fit,
                            newdata = data.frame(SNR_numeric = snr_grid$SNR_numeric - 3),
                            type = "response")
snr_levels <- c(-18, -15, -12, -9, -6, -3, 0, 3, 6)

compute_ci <- function(p, n, level, method) {
  m  <- if (method == "wilson") "wilson" else "asymptotic"
  ci <- binom.confint(round(p * n), n, conf.level = level, methods = m)
  list(lwr = max(0, ci$lower), upr = min(1, ci$upper))
}

classify_zone <- function(p, upr, threshold) {
  if      (p   >= threshold) "green"
  else if (upr >= threshold) "orange"
  else                        "red"
}

zone_colour     <- function(z) switch(z, green="#548a37", orange="#e07b00", red="#94262e")
zone_bg         <- function(z) switch(z, green="#edf7e6",  orange="#fff5e6",  red="#fdeaea")
zone_border     <- function(z) switch(z, green="#a8d98a",  orange="#f5c07a",  red="#f0a0a8")
zone_text       <- function(z) switch(z, green="#2d6e18",  orange="#8a4a00",  red="#7a1520")
zone_icon       <- function(z) switch(z, green="✓",        orange="⚠",        red="✗")
zone_label_short <- function(z) switch(z,
                                       green  = "NORMAL",
                                       orange = "AMBIGUOUS",
                                       red    = "ABNORMAL"
)
zone_label_full <- function(z) switch(z,
                                      green  = "Mean and upper IC out of zone",
                                      orange = "Mean in zone, upper IC out of zone",
                                      red    = "Mean and upper IC in zone"
)

##

predict_sib50 <- function(snrs, perfs, n_trials) {
  # Methode : GLM binomial individuel ajuste sur les 3 points (SNR -6, -9, -12 dB)
  # intercept ET pente re-estimes librement — coherent avec le code de validation R2.
  # SIB50 = (logit(0.5) - b0) / b1 = -b0 / b1  (car logit(0.5) = 0)
  valid <- !is.na(perfs) & !is.na(snrs) & perfs > 0 & perfs < 1
  if (sum(valid) < 2) return(NA_real_)
  df <- data.frame(
    SNR_numeric = snrs[valid],
    prop        = perfs[valid],
    n           = n_trials[valid]
  )
  print(df)
  fit <- tryCatch(
    glm(prop ~ SNR_numeric, family = binomial, data = df),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  b0 <- coef(fit)["(Intercept)"]
  b1 <- coef(fit)["SNR_numeric"]
  as.numeric((log(0.5 / (1 - 0.5)) - b0) / b1)
}

#predict_sib50 <- function(snrs, perfs, base_model) {
#  valid <- !is.na(perfs) & !is.na(snrs) & perfs > 0 & perfs < 1
#  if (sum(valid) < 1) return(NA_real_)
#  beta1   <- coef(base_model)["SNR_numeric"]
#  logit_p <- log(perfs[valid] / (1 - perfs[valid]))
#  b0      <- mean(logit_p - beta1 * snrs[valid])
#  as.numeric(-b0 / beta1)
#}

build_result <- function(snr, perf_pct, n, lvl, method) {
  p         <- perf_pct / 100
  ci        <- compute_ci(p, n, lvl, method)
  threshold <- predict(glm_fit,
                       newdata = data.frame(SNR_numeric = snr - 3),
                       type = "response")
  list(snr=snr, p=p, lwr=ci$lwr, upr=ci$upr,n=n,
       threshold=threshold, zone=classify_zone(p, ci$upr, threshold))
}

##

#predicted_SNR = (log(prop / (1 - prop)) - coef(glm_fit)[1]) / coef(glm_fit)[2]
# ca revient au meme parce que prop = 0.5 et log(0.5/1-0.5) = 0 donc ca revient à faire -coef(1)/coef(2)

# ------------- CSS ---------------

app_css <- "
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
 
body, .shiny-frame, .container-fluid {
  font-family: 'DM Sans', sans-serif;
  background: #f0f2f7;
  color: #1e2535;
  font-size: 14px;
}

/* ── Top header bar ── */
.app-header {
  background: #1e2535;
  color: #fff;
  padding: 14px 24px 12px 24px;
  margin: -15px -15px 18px -15px;
  display: flex;
  align-items: baseline;
  gap: 14px;
}
.app-header h1 {
  font-size: 1.2em;
  font-weight: 600;
  letter-spacing: -0.2px;
  color: #fff;
}
.app-header span {
  font-size: 0.82em;
  color: #8896b3;
  font-weight: 400;
}

/* ── Cards ── */
.card {
  background: #ffffff;
  border-radius: 10px;
  padding: 16px 18px;
  margin-bottom: 12px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06), 0 1px 8px rgba(0,0,0,0.04);
  border: 1px solid #e4e8f0;
}
.card-title {
  font-size: 0.7em;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: #9aa5be;
  margin-bottom: 12px;
}

/* ── Participant selector ── */
.participant-row {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 0;
}
.participant-id {
  font-family: 'DM Mono', monospace;
  font-size: 1.1em;
  font-weight: 500;
  color: #1e2535;
  background: #f4f6fb;
  border: 1.5px solid #d0d7e8;
  border-radius: 7px;
  padding: 6px 12px;
  min-width: 90px;
  text-align: center;
}

/* ── Zone badge ── */
.zone-badge {
  border-radius: 9px;
  padding: 11px 14px;
  margin-bottom: 8px;
  display: flex;
  align-items: flex-start;
  gap: 12px;
  border-width: 1.5px;
  border-style: solid;
}
.zone-badge-icon {
  font-size: 1.5em;
  line-height: 1;
  margin-top: 1px;
  flex-shrink: 0;
}
.zone-badge-snr {
  font-family: 'DM Mono', monospace;
  font-size: 0.88em;
  opacity: 0.7;
  margin-bottom: 1px;
}
.zone-badge-main {
  font-size: 1.05em;
  font-weight: 600;
  line-height: 1.2;
}
.zone-badge-sub {
  font-size: 0.78em;
  margin-top: 3px;
  opacity: 0.8;
  font-style: italic;
}

/* ── Result table ── */
.result-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.87em;
}
.result-table th {
  background: #f4f6fb;
  color: #8896b3;
  font-weight: 600;
  font-size: 0.72em;
  text-transform: uppercase;
  letter-spacing: 0.7px;
  padding: 7px 10px;
  text-align: center;
  border-bottom: 2px solid #e4e8f0;
}
.result-table td {
  padding: 9px 10px;
  text-align: center;
  border-bottom: 1px solid #f0f3f8;
  font-family: 'DM Mono', monospace;
  font-size: 0.95em;
}
.result-table tr:last-child td { border-bottom: none; }
.td-zone {
  font-family: 'DM Sans', sans-serif !important;
  font-size: 0.82em !important;
  font-weight: 600 !important;
}

/* ── SIB50 comparison ── */
.sib50-row {
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  gap: 8px;
  margin-bottom: 12px;
}
.sib50-cell {
  border-radius: 8px;
  padding: 12px 10px;
  text-align: center;
  border-width: 1.5px;
  border-style: solid;
}
.sib50-val {
  font-family: 'DM Mono', monospace;
  font-size: 1.75em;
  font-weight: 500;
  line-height: 1.1;
}
.sib50-lbl {
  font-size: 0.7em;
  text-transform: uppercase;
  letter-spacing: 0.7px;
  margin-top: 4px;
  opacity: 0.72;
}
.sib50-pred { background:#e8f0fb; color:#1a3a6e; border-color:#93b8e8; }
.sib50-real { background:#f4f6fb; color:#2a3550; border-color:#c8d0e0; }
.sib50-ok   { background:#edf7e6; color:#2d6e18; border-color:#a8d98a; }
.sib50-warn { background:#fff5e6; color:#8a4a00; border-color:#f5c07a; }
.sib50-bad  { background:#fdeaea; color:#7a1520; border-color:#f0a0a8; }
.sib50-na   { background:#f4f6fb; color:#9aa5be; border-color:#d0d7e8; font-style:italic; }

/* ── Retest banner ── */
.retest-banner {
  background: #fffbf0;
  border-left: 4px solid #e07b00;
  border-radius: 0 8px 8px 0;
  padding: 10px 14px;
  margin: 2px 0 14px 0;
  font-size: 0.86em;
  color: #6b3f00;
  line-height: 1.6;
}

/* ── Shiny overrides ── */
.well {
  background: #fff !important;
  border-radius: 10px !important;
  border: 1px solid #e4e8f0 !important;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06) !important;
  padding: 16px !important;
}
.form-control, .selectize-input {
  border-radius: 7px !important;
  border: 1.5px solid #d8dded !important;
  font-size: 0.92em !important;
  color: #1e2535 !important;
}
.form-control:focus {
  border-color: #3a6fc4 !important;
  box-shadow: 0 0 0 3px rgba(58,111,196,0.13) !important;
  outline: none !important;
}
.irs--shiny .irs-bar        { background: #3a6fc4 !important; border-color: #3a6fc4 !important; }
.irs--shiny .irs-handle     { background: #fff !important; border-color: #3a6fc4 !important; }
.irs--shiny .irs-single     { background: #3a6fc4 !important; }
label { font-size: 0.85em !important; color: #5a6480 !important; font-weight: 500 !important; margin-bottom: 4px !important; }
.radio label, .checkbox label { color: #1e2535 !important; }
hr { border-color: #e4e8f0 !important; margin: 10px 0 !important; }
"

# -------------- UI --------------
ui_content <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  
  # ── Header ──────────────────────────────────────────────
  div(class = "app-header",
      tags$h1("Reimbursement eligibility"),
      tags$span("French CRM normalization")
  ),
  
  sidebarLayout(
    # ── Left panel ────────────────────────────────────────
    sidebarPanel(width = 4,
                 
                 # SIB50 normatif fixe
                 div(class = "card",
                     p(class = "card-title", "Normative reference"),
                     div(style="display:flex;align-items:center;gap:10px;padding:6px 10px;
                   background:#f4f6fb;border-radius:7px;border:1px solid #d0d7e8;",
                         div(style="font-size:0.75em;text-transform:uppercase;letter-spacing:0.7px;
                     color:#8896b3;font-weight:600;", "SIB50 normatif"),
                         uiOutput("sib50_reel_display")
                     ),
                     tags$p(style="color:#9aa5be;font-size:0.78em;margin-top:8px;line-height:1.5;",
                            "SNR of the normative curve for 50% of correct response.")
                 ),
                 
                 # Test initial
                 div(class = "card",
                     p(class = "card-title", "Initial test"),
                     sliderInput("snr_init", "Tested SNR (dB)",
                                 min=SNR_min, max=SNR_max, value=-9, step=3),
                     fluidRow(
                       column(6, numericInput("perf_init",    "Performance (%)", value=50, min=0, max=100, step=1)),
                       column(6, numericInput("ntrials_init", "Number of trials",       value=32, min=1, max=250, step=1))
                     )
                 ),
                 
                 # Retests conditionnels
                 uiOutput("retest_inputs"),
                 
                 # IC
                 div(class = "card",
                     p(class = "card-title", "Confidence interval"),
                     radioButtons("ci_method", NULL,
                                  choices=c("Wilson"="wilson","Wald"="wald"),
                                  selected="wilson", inline=TRUE),
                     sliderInput("ci_level", "Level (%)", min=80, max=99, value=95, step=1)
                 ),
                 
                 # Verdict
                 div(class = "card",
                     p(class = "card-title", "Result per SNR"),
                     uiOutput("verdict_ui")
                 )
    ),
    
    # ── Right panel ───────────────────────────────────────
    mainPanel(width = 8,
              plotOutput("plot", height = "450px"),
              br(),
              uiOutput("results_ui")
    )
  )
)

ui <- secure_app(ui_content)

# -------------- Server ----------------
server <- function(input, output, session) {
  
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # ── SIB50 normatif ───────
  sib50_reel_selected <- reactive({ SIB50_REEL })
  
  output$sib50_reel_display <- renderUI({
    div(style="font-family:'DM Mono',monospace;font-size:1.15em;font-weight:500;color:#1e2535;",
        sprintf("%.1f dB", SIB50_REEL))
  })
  
  # ── Test initial ────────────────────────────────────────
  result_init <- reactive({
    build_result(input$snr_init, input$perf_init, input$ntrials_init,
                 input$ci_level/100, input$ci_method)
  })
  
  snr_plus  <- reactive({ input$snr_init + 3 })
  snr_minus <- reactive({ input$snr_init - 3 })
  
  # ── Retest UI ───────────────────────────────────────────
  output$retest_inputs <- renderUI({
    ri <- result_init()
    if (!(ri$zone %in% c("orange","red"))) return(NULL)
    sp <- snr_plus(); sm <- snr_minus()
    sp_default = predict(glm_fit,
                         newdata = data.frame(SNR_numeric = sp - 3),
                         type = "response")*100
    sm_default = predict(glm_fit,
                         newdata = data.frame(SNR_numeric = sm - 3),
                         type = "response")*100
    
    div(class = "card",
        p(class = "card-title", "Retests"),
        div(class = "retest-banner",
            HTML(sprintf(
              "⚠ Ambiguous or abnormal results<br>
           Retests recomanded à <b>%+d dB</b> et <b>%+d dB</b>.",
              sp, sm))
        ),
        tags$p(style="font-size:0.82em;font-weight:600;color:#5a6480;margin-bottom:6px;",
               sprintf("SNR %+d dB", sp)),
        fluidRow(
          column(6, numericInput("perf_plus",    "Performance (%)", value=sp_default, min=0, max=100, step=1)),
          column(6, numericInput("ntrials_plus", "Number of trials",       value=32, min=1, max=250, step=1))
        ),
        tags$p(style="font-size:0.82em;font-weight:600;color:#5a6480;margin:10px 0 6px 0;",
               sprintf("SNR %+d dB", sm)),
        fluidRow(
          column(6, numericInput("perf_minus",    "Performance (%)", value=sm_default, min=0, max=100, step=1)),
          column(6, numericInput("ntrials_minus", "Number of trials",       value=32, min=1, max=250, step=1))
        )
    )
  })
  
  result_plus <- reactive({
    req(input$perf_plus, input$ntrials_plus)
    build_result(snr_plus(), input$perf_plus, input$ntrials_plus,
                 input$ci_level/100, input$ci_method)
  })
  result_minus <- reactive({
    req(input$perf_minus, input$ntrials_minus)
    build_result(snr_minus(), input$perf_minus, input$ntrials_minus,
                 input$ci_level/100, input$ci_method)
  })
  
  all_results <- reactive({
    ri <- result_init()
    if (ri$zone %in% c("orange","red") &&
        !is.null(input$perf_plus) && !is.null(input$perf_minus)) {
      list(ri, result_plus(), result_minus())
    } else {
      list(ri)
    }
  })
  
  # ── Verdict badges ──────────────────────────────────────
  output$verdict_ui <- renderUI({
    items <- lapply(all_results(), function(r) {
      div(class = "zone-badge",
          style = sprintf("background:%s;border-color:%s;color:%s;",
                          zone_bg(r$zone), zone_border(r$zone), zone_text(r$zone)),
          div(class="zone-badge-icon", zone_icon(r$zone)),
          div(
            div(class="zone-badge-snr",  sprintf("SNR %+d dB", r$snr)),
            div(class="zone-badge-main", zone_label_short(r$zone)),
            div(class="zone-badge-sub",  zone_label_full(r$zone))
          )
      )
    })
    do.call(tagList, items)
  })
  
  # ── Main results block ──────────────────────────────────
  output$results_ui <- renderUI({
    results <- all_results()
    reel    <- sib50_reel_selected()
    
    # ── Detail table ──────────────────────────────────────
    rows_html <- paste0(sapply(results, function(r) {
      sprintf(
        '<tr style="background:%s;">
          <td style="font-weight:600;">%+d dB</td>
          <td>%.1f%%</td><td>%.1f%%</td><td>%.1f%%</td><td>%.1f%%</td>
          <td class="td-zone" style="color:%s;">%s %s</td>
        </tr>',
        zone_bg(r$zone),
        r$snr,
        r$p*100, r$lwr*100, r$upr*100, r$threshold*100,
        zone_colour(r$zone),
        zone_icon(r$zone), zone_label_short(r$zone)
      )
    }), collapse="")
    
    table_card <- div(class="card",
                      p(class="card-title", "Detailed results"),
                      HTML(sprintf('
        <table class="result-table">
          <thead><tr>
            <th>SNR</th><th>Performance</th>
            <th>under IC</th><th>upper IC</th><th>Threshold</th><th>Zone</th>
          </tr></thead>
          <tbody>%s</tbody>
        </table>', rows_html))
    )
    
    # ── SIB50 card (3 résultats seulement) ────────────────
    sib50_card <- NULL
    
    if (length(results) == 3) {
      snrs       <- sapply(results, `[[`, "snr")
      perfs      <- sapply(results, `[[`, "p")
      n_vec      <- sapply(results, `[[`, "n")
      sib50_pred <- predict_sib50(snrs, perfs, n_vec)
      
      # Cellule prédit
      cell_pred <- div(class="sib50-cell sib50-pred",
                       div(class="sib50-val",
                           if(is.na(sib50_pred)) "—" else sprintf("%.1f dB", sib50_pred)),
                       div(class="sib50-lbl", "Predicted SIB50")
      )
      
      # Cellule réel
      cell_real <- div(class="sib50-cell sib50-real",
                       div(class="sib50-val",
                           if(is.na(reel)) "—" else sprintf("%.1f dB", reel)),
                       div(class="sib50-lbl",
                           if(is.na(reel)) "Normative SIB50 (non chargé)" else "Normative SIB50")
      )
      
      # Cellule écart
      cell_diff <- if (!is.na(sib50_pred) && !is.na(reel)) {
        diff  <- sib50_pred - reel
        adiff <- abs(diff)
        cls   <- if (adiff <= 1) "sib50-ok" else if (adiff <= 3) "sib50-warn" else "sib50-bad"
        icon  <- if (adiff <= 1) "✓" else if (adiff <= 3) "⚠" else "✗"
        div(class=paste0("sib50-cell ", cls),
            div(class="sib50-val", sprintf("%s %+.1f", icon, diff)),
            div(class="sib50-lbl", "Difference (predicted − normative)")
        )
      } else {
        div(class="sib50-cell sib50-na",
            div(class="sib50-val", "—"),
            div(class="sib50-lbl", "Impossible to compute")
        )
      }
      
      # Note méthode
      note <- if (is.na(sib50_pred)) {
        tags$p(style="color:#e07b00;font-size:0.82em;margin-top:6px;",
               "⚠ SIB50 cannot be computed— performance at 0 % or 100 %(logit undefined).")
      } else {
        tags$p(style="color:#9aa5be;font-size:0.78em;margin-top:8px;line-height:1.5;",
               "Method: fixed slope of the normative curve, participant intercept estimated on the 3
               tested SNR. The SIB50 is the SNR at which the preformance reaches 50%.")
      }
      
      sib50_card <- div(class="card",
                        p(class="card-title", "SIB50"),
                        div(class="sib50-row", cell_pred, cell_real, cell_diff),
                        note
      )
    }
    
    tagList(table_card, sib50_card)
  })
  
  # ── Plot ────────────────────────────────────────────────
  output$plot <- renderPlot({
    results    <- all_results()
    reel       <- sib50_reel_selected()
    
    p_base <- ggplot() +
      geom_ribbon(data=snr_grid,
                  aes(x=SNR_numeric, ymin=0, ymax=fit_3dB),
                  fill="#94262e", alpha=0.10) + #
      geom_line(data=snr_grid,
                aes(x=SNR_numeric, y=fit, colour="Normative curve"),
                linewidth=1.1) +
      geom_line(data=snr_grid,
                aes(x=SNR_numeric, y=fit_3dB, colour="Threshold +3 dB"),
                linewidth=1.1, linetype="dashed") +
      annotate("text", x=-16, y=0.04, label="Eligibility zone",
               colour="#94262e", hjust=0, size=3.8, fontface="italic") +
      scale_colour_manual(values=c("Normative curve"="#548a37","Threshold +3 dB"="#94262e")) +
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) +
      scale_x_continuous(breaks=snr_levels, labels=snr_levels) +
      labs(title="Psychometric curve — Eligibility zone ",
           x="Signal to Noise Ratio (dB)", y="Correct responses (%)", colour=NULL) +
      theme_bw(base_size=13) +
      theme(legend.position="bottom",
            panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold", size=13, colour="#1e2535"),
            axis.title=element_text(size=11, colour="#5a6480"),
            panel.background=element_rect(fill="#fafbfd"),
            plot.background=element_rect(fill="#fafbfd", colour=NA))
    
    # Points de test
    for (r in results) {
      col <- zone_colour(r$zone)
      lbl <- paste0(zone_icon(r$zone), " ", zone_label_short(r$zone))
      p_base <- p_base +
        geom_errorbar(aes(x=!!r$snr, ymin=!!r$lwr, ymax=!!r$upr),
                      colour=col, width=0.5, linewidth=1.4) +
        geom_point(aes(x=!!r$snr, y=!!r$p),
                   colour=col, size=5, shape=18) #+
       # annotate("text", x=r$snr+0.3, y=min(r$upr+0.07, 0.97),
       #          label=lbl, colour=col, hjust=0, size=3.5, fontface="bold")
    }
    
    # SIB50 prédit + réel sur le graphe ANCIENNE VERSION SANS LA COURBE SIB50
    if (length(results) == 800) {
      snrs       <- sapply(results, `[[`, "snr")
      perfs      <- sapply(results, `[[`, "p")
      n_vec      <- sapply(results, `[[`, "n")
      sib50_pred <- predict_sib50(snrs, perfs,n_vec )
      
      if (!is.na(sib50_pred) && sib50_pred >= SNR_min && sib50_pred <= SNR_max) {
        p_base <- p_base +
          geom_vline(xintercept=sib50_pred, linetype="dotted",
                     colour="#3a6fc4", linewidth=1.1) +
          annotate("text", x=sib50_pred+0.2, y=0.60,
                   label=sprintf("Predicted SIB50\n%.1f dB", sib50_pred),
                   colour="#3a6fc4", hjust=0, size=3.4, fontface="bold")
      }
      
      if (!is.na(reel) && reel >= SNR_min && reel <= SNR_max) {
        p_base <- p_base +
          geom_vline(xintercept=reel, linetype="dotted",
                     colour="#444444", linewidth=1.0) +
          annotate("text", x=reel+0.2, y=0.44,
                   label=sprintf("Normative SIB50\n%.1f dB", reel),
                   colour="#444444", hjust=0, size=3.4, fontface="bold")
      }
    }
    # SIB50 prédit + réel + fit participant
    if (length(results) == 3) {
      
      snrs       <- sapply(results, `[[`, "snr")
      perfs      <- sapply(results, `[[`, "p")
      n_vec      <- sapply(results, `[[`, "n")
      
      # sécurisation logit
      perfs_fit <- pmin(pmax(perfs, 0.01), 0.99)
      
      # données participant
      df_fit <- data.frame(
        SNR_numeric = snrs,
        prop        = perfs_fit,
        n           = n_vec
      )
      
      # fit individuel
      fit_participant <- tryCatch(
        glm(
          prop ~ SNR_numeric,
          family = binomial,
          data = df_fit
        ),
        error = function(e) NULL
      )
      
      sib50_pred <- predict_sib50(snrs, perfs_fit, n_vec)
      
      # ─────────────────────────────────────
      # courbe psychométrique participant
      # ─────────────────────────────────────
      
      if (!is.null(fit_participant)) {
        
        fit_grid <- data.frame(
          SNR_numeric = seq(SNR_min, SNR_max, length.out = 300)
        )
        
        fit_grid$fit <- predict(
          fit_participant,
          newdata = fit_grid,
          type = "response"
        )
        
        p_base <- p_base +
          
          geom_line(
            data = fit_grid,
            aes(
              x = SNR_numeric,
              y = fit,
              colour = "Participant fit"
            ),
            linewidth = 1.4
          ) +
          
          scale_colour_manual(
            values = c(
              "Normative curve" = "#548a37",
              "Threshold +3 dB" = "#94262e",
              "Participant fit" = "#3a6fc4"
            )
          )
      }
      
      # ─────────────────────────────────────
      # SIB50 participant
      # ─────────────────────────────────────
      
      if (!is.na(sib50_pred) &&
          sib50_pred >= SNR_min &&
          sib50_pred <= SNR_max) {
        
        p_base <- p_base +
          
          geom_vline(
            xintercept = sib50_pred,
            linetype = "dotted",
            colour = "#3a6fc4",
            linewidth = 1.1
          ) +
          
          annotate(
            "text",
            x = sib50_pred + 0.2,
            y = 0.60,
            label = sprintf(
              "Predicted SIB50\n%.1f dB",
              sib50_pred
            ),
            colour = "#3a6fc4",
            hjust = 0,
            size = 3.4,
            fontface = "bold"
          )
      }
      
      # ─────────────────────────────────────
      # SIB50 normatif
      # ─────────────────────────────────────
      
      if (!is.na(reel) &&
          reel >= SNR_min &&
          reel <= SNR_max) {
        
        p_base <- p_base +
          
          geom_vline(
            xintercept = reel,
            linetype = "dotted",
            colour = "#444444",
            linewidth = 1.0
          ) +
          
          annotate(
            "text",
            x = reel + 0.2,
            y = 0.44,
            label = sprintf(
              "Normative SIB50\n%.1f dB",
              reel
            ),
            colour = "#444444",
            hjust = 0,
            size = 3.4,
            fontface = "bold"
          )
      }
    }
    
    
    p_base
  }, bg="#fafbfd")
}

shinyApp(ui, server)