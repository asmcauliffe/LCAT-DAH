library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(DT)

# ── Palette & Theme ───────────────────────────────────────────────────────────

mc_red   <- "#BE3144"
mc_dark  <- "#1a1a1a"
mc_card  <- "#242424"

app_theme <- bs_theme(
  bootswatch  = "darkly",
  primary     = mc_red,
  bg          = mc_dark,
  fg          = "#e8e8e8",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  "card-bg"   = mc_card,
  "navbar-bg" = "#111111",
  # Navbar text — override darkly's 55%-opacity default
  "navbar-dark-color"           = "rgba(232,232,232,0.95)",
  "navbar-dark-hover-color"     = "#ffffff",
  "navbar-dark-active-color"    = "#ffffff",
  "navbar-dark-disabled-color"  = "rgba(232,232,232,0.4)",
  # Form elements inside the dark sidebar
  "form-label-color"            = "#e8e8e8",
  "form-check-label-color"      = "#e8e8e8"
)

# ── Shared sidebar style ──────────────────────────────────────────────────────

sidebar_style <- "background-color: #1e1e1e; padding: 14px;"

# ── UI ────────────────────────────────────────────────────────────────────────

page_navbar(
  title = tags$span(
    tags$span("LCAT", style = "color: #BE3144; font-weight: 700;"),
    tags$span(" Data Analysis Hub", style = "color: #e8e8e8; font-weight: 400;")
  ),
  theme    = app_theme,
  fillable = TRUE,
  lang     = "en",

  # ── Global contrast fixes ──────────────────────────────────────────────────
  header = tags$head(tags$style(HTML("
    /* Navbar: page-tab links and icons */
    .navbar-nav .nav-link,
    .navbar-nav .nav-link .bi  { color: rgba(232,232,232,0.95) !important; }
    .navbar-nav .nav-link:hover,
    .navbar-nav .nav-link:focus { color: #ffffff !important; }
    .navbar-nav .nav-link.active { color: #ffffff !important; font-weight: 600; }

    /* navset_card_underline tab labels (Page 1 inner tabs) */
    .nav-underline .nav-link          { color: #c0c0c0 !important; }
    .nav-underline .nav-link:hover    { color: #ffffff !important; }
    .nav-underline .nav-link.active   { color: #ffffff !important; font-weight: 600; }

    /* Sidebar: all label text, radio/checkbox labels */
    .bslib-sidebar-layout label,
    .bslib-sidebar-layout .control-label,
    .bslib-sidebar-layout .shiny-input-container > label,
    .bslib-sidebar-layout .form-check-label { color: #e8e8e8 !important; }

    /* Card headers */
    .card-header { color: #e8e8e8 !important; }

    /* Value-box titles */
    .value-box-title { color: #e8e8e8 !important; }

    /* DT table: search box, filter inputs, pagination text */
    .dataTables_filter label,
    .dataTables_length label,
    .dataTables_info,
    .dataTables_paginate,
    thead th,
    .dt-buttons .btn { color: #e8e8e8 !important; }
    .dataTables_filter input,
    .dataTables_length select { background-color: #2e2e2e !important;
                                 color: #e8e8e8 !important;
                                 border-color: #555 !important; }
    thead tr th input[type='search'] { background-color: #2e2e2e !important;
                                        color: #e8e8e8 !important;
                                        border-color: #555 !important; }

    /* Page-3 notes body text */
    .card p, .card li { color: #d8d8d8; }
  "))),

  # ── Page 1: Food Prices & SMEB ─────────────────────────────────────────────
  nav_panel(
    title = "Food Prices & SMEB",
    icon  = icon("basket-shopping"),

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 280,
        open  = "open",
        style = sidebar_style,

        # Mode toggle — shown always
        radioButtons(
          inputId  = "food_mode",
          label    = "Comparison mode",
          choices  = c(
            "Same item, compare sources" = "cross_source",
            "Multiple items, one source" = "within_source"
          ),
          selected = "cross_source"
        ),
        hr(style = "border-color: #444;"),

        # Dynamic controls swap based on mode
        uiOutput("food_controls"),

        hr(style = "border-color: #444;"),

        # Currency shown on both tabs
        radioButtons(
          inputId  = "food_currency",
          label    = "Currency",
          choices  = c("USD" = "usd", "LBP" = "lbp"),
          selected = "usd",
          inline   = TRUE
        ),

        hr(style = "border-color: #444;"),

        # SMEB-tab source selector (hidden when on food tab via CSS / server toggle)
        checkboxGroupInput(
          inputId  = "smeb_sources",
          label    = "SMEB sources",
          choices  = c(
            "Lebanese Government" = "Lebanese Government",
            "WFP"                 = "WFP",
            "Carrefour – Food"    = "food_SMEB",
            "Carrefour – NFI"     = "nfi_SMEB",
            "Carrefour – Total"   = "total_SMEB"
          ),
          selected = c("Lebanese Government", "WFP")
        ),

      ),

      # ── Main: tabbed cards ───────────────────────────────────────────────
      navset_card_underline(
        id    = "p1_tabs",
        title = NULL,

        nav_panel(
          title = "Food Items",
          plotOutput("food_plot", height = "520px")
        ),

        nav_panel(
          title = "Non-Food Items",
          plotOutput("nfi_plot", height = "520px")
        ),

        nav_panel(
          title = "SMEB",
          plotOutput("smeb_plot", height = "520px")
        )
      )
    )
  ),

  # ── Page 2: Economic Indicators ────────────────────────────────────────────
  nav_panel(
    title = "Economic Indicators",
    icon  = icon("chart-line"),

    layout_columns(
      fill         = FALSE,
      col_widths   = c(4, 4, 4),
      value_box(
        title    = "Diesel Price",
        value    = textOutput("vb_diesel"),
        showcase = bsicons::bs_icon("fuel-pump"),
        theme    = value_box_theme(bg = mc_card, fg = mc_red)
      ),
      value_box(
        title    = "Exchange Rate",
        value    = textOutput("vb_exch"),
        showcase = bsicons::bs_icon("currency-exchange"),
        theme    = value_box_theme(bg = mc_card, fg = "#e8e8e8")
      ),
      value_box(
        title    = "Consumer Price Index",
        value    = textOutput("vb_cpi"),
        showcase = bsicons::bs_icon("graph-up"),
        theme    = value_box_theme(bg = mc_card, fg = "#e8e8e8")
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      row_heights = "420px",

      card(
        full_screen = TRUE,
        card_header(
          "Fuel Prices",
          class = "d-flex justify-content-between align-items-center",
          switchInput(
            inputId    = "fuel_currency",
            label      = "USD",
            value      = TRUE,
            onLabel    = "USD",
            offLabel   = "LBP",
            size       = "mini",
            onStatus   = "danger",
            offStatus  = "secondary"
          )
        ),
        plotOutput("fuel_plot", height = "340px")
      ),

      card(
        full_screen = TRUE,
        card_header("LBP / USD Exchange Rate"),
        plotOutput("exch_plot", height = "340px")
      )
    ),

    layout_columns(
      col_widths = 12,
      row_heights = "420px",

      card(
        full_screen = TRUE,
        card_header(
          "Consumer Price Index",
          class = "d-flex justify-content-between align-items-center",
          selectInput(
            inputId  = "cpi_category",
            label    = NULL,
            choices  = NULL,   # populated in server
            width    = "260px"
          )
        ),
        plotOutput("cpi_plot", height = "320px")
      )
    )
  ),

  # ── Page 3: Economic Vulnerability Index ───────────────────────────────────
  nav_panel(
    title = "Vulnerability Index",
    icon  = icon("map"),
    fillable = TRUE,

    # Weight toggle at top
    div(
      class = "d-flex align-items-center gap-3 mb-3",
      tags$span("Population weighted:", style = "color: #e8e8e8; font-size: 0.9rem;"),
      switchInput(
        inputId   = "pop_weight",
        label     = NULL,
        value     = FALSE,
        onStatus  = "danger",
        offStatus = "secondary",
        size      = "mini"
      )
    ),

    layout_columns(
      col_widths  = c(7, 5),
      row_heights = "460px",

      card(
        full_screen = TRUE,
        card_header("Economic Vulnerability Index"),
        leafletOutput("nlr_map", height = "100%")
      ),

      card(
        full_screen = TRUE,
        card_header("Night Light Radiance & Diesel Price"),
        plotOutput("ts_nlr", height = "100%")
      )
    ),

    card(
      full_screen = TRUE,
      card_header("Economic Vulnerability by Cadaster (sorted by most vulnerable)"),
      style = "max-height: 380px; overflow-y: auto;",
      DTOutput("vulnerability_table")
    ),

    card(
      class = "mt-2",
      card_header("Notes"),
      p("The Economic Vulnerability Score (EVS) is based on two indicators from nighttime lights reflectance (NLR) satellite data:"),
      tags$ul(
        tags$li(tags$b("Change in NLR concentration:"), " Change between the latest monthly NLR image and the same month in 2019. Negative values indicate more concentrated electricity consumption."),
        tags$li(tags$b("Price elasticity of generator cost:"), " Correlation between diesel price growth and total NLR growth per geographic unit. Negative correlations indicate communities unable to maintain consumption when generator costs rise.")
      ),
      p("Percentile rankings of these indicators are summed for the unweighted EVS; the population-weighted EVS multiplies by local population. Only the top 25% most vulnerable cadasters are shown on the map.")
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(
      href   = "https://www.mercycorps.org/",
      target = "_blank",
      tags$span("Mercy Corps LCAT", style = "font-size: 0.85rem; color: #aaa;")
    )
  )
)
