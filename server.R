library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(sf)
library(leaflet)
library(DT)
library(thematic)
library(readr)
library(shinyWidgets)
library(readxl)

source("modules.R")

# ── SMEB item definitions for cross-source comparison ─────────────────────────

deffer <- readxl::read_excel("SMEB_tables_def.xlsx", sheet = "deffer") %>%
  filter(baskett == "Food SMEB")

# ── Static data loaded once at startup ───────────────────────────────────────

vuln_raw <- sf::st_read("evs_sep24.geojson", quiet = TRUE) %>%
  mutate(Rescaled_Vuln = { inv <- function(x) ((x - max(x)) * -1) + min(x); inv(Rescaled_Vuln) })

most_vuln <- vuln_raw %>%
  arrange(desc(Rescaled_Vuln_Weighted)) %>%
  mutate(
    Cadaster             = as.factor(Cadaster),
    District             = as.factor(District),
    Governorate          = as.factor(Governorate),
    Vulnerability_Weighted = as.factor(Vulnerability_Weighted),
    Vulnerability        = as.factor(Vulnerability)
  )

nlr_raw <- readr::read_csv("new_nlr_ts.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# ── Derived lookups from app_set ──────────────────────────────────────────────

# All items coming from the IPT (fuel) source are fuel-only
fuel_items <- app_set %>%
  filter(source == "IPT") %>%
  pull(item) %>% unique()

# Hygiene / household non-food items (NFI) — identified by name across sources
nfi_item_names <- c(
  "Blanket", "Diapers", "Disinfectant fluid / Bleach", "Drinking Water",
  "Individual soap", "Laundry soap/detergent", "Liquid Dish detergent",
  "Sanitary napkins", "Shampoo", "Toilet Paper", "Toothbrush", "Toothpaste"
)

# Exclude: SMEB-type baskets, index (CPI), and fuel items
smeb_items <- c("SMEB", "food_SMEB", "nfi_SMEB", "total_SMEB")

nfi_items_all <- app_set %>%
  filter(
    unit != "index",
    unit != "basket",
    !item %in% smeb_items,
    !item %in% fuel_items,
    item %in% nfi_item_names
  ) %>%
  pull(item) %>% unique() %>% sort()

food_items_all <- app_set %>%
  filter(
    unit != "index",
    unit != "basket",
    !item %in% smeb_items,
    !item %in% fuel_items,
    !item %in% nfi_item_names
  ) %>%
  pull(item) %>% unique() %>% sort()

# Sources that carry food/NFI data (not IPT)
food_sources_all <- app_set %>%
  filter(source != "IPT", unit != "index") %>%
  pull(source) %>% unique() %>% sort()

cpi_categories <- app_set %>%
  filter(unit == "index") %>%
  pull(item) %>% unique() %>% sort()

diesel_monthly <- app_set %>%
  filter(item == "Diesel", source == "IPT") %>%
  mutate(date = floor_date(as.Date(date), "month")) %>%
  group_by(date) %>%
  summarise(price = mean(price, na.rm = TRUE),
            price_usd = mean(price_usd, na.rm = TRUE),
            .groups = "drop")

# ── Shared ggplot theme ───────────────────────────────────────────────────────

mc_theme <- function() {
  theme_minimal(base_family = "sans", base_size = 13) +
    theme(
      plot.background    = element_rect(fill = "#242424", colour = NA),
      panel.background   = element_rect(fill = "#242424", colour = NA),
      panel.grid.major   = element_line(colour = "#333333"),
      panel.grid.minor   = element_blank(),
      text               = element_text(colour = "#e8e8e8"),
      axis.text          = element_text(colour = "#b0b0b0"),
      axis.title         = element_text(colour = "#e8e8e8"),
      legend.background  = element_rect(fill = "#242424", colour = NA),
      legend.text        = element_text(colour = "#e8e8e8"),
      legend.title       = element_text(colour = "#e8e8e8"),
      strip.text         = element_text(colour = "#e8e8e8"),
      plot.title         = element_text(colour = "#e8e8e8", face = "bold", size = 14),
      plot.subtitle      = element_text(colour = "#aaaaaa", size = 11)
    )
}

# Humanitarian palette for up to 8 series
mc_palette <- c("#BE3144", "#E8963A", "#4A90D9", "#5CB85C", "#9B59B6",
                "#1ABC9C", "#E74C3C", "#F39C12")

# ── Leaflet vulnerability palette ────────────────────────────────────────────

vuln_pal <- colorBin(
  palette = c("#262626", "#7d0f5f", "#d64b41", "red"),
  domain  = c(0, 100),
  bins    = 5,
  pretty  = FALSE
)

highlight_icon <- makeAwesomeIcon(icon = "flag", markerColor = "white", iconColor = "black")

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

thematic::thematic_shiny()

function(input, output, session) {

  # ── Page 1: dynamic sidebar controls ───────────────────────────────────────

  output$food_controls <- renderUI({
    tab <- input$p1_tabs %||% "SMEB by Source"
    if (tab == "Item Comparison") {
      selectInput(
        inputId  = "compare_item",
        label    = "Item",
        choices  = deffer$itemm,
        selected = deffer$itemm[1]
      )
    } else {
      selectInput(
        inputId  = "smeb_source_v1",
        label    = "Source",
        choices  = c("Lebanese Government", "WFP", "Carrefour"),
        selected = "Lebanese Government"
      )
    }
  })

  # ── Page 1: SMEB by Source ──────────────────────────────────────────────────

  smeb_data <- reactive({
    req(input$smeb_source_v1)
    price_col <- if (input$food_currency == "usd") "price_usd" else "price"

    if (input$smeb_source_v1 == "Carrefour") {
      app_set %>%
        filter(source == "Carrefour", item %in% c("food_SMEB", "nfi_SMEB")) %>%
        mutate(
          series = case_when(
            item == "food_SMEB" ~ "Food SMEB",
            item == "nfi_SMEB"  ~ "NFI SMEB",
            TRUE                ~ item
          ),
          plot_price = .data[[price_col]],
          date = as.Date(date)
        )
    } else {
      app_set %>%
        filter(item == "SMEB", source == input$smeb_source_v1) %>%
        mutate(
          series = "Food SMEB",
          plot_price = .data[[price_col]],
          date = as.Date(date)
        )
    }
  })

  output$smeb_plot <- renderPlot({
    df <- smeb_data()
    req(nrow(df) > 0)

    ylabel <- if (input$food_currency == "usd") "SMEB Cost (USD)" else "SMEB Cost (LBP)"

    ggplot(df, aes(x = date, y = plot_price, colour = series)) +
      geom_line(linewidth = 1.3, na.rm = TRUE) +
      geom_point(size = 1.8, na.rm = TRUE) +
      scale_colour_manual(values = mc_palette, name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      labs(
        title    = paste("SMEB —", input$smeb_source_v1),
        subtitle = "Monthly cost by basket type",
        x        = NULL,
        y        = ylabel
      ) +
      mc_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  }, bg = "#242424")

  # ── Page 1: Item Comparison ─────────────────────────────────────────────────

  item_compare_data <- reactive({
    req(input$compare_item)
    price_col <- if (input$food_currency == "usd") "price_usd" else "price"

    row <- deffer %>% filter(itemm == input$compare_item)
    req(nrow(row) == 1)

    leb <- if (!is.na(row$mnstry_item)) {
      app_set %>%
        filter(source == "Lebanese Government",
               item == stringr::str_to_title(row$mnstry_item)) %>%
        mutate(
          plot_price = .data[[price_col]] * row$minstry_multi_ltr_kg,
          series = "Lebanese Government"
        )
    } else {
      NULL
    }

    wfp <- if (!is.na(row$wfp_itemm)) {
      app_set %>%
        filter(source == "WFP", item == row$wfp_itemm) %>%
        mutate(
          plot_price = .data[[price_col]] * row$wfp_multi_ltr_kg,
          series = "WFP"
        )
    } else {
      NULL
    }

    car <- carrefour_items %>%
      filter(item == row$itemm) %>%
      mutate(
        plot_price = .data[[price_col]],
        series = "Carrefour"
      )

    bind_rows(leb, wfp, car) %>%
      mutate(date = as.Date(date))
  })

  output$item_compare_plot <- renderPlot({
    df <- item_compare_data()
    req(nrow(df) > 0)

    ylabel <- if (input$food_currency == "usd") {
      "Price (USD, per kg / normalized unit)"
    } else {
      "Price (LBP, per kg / normalized unit)"
    }

    ggplot(df, aes(x = date, y = plot_price, colour = series)) +
      geom_line(linewidth = 1.1, na.rm = TRUE) +
      geom_point(size = 1.6, na.rm = TRUE) +
      scale_colour_manual(values = mc_palette, name = NULL) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      labs(
        title    = paste("Item Comparison —", input$compare_item),
        subtitle = "Quantity-normalized prices across sources",
        x        = NULL,
        y        = ylabel
      ) +
      mc_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  }, bg = "#242424")

  # ── Page 2: value boxes ─────────────────────────────────────────────────────

  output$vb_diesel <- renderText({
    latest <- diesel_monthly %>% filter(!is.na(price_usd)) %>% slice_max(date, n = 1)
    if (nrow(latest) == 0) return("N/A")
    paste0("$", round(latest$price_usd, 2), " / 20L")
  })

  output$vb_exch <- renderText({
    latest <- app_set %>%
      filter(!is.na(lbp_usd)) %>%
      slice_max(date, n = 1)
    if (nrow(latest) == 0) return("N/A")
    paste0(format(round(latest$lbp_usd[1], 0), big.mark = ","), " LBP/USD")
  })

  output$vb_cpi <- renderText({
    latest <- app_set %>%
      filter(unit == "index", tolower(item) == "consumer price index") %>%
      arrange(date) %>%
      mutate(growth = (price - lag(price)) / lag(price) * 100) %>%
      filter(!is.na(growth)) %>%
      slice_max(date, n = 1)
    if (nrow(latest) == 0) return("N/A")
    paste0(round(latest$growth[1], 1), "% Growth Rate")
  })

  # Populate CPI category selector once app_set is available
  observe({
    default_cpi <- cpi_categories[tolower(cpi_categories) == "consumer price index"][1]
    if (is.na(default_cpi)) default_cpi <- cpi_categories[1]
    updateSelectInput(session, "cpi_category",
                      choices  = cpi_categories,
                      selected = default_cpi)
  })

  # ── Page 2: fuel plot ───────────────────────────────────────────────────────

  output$fuel_plot <- renderPlot({
    use_usd <- isTRUE(input$fuel_currency)
    price_col <- if (use_usd) "price_usd" else "price"
    ylabel    <- if (use_usd) "Price (USD)" else "Price (LBP)"

    df <- app_set %>%
      filter(source == "IPT") %>%
      mutate(plot_price = .data[[price_col]], date = as.Date(date)) %>%
      filter(!is.na(plot_price))

    ggplot(df, aes(x = date, y = plot_price, colour = item)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      scale_colour_manual(
        values = c("Diesel" = "#BE3144", "Gas" = "#E8963A",
                   "Octane95" = "#4A90D9", "Octane98" = "#5CB85C"),
        name = NULL
      ) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      labs(title = "Fuel Prices in Lebanon", x = NULL, y = ylabel) +
      mc_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  }, bg = "#242424")

  # ── Page 2: exchange rate plot ───────────────────────────────────────────────

  output$exch_plot <- renderPlot({
    df <- app_set %>%
      select(date, lbp_usd) %>%
      filter(!is.na(lbp_usd)) %>%
      mutate(date = floor_date(as.Date(date), "month")) %>%
      group_by(date) %>%
      summarise(lbp_usd = mean(lbp_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(date) %>%
      mutate(
        roll_min = slider::slide_dbl(lbp_usd, min, .before = 1, .after = 1),
        roll_max = slider::slide_dbl(lbp_usd, max, .before = 1, .after = 1)
      )

    ggplot(df, aes(x = date)) +
      geom_ribbon(aes(ymin = roll_min, ymax = roll_max),
                  fill = "#BE3144", alpha = 0.2) +
      geom_line(aes(y = lbp_usd), colour = "#BE3144", linewidth = 1.4) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = comma) +
      labs(title = "LBP / USD Exchange Rate",
           subtitle = "Monthly average with ±1-month range",
           x = NULL, y = "LBP per USD") +
      mc_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, bg = "#242424")

  # ── Page 2: CPI plot ────────────────────────────────────────────────────────

  output$cpi_plot <- renderPlot({
    req(input$cpi_category)

    df <- app_set %>%
      filter(unit == "index", item == input$cpi_category) %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(price)) %>%
      arrange(date) %>%
      mutate(growth = (price - lag(price)) / lag(price) * 100) %>%
      filter(!is.na(growth))

    req(nrow(df) > 0)

    ggplot(df, aes(x = date, y = growth)) +
      geom_col(fill = "#BE3144", alpha = 0.6, width = 20) +
      geom_line(colour = "#BE3144", linewidth = 1.4) +
      geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.5) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title    = paste(input$cpi_category, "— Monthly Growth Rate"),
        subtitle = "Month-over-month % change",
        x        = NULL,
        y        = "Month-over-Month % Change"
      ) +
      mc_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, bg = "#242424")

  # ── Page 3: Leaflet map ─────────────────────────────────────────────────────

  output$nlr_map <- renderLeaflet({
    leaflet(data = most_vuln) %>%
      setView(lat = 33.9, lng = 36, zoom = 8) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        weight       = 0.5,
        color        = "#8c8c8c",
        dashArray    = "3",
        layerId      = ~Cadaster,
        smoothFactor = 0.3,
        fillOpacity  = 0.7,
        fillColor    = ~vuln_pal(Rescaled_Vuln),
        label        = ~lapply(paste0(
          "<b>", Cadaster, "</b><br/>",
          "District: ", District, "<br/>",
          "Governorate: ", Governorate, "<br/>",
          "Vulnerability: ", Vulnerability, "<br/>",
          "Score: ", round(Rescaled_Vuln, 2)
        ), htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2, color = "#BE3144", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addAwesomeMarkers(
        data   = most_vuln[2, ],
        lng    = ~lng, lat = ~lat,
        layerId = ~as.character(Cadaster),
        icon   = highlight_icon,
        popup  = ~paste0("<b>", Cadaster, "</b><br/>",
                         "District: ", District, "<br/>",
                         "Governorate: ", Governorate, "<br/>",
                         "Population: ", Population, "<br/>",
                         "Vulnerability: ", Vulnerability)
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = vuln_pal,
        values   = ~c(0, 100),
        title    = "Vulnerability",
        opacity  = 1
      )
  })

  # ── Page 3: vulnerability table ─────────────────────────────────────────────

  output$vulnerability_table <- renderDT({
    most_vuln %>%
      st_drop_geometry() %>%
      select(Cadaster, District, Governorate, Population,
             Vulnerability, Vulnerability_Weighted) %>%
      datatable(
        rownames  = FALSE,
        filter    = "top",
        selection = list(mode = "single", selected = 2),
        extensions = "Buttons",
        options   = list(
          scrollX   = TRUE,
          paging    = FALSE,
          dom       = "Bfrti",
          buttons   = c("csv", "excel"),
          autoWidth = TRUE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      ) %>%
      formatStyle(
        columns     = 1:6,
        color       = "white",
        fontSize    = "13px"
      )
  }, server = FALSE)

  # ── Page 3: sync table → map marker ─────────────────────────────────────────

  observeEvent(input$vulnerability_table_rows_selected, {
    row <- most_vuln[input$vulnerability_table_rows_selected, ]
    leafletProxy("nlr_map") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        data    = row,
        lng     = ~lng, lat = ~lat,
        layerId = ~as.character(Cadaster),
        icon    = highlight_icon,
        popup   = ~paste0("<b>", Cadaster, "</b><br/>",
                          "District: ", District, "<br/>",
                          "Governorate: ", Governorate, "<br/>",
                          "Population: ", Population, "<br/>",
                          "Vulnerability: ", Vulnerability)
      )
  })

  # ── Page 3: sync map click → table selection ─────────────────────────────────

  observeEvent(input$nlr_map_shape_click, {
    click_id <- input$nlr_map_shape_click$id
    row_idx  <- which(most_vuln$Cadaster == click_id)
    if (length(row_idx) > 0) {
      dataTableProxy("vulnerability_table") %>%
        selectRows(row_idx)
    }
  })

  # ── Page 3: population-weight toggle ────────────────────────────────────────

  observeEvent(input$pop_weight, {
    row  <- most_vuln[input$vulnerability_table_rows_selected %||% 2, ]
    fill_var <- if (isTRUE(input$pop_weight)) "Rescaled_Vuln_Weighted" else "Rescaled_Vuln"
    label_var <- if (isTRUE(input$pop_weight)) {
      ~lapply(paste0("<b>", Cadaster, "</b><br/>",
                     "District: ", District, "<br/>",
                     "Vulnerability (pop. weighted): ", Vulnerability_Weighted, "<br/>",
                     "Score: ", round(Rescaled_Vuln_Weighted, 2)),
              htmltools::HTML)
    } else {
      ~lapply(paste0("<b>", Cadaster, "</b><br/>",
                     "District: ", District, "<br/>",
                     "Vulnerability: ", Vulnerability, "<br/>",
                     "Score: ", round(Rescaled_Vuln, 2)),
              htmltools::HTML)
    }

    leafletProxy("nlr_map", data = most_vuln) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(
        weight       = 0.5,
        color        = "#8c8c8c",
        dashArray    = "3",
        layerId      = ~Cadaster,
        smoothFactor = 0.3,
        fillOpacity  = 0.7,
        fillColor    = ~vuln_pal(get(fill_var)),
        label        = label_var,
        highlightOptions = highlightOptions(
          weight = 2, color = "#BE3144", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addAwesomeMarkers(
        data    = row,
        lng     = ~lng, lat = ~lat,
        layerId = ~as.character(Cadaster),
        icon    = highlight_icon,
        popup   = ~paste0("<b>", Cadaster, "</b>")
      )
  })

  # ── Page 3: NLR time series ─────────────────────────────────────────────────

  nlr_set <- reactive({
    idx <- input$vulnerability_table_rows_selected
    if (is.null(idx) || length(idx) == 0) idx <- 2L
    cadaster_row <- most_vuln[idx, ] %>% st_drop_geometry()

    nlr_raw %>%
      filter(admin3Name == cadaster_row$Cadaster[[1]]) %>%
      arrange(date) %>%
      mutate(nlr_growth = (mean - lag(mean)) / lag(mean) * 100)
  })

  output$ts_nlr <- renderPlot({
    df <- nlr_set()
    req(nrow(df) > 0)

    cadaster_name <- df$admin3Name[1]

    # Scale factor: map growth-rate range onto NLR range for sec.axis
    nlr_range    <- range(df$mean,       na.rm = TRUE)
    growth_range <- range(df$nlr_growth, na.rm = TRUE)
    nlr_span     <- diff(nlr_range)
    growth_span  <- diff(growth_range)

    if (is.na(growth_span) || growth_span == 0 || is.na(nlr_span) || nlr_span == 0) {
      scale_factor <- 1
      shift        <- 0
    } else {
      scale_factor <- nlr_span / growth_span
      shift        <- nlr_range[1] - growth_range[1] * scale_factor
    }

    df <- df %>%
      mutate(growth_scaled = nlr_growth * scale_factor + shift)

    ggplot(df, aes(x = date)) +
      geom_line(aes(y = mean), colour = "#BE3144", linewidth = 1.3, na.rm = TRUE) +
      geom_line(aes(y = growth_scaled), colour = "#e8e8e8",
                linewidth = 1.1, linetype = "dashed", na.rm = TRUE) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
      scale_y_continuous(
        name   = "NLR (nW/cm²/sr)",
        labels = number_format(accuracy = 0.1),
        sec.axis = sec_axis(
          transform = ~ (. - shift) / scale_factor,
          name      = "NLR Growth Rate (%)",
          labels    = function(x) paste0(round(x, 1), "%")
        )
      ) +
      labs(
        title    = paste("Night Light Radiance —", cadaster_name),
        subtitle = "Red: NLR  |  Dashed: NLR monthly growth rate (%)",
        x        = NULL
      ) +
      mc_theme() +
      theme(
        axis.text.x        = element_text(angle = 45, hjust = 1),
        axis.title.y.right = element_text(colour = "#b0b0b0")
      )
  }, bg = "#242424")

}
