# modules.R
# Builds `app_set`: the combined monthly Lebanon food/fuel price dataset.
# Source this file at app startup; the final object is `app_set`.
#
# AWS credentials are read from environment variables AWS_ACCESS_KEY_ID and
# AWS_SECRET_ACCESS_KEY. Set these in:
#   - Local development: ~/.Renviron
#   - GitHub/Posit Connect: repository secrets / environment variables

library(aws.s3)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)
library(slider)
library(stringr)

# Validate that credentials are present before attempting any S3 reads
if (nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0 ||
    nchar(Sys.getenv("AWS_SECRET_ACCESS_KEY")) == 0) {
  stop(
    "AWS credentials not found. Set AWS_ACCESS_KEY_ID and ",
    "AWS_SECRET_ACCESS_KEY as environment variables before running the app."
  )
}



# ── S3 helpers ────────────────────────────────────────────────────────────────

s3_csv  <- function(key) s3read_using(FUN = read.csv,          object = key)
s3_json <- function(key) s3read_using(FUN = jsonlite::fromJSON, object = key)


# ── Shared helper: outlier smoothing ─────────────────────────────────────────
# Replaces values more than 2 SD from the group median with a 3-point
# centred moving average. Applied within an existing group_by context.

smooth_outliers <- function(x) {
  is_outlier <- abs(x - median(x, na.rm = TRUE)) > 2 * sd(x, na.rm = TRUE)
  ifelse(!is_outlier, x, slider::slide_dbl(x, mean, .before = 1, .after = 1))
}


# ── Reference data ────────────────────────────────────────────────────────────

load_exchange_rates <- function() {
  s3_csv("s3://mena-regional/Lebanon/shiny/exch.csv") %>%
    mutate(date = floor_date(as.Date(date, tryFormats = "%m/%d/%Y"), "week")) %>%
    group_by(date) %>%
    summarise(lbp_usd = mean(lbp_usd), .groups = "drop")
}

load_smeb_def <- function() {
  s3_csv("s3://mena-regional/Lebanon/shiny/smeb_real.csv")
}

load_cpi <- function(exch_monthly) {
  s3_csv("s3://mena-regional/Lebanon/shiny/cpi.csv") %>%
    rename(item = id, price = value) %>%
    mutate(
      date      = floor_date(as.Date(date), "month"),
      price_usd = price,
      quantity  = 1,
      unit      = "index",
      source    = "Lebanese Government"
    ) %>%
    left_join(exch_monthly, by = "date")
}


# ── Source: Lebanese Government basket ───────────────────────────────────────

make_lebgov_set <- function(smeb_def, exch) {

  lebgov <- s3_csv("s3://mena-regional/Lebanon/shiny/fullbasket.csv") %>%
    select(date, item = item_en, item_ar, quantity, unit,
           price = price_lbp, price_usd) %>%
    mutate(item = str_to_title(item)) %>%
    group_by(item) %>%
    arrange(date) %>%
    mutate(
      price     = smooth_outliers(price),
      price_usd = smooth_outliers(price_usd)
    ) %>%
    filter(mean(is.na(price)) <= 0.70) %>%
    ungroup()

  smeb_items <- smeb_def %>%
    mutate(leb_item = str_to_title(leb_item)) %>%
    rename(item = leb_item) %>%
    select(item, leb_quant_hh)

  smeb_total <- lebgov %>%
    inner_join(smeb_items, by = "item") %>%
    group_by(date, item, leb_quant_hh) %>%
    summarise(
      price     = mean(price,     na.rm = TRUE),
      price_usd = mean(price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_price     = leb_quant_hh * price,
      weighted_price_usd = leb_quant_hh * price_usd
    ) %>%
    group_by(date) %>%
    summarise(
      price     = sum(weighted_price,     na.rm = TRUE),
      price_usd = sum(weighted_price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    mutate(item = "SMEB", item_ar = "SMEB", quantity = 1, unit = "basket")

  bind_rows(smeb_total, lebgov) %>%
    select(-item_ar) %>%
    mutate(date = floor_date(as.Date(date), "week")) %>%
    group_by(date, item, unit, quantity) %>%
    summarise(
      price     = mean(price,     na.rm = TRUE),
      price_usd = mean(price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exch, by = "date") %>%
    mutate(source = "Lebanese Government")
}


# ── Source: WFP ──────────────────────────────────────────────────────────────

make_wfp_set <- function(smeb_def, exch) {

  wfp_raw <- s3_csv("s3://mena-regional/Lebanon/food-prices/wfp_simple.csv")

  smeb_items <- smeb_def %>%
    select(item = wfp_item, quant = org_wfp_name)

  smeb_total <- wfp_raw %>%
    filter(priceflag == "actual", pricetype == "Retail") %>%
    select(date, item = commodity, price, price_usd = usdprice) %>%
    mutate(item = as.character(item)) %>%
    inner_join(smeb_items, by = "item") %>%
    mutate(
      weighted_price     = price     * quant,
      weighted_price_usd = price_usd * quant
    ) %>%
    group_by(date, item) %>%
    summarise(
      weighted_price     = mean(weighted_price,     na.rm = TRUE),
      weighted_price_usd = mean(weighted_price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(date) %>%
    summarise(
      price     = sum(weighted_price,     na.rm = TRUE),
      price_usd = sum(weighted_price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(date)) %>%
    arrange(date) %>%
    mutate(item = "SMEB", unit = "basket", quantity = 1)

  fuel_labels <- c(
    "Fuel (diesel)"                     = "Diesel",
    "Fuel (gas)"                        = "Gas",
    "Fuel (petrol-gasoline, 95 octane)" = "Octane95"
  )

  exch_monthly <- exch %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(lbp_usd = mean(lbp_usd), .groups = "drop")

  wfp_processed = wfp_raw %>%
    select(date, price, price_usd = usdprice, item = commodity, unit) %>%
    mutate(item = as.character(item)) %>%
    bind_rows(smeb_total) %>%
    filter(item != "Exchange rate (unofficial)") %>%
    mutate(
      item = coalesce(fuel_labels[item], item),
      date = floor_date(as.Date(date), "month"),
      unit = str_trim(unit),
      quantity = if_else(
        str_detect(unit, "^[0-9]"),
        as.numeric(str_extract(unit, "^[0-9]+(\\.[0-9]+)?")),
        1
      ),
      unit = str_extract(unit, "[A-Za-z]+$"),
      unit = case_when(
        unit == "G"    ~ "g",
        unit == "KG"   ~ "kg",
        unit == "pcs"  ~ "pieces",
        unit == "Head" ~ "head",
        TRUE           ~ unit
      )
    ) 
  
  wfp_processed %>%
    left_join(exch_monthly, by = "date") %>%
    mutate(source = "WFP") %>%
    dplyr::mutate(lbp_usd = ifelse(is.na(lbp_usd), price/price_usd, lbp_usd))
}


# ── Source: Carrefour ─────────────────────────────────────────────────────────

make_carrefour_set <- function(exch) {

  all_products <- s3_json("s3://mena-regional/Lebanon/food-prices/Carrefour/normalized/normalized_all.json")
  smeb_basket  <- s3_json("s3://mena-regional/Lebanon/food-prices/Carrefour/normalized/smeb_all.json")

  smeb_long <- smeb_basket %>%
    select(date, food_smeb_lbp:total_smeb_usd) %>%
    pivot_longer(
      cols          = food_smeb_lbp:total_smeb_usd,
      names_to      = c("item", ".value"),
      names_pattern = "^(food|nfi|total)_smeb_(lbp|usd)$"
    ) %>%
    rename(price = lbp, price_usd = usd) %>%
    mutate(item = paste0(item, "_SMEB"), unit = "basket", quantity = 1)

  all_products %>%
    select(date, item = item_name, unit = standard_unit,
           price = median_price_lbp, price_usd = median_price_usd) %>%
    mutate(quantity = 1) %>%
    bind_rows(smeb_long) %>%
    mutate(
      date = floor_date(as.Date(date), "week"),
      unit = if_else(unit == "liter", "L", unit)
    ) %>%
    group_by(date, item, unit, quantity) %>%
    summarise(
      price     = mean(price,     na.rm = TRUE),
      price_usd = mean(price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exch, by = "date") %>%
    mutate(source = "Carrefour")
}


# ── Source: IPT fuel prices ───────────────────────────────────────────────────

make_fuel_set <- function(exch, col_order) {

  s3_csv("s3://mena-regional/Lebanon/shiny/ipt_fuel_prices.csv") %>%
    mutate(date = floor_date(as.Date(Date, tryFormats = "%m/%d/%Y"), "week")) %>%
    select(-Date) %>%
    pivot_longer(cols = Octane98:Gas, names_to = "item", values_to = "price") %>%
    group_by(date, item) %>%
    summarise(price = mean(price), .groups = "drop") %>%
    left_join(exch, by = "date") %>%
    mutate(
      price_usd = price / lbp_usd,
      unit      = "canister",
      quantity  = 1,
      source    = "IPT"
    ) %>%
    select(all_of(col_order))
}


# Per-unit Carrefour prices for cross-source item comparison (from smeb_all items)
make_carrefour_items <- function(exch) {
  smeb_basket <- s3_json("s3://mena-regional/Lebanon/food-prices/Carrefour/normalized/smeb_all.json")

  exch_monthly <- exch %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(lbp_usd = mean(lbp_usd), .groups = "drop")

  smeb_basket %>%
    select(date, items) %>%
    unnest(items) %>%
    select(date, item, unit,
           price = price_per_unit_lbp, price_usd = price_per_unit_usd) %>%
    mutate(
      date     = floor_date(as.Date(date), "month"),
      unit     = if_else(unit == "liter", "L", unit),
      quantity = 1
    ) %>%
    group_by(date, item, unit, quantity) %>%
    summarise(
      price     = mean(price,     na.rm = TRUE),
      price_usd = mean(price_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(exch_monthly, by = "date") %>%
    mutate(source = "Carrefour")
}


# ── Main: assemble app_set ────────────────────────────────────────────────────

build_app_set <- function() {

  exch     <- load_exchange_rates()
  smeb_def <- load_smeb_def()

  exch_monthly <- exch %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(lbp_usd = mean(lbp_usd), .groups = "drop")

  lebgov    <- make_lebgov_set(smeb_def, exch)
  carrefour <- make_carrefour_set(exch)
  fuel      <- make_fuel_set(exch, col_order = colnames(carrefour))
  wfp       <- make_wfp_set(smeb_def, exch)
  cpi       <- load_cpi(exch_monthly)

  # Weekly sources combined, then averaged to monthly
  weekly_set <- bind_rows(lebgov, carrefour, fuel) %>%
    filter(date >= min(lebgov$date))

  monthly_set <- weekly_set %>%
    mutate(date = floor_date(as.Date(date), "month")) %>%
    group_by(date, item, unit, quantity, source) %>%
    summarise(
      price     = mean(price,     na.rm = TRUE),
      price_usd = mean(price_usd, na.rm = TRUE),
      lbp_usd   = mean(lbp_usd,   na.rm = TRUE),
      .groups = "drop"
    )

  # WFP is already monthly; trim to match coverage window and append
  wfp_trimmed <- wfp %>%
    filter(date >= min(lebgov$date)) %>%
    select(all_of(colnames(monthly_set)))

  list(
    app_set         = bind_rows(monthly_set, wfp_trimmed, cpi),
    carrefour_items = make_carrefour_items(exch)
  )
}

built <- build_app_set()
app_set         <- built$app_set
carrefour_items <- built$carrefour_items
