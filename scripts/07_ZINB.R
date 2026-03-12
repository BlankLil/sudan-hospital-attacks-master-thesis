#Zero-Inflated Negative Binomial Regression 

#ZINB_pop ----
zinb_pop <- zeroinfl(
  hospital_attacks ~ 
    territorial_events_lag1 +
    conflict_intensity + 
    n_territorial_events +
    log_population |
    conflict_intensity +
    log_population + 
    territorial_events_lag1 +
    n_territorial_events,
  data = panel_master_lag,
  dist = "negbin"
)

summary(zinb_pop)

library(stargazer)

stargazer(
  zinb_pop,
  type = "text",
  title = "ZINB Regression Results"
)

zinb_pop_lag <- zeroinfl(
  hospital_attacks ~ 
    conflict_intensity_lag1 +
    territorial_events_lag1 +
    n_territorial_events +
    log_population |
    conflict_intensity_lag1 +
    log_population + 
    territorial_events_lag1 +
    n_territorial_events,
  data = panel_master_lag,
  dist = "negbin"
)

summary(zinb_pop_lag)
#actor specific ZINBs ----
#first: add actor specifications in panel_master_lag ----
hospital_attacks_actor <- sudan_hospital_attacks_sf %>%
  mutate(
    RSF = grepl("Rapid Support Forces", `Reported Perpetrator Name`, ignore.case = TRUE),
    SAF = grepl("Sudanese Armed Forces", `Reported Perpetrator Name`, ignore.case = TRUE)
  )

hospital_attacks_actor <- st_join(
  hospital_attacks_actor,
  grid_sudan %>% dplyr::select(grid_id),
  join = st_intersects
)
sum(is.na(hospital_attacks_actor$grid_id))

#aggregation grid-week-actor
attacks_gridweek <- hospital_attacks_actor %>%
  st_drop_geometry() %>%
  group_by(grid_id, week) %>%
  summarise(
    hospital_attacks_RSF = sum(RSF, na.rm = TRUE),
    hospital_attacks_SAF = sum(SAF, na.rm = TRUE),
    .groups = "drop"
  )

panel_master_lag_actors <- panel_master_lag %>%
  left_join(attacks_gridweek, by = c("grid_id", "week")) %>%
  mutate(
    hospital_attacks_RSF = ifelse(is.na(hospital_attacks_RSF), 0, hospital_attacks_RSF),
    hospital_attacks_SAF = ifelse(is.na(hospital_attacks_SAF), 0, hospital_attacks_SAF)
  )

#second: add territorial losses 
territorial_losses <- territorial_events_sf %>%
  mutate(
    loss_SAF = sub_event_type == "Non-state actor overtakes territory",
    loss_RSF = sub_event_type == "Government regains territory",
    loss_nonviolent = sub_event_type == "Non-violent transfer of territory"
  )
#put to grids
territorial_losses <- st_join(
  territorial_losses,
  grid_sudan %>% dplyr::select(grid_id),
  join = st_intersects
)
#aggregate on grid-week
territorial_losses_gridweek <- territorial_losses %>%
  st_drop_geometry() %>%
  group_by(grid_id, week) %>%
  summarise(
    territorial_losses_RSF = sum(loss_RSF, na.rm = TRUE),
    territorial_losses_SAF = sum(loss_SAF, na.rm = TRUE),
    territorial_transfers_nonviolent = sum(loss_nonviolent, na.rm = TRUE),
    .groups = "drop"
  )
#merge in panel
panel_master_lag_actors <- panel_master_lag_actors %>%
  left_join(territorial_losses_gridweek, by = c("grid_id", "week")) %>%
  mutate(
    territorial_losses_RSF = ifelse(is.na(territorial_losses_RSF), 0, territorial_losses_RSF),
    territorial_losses_SAF = ifelse(is.na(territorial_losses_SAF), 0, territorial_losses_SAF),
    territorial_transfers_nonviolent = ifelse(is.na(territorial_transfers_nonviolent), 0,
                                              territorial_transfers_nonviolent)
  )

#create lags for territorial losses
panel_master_lag_actors <- panel_master_lag_actors %>%
  arrange(grid_id, week) %>%
  group_by(grid_id) %>%
  mutate(
    territorial_losses_RSF_lag1 = lag(territorial_losses_RSF, 1),
    territorial_losses_SAF_lag1 = lag(territorial_losses_SAF, 1)
  ) %>%
  ungroup()
#sanity check
summary(panel_master_lag_actors$territorial_losses_RSF_lag1)
summary(panel_master_lag_actors$territorial_losses_SAF_lag1)

#check share: Lag share - Did something happen before (in grid), that might have influenced the attack? 
panel_master_lag_actors %>%
  filter(hospital_attacks_RSF > 0) %>%
  summarise(
    share_conflict_lag1 = mean(conflict_intensity_lag1 > 0, na.rm = TRUE),
    share_territorial_lag1 = mean(territorial_losses_RSF_lag1 > 0, na.rm = TRUE)
  )

#ZINB_RSF ----

#zinb_RSF - contemporaneous
# RSF – contemporaneous
zinb_RSF_t <- zeroinfl(
  hospital_attacks_RSF ~
    conflict_intensity +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population |
    conflict_intensity +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population,
  data = panel_master_lag_actors,
  dist = "negbin"
)

summary(zinb_RSF_t)
# RSF – lagged
zinb_RSF_lag <- zeroinfl(
  hospital_attacks_RSF ~
    conflict_intensity_lag1 +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population |
    conflict_intensity_lag1 +
    territorial_losses_RSF +
    territorial_losses_RSF_lag1 +
    log_population,
  data = panel_master_lag_actors,
  dist = "negbin"
)

summary(zinb_RSF_lag)

#model fit? 
AIC(zinb_RSF_t, zinb_RSF_lag)

#ZINB_SAF ---- 
#zinb SAF contemporaneous 
zinb_SAF_t <- zeroinfl(
  hospital_attacks_SAF ~
    conflict_intensity +
    territorial_losses_SAF +
    log_population |
    conflict_intensity +
    territorial_losses_SAF +
    log_population,
  data = panel_master_lag_actors,
  dist = "negbin"
)

summary(zinb_SAF_t)
# SAF – lagged
zinb_SAF_lag <- zeroinfl(
  hospital_attacks_SAF ~
    conflict_intensity_lag1 +
    territorial_losses_SAF +
    log_population |
    conflict_intensity_lag1 +
    territorial_losses_SAF +
    log_population,
  data = panel_master_lag_actors,
  dist = "negbin"
)

summary(zinb_SAF_lag)

#exporting regression tables ----
library(dplyr)
library(tidyr)
library(tibble)
library(knitr)
library(kableExtra)

# Two clean ZINB regression tables (contemporaneous vs lagged) ----
# Fixes MASS::select() masking by using explicit namespaces
# Models:
#   zinb_pop, zinb_pop_lag
#   zinb_RSF_t, zinb_RSF_lag
#   zinb_SAF_t, zinb_SAF_lag
# Output:
#   ZINB_table_contemporaneous.html
#   ZINB_table_lagged.html


# Helpers ----
pstars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01,  "**",
                ifelse(p < 0.05,  "*",
                       ifelse(p < 0.1,   ".", ""))))
}

fmt_cell <- function(est, se, p) {
  paste0(sprintf("%.2f", est), pstars(p),
         "<br>(", sprintf("%.2f", se), ")")
}

extract_zinb_component <- function(model, component = c("count", "zero"), model_label) {
  component <- match.arg(component)
  s <- summary(model)$coefficients[[component]]
  
  as.data.frame(s) |>
    tibble::rownames_to_column("term") |>
    dplyr::transmute(
      component = ifelse(component == "count", "Count model", "Zero model"),
      term,
      model = model_label,
      estimate = Estimate,
      se = `Std. Error`,
      p = `Pr(>|z|)`
    )
}

clean_terms <- function(x) {
  dplyr::recode(
    x,
    "(Intercept)" = "(Intercept)",
    "conflict_intensity"      = "Conflict intensity (t)",
    "conflict_intensity_lag1" = "Conflict intensity (t−1)",
    "territorial_events_lag1" = "Territorial events (t−1)",
    "n_territorial_events"    = "Number of territorial events (t)",
    "n_territorial_events_lag1" = "Number of territorial events (t−1)", # if it exists
    "log_population"          = "Log (1+ population)",
    "territorial_losses_RSF"      = "Territorial losses (RSF, t)",
    "territorial_losses_RSF_lag1" = "Territorial losses (RSF, t−1)",
    "territorial_losses_SAF"      = "Territorial losses (SAF, t)",
    "territorial_losses_SAF_lag1" = "Territorial losses (SAF, t−1)",     # if it exists
    "Log(theta)" = "Log(theta)",
    .default = x
  )
}

# Table builder (single table) ----

make_zinb_table <- function(models_named,
                            count_levels,
                            zero_levels,
                            caption,
                            outfile) {
  
  # Extract count + zero components for each model
  df_long <- dplyr::bind_rows(lapply(names(models_named), function(lbl) {
    m <- models_named[[lbl]]
    dplyr::bind_rows(
      extract_zinb_component(m, "count", lbl),
      extract_zinb_component(m, "zero",  lbl)
    )
  })) |>
    dplyr::mutate(term = clean_terms(term)) |>
    dplyr::distinct(component, term, model, .keep_all = TRUE) |>
    dplyr::mutate(cell = fmt_cell(estimate, se, p))
  
  # Force stable panel row ordering
  df_long <- df_long |>
    dplyr::mutate(
      component = factor(component, levels = c("Count model", "Zero model")),
      term_ord = dplyr::case_when(
        component == "Count model" ~ match(term, count_levels),
        component == "Zero model"  ~ match(term, zero_levels),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::arrange(component, term_ord, term, model)
  
  # Wide format
  df_wide <- df_long |>
    dplyr::select(component, term, model, cell) |>
    tidyr::pivot_wider(names_from = model, values_from = cell)
  
  model_cols <- names(models_named)
  model_cols <- intersect(model_cols, names(df_wide))  # safety
  
  df_wide <- df_wide |>
    dplyr::select(component, term, dplyr::all_of(model_cols))
  
  # Fit statistics
  fit <- tibble::tibble(
    component = "",
    term = c("AIC", "Log Likelihood", "Num. obs.")
  )
  
  for (lbl in model_cols) {
    m <- models_named[[lbl]]
    fit[[lbl]] <- c(stats::AIC(m), as.numeric(stats::logLik(m)), nrow(m$model))
  }
  
  fit <- fit |>
    dplyr::mutate(dplyr::across(dplyr::all_of(model_cols), ~ as.character(round(.x, 2))))
  
  df_out <- dplyr::bind_rows(df_wide, fit)
  
  # Render (Count/Zero panel grouping)
  count_rows <- which(df_out$component == "Count model")
  zero_rows  <- which(df_out$component == "Zero model")
  
  kbl <- df_out |>
    dplyr::select(term, dplyr::all_of(model_cols)) |>
    knitr::kable(
      format = "html",
      escape = FALSE,
      booktabs = TRUE,
      align = paste0("l", paste(rep("c", length(model_cols)), collapse = "")),
      col.names = c("", model_cols),
      caption = caption
    ) |>
    kableExtra::kable_styling(full_width = FALSE, html_font = "Times New Roman") |>
    kableExtra::pack_rows("Count model", min(count_rows), max(count_rows), bold = TRUE) |>
    kableExtra::pack_rows("Zero model",  min(zero_rows),  max(zero_rows),  bold = TRUE) |>
    kableExtra::footnote(
      general = "Standard errors in parentheses. *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
      general_title = ""
    )
  
  print(kbl)
  kableExtra::save_kable(kbl, outfile)
  invisible(kbl)
}

# Table 1: Contemporaneous (t) models ----

models_t <- list(
  "Baseline" = zinb_pop,
  "RSF"      = zinb_RSF_t,
  "SAF"      = zinb_SAF_t
)

count_levels_t <- c(
  "(Intercept)",
  "Conflict intensity (t)",
  "Territorial events (t-1)",
  "Number of territorial events (t)",
  "Territorial losses (RSF, t)",
  "Territorial losses (SAF, t)",
  "Log (1 + population)",
  "Log(theta)"
)

zero_levels_t <- c(
  "(Intercept)",
  "Conflict intensity (t)",
  "Territorial events (t-1)",
  "Territorial losses (RSF, t)",
  "Territorial losses (SAF, t)",
  "Log (1 + population)",
  "Number of territorial events (t)"
)

make_zinb_table(
  models_named = models_t,
  count_levels = count_levels_t,
  zero_levels  = zero_levels_t,
  caption = "ZINB contemporaneous models: count and zero-inflation components",
  outfile = "ZINB_table_contemporaneous.html"
)

# Table 2: Lagged (t−1) models----

models_lag <- list(
  "Baseline" = zinb_pop_lag,
  "RSF"      = zinb_RSF_lag,
  "SAF"      = zinb_SAF_lag
)

count_levels_lag <- c(
  "(Intercept)",
  "Conflict intensity (t−1)",
  "Territorial events (t−1)",
  "Number of territorial events (t)",   # keep if your lag model still has it
  "Territorial losses (RSF, t−1)",      # keep if present
  "Territorial losses (SAF, t−1)",      # keep if present
  "Log (1 + population)",
  "Log(theta)"
)

zero_levels_lag <- c(
  "(Intercept)",
  "Conflict intensity (t−1)",
  "Territorial events (t−1)",
  "Territorial losses (RSF, t−1)",      # keep if present
  "Territorial losses (SAF, t−1)",      # keep if present
  "Log (1 + population)",
  "Number of territorial events (t)"    # keep if present in zero part
)

make_zinb_table(
  models_named = models_lag,
  count_levels = count_levels_lag,
  zero_levels  = zero_levels_lag,
  caption = "ZINB lagged models: count and zero-inflation components",
  outfile = "ZINB_table_lagged.html"
)


# test plotting ZINBs ----
library(dplyr)
library(ggplot2)
library(MASS)

p_any_attack_zeroinfl <- function(model, newdata) {
  # linear predictors
  eta_zero  <- predict(model, newdata = newdata, type = "zero")
  eta_count <- predict(model, newdata = newdata, type = "count")
  
  # components
  pi <- plogis(eta_zero)        # structural-zero probability
  mu <- exp(eta_count)          # count mean
  theta <- model$theta          # NB dispersion
  
  # NB zero probability
  p_nb0 <- (theta / (theta + mu))^theta
  
  # overall Pr(Y >= 1)
  1 - (pi + (1 - pi) * p_nb0)
}


#prediction grids (scale identical for SAF & RSF)
make_grid_ci <- function(data, var, n = 100) {
  base <- data %>%
    summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>%
    slice(1)
  
  grid <- base[rep(1, n), , drop = FALSE]
  grid[[var]] <- seq(
    min(data[[var]], na.rm = TRUE),
    max(data[[var]], na.rm = TRUE),
    length.out = n
  )
  grid
}

#predicted probability plot RSF
grid_RSF <- make_grid_ci(panel_master_lag_actors, "conflict_intensity")

grid_RSF$pr_any <- p_any_attack_zeroinfl(zinb_RSF_t, grid_RSF)

ggplot(grid_RSF, aes(x = conflict_intensity, y = pr_any)) +
  
  geom_line(size = 0.5) +
  labs(
    x = "Conflict intensity",
    y = "Pr(≥1 hospital attack)",
    title = "RSF: Predicted probability of hospital attacks"
  ) +
  theme_minimal(base_family = "Times New Roman")

#predicted probability plot SAF
grid_SAF <- make_grid_ci(panel_master_lag_actors, "conflict_intensity")

grid_SAF$pr_any <- p_any_attack_zeroinfl(zinb_SAF_t, grid_SAF)

ggplot(grid_SAF, aes(x = conflict_intensity, y = pr_any)) +
  geom_line(size = 0.5) +
  labs(
    x = "Conflict intensity",
    y = "Pr(≥1 hospital attack)",
    title = "SAF: Predicted probability of hospital attacks"
  ) +
  theme_minimal(base_family = "Times New Roman")

#figure: SAF vs RSF 

grid_RSF$actor <- "RSF"
grid_SAF$actor <- "SAF"

plot_df <- bind_rows(
  grid_RSF %>% dplyr::select(conflict_intensity, pr_any, actor),
  grid_SAF %>% dplyr::select(conflict_intensity, pr_any, actor)
)

ggplot(plot_df,
       aes(x = conflict_intensity, y = pr_any, linetype = actor)) +
  geom_line(size = 0.5) +
  labs(
    x = "Conflict intensity",
    y = "Pr(≥1 hospital attack)",
    title = "Predicted probability of hospital attacks by actor",
    caption = "Predictions based on zero-inflated negative binomial models."
  ) +
  theme_minimal(base_family = "Times New Roman")

#plot RSF x territorial loss
grid_RSF_loss <- expand.grid(
  conflict_intensity = seq(
    min(panel_master_lag_actors$conflict_intensity, na.rm = TRUE),
    max(panel_master_lag_actors$conflict_intensity, na.rm = TRUE),
    length.out = 80
  ),
  territorial_losses_RSF = c(0, 1)
)

grid_RSF_loss$territorial_losses_RSF_lag1 <- median(
  panel_master_lag_actors$territorial_losses_RSF_lag1, na.rm = TRUE
)
grid_RSF_loss$log_population <- median(
  panel_master_lag_actors$log_population, na.rm = TRUE
)

grid_RSF_loss$pr_any <- p_any_attack_zeroinfl(zinb_RSF_t, grid_RSF_loss)

grid_RSF_loss$loss <- factor(
  grid_RSF_loss$territorial_losses_RSF,
  labels = c("No territorial loss", "Territorial loss")
)

ggplot(grid_RSF_loss,
       aes(x = conflict_intensity, y = pr_any, linetype = loss)) +
  geom_line(size = 0.5) +
  labs(
    x = "Conflict intensity",
    y = "Pr(≥1 hospital attack)",
    title = "RSF: Conflict intensity × territorial losses"
  ) +
  theme_minimal(base_family = "Times New Roman")

##
#structural zero probability vs conflict intensity
grid_RSF$pi <- plogis(predict(zinb_RSF_t, newdata = grid_RSF, type = "zero"))
grid_RSF$mu <- exp(predict(zinb_RSF_t, newdata = grid_RSF, type = "count"))

ggplot(grid_RSF, aes(x = conflict_intensity, y = pi)) +
  geom_line() +
  labs(x = "Conflict intensity", y = "Structural zero probability (π)",
       title = "RSF: Structural-zero probability vs conflict intensity") +
  theme_minimal(base_family = "Times New Roman")

#expected count (given at risk) vs conflict intensity 
ggplot(grid_RSF, aes(x = conflict_intensity, y = mu)) +
  geom_line() +
  labs(x = "Conflict intensity", y = "Count mean (μ)",
       title = "RSF: Expected count (given at risk) vs conflict intensity") +
  theme_minimal(base_family = "Times New Roman")

#test further (Plot:Zero inflation coefficients: SAF vs RSF) ----
summary(zinb_RSF_t)$coefficients
extract_zeroinfl_coefs <- function(model, component = c("count", "zero")) {
  component <- match.arg(component)
  
  coefs <- summary(model)$coefficients[[component]]
  
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    z = coefs[, "z value"],
    p.value = coefs[, "Pr(>|z|)"],
    row.names = NULL
  )
}

#coefficient plots - SAF vs. RSF, zero part only 
zi_SAF <- extract_zeroinfl_coefs(zinb_SAF_t, "zero") %>%
  mutate(actor = "SAF")

zi_RSF <- extract_zeroinfl_coefs(zinb_RSF_t, "zero") %>%
  mutate(actor = "RSF")

zi_all <- bind_rows(zi_SAF, zi_RSF) %>%
  filter(term %in% c(
    "conflict_intensity",
    "conflict_intensity_lag1",
    "territorial_losses_SAF",
    "territorial_losses_RSF"
  )) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

zi_all <- zi_all %>%
  mutate(term_actor = paste(term, actor, sep = " - "))


ggplot(zi_all, aes(x = estimate, y = term, shape = actor)) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect on probability of being structurally attack-free",
    y = "",
    title = "Zero-inflation coefficients: SAF vs RSF",
    caption = "Negative values indicate a higher risk of hospital attacks."
  ) +
  theme_minimal(base_family = "Times New Roman")


#new with separate lines 
# coefficient plots - SAF vs. RSF, zero part only 
zi_SAF <- extract_zeroinfl_coefs(zinb_SAF_t, "zero") %>%
  mutate(actor = "SAF")

zi_RSF <- extract_zeroinfl_coefs(zinb_RSF_t, "zero") %>%
  mutate(actor = "RSF")

zi_all <- bind_rows(zi_SAF, zi_RSF) %>%
  filter(term %in% c(
    "conflict_intensity",
    "conflict_intensity_lag1",
    "territorial_losses_SAF",
    "territorial_losses_RSF"
  )) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# create combined label
zi_all <- zi_all %>%
  mutate(term_actor = paste(term, actor, sep = " - "))

zi_all <- zi_all %>%
  mutate(
    term_actor = case_when(
      term == "conflict_intensity" & actor == "SAF" ~ "Conflict intensity (SAF)",
      term == "conflict_intensity" & actor == "RSF" ~ "Conflict intensity (RSF)",
      term == "territorial_losses_SAF" ~ "Territorial losses (SAF)",
      term == "territorial_losses_RSF" ~ "Territorial losses (RSF)"
    )
  )

# plot
p<- ggplot(zi_all, aes(x = estimate, y = term_actor, shape = actor)) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect on probability of being structurally attack-free",
    y = "",
    title = "Zero-inflation coefficients: SAF vs RSF",
  ) +
  theme_minimal(base_family = "Times New Roman")


ggsave(
  filename = "zero_inflation_coefficients_plot.png",
  plot = p,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
