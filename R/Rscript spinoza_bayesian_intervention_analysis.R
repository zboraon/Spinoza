# Bayesian interrupted time-series analysis of Spinoza theses tagged "siyaset"
# Intervention years: 2008, 2009, and 2010
# Each analysis is saved in its own subfolder.
#
# This is the Bayesian-only version of spinoza_intervention_analysis.R:
#
# Default input files:
#   spinoza.xlsx    : cleaned all-Spinoza-theses master dataset (denominator)
#   TPT_ve_PT.xlsx  : theses tagged "siyaset" (numerator)
#
# Run:
#   Rscript spinoza_bayesian_intervention_analysis.R
#
# Optional custom paths:
#   Rscript spinoza_bayesian_intervention_analysis.R /path/spinoza.xlsx /path/TPT_ve_PT.xlsx

INTERVENTION_YEARS <- c(2008L, 2009L, 2010L)
CHAINS <- 4L
ITER <- 4000L
CORES <- min(4L, parallel::detectCores())
SEED <- 20260831L

required_packages <- c(
  "readxl", "dplyr", "tidyr", "readr", "stringr",
  "ggplot2", "scales", "showtext", "sysfonts",
  "brms", "bayesplot", "posterior"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) {
  stop(
    "Install missing packages first:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

default_dir <- paste0(
  "~/Documents/article_drafts/efecenk_text/20260829_spinoza_files/main_text/r_codes_for_spinoza/"
)
args <- commandArgs(trailingOnly = TRUE)
all_file <- if (length(args) >= 1L) args[[1L]] else file.path(default_dir, "Spinoza.xlsx")
politics_file <- if (length(args) >= 2L) args[[2L]] else file.path(default_dir, "TPT_ve_PT.xlsx")
root_output_dir <- file.path(
  dirname(politics_file),
  "bayesian_intervention_runs"
)
dir.create(root_output_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(all_file, politics_file)) {
  if (!file.exists(f)) stop("File not found: ", f, call. = FALSE)
}

# Helpers --------------------------------------------------------------------
clean_text <- function(x) {
  out <- str_squish(as.character(x))
  low <- str_to_lower(out, locale = "tr")
  out[is.na(out) | low %in% c("", "na", "n/a", "null", "<br>", "-", "—")] <- NA
  out
}

read_theses <- function(path) {
  dat <- read_excel(path)
  names(dat) <- trimws(names(dat))
  needed <- c("Tez No", "Yıl")
  missing <- setdiff(needed, names(dat))
  if (length(missing)) {
    stop(basename(path), " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  dat |>
    mutate(
      .row = row_number(),
      thesis_no = clean_text(`Tez No`),
      thesis_id = if_else(is.na(thesis_no), paste0(basename(path), "_row_", .row), thesis_no),
      year = suppressWarnings(parse_integer(as.character(`Yıl`)))
    ) |>
    filter(!is.na(year)) |>
    distinct(thesis_id, .keep_all = TRUE)
}

capture_to_file <- function(file, ...) {
  txt <- capture.output(...)
  writeLines(txt, file)
}

# Data preparation -----------------------------------------------------------
# Spinoza.xlsx is treated as the master analysis universe. Therefore, if the
# master workbook is the cleaned dataset with foreign-university theses removed,
# those theses cannot re-enter the analysis through TPT_ve_PT.xlsx.
all_theses <- read_theses(all_file)
politics_theses_raw <- read_theses(politics_file)

if (!nrow(all_theses) || !nrow(politics_theses_raw)) {
  stop("One or both workbooks contain no valid thesis records.", call. = FALSE)
}

# Keep only politics theses that are present in the cleaned master dataset.
# This also protects the analysis if TPT_ve_PT.xlsx still contains a thesis
# removed from the new Spinoza.xlsx dataset.
excluded_politics <- politics_theses_raw |>
  filter(!thesis_id %in% all_theses$thesis_id)

politics_theses <- politics_theses_raw |>
  filter(thesis_id %in% all_theses$thesis_id)

if (nrow(excluded_politics) > 0L) {
  write_csv(
    excluded_politics,
    file.path(root_output_dir, "politics_theses_excluded_not_in_cleaned_spinoza.csv")
  )
  message(
    nrow(excluded_politics),
    " politics thesis/theses excluded because their thesis IDs are not present ",
    "in the cleaned Spinoza.xlsx master dataset."
  )
}

if (!nrow(politics_theses)) {
  stop(
    "No politics theses remain after restricting TPT_ve_PT.xlsx to the cleaned ",
    "Spinoza.xlsx master dataset.",
    call. = FALSE
  )
}

cat("Master dataset: ", normalizePath(all_file), "\n", sep = "")
cat("Politics dataset: ", normalizePath(politics_file), "\n", sep = "")
cat("Valid theses in cleaned master dataset: ", nrow(all_theses), "\n", sep = "")
cat("Politics theses before master-dataset restriction: ",
    nrow(politics_theses_raw), "\n", sep = "")
cat("Politics theses retained for analysis: ", nrow(politics_theses), "\n", sep = "")
cat("Politics theses excluded as absent from cleaned master: ",
    nrow(excluded_politics), "\n", sep = "")

year_range <- seq.int(
  min(c(all_theses$year, politics_theses$year)),
  max(c(all_theses$year, politics_theses$year))
)

# MLModern Desktop Sans setup -------------------------------------------------
font_dir <- "/home/zboraon/Apps/fonts/mlmodern/MLModern-Desktop-OTF-1.0/fonts"

mlm_fonts <- c(
  regular    = file.path(font_dir, "MLModernDesktopSans-Regular.otf"),
  bold       = file.path(font_dir, "MLModernDesktopSans-Bold.otf"),
  italic     = file.path(font_dir, "MLModernDesktopSans-Oblique.otf"),
  bolditalic = file.path(font_dir, "MLModernDesktopSans-BoldOblique.otf")
)

missing_fonts <- mlm_fonts[!file.exists(mlm_fonts)]
if (length(missing_fonts) > 0L) {
  stop(
    "MLModern Sans font file(s) not found:\n",
    paste(missing_fonts, collapse = "\n"),
    call. = FALSE
  )
}

plot_font <- "MLModern Sans"
sysfonts::font_add(
  family = plot_font,
  regular = mlm_fonts[["regular"]],
  bold = mlm_fonts[["bold"]],
  italic = mlm_fonts[["italic"]],
  bolditalic = mlm_fonts[["bolditalic"]]
)
showtext::showtext_auto(TRUE)
showtext::showtext_opts(dpi = 600)

# Make bayesplot diagnostics grayscale as well.
bayesplot::color_scheme_set("gray")

base_theme <- theme_minimal(base_family = plot_font, base_size = 11) +
  theme(
    text = element_text(family = plot_font, colour = "black"),
    plot.title = element_text(family = plot_font, face = "bold", size = 14),
    plot.subtitle = element_text(family = plot_font, colour = "black"),
    plot.caption = element_text(family = plot_font, colour = "grey25"),
    axis.title = element_text(family = plot_font, face = "bold", colour = "black"),
    axis.text = element_text(family = plot_font, colour = "black"),
    legend.title = element_text(family = plot_font, colour = "black"),
    legend.text = element_text(family = plot_font, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

run_intervention_analysis <- function(INTERVENTION_YEAR) {
output_dir <- file.path(
  root_output_dir,
  paste0("intervention_", INTERVENTION_YEAR)
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
run_seed <- SEED + INTERVENTION_YEAR
pretrend_label <- paste0(INTERVENTION_YEAR, " öncesi eğilimin devamı")

cat("\n============================================================\n")
cat("Running Bayesian ITS with intervention year:", INTERVENTION_YEAR, "\n")
cat("Output folder:", output_dir, "\n")
cat("============================================================\n\n")

annual <- tibble(year = year_range) |>
  left_join(all_theses |> count(year, name = "total"), by = "year") |>
  left_join(politics_theses |> count(year, name = "politics"), by = "year") |>
  mutate(
    across(c(total, politics), ~replace_na(.x, 0L)),
    time10 = (year - INTERVENTION_YEAR) / 10,
    post = as.integer(year >= INTERVENTION_YEAR),
    time_after10 = pmax(0, year - INTERVENTION_YEAR) / 10,
    proportion = if_else(total > 0, politics / total, NA_real_)
  )

bad_years <- annual |> filter(politics > total)
if (nrow(bad_years)) {
  stop(
    "For these years politics exceeds all Spinoza theses: ",
    paste(bad_years$year, collapse = ", "),
    ". Reconcile the two workbooks before fitting models.",
    call. = FALSE
  )
}

write_csv(annual, file.path(output_dir, "spinoza_intervention_annual_data.csv"))

# Years with no Spinoza theses contain no binomial information and are omitted
# from the share model.
binom_data <- annual |> filter(total > 0)

# Bayesian interrupted time-series models ------------------------------------
count_priors <- c(
  brms::prior(student_t(3, 0, 2.5), class = "Intercept"),
  brms::prior(normal(0, 0.5), class = "b", coef = "time10"),
  brms::prior(normal(0, 1), class = "b", coef = "post"),
  brms::prior(normal(0, 0.5), class = "b", coef = "time_after10"),
  brms::prior(exponential(1), class = "shape")
)

bayes_count <- brms::brm(
  politics ~ time10 + post + time_after10,
  data = annual,
  family = brms::negbinomial(link = "log"),
  prior = count_priors,
  chains = CHAINS, iter = ITER, cores = CORES, seed = run_seed,
  control = list(adapt_delta = 0.97, max_treedepth = 12),
  file = file.path(output_dir, "bayes_spinoza_count_its"),
  file_refit = "on_change",
  refresh = 200
)

share_priors <- c(
  brms::prior(student_t(3, 0, 2.5), class = "Intercept"),
  brms::prior(normal(0, 0.5), class = "b", coef = "time10"),
  brms::prior(normal(0, 1), class = "b", coef = "post"),
  brms::prior(normal(0, 0.5), class = "b", coef = "time_after10")
)

bayes_share <- brms::brm(
  politics | trials(total) ~ time10 + post + time_after10,
  data = binom_data,
  family = stats::binomial(link = "logit"),
  prior = share_priors,
  chains = CHAINS, iter = ITER, cores = CORES, seed = run_seed,
  control = list(adapt_delta = 0.97, max_treedepth = 12),
  file = file.path(output_dir, "bayes_spinoza_share_its"),
  file_refit = "on_change",
  refresh = 200
)

bayes_effect_table <- function(fit, model_name, effect_scale) {
  fx <- as.data.frame(brms::fixef(fit, probs = c(0.025, 0.975)))
  tibble(
    model = model_name,
    term = rownames(fx),
    estimate = exp(fx$Estimate),
    lower_95 = exp(fx$Q2.5),
    upper_95 = exp(fx$Q97.5),
    effect_scale = effect_scale
  )
}

bayes_effects <- bind_rows(
  bayes_effect_table(bayes_count, "Bayesian negative-binomial ITS", "Rate ratio"),
  bayes_effect_table(bayes_share, "Bayesian binomial ITS", "Odds ratio")
)
write_csv(bayes_effects, file.path(output_dir, "spinoza_bayesian_effects.csv"))

# Fitted and counterfactual trajectories --------------------------------------
# posterior_epred returns expected counts for both models. For the binomial
# model, divide each posterior draw by the annual number of trials to obtain
# the expected proportion.
summarise_draw_matrix <- function(draws) {
  tibble(
    estimate = apply(draws, 2, mean),
    lower = apply(draws, 2, quantile, probs = 0.025),
    upper = apply(draws, 2, quantile, probs = 0.975)
  )
}

bayes_count_draws <- brms::posterior_epred(bayes_count, newdata = annual)
bayes_count_cf_draws <- brms::posterior_epred(
  bayes_count,
  newdata = annual |> mutate(post = 0L, time_after10 = 0)
)
bayes_count_plot_data <- bind_cols(
  annual |> select(year, politics),
  summarise_draw_matrix(bayes_count_draws) |>
    rename(fitted = estimate, fitted_lower = lower, fitted_upper = upper),
  summarise_draw_matrix(bayes_count_cf_draws) |>
    rename(counterfactual = estimate, cf_lower = lower, cf_upper = upper)
)

bayes_share_count_draws <- brms::posterior_epred(
  bayes_share,
  newdata = binom_data
)
bayes_share_cf_count_draws <- brms::posterior_epred(
  bayes_share,
  newdata = binom_data |> mutate(post = 0L, time_after10 = 0)
)
bayes_share_draws <- sweep(
  bayes_share_count_draws, 2, binom_data$total, FUN = "/"
)
bayes_share_cf_draws <- sweep(
  bayes_share_cf_count_draws, 2, binom_data$total, FUN = "/"
)
bayes_share_plot_data <- bind_cols(
  binom_data |> select(year, politics, total, proportion),
  summarise_draw_matrix(bayes_share_draws) |>
    rename(fitted = estimate, fitted_lower = lower, fitted_upper = upper),
  summarise_draw_matrix(bayes_share_cf_draws) |>
    rename(counterfactual = estimate, cf_lower = lower, cf_upper = upper)
)

write_csv(
  bayes_count_plot_data,
  file.path(output_dir, "spinoza_bayesian_count_predictions.csv")
)
write_csv(
  bayes_share_plot_data,
  file.path(output_dir, "spinoza_bayesian_share_predictions.csv")
)

p_bayes_count <- ggplot(bayes_count_plot_data, aes(year)) +
  geom_col(
    aes(y = politics),
    width = 0.8, fill = "grey82", colour = "black", linewidth = 0.20
  ) +
  geom_ribbon(
    aes(ymin = fitted_lower, ymax = fitted_upper),
    fill = "grey65", alpha = 0.45
  ) +
  geom_line(
    aes(y = fitted, linetype = "Bayes modeli"),
    colour = "black", linewidth = 0.95
  ) +
  geom_line(
    aes(y = counterfactual, linetype = pretrend_label),
    colour = "black", linewidth = 0.85
  ) +
  geom_vline(
    xintercept = INTERVENTION_YEAR,
    colour = "black", linewidth = 0.65, linetype = "dotted"
  ) +
  scale_linetype_manual(
    values = c(
      "Bayes modeli" = "solid",
      setNames("22", pretrend_label)
    ),
    name = NULL
  ) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA)) +
  labs(
    title = "‘Siyaset’ etiketli tezler",
    subtitle = "Negatif binom model; gri alan %95 güvenilirlik aralığıdır",
    x = "Yıl", y = "Tez sayısı",
    caption = paste0(
      "Noktalı dikey çizgi müdahale yılını; kesikli eğri ise ", INTERVENTION_YEAR,
      " öncesi eğilimin değişmeden sürdüğü karşıolgusal senaryoyu gösterir."
    )
  ) +
  base_theme

p_bayes_share <- ggplot(bayes_share_plot_data, aes(year)) +
  geom_ribbon(
    aes(ymin = fitted_lower, ymax = fitted_upper),
    fill = "grey65", alpha = 0.45
  ) +
  geom_line(
    aes(y = fitted, linetype = "Bayes modeli"),
    colour = "black", linewidth = 0.95
  ) +
  geom_line(
    aes(y = counterfactual, linetype = pretrend_label),
    colour = "black", linewidth = 0.85
  ) +
  geom_vline(
    xintercept = INTERVENTION_YEAR,
    colour = "black", linewidth = 0.65, linetype = "dotted"
  ) +
  geom_point(
    aes(y = proportion, size = total),
    shape = 21, fill = "grey70", colour = "black",
    stroke = 0.30, alpha = 0.90
  ) +
  scale_linetype_manual(
    values = c(
      "Bayes modeli" = "solid",
      setNames("22", pretrend_label)
    ),
    name = NULL
  ) +
  scale_size_continuous(
    name = "Tüm Spinoza tezleri", range = c(1.5, 5)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), limits = c(0, 1)
  ) +
  guides(
    size = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  labs(
    title = "Spinoza tezlerinde ‘siyaset’ etiketinin payı",
    subtitle = "Bayes binom modeli; gri alan %95 güvenilirlik aralığıdır",
    x = "Yıl", y = "Siyaset etiketli tezlerin oranı",
    caption = paste0(
      "Nokta büyüklüğü ilgili yıldaki toplam Spinoza tezi sayısını gösterir. ",
      "Noktalı dikey çizgi müdahale yılını; kesikli eğri ise ", INTERVENTION_YEAR,
      " öncesi eğilimin değişmeden sürdüğü karşıolgusal senaryoyu gösterir."
    )
  ) +
  base_theme

ggsave(
  file.path(output_dir, "spinoza_bayesian_its_raw_counts.png"),
  p_bayes_count, width = 10.5, height = 6.6, dpi = 600, bg = "white"
)
ggsave(
  file.path(output_dir, "spinoza_bayesian_its_raw_counts.pdf"),
  p_bayes_count, width = 10.5, height = 6.6,
  device = cairo_pdf, bg = "white"
)
ggsave(
  file.path(output_dir, "spinoza_bayesian_its_politics_share.png"),
  p_bayes_share, width = 10.5, height = 6.6, dpi = 600, bg = "white"
)
ggsave(
  file.path(output_dir, "spinoza_bayesian_its_politics_share.pdf"),
  p_bayes_share, width = 10.5, height = 6.6,
  device = cairo_pdf, bg = "white"
)

capture_to_file(
  file.path(output_dir, "spinoza_bayesian_results.txt"),
  cat("BAYESIAN NEGATIVE-BINOMIAL ITS\n"), print(summary(bayes_count)),
  cat("\nPosterior probability of a positive immediate change:\n"),
  print(brms::hypothesis(bayes_count, "post > 0")),
  cat("\nPosterior probability of a positive slope change:\n"),
  print(brms::hypothesis(bayes_count, "time_after10 > 0")),
  cat("\nBAYESIAN BINOMIAL ITS\n"), print(summary(bayes_share)),
  cat("\nPosterior probability of a positive immediate change:\n"),
  print(brms::hypothesis(bayes_share, "post > 0")),
  cat("\nPosterior probability of a positive slope change:\n"),
  print(brms::hypothesis(bayes_share, "time_after10 > 0"))
)

pp_count <- brms::pp_check(bayes_count) +
  ggtitle("Posterior predictive check: thesis counts") + base_theme
pp_share <- brms::pp_check(bayes_share) +
  ggtitle("Posterior predictive check: politics share") + base_theme
ggsave(file.path(output_dir, "spinoza_bayesian_ppcheck_counts.png"),
       pp_count, width = 8, height = 5.5, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "spinoza_bayesian_ppcheck_share.png"),
       pp_share, width = 8, height = 5.5, dpi = 600, bg = "white")

# Convergence diagnostics -----------------------------------------------------
# For each model: Rhat / ESS table, divergence & treedepth counts, an E-BFMI
# check, and trace/rank plots for the structural (fixed-effect) parameters.
run_convergence_diagnostics <- function(fit, model_name, file_prefix) {
  
  pars <- brms::variables(fit)
  fixed_pars <- pars[grepl("^b_|^shape$", pars)]
  
  # 1. Rhat / bulk-ESS / tail-ESS summary table -------------------------------
  diag_summary <- posterior::summarise_draws(
    fit, "rhat", "ess_bulk", "ess_tail"
  ) |>
    filter(variable %in% pars) |>
    arrange(desc(rhat))
  
  write_csv(
    diag_summary,
    file.path(output_dir, paste0(file_prefix, "_convergence_summary.csv"))
  )
  
  flag_rhat <- diag_summary |> filter(rhat > 1.01)
  flag_ess <- diag_summary |> filter(ess_bulk < 400 | ess_tail < 400)
  
  # 2. Sampler diagnostics: divergences and treedepth saturation --------------
  nuts <- brms::nuts_params(fit)
  n_divergent <- sum(nuts$Value[nuts$Parameter == "divergent__"])
  td_limit <- tryCatch(
    fit$fit@stan_args[[1]]$control$max_treedepth,
    error = function(e) NULL
  )
  n_max_treedepth <- if (!is.null(td_limit)) {
    sum(nuts$Value[nuts$Parameter == "treedepth__"] >= td_limit)
  } else {
    NA_integer_
  }
  
  capture_to_file(
    file.path(output_dir, paste0(file_prefix, "_convergence_report.txt")),
    cat("CONVERGENCE DIAGNOSTICS:", model_name, "\n"),
    cat("=====================================\n\n"),
    cat("Rhat / ESS (fixed effects + shape/aux params)\n"),
    print(as.data.frame(diag_summary)),
    cat("\nParameters with Rhat > 1.01:", nrow(flag_rhat), "\n"),
    if (nrow(flag_rhat)) print(as.data.frame(flag_rhat)),
    cat("\nParameters with bulk- or tail-ESS < 400:", nrow(flag_ess), "\n"),
    if (nrow(flag_ess)) print(as.data.frame(flag_ess)),
    cat("\nDivergent transitions (post-warmup):", n_divergent, "\n"),
    cat("Iterations that hit max treedepth:", n_max_treedepth, "\n"),
    cat("\nInterpretation:\n"),
    cat("  Rhat should be <= 1.01 for every parameter.\n"),
    cat("  Bulk- and tail-ESS should each be >= ~400 (400 per chain x", CHAINS, "chains is a common rule of thumb).\n"),
    cat("  Divergent transitions should be 0; if not, increase adapt_delta and re-fit.\n"),
    cat("  Treedepth saturation suggests raising max_treedepth.\n")
  )
  
  # 3. Diagnostic plots ---------------------------------------------------
  post_array <- as.array(fit, variable = fixed_pars)
  
  p_trace <- bayesplot::mcmc_trace(post_array) +
    ggtitle(paste("Trace plots:", model_name)) + base_theme
  ggsave(
    file.path(output_dir, paste0(file_prefix, "_trace.png")),
    p_trace, width = 10, height = 6.5, dpi = 600, bg = "white"
  )
  
  p_rank <- bayesplot::mcmc_rank_overlay(post_array) +
    ggtitle(paste("Rank plots:", model_name)) + base_theme
  ggsave(
    file.path(output_dir, paste0(file_prefix, "_rank.png")),
    p_rank, width = 10, height = 6.5, dpi = 600, bg = "white"
  )
  
  rhat_vec <- setNames(diag_summary$rhat, diag_summary$variable)
  p_rhat <- bayesplot::mcmc_rhat(rhat_vec) +
    ggtitle(paste("Rhat by parameter:", model_name)) + base_theme
  ggsave(
    file.path(output_dir, paste0(file_prefix, "_rhat.png")),
    p_rhat, width = 8, height = 5.5, dpi = 600, bg = "white"
  )
  
  p_energy <- bayesplot::mcmc_nuts_energy(nuts) +
    ggtitle(paste("NUTS energy (E-BFMI) diagnostic:", model_name)) + base_theme
  ggsave(
    file.path(output_dir, paste0(file_prefix, "_energy.png")),
    p_energy, width = 8, height = 5.5, dpi = 600, bg = "white"
  )
  
  invisible(list(
    summary = diag_summary,
    n_divergent = n_divergent,
    n_max_treedepth = n_max_treedepth
  ))
}

diag_count <- run_convergence_diagnostics(
  bayes_count, "Negative-binomial count ITS", "spinoza_bayes_count"
)
diag_share <- run_convergence_diagnostics(
  bayes_share, "Binomial share ITS", "spinoza_bayes_share"
)

cat("\nAnalysis complete.\n")
cat("Intervention:", INTERVENTION_YEAR, "and later.\n")
cat("Bayesian results, predictions, figures, convergence diagnostics, and annual data were saved to:\n")
cat(output_dir, "\n")
cat("\nConvergence summary:\n")
cat("  Count model  - divergences:", diag_count$n_divergent,
    "| max-treedepth hits:", diag_count$n_max_treedepth, "\n")
cat("  Share model  - divergences:", diag_share$n_divergent,
    "| max-treedepth hits:", diag_share$n_max_treedepth, "\n")

invisible(list(
  intervention_year = INTERVENTION_YEAR,
  output_dir = output_dir,
  effects = bayes_effects |>
    mutate(intervention_year = INTERVENTION_YEAR, .before = 1),
  count_diagnostics = diag_count,
  share_diagnostics = diag_share
))
}

analysis_runs <- lapply(
  INTERVENTION_YEARS,
  function(year) {
    result <- run_intervention_analysis(year)
    gc()
    result
  }
)

all_intervention_effects <- bind_rows(
  lapply(analysis_runs, `[[`, "effects")
)
write_csv(
  all_intervention_effects,
  file.path(root_output_dir, "spinoza_bayesian_effects_all_interventions.csv")
)
write_csv(
  all_intervention_effects |>
    filter(term %in% c("post", "time_after10")) |>
    arrange(model, term, intervention_year),
  file.path(root_output_dir, "spinoza_bayesian_intervention_comparison.csv")
)

cat("\nAll intervention-year analyses completed.\n")
cat("Root output folder:\n", root_output_dir, "\n", sep = "")
cat("Created subfolders:\n")
cat(paste0("  intervention_", INTERVENTION_YEARS, "\n"), sep = "")

