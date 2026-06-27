
# plot_figures_4_5_8.R
# Generates Figures 4, 5, and 8 for the paper.
# Run from the RF-GCM directory: Rscript plot_figures_4_5_8.R

source("config.R")
source(file.path("method_code", "RF_related.R"))

library(randomForest)
library(tidyverse)
library(patchwork)

dta_des <- read.csv(DATASET_DESC_FILE)

OVERLEAF_DIR <- file.path(
  Sys.getenv("HOME"),
  "Library", "CloudStorage", "Dropbox",
  "\u61c9\u7528\u7a0b\u5f0f", "Overleaf", "RF-CCT"
)

SEED_FIG <- 120
NTREE_FIG <- 100

# ── Helper: Compute reliability data ──────────────────────────
make_reliability_data <- function(predicted_prob, true_labels, n_bins = 10) {
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bin_idx <- cut(predicted_prob, breaks = breaks,
                 include.lowest = TRUE, labels = FALSE)
  result <- data.frame(
    bin_mid  = numeric(n_bins),
    emp_prob = numeric(n_bins),
    count    = integer(n_bins)
  )
  for (b in seq_len(n_bins)) {
    in_bin <- which(bin_idx == b)
    result$bin_mid[b]  <- (breaks[b] + breaks[b + 1]) / 2
    result$count[b]    <- length(in_bin)
    result$emp_prob[b] <- if (length(in_bin) > 0) mean(true_labels[in_bin]) else NA
  }
  result
}

# ── Helper: Single reliability panel ──────────────────────────
plot_reliability <- function(pred_prob, true_labels, prop, panel_title,
                             n_total, n_bins = 10) {
  rel <- make_reliability_data(pred_prob, true_labels, n_bins)
  hist_df <- data.frame(prob = pred_prob)

  # Normalize histogram to [0, 1]
  breaks_h <- seq(0, 1, length.out = n_bins + 1)
  counts_h <- hist(pred_prob, breaks = breaks_h, plot = FALSE)$counts
  max_count <- max(counts_h, 1)

  hist_bar <- data.frame(
    xmin = breaks_h[-length(breaks_h)],
    xmax = breaks_h[-1],
    ymax = counts_h / max_count
  )

  rel_valid <- rel[!is.na(rel$emp_prob), ]

  ggplot() +
    geom_rect(data = hist_bar,
              aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax),
              fill = "grey80", color = "grey60", alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey50") +
    geom_line(data = rel_valid,
              aes(x = bin_mid, y = emp_prob), linewidth = 0.7) +
    geom_point(data = rel_valid,
               aes(x = bin_mid, y = emp_prob), size = 2) +
    annotate("point", x = prop, y = prop,
             color = "blue", size = 4.5) +
    labs(title = paste0(panel_title, "\nn: ", n_total,
                        " ; prop: ", round(prop, 2)),
         x = expression(hat(p)(y == 1 ~ "|" ~ x)),
         y = "Empirical probability") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          plot.title = element_text(size = 15, hjust = 0.5))
}

# ── Helper: Re-fit RF to get OOB probabilities ────────────────
get_oob_probs <- function(dataset_name, target_name, seed, ntree) {
  dta <- readRDS(file.path(DATASET_DIR, paste0(dataset_name, ".rds")))
  split <- train_test_split_stratify(dta, TRAIN_PROP, target_name, seed)
  train_data <- split[[1]]

  formula <- as.formula(paste0(target_name, "~ ."))
  set.seed(seed)
  rf <- randomForest(formula, data = train_data, ntree = ntree,
                     prox = TRUE, norm.votes = FALSE, keep.inbag = TRUE)

  votes <- rf[["votes"]]
  oob_prob <- votes[, 2] / (votes[, 1] + votes[, 2])
  oob_prob[is.nan(oob_prob)] <- NA
  oob_true <- as.numeric(as.character(train_data[[target_name]]))

  valid <- !is.na(oob_prob)
  list(prob = oob_prob[valid], true = oob_true[valid])
}

# ══════════════════════════════════════════════════════════════
# FIGURE 4: Reliability plots for 3 prototypical datasets
# ══════════════════════════════════════════════════════════════
generate_figure_4 <- function() {
  cat("Generating Figure 4...\n")
  datasets <- c("breast_cancer", "satimage", "yeast5")

  plots_test <- list()
  plots_oob  <- list()

  for (d in datasets) {
    info <- dta_des[dta_des$dta_name == d, ]
    target_nm <- info$target_name
    prop <- info$prop.1.
    n_total <- info$instances

    # Test set
    dir_name <- file.path(RESULTS_DIR, d,
                          paste0("seed=", SEED_FIG),
                          paste0("ntree=", NTREE_FIG))
    pred_prob <- readRDS(file.path(dir_name, "prediction_prob.rds"))
    pred      <- readRDS(file.path(dir_name, "prediction.rds"))
    true_lab  <- as.numeric(as.character(pred$true))
    rf_prob   <- pred_prob$RF

    plots_test[[d]] <- plot_reliability(
      rf_prob, true_lab, prop, d, n_total
    )

    # OOB
    oob <- get_oob_probs(d, target_nm, SEED_FIG, NTREE_FIG)
    plots_oob[[d]] <- plot_reliability(
      oob$prob, oob$true, prop, paste0(d, " (OOB)"), n_total
    )
  }

  p_test <- plots_test[[1]] | plots_test[[2]] | plots_test[[3]]
  p_oob  <- plots_oob[[1]]  | plots_oob[[2]]  | plots_oob[[3]]

  ggsave(file.path(OVERLEAF_DIR, "reliability.pdf"),
         plot = p_test, width = 12, height = 4, bg = "white", dpi = 300)
  ggsave(file.path(OVERLEAF_DIR, "reliability_oob.pdf"),
         plot = p_oob, width = 12, height = 4, bg = "white", dpi = 300)

  cat("  Saved: reliability.pdf, reliability_oob.pdf\n")
}

# ══════════════════════════════════════════════════════════════
# FIGURE 5: oil_spill reliability under multiple methods
# ══════════════════════════════════════════════════════════════
generate_figure_5 <- function() {
  cat("Generating Figure 5...\n")
  d <- "oil_spill"
  info <- dta_des[dta_des$dta_name == d, ]
  target_nm <- info$target_name
  prop <- info$prop.1.
  n_total <- info$instances

  dir_name <- file.path(RESULTS_DIR, d,
                        paste0("seed=", SEED_FIG),
                        paste0("ntree=", NTREE_FIG))
  pred_prob <- readRDS(file.path(dir_name, "prediction_prob.rds"))
  pred      <- readRDS(file.path(dir_name, "prediction.rds"))
  true_lab  <- as.numeric(as.character(pred$true))

  # RF test
  p_rf_test <- plot_reliability(
    pred_prob$RF, true_lab, prop, paste0(d), n_total
  )

  # RF OOB
  oob <- get_oob_probs(d, target_nm, SEED_FIG, NTREE_FIG)
  p_rf_oob <- plot_reliability(
    oob$prob, oob$true, prop, paste0(d, " (OOB)"), n_total
  )

  # SMOTE, BRF
  p_smote <- plot_reliability(
    pred_prob$SMOTE, true_lab, prop, paste0(d, " (SMOTE)"), n_total
  )
  p_brf <- plot_reliability(
    pred_prob$BRF, true_lab, prop, paste0(d, " (BRF)"), n_total
  )

  combined <- (p_rf_test | p_rf_oob) /
              (p_smote | p_brf)

  ggsave(file.path(OVERLEAF_DIR, "oil_reliability.pdf"),
         plot = combined, width = 10, height = 8, bg = "white", dpi = 300)

  cat("  Saved: oil_reliability.pdf\n")
}

# ══════════════════════════════════════════════════════════════
# FIGURE 8: yeast5 G-mean & F1 vs number of trees
# ══════════════════════════════════════════════════════════════
generate_figure_8 <- function() {
  cat("Generating Figure 8...\n")
  d <- "yeast5"
  fig8_methods <- c("lamda_prior", "BRF", "GCM",
                    "SMOTE", "CSRF", "EasyEnsemble", "RF")

  # Collect per-seed performance
  perf_data <- data.frame()
  for (seed in SEED_RANGE) {
    for (tree in TREE_SIZES) {
      pred_file <- file.path(RESULTS_DIR, d,
                             paste0("seed=", seed),
                             paste0("ntree=", tree),
                             "prediction.rds")
      if (!file.exists(pred_file)) next
      pred <- readRDS(pred_file)
      true_lab <- as.numeric(as.character(pred$true))

      for (method in fig8_methods) {
        result <- pred[[method]]
        if (is.null(result) || all(is.na(result))) next
        result_num <- as.numeric(as.character(result))

        TP <- sum(result_num == 1 & true_lab == 1)
        TN <- sum(result_num == 0 & true_lab == 0)
        FP <- sum(result_num == 1 & true_lab == 0)
        FN <- sum(result_num == 0 & true_lab == 1)

        sens <- if ((TP + FN) > 0) TP / (TP + FN) else 0
        spec <- if ((TN + FP) > 0) TN / (TN + FP) else 0
        gmean <- sqrt(sens * spec)

        prec <- if ((TP + FP) > 0) TP / (TP + FP) else 0
        f1 <- if ((prec + sens) > 0) 2 * prec * sens / (prec + sens) else 0

        perf_data <- rbind(perf_data,
          data.frame(seed = seed, tree = tree, method = method,
                     Gmean = gmean, F1 = f1))
      }
    }
  }

  # Summarise
  perf_summary <- perf_data |>
    group_by(method, tree) |>
    summarise(avg_Gmean = mean(Gmean, na.rm = TRUE),
              avg_F1 = mean(F1, na.rm = TRUE),
              .groups = "drop")

  # Aesthetics
  method_order <- c("RF", "GCM", "lamda_prior", "BRF",
                    "SMOTE", "CSRF", "EasyEnsemble")
  method_labels <- c(
    "RF" = "RF", "GCM" = "GCM",
    "lamda_prior" = expression(lambda[prior]),
    "BRF" = "BRF", "SMOTE" = "SMOTE",
    "CSRF" = "CSRF", "EasyEnsemble" = "EasyEnsemble"
  )
  method_colors <- c(
    "RF" = "grey50",
    "GCM" = "black",
    "lamda_prior" = "#E41A1C",
    "BRF" = "#377EB8",
    "SMOTE" = "#4DAF4A",
    "CSRF" = "#FF7F00",
    "EasyEnsemble" = "#984EA3"
  )
  method_linetypes <- c(
    "RF" = "dotted",
    "GCM" = "solid",
    "lamda_prior" = "dashed",
    "BRF" = "dotdash",
    "SMOTE" = "longdash",
    "CSRF" = "twodash",
    "EasyEnsemble" = "solid"
  )
  method_shapes <- c(
    "RF" = 4, "GCM" = 16, "lamda_prior" = 17,
    "BRF" = 1, "SMOTE" = 2, "CSRF" = 3, "EasyEnsemble" = 5
  )

  perf_summary$method <- factor(perf_summary$method, levels = method_order)

  info <- dta_des[dta_des$dta_name == d, ]

  # G-mean panel
  p_gm <- ggplot(perf_summary,
                 aes(x = tree, y = avg_Gmean,
                     color = method, linetype = method, shape = method)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = TREE_SIZES) +
    scale_color_manual(values = method_colors, labels = method_labels) +
    scale_linetype_manual(values = method_linetypes, labels = method_labels) +
    scale_shape_manual(values = method_shapes, labels = method_labels) +
    labs(title = paste0(d, "\n(Minority class proportion: ",
                        round(info$prop.1., 2), ")"),
         x = "Number of trees", y = "G-mean") +
    theme_minimal() +
    theme(text = element_text(size = 16),
          plot.title = element_text(size = 15, hjust = 0.5),
          axis.text = element_text(size = 13),
          legend.title = element_blank(),
          legend.position = "right")

  # F1 panel
  p_f1 <- ggplot(perf_summary,
                 aes(x = tree, y = avg_F1,
                     color = method, linetype = method, shape = method)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = TREE_SIZES) +
    scale_color_manual(values = method_colors, labels = method_labels) +
    scale_linetype_manual(values = method_linetypes, labels = method_labels) +
    scale_shape_manual(values = method_shapes, labels = method_labels) +
    labs(title = paste0(d, "\n(Minority class proportion: ",
                        round(info$prop.1., 2), ")"),
         x = "Number of trees", y = "F1 score") +
    theme_minimal() +
    theme(text = element_text(size = 16),
          plot.title = element_text(size = 15, hjust = 0.5),
          axis.text = element_text(size = 13),
          legend.title = element_blank(),
          legend.position = "right")

  combined <- p_gm + p_f1 + plot_layout(guides = "collect") &
    theme(legend.position = "right")

  ggsave(file.path(OVERLEAF_DIR, "yeast5.pdf"),
         plot = combined, width = 12, height = 5, bg = "white", dpi = 300)

  cat("  Saved: yeast5.pdf\n")

  # ── Statistical tests at ntree=100 ──
  cat("\n=== Statistical Tests (ntree=100) ===\n")
  perf_100 <- perf_data |> filter(tree == 100)

  for (metric in c("Gmean", "F1")) {
    cat(paste0("\n--- ", metric, " ---\n"))

    wide <- perf_100 |>
      select(seed, method, !!sym(metric)) |>
      pivot_wider(names_from = method, values_from = !!sym(metric))

    mat <- as.matrix(wide[, -1])  # remove seed column

    # Friedman test
    ft <- friedman.test(mat)
    cat(sprintf("Friedman: chi^2(%d, N=%d) = %.2f, p %s\n",
                ft$parameter, nrow(mat), ft$statistic,
                ifelse(ft$p.value < 0.001, "< .001",
                       paste0("= ", round(ft$p.value, 3)))))

    # Mean ranks per method
    ranks_per_seed <- t(apply(mat, 1, function(x) rank(-x)))
    mean_ranks <- colMeans(ranks_per_seed)
    rank_order <- sort(mean_ranks)
    cat("Rank order (best first):\n")
    for (i in seq_along(rank_order)) {
      cat(sprintf("  %s: M = %.2f\n", names(rank_order)[i], rank_order[i]))
    }

    # Pairwise Wilcoxon (consecutive in rank order)
    ordered_methods <- names(rank_order)
    p_values <- numeric()
    pair_labels <- character()
    w_values <- numeric()

    for (i in seq_len(length(ordered_methods) - 1)) {
      m1 <- ordered_methods[i]
      m2 <- ordered_methods[i + 1]
      test <- wilcox.test(wide[[m1]], wide[[m2]], paired = TRUE)
      p_values <- c(p_values, test$p.value)
      w_values <- c(w_values, test$statistic)
      pair_labels <- c(pair_labels, paste0(m1, " vs ", m2))
    }
    p_adj <- p.adjust(p_values, method = "holm")

    cat("\nPairwise comparisons (consecutive rank order, Holm-corrected):\n")
    for (i in seq_along(pair_labels)) {
      m1 <- ordered_methods[i]
      m2 <- ordered_methods[i + 1]
      sig <- ifelse(p_adj[i] < 0.001, "***",
             ifelse(p_adj[i] < 0.01, "**",
             ifelse(p_adj[i] < 0.05, "*", "n.s.")))
      cat(sprintf("  %s (M=%.2f) vs %s (M=%.2f): W=%.0f, p_adj=%s %s\n",
                  m1, rank_order[i], m2, rank_order[i + 1],
                  w_values[i],
                  ifelse(p_adj[i] < 0.001, "<.001",
                         sprintf("%.3f", p_adj[i])),
                  sig))
    }
  }
}

# ── Run all ───────────────────────────────────────────────────
generate_figure_4()
generate_figure_5()
generate_figure_8()
cat("\nAll figures generated.\n")
