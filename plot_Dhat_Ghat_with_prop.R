
# Plot D-hat vs g-hat with original proportion (pi) as reference
# Output: Dhat_Ghat.pdf in the Overleaf figures directory

library(tidyverse)
library(ggrepel)
source("config.R")

dta_des <- read.csv(DATASET_DESC_FILE)
file_nm <- dta_des[["dta_name"]]

# Collect D_hat, g_hat from gcm_log
dg_data <- data.frame()
for (i in seq_along(file_nm)){
  for (seed in SEED_RANGE){
    dir_name <- file.path(RESULTS_DIR, file_nm[i], paste0("seed=", seed), paste0("ntree=100"))
    log_file <- file.path(dir_name, "gcm_log.rds")
    if (file.exists(log_file)){
      log <- readRDS(log_file)
      H <- log$H; FA <- log$FA
      D_hat <- H - FA
      g_hat <- if (D_hat < 1) FA / (1 - D_hat) else NA
      dg_data <- rbind(dg_data,
        data.frame(file_name = file_nm[i], seed = seed,
                   D_hat = D_hat, g_hat = g_hat))
    }
  }
}

# Summarise per dataset
dg_summary <- dg_data |>
  group_by(file_name) |>
  summarise(D_hat = mean(D_hat, na.rm = TRUE),
            g_hat = mean(g_hat, na.rm = TRUE), .groups = "drop") |>
  left_join(dta_des, by = c("file_name" = "dta_name"))

# Use separate layers for labeling
dg_ghat <- dg_summary |> select(file_name, D_hat, g_hat)
dg_prop <- dg_summary |> select(file_name, D_hat, prop.1.)

p <- ggplot() +
  # g_hat points (black circles)
  geom_point(data = dg_ghat, aes(x = D_hat, y = g_hat), size = 3, shape = 16) +
  # prop points (red triangles)
  geom_point(data = dg_prop, aes(x = D_hat, y = prop.1.), size = 3, shape = 17, color = "red") +
  # connecting segments
  geom_segment(data = dg_summary,
               aes(x = D_hat, xend = D_hat, y = g_hat, yend = prop.1.),
               linetype = "dotted", color = "grey50", linewidth = 0.4) +
  # labels: full-size font, separated purely by position (ggrepel leader lines)
  geom_text_repel(data = dg_ghat, aes(x = D_hat, y = g_hat, label = file_name),
                  size = 4, max.overlaps = Inf, box.padding = 0.9,
                  point.padding = 0.6, min.segment.length = 0,
                  force = 8, force_pull = 0.2, max.time = 2, max.iter = 100000,
                  segment.size = 0.2, segment.color = "grey60", seed = 42) +
  labs(x = expression(hat(D)), y = " ") +
  ggtitle(" ") +
  # manual legend
  annotate("point", x = 0.05, y = max(dg_summary$g_hat, dg_summary$prop.1.) * 0.95,
           size = 3, shape = 16) +
  annotate("text", x = 0.09, y = max(dg_summary$g_hat, dg_summary$prop.1.) * 0.95,
           label = expression(hat(g)), size = 4, hjust = 0) +
  annotate("point", x = 0.05, y = max(dg_summary$g_hat, dg_summary$prop.1.) * 0.88,
           size = 3, shape = 17, color = "red") +
  annotate("text", x = 0.09, y = max(dg_summary$g_hat, dg_summary$prop.1.) * 0.88,
           label = expression(pi), size = 4, hjust = 0) +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.18)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.margin = margin(10, 16, 10, 10))

out_dir <- file.path(
  Sys.getenv("HOME"),
  "Library", "CloudStorage", "Dropbox",
  "\u61c9\u7528\u7a0b\u5f0f", "Overleaf", "RF-CCT"
)
ggsave(file.path(out_dir, "Dhat_Ghat.pdf"),
       plot = p, width = 8, height = 5.5, bg = "white", dpi = 300)

cat("Saved to:", file.path(out_dir, "Dhat_Ghat.pdf"), "\n")
