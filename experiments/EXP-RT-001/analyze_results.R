# EXP-RT-001 Phase 5: Statistical Analysis and Reporting
# Purpose: Analyze full factorial experiment results and generate recommendations
# Date: 2025-10-24
# Agent: Toby Esterhase

# Load required packages
library(ggplot2)
library(reshape2)

cat("==================================================\n")
cat("EXP-RT-001 Phase 5: Statistical Analysis\n")
cat("==================================================\n\n")

# Load results
results_file <- "artifacts/cluster_full_results.csv"

if (!file.exists(results_file)) {
  stop("Results file not found: ", results_file,
       "\nPlease retrieve artifacts from cluster first.")
}

results <- read.csv(results_file)

cat("Results loaded successfully\n")
cat("  File:", results_file, "\n")
cat("  Total observations:", nrow(results), "\n")
cat("  Date range:", min(results$date), "to", max(results$date), "\n\n")

# Data validation
cat("DATA VALIDATION\n")
cat("==================================================\n")
expected_rows <- 6 * 6 * 10  # 6 batch sizes × 6 cores × 10 reps
cat("  Expected rows:", expected_rows, "\n")
cat("  Actual rows:", nrow(results), "\n")
if (nrow(results) == expected_rows) {
  cat("  Status: ✓ Complete\n\n")
} else {
  cat("  Status: ⚠ Incomplete (", nrow(results), "/", expected_rows, ")\n\n")
}

# Convert factors
results$batch_size <- as.factor(results$batch_size)
results$cores <- as.factor(results$cores)

# Summary statistics
cat("DESCRIPTIVE STATISTICS\n")
cat("==================================================\n")
cat("Computation Time (seconds):\n")
cat("  Mean:", round(mean(results$elapsed_seconds), 2), "\n")
cat("  Median:", round(median(results$elapsed_seconds), 2), "\n")
cat("  SD:", round(sd(results$elapsed_seconds), 2), "\n")
cat("  Range:", round(min(results$elapsed_seconds), 2), "-",
    round(max(results$elapsed_seconds), 2), "\n\n")

# ANOVA: Main effects and interaction
cat("ANALYSIS OF VARIANCE\n")
cat("==================================================\n")

# Full factorial ANOVA
anova_model <- aov(elapsed_seconds ~ batch_size * cores, data = results)
cat("\nFull Factorial ANOVA:\n")
print(summary(anova_model))

# Effect sizes (eta-squared)
ss_total <- sum((results$elapsed_seconds - mean(results$elapsed_seconds))^2)
anova_table <- summary(anova_model)[[1]]
eta_sq <- anova_table$`Sum Sq` / ss_total

cat("\nEffect Sizes (η²):\n")
cat("  Batch size:", round(eta_sq[1], 3), "\n")
cat("  Cores:", round(eta_sq[2], 3), "\n")
cat("  Interaction:", round(eta_sq[3], 3), "\n\n")

# Marginal means
cat("\nMARGINAL MEANS\n")
cat("==================================================\n")

# By batch size
batch_means <- aggregate(elapsed_seconds ~ batch_size, results, mean)
batch_means$sd <- aggregate(elapsed_seconds ~ batch_size, results, sd)$elapsed_seconds
cat("\nMean Time by Batch Size:\n")
print(batch_means)

# By cores
core_means <- aggregate(elapsed_seconds ~ cores, results, mean)
core_means$sd <- aggregate(elapsed_seconds ~ cores, results, sd)$elapsed_seconds
cat("\nMean Time by Core Count:\n")
print(core_means)

# Interaction means
interaction_means <- aggregate(elapsed_seconds ~ batch_size + cores, results, mean)
interaction_means$sd <- aggregate(elapsed_seconds ~ batch_size + cores, results, sd)$elapsed_seconds
cat("\nMean Time by Condition (Batch Size × Cores):\n")
print(head(interaction_means, 12))
cat("... (showing first 12 of 36 conditions)\n\n")

# Speedup analysis
cat("\nSPEEDUP ANALYSIS\n")
cat("==================================================\n")

# Baseline: 1 core, batch_size=256 (largest batch, sequential)
baseline <- interaction_means[interaction_means$batch_size == 256 &
                              interaction_means$cores == 1, ]
baseline_time <- baseline$elapsed_seconds

# Compute speedup for all conditions
interaction_means$speedup <- baseline_time / interaction_means$elapsed_seconds

cat("Baseline condition (slowest expected):\n")
cat("  Batch size: 256, Cores: 1\n")
cat("  Mean time:", round(baseline_time, 2), "seconds\n\n")

# Find optimal condition
optimal <- interaction_means[which.max(interaction_means$speedup), ]
cat("Optimal condition (maximum speedup):\n")
cat("  Batch size:", optimal$batch_size, "\n")
cat("  Cores:", optimal$cores, "\n")
cat("  Mean time:", round(optimal$elapsed_seconds, 2), "seconds\n")
cat("  Speedup:", round(optimal$speedup, 2), "x\n\n")

# Optimal batch size per core count
cat("Optimal Batch Size by Core Count:\n")
for (c in levels(results$cores)) {
  subset_data <- interaction_means[interaction_means$cores == c, ]
  optimal_bs <- subset_data[which.min(subset_data$elapsed_seconds), ]
  cat(sprintf("  %2s cores: batch_size = %3s (%.2f sec, %.2fx speedup)\n",
              c, optimal_bs$batch_size, optimal_bs$elapsed_seconds,
              optimal_bs$speedup))
}
cat("\n")

# VISUALIZATION
cat("GENERATING VISUALIZATIONS\n")
cat("==================================================\n")

# Create plots directory
dir.create("artifacts/plots", showWarnings = FALSE, recursive = TRUE)

# 1. Heatmap: batch_size × cores → mean time
cat("Creating heatmap of computation time...\n")
heatmap_data <- dcast(interaction_means, batch_size ~ cores,
                      value.var = "elapsed_seconds")
rownames(heatmap_data) <- heatmap_data$batch_size
heatmap_data$batch_size <- NULL
heatmap_matrix <- as.matrix(heatmap_data)

pdf("artifacts/plots/heatmap_computation_time.pdf", width = 10, height = 8)
par(mar = c(5, 5, 4, 6))
image(
  x = 1:ncol(heatmap_matrix),
  y = 1:nrow(heatmap_matrix),
  z = t(heatmap_matrix),
  col = heat.colors(20, rev = TRUE),
  xlab = "Core Count",
  ylab = "Batch Size",
  main = "EXP-RT-001: Computation Time Heatmap",
  axes = FALSE
)
axis(1, at = 1:ncol(heatmap_matrix), labels = colnames(heatmap_matrix))
axis(2, at = 1:nrow(heatmap_matrix), labels = rownames(heatmap_matrix))

# Add values as text
for (i in 1:nrow(heatmap_matrix)) {
  for (j in 1:ncol(heatmap_matrix)) {
    text(j, i, sprintf("%.1f", heatmap_matrix[i, j]), cex = 0.7)
  }
}

# Add color legend
par(new = TRUE)
par(mar = c(5, 5, 4, 6))
plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
color_breaks <- seq(min(heatmap_matrix), max(heatmap_matrix), length.out = 21)
legend(
  "right",
  legend = sprintf("%.1f", color_breaks[seq(1, 21, by = 4)]),
  fill = heat.colors(20, rev = TRUE)[seq(1, 20, by = 4)],
  title = "Time (sec)",
  xpd = TRUE,
  inset = c(-0.15, 0)
)
dev.off()

# 2. Line plot: timing vs batch_size (one line per core count)
cat("Creating line plot of batch size effect...\n")
pdf("artifacts/plots/lines_batch_effect.pdf", width = 10, height = 7)
plot(
  NULL,
  xlim = range(as.numeric(as.character(results$batch_size))),
  ylim = range(interaction_means$elapsed_seconds),
  xlab = "Batch Size",
  ylab = "Computation Time (seconds)",
  main = "EXP-RT-001: Effect of Batch Size on Computation Time",
  log = "x"
)

colors <- rainbow(length(levels(results$cores)))
for (i in seq_along(levels(results$cores))) {
  c <- levels(results$cores)[i]
  subset_data <- interaction_means[interaction_means$cores == c, ]
  lines(
    x = as.numeric(as.character(subset_data$batch_size)),
    y = subset_data$elapsed_seconds,
    col = colors[i],
    lwd = 2
  )
  points(
    x = as.numeric(as.character(subset_data$batch_size)),
    y = subset_data$elapsed_seconds,
    col = colors[i],
    pch = 19,
    cex = 1.2
  )
}

legend(
  "topright",
  legend = paste(levels(results$cores), "cores"),
  col = colors,
  lwd = 2,
  pch = 19,
  title = "Core Count",
  bg = "white"
)
grid()
dev.off()

# 3. Speedup curves
cat("Creating speedup plot...\n")
pdf("artifacts/plots/speedup_curves.pdf", width = 10, height = 7)
plot(
  NULL,
  xlim = c(1, max(as.numeric(as.character(results$cores)))),
  ylim = c(1, max(interaction_means$speedup)),
  xlab = "Number of Cores",
  ylab = "Speedup (relative to baseline)",
  main = "EXP-RT-001: Parallel Speedup by Batch Size",
  log = "xy"
)

# Add ideal speedup line
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
text(2, 2, "Ideal Linear Speedup", pos = 4, col = "gray50")

colors_bs <- rainbow(length(levels(results$batch_size)))
for (i in seq_along(levels(results$batch_size))) {
  bs <- levels(results$batch_size)[i]
  subset_data <- interaction_means[interaction_means$batch_size == bs, ]
  lines(
    x = as.numeric(as.character(subset_data$cores)),
    y = subset_data$speedup,
    col = colors_bs[i],
    lwd = 2
  )
  points(
    x = as.numeric(as.character(subset_data$cores)),
    y = subset_data$speedup,
    col = colors_bs[i],
    pch = 19,
    cex = 1.2
  )
}

legend(
  "topleft",
  legend = paste("batch =", levels(results$batch_size)),
  col = colors_bs,
  lwd = 2,
  pch = 19,
  title = "Batch Size",
  bg = "white"
)
grid()
dev.off()

cat("All plots saved to artifacts/plots/\n\n")

# RECOMMENDATIONS
cat("==================================================\n")
cat("RECOMMENDATIONS FOR RIEMTAN PACKAGE\n")
cat("==================================================\n\n")

# Determine optimal default based on common use case
# Assume users have ~4-8 cores typically
typical_cores <- interaction_means[interaction_means$cores %in% c(4, 8), ]
avg_by_batch <- aggregate(elapsed_seconds ~ batch_size, typical_cores, mean)
optimal_default <- avg_by_batch[which.min(avg_by_batch$elapsed_seconds), ]

cat("Current default: batch_size = 32\n\n")

cat("Recommended default based on analysis:\n")
cat("  batch_size =", optimal_default$batch_size, "\n")
cat("  Rationale: Optimal for typical 4-8 core systems\n")
cat("  Average time (4-8 cores):", round(optimal_default$elapsed_seconds, 2), "sec\n\n")

cat("Adaptive recommendation:\n")
cat("  Consider making batch_size adaptive based on parallel::detectCores()\n")
cat("  Suggested formula: batch_size = max(8, n / (4 * detectCores()))\n")
cat("  This would:\n")
cat("    - Use smaller batches with more cores (more parallelization)\n")
cat("    - Use larger batches with fewer cores (less overhead)\n")
cat("    - Scale with sample size n\n\n")

# Documentation recommendations
cat("Documentation updates needed:\n")
cat("  1. Add guidance on batch_size selection in ?compute_frechet_mean\n")
cat("  2. Explain batch_size vs parallelization tradeoff\n")
cat("  3. Provide timing benchmarks from this experiment\n")
cat("  4. Note that Windows users won't benefit from batching (sequential)\n\n")

# Save summary report
cat("==================================================\n")
cat("SAVING ANALYSIS SUMMARY\n")
cat("==================================================\n")

summary_file <- "artifacts/analysis_summary.txt"
sink(summary_file)
cat("EXP-RT-001 Analysis Summary\n")
cat("Generated:", Sys.time(), "\n\n")
cat("ANOVA Results:\n")
print(summary(anova_model))
cat("\nEffect Sizes (η²):\n")
cat("  Batch size:", round(eta_sq[1], 3), "\n")
cat("  Cores:", round(eta_sq[2], 3), "\n")
cat("  Interaction:", round(eta_sq[3], 3), "\n\n")
cat("Optimal Condition:\n")
cat("  Batch size:", optimal$batch_size, "\n")
cat("  Cores:", optimal$cores, "\n")
cat("  Speedup:", round(optimal$speedup, 2), "x\n\n")
cat("Recommended Default:\n")
cat("  batch_size =", optimal_default$batch_size, "\n")
sink()

cat("Summary report saved to:", summary_file, "\n\n")

cat("==================================================\n")
cat("PHASE 5 COMPLETE: ✓\n")
cat("==================================================\n")
cat("\nAll analyses completed successfully!\n\n")

cat("Files generated:\n")
cat("  - artifacts/cluster_full_results.csv (data)\n")
cat("  - artifacts/analysis_summary.txt (text summary)\n")
cat("  - artifacts/plots/heatmap_computation_time.pdf\n")
cat("  - artifacts/plots/lines_batch_effect.pdf\n")
cat("  - artifacts/plots/speedup_curves.pdf\n\n")

cat("Next steps:\n")
cat("  1. Review plots and analysis summary\n")
cat("  2. Update Notion EXP-RT-001 page with results\n")
cat("  3. Create LOG entry documenting findings\n")
cat("  4. Update riemtan documentation\n")
cat("  5. Consider implementing adaptive batch_size\n")
