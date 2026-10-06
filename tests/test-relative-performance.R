source("relative_performance.R")
pairs <- read.csv("tests/fixtures/hard-paired-2026-10-04.csv")
results <- analyze_relative_performance(pairs)
close_to <- function(actual, expected, tolerance = 1e-8) {
  stopifnot(length(actual) == length(expected), all(abs(actual - expected) < tolerance))
}

# Reference estimates come from the supplied independent Python analysis.
stopifnot(nrow(pairs) == 306, results$wins$n == 300)
close_to(results$primary$slope, 0.01825190)
close_to(results$primary$se, 0.01505812)
close_to(results$primary$p_value, 0.2254754, 1e-6)
close_to(results$checks$P[2:6], c(0.3699957, 0.2156743, 0.1689468,
  0.4161514, 0.4081295), 1e-6)
close_to(exp(results$wins$slope), 0.9453907, 1e-6)
close_to(results$wins$p_value, 0.1678232, 1e-6)
close_to(results$periods$Ratio, c(0.9900318, 1.3840561, 1.0436423, 2.5339477), 1e-6)
stopifnot(identical(results$periods$Pairs, c(120L, 91L, 91L, 4L)))
stopifnot(sum(pairs$Capped_Winner == "David") == 133,
  sum(pairs$Capped_Winner == "Ryan") == 167,
  sum(pairs$Capped_Winner == "Tie") == 6,
  sum(pairs$David_Was_Capped) == 7, sum(pairs$Ryan_Was_Capped) == 4,
  pairs$David_Seconds[pairs$Date == "2026-03-13"] == 377,
  !any(pairs$Date %in% c("2026-03-07", "2026-09-29", "2026-10-05")))
close_to(tail(results$monthly$Ratio, 1), (270 / 142 * 498 / 105 * 723 / 339 * 433 / 202)^(1 / 4))

# Whole-second synthetic data exercise caps, exclusions, and recomputed ties.
input <- data.frame(Date = as.Date("2026-01-01") + 0:5,
  David_Seconds = c(1500, 1200, 100, NA, 0, 200),
  Ryan_Seconds = c(1300, 1400, 200, 100, 100, 100))
prepared <- prepare_relative_pairs(input, as_of = as.Date("2026-01-05"))
stopifnot(nrow(prepared$pairs) == 3, nrow(prepared$excluded) == 3,
  all(head(prepared$pairs$Capped_Winner, 2) == "Tie"),
  all(head(prepared$pairs$Log_Ratio, 2) == 0),
  tail(prepared$pairs$Capped_Winner, 1) == "David")
duplicate <- rbind(input, input[1, ])
stopifnot(inherits(try(prepare_relative_pairs(duplicate), silent = TRUE), "try-error"))
inconsistent <- pairs
inconsistent$Log_Ratio[1] <- inconsistent$Log_Ratio[1] + 1
stopifnot(inherits(try(analyze_relative_performance(inconsistent), silent = TRUE), "try-error"))

# Check calendar padding against a separate explicit weighted score sum.
dates <- as.Date("2026-01-01") + c(0, 1, 4, 5, 9, 10)
time <- as.numeric(dates - min(dates)) / 30
model <- lm(c(0.2, 0.9, -0.3, 0.5, 1.2, 0.8) ~ time)
x <- model.matrix(model)
scores <- x * as.numeric(residuals(model))
meat <- crossprod(scores)
for (i in seq_len(nrow(scores))) {
  for (j in seq_len(nrow(scores))) {
    distance <- as.integer(dates[i] - dates[j])
    if (distance > 0 && distance <= 7) {
      cross <- outer(scores[i, ], scores[j, ])
      meat <- meat + (1 - distance / 8) * (cross + t(cross))
    }
  }
}
bread <- summary(model)$cov.unscaled
expected <- bread %*% meat %*% bread * nrow(x) / (nrow(x) - ncol(x))
close_to(as.numeric(calendar_hac(model, dates)), as.numeric(expected), 1e-7)

stopifnot(results$bootstrap$lower < 0, results$bootstrap$upper > 0,
  all(is.finite(results$prediction$Lower)),
  all(results$prediction$Lower <= results$prediction$Ratio),
  all(results$prediction$Ratio <= results$prediction$Upper))
bootstrap_a <- relative_block_bootstrap(pairs, results$primary, replicates = 100)
bootstrap_b <- relative_block_bootstrap(pairs, results$primary, replicates = 100)
stopifnot(identical(bootstrap_a, bootstrap_b))
cat("Relative-performance tests passed: reference estimates, data handling, calendar HAC, and reproducible bootstrap.\n")
