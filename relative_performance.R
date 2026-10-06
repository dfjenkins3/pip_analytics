prepare_relative_pairs <- function(data, cap_seconds = 1200, as_of = Sys.Date()) {
  required <- c("Date", "David_Seconds", "Ryan_Seconds")
  stopifnot(all(required %in% names(data)), is.finite(cap_seconds), cap_seconds > 0)
  data$Date <- as.Date(data$Date)
  if (anyNA(data$Date) || anyDuplicated(data$Date)) {
    stop("Relative-performance data must contain one row per valid date.")
  }
  valid <- with(data, Date <= as_of & is.finite(David_Seconds) &
    is.finite(Ryan_Seconds) & David_Seconds > 0 & Ryan_Seconds > 0)
  excluded <- data[!valid, required, drop = FALSE]
  pairs <- data[valid, required, drop = FALSE]
  pairs <- pairs[order(pairs$Date), , drop = FALSE]
  pairs$David_Was_Capped <- pairs$David_Seconds > cap_seconds
  pairs$Ryan_Was_Capped <- pairs$Ryan_Seconds > cap_seconds
  pairs$David_Seconds <- pmin(pairs$David_Seconds, cap_seconds)
  pairs$Ryan_Seconds <- pmin(pairs$Ryan_Seconds, cap_seconds)
  pairs$Log_Ratio <- log(pairs$David_Seconds / pairs$Ryan_Seconds)
  pairs$Capped_Winner <- ifelse(
    pairs$David_Seconds == pairs$Ryan_Seconds, "Tie",
    ifelse(pairs$David_Seconds < pairs$Ryan_Seconds, "David", "Ryan")
  )
  rownames(pairs) <- NULL
  list(pairs = pairs, excluded = excluded)
}

calendar_hac <- function(model, dates, lag_days = 7) {
  x <- stats::model.matrix(model)
  n <- nrow(x)
  k <- ncol(x)
  dates <- as.Date(dates)
  stopifnot(length(dates) == n, !anyNA(dates), !anyDuplicated(dates),
    n > k, lag_days >= 0, lag_days == as.integer(lag_days))
  if (inherits(model, "glm")) {
    if (model$family$family != "binomial" || model$family$link != "logit" || !model$converged) {
      stop("The supporting win model must be a converged binomial logit model.")
    }
    bread <- stats::vcov(model)
  } else {
    bread <- summary(model)$cov.unscaled
  }
  scores <- x * as.numeric(stats::residuals(model, type = "response"))
  day_index <- as.integer(dates - min(dates)) + 1L
  calendar_scores <- matrix(0, max(day_index), k)
  # Padding score vectors preserves calendar lags without imputing missing solves.
  calendar_scores[day_index, ] <- scores
  meat <- crossprod(calendar_scores)
  used_lag <- min(lag_days, nrow(calendar_scores) - 1L)
  if (used_lag > 0) {
    for (lag in seq_len(used_lag)) {
      later <- (lag + 1L):nrow(calendar_scores)
      earlier <- seq_len(nrow(calendar_scores) - lag)
      cross <- crossprod(calendar_scores[later, , drop = FALSE],
        calendar_scores[earlier, , drop = FALSE])
      meat <- meat + (1 - lag / (lag_days + 1)) * (cross + t(cross))
    }
  }
  covariance <- bread %*% meat %*% bread * n / (n - k)
  dimnames(covariance) <- list(colnames(x), colnames(x))
  covariance
}

fit_relative_trend <- function(pairs, outcome = "Log_Ratio", lag_days = 7,
  logistic = FALSE) {
  if (nrow(pairs) < 10 || length(unique(pairs$Date)) < 3) {
    stop("At least 10 paired results are required for a relative trend test.")
  }
  data <- pairs
  data$Time_30 <- as.numeric(data$Date - min(data$Date)) / 30
  formula <- stats::reformulate("Time_30", outcome)
  model <- if (logistic) {
    stats::glm(formula, data = data, family = stats::binomial())
  } else stats::lm(formula, data = data)
  covariance <- calendar_hac(model, data$Date, lag_days)
  slope <- unname(stats::coef(model)["Time_30"])
  se <- sqrt(covariance["Time_30", "Time_30"])
  interval <- slope + c(-1, 1) * stats::qnorm(0.975) * se
  list(model = model, covariance = covariance, origin = min(data$Date),
    n = nrow(data), slope = slope, se = se, lower = interval[1],
    upper = interval[2], p_value = 2 * stats::pnorm(-abs(slope / se)))
}

relative_block_bootstrap <- function(pairs, primary, replicates = 10000,
  block_length = 7, seed = 20261005) {
  set.seed(seed)
  x <- stats::model.matrix(primary$model)
  fitted <- as.numeric(stats::fitted(primary$model))
  residuals <- as.numeric(stats::residuals(primary$model))
  residuals <- residuals - mean(residuals)
  # Fixed dates and circular residual blocks retain the fitted calendar trend.
  bootstrap <- boot::tsboot(residuals, statistic = function(resampled) {
    unname(stats::lm.fit(x, fitted + resampled)$coefficients[2])
  }, R = replicates, l = block_length, sim = "fixed", endcorr = TRUE)
  interval <- 2 * primary$slope - rev(stats::quantile(bootstrap$t[, 1],
    c(0.025, 0.975), names = FALSE))
  list(lower = interval[1], upper = interval[2], replicates = replicates,
    block_length = block_length, seed = seed)
}

analyze_relative_performance <- function(pairs, replicates = 10000) {
  stopifnot(all(c("Date", "David_Seconds", "Ryan_Seconds", "Log_Ratio",
    "Capped_Winner") %in% names(pairs)))
  pairs$Date <- as.Date(pairs$Date)
  pairs <- pairs[order(pairs$Date), , drop = FALSE]
  if (anyNA(pairs$Date) || anyDuplicated(pairs$Date) ||
    any(!is.finite(pairs$David_Seconds) | !is.finite(pairs$Ryan_Seconds)) ||
    any(pairs$David_Seconds <= 0 | pairs$Ryan_Seconds <= 0 |
      pairs$David_Seconds > 1200 | pairs$Ryan_Seconds > 1200)) {
    stop("Analysis requires unique dates and positive paired times capped at 1200 seconds.")
  }
  expected_log_ratio <- log(pairs$David_Seconds / pairs$Ryan_Seconds)
  expected_winner <- ifelse(pairs$David_Seconds == pairs$Ryan_Seconds, "Tie",
    ifelse(pairs$David_Seconds < pairs$Ryan_Seconds, "David", "Ryan"))
  if (anyNA(pairs$Log_Ratio) || anyNA(pairs$Capped_Winner) ||
    any(abs(pairs$Log_Ratio - expected_log_ratio) > 1e-10) ||
    any(pairs$Capped_Winner != expected_winner)) {
    stop("Log ratios and capped winners must agree with the paired times.")
  }
  primary <- fit_relative_trend(pairs)
  latest_month <- as.Date(format(max(pairs$Date), "%Y-%m-01"))
  checks <- list(
    "Full record; 7-day window" = primary,
    "Exclude latest calendar month" = fit_relative_trend(pairs[pairs$Date < latest_month, ]),
    "Full record; 14-day window" = fit_relative_trend(pairs, lag_days = 14),
    "Full record; 30-day window" = fit_relative_trend(pairs, lag_days = 30)
  )
  if (sum(pairs$Date >= as.Date("2026-04-01")) >= 10) {
    checks[["Since April 2026 (exploratory)"]] <- fit_relative_trend(pairs[pairs$Date >= as.Date("2026-04-01"), ])
  }
  recent_start <- max(pairs$Date) - 89
  checks[["Last 90 calendar days (exploratory)"]] <- fit_relative_trend(pairs[pairs$Date >= recent_start, ])
  check_table <- do.call(rbind, lapply(names(checks), function(name) {
    result <- checks[[name]]
    data.frame(Check = name, Pairs = result$n, Change = expm1(result$slope),
      Lower = expm1(result$lower), Upper = expm1(result$upper), P = result$p_value)
  }))
  decisive <- pairs[pairs$Capped_Winner != "Tie", ]
  decisive$David_Win <- as.integer(decisive$Capped_Winner == "David")
  wins <- fit_relative_trend(decisive, outcome = "David_Win", logistic = TRUE)
  pairs$Time_Gap_Minutes <- (pairs$David_Seconds - pairs$Ryan_Seconds) / 60
  time_gap <- fit_relative_trend(pairs, outcome = "Time_Gap_Minutes")
  bootstrap <- relative_block_bootstrap(pairs, primary, replicates = replicates)
  monthly <- do.call(rbind, lapply(split(pairs, format(pairs$Date, "%Y-%m")), function(data) {
    data.frame(Month = as.Date(paste0(format(data$Date[1], "%Y-%m"), "-01")),
      Pairs = nrow(data), David_Wins = sum(data$Capped_Winner == "David"),
      Ryan_Wins = sum(data$Capped_Winner == "Ryan"), Ties = sum(data$Capped_Winner == "Tie"),
      Ratio = exp(mean(data$Log_Ratio)), Median_Ratio = stats::median(exp(data$Log_Ratio)))
  }))
  period_name <- ifelse(pairs$Date < as.Date("2026-04-01"), "December 2025-March 2026",
    ifelse(pairs$Date < as.Date("2026-07-01"), "April-June 2026",
      ifelse(pairs$Date < as.Date("2026-10-01"), "July-September 2026", format(pairs$Date, "%B %Y"))))
  periods <- do.call(rbind, lapply(unique(period_name), function(period) {
    data <- pairs[period_name == period, ]
    data.frame(Period = period, Pairs = nrow(data), Ratio = exp(mean(data$Log_Ratio)),
      Start = min(data$Date), End = max(data$Date))
  }))
  prediction <- data.frame(Date = seq(min(pairs$Date), max(pairs$Date), by = "day"))
  x <- cbind(1, as.numeric(prediction$Date - primary$origin) / 30)
  estimate <- as.numeric(x %*% stats::coef(primary$model))
  se <- sqrt(rowSums((x %*% primary$covariance) * x))
  prediction$Ratio <- exp(estimate)
  prediction$Lower <- exp(estimate - stats::qnorm(0.975) * se)
  prediction$Upper <- exp(estimate + stats::qnorm(0.975) * se)
  list(primary = primary, checks = check_table, wins = wins, time_gap = time_gap, bootstrap = bootstrap,
    monthly = monthly, periods = periods, prediction = prediction)
}
