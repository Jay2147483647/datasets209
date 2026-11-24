# POLS 209: binned scatterplots, with optional adjustment for controls.
#
# Familiar Homework 3 call:
#   binscatter(my_data, "X", "Y")
# After accounting for controls (the Homework 4 residualization idea):
#   binscatter(my_data, "X", "Y", controls = c("CONTROL1", "CONTROL2"))
# Both X and Y are adjusted using the same usable rows. The line is fitted
# to the underlying observations, never to the bin averages.
# The returned ggplot can be labeled with + labs(...) and saved with ggsave().
#
# course_binscatter() remains available with its earlier argument order and
# list result. Its result now includes $plot for labeling and saving.
#
# November 2026 version (results lab): identical values of X are no longer split
# into separate dots by rounding error; when X takes only two values (a 0/1 X),
# no dot averages cases from both groups; both functions print the slope of
# the line.
#
# November 12 version: the figure no longer draws a gray point for every
# observation (show_points = TRUE draws them) and no longer puts a color legend
# in the subtitle. Instead, a note under the figure says what each dot is and
# what the line is, and gives the line's slope, which equals X's slope in the
# regression with the same controls. units names what one row of the data is
# (units = "polling locations"), for the note. Anything you give as caption
# goes on a new line after the note, and note = FALSE leaves the note out.
# course_binscatter() follows the same defaults: no default title, axis labels
# that name the controls taken out, and units for the note.

.course_field <- function(value, label, data) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value) || !value %in% names(data)) {
    stop(label, " must name one existing column, in quotation marks.", call. = FALSE)
  }
  value
}

.course_bin_count <- function(bins) {
  if (!is.numeric(bins) || length(bins) != 1L || !is.finite(bins) ||
      bins < 2 || bins != floor(bins) || bins > .Machine$integer.max) {
    stop("Use a whole number of at least 2 for the number of groups/bins.", call. = FALSE)
  }
  as.integer(bins)
}

.course_bins <- function(x, bins) {
  # Keep small/discrete measures together at their observed values.
  # Compare values after rounding at a tiny fraction of X's scale, so values
  # that differ only by computer rounding error share one group.
  scale <- max(abs(x))
  key <- if (is.finite(scale) && scale > 0) round(x / scale, 9) else x
  distinct <- sort(unique(key))
  bins <- min(bins, length(x))
  if (length(distinct) <= bins) return(match(key, distinct))
  # Homework 3's approximately equal-count rule, except that rows with the
  # same value of X stay in one group (so groups can differ a little in size).
  membership <- integer(length(x))
  membership[order(x, seq_along(x))] <- ceiling(seq_along(x) * bins / length(x))
  # November 2026: rows with the same value of X stay in one group, so one X
  # value never shows up as several dots.
  first_group <- tapply(membership, key, min)
  membership <- as.integer(first_group[as.character(key)])
  match(membership, sort(unique(membership)))
}

.course_plot_data <- function(data, outcome, explanatory, controls, bins, weights) {
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (anyDuplicated(names(data))) stop("The data frame has duplicated column names.", call. = FALSE)
  outcome <- .course_field(outcome, "outcome/y", data)
  explanatory <- .course_field(explanatory, "explanatory/x", data)
  if (is.null(controls)) controls <- character()
  if (!is.character(controls) || anyNA(controls) || any(!nzchar(controls))) {
    stop("controls must be a character vector of column names, in quotation marks.", call. = FALSE)
  }
  controls <- unique(controls)
  missing_controls <- setdiff(controls, names(data))
  if (length(missing_controls)) {
    stop("These control columns are missing: ", paste(missing_controls, collapse = ", "),
         if (any(grepl("(", missing_controls, fixed = TRUE)))
           ". Name columns only. For categories stored as numbers, make the column first, for example df$county <- factor(df$county_id), and name that column in controls."
         else "", call. = FALSE)
  }
  id_like <- controls[vapply(controls, function(v) {
    z <- data[[v]][!is.na(data[[v]])]
    is.numeric(z) && all(z == round(z)) && length(unique(z)) > 2L && length(unique(z)) <= 300L &&
      grepl("(^|_)(id|code|group|block|cell|pair|strata|stratum|cluster|team|section|district|county|state|region|province|wave|period|year)(_|$)",
            v, ignore.case = TRUE)
  }, logical(1))]
  if (length(id_like)) {
    message("Note: ", paste(id_like, collapse = ", "), " looks like a category stored as numbers, so it enters as an ",
            "amount. If it is a category, make a factor column, as with factor(), and use that column in lm() too.")
  }
  if (any(controls %in% c(outcome, explanatory))) {
    stop("Do not also put X or Y in controls; controls must name other columns.", call. = FALSE)
  }
  bins <- .course_bin_count(bins)
  numeric_measure <- function(z) is.numeric(z) || is.logical(z)
  if (!numeric_measure(data[[outcome]]) || !numeric_measure(data[[explanatory]])) {
    stop("X and Y must be numeric measures (including 0/1 indicators).", call. = FALSE)
  }
  if (any(!vapply(data[controls], function(z) numeric_measure(z) || is.factor(z) || is.character(z), logical(1)))) {
    stop("Each control must be numeric, a factor, or a character category.", call. = FALSE)
  }

  weight_name <- NULL
  if (is.null(weights)) {
    w <- rep(1, nrow(data))
  } else if (is.character(weights)) {
    weight_name <- .course_field(weights, "weights", data)
    w <- data[[weight_name]]
  } else {
    w <- weights
  }
  if (!is.numeric(w) || length(w) != nrow(data)) {
    stop("weights must name one numeric weight column or supply one number per data row.", call. = FALSE)
  }
  if (any(is.finite(w) & w <= 0)) {
    stop("Weights must be positive. Missing or non-finite weights are omitted automatically.", call. = FALSE)
  }

  needed <- unique(c(outcome, explanatory, controls))
  d <- data[needed]
  keep <- complete.cases(d) & is.finite(w)
  for (name in needed) {
    if (numeric_measure(d[[name]])) keep <- keep & is.finite(d[[name]])
  }
  used <- which(keep)
  omitted <- which(!keep)
  if (length(used) < 3L) {
    stop("Fewer than three usable rows remain after missing/non-finite values are omitted.", call. = FALSE)
  }
  if (length(omitted)) {
    message("Using ", length(used), " of ", nrow(data),
            " rows; omitted ", length(omitted), " with missing or non-finite plotting fields/weights.")
  }
  d <- d[keep, , drop = FALSE]
  w <- w[keep]
  # Rescaling every weight by the same constant leaves the fitted line and
  # averages unchanged, and avoids unnecessarily large working weights.
  w <- w / max(w)
  x <- as.numeric(d[[explanatory]])
  y <- as.numeric(d[[outcome]])
  if (length(unique(x)) < 2L) {
    stop("X has no variation in the usable rows, so its slope cannot be estimated.", call. = FALSE)
  }

  adjusted <- length(controls) > 0L
  x_plot <- x
  y_plot <- y
  if (adjusted) {
    control_data <- droplevels(d[controls])
    z <- tryCatch(model.matrix(~ ., data = control_data), error = function(e) {
      stop("The controls cannot be fitted: ", conditionMessage(e),
           ". Check their coding and variation in the usable rows.", call. = FALSE)
    })
    # This is the same two-regression step as lm()/resid() in Homework 4.
    # Building the control matrix once ensures exactly the same rows/coding
    # for both regressions, including categorical controls and weights.
    x_plot <- lm.wfit(z, x, w)$residuals
    y_plot <- lm.wfit(z, y, w)$residuals
    original_size <- sqrt(sum(w * (x - weighted.mean(x, w))^2))
    remaining_size <- sqrt(sum(w * x_plot^2))
    if (!is.finite(remaining_size) ||
        remaining_size <= sqrt(.Machine$double.eps) * original_size) {
      stop("These controls leave no independently estimable variation in X. An adjusted slope cannot be estimated.", call. = FALSE)
    }
  }

  row_fit <- lm.wfit(cbind(`(Intercept)` = 1, X = x_plot), y_plot, w)
  if (row_fit$rank < 2L || any(!is.finite(row_fit$coefficients))) {
    stop("The usable X values do not support an estimable line. Check X and the controls.", call. = FALSE)
  }
  two_groups <- sort(unique(x))
  if (length(two_groups) == 2L) {
    # A 0/1 X: form the groups within each value of X, in proportion to its
    # number of rows, so that no dot averages cases from both values.
    bin_id <- integer(length(x_plot))
    offset <- 0L
    for (value in two_groups) {
      at <- which(x == value)
      k <- max(min(3L, length(at)), round(bins * length(at) / length(x)))
      bin_id[at] <- .course_bins(x_plot[at], k) + offset
      offset <- max(bin_id[at])
    }
  } else {
    bin_id <- .course_bins(x_plot, bins)
  }
  positions <- split(seq_along(x_plot), bin_id)
  plotted <- do.call(rbind, lapply(names(positions), function(id) {
    at <- positions[[id]]
    data.frame(bin = as.integer(id),
               explanatory_value = weighted.mean(x_plot[at], w[at]),
               outcome_value = weighted.mean(y_plot[at], w[at]),
               n = length(at),
               explanatory_low = min(x_plot[at]),
               explanatory_high = max(x_plot[at]))
  }))
  plotted <- plotted[order(plotted$bin), , drop = FALSE]
  rownames(plotted) <- NULL
  list(plotted_values = plotted,
       observations = data.frame(explanatory_value = x_plot, outcome_value = y_plot),
       intercept = unname(row_fit$coefficients[[1L]]),
       slope = unname(row_fit$coefficients[[2L]]),
       n = length(used), bins = nrow(plotted), adjusted = adjusted,
       outcome = outcome, explanatory = explanatory, controls = controls,
       weighted = !is.null(weights), weight_column = weight_name,
       n_total = nrow(data), rows_used = used, rows_omitted = omitted)
}

.course_say_slope <- function(result) {
  n <- format(result$n, big.mark = ",")
  message("Slope of the line: ", format(signif(result$slope, 4)),
          if (result$adjusted) paste0(" (the same as the slope on ", result$explanatory,
                                      " in lm() with these controls, on these ", n, " rows)")
          else paste0(" (", n, " rows)"))
}

# The slope as the note prints it: two decimals, like the regression table,
# unless the slope is small enough to need more.
.course_slope_text <- function(b) {
  if (!is.finite(b)) return(format(b))
  if (abs(b) >= 1000) return(formatC(b, format = "f", digits = 0, big.mark = ","))
  if (abs(b) >= 0.1 || b == 0) return(formatC(b, format = "f", digits = 2))
  format(signif(b, 2), scientific = FALSE)
}

# The note under the figure: what each dot is, what the line is, and the
# line's slope. width is the number of characters per line of the note.
.course_note <- function(result, units = "observations", caption = NULL, note = TRUE, width = 115) {
  parts <- character()
  if (isTRUE(note)) {
    n <- format(result$n, big.mark = ",")
    w <- if (result$weighted) "weighted " else ""
    pv <- result$plotted_values
    one_value_each <- !result$adjusted && all(pv$explanatory_low == pv$explanatory_high)
    if (result$adjusted) {
      dots <- paste0("Each dot stands for a group of about ", round(result$n / result$bins), " ", units,
                     " with similar residuals on the horizontal axis, and it sits at the group's ",
                     w, "average residual on each axis.")
      line <- paste0("The line is the ", w, "OLS line through the residuals of all ", n, " ", units,
                     ". Its slope, ", .course_slope_text(result$slope),
                     ", is the same as X's slope in the regression with these controls.")
    } else {
      dots <- if (one_value_each) {
        paste0("Each dot stands for the ", units, " that share one value on the horizontal axis, ",
               "and it sits at their ", w, "average on the vertical axis.")
      } else {
        paste0("Each dot stands for a group of about ", round(result$n / result$bins), " ", units,
               " with similar values on the horizontal axis, and it sits at the group's ",
               w, "average on each axis.")
      }
      line <- paste0("The line is the ", w, "OLS line through all ", n, " ", units,
                     ". Its slope, ", .course_slope_text(result$slope),
                     ", is the same as X's slope in the regression without controls.")
    }
    parts <- paste(strwrap(paste("Notes:", dots, line), width = width), collapse = "\n")
  }
  if (!is.null(caption) && length(caption) && any(nzchar(caption))) {
    parts <- c(parts, paste(caption, collapse = "\n"))
  }
  if (!length(parts)) return(NULL)
  paste(parts, collapse = "\n")
}

.course_draw <- function(result, title, subtitle, xlab, ylab, point_col, line_col,
                         show_points = FALSE, shape = 19, point_size = 2.7,
                         xlim = NULL, ylim = NULL, caption = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This plotting helper needs ggplot2. Install it, then run the call again.", call. = FALSE)
  }
  p <- ggplot2::ggplot()
  if (show_points) {
    p <- p + ggplot2::geom_point(data = result$observations,
                                ggplot2::aes(x = explanatory_value, y = outcome_value),
                                color = "grey60", alpha = 0.35, size = 1.5)
  }
  p <- p +
    ggplot2::geom_abline(intercept = result$intercept, slope = result$slope,
                        color = line_col, linewidth = 1) +
    ggplot2::geom_point(data = result$plotted_values,
                       ggplot2::aes(x = explanatory_value, y = outcome_value),
                       color = point_col, size = point_size, shape = shape) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab, caption = caption) +
    # The note under the figure starts at the left edge, whatever theme is set.
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0),
                   plot.caption.position = "plot")
  if (!is.null(xlim) || !is.null(ylim)) p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  p
}

# Without your own axis labels, each axis is labeled with its column name,
# and for an adjusted binscatter, with the controls taken out of it.
.course_axis_label <- function(v, result) {
  if (!result$adjusted) return(v)
  paste(strwrap(paste0(v, ", residual after controlling for ",
                       paste(result$controls, collapse = ", ")), width = 45),
        collapse = "\n")
}

binscatter <- function(data, x, y, groups = 20, controls = character(), weights = NULL,
                       title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
                       caption = NULL, units = "observations", note = TRUE,
                       show_points = FALSE, show_table = FALSE) {
  result <- .course_plot_data(data, y, x, controls, groups, weights)
  if (is.null(xlab)) xlab <- .course_axis_label(x, result)
  if (is.null(ylab)) ylab <- .course_axis_label(y, result)
  p <- .course_draw(result, title, subtitle, xlab, ylab,
                    point_col = "#6B2737", line_col = "#B8860B",
                    caption = .course_note(result, units, caption, note),
                    show_points = isTRUE(show_points))
  # Optional inspection data: no additional report table is required.
  attr(p, "bin_summary") <- result$plotted_values
  attr(p, "underlying_slope") <- result$slope
  attr(p, "course_details") <- result[setdiff(names(result), "observations")]
  if (isTRUE(show_table)) print(result$plotted_values, row.names = FALSE)
  .course_say_slope(result)
  print(p)
  invisible(p)
}

course_binscatter <- function(data, outcome, explanatory, controls = character(), bins = 20L,
                             main = NULL, xlab = NULL, ylab = NULL,
                             point_col = "#500000", line_col = "#5F5F5F",
                             ..., weights = NULL, units = "observations") {
  result <- .course_plot_data(data, outcome, explanatory, controls, bins, weights)
  # November 12: the same defaults as binscatter(). No default title (give one
  # with main = "..."), and the axis labels say which controls were taken out.
  if (is.null(xlab)) xlab <- .course_axis_label(explanatory, result)
  if (is.null(ylab)) ylab <- .course_axis_label(outcome, result)
  dots <- list(...)
  allowed <- c("pch", "cex", "xlim", "ylim", "sub")
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) ||
                       any(!names(dots) %in% allowed))) {
    stop("Extra plotting arguments supported here are pch, cex, xlim, ylim, and sub. You can also edit the returned $plot with ggplot2.", call. = FALSE)
  }
  get_extra <- function(name, default = NULL) if (name %in% names(dots)) dots[[name]] else default
  result$plot <- .course_draw(result, main, NULL, xlab, ylab, point_col, line_col,
                              shape = get_extra("pch", 19),
                              point_size = 2.7 * get_extra("cex", 1),
                              xlim = get_extra("xlim"), ylim = get_extra("ylim"),
                              caption = .course_note(result, units, caption = get_extra("sub")))
  .course_say_slope(result)
  print(result$plot)
  invisible(result)
}
