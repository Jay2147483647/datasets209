# POLS 209: supplied table functions for the results lab (November 13 and 20).
# source("results_tools.R") runs this file. It defines regression_table(), for
# the results lab, and it also defines make_summary() and export_summary(), the
# same functions as in project_tools.R from the October 2 lab, so a script that
# sources only this file has every table function. It does not replace
# project_tools.R; keep both files in your folder. The calculations use base R
# functions you know.

# List each column with its label: make_summary(df_project, c(age = "Age (years)")).
# A column coded only 0 and 1 is summarized as an indicator; everything else as a
# number. Name ordered responses stored as scores in ordinal = "column_name".
make_summary <- function(data, variables, labels=NULL, types=NULL, ordinal=NULL) {
  if (!is.data.frame(data)) stop("data must be the data frame you imported.", call.=FALSE)
  if (nrow(data) == 0L) stop("There are no rows to summarize. Check your row selection.", call.=FALSE)
  if (!is.character(variables) || !length(variables) || anyNA(variables)) {
    stop('List your columns like this: c(column_name = "Readable label (units)").', call.=FALSE)
  }
  if (is.null(labels)) {
    if (is.null(names(variables)) || any(!nzchar(names(variables)))) {
      stop('Give every column a label: c(column_name = "Readable label (units)").', call.=FALSE)
    }
    labels <- unname(variables)
    variables <- names(variables)
  }
  if (length(variables) != length(labels)) {
    stop("Give one label for each column, in the same order.", call.=FALSE)
  }
  if (anyDuplicated(variables)) stop("Each column should appear only once.", call.=FALSE)
  if (anyNA(labels) || any(!nzchar(trimws(labels)))) stop("Give every column a readable label with its units.", call.=FALSE)
  absent <- setdiff(variables, names(data))
  if (length(absent)) stop("Column not found: ", paste(absent, collapse=", "),
                          ". Check the spelling with names(df_project).", call.=FALSE)
  if (length(setdiff(ordinal, variables))) {
    stop("ordinal names a column that is not in your list: ", paste(setdiff(ordinal, variables), collapse=", "), call.=FALSE)
  }
  if (is.null(types)) {
    types <- vapply(variables, function(v) {
      z <- data[[v]]
      z <- z[!is.na(z)]
      if (v %in% ordinal) "ordinal" else
        if (length(z) && is.numeric(z) && all(z %in% c(0, 1))) "indicator" else "numeric"
    }, character(1), USE.NAMES=FALSE)
  }
  if (length(types) != length(variables) || anyNA(types) ||
      any(!types %in% c("numeric", "indicator", "binary", "ordinal"))) {
    stop("Types must be numeric, indicator, or ordinal, one for each column. For categories, first make relevant 0/1 indicators; do not average category codes.", call.=FALSE)
  }
  types[types == "binary"] <- "indicator"
  probs <- c(0, .01, .10, .25, .50, .75, .90, .99, 1)
  pnames <- c("Min", "P1", "P10", "P25", "P50", "P75", "P90", "P99", "Max")
  summary_rows <- percentile_rows <- vector("list", length(variables))
  for (i in seq_along(variables)) {
    v <- data[[variables[i]]]
    # A CSV column that is entirely missing may be imported as logical.
    if (is.logical(v) && all(is.na(v))) v <- as.numeric(v)
    if (!is.numeric(v) || is.object(v)) {
      stop(variables[i], " is not a plain numeric column. Check the codebook; convert relevant categories to 0/1 indicators before summarizing.", call.=FALSE)
    }
    z <- v[!is.na(v)]
    if (any(!is.finite(z))) stop(variables[i], " contains an infinite value. Check its calculation before summarizing.", call.=FALSE)
    if (types[i] == "indicator" && any(!z %in% c(0, 1))) {
      stop(variables[i], " is not coded only 0, 1, and NA. Check your indicator definition.", call.=FALSE)
    }
    n <- length(z)
    summary_rows[[i]] <- data.frame(Variable=labels[i], N=n,
      Mean=if (n) mean(z) else NA_real_, SD=if (n > 1L) sd(z) else NA_real_,
      Observed=100*n/nrow(data), check.names=FALSE)
    # Same convention as quantile() and median(), so the table matches the Console.
    q <- if (n) quantile(z, probs, names=FALSE) else rep(NA_real_, 9L)
    percentile_rows[[i]] <- data.frame(Variable=labels[i],
      as.list(setNames(q, pnames)), check.names=FALSE)
  }
  list(summary=do.call(rbind, summary_rows), percentiles=do.call(rbind, percentile_rows),
       types=types, rows=nrow(data))
}

# Formatting and export. You can change the look of the tables with the
# optional arguments of export_summary(); see "How do I customize these tables?"
# in the lab page. The calculations above do not change.

.project_number <- function(x, digits, commas=TRUE) {
  if (is.na(x)) return("\u2014")
  if (digits > 6L) return(formatC(x, format="e", digits=1))
  formatC(x, format="f", digits=digits, big.mark=if (commas) "," else "")
}

# Commas only in rows whose values reach 10,000, so years print as 2006.
.project_commas <- function(values) {
  m <- suppressWarnings(max(abs(values), na.rm=TRUE))
  is.finite(m) && m >= 10000
}

# One number of decimals per row, chosen from the row's largest value: whole
# numbers from 1,000 up, the default below that, and more decimals for small
# values so they do not print as zeros.
.project_row_digits <- function(values, default) {
  m <- suppressWarnings(max(abs(values), na.rm=TRUE))
  if (!is.finite(m) || m == 0) return(default)
  if (m >= 1000) return(0L)
  if (m < 1) return(max(default, 1L - floor(log10(m))))
  default
}

.project_digits <- function(digits, n) {
  if (is.null(digits)) return(vector("list", n))
  if (!is.numeric(digits) || anyNA(digits) || any(digits < 0) || !length(digits) %in% c(1L, n)) {
    stop("Set digits to one number, such as digits = 2, or to one number per variable.", call.=FALSE)
  }
  as.list(rep(as.integer(digits), length.out=n))
}

.project_table <- function(df, widths, font, size) {
  ft <- flextable::flextable(df)
  ft <- flextable::font(ft, fontname=font, part="all")
  ft <- flextable::fontsize(ft, size=size, part="all")
  ft <- flextable::color(ft, color="#000000", part="all")
  ft <- flextable::bold(ft, bold=FALSE, part="all")
  ft <- flextable::align(ft, j=1, align="left", part="all")
  ft <- flextable::align(ft, j=2:ncol(df), align="right", part="all")
  ft <- flextable::valign(ft, valign="bottom", part="header")
  ft <- flextable::valign(ft, valign="top", part="body")
  ft <- flextable::padding(ft, padding.top=3, padding.bottom=3,
                          padding.left=4, padding.right=4, part="all")
  ft <- flextable::border_remove(ft)
  heavy <- officer::fp_border(color="#000000", width=1.25)
  light <- officer::fp_border(color="#000000", width=0.75)
  ft <- flextable::hline_top(ft, border=heavy, part="header")
  ft <- flextable::hline_bottom(ft, border=light, part="header")
  ft <- flextable::hline_bottom(ft, border=heavy, part="body")
  ft <- flextable::width(ft, width=widths)
  flextable::set_table_properties(ft, layout="fixed", align="left",
    opts_word=list(split=FALSE, repeat_headers=TRUE))
}

# Column widths in inches for a 6.8-inch text column. Number columns grow with
# their longest entry; the label column takes the rest. If the numbers are too
# long to fit, the font shrinks, down to 7 points.
.project_layout <- function(df, size) {
  chars <- vapply(names(df)[-1], function(j) max(nchar(c(j, df[[j]]))), numeric(1))
  repeat {
    number_widths <- pmax(.5, chars * .075 * size / 10 + .14)
    if (sum(number_widths) + 1.5 <= 6.8 || size <= 7) break
    size <- size - .5
  }
  widths <- c(max(1.5, 6.8 - sum(number_widths)), number_widths)
  list(widths=widths * min(1, 6.8 / sum(widths)), size=size)
}

.project_text <- function(doc, text, font, size=11, bold=FALSE, italic=FALSE, style="Normal",
                          after=6, before=0) {
  officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(text, officer::fp_text(font.family=font, font.size=size,
                                       color="#000000", bold=bold, italic=italic)),
    fp_p=officer::fp_par(padding=0, padding.bottom=after, padding.top=before,
      line_spacing=1.10, word_style=style,
      keep_with_next=style %in% c("Title", "heading 1"))))
}

# Table images carry their own title and notes, because a picture cannot hold
# a Word caption.
.project_image_table <- function(ft, heading, description, notes, font, size) {
  header <- c(heading, description)
  rows <- seq_along(header)
  ft <- flextable::add_header_lines(ft, values=header, top=TRUE)
  ft <- flextable::add_footer_lines(ft, values=notes)
  ft <- flextable::font(ft, fontname=font, part="all")
  ft <- flextable::align(ft, i=rows, align="left", part="header")
  ft <- flextable::fontsize(ft, i=rows, size=size, part="header")
  ft <- flextable::bold(ft, i=1, bold=TRUE, part="header")
  ft <- flextable::padding(ft, i=rows, padding.left=4, padding.right=4,
                           padding.top=if (length(rows)) 2 else 0, padding.bottom=4, part="header")
  ft <- flextable::fontsize(ft, size=size - 1, part="footer")
  ft <- flextable::align(ft, align="left", part="footer")
  ft <- flextable::padding(ft, padding.top=3, padding.bottom=2,
                           padding.left=4, padding.right=4, part="footer")
  # Redraw the three rules: above and below the column labels, and below the body.
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, i=max(rows), border=officer::fp_border(color="#000000", width=1.25),
                         part="header")
  ft <- flextable::hline_bottom(ft, border=officer::fp_border(color="#000000", width=.75),
                                part="header")
  ft <- flextable::hline_bottom(ft, border=officer::fp_border(color="#000000", width=1.25),
                                part="body")
  flextable::bg(ft, bg="white", part="all")
}

export_summary <- function(results, file="Project_Tables", title="Project data",
                           description=NULL, png=FALSE, digits=NULL, notes=NULL,
                           observed_column=TRUE, font="Arial", font_size=10,
                           table_titles=c("Summary statistics", "Percentiles")) {
  if (!is.list(results) || !all(c("summary", "percentiles", "types", "rows") %in% names(results))) {
    stop("Use the result from make_summary() as the first argument.", call.=FALSE)
  }
  needed <- c("flextable", "officer")
  missing_packages <- needed[!vapply(needed, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
  if (length(missing_packages)) {
    stop("To export the Word tables, run install.packages(c(",
         paste(sprintf('"%s"', missing_packages), collapse=", "), ")) once, then run export_summary() again.", call.=FALSE)
  }
  for (arg in list(png, observed_column)) if (!is.logical(arg) || length(arg) != 1L || is.na(arg)) {
    stop("Set png and observed_column to TRUE or FALSE.", call.=FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("Give file a name such as 'output/Project_Tables'.", call.=FALSE)
  }
  if (!is.numeric(font_size) || length(font_size) != 1L || font_size < 6 || font_size > 14) {
    stop("Set font_size to a number between 6 and 14.", call.=FALSE)
  }
  if (length(table_titles) != 2L) stop("Give two table titles, one for each table.", call.=FALSE)
  file <- sub("\\.docx$", "", file, ignore.case=TRUE)
  dir.create(dirname(file), recursive=TRUE, showWarnings=FALSE)
  types <- results$types
  row_digits <- .project_digits(digits, length(types))

  # Table 1: N, mean, SD, and (optionally) percent observed.
  s <- results$summary
  s$N <- formatC(s$N, format="f", digits=0, big.mark=",")
  s_digits <- vapply(seq_len(nrow(s)), function(i) if (!is.null(row_digits[[i]])) row_digits[[i]] else
    .project_row_digits(c(s$Mean[i], s$SD[i]), if (types[i] == "indicator") 3L else 2L), numeric(1))
  s_commas <- vapply(seq_len(nrow(s)), function(i) .project_commas(c(s$Mean[i], s$SD[i])), logical(1))
  for (j in c("Mean", "SD")) s[[j]] <- vapply(seq_len(nrow(s)), function(i)
    .project_number(s[[j]][i], s_digits[i], s_commas[i]), character(1))
  s$Observed <- vapply(s$Observed, function(rate) {
    # One decimal throughout. A nearly complete column shows 99.9%, never 100.0%;
    # a nearly empty one shows 0.1%, never 0.0%.
    if (rate > 99.9 && rate < 100) rate <- 99.9
    if (rate > 0 && rate < 0.1) rate <- 0.1
    paste0(formatC(rate, format="f", digits=1), "%")
  }, character(1))
  names(s)[names(s) == "Observed"] <- "% observed"
  if (!observed_column) s[["% observed"]] <- NULL

  # Table 2: percentiles. Indicator rows of whole numbers print as 0 and 1.
  p <- results$percentiles
  p_digits <- vapply(seq_len(nrow(p)), function(i) if (!is.null(row_digits[[i]])) row_digits[[i]] else
    if (types[i] == "indicator" && all(unlist(p[i, -1]) %in% c(0, 1, NA))) 0L else
    .project_row_digits(unlist(p[i, -1]), if (types[i] == "ordinal") 1L else 2L), numeric(1))
  p_commas <- vapply(seq_len(nrow(p)), function(i) .project_commas(unlist(p[i, -1])), logical(1))
  for (j in 2:ncol(p)) p[[j]] <- vapply(seq_len(nrow(p)), function(i)
    .project_number(p[[j]][i], p_digits[i], p_commas[i]), character(1))

  long <- vapply(seq_len(nrow(p)), function(i) max(nchar(unlist(p[i, -1]))), numeric(1)) >= 9
  if (any(long)) message("Some values are very long (", paste(p$Variable[long], collapse="; "),
    "). Consider dividing that variable by 1,000 and saying 'thousands' in its label.")

  sample_n <- formatC(results$rows, format="f", digits=0, big.mark=",")
  note1 <- paste0("Note: N is the number of observations with a recorded value",
    if (observed_column) paste0("; % observed is N divided by the ", sample_n, " rows in the data") else
      paste0(", out of ", sample_n, " rows"),
    ". Summaries are unweighted.",
    if (any(types == "indicator")) " Means of 0/1 indicators are proportions." else "",
    if (any(types == "ordinal")) " Ordered responses are summarized with their numerical scores." else "")
  note2 <- "Note: P50 is the median. Each row uses the observations with a recorded value for that variable."
  notes1 <- c(note1, notes)
  notes2 <- c(note2, notes)

  layout1 <- .project_layout(s, font_size)
  layout2 <- .project_layout(p, font_size - 1)
  fs <- .project_table(s, layout1$widths, font, layout1$size)
  fp <- .project_table(p, layout2$widths, font, layout2$size)
  headings <- paste0("Table ", 1:2, ". ", table_titles)

  doc <- officer::read_docx()
  doc <- officer::docx_set_paragraph_style(doc, style_id="Title", style_name="Title",
    fp_p=officer::fp_par(keep_with_next=TRUE),
    fp_t=officer::fp_text(font.family=font, font.size=16, color="#000000", bold=TRUE))
  doc <- officer::docx_set_paragraph_style(doc, style_id="Titre1", style_name="heading 1",
    fp_p=officer::fp_par(keep_with_next=TRUE),
    fp_t=officer::fp_text(font.family=font, font.size=11, color="#000000", bold=TRUE))
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size=officer::page_size(width=8.5, height=11),
    page_margins=officer::page_mar(top=.8, bottom=.8, left=.85, right=.85)))
  doc <- .project_text(doc, title, font, 16, TRUE, style="Title", after=10)
  for (k in 1:2) {
    doc <- .project_text(doc, headings[k], font, 11, TRUE, style="heading 1",
                         after=if (is.null(description)) 4 else 2, before=if (k == 1) 0 else 18)
    if (!is.null(description)) doc <- .project_text(doc, description, font, font_size, after=4)
    doc <- flextable::body_add_flextable(doc, if (k == 1) fs else fp)
    for (line in if (k == 1) notes1 else notes2) doc <- .project_text(doc, line, font, font_size - 1,
                                                                     after=2, before=3)
  }
  # Remove paragraph rules and underlines from officer's built-in Word styles so
  # headings look the same in Word and Google Docs. Table rules are unaffected.
  style_file <- file.path(doc$package_dir, "word", "styles.xml")
  style_xml <- xml2::read_xml(style_file)
  xml2::xml_remove(xml2::xml_find_all(style_xml,
    ".//w:style[@w:type='paragraph']/w:pPr/w:pBdr | .//w:style[@w:styleId='Title']//w:u"))
  xml2::write_xml(style_xml, style_file)
  xml2::xml_remove(xml2::xml_find_all(doc$doc_obj$get(), ".//w:pPr/w:pBdr | .//w:rPr/w:u"))
  print(doc, target=paste0(file, ".docx"))
  write.csv(results$summary, paste0(file, "_summary.csv"), row.names=FALSE, na="")
  write.csv(results$percentiles, paste0(file, "_percentiles.csv"), row.names=FALSE, na="")
  message("Saved ", file, ".docx, ", file, "_summary.csv, and ", file, "_percentiles.csv")
  if (png) {
    image_summary <- .project_image_table(fs, headings[1], description, notes1, font, layout1$size)
    image_percentiles <- .project_image_table(fp, headings[2], description, notes2, font, layout2$size)
    flextable::save_as_image(image_summary, path=paste0(file, "_summary.png"), res=300, expand=0)
    flextable::save_as_image(image_percentiles, path=paste0(file, "_percentiles.png"), res=300, expand=0)
    message("Also saved ", file, "_summary.png and ", file, "_percentiles.png (300 dpi).")
  }
  invisible(results)
}


# ---------------------------------------------------------------------------
# regression_table(): regressions side by side, in the style of a published
# table, saved as a Word file (results lab, November 2026).
#
# regression_table(
#   list("Without controls" = fit_without, "With controls" = fit_with),
#   labels  = c(x_column = "Readable label (units)", control_column = "Label"),
#   outcome = "what Y measures (units)",
#   title   = "Table 1. What the table shows",
#   file    = "Main_Question_Table")
#
# Each column is one model from lm() (or from grouped_lm()). Each slope prints
# with its standard error in parentheses below it. The numbers come straight
# from coef() and summary() (for grouped_lm(), from its grouped standard
# errors), so they match what R prints in the Console. The
# Console also shows the 95% confidence interval for the first variable in
# the table (your X) in each column.
#
# It saves the table twice: a Word file (FILE.docx) with an editable table to
# copy into your report, and a picture (FILE.png) to insert like a figure.
# It also saves FILE.csv with every number. png = FALSE skips the picture, and
# file = NULL saves nothing.
# ---------------------------------------------------------------------------

.rt_strip <- function(v) sub("^(as\\.)?factor\\((.*)\\)$", "\\2", v)

.rt_label <- function(v, labels) {
  if (!is.null(labels)) {
    if (v %in% names(labels)) return(unname(labels[[v]]))
    s <- .rt_strip(v)
    stripped <- .rt_strip(names(labels))
    if (s %in% stripped) return(unname(labels[[match(s, stripped)]]))
  }
  v
}

.rt_is_grouped <- function(m) is.list(m) && !inherits(m, "lm") && all(c("model", "grouped_se") %in% names(m))

# One column's numbers: estimates, standard errors, p-values and confidence
# intervals by coefficient, plus what is needed to label each coefficient.
.rt_model <- function(m, name, hidden_vars = character()) {
  grouped <- NULL
  if (.rt_is_grouped(m)) {
    grouped <- m
    m <- grouped$model
  }
  if (!inherits(m, "lm") || inherits(m, "glm")) {
    stop("Column '", name, "' is not a model from lm(). Put lm() results in the list, ",
         "like list(\"Without controls\" = fit_without, \"With controls\" = fit_with).", call. = FALSE)
  }
  mf <- stats::model.frame(m)
  b <- coef(m)
  if (anyNA(b)) {
    bad <- names(b)[is.na(b)]
    constant <- names(mf)[-1][vapply(mf[-1], function(z) length(unique(z)) < 2L, logical(1))]
    if (length(constant)) {
      stop("In column '", name, "', ", constant[1], " has the same value in every row (",
           as.character(unique(mf[[constant[1]]])[1]), "), so there is nothing to compare. ",
           "Run table() on it and check the line that made it. If you kept only some rows on ",
           "purpose, take it out of the model.", call. = FALSE)
    }
    # Is X itself still estimable once R drops the redundant columns?
    mm <- stats::model.matrix(m)
    asg <- attr(mm, "assign")
    first <- attr(stats::terms(m), "term.labels")[1]
    x_cols <- which(asg == 1L)
    others <- setdiff(seq_len(ncol(mm)), x_cols)
    x_lost <- FALSE
    for (j in x_cols) {
      fit_x <- stats::lm.fit(mm[, others, drop = FALSE], mm[, j])
      if (sum(fit_x$residuals^2) <= 1e-8 * sum((mm[, j] - mean(mm[, j]))^2)) x_lost <- TRUE
    }
    if (x_lost) {
      stop("In column '", name, "', the controls predict ", first, " exactly, so nothing is left of X to ",
           "compare. Usually a control is the group X was assigned by (or a smaller group inside it), ",
           "such as county indicators when X was assigned by county, or a control is the same as X. ",
           "Take that control out.", call. = FALSE)
    }
    bad_terms <- unique(.rt_strip(c("(Intercept)", attr(stats::terms(m), "term.labels"))[asg[match(bad, colnames(mm))] + 1L]))
    if (length(bad_terms) && all(bad_terms %in% hidden_vars)) {
      message("In column '", name, "', R dropped ", length(bad), " indicator", if (length(bad) > 1) "s" else "",
              " that the other controls already cover (", paste(head(bad, 3), collapse = ", "),
              if (length(bad) > 3) ", ..." else "", "). That doesn't change the other estimates.")
      b <- b[!is.na(b)]
    } else {
      stop("In column '", name, "', R could not estimate ", paste(head(bad, 3), collapse = ", "),
           if (length(bad) > 3) ", ..." else "", " (lm() returned NA). Some variable is completely ",
           "predicted by the others. Common causes: an indicator for every category of the same ",
           "variable (leave one out); a category control that sits inside another (a league when each ",
           "block is in one league); a control that is the same as X, or the sum or difference of ",
           "others; or, with an interaction, a group that is already inside your category controls ",
           "(write X + X:GROUP instead of X * GROUP), or a group where every case has the same X.", call. = FALSE)
    }
  }
  if (!"(Intercept)" %in% names(b)) {
    stop("Column '", name, "' has no intercept. Remove '- 1' or '+ 0' from its formula.", call. = FALSE)
  }
  s <- summary(m)
  cs <- coef(s)
  if (is.null(grouped)) {
    se <- setNames(cs[, "Std. Error"], rownames(cs))[names(b)]
    p <- setNames(cs[, "Pr(>|t|)"], rownames(cs))[names(b)]
    df <- m$df.residual
    group_fields <- NULL
    group_counts <- NULL
  } else {
    se <- grouped$grouped_se[names(b)]
    if (anyNA(se)) stop("Column '", name, "': the grouped standard errors do not match the model's terms.", call. = FALSE)
    df <- min(grouped$groups) - 1
    p <- 2 * stats::pt(-abs(b / se), df = df)
    group_fields <- if (!is.null(grouped$grouping_fields)) grouped$grouping_fields else grouped$grouping_field
    group_counts <- grouped$groups
    # Few treated groups make grouped standard errors unreliable.
    xv <- mf[[attr(stats::terms(m), "term.labels")[1]]]
    if (!is.null(grouped$group_values) && is.numeric(xv) && all(xv %in% c(0, 1))) {
      gmean <- tapply(xv, grouped$group_values, mean)
      if (all(gmean %in% c(0, 1)) && min(sum(gmean == 1), sum(gmean == 0)) < 5) {
        message("In column '", name, "', X is the same within each group, and only ",
                min(sum(gmean == 1), sum(gmean == 0)), " group", if (min(sum(gmean == 1), sum(gmean == 0)) > 1) "s have" else " has",
                " X = ", if (sum(gmean == 1) < sum(gmean == 0)) 1 else 0,
                ". Grouped standard errors are unreliable with so few groups on one side.")
      }
    }
  }
  crit <- stats::qt(0.975, df)
  tt <- stats::terms(m)
  term_labels <- attr(tt, "term.labels")
  assign <- attr(stats::model.matrix(m), "assign")[!is.na(coef(m))]
  y_name <- names(mf)[1]
  categorical <- names(mf)[vapply(mf, function(z) is.factor(z) || is.character(z) || is.logical(z), logical(1))]
  list(b = b, se = se, p = p, low = b - crit * se, high = b + crit * se,
       r2 = s$r.squared, n = stats::nobs(m), name = name,
       term_of = setNames(c("(Intercept)", term_labels)[assign + 1L], names(b)),
       first_term = term_labels[1], categorical = categorical, mf = mf, y = y_name,
       group_fields = group_fields, group_counts = group_counts,
       variables = setdiff(names(mf), y_name))
}

# Split a coefficient name like "x:regionSouth" into its variables and levels.
# Rebuild the names R gives each column of the term and match them, so a
# category whose name contains ":" still works.
.rt_levels <- function(x) {
  if (is.factor(x)) levels(x) else if (is.logical(x)) c("FALSE", "TRUE") else sort(unique(as.character(x)))
}

.rt_parts <- function(coef_name, term, info) {
  vars <- strsplit(term, ":", fixed = TRUE)[[1]]
  options <- lapply(vars, function(v) {
    if (v %in% info$categorical) {
      lev <- .rt_levels(info$mf[[v]])
      data.frame(piece = paste0(v, lev), level = lev, stringsAsFactors = FALSE)
    } else data.frame(piece = v, level = NA_character_, stringsAsFactors = FALSE)
  })
  grid <- expand.grid(lapply(options, function(o) seq_len(nrow(o))))
  for (r in seq_len(nrow(grid))) {
    idx <- unlist(grid[r, ])
    pieces <- vapply(seq_along(vars), function(i) options[[i]]$piece[idx[i]], "")
    if (identical(paste(pieces, collapse = ":"), coef_name)) {
      return(lapply(seq_along(vars), function(i) {
        lev <- options[[i]]$level[idx[i]]
        list(var = vars[i], level = if (is.na(lev)) NULL else lev)
      }))
    }
  }
  list(list(var = coef_name, level = NULL))
}

.rt_reference <- function(z, v) {
  x <- z$mf[[v]]
  if (is.factor(x)) levels(droplevels(x))[1] else sort(unique(as.character(x)))[1]
}

regression_table <- function(models, labels = NULL, outcome = NULL, title = NULL,
                             file = "Results_Table", notes = NULL, hide = NULL,
                             observations = "observations", digits = NULL,
                             png = TRUE, font = "Arial", font_size = 10) {
  if (inherits(models, "lm") || .rt_is_grouped(models)) models <- list(models)
  if (inherits(labels, "lm") || .rt_is_grouped(labels)) {
    stop("Put all your models inside one list(): regression_table(list(\"Without controls\" = fit_without, ",
         "\"With controls\" = fit_with), ...).", call. = FALSE)
  }
  if (!is.list(models) || !length(models)) {
    stop("Put your models in a list: list(\"Without controls\" = fit_without, \"With controls\" = fit_with).", call. = FALSE)
  }
  if (is.null(title) || !is.character(title) || length(title) != 1L || !nzchar(title)) {
    stop('Give the table a title, like title = "Table 1. What predicts YOUR OUTCOME".', call. = FALSE)
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    stop('Give labels like this: labels = c(column_name = "Readable label (units)").', call. = FALSE)
  }
  if (!is.null(hide) && (!is.list(hide) || is.null(names(hide)))) {
    stop('Give hide like this: hide = list("Region indicators" = "region").', call. = FALSE)
  }
  if (!is.null(digits) && (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0)) {
    stop("Set digits to one whole number, such as digits = 2.", call. = FALSE)
  }
  column_names <- names(models)
  if (is.null(column_names)) column_names <- rep("", length(models))
  column_names[!nzchar(column_names)] <- paste0("(", which(!nzchar(column_names)), ")")
  hidden_vars <- unlist(lapply(hide, .rt_strip), use.names = FALSE)
  info <- Map(.rt_model, models, column_names, MoreArgs = list(hidden_vars = hidden_vars))

  if (is.null(outcome)) outcome <- vapply(info, function(z) .rt_label(z$y, labels), character(1))
  if (!length(outcome) %in% c(1L, length(info))) stop("Give one outcome label, or one for each column.", call. = FALSE)
  outcome <- rep(outcome, length.out = length(info))
  same_outcome <- length(unique(outcome)) == 1L

  # Variables collapsed into one Yes/No row: each must be in some model.
  known_vars <- unique(unlist(lapply(info, function(z) .rt_strip(z$variables))))
  unused <- setdiff(hidden_vars, known_vars)
  if (length(unused)) {
    stop("hide names ", paste(unused, collapse = ", "), ", which is not a variable in any of the models. ",
         "Use the column name from your lm() formula, once, like hide = list(\"Region indicators\" = \"region\"). ",
         "If you have no categorical controls, delete the hide line.",
         call. = FALSE)
  }

  # Warnings a student should see.
  told <- character()
  for (z in info) {
    v <- z$first_term
    if (!is.na(v) && v %in% z$categorical && !v %in% told) {
      told <- c(told, v)
      message("Check: the first variable, ", v, ", is text or a factor, so its rows compare each category with ",
              .rt_reference(z, v), ". If that is not your comparison group, make a 0/1 column for X, or use ",
              "factor(..., levels = ...) with the comparison group first.")
    }
  }
  for (z in info) for (v in setdiff(z$variables, c(z$categorical, z$first_term))) {
    x <- z$mf[[v]]
    if (is.numeric(x) && !v %in% told && all(x == round(x)) && length(unique(x)) > 2L && length(unique(x)) <= 300L &&
        grepl("(^|_)(id|code|group|block|cell|pair|strata|stratum|cluster|team|section|district|county|state|region|province|wave|period|year)(_|$)",
              v, ignore.case = TRUE)) {
      told <- c(told, v)
      message("Check: ", v, " looks like categories stored as numbers, so lm() treats it as an amount. ",
              "If it is a category, make a factor column and use that instead.")
    }
  }
  if (length(info) > 1L) {
    ys <- vapply(info, `[[`, "", "y")
    ns <- vapply(info, `[[`, 0, "n")
    for (y in unique(ys)) {
      at <- which(ys == y)
      if (length(unique(ns[at])) > 1L) {
        message("Note: the columns that predict ", y, " use different numbers of rows (",
                paste(paste0(column_names[at], ": ", format(ns[at], big.mark = ",")), collapse = "; "),
                "). To compare them, drop rows missing X, Y, or any control before fitting the models.")
      }
    }
  }

  # Collect the rows: one key per coefficient, shared across columns.
  label_vars <- if (is.null(labels)) character() else .rt_strip(names(labels))
  rows <- list()
  for (k in seq_along(info)) {
    z <- info[[k]]
    for (cn in names(z$b)) {
      term <- z$term_of[[cn]]
      if (term == "(Intercept)") next
      parts <- .rt_parts(cn, term, z)
      if (length(parts) == 1L && .rt_strip(parts[[1]]$var) %in% hidden_vars) next
      # Labeled variables first, in the order of labels.
      pos <- vapply(parts, function(q) { m <- match(.rt_strip(q$var), label_vars); if (is.na(m)) 1000L else m }, 0L)
      parts <- parts[order(pos)]
      pos <- sort(pos)
      key <- paste(vapply(parts, function(q) paste0(.rt_strip(q$var), "|", if (is.null(q$level)) "" else q$level),
                          character(1)), collapse = " x ")
      if (is.null(rows[[key]])) {
        lab <- paste(vapply(parts, function(q) {
          l <- .rt_label(q$var, labels)
          # In a product row, drop each label's closing note in parentheses.
          if (length(parts) > 1L) l <- sub("\\s*\\([^()]*\\)\\s*$", "", l)
          if (is.null(q$level)) l else paste0(l, ": ", q$level)
        }, character(1)), collapse = " \u00d7 ")
        rows[[key]] <- list(label = lab, est = rep(NA_real_, length(info)), se = rep(NA_real_, length(info)),
                            p = rep(NA_real_, length(info)), low = rep(NA_real_, length(info)),
                            high = rep(NA_real_, length(info)),
                            order = pos[1] + 0.01 * (length(parts) - 1), seen = length(rows))
      }
      rows[[key]]$est[k] <- z$b[[cn]]
      rows[[key]]$se[k] <- z$se[[cn]]
      rows[[key]]$p[k] <- z$p[[cn]]
      rows[[key]]$low[k] <- z$low[[cn]]
      rows[[key]]$high[k] <- z$high[[cn]]
    }
  }
  if (length(rows)) rows <- rows[order(vapply(rows, `[[`, 0, "order"), vapply(rows, `[[`, 0, "seen"))]
  intercept <- list(label = "Intercept",
                    est = vapply(info, function(z) z$b[["(Intercept)"]], 0),
                    se = vapply(info, function(z) z$se[["(Intercept)"]], 0),
                    p = vapply(info, function(z) z$p[["(Intercept)"]], 0),
                    low = vapply(info, function(z) z$low[["(Intercept)"]], 0),
                    high = vapply(info, function(z) z$high[["(Intercept)"]], 0))
  all_rows <- c(unname(rows), list(intercept))

  # Decimals: about two digits of each standard error. When every column
  # predicts the same outcome, one row shares one number of decimals.
  cell_digits <- function(s) {
    if (!is.null(digits)) return(as.integer(digits))
    if (is.na(s) || !is.finite(s) || s == 0) return(2L)
    s <- abs(s)
    if (s >= 100) return(0L)
    if (s >= 10) return(1L)
    as.integer(max(2, min(4, 1 - floor(log10(s)))))
  }
  fmt <- function(x, d) if (is.na(x)) "" else formatC(x, format = "f", digits = d, big.mark = ",")
  table_rows <- list()
  tiny <- character()
  for (r in all_rows) {
    d <- vapply(r$se, cell_digits, 0L)
    if (same_outcome && is.null(digits)) d <- rep(max(d[!is.na(r$se)], 0L), length(d))
    if (is.null(digits) && any(!is.na(r$se) & abs(r$se) < 0.00005)) tiny <- c(tiny, r$label)
    est <- vapply(seq_along(r$est), function(j) {
      if (is.na(r$est[j])) return("")
      v <- if (round(r$est[j], d[j]) == 0) 0 else r$est[j]
      e <- paste0(fmt(v, d[j]), if (!is.na(r$p[j]) && r$p[j] < 0.05) "*" else "")
      sub("^-", "\u2212", e)
    }, "")
    se <- vapply(seq_along(r$se), function(j) if (is.na(r$se[j])) "" else paste0("(", fmt(r$se[j], d[j]), ")"), "")
    table_rows[[length(table_rows) + 1L]] <- c(r$label, est)
    table_rows[[length(table_rows) + 1L]] <- c("", se)
  }
  if (length(tiny)) {
    message("These rows print as zeros because the variable is measured in large units: ",
            paste(tiny, collapse = "; "), ". Divide it by 1,000 or 1,000,000 before lm() and say so in its label.")
  }
  n_coef_lines <- length(table_rows)
  for (h in names(hide)) {
    vars <- .rt_strip(hide[[h]])
    has <- vapply(info, function(z) any(.rt_strip(z$variables) %in% vars), logical(1))
    table_rows[[length(table_rows) + 1L]] <- c(h, ifelse(has, "Yes", "No"))
  }
  table_rows[[length(table_rows) + 1L]] <- c("R-squared", formatC(vapply(info, `[[`, 0, "r2"), format = "f", digits = 3))
  table_rows[[length(table_rows) + 1L]] <- c(paste("Number of", observations),
                                             formatC(vapply(info, `[[`, 0, "n"), format = "d", big.mark = ","))
  body <- as.data.frame(do.call(rbind, table_rows), stringsAsFactors = FALSE)
  names(body) <- c(" ", make.unique(column_names, sep = " "))

  # The note: how to read the table, then reference categories and grouping.
  note <- if (same_outcome) {
    paste0("Notes: Each column is a separate regression that predicts ", outcome[1], ".")
  } else {
    "Notes: Each column is a separate regression; the top of each column names the variable it predicts."
  }
  note <- paste(note, "In each row, the top number is the slope on that variable (or the intercept), and the number in parentheses below it is its standard error. A blank means the variable is not in that regression.")
  refs <- character()
  for (z in info) for (v in intersect(z$categorical, z$variables)) {
    if (.rt_strip(v) %in% hidden_vars) next
    refs[.rt_strip(v)] <- paste0("Rows for \u201c", .rt_label(v, labels), "\u201d compare each category with the reference category, ",
                                 .rt_reference(z, v), ".")
  }
  note <- paste(c(note, unique(refs)), collapse = " ")
  grouped_cols <- which(!vapply(info, function(z) is.null(z$group_fields), logical(1)))
  if (length(grouped_cols)) {
    describe <- function(z) paste(paste0(vapply(z$group_fields, .rt_label, "", labels = labels), " (",
                                         format(z$group_counts, big.mark = ","), " groups)"), collapse = " and ")
    groups_text <- vapply(info[grouped_cols], describe, "")
    quoted <- paste0("\u201c", column_names[grouped_cols], "\u201d")
    if (length(unique(groups_text)) == 1L) {
      where <- if (length(grouped_cols) == length(info)) "every column" else
        paste0("the ", paste(quoted, collapse = " and "), " column", if (length(grouped_cols) > 1L) "s" else "")
      note <- paste0(note, " Standard errors in ", where, " are grouped by ", groups_text[1], ".")
    } else {
      note <- paste0(note, " Standard errors are grouped by ", paste0(groups_text, " in the ", quoted, " column", collapse = "; "), ".")
    }
  }
  note <- paste(note, "* p < 0.05, two-tailed t-test.")
  note_lines <- c(note, notes)

  # Console version, and the interval for the first variable (your X).
  cat("\n", title, "\n", sep = "")
  if (!same_outcome) cat("Outcomes: ", paste(paste0(column_names, ": ", outcome), collapse = "; "), "\n", sep = "")
  shown <- body
  long <- nchar(shown[[1]]) > 48
  shown[[1]][long] <- paste0(substr(shown[[1]][long], 1, 46), "...")
  print(shown, row.names = FALSE, right = FALSE)
  for (v in unique(refs)) message(v)
  if (length(rows)) {
    r <- rows[[1]]
    have <- which(!is.na(r$est))
    d <- vapply(r$se, cell_digits, 0L)
    if (same_outcome) d[] <- max(d[have])
    message("95% confidence interval for ", r$label, ": ",
            paste(vapply(have, function(j) paste0(column_names[j], ", ", fmt(r$low[j], d[j]), " to ", fmt(r$high[j], d[j])), ""),
                  collapse = "; "),
            if (!is.null(file)) paste0(". Every row's interval is in ", sub("\\.docx$", "", file), ".csv.") else "")
  }

  num <- do.call(rbind, lapply(all_rows, function(r) {
    data.frame(row = r$label, column = column_names, estimate = r$est, std_error = r$se, p_value = r$p,
               ci_low = r$low, ci_high = r$high, check.names = FALSE)
  }))
  num <- num[!is.na(num$estimate), , drop = FALSE]
  if (is.null(file)) return(invisible(num))

  needed <- c("flextable", "officer")
  missing_packages <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_packages)) {
    stop("To save the Word table, run install.packages(c(",
         paste(sprintf('"%s"', missing_packages), collapse = ", "), ")) once, then run regression_table() again.", call. = FALSE)
  }
  file <- sub("\\.docx$", "", file, ignore.case = TRUE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  # Column widths for a 6.8-inch text column: number columns first, the label
  # column takes what is left.
  k_cols <- ncol(body) - 1L
  want_label <- max(1.6, 0.075 * font_size / 10 * max(nchar(body[[1]])))
  col_width <- max(0.75, min(1.4, (6.8 - min(want_label, 3.4)) / k_cols))
  label_width <- min(want_label, 6.8 - k_cols * col_width)
  if (label_width < 1.4) {
    label_width <- 1.4
    col_width <- (6.8 - label_width) / k_cols
  }

  style_table <- function(ft) {
    ft <- flextable::font(ft, fontname = font, part = "all")
    ft <- flextable::fontsize(ft, size = font_size, part = "all")
    ft <- flextable::color(ft, color = "#000000", part = "all")
    ft <- flextable::bold(ft, bold = FALSE, part = "all")
    ft <- flextable::align(ft, j = 1, align = "left", part = "all")
    ft <- flextable::align(ft, j = 2:ncol(body), align = "center", part = "all")
    ft <- flextable::valign(ft, valign = "bottom", part = "header")
    ft <- flextable::valign(ft, valign = "top", part = "body")
    # As in a published table (and Homework 4's Table 1): each standard error
    # sits right under its slope, with a little space between variables.
    ft <- flextable::padding(ft, padding.top = 0, padding.bottom = 0, padding.left = 4, padding.right = 4, part = "body")
    ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "header")
    est_rows <- seq(1, n_coef_lines, by = 2)
    ft <- flextable::padding(ft, i = est_rows, padding.top = 6, part = "body")
    ft <- flextable::padding(ft, i = 1, padding.top = 3, part = "body")
    if (nrow(body) > n_coef_lines) {
      ft <- flextable::padding(ft, i = n_coef_lines + 1, padding.top = 3, part = "body")
      ft <- flextable::padding(ft, i = nrow(body), padding.bottom = 3, part = "body")
    }
    ft <- flextable::line_spacing(ft, space = 1, part = "all")
    # Rows as tall as their text in Word, LibreOffice and Google Docs alike.
    ft <- flextable::height_all(ft, height = 0.05, part = "all")
    ft <- flextable::hrule(ft, rule = "atleast", part = "all")
    ft
  }
  rules <- function(ft, header_rule_row) {
    heavy <- officer::fp_border(color = "#000000", width = 1.25)
    light <- officer::fp_border(color = "#000000", width = 0.75)
    ft <- flextable::border_remove(ft)
    ft <- flextable::hline(ft, i = header_rule_row, border = heavy, part = "header")
    ft <- flextable::hline_bottom(ft, border = light, part = "header")
    ft <- flextable::hline(ft, i = n_coef_lines, border = light, part = "body")
    flextable::hline_bottom(ft, border = heavy, part = "body")
  }
  ft <- flextable::flextable(body)
  if (!same_outcome) {
    ft <- flextable::add_header_row(ft, values = c("", outcome), top = TRUE)
    ft <- flextable::merge_h(ft, i = 1, part = "header")
  }
  ft <- style_table(ft)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = officer::fp_border(color = "#000000", width = 1.25), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "#000000", width = 0.75), part = "header")
  ft <- flextable::hline(ft, i = n_coef_lines, border = officer::fp_border(color = "#000000", width = 0.75), part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "#000000", width = 1.25), part = "body")
  ft <- flextable::width(ft, j = 1, width = label_width)
  ft <- flextable::width(ft, j = 2:ncol(body), width = col_width)
  ft <- flextable::set_table_properties(ft, layout = "fixed", align = "left",
                                        opts_word = list(split = FALSE, repeat_headers = TRUE))

  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 8.5, height = 11),
    page_margins = officer::page_mar(top = .8, bottom = .8, left = .85, right = .85)))
  doc <- .project_text(doc, title, font, 11, TRUE, after = 4)
  doc <- flextable::body_add_flextable(doc, ft)
  for (line in note_lines) doc <- .project_text(doc, line, font, font_size - 1, after = 2, before = 3)
  xml2::xml_remove(xml2::xml_find_all(doc$doc_obj$get(), ".//w:pPr/w:pBdr | .//w:rPr/w:u"))
  print(doc, target = paste0(file, ".docx"))
  write.csv(num, paste0(file, ".csv"), row.names = FALSE)

  if (png) {
    image <- flextable::flextable(body)
    if (!same_outcome) image <- flextable::add_header_row(image, values = c("", outcome), top = TRUE)
    image <- flextable::add_header_lines(image, values = title, top = TRUE)
    image <- flextable::add_footer_lines(image, values = note_lines)
    image <- style_table(image)
    image <- flextable::bold(image, i = 1, bold = TRUE, part = "header")
    image <- flextable::align(image, i = 1, align = "left", part = "header")
    image <- flextable::fontsize(image, size = font_size - 1, part = "footer")
    image <- flextable::align(image, align = "left", part = "footer")
    image <- flextable::padding(image, padding.top = 3, padding.bottom = 2, part = "footer")
    image <- flextable::line_spacing(image, space = 1.15, part = "all")
    # A picture needs a little more room next to its rules than Word does.
    image <- flextable::padding(image, i = seq(2, n_coef_lines, by = 2), padding.bottom = 2, part = "body")
    image <- flextable::padding(image, i = n_coef_lines, padding.bottom = 5, part = "body")
    if (nrow(body) > n_coef_lines) {
      image <- flextable::padding(image, i = n_coef_lines + 1, padding.top = 5, part = "body")
      image <- flextable::padding(image, i = nrow(body), padding.bottom = 5, part = "body")
    }
    image <- rules(image, 1)
    image <- flextable::width(image, j = 1, width = label_width)
    image <- flextable::width(image, j = 2:ncol(body), width = col_width)
    image <- flextable::bg(image, bg = "white", part = "all")
    saved <- tryCatch({
      flextable::save_as_image(image, path = paste0(file, ".png"), res = 300, expand = 0)
      TRUE
    }, error = function(e) FALSE)
    if (!saved) message("Could not save the picture ", file, ".png; the Word file is fine. ",
                        "To try again, run install.packages(\"ragg\").")
    png <- saved
  }
  message("Saved ", file, ".docx (Word table), ", if (png) paste0(file, ".png (picture), ") else "",
          "and ", file, ".csv (numbers)")
  invisible(num)
}
