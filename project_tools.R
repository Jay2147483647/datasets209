# POLS 209: supplied table functions for the October 2 project-data lab.
# source("project_tools.R") runs this file, which defines make_summary() and
# export_summary(). The calculations below use base R functions you know.

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
