# =====================================================================
# labreport/scripts/registry.R -- rendering helpers driven by the v2
# registry.
#
# Replaces scripts/generate_table.R. The differences that matter:
#
#  * Every helper takes a CONDITION or STUDY NAME. No page names an R
#    object, a column, or a trial count any more -- those live in the
#    registry, once.
#  * Nothing is loaded into the global environment as a side effect.
#    generate_table() used to load study_objects.RData behind the
#    scenes, which is why the old pages referenced variables that
#    appeared from nowhere.
#  * `links` is a named lookup, not a 53-element positional vector that
#    silently mislinks every row after an inserted study.
#  * One path to the data, resolved from the project root, instead of
#    the "data/" vs "../data/" branch keyed on an empty string.
# =====================================================================

# suppressWarnings: packages built under a newer patch release of R warn
# on load ("Paket 'ggplot2' wurde unter R Version 4.5.3 erstellt"), which
# would show up on every page that echoes its code.
suppressWarnings(suppressPackageStartupMessages({
  library(knitr)
  library(kableExtra)
  library(changeofevidence)
  # Pages add their own geoms to the returned plots (coe-measures.qmd),
  # and the old generate_table.R attached ggplot2 as a side effect of
  # library(tidyverse). Attach it explicitly instead of pulling in the
  # whole tidyverse for two functions.
  library(ggplot2)
}))

# ---------------------------------------------------------------------
# Project root and registry
# ---------------------------------------------------------------------
.lr_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in 1:4) {
    if (file.exists(file.path(d, "_quarto.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate the Quarto project root from ", getwd())
}

.reg_env <- new.env(parent = emptyenv())

registry <- function() {
  if (!is.null(.reg_env$r)) return(.reg_env$r)
  p <- file.path(.lr_root(), "data", "registry.rds")
  if (!file.exists(p))
    stop("registry.rds not found at ", p,
         "\n  Build it with: Rscript v2/run_all.R --coe && Rscript v2/R/export.R")
  .reg_env$r <- readRDS(p)
  .reg_env$r
}

# Resolve a study key to its condition names, or pass a condition
# through. Accepts either, so a page can say study = "monks" or name a
# single condition.
conditions_for <- function(x, r = registry()) {
  if (all(x %in% names(r$bf))) return(x)
  hit <- names(r$study)[r$study %in% x]
  if (!length(hit)) stop("Unknown study or condition: ", paste(x, collapse = ", "))
  hit
}

conditions_on_page <- function(page, r = registry()) {
  hit <- names(r$page)[r$page == page]
  if (!length(hit))
    stop("No registry conditions for page '", page, "'.\n",
         "  Known pages: ", paste(sort(unique(unname(r$page))), collapse = ", "))
  hit
}

# ---------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------
# SD and Var stay in the registry (the meta-analysis needs Var) but are
# not shown in the tables.
ST_HIDDEN <- c("SD", "Var")
COLS_ST <- c("Study", "Effect expected", "N", "Trials", "M",
             "Hits (%)", "t", "p", "ES", "BF", "Direction", "Year",
             "Lab/Online", "Prereg.", "Published")

# Relative path from the calling page back to the site root: study pages
# live one level down in studies/.
.site_prefix <- function() {
  here <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (identical(here, .lr_root())) "" else "../"
}

# Preregistration cell: a linked check mark for a preregistration, a
# linked "internal" for internal training preregistrations, a grey cross
# otherwise. Site-relative links (the internal PDFs) get the prefix.
.prereg_cell <- function(pr) {
  if (is.null(pr)) return("")
  url <- pr$url
  if (!is.null(url) && !grepl("^https?://", url)) url <- paste0(.site_prefix(), url)
  switch(pr$status,
    yes      = text_spec("✔", link = url, color = "darkgreen"),
    internal = text_spec("internal", link = url, color = "grey40"),
    no       = cell_spec("✖", color = "grey60"))
}

# ---------------------------------------------------------------------
# Publications
#
# Declared once, in the front matter of the study page, e.g.
#
#   publication:
#     key: dechamps2025        # entry in references.bib
#     url: https://...
#
# or, while not yet published,
#
#   publication:
#     status: in press
#
# A page that combines several studies of which only some are published
# lists one entry per publication and names the conditions it covers:
#
#   publication:
#     - studies: Sobjectivity 1
#       key: maier2022
#       url: https://...
#
# Conditions not named in any entry show a dash. Both the Published
# column of the tables and the Publication line under Resources
# (publication_link()) are built from it, and the short citation
# ("Dechamps et al., 2025") from the bibliography. Publications are not
# analysis data, so they do not live in the v2 registry: changing one
# needs only quarto render.
# ---------------------------------------------------------------------
.meta_cache <- new.env(parent = emptyenv())

.page_meta <- function(page) {
  if (is.null(page) || is.na(page)) return(list())
  if (!is.null(.meta_cache[[page]])) return(.meta_cache[[page]])
  f <- file.path(.lr_root(), "studies", paste0(page, ".qmd"))
  meta <- list()
  if (file.exists(f)) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    d <- which(trimws(l) == "---")
    if (length(d) >= 2 && d[1] == 1)
      meta <- yaml::yaml.load(paste(l[(d[1] + 1):(d[2] - 1)], collapse = "\n"))
  }
  .meta_cache[[page]] <- meta
  meta
}

# "Maier & Dechamps, 2018", "Jakob et al., 2020" from a references.bib key
.bib_label <- function(key) {
  bib <- paste(readLines(file.path(.lr_root(), "references.bib"), warn = FALSE,
                         encoding = "UTF-8"), collapse = "\n")
  start <- regexpr(paste0("@[A-Za-z]+\\{", key, ","), bib)
  if (start < 0) stop("Publication key '", key, "' not found in references.bib")
  entry <- substr(bib, start, start + 3000)
  entry <- sub("\n@.*", "", entry)               # stop at the next entry
  au <- regmatches(entry, regexpr("author\\s*=\\s*\\{[^}]*\\}", entry))
  yr <- regmatches(entry, regexpr("year\\s*=\\s*\\{?[0-9]{4}", entry))
  au <- sub("^author\\s*=\\s*\\{", "", sub("\\}$", "", au))
  surnames <- trimws(sub(",.*", "", strsplit(au, "\\s+and\\s+")[[1]]))
  surnames <- gsub("[{}]", "", surnames)
  who <- if (length(surnames) == 1) surnames
         else if (length(surnames) == 2) paste(surnames, collapse = " & ")
         else paste(surnames[1], "et al.")
  paste0(who, ", ", sub(".*([0-9]{4})$", "\\1", yr))
}

# The publication entries of a page as a list of entries, whether the
# front matter gives one mapping or a list of them.
.publications <- function(page) {
  pb <- .page_meta(page)$publication
  if (is.null(pb)) return(list())
  if (!is.null(names(pb))) list(pb) else pb
}

# Published cell: the short citation linked to the article, the status
# ("in press", "submitted", ...) in grey italics, or a dash.
.publication_cell <- function(page, study = NULL) {
  pbs <- Filter(function(e) is.null(e$studies) || study %in% e$studies,
                .publications(page))
  if (length(pbs) == 0) return(cell_spec("–", color = "grey60"))
  pb <- pbs[[1]]
  if (!is.null(pb$url) && !is.null(pb$key))
    return(text_spec(.bib_label(pb$key), link = pb$url))
  if (!is.null(pb$url)) return(text_spec("Publication", link = pb$url))
  cell_spec(pb$status, color = "grey40", italic = TRUE)
}

# The Publication line under Resources, as inline R on a study page:
#   -   `{r} publication_link()`
# Wrapped in I(): Quarto's `{r} ...` inline syntax escapes markdown in the
# result unless it is marked as-is (knitr::asis_output() does not help
# there), which left "[Publication](...)" as literal text.
publication_link <- function(page = NULL) {
  md <- vapply(.publications(page %||% .current_page()), function(pb) {
    label <- if (is.null(pb$studies)) "Publication"
             else paste("Publication of", paste(pb$studies, collapse = " & "))
    if (!is.null(pb$url))
      paste0("[", label, "](", pb$url, ")",
             if (!is.null(pb$key)) paste0(" [@", pb$key, "]") else "")
    else paste0(label, " (", pb$status, ")")
  }, "")
  I(paste(md, collapse = ", "))
}

.mark <- function(x, yes, no, col_yes, col_no)
  cell_spec(ifelse(x, yes, no), color = ifelse(x, col_yes, col_no))

# With no argument, every helper below resolves to the conditions of the
# page it is called from -- the .qmd filename IS the registry `page`.
# That is what lets each study page shrink to bare calls.
study_table <- function(x = NULL, page = NULL, linked = FALSE, all = FALSE,
                        r = registry()) {
  rows <- if (isTRUE(all)) r$st$Study
          else if (!is.null(page)) conditions_on_page(page, r)
          else if (!is.null(x)) conditions_for(x, r)
          else conditions_on_page(.current_page(), r)

  ft <- r$st[match(rows, r$st$Study), setdiff(names(r$st), ST_HIDDEN),
             drop = FALSE]
  # The full overview lists studies chronologically. order() is stable,
  # so conditions of the same year keep their registry order.
  if (isTRUE(all)) ft <- ft[order(ft$Year), , drop = FALSE]
  # Subsetting keeps the original row indices, which kbl() would render
  # as an extra unnamed column. The old dplyr filter() reset them.
  rownames(ft) <- NULL
  ft[] <- lapply(ft, function(cl) if (is.numeric(cl)) round(cl, 2) else cl)
  ft$`Hits (%)` <- formattable::color_tile("coral2", "cornflowerblue")(ft$`Hits (%)`)
  ft$p <- cell_spec(ft$p, bold = ft$p < 0.05)
  ft$Experimental <- .mark(ft$Experimental, "✔", "✖",
                           "darkgreen", "red")
  ft$Labstudy <- ifelse(ft$Labstudy, "\U0001F3E0", "\U0001F30D")
  ft$Prereg <- vapply(ft$Study, function(n) .prereg_cell(r$prereg[[n]]), "")
  ft$Published <- vapply(ft$Study, function(n) .publication_cell(r$page[[n]], n), "")
  if (linked) ft$Study <- text_spec(ft$Study, link = unname(r$links[ft$Study]))

  kbl(ft, escape = FALSE, col.names = COLS_ST,
      align = paste0("l", strrep("c", length(COLS_ST) - 1))) |>
    kable_classic("hover") |>
    kable_styling(fixed_thead = TRUE, html_font = "Arial",
                  bootstrap_options = c("striped", "hover", "condensed"))
}

# The raw Energy and FFT amplitude sums are not shown: both scale with N
# (Relaxation's energy is -6614, Monks' 6.4) and say nothing without the
# null they are compared against -- their p-values carry the content.
# MaxBF stays, as the one raw value that reads on its own.
COLS_ST2 <- c("Study", "Effect expected", "N", "CoE p", "Value (n)", "p",
              "p", "p", "Direction", "Lab/Online")

coe_table <- function(r = registry()) {
  ft <- r$st2[, c("Study", "Experimental", "N", "COE p", "MaxBF", "MaxBF p",
                  "Energy p", "FFT p", "Direction", "Labstudy")]
  rownames(ft) <- NULL
  ft[] <- lapply(ft, function(cl) if (is.numeric(cl)) round(cl, 3) else cl)
  for (cl in c("COE p", "MaxBF p", "Energy p", "FFT p"))
    ft[[cl]] <- cell_spec(ft[[cl]], bold = ft[[cl]] < 0.05)
  ft$Experimental <- .mark(ft$Experimental, "✔", "✖",
                           "darkgreen", "red")
  ft$Labstudy <- ifelse(ft$Labstudy, "\U0001F3E0", "\U0001F30D")
  ft$Study <- text_spec(ft$Study, link = unname(r$links[ft$Study]))

  x <- kbl(ft, escape = FALSE, col.names = COLS_ST2, align = "lccccccccc") |>
    kable_classic("hover", fixed_thead = TRUE) |>
    add_header_above(c(" " = 3, "Combined" = 1, "Max BF" = 2,
                       "BF Energy" = 1, "FFT Amp.sum" = 1, " " = 2)) |>
    footnote(general = paste(
      "CoE p is the harmonic mean of the three component p-values. All",
      "p-values are the share of 1,000 null simulations with an equal or",
      "more extreme value."),
      general_title = "Note:", footnote_as_chunk = TRUE)

  # kableExtra writes colspan="100%" into the footer row. That is not
  # valid HTML, and Quarto's post-processing drops it, which squeezes
  # the note into the first column. Use the real column count.
  x[1] <- gsub('colspan="100%"', sprintf('colspan="%d"', ncol(ft)), x[1],
               fixed = TRUE)
  x
}

# Keyed by condition: sobjectivity.qmd shows one table per study on a
# single page. With no argument it takes the page's first condition.
demographics_table <- function(x = NULL, page = NULL, r = registry()) {
  cond <- if (!is.null(x)) conditions_for(x, r)[1]
          else .rows_arg(NULL, page, r)[1]
  d <- r$demographics[[cond]]
  if (is.null(d)) return(invisible(NULL))
  if (isTRUE(d$none)) return(cat(d$note %||% "Demographic Data Not Available"))
  k <- kable(d$rows, format = "html",
             col.names = c("Characteristic", "Count/Statistic")) |>
    kable_styling(full_width = FALSE)
  if (!is.null(d$note))
    k <- footnote(k, general = d$note, general_title = "Note:",
                  footnote_as_chunk = TRUE)
  k
}

# The page name is the source filename, which is also the registry
# `page`. Quarto runs the knitr engine on a temporary `.rmarkdown`
# copy, so current_input() reports "allgood.rmarkdown" rather than
# "allgood.qmd" -- strip whatever extension it carries.
.current_page <- function() {
  f <- tryCatch(knitr::current_input(), error = function(e) NULL)
  if (is.null(f) || !nzchar(f))
    stop("Not rendering a document -- pass page = \"...\" or a condition name")
  sub("\\.[^.]*$", "", basename(f))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------
.labels_for <- function(rows) {
  # "Monks T1 Exp" on the monks page reads better as "T1 Exp"
  if (length(rows) < 2) return(NULL)
  pre <- Reduce(function(a, b) {
    n <- min(nchar(a), nchar(b))
    i <- 0
    while (i < n && substr(a, 1, i + 1) == substr(b, 1, i + 1)) i <- i + 1
    substr(a, 1, i)
  }, rows)
  out <- trimws(substring(rows, nchar(pre) + 1))
  if (any(out == "")) rows else out
}

.rows_arg <- function(x, page, r) {
  if (!is.null(page)) conditions_on_page(page, r)
  else if (!is.null(x)) conditions_for(x, r)
  else conditions_on_page(.current_page(), r)
}

bfplot <- function(x = NULL, page = NULL, labels = NULL, r = registry()) {
  rows <- .rows_arg(x, page, r)
  v <- r$bf[rows]
  names(v) <- labels %||% .labels_for(rows)
  if (length(v) == 1) plotbf(v[[1]]) else plotbf(v)
}

# The random walk is declared in the registry (column, offset, sign,
# n_bits), so Baseline, Stories and Games need no bespoke page code.
rwplot <- function(x = NULL, page = NULL, labels = NULL, r = registry()) {
  rows <- .rows_arg(x, page, r)
  v <- r$rw[rows]
  v <- v[!vapply(v, is.null, TRUE)]
  if (!length(v)) return(invisible(NULL))
  nb <- vapply(v, function(z) attr(z, "n_bits") %||% NA_real_, 0)
  pp <- vapply(v, function(z) attr(z, "p") %||% 0.5, 0)

  # Walks may share one plot when they share one null band. With n_bits
  # given, plotrw draws z * sqrt(n_bits * p * (1 - p) * n), symmetric
  # about zero -- so it depends on p only through p*(1-p). The two
  # Baseline 1 arms (p = .6 and p = .4) therefore have the SAME band
  # (20 * .24 either way) and legitimately share a plot.
  band <- ifelse(is.na(nb), pp, nb * pp * (1 - pp))
  if (length(unique(signif(band, 12))) > 1)
    stop("Cannot overlay random walks with different null bands: ",
         paste(sprintf("%s (n_bits=%s, p=%s)", names(v), nb, pp),
               collapse = "; "))

  names(v) <- labels %||% .labels_for(names(v))
  args <- list(if (length(v) == 1) v[[1]] else v, p = pp[[1]])
  if (!is.na(nb[[1]])) args$n_bits <- nb[[1]]
  do.call(plotrw, args)
}

# ---------------------------------------------------------------------
# Change of evidence
#
# plot.coe() draws the null band and the ridgeline distributions from
# sims.df. Those files live outside the Quarto project and are large, so
# they are loaded lazily and cached per session. If they are not
# reachable -- a checkout without the sims/ folder -- plot.coe()
# degrades to its p-value bar chart, and the site still renders.
# ---------------------------------------------------------------------
.sims_cache <- new.env(parent = emptyenv())

.load_sims_for <- function(cond, r) {
  f <- r$sims_file[[cond]]
  if (is.null(f)) return(NULL)
  p <- file.path(dirname(.lr_root()), f)
  if (!file.exists(p)) {
    warning("Simulation set not found: ", p,
            " -- plotting without the null distribution", call. = FALSE)
    return(NULL)
  }
  if (is.null(.sims_cache[[p]])) .sims_cache[[p]] <- readRDS(p)
  full <- .sims_cache[[p]]

  # Trim to the length of THIS study's BF series before plotting.
  # coe() does this itself (via simredo()), so the p-values were always
  # right -- but plot.coe() uses sims.df as given. With the untrimmed
  # 12571-trial sims100 behind Loving Kind (N = 171), the BF panel
  # stretched to 12571 and the ridgeline panel computed MaxBF, energy
  # and FFT amplitude over the full length: for Monks T1 Exp the red
  # marker sat at the 64.6th percentile while its own label (p = .139)
  # implies the 86.1th.
  n <- sum(!is.na(r$bf[[cond]]))
  if (sum(full$simid == full$simid[1]) > n)
    suppressMessages(simredo(full, n, rw = FALSE))
  else full
}

coeplot <- function(x = NULL, page = NULL, sims = TRUE, r = registry()) {
  rows <- .rows_arg(x, page, r)
  for (cond in rows) {
    obj <- r$coe[[cond]]
    if (is.null(obj)) { .coeplot_descriptive(r$bf[[cond]], cond); next }
    plot(obj, sims.df = if (isTRUE(sims)) .load_sims_for(cond, r) else NULL)
  }
  invisible(NULL)
}

# Games has a variable trial count, so no null simulation matches it and
# it carries no coe() result. The curve itself is still worth showing --
# this reproduces the three descriptive panels the page had before,
# without the p-values it never had a valid null for.
.coeplot_descriptive <- function(bf, cond) {
  if (is.null(bf)) { warning("No BF series for ", cond, call. = FALSE); return(invisible(NULL)) }
  n <- seq_along(bf)
  p1 <- plotbf(bf) +
    geom_point(aes(x = which.max(bf), y = max(bf)), color = "red", size = 3) +
    ggtitle(sprintf("Maximum BF (%.2f at n = %d)", max(bf), which.max(bf)))
  p2 <- plotbf(bf) +
    geom_ribbon(aes(x = n, ymin = pmin(bf, 1), ymax = 1), fill = "red", alpha = 0.3) +
    geom_ribbon(aes(x = n, ymin = 1, ymax = pmax(bf, 1)), fill = "green", alpha = 0.3) +
    ggtitle("Energy of BF") + theme(axis.title.y = element_blank())
  p3 <- plotfft(fftcreate(bf)) + ggtitle("FFT Test")
  thm <- theme(text = element_text(size = 10))
  print((patchwork::wrap_plots(list(p1 + thm, p2 + thm)) / (p3 + thm)))
  cat("\n*No empirical p-values: this study has a variable trial count,",
      "so no matching null simulation exists.*\n")
  invisible(NULL)
}

drop_sims_cache <- function() rm(list = ls(.sims_cache), envir = .sims_cache)
