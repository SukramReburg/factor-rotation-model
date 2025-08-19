#' Check Package Presence and Minimum Versions, Then Report
#'
#' Checks whether required packages are installed and whether their installed
#' versions meet (or exceed) a specified minimum version. Prints two tables
#' to the console:
#'
#' 1) Missing packages (not installed)
#' 2) Packages with versions below the required minimum
#'
#' This function does **not** install or update packages; it only reports.
#'
#' @param pkg_list A named character vector where names are package names and
#'   values are the minimum required versions, e.g.
#'   `c(dplyr = "1.1.0", data.table = "1.14.0")`.
#' @param quiet Logical; if `FALSE` (default) prints tables to the console.
#'   If `TRUE`, nothing is printed and results are returned invisibly.
#'
#' @return Invisibly returns a list with two data frames:
#'   \describe{
#'     \item{missing}{Data frame with column `package` listing not-installed pkgs.}
#'     \item{needs_update}{Data frame with columns `package`, `installed_version`,
#'       and `required_version` for pkgs with installed version below required.}
#'   }
#'
#' @examples
#' \dontrun{
#' req <- c(dplyr = "1.1.0", data.table = "1.15.0", ggplot2 = "3.4.0")
#' res <- get_packages(req)
#' # Access data frames programmatically:
#' res$missing
#' res$needs_update
#' }
#'
#' @export
check_packages <- function(pkg_list, quiet = FALSE) {
  stopifnot(is.character(pkg_list), !is.null(names(pkg_list)))
  pkgs <- names(pkg_list)
  
  is_installed <- vapply(pkgs, function(p)
    requireNamespace(p, quietly = TRUE), logical(1))
  
  # Missing packages table
  missing_tbl <- data.frame(package = pkgs[!is_installed],
                            row.names = NULL,
                            check.names = FALSE)
  
  # For installed: check versions against required
  needs_update_tbl <- NULL
  if (any(is_installed)) {
    installed_versions <- vapply(pkgs[is_installed], function(p) {
      v <- tryCatch(
        utils::packageVersion(p),
        error = function(e)
          NA
      )
      if (is.na(v[1]))
        NA_character_
      else
        as.character(v)
    }, character(1))
    
    required_versions <- unname(pkg_list[is_installed])
    
    cmp <- mapply(utils::compareVersion,
                  installed_versions,
                  required_versions,
                  USE.NAMES = FALSE)
    
    idx_below <- cmp < 0 | is.na(cmp)
    if (any(idx_below)) {
      needs_update_tbl <- data.frame(
        package = pkgs[is_installed][idx_below],
        installed_version = installed_versions[idx_below],
        required_version  = required_versions[idx_below],
        row.names = NULL,
        check.names = FALSE
      )
    } else {
      needs_update_tbl <- data.frame(
        package = character(0),
        installed_version = character(0),
        required_version = character(0),
        check.names = FALSE
      )
    }
  } else {
    needs_update_tbl <- data.frame(
      package = character(0),
      installed_version = character(0),
      required_version = character(0),
      check.names = FALSE
    )
  }
  
  if (!quiet) {
    # Print Missing first
    cat("\n=== Missing packages (not installed) ===\n")
    if (nrow(missing_tbl) == 0) {
      cat("None 🎉\n")
    } else {
      print(missing_tbl, row.names = FALSE)
    }
    
    # Then packages needing update
    cat("\n=== Packages below required version ===\n")
    if (nrow(needs_update_tbl) == 0) {
      cat("None 🎉\n")
    } else {
      print(needs_update_tbl, row.names = FALSE)
    }
    cat("\n")
  }
  
  invisible(list(missing = missing_tbl, needs_update = needs_update_tbl))
}


#' Load Installed Packages into the Session
#'
#' Loads a set of packages (given with minimum versions in a named vector)
#' into the current R session. The version numbers are ignored; only the
#' package names are used for loading. If a package is not installed, it
#' will be skipped with a warning.
#'
#' @param pkg_list A named character vector where names are package names
#'   and values are (ignored) version strings, e.g.
#'   `c(data.table = "1.15.0", openxlsx2 = "1.18")`.
#' @param quietly Logical; if `TRUE` suppresses startup messages from `library()`.
#'
#' @return Invisibly returns a vector of successfully loaded package names.
#'
#' @examples
#' \dontrun{
#' pkgs <- c(data.table = "1.15.0", openxlsx2 = "1.18")
#' load_packages(pkgs)
#' }
#'
#' @export
load_packages <- function(pkg_list, quietly = FALSE) {
  stopifnot(is.character(pkg_list), !is.null(names(pkg_list)))
  
  pkgs <- names(pkg_list)
  loaded <- character()
  
  for (pkg in pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(
        pkg,
        character.only = TRUE,
        quietly = quietly,
        warn.conflicts = FALSE
      )
      loaded <- c(loaded, pkg)
    } else {
      warning(sprintf("Package '%s' is not installed and cannot be loaded.", pkg))
    }
  }
  
  invisible(loaded)
}

#' Add Month Ultimo Column to Data Table
#'
#' Given columns `year` and `month` in a data.table, this function creates a new
#' column `month_ultimo` containing the last day of each corresponding month.
#'
#' @param dt A data.table with integer columns `year` and `month`.
#' @param colname Character string giving the name of the new column.
#'   Default is `"month_ultimo"`.
#'
#' @return The input data.table with an added column containing Date values.
#'   The function modifies `dt` by reference.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(year = c(2000, 2025), month = c(2, 4))
#' add_month_ultimo(dt)
#' }
#'
#' @export
add_month_ultimo <- function(dt, colname = "month_ultimo") {
  stopifnot("year" %in% names(dt), "month" %in% names(dt))
  
  dt[, (colname) := as.Date(paste(year, month + 1, 1, sep = "-"), format = "%Y-%m-%d") - 1L]
  
  invisible(dt)
}

#' Get Month Ultimo (Last Day of Month) for Dates
#'
#' Vectorised function returning the last calendar day of the month
#' (month ultimo) for each input `Date`.
#'
#' @param dates A vector of class `Date`.
#'
#' @return A `Date` vector of the same length as `dates`.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(date = as.Date(c("2000-01-15", "2000-02-02", "2025-04-10")))
#' dt[, date_ultimo := month_ultimo(date)]
#' }
#'
#' @export
month_ultimo <- function(dates) {
  stopifnot(inherits(dates, "Date"))
  # First day of current month
  first_this <- as.Date(cut(dates, "month"))
  # First day of next month (add >=31 days guarantees next month, then cut to month)
  first_next <- as.Date(cut(first_this + 31, "month"))
  # Ultimo is the day before the first of next month
  first_next - 1L
}


plot_trend_fits <- function(y,
                            fits,
                            df,
                            df_val,
                            main = "L1 Trend Filtering: Different Smoothness (df)",
                            ylab = "Macro indicator",
                            xlab = "Time (index)",
                            palette = "Viridis") {
  stopifnot(is.numeric(y), is.list(fits), length(fits) == length(df))
  n <- length(df)
  
  # order by increasing df so colors go light -> dark
  ord <- order(df)
  df_ord   <- df[ord]
  fits_ord <- fits[ord]
  
  # colors (light -> dark with increasing df)
  cols <- hcl.colors(n, palette = palette, rev = FALSE)
  
  # which one to highlight
  hit_idx <- which(df_ord == df_val)
  hit_idx <- if (length(hit_idx))
    hit_idx[1]
  else
    NA_integer_
  
  # base plot (extra space on the right for legend)
  op <- par(
    mar = c(4, 4, 3, 20),
    xaxs = "i",
    yaxs = "i",
    xpd = NA
  )
  on.exit(par(op), add = TRUE)
  
  plot(
    y,
    type = "l",
    lwd = 1,
    col = "grey40",
    xlab = xlab,
    ylab = ylab,
    main = main
  )
  
  # overlay fits
  for (i in seq_len(n)) {
    lines(fits_ord[[i]], lwd = if (!is.na(hit_idx) &&
                                   i == hit_idx)
      3
      else
        2, col = cols[i])
  }
  if (!is.na(hit_idx)) {
    lines(fits_ord[[hit_idx]], lwd = 4, col = cols[hit_idx])
  }
  
  # legend outside on the right
  leg_labels <- c("raw", paste0("df = ", df_ord))
  leg_cols   <- c("grey40", cols)
  leg_lwd    <- c(1, ifelse(seq_len(n) == hit_idx, 3, 2))
  
  legend(
    "topright",
    inset = c(-0.7, 0),
    xpd = NA,
    bty = "n",
    legend = leg_labels,
    col = leg_cols,
    lty = 1,
    lwd = leg_lwd,
    title = "Series / df"
  )
  invisible(NULL)
}

#' Fill edge NAs of a trend by linear interpolation to raw endpoints
#'
#' Replaces leading and trailing NAs in a smoothed/trend vector by a straight
#' line to the first/last available trend point using the raw series values
#' y[1] and y[n] as anchors.
#'
#' @param y Numeric vector of raw values (length n).
#' @param trend Numeric vector (length n) with possible NAs at the edges.
#'
#' @return Numeric vector `trend` with edge NAs filled; interior NAs are unchanged.
#' @examples
#' \dontrun{
#' y <- rnorm(100)
#' tr <- rep(NA_real_, 100); tr[6:95] <- seq(0,1,length.out=90)
#' tr_filled <- fill_trend_edges(y, tr)
#' }
#' @export
fill_trend_edges <- function(y, trend) {
  stopifnot(is.numeric(y), is.numeric(trend), length(y) == length(trend))
  n <- length(y)
  if (n == 0L)
    return(trend)
  
  # first and last non-NA indices in trend
  idx <- which(!is.na(trend))
  if (!length(idx))
    return(trend)  # nothing to anchor to — leave as is
  
  i0 <- idx[1]                 # first non-NA
  i1 <- idx[length(idx)]       # last non-NA
  
  # fill leading NAs: from x=1 (y[1]) to x=i0 (trend[i0])
  if (i0 > 1L) {
    x  <- 1:i0
    y0 <- y[1]
    y1 <- trend[i0]
    trend[1:(i0 - 1)] <- y0 + (y1 - y0) * ((x[1:(i0 - 1)] - 1) / (i0 - 1))
  }
  
  # fill trailing NAs: from x=i1 (trend[i1]) to x=n (y[n])
  if (i1 < n) {
    x0 <- i1
    xN <- n
    y0 <- trend[i1]
    yN <- y[n]
    k  <- (i1 + 1):n
    trend[(i1 + 1):n] <- y0 + (yN - y0) * ((k - x0) / (xN - x0))
  }
  
  trend
}

library(data.table)

momentum_pillar <- function(dt,
                            styles = c("msci_usa_value", "msci_usa_minvol", "msci_usa_quality"),
                            horizons = c(
                              monthly = 1,
                              quarterly = 3,
                              semiannual = 6,
                              annual = 12
                            ),
                            weights = c(
                              monthly = 0.2,
                              quarterly = 0.3,
                              semiannual = 0.3,
                              annual = 0.2
                            )) {
  stopifnot(is.data.table(dt))
  
  # trailing returns with skip-month rule
  for (nm in names(horizons)) {
    h <- horizons[[nm]]
    for (s in styles) {
      if (h == 1) {
        # monthly skip: compare t-2 to t-1
        dt[, paste0(s, "_ret_", nm) := shift(get(s), 1) / shift(get(s), 2) - 1]
      } else {
        # horizon skip: compare t-(h+1) to t-1
        dt[, paste0(s, "_ret_", nm) :=
             shift(get(s), 1) / shift(get(s), h + 1) - 1]
      }
    }
  }
  
  for (nm in names(horizons)) {
    ret_cols <- paste0(styles, "_ret_", nm)
    z_cols   <- paste0(styles, "_z_", nm)
    
    dt[, (z_cols) := {
      x <- unlist(.SD)                 # returns for all styles at this date
      mu <- mean(x, na.rm = TRUE)
      sdv <- sd(x, na.rm = TRUE)
      as.list((x - mu) / sdv)
    }, by = date, .SDcols = ret_cols]
  }
  
  
  return(dt)
}

#' Build Composite Momentum Scores from Horizon Z-Scores
#'
#' Aggregates horizon-specific z-scores into a composite momentum score per style,
#' using named horizons (e.g., monthly, quarterly, semiannual, annual) and weights.
#' Assumes columns like `<style>_z_<horizon>` already exist in `dt`.
#'
#' @param dt A data.table with z-score columns for each style and horizon.
#' @param styles Character vector of style columns (base names), e.g.
#'   c("msci_usa_value","msci_usa_minvol","msci_usa_quality").
#' @param horizons Named integer vector mapping horizon names to months,
#'   e.g. c(monthly=1, quarterly=3, semiannual=6, annual=12).
#'   Only the NAMES are used for column suffixes here.
#' @param weights Named numeric vector of aggregation weights whose names match
#'   \code{names(horizons)}. If not summing to 1 they are normalized.
#' @param make_leader Logical; if TRUE adds \code{mom_leader} column.
#'
#' @return The modified data.table (by reference) with `<style>_mom` columns
#'   (and optionally `mom_leader`).
#'
#' @examples
#' \dontrun{
#' styles   <- c("msci_usa_value","msci_usa_minvol","msci_usa_quality")
#' horizons <- c(monthly=1, quarterly=3, semiannual=6, annual=12)
#' w        <- c(monthly=.2, quarterly=.3, semiannual=.3, annual=.2)
#' dt_base <- build_momentum_composite(dt_base, styles, horizons, w, make_leader=TRUE)
#' }
#' @export
build_momentum_composite <- function(dt,
                                     styles = c(
                                       "msci_usa_value", 
                                       "msci_usa_minvol", 
                                       "msci_usa_quality"),
                                     horizons = c(
                                       monthly = 1,
                                       quarterly = 3,
                                       semiannual = 6,
                                       annual = 12
                                     ),
                                     weights  = c(
                                       monthly = .2,
                                       quarterly = .3,
                                       semiannual = .3,
                                       annual = .2
                                     ),
                                     make_leader = TRUE) {
  stopifnot(data.table::is.data.table(dt))
  # Ensure weight names match horizon names
  if (!all(names(horizons) %in% names(weights))) {
    stop("All horizon names must be present in 'weights'.")
  }
  # Normalize weights to sum to 1 (safe even if already sums to 1)
  weights <- weights[names(horizons)]
  sw <- sum(weights)
  if (sw == 0)
    stop("'weights' must not all be zero.")
  weights <- weights / sw
  
  # Build composite per style
  for (s in styles) {
    z_cols <- paste0(s, "_z_", names(horizons))
    missing_cols <- setdiff(z_cols, names(dt))
    if (length(missing_cols)) {
      stop(sprintf(
        "Missing z-score columns for style '%s': %s",
        s,
        paste(missing_cols, collapse = ", ")
      ))
    }
    dt[, (paste0(s, "_mom")) :=
         Reduce(`+`, Map(function(col, w)
           w * get(col), z_cols, as.numeric(weights)))]
  }
  
  if (make_leader) {
    comp_cols <- paste0(styles, "_mom")
    dt[, mom_leader := styles[as.integer(apply(.SD, 1L, which.max))],
       .SDcols = comp_cols]
  }
  return(dt)
}