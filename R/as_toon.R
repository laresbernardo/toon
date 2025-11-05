####################################################################
#' Convert R Objects to TOON (Token-Oriented Object Notation)
#'
#' Converts a variety of R objects, including named lists (objects),
#' unnamed lists, vectors (arrays), and data frames, into a character string
#' formatted according to TOON (Token-Oriented Object Notation) specification.
#'
#' TOON is designed as a highly human-readable, lightweight data serialization
#' format that supports nested structures.
#'
#' @param x The R object to be converted. Supported types include:
#'   \itemize{
#'     \item Named lists
#'     \item Unnamed lists and atomic vectors
#'     \item Data frames
#'     \item Primitive types (numeric, character, logical, NULL)
#'   }
#' @param ... Additional arguments passed to specific S3 methods (e.g., internal
#'   indentation parameters).
#' @return A character vector of class \code{toon}, containing the fully 
#' formatted TOON string.
#' @examples
#' # 1. Simple Object (Named List)
#' config_obj <- list(
#'   version = 1.0,
#'   is_active = TRUE,
#'   user_id = 99
#' )
#' as_toon(config_obj)
#'
#' # 2. Expanded Array (Unnamed List)
#' items <- list(
#'   "apple",
#'   list(color = "red", weight = 150),
#'   "banana"
#' )
#' as_toon(items)
#'
#' # 3. Data Frame
#' df <- data.frame(
#'   time = c(9.1, 15.4),
#'   action = c("login", "update"),
#'   success = c(TRUE, FALSE)
#' )
#' as_toon(df)
#' @export
as_toon <- function(x, ...) {
  s <- as_toon_string(x, ...)
  structure(s, class = "toon")
}

as_toon_string <- function(x, ...) {
  is_multi_atomic <- is.atomic(x) && length(x) > 1 && !is.list(x)
  if (is_multi_atomic) {
    return(dispatch_to_list(x, ...))
  }
  
  UseMethod("as_toon_string")
}

#' @exportS3Method as_toon_string NULL
as_toon_string.NULL <- function(x, ...) {
  "null"
}

#' @exportS3Method as_toon_string default
as_toon_string.default <- function(x, ...) {
  warning("No explicit 'as_toon' method for class: ", class(x)[1], ". Attempting character conversion.")
  as_toon_string(as.character(x), ...)
}

make_indent <- function(n) {
  if (n < 0) n <- 0
  paste(rep("  ", n), collapse = "")
}

quote_toon_string <- function(s, force_quote = FALSE) {
  if (is.na(s)) return("null")
  
  if (!force_quote) {
    keywords <- c("null", "true", "false", "Inf", "-Inf", "NaN")
    
    is_keyword <- s %in% keywords
    is_bare_number <- grepl("^[0-9]+(\\.[0-9]+)?$", s)
    
    if (is_keyword || is_bare_number) return(s)
  }
  
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\"", "\\\\\"", s)
  return(paste0('"', s, '"'))
}

format_special_numeric <- function(x) {
  if (is.na(x)) return("null")
  if (is.infinite(x)) return("null")
  if (is.nan(x)) return("null")
  as.character(x)
}

dispatch_to_list <- function(x, ...) {
  x_list <- unclass(as.list(x))
  class(x_list) <- "list"
  return(as_toon_string(x_list, ...))
}

#' @exportS3Method as_toon_string logical
as_toon_string.logical <- function(x, .indent = 0, ...) {
  ifelse(is.na(x), "null", tolower(as.character(x)))
}

#' @exportS3Method as_toon_string numeric
as_toon_string.numeric <- function(x, .indent = 0, ...) {
  format_special_numeric(x)
}

#' @exportS3Method as_toon_string integer
as_toon_string.integer <- function(x, .indent = 0, ...) {
  as_toon_string.numeric(x, .indent, ...)
}

#' @exportS3Method as_toon_string character
as_toon_string.character <- function(x, .indent = 0, ...) {
  quote_toon_string(x, force_quote = TRUE)
}

#' @exportS3Method as_toon_string data.frame
as_toon_string.data.frame <- function(x, .indent = 0, ...) {
  if (nrow(x) == 0) return("[]")
  
  # Standard Logic: Header is at .indent, Rows are at .indent + 1
  indent_str <- make_indent(.indent)
  row_indent_level <- if (.indent >= 2) .indent else .indent + 1
  row_indent_str <- make_indent(row_indent_level)
  
  cols <- paste(names(x), collapse = ",")
  header <- sprintf("%s[%d]{%s}:", indent_str, nrow(x), cols)
  col_types <- sapply(x, class)
  
  format_cell <- function(cell, col_class) {
    cell_type <- switch(col_class[1], "character" = "character", "factor" = "factor", "numeric" = "numeric", "integer" = "integer", "logical" = "logical", "character")
    if (cell_type == "character" || cell_type == "factor") quote_toon_string(as.character(cell), force_quote = TRUE)
    else if (cell_type == "numeric" || cell_type == "integer") format_special_numeric(cell)
    else if (cell_type == "logical") ifelse(is.na(cell), "null", tolower(as.character(cell)))
    else quote_toon_string(as.character(cell), force_quote = TRUE)
  }
  
  rows <- vapply(seq_len(nrow(x)), function(i) {
    row_cells <- mapply(format_cell, x[i, ], col_types, SIMPLIFY = FALSE)
    paste0(row_indent_str, paste(row_cells, collapse = ","))
  }, character(1))
  
  return(paste(header, paste(rows, collapse = "\n"), sep = "\n"))
}

#' @exportS3Method as_toon_string list
as_toon_string.list <- function(x, .indent = 0, ...) {
  indent_str <- make_indent(.indent)
  
  if (length(x) == 0) return("[]")
  
  nms <- names(x)
  
  # DELETE THE recursive_toon_call HELPER FUNCTION HERE
  
  format_primitive_element_local <- function(v) {
    if (is.character(v)) return(quote_toon_string(v, force_quote = TRUE))
    if (is.logical(v)) return(ifelse(is.na(v), "null", tolower(as.character(v))))
    if (is.numeric(v) || is.integer(v)) return(format_special_numeric(v))
    return("null")
  }
  
  # Case 1: Named List (TOON Object)
  if (!is.null(nms) && all(nzchar(nms))) {
    lines <- character(length(x))
    obj_indent_str <- make_indent(.indent)
    for (i in seq_along(x)) {
      key <- nms[i]
      val <- x[[i]]
      
      is_complex <- is.list(val) && length(val) > 0 || is.data.frame(val)
      
      if (is_complex) {
        # Call complex multi-line value with current .indent, not .indent + 1.
        # This resolves the duplicate .indent issue by allowing the dispatch 
        # to find its arguments cleanly.
        formatted_val <- as_toon_string(val, .indent = .indent, ...) 
        lines[i] <- sprintf("%s%s:\n%s", obj_indent_str, key, formatted_val)
      } else {
        # Primitives still need + 1 indent for the inline value.
        formatted_val <- as_toon_string(val, .indent = .indent + 1, ...) 
        lines[i] <- sprintf("%s%s: %s", obj_indent_str, key, formatted_val)
      }
    }
    return(paste(lines, collapse = "\n"))
  }
  
  # Case 2: Unnamed List (TOON Array) - Primitives
  is_primitive_value <- function(v) is.atomic(v) && length(v) == 1 && !is.list(v)
  all_primitive <- all(sapply(x, is_primitive_value))
  
  if (all_primitive) {
    vals_list <- sapply(x, format_primitive_element_local, simplify = FALSE)
    vals_string <- paste(unlist(vals_list), collapse = ",")
    return(sprintf("[%d]: %s", length(x), vals_string))
  }
  
  # Case 3: Mixed or Complex Unnamed List (TOON Expanded Array)
  header <- sprintf("%s[%d]:", indent_str, length(x))
  items <- character(length(x))
  item_indent_str <- make_indent(.indent + 1)
  
  for (i in seq_along(x)) {
    val <- x[[i]]
    
    is_complex_value <- is.list(val) && !is.null(names(val)) || is.data.frame(val)
    
    if (is_complex_value) {
      # Recurse with .indent + 2 (Standard for nested object/array content)
      formatted_val <- as_toon_string(val, .indent = .indent + 2, ...)      
      lines_of_val <- strsplit(formatted_val, "\n")[[1]]
      
      # Trim the line and prepend the array marker. 
      trimmed_line <- trimws(lines_of_val[1], which = "left")
      lines_of_val <- strsplit(formatted_val, "\n")[[1]]
      
      # FIX: Replace the exact leading indent (make_indent(.indent + 2)) 
      # with the array marker (item_indent_str + "- ").
      # This is the ONLY reliable way to strip *only* the recursion-added indent.
      
      leading_indent_to_replace <- make_indent(.indent + 2)
      
      # Substitute the exact indent used for recursion with the desired array prefix
      lines_of_val[1] <- sub(
        pattern = leading_indent_to_replace, 
        replacement = paste0(item_indent_str, "- "), 
        x = lines_of_val[1], 
        fixed = TRUE
      )
      
      items[i] <- paste(lines_of_val, collapse = "\n")
      
    } else {
      # Primitives and inline primitive arrays. Recurse with .indent + 1.
      formatted_val <- as_toon_string(val, .indent = .indent + 1, ...) 
      items[i] <- sprintf("%s- %s", item_indent_str, formatted_val)
    }
  }
  
  return(paste(header, paste(items, collapse = "\n"), sep = "\n"))
}

#' @name as_toon
#' @exportS3Method print toon
#' @export
print.toon <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}