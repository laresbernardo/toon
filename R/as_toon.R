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
  row_indent_str <- make_indent(.indent + 1)
  
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
  
  recursive_toon_call <- function(val, current_indent) {
    as_toon_string(val, .indent = current_indent, ...)
  }
  
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
        # FIX: Call complex multi-line value with current .indent, not .indent + 1.
        # This aligns the array/data.frame header to the object's body indent.
        formatted_val <- recursive_toon_call(val, .indent = .indent) 
        lines[i] <- sprintf("%s%s:\n%s", obj_indent_str, key, formatted_val)
      } else {
        # Primitives still need + 1 indent for the inline value.
        formatted_val <- recursive_toon_call(val, .indent = .indent + 1)
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
      
      # Replace the indent of the *nested key* (at .indent + 2) 
      # with the array marker (' - ') at .indent + 1.
      lines_of_val[1] <- sub(make_indent(.indent + 2), paste0(item_indent_str, "- "), lines_of_val[1], fixed = TRUE)
      items[i] <- paste(lines_of_val, collapse = "\n")
      
    } else {
      # Primitives and inline primitive arrays. Recurse with .indent + 1.
      formatted_val <- as_toon_string(val, .indent = .indent + 1, ...) 
      items[i] <- sprintf("%s- %s", item_indent_str, formatted_val)
    }
  }
  
  return(paste(header, paste(items, collapse = "\n"), sep = "\n"))
}


as_toon <- function(x, ...) {
  s <- as_toon_string(x, ...)
  structure(s, class = "toon")
}

#' @exportS3Method print toon
print.toon <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}