####################################################################
#' Convert TOON String Back to R Object
#' 
#' Parses a TOON string and reconstructs the corresponding R list,
#' vector, or data frame structure.
#' 
#' @param toon_string A character vector containing the TOON string.
#' @return The reconstructed R object (list, vector, or data frame).
#' @export
from_toon <- function(toon_string) {
  # Clean input string: collapse lines and trim overall whitespace.
  cleaned_toon <- trimws(paste(toon_string, collapse = "\n"))
  
  if (nchar(cleaned_toon) == 0) return(list())
  
  # --- Top-Level Structure Detection ---
  
  if (grepl("^\\[[0-9]+\\]\\{[^\\}]+\\}:", cleaned_toon)) {
    return(parse_toon_dataframe(cleaned_toon))
  }
  
  if (grepl("^\\[[0-9]+\\]:\\s*\\n\\s*-", cleaned_toon)) {
    return(parse_toon_expanded_array(cleaned_toon))
  }
  
  if (grepl("^\\[[0-9]+\\]:\\s*[^\\n]", cleaned_toon)) {
    return(parse_toon_primitive_array(cleaned_toon))
  }
  
  return(parse_toon_object(cleaned_toon))
}

# --- Internal Utility Helpers ---

unquote_string <- function(s) {
  s <- trimws(s)
  if (grepl("^\"(.*)\"$", s)) {
    s <- sub("^\"", "", sub("\"$", "", s))
    # FIX: Ensure correct pattern for escaped quotes (\\") and backslashes (\\\\)
    s <- gsub('\\\\"', '"', s, fixed = TRUE)
    s <- gsub('\\\\\\\\', '\\\\', s, fixed = TRUE)
  }
  return(s)
}

convert_value_to_r <- function(val_str) {
  # FIX: Rigorous trim to handle non-standard spaces (like \u00A0) for number conversion
  val_str <- trimws(val_str)
  
  if (nchar(val_str) == 0) return(NA)
  
  # FIX: For the specific unit tests expecting NULL, we return NULL here.
  # The calling functions (array/df parsers) must convert this back to NA for coercion safety.
  if (val_str == "null" || val_str == "NaN" || val_str %in% c("Inf", "-Inf")) {
    if (val_str == "null") return(NULL)
    # For NaN/Inf, return the R value, which can be coerced to NA later
    if (val_str == "NaN") return(NA_real_)
    if (val_str == "Inf") return(Inf)
    if (val_str == "-Inf") return(-Inf)
  }
  
  if (val_str == "true") return(TRUE)
  if (val_str == "false") return(FALSE)
  
  # Number recognition
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", val_str)) {
    if (grepl("\\.", val_str)) return(as.numeric(val_str))
    if (val_str == as.character(as.integer(val_str))) return(as.integer(val_str))
    return(as.numeric(val_str))
  }
  
  return(unquote_string(val_str))
}

# --- Parsers for Complex Structures ---

parse_toon_object <- function(toon_str) {
  result <- list()
  lines <- strsplit(toon_str, "\n")[[1]]
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    
    key_match <- regexpr("^\\s*([a-zA-Z0-9_]+):\\s*", line)
    
    if (key_match != -1) {
      key_name <- trimws(sub(":", "", regmatches(line, key_match)))
      val_str <- trimws(sub("^\\s*[a-zA-Z0-9_]+:\\s*", "", line))
      
      if (nchar(val_str) > 0) {
        # Primitive inline value
        raw_value <- convert_value_to_r(val_str)
        
        # Correctly handle NULL/Inf/NaN assignment for object keys
        if (is.null(raw_value)) { 
          result[[key_name]] <- NULL
        } else if (is.infinite(raw_value) || is.nan(raw_value)) { 
          result[[key_name]] <- raw_value
        } else {
          result[[key_name]] <- raw_value
        }
        
        i <- i + 1
      } else {
        # Complex or multi-line value (key:\n next_line)
        start_indent_level <- nchar(gsub("^(\\s*).*", "\\1", line))
        
        j <- i + 1
        val_lines <- character(0)
        
        # Collect indented lines
        while (j <= length(lines)) {
          current_line <- lines[j]
          
          if (trimws(current_line) == "") {
            j <- j + 1
            next
          }
          current_indent_level <- nchar(gsub("^(\\s*).*", "\\1", current_line))
          trimmed_content <- trimws(current_line, which = "left")
          
          # FIX (Failure 7): Final simplified break logic for zero-indent children.
          # We only break if the line is a new key/array marker and is STRICTLY LESS than the start indent.
          # If indent is equal (0 == 0), we collect it, relying on recursion.
          if (current_indent_level < start_indent_level) {
            if (grepl("^[a-zA-Z0-9_]+:", trimmed_content) || grepl("^- ", trimmed_content)) break
          }
          
          val_lines <- c(val_lines, current_line)
          j <- j + 1
        }
        
        if (length(val_lines) > 0) {
          # Robust indent calculation and stripping
          non_empty_lines <- val_lines[trimws(val_lines) != ""]
          if (length(non_empty_lines) > 0) {
            # Find the smallest indent level of the collected lines
            min_indent <- min(nchar(gsub("^(\\s*).*", "\\1", non_empty_lines)))
            # Strip only that minimum indent amount
            val_block <- sapply(val_lines, function(l) substr(l, min_indent + 1, nchar(l)))
          } else {
            val_block <- character(0)
          }
          
          result[[key_name]] <- from_toon(paste(val_block, collapse = "\n"))
          i <- j
        } else {
          result[[key_name]] <- list()
          i <- i + 1
        }
      }
    } else {
      i <- i + 1
    }
  }
  return(result)
}

parse_toon_primitive_array <- function(toon_str) {
  val_str <- sub("^\\[[0-9]+\\]:\\s*", "", toon_str)
  
  vals <- strsplit(val_str, ",\\s*")[[1]]
  
  # FIX: Need to check for NULL return from convert_value_to_r and replace with NA
  result_list <- lapply(vals, function(v) {
    r <- convert_value_to_r(v)
    # Convert NULL back to NA for vector/array cohesion
    if (is.null(r)) return(NA) else return(r)
  })
  
  # Coerce to atomic vector if possible
  if (all(sapply(result_list, function(x) length(x) <= 1 && !is.list(x)))) {
    result <- unlist(result_list)
    
    if (all(sapply(result_list, function(x) is.logical(x) || is.na(x)))) {
      return(as.logical(result))
    }
    
    return(result)
  }
  
  return(result_list)
}

parse_toon_dataframe <- function(toon_str) {
  lines <- strsplit(toon_str, "\n")[[1]]
  header_line <- lines[1]
  
  header_match <- regexec("\\[([0-9]+\\s*)\\]\\{([^\\}]+)\\}:", header_line)
  header_groups <- regmatches(header_line, header_match)[[1]]
  
  num_rows <- as.integer(trimws(header_groups[2]))
  col_names <- trimws(strsplit(header_groups[3], ",")[[1]])
  
  if (num_rows == 0) {
    return(setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)), col_names))
  }
  
  row_lines <- lines[2:length(lines)]
  
  data_list <- setNames(lapply(col_names, function(x) vector("list", num_rows)), col_names)
  
  for (i in seq_len(num_rows)) {
    if (i > length(row_lines)) break
    
    # Aggressively trim row string to handle leading spaces/special characters
    row_str <- trimws(row_lines[i])
    
    cells <- strsplit(row_str, ",\\s*")[[1]]
    
    if (length(cells) != length(col_names)) {
      next
    }
    
    for (j in seq_along(col_names)) {
      # FIX: Need to check for NULL return from convert_value_to_r and replace with NA
      r <- convert_value_to_r(cells[j])
      if (is.null(r)) {
        data_list[[j]][[i]] <- NA
      } else {
        data_list[[j]][[i]] <- r
      }
    }
  }
  
  final_df <- as.data.frame(lapply(data_list, unlist), stringsAsFactors = FALSE)
  
  return(final_df)
}

parse_toon_expanded_array <- function(toon_str) {
  result <- list()
  lines <- strsplit(toon_str, "\n")[[1]]
  
  start_idx <- which(grepl("^\\[[0-9]+\\]:", lines))[1] + 1
  if (is.na(start_idx) || start_idx > length(lines)) return(list())
  
  item_lines <- lines[start_idx:length(lines)]
  
  i <- 1
  while (i <= length(item_lines)) {
    line <- item_lines[i]
    current_indent <- nchar(gsub("^(\\s*).*", "\\1", line))
    trimmed_line <- trimws(line, which = "left")
    
    if (grepl("^- ", trimmed_line)) {
      item_content_first_line <- sub("^- ", "", trimmed_line)
      
      is_complex <- grepl("^[a-zA-Z0-9_]+:", item_content_first_line) || grepl("^\\[[0-9]+\\]\\{[^\\}]+\\}:", item_content_first_line) || grepl("^\\[[0-9]+\\]:\\s*[^\\n]", item_content_first_line)
      
      if (!is_complex) {
        # Simple primitives: check for NULL and convert to NA if needed
        item_obj <- convert_value_to_r(item_content_first_line)
        if (is.null(item_obj)) item_obj <- NA
        result <- append(result, list(item_obj))
        i <- i + 1
      } else {
        # Handle complex item (collect block)
        j <- i + 1
        item_block_lines <- c()
        
        while (j <= length(item_lines)) {
          next_line <- item_lines[j]
          next_indent <- nchar(gsub("^(\\s*).*", "\\1", next_line))
          
          if (grepl("^- ", trimws(next_line, which = "left")) || next_indent <= current_indent) break
          
          item_block_lines <- c(item_block_lines, next_line)
          j <- j + 1
        }
        
        val_block <- item_content_first_line
        
        if (length(item_block_lines) > 0) {
          # Robust indent calculation and stripping
          non_empty_continuation <- item_block_lines[trimws(item_block_lines) != ""]
          if (length(non_empty_continuation) > 0) {
            min_indent <- min(nchar(gsub("^(\\s*).*", "\\1", non_empty_continuation)))
            stripped_continuation <- sapply(item_block_lines, function(l) substr(l, min_indent + 1, nchar(l)))
            val_block <- c(val_block, stripped_continuation)
          }
        }
        
        item_obj <- from_toon(paste(val_block, collapse = "\n"))
        
        result <- append(result, list(item_obj))
        i <- j
      }
    } else {
      i <- i + 1
    }
  }
  return(result)
}