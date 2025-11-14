library(testthat)

# --- Test Helper Functions (Internal Logic) ---

test_that("unquote_string handles quotes and escapes correctly", {
  # Simple unquoting
  expect_equal(unquote_string("\"hello\""), "hello")
  
  # No change for unquoted strings/numbers
  expect_equal(unquote_string("null"), "null")
  expect_equal(unquote_string("123.45"), "123.45")
  
  # Handles escaped quotes
  expect_equal(unquote_string("\"string with \"quote\"\""), "string with \"quote\"")
  
  # Handles escaped backslashes
  expect_equal(unquote_string("\"string\\with\\backslash\""), "string\\with\\backslash")
  
  # Handles both
  expect_equal(unquote_string("\"string\\ with \"both\"\""), "string\\ with \"both\"")
})

test_that("convert_value_to_r converts TOON primitives to R types", {
  # TOON keywords to R
  expect_equal(convert_value_to_r("null"), NULL)
  expect_equal(convert_value_to_r("true"), TRUE)
  expect_equal(convert_value_to_r("false"), FALSE)
  expect_equal(convert_value_to_r("NaN"), NaN)
  expect_equal(convert_value_to_r("Inf"), Inf)
  
  # Numbers to numeric/integer
  expect_equal(convert_value_to_r("123"), 123L)
  expect_equal(convert_value_to_r("123.45"), 123.45)
  expect_equal(convert_value_to_r("-5"), -5L)
  
  # Strings (quoted and unquoted, post-conversion)
  expect_equal(convert_value_to_r("\"hello\""), "hello")
  expect_equal(convert_value_to_r("\"123.45\""), "123.45")
})

# --- Test Primitive Array Parsing (parse_toon_primitive_array) ---

test_that("parse_toon_primitive_array handles primitive vectors", {
  # Character vector
  toon_char <- '[3]: "a","b","c"'
  expect_equal(parse_toon_primitive_array(toon_char), c("a", "b", "c"))
  
  # Numeric vector
  toon_num <- '[3]: 1,2.5,null'
  expect_equal(parse_toon_primitive_array(toon_num), c(1, 2.5, NA_real_)) # NA_real_ due to unlist coercion
  
  # Logical vector
  toon_log <- '[3]: true,false,null'
  expect_equal(parse_toon_primitive_array(toon_log), c(TRUE, FALSE, NA))
  
  # Mixed types (should result in character vector or list)
  toon_mixed <- '[3]: 1,"a",true'
  expect_equal(parse_toon_primitive_array(toon_mixed), c("1", "a", "TRUE"))
})

# --- Test Object Parsing (parse_toon_object) ---

test_that("parse_toon_object handles simple named list (Object)", {
  toon_obj <- 'version: 1\nis_active: true\nname: "main"'
  expected_r <- list(version = 1L, is_active = TRUE, name = "main")
  expect_equal(parse_toon_object(toon_obj), expected_r)
})

test_that("parse_toon_object handles nested objects correctly (recursion)", {
  toon_nested <- 'config:\nkey1: 10\nkey2: false'
  expected_r <- list(config = list(key1 = 10L, key2 = FALSE))
  expect_equal(parse_toon_object(toon_nested), expected_r)
})

# --- Test Data Frame Parsing (parse_toon_dataframe) ---

test_that("parse_toon_dataframe handles empty data frames", {
  toon_empty_df <- '[]'
  # Expecting a list from untoon if passed '[]' as a top-level. 
  # parse_toon_dataframe needs the header format to work.
  df <- data.frame(a = numeric(), b = logical())
  empty_header <- '[0]{a,b}:'
  expected_df <- stats::setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("a", "b"))
  expect_equal(parse_toon_dataframe(empty_header), expected_df)
})

test_that("parse_toon_dataframe formats standard data frames correctly", {
  toon_df <- '[2]{time,action,success}:\n 9.1,"login",true\n 15.4,"update",false'
  expected_df <- data.frame(
    time = c(9.1, 15.4),
    action = c("login", "update"),
    success = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  expect_equal(parse_toon_dataframe(toon_df), expected_df)
  
  # Test with special values in data.frame (NA/Inf/NaN become NULL/"null")
  toon_df_special <- '[3]{n,l}:\n null,null\n null,true\n 10,false'
  expected_df_special <- data.frame(
    n = c(NA_real_, NA_real_, 10),
    l = c(NA, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  expect_equal(parse_toon_dataframe(toon_df_special), expected_df_special)
})

# --- Test Expanded Array Parsing (parse_toon_expanded_array) ---

test_that("parse_toon_expanded_array handles mixed unnamed list", {
  toon_mixed <- '[4]:\n - "apple"\n - color: "red"\n   weight: 150\n - 10.5\n - nested: true'
  expected_r <- list(
    "apple",
    list(color = "red", weight = 150L),
    10.5,
    list(nested = TRUE)
  )
  expect_equal(parse_toon_expanded_array(toon_mixed), expected_r)
})

test_that("parse_toon_expanded_array handles nested data.frame", {
  toon_array_with_df <- '[2]:\n - "start"\n - [2]{x,y}:\n   1,true\n   2,false'
  expected_df <- data.frame(
    x = 1:2,
    y = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  expected_r <- list(
    "start",
    expected_df
  )
  expect_equal(parse_toon_expanded_array(toon_array_with_df), expected_r)
})

# --- Test Top-Level Dispatch (untoon) ---

test_that("untoon dispatches to correct parser for top-level object", {
  toon_obj <- 'key: "value"'
  expect_equal(untoon(toon_obj), list(key = "value"))
})

test_that("untoon dispatches to correct parser for top-level primitive array", {
  toon_arr <- '[2]: 1,2'
  expect_equal(untoon(toon_arr), c(1L, 2L))
})

test_that("untoon dispatches to correct parser for top-level expanded array", {
  toon_exp_arr <- '[2]:\n - 1\n - 2'
  expect_equal(untoon(toon_exp_arr), list(1L, 2L))
})

test_that("untoon dispatches to correct parser for top-level data frame", {
  toon_df <- '[1]{x}:\n 1'
  expected_df <- data.frame(x = 1L)
  expect_equal(untoon(toon_df), expected_df)
})
