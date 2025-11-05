library(testthat)

# --- Test Helper Functions ---

test_that("make_indent generates correct indentation", {
  # Assuming '  ' (two spaces) is the intended indentation character
  expect_equal(make_indent(0), "")
  expect_equal(make_indent(1), "  ")
  expect_equal(make_indent(3), "      ")
  expect_equal(make_indent(5), "          ")
})

test_that("format_special_numeric handles special values", {
  expect_equal(format_special_numeric(NA_real_), "null")
  expect_equal(format_special_numeric(NaN), "null")
  expect_equal(format_special_numeric(Inf), "null")
  expect_equal(format_special_numeric(-Inf), "null")
  expect_equal(format_special_numeric(123.45), "123.45")
  expect_equal(format_special_numeric(0), "0")
})

test_that("quote_toon_string handles quoting and keywords", {
  # Keywords (should not be quoted)
  expect_equal(quote_toon_string("null"), "null")
  expect_equal(quote_toon_string("true"), "true")
  expect_equal(quote_toon_string("NaN"), "NaN")
  expect_equal(quote_toon_string("Inf"), "Inf")
  
  # Bare numbers (should not be quoted)
  expect_equal(quote_toon_string("123"), "123")
  expect_equal(quote_toon_string("123.45"), "123.45")
  
  # Regular strings (should be quoted)
  expect_equal(quote_toon_string("hello world"), "\"hello world\"")
  
  # Strings with quotes or backslashes (should be escaped)
  expect_equal(quote_toon_string("string with \"quote\""), "\"string with \\\"quote\\\"\"")
  expect_equal(quote_toon_string("string\\with\\backslash"), "\"string\\\\with\\\\backslash\"")
  expect_equal(quote_toon_string("string\\ with \"both\""), "\"string\\\\ with \\\"both\\\"\"")
  
  # NA handling
  expect_equal(quote_toon_string(NA_character_), "null")
})

# --- Test Primitive S3 Methods (as_toon_string.*) ---

test_that("as_toon_string.NULL returns 'null'", {
  expect_equal(as_toon_string(NULL), "null")
})

test_that("as_toon_string.logical handles values and NA", {
  expect_equal(as_toon_string(TRUE), "true")
  expect_equal(as_toon_string(FALSE), "false")
  expect_equal(as_toon_string(NA), "null")
})

test_that("as_toon_string.numeric handles normal, special, and NA values", {
  expect_equal(as_toon_string(123.45), "123.45")
  expect_equal(as_toon_string(0), "0")
  expect_equal(as_toon_string(NA_real_), "null")
  expect_equal(as_toon_string(Inf), "null")
  expect_equal(as_toon_string(NaN), "null")
})

test_that("as_toon_string.integer delegates to numeric method", {
  expect_equal(as_toon_string(1L), "1")
  expect_equal(as_toon_string(NA_integer_), "null")
})

test_that("as_toon_string.character quotes and handles special strings", {
  expect_equal(as_toon_string("test"), "\"test\"")
  expect_equal(as_toon_string("123"), "\"123\"")
  expect_equal(as_toon_string("false"), "\"false\"")
  expect_equal(as_toon_string(NA_character_), "null")
})

# --- Test Vector/Array Dispatch (as_toon_string) ---

test_that("as_toon_string dispatches multi-atomic vectors to dispatch_to_list", {
  char_vec <- c("a", "b", "c")
  expected_char_array <- '[3]: "a","b","c"'
  expect_equal(as_toon_string(char_vec), expected_char_array)
  
  num_vec <- c(1, 2.5, NA)
  expected_num_array <- '[3]: 1,2.5,null'
  expect_equal(as_toon_string(num_vec), expected_num_array)
  
  log_vec <- c(TRUE, FALSE, NA)
  expected_log_array <- '[3]: true,false,null'
  expect_equal(as_toon_string(log_vec), expected_log_array)
  
  # Single-element atomic (should not dispatch to list/array)
  expect_equal(as_toon_string(1), "1")
  expect_equal(as_toon_string("a"), "\"a\"")
})

# --- Test Complex Structure S3 Methods ---

test_that("as_toon_string.data.frame handles empty data frames", {
  df <- data.frame()
  expect_equal(as_toon_string(df), "[]")
})

test_that("as_toon_string.data.frame formats standard data frames correctly", {
  df <- data.frame(
    time = c(9.1, 15.4),
    action = c("login", "update"),
    success = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  expected_output <- '[2]{time,action,success}:
  9.1,"login",true
  15.4,"update",false'
  expect_equal(as_toon_string(df, .indent = 0), expected_output)
  
  # Test with indentation
  expected_output_indented <- '  [2]{time,action,success}:
    9.1,"login",true
    15.4,"update",false'
  expect_equal(as_toon_string(df, .indent = 1), expected_output_indented)
  
  # Test special values in data.frame
  df_special <- data.frame(
    n = c(NaN, Inf, 10),
    l = c(NA, TRUE, FALSE)
  )
  expected_output_special <- '[3]{n,l}:
  null,null
  null,true
  10,false'
  expect_equal(as_toon_string(df_special, .indent = 0), expected_output_special)
})

test_that("as_toon_string.list handles empty list", {
  expect_equal(as_toon_string(list()), "[]")
})

test_that("as_toon_string.list handles primitive unnamed list (inline array - Case 2)", {
  l_primitive <- list("a", 1, TRUE, NA, NaN)
  expected_toon <- '[5]: "a",1,true,null,null'
  expect_equal(as_toon_string(l_primitive), expected_toon)
})

test_that("as_toon_string.list handles named list (TOON Object - Case 1)", {
  config_obj <- list(
    version = 1.0,
    is_active = TRUE,
    name = "main"
  )
  expected_toon <- 'version: 1
is_active: true
name: "main"'
  expect_equal(as_toon_string(config_obj, .indent = 0), expected_toon)
  
  # Named list with indentation
  expected_indented <- '  version: 1
  is_active: true
  name: "main"'
  expect_equal(as_toon_string(config_obj, .indent = 1), expected_indented)
})

test_that("as_toon_string.list handles complex nested object (Case 1 recursion)", {
  nested_obj <- list(
    config = list(
      key1 = 10,
      key2 = FALSE
    )
  )
  expected_toon <- 'config:
key1: 10
key2: false'
  expect_equal(as_toon_string(nested_obj, .indent = 0), expected_toon)
  
  # Test with external indentation
  expected_indented <- '  config:
  key1: 10
  key2: false'
  expect_equal(as_toon_string(nested_obj, .indent = 1), expected_indented)
})

test_that("as_toon_string.list handles mixed unnamed list (Expanded Array - Case 3)", {
  mixed_list <- list(
    "apple",
    list(color = "red", weight = 150),
    10.5,
    list(
      nested = TRUE
    )
  )
  expected_toon <- '[4]:
  - "apple"
  - color: "red"
    weight: 150
  - 10.5
  - nested: true'
  
  expect_equal(as_toon_string(mixed_list, .indent = 0), expected_toon)
  
  # Test with external indentation
  expected_indented <- '  [4]:
    - "apple"
    - color: "red"
      weight: 150
    - 10.5
    - nested: true'
  expect_equal(as_toon_string(mixed_list, .indent = 1), expected_indented)
})

test_that("as_toon_string.list handles data.frame inside an Expanded Array (Case 3)", {
  df <- data.frame(
    x = 1:2,
    y = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  array_with_df <- list(
    "start",
    df
  )
  
  expected_output <- '[2]:
  - "start"
  - [2]{x,y}:
    1,true
    2,false'
  
  expect_equal(as_toon_string(array_with_df, .indent = 0), expected_output)
})

test_that("Full Config_Test Structure (Complex Nesting)", {
  df_log <- data.frame(
    time = c(9.1, 15.4),
    action = c("login", "update"),
    success = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  Config_Test <- list(
    Settings = list(
      Timeout = 300,
      Retries = 5,
      Debug = FALSE
    ),
    Logs = df_log,
    Servers = list(
      list(
        host = "web01.int",
        ip = "10.0.0.1",
        ports = c(80, 443)
      ),
      c("failover.ext", "192.168.1.1"),
      list(
        host = "db01.int",
        locked = TRUE
      )
    )
  )
  
  expected_toon <- 'Settings:
Timeout: 300
Retries: 5
Debug: false
Logs:
[2]{time,action,success}:
  9.1,"login",true
  15.4,"update",false
Servers:
[3]:
  - host: "web01.int"
    ip: "10.0.0.1"
    ports: [2]: 80,443
  - [2]: "failover.ext","192.168.1.1"
  - host: "db01.int"
    locked: true'
  
  expect_equal(as_toon_string(Config_Test), expected_toon)
})

# --- Test Wrapper and Print Functions ---

test_that("as_toon returns a 'toon' class string", {
  result <- as_toon(1)
  expect_s3_class(result, "toon")
  expect_equal(as.character(result), "1")
})

test_that("print.toon prints the string to console", {
  result <- as_toon("test")
  output <- capture.output(print(result))
  expect_equal(output[1], "\"test\"")
})