# toon <a href='https://github.com/laresbernardo/toon' target="_blank"><img src='man/figures/toon.png' align="right" height="139" /></a>

`toon` is a dependency-free R package that provides functions to serialize native R objects (lists, data frames, vectors) into the **Token-Oriented Object Notation** (TOON) format.

TOON is specifically designed to **significantly reduce token usage** when passing structured data to Large Language Models (LLMs). It offers a substantial token-efficiency advantage over traditional formats like JSON, particularly for **tabular or highly uniform data**.

---

## Installation

Install the development version directly from GitHub:

```r
# If you don't have 'remotes' installed, run: install.packages('remotes')
remotes::install_github("laresbernardo/toon")
```

-----

## Usage Example

The primary function is `as_toon()`, which takes an R object and returns a `toon` class string.

### Complex List Serialization

Let's define a complex R list containing vectors, a data frame, and nested lists (objects and arrays):

```r
Config_Test <- list(
  Top_Array = c("alpha", "beta", "gamma"),
  Empty_Settings = list(),
  User_Profile = list(
    ID = 12345,
    Is_Admin = TRUE,
    Status = "Active",
    Null_Field = NULL,
    Not_A_Number = NaN
  ),
  Usage_Log = data.frame(
    time = c(9.1, 15.4),
    action = c("login", "update"),
    success = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ),
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
```

Calling `as_toon(Config_Test)` yields the following **token-optimized format**:

```
Top_Array: [3]: "alpha","beta","gamma"
Empty_Settings: []
User_Profile:
  ID: 12345
  Is_Admin: true
  Status: "Active"
  Null_Field: null
  Not_A_Number: null
Usage_Log:
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
    locked: true
```

### Tabular Data Efficiency (The Key Benefit)

While the complex example shows `toon`'s restructuring capabilities, its **most powerful use case** is dramatically reducing the size of datasets.

```r
x <- iris # A simple 150 rows and 5 columns dataset
# Compare the TOON string size to a standard JSON string
nchar(as_toon(x)) / nchar(jsonlite::toJSON(x)) 
# Result: ~0.28 (Same info, roughly 28% the size)
```

-----

## Read More about TOON

The `toon` package is based on and inspired by the official format specification. You can read more about the **Token-Oriented Object Notation** here:

[toon-format/toon](https://github.com/toon-format/toon)
