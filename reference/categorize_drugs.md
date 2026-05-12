# Categorize medications by therapeutic class

Adds binary indicator columns for medication categories based on pattern
matching in medication names and/or ATC codes.

## Usage

``` r
categorize_drugs(
  dataf,
  med_col,
  atc_col = NULL,
  categories = NULL,
  prefix = "is_",
  suffix = "_med"
)
```

## Arguments

- dataf:

  Data frame with medication data.

- med_col:

  Column with medication names.

- atc_col:

  Column with ATC codes/names. Optional; if supplied, a match in either
  `med_col` or `atc_col` flags the row.

- categories:

  Named list of pattern vectors. If `NULL` (default), uses built-in
  cardiometabolic categories (cholesterol, diabetes, hypertension).
  Supply your own list for any other therapeutic grouping.

- prefix:

  Prefix for output column names (default: `"is_"`). Empty string
  allowed.

- suffix:

  Suffix for output column names (default: `"_med"`). Empty string
  allowed.

## Value

The input data frame with one binary (0/1) column per category.

## Details

Patterns are matched as whole words (`\b...\b`) using
[`grepl()`](https://rdrr.io/r/base/grep.html) with `ignore.case = TRUE`.
Patterns are interpreted as **regular expressions**: regex
metacharacters (`.`, `+`, `*`, `(`, `)`, `[`, `]`, `?`, `^`, `$`, `|`,
`\\`) inside a pattern have their usual regex meaning. To match a
literal metacharacter, escape it (e.g. `"hmg-coa\\."`). Plain word
patterns like `"statin"` need no escaping.

Because matches are anchored at word boundaries, a pattern like
`"statin"` matches the standalone word `"statin"` (or the plural form
`"statins"` if listed) but **not** generic names that contain it as a
suffix (e.g. `"atorvastatin"`). This is intentional: it prevents false
positives when patterns are short common substrings. To match drugs by
generic name, supply explicit patterns (e.g. `"atorvastatin"`,
`"rosuvastatin"`) or rely on the ATC class column instead.

The built-in defaults assume `atc_col` (when supplied) contains
second-level ATC class names, i.e. the output of
[`add_atc_classification()`](https://anaboeriu14.github.io/adRpheno/reference/add_atc_classification.md)
with `atc_level = "second"`. They mix exact ATC2 class names with common
generic-name patterns so they match against either column.

Default categories:

- **cholesterol**: lipid modifying agents, statins

- **diabetes**: drugs used in diabetes, blood glucose lowering agents,
  antidiabetics

- **hypertension**: renin-angiotensin agents, calcium channel blockers,
  diuretics, beta blockers, antihypertensives

For other therapeutic groups, or for matching against a different ATC
level, supply `categories` explicitly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use defaults (cardiometabolic, ATC2-aware)
df <- categorize_drugs(df, med_col = "medication", atc_col = "atc2_class")

# Custom categories
my_cats <- list(
  anticoagulant  = c("antithrombotic agents", "warfarin", "apixaban", "rivaroxaban"),
  antidepressant = c("antidepressants", "ssri", "sertraline", "fluoxetine")
)
df <- categorize_drugs(df, med_col = "medication", categories = my_cats)
} # }
```
