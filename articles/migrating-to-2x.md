# Migrating from 1.x to 2.x

This guide covers the changes you need to make to upgrade code written
for adRpheno 1.x to the 2.x series. It lists **breaking changes only** —
the things that will error or behave differently until you update your
calls. New features are not listed here; see the
[2.0.0](https://github.com/anaboeriu14/adRpheno/releases/tag/v2.0.0) and
[2.1.0](https://github.com/anaboeriu14/adRpheno/releases/tag/v2.1.0)
release notes for the full changelog.

## Before you start: install adRutils

The 2.x series relocated several functions to the companion package
[adRutils](https://github.com/anaboeriu14/adRutils). Install (or update)
it first, then update adRpheno:

``` r

remotes::install_github("anaboeriu14/adRutils")
remotes::install_github("anaboeriu14/adRpheno")
```

adRpheno’s `DESCRIPTION` declares the minimum adRutils version it needs,
so a normal install will pull a compatible version automatically.

## Renamed functions

The function does the same thing under a new name. Update the call;
arguments and behavior are unchanged.

| 1.x | 2.x |
|----|----|
| `sum_cognitive_test_components(df, ...)` | `combine_cognitive_test_components(df, ...)` |

The new name is method-neutral, which reads correctly whether you pass
`method = "sum"` or `method = "mean"`.

## Functions moved to adRutils

These functions left adRpheno entirely. There is **no deprecation shim**
— calls to the old `adRpheno::` location will error. Update the
namespace (and attach adRutils, or call with `adRutils::`).

| 1.x (adRpheno) | 2.x (adRutils) |
|----|----|
| `compute_zscores(df, ...)` | `adRutils::compute_zscores(df, ...)` |
| `detect_outlier_thresholds(...)` | `adRutils::detect_outlier_thresholds(...)` |
| `replace_outliers_with_na(...)` | `adRutils::replace_outliers_with_na(...)` |

Signatures and behavior are unchanged; only the package they live in
changed.

## Changed arguments

The function stayed, but a parameter changed name or became required.

| Function | 1.x | 2.x |
|----|----|----|
| [`add_atc_classification()`](https://anaboeriu14.github.io/adRpheno/reference/add_atc_classification.md) | `unnest = TRUE` | `as_list_column = TRUE` |
| [`pivot_medication_data_wide()`](https://anaboeriu14.github.io/adRpheno/reference/pivot_medication_data_wide.md) | `position_col` optional | `position_col` **required** (no default) |
| [`get_single_rxcui()`](https://anaboeriu14.github.io/adRpheno/reference/get_single_rxcui.md) | had `retry_count` | `retry_count` removed (retries now happen at the batch layer) |

For
[`add_atc_classification()`](https://anaboeriu14.github.io/adRpheno/reference/add_atc_classification.md),
the behavior is also slightly different: with `as_list_column = TRUE`
the output is a list-column rather than exploded rows. To recover the
old row-per-class shape, follow with
[`tidyr::unnest_longer()`](https://tidyr.tidyverse.org/reference/unnest_longer.html).

## Removed functions

These were removed with no direct replacement.

| Removed                      | What to do instead                       |
|------------------------------|------------------------------------------|
| `reverse_cognitive_scores()` | Multiply the column(s) by `-1` directly. |

## Dependency change

adRpheno 2.x no longer depends on the unmaintained `rxnorm` GitHub
package.
[`add_atc_classification()`](https://anaboeriu14.github.io/adRpheno/reference/add_atc_classification.md)
now calls the NLM RxClass REST API directly. No code change is required
on your side, but if you previously installed `rxnorm` only for
adRpheno, you can remove it.
