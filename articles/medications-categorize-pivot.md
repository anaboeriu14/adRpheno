# Categorize & pivot to wide format

``` r

library(adRpheno)
```

This article picks up where [RxNorm & ATC
lookup](https://anaboeriu14.github.io/adRpheno/articles/medications-rxnorm-atc.md)
left off. You have a long-format medication table — one row per
(participant, medication) — with RxCUIs and ATC classes attached. Two
final transformations to make it analysis-ready:

1.  **Categorize**: flag each medication as belonging to therapeutic
    categories (statins, antidiabetics, antihypertensives, …).
2.  **Pivot wide**: collapse to one row per participant, with
    per-category indicators and counts.

## Starting point

Say this is what you have:

``` r

meds <- data.frame(
  participant_id = c(1, 1, 1, 2, 2, 3),
  med_slot       = c("medication_1_name", "medication_2_name",
                     "medication_3_name", "medication_1_name",
                     "medication_2_name", "medication_1_name"),
  medication     = c("atorvastatin", "metformin", "lisinopril",
                     "aspirin", "amlodipine", "hydrochlorothiazide"),
  atc2_class     = c("lipid modifying agents",
                     "drugs used in diabetes",
                     "agents acting on the renin-angiotensin system",
                     "antithrombotic agents",
                     "calcium channel blockers",
                     "diuretics")
)
```

Participant 1 takes three medications, participant 2 takes two,
participant 3 takes one. The `med_slot` column encodes position — the
leading integer (`1`, `2`, `3`) marks order within participant.

## Categorize: which therapeutic class is this?

[`categorize_drugs()`](https://anaboeriu14.github.io/adRpheno/reference/categorize_drugs.md)
adds one binary column per category. With no arguments beyond column
names, it uses three built-in cardiometabolic categories:

``` r

meds <- categorize_drugs(meds, med_col = "medication", atc_col = "atc2_class")

# New columns: is_cholesterol_med, is_diabetes_med, is_hypertension_med
```

**How matching works.** Each category is a vector of patterns. They’re
joined into `\b(pattern1|pattern2|...)\b` and matched as **whole
words**, case-insensitive, against both `med_col` and `atc_col`. A hit
in either column flags the row.

This is why the defaults mix ATC class names with generic drug names:

``` r

# What the defaults actually contain (excerpt):
list(
  cholesterol = c("lipid modifying agents", "statin", "statins"),
  diabetes    = c("drugs used in diabetes", "blood glucose lowering", "antidiabetic"),
  hypertension = c("agents acting on the renin-angiotensin system",
                   "calcium channel blockers", "beta blocking agents",
                   "diuretics", "antihypertensive")
)
```

The ATC2 class names catch most cases via `atc_col`. The bare drug-class
names (`"statin"`, `"antidiabetic"`) are fallbacks for rows where the
ATC lookup failed but the medication name itself contains the class.

### Custom categories

The defaults are cardiometabolic-focused; if you’re studying something
else, pass your own:

``` r

my_cats <- list(
  anticoagulant = c(
    "antithrombotic agents",       # ATC2 class
    "warfarin", "apixaban", "rivaroxaban", "dabigatran"  # specific drugs
  ),
  antidepressant = c(
    "antidepressants",             # ATC2 class
    "ssri", "snri",                # subclass shorthand
    "sertraline", "fluoxetine", "venlafaxine"
  ),
  benzodiazepine = c(
    "anxiolytics",                 # ATC2 class — catches most
    "benzodiazepine", "diazepam", "lorazepam", "alprazolam"
  )
)

meds <- categorize_drugs(meds, med_col = "medication", atc_col = "atc2_class",
                         categories = my_cats)
```

**A note on regex.** The patterns are passed to
[`grepl()`](https://rdrr.io/r/base/grep.html) as-is, so regex
metacharacters work. `"statin"` matches `statin` and `statins` (it’s a
substring within `\b...\b`). `"hmg-coa"` matches literally. If you need
a literal dot or parenthesis, escape it: `"hmg-coa\\."`. For plain words
this doesn’t matter.

**Whole-word matching has one consequence worth knowing:** `"insulin"`
as a pattern won’t accidentally match `"insulin-like growth factor"`
because of the word boundaries, but it *will* match
`"insulin glargine"`. That’s almost always what you want; just be aware
that hyphens count as word breaks.

## Pivot to wide format

Now collapse from long to one row per participant:

``` r

wide <- pivot_medication_data_wide(
  meds,
  id_col         = "participant_id",
  medication_col = "medication",
  position_col   = "med_slot",
  category_cols  = c("is_cholesterol_med", "is_diabetes_med", "is_hypertension_med")
)
```

The position numbers are extracted from `position_col`
(`"medication_1_name"` → 1), preserving the original ordering. The
output has three blocks of columns:

**1. Medication name slots** (`new_<position>`):

    new_medication_1_name  new_medication_2_name  new_medication_3_name
    atorvastatin           metformin              lisinopril
    aspirin                amlodipine             NA
    hydrochlorothiazide    NA                     NA

**2. Per-slot category indicators** (`<category>_<position>`):

    is_cholesterol_med_1  is_cholesterol_med_2  is_cholesterol_med_3
                       1                     0                     0
                       0                     0                     0
                       0                     0                     0

**3. Per-participant summaries** (`total_<cat>_meds`,
`taking_<cat>_med`):

    total_cholesterol_meds  taking_cholesterol_med  total_hypertension_meds  taking_hypertension_med
                         1                       1                        1                        1
                         0                       0                        1                        1
                         0                       0                        1                        1

The summary names assume your category columns follow the
`is_<name>_med` convention from
[`categorize_drugs()`](https://anaboeriu14.github.io/adRpheno/reference/categorize_drugs.md).
The function strips the `is_` prefix and `_med` suffix, so
`is_diabetes_med` becomes `total_diabetes_meds` / `taking_diabetes_med`.
Columns named differently still work, just with less polished summary
names.

## Optional knobs

- **`max_meds`** — cap the number of medication slots if some
  participants take 20+ and you only want the first 10:

  ``` r

  pivot_medication_data_wide(meds, ..., max_meds = 10)
  ```

- **`fill_value`** — what to put in unfilled slots. Defaults to `NA`.
  Use `0` for category indicators if you want zero-filled rather than
  NA-filled summaries (the summary columns already handle this — they’re
  computed with `na.rm = TRUE`).

- **`med_prefix`** — controls the medication-slot column prefix.
  Defaults to `"new_"` to avoid collision with whatever existed in the
  input.

## The full pipeline, end to end

Putting both articles together, the full medication processing flow is:

``` r

medications_raw |>
  # (your own cleaning step here)
  add_rxcuis(med_column = "med_clean") |>
  add_atc_classification(atc_level = "second") |>
  categorize_drugs(med_col = "med_clean", atc_col = "atc2_class") |>
  pivot_medication_data_wide(
    id_col         = "participant_id",
    medication_col = "med_clean",
    position_col   = "med_slot",
    category_cols  = c("is_cholesterol_med", "is_diabetes_med", "is_hypertension_med")
  )
```

Run it once, let the cache populate, and reruns are minutes instead of
hours.
