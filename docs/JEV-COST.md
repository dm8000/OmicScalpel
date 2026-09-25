# What a question to Jev costs

Measured 2026-09-25 against the live API, 18 datasets in the catalog.
Input tokens are billed at $0.042 per million; output tokens are free
(docs.typesafe.ai/models). One question is one request: the catalog goes out
once and every typed question is answered against it in parallel.

## Per question

| level | lang | question | questions asked | input tokens | cost | outcome | as expected |
|---|---|---|---|---|---|---|---|
| 1 | en | Does leptin expression increase with BMI in human adipose tissue |  7 | 11,735 | $0.000493 | meta_analysis | yes |
| 1 | pt | A expressao de leptina aumenta com o IMC no tecido adiposo human |  7 | 11,740 | $0.000493 | meta_analysis | yes |
| 1 | en | Is UCP1 expression related to age in brown adipose tissue of mic |  7 | 11,756 | $0.000494 | compare_samples | **no** |
| 1 | pt | A expressao de UCP1 se relaciona com a idade no tecido adiposo m |  7 | 11,751 | $0.000494 | ambiguous | yes |
| 2 | en | Is adiponectin expression different between obese and lean peopl |  7 | 11,741 | $0.000493 | meta_analysis | yes |
| 2 | pt | A expressao de adiponectina difere entre pessoas obesas e magras |  7 | 11,747 | $0.000493 | meta_analysis | yes |
| 3 | en | (setup) Does leptin expression increase with BMI in human adipos | NA | 11,735 | $0.000493 | meta_analysis | yes |
| 3 | en | Does that differ between the sexes? |  8 | 13,103 | $0.000550 | compare_samples | yes |
| 3 | pt | (setup) A expressao de leptina aumenta com o IMC no tecido adipo | NA | 11,740 | $0.000493 | meta_analysis | yes |
| 3 | pt | Isso muda entre os sexos? |  8 | 13,108 | $0.000551 | compare_samples | yes |
| 4 | en | Is shh expression higher in the zebrafish brain after hypoxia? |  7 | 11,732 | $0.000493 | none | yes |
| 4 | pt | A expressao de shh e maior no cerebro de peixe-zebra apos hipoxi |  7 | 11,737 | $0.000493 | none | yes |

## Per level

| level | what it needs | questions | mean tokens | mean cost | as expected |
|---|---|---|---|---|---|
| 1 | one gene, one variable | 4 | 11,746 | $0.000493 | 3/4 |
| 2 | several datasets pooled | 2 | 11,744 | $0.000493 | 2/2 |
| 3 | a follow-up that splits the previous answer | 4 | 12,422 | $0.000522 | 4/4 |
| 4 | nothing in the hub can answer it | 2 | 11,734 | $0.000493 | 2/2 |

## Per language

| language | questions | mean tokens | mean cost | as expected |
|---|---|---|---|---|
| en | 6 | 11,967 | $0.000503 | 5/6 |
| pt | 6 | 11,970 | $0.000503 | 6/6 |

Total for this run: 143,625 input tokens, $0.006032.

## The same question, asked five times

"Is UCP1 expression related to age in brown adipose tissue of mice?", and its Portuguese translation, 5 times each. This question sits
near the confidence threshold on purpose: the catalog holds `Age`, `Age.Class`
and `Differentiation.day`, so the model spreads its probability across them.

| lang | run | variable chosen | confidence | outcome |
|---|---|---|---|---|
| en | 1 | Age | 0.56 | compare_samples |
| en | 2 | Age | 0.61 | compare_samples |
| en | 3 | Age | 0.54 | ambiguous |
| en | 4 | Age | 0.56 | compare_samples |
| en | 5 | Age | 0.56 | compare_samples |
| pt | 1 | Age | 0.54 | ambiguous |
| pt | 2 | Age | 0.49 | ambiguous |
| pt | 3 | Age | 0.57 | compare_samples |
| pt | 4 | Age.Class | 0.49 | ambiguous |
| pt | 5 | Age | 0.51 | ambiguous |

Distinct outcomes: 2 in English, 2 in Portuguese. Mean confidence 0.57 and 0.52.

## What "as expected" means

The expected outcome is what the tab *should* do, which is not always a plot.
The two UCP1 questions are expected to come back as **ambiguous**: the catalog
holds `Age`, `Age.Class` and `Differentiation.day`, the model spreads its
probability across them, and the tab asks which was meant instead of picking
one and drawing a plausible wrong answer. The zebrafish questions are expected
to be refused, because nothing in the collection is zebrafish.

Regenerate with `Rscript tools/jev-cost.R`. Every row is a real request.
