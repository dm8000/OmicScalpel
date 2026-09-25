# What a question to Jev costs

Measured 2026-09-25 against the live API, 18 datasets in the catalog.
Input tokens are billed at $0.042 per million; output tokens are free
(docs.typesafe.ai/models). One question is one request: the catalog goes out
once and every typed question is answered against it in parallel.

## Per question

| level | lang | question | questions asked | input tokens | cost | outcome | as expected |
|---|---|---|---|---|---|---|---|
| 1 | en | Does leptin expression increase with BMI in human adipose tissue | 11 | 12,747 | $0.000535 | meta_analysis | yes |
| 1 | pt | A expressao de leptina aumenta com o IMC no tecido adiposo human | 11 | 12,752 | $0.000536 | meta_analysis | yes |
| 1 | en | Is UCP1 expression related to age in brown adipose tissue of mic | 11 | 12,768 | $0.000536 | none | **no** |
| 1 | pt | A expressao de UCP1 se relaciona com a idade no tecido adiposo m | 11 | 12,763 | $0.000536 | compare_samples | **no** |
| 2 | en | Is adiponectin expression different between obese and lean peopl | 11 | 12,753 | $0.000536 | compare_samples | yes |
| 2 | pt | A expressao de adiponectina difere entre pessoas obesas e magras | 11 | 12,759 | $0.000536 | compare_samples | yes |
| 3 | en | (setup) Does leptin expression increase with BMI in human adipos | NA | 12,747 | $0.000535 | meta_analysis | yes |
| 3 | en | Does that differ between the sexes? | 12 | 14,115 | $0.000593 | compare_samples | yes |
| 3 | pt | (setup) A expressao de leptina aumenta com o IMC no tecido adipo | NA | 12,752 | $0.000536 | meta_analysis | yes |
| 3 | pt | Isso muda entre os sexos? | 12 | 14,120 | $0.000593 | compare_samples | yes |
| 1 | en | is Leptin expressed in tissues other than adipose tissue? | 11 | 12,747 | $0.000535 | across_datasets | yes |
| 1 | en | is UCP1 expressed in the stromal vascular fraction of adipose ti | 11 | 12,752 | $0.000536 | none | yes |
| 4 | en | Is shh expression higher in the zebrafish brain after hypoxia? | 11 | 12,744 | $0.000535 | none | yes |
| 4 | pt | A expressao de shh e maior no cerebro de peixe-zebra apos hipoxi | 11 | 12,749 | $0.000535 | none | yes |

## Per level

| level | what it needs | questions | mean tokens | mean cost | as expected |
|---|---|---|---|---|---|
| 1 | one gene, one variable | 6 | 12,755 | $0.000536 | 4/6 |
| 2 | several datasets pooled | 2 | 12,756 | $0.000536 | 2/2 |
| 3 | a follow-up that splits the previous answer | 4 | 13,434 | $0.000564 | 4/4 |
| 4 | nothing in the hub can answer it | 2 | 12,746 | $0.000535 | 2/2 |

## Per language

| language | questions | mean tokens | mean cost | as expected |
|---|---|---|---|---|
| en | 8 | 12,922 | $0.000543 | 7/8 |
| pt | 6 | 12,982 | $0.000545 | 5/6 |

Total for this run: 181,268 input tokens, $0.007613.

## The same question, asked five times

"Is UCP1 expression related to age in brown adipose tissue of mice?", and its Portuguese translation, 5 times each. This question sits
near the confidence threshold on purpose: the catalog holds `Age`, `Age.Class`
and `Differentiation.day`, so the model spreads its probability across them.

| lang | run | variable chosen | confidence | outcome |
|---|---|---|---|---|
| en | 1 | Age | 0.60 | compare_samples |
| en | 2 | Age | 0.50 | ambiguous |
| en | 3 | Age | 0.54 | ambiguous |
| en | 4 | Age | 0.58 | compare_samples |
| en | 5 | Age | 0.60 | compare_samples |
| pt | 1 | Age | 0.55 | compare_samples |
| pt | 2 | Age | 0.52 | ambiguous |
| pt | 3 | Age | 0.59 | compare_samples |
| pt | 4 | Age | 0.63 | compare_samples |
| pt | 5 | Age | 0.56 | compare_samples |

Distinct outcomes: 2 in English, 2 in Portuguese. Mean confidence 0.56 and 0.57.

## What "as expected" means

The expected outcome is what the tab *should* do, which is not always a plot.
The stromal vascular fraction question is expected to be **refused**: the
collection holds no such cell fraction, and saying so is the answer. The two
UCP1-and-age questions are expected to come back as **ambiguous**: the catalog
holds `Age`, `Age.Class` and `Differentiation.day`, the model spreads its
probability across them, and the tab asks which was meant instead of picking
one and drawing a plausible wrong answer. The zebrafish questions are expected
to be refused, because nothing in the collection is zebrafish.

Regenerate with `Rscript tools/jev-cost.R`. Every row is a real request.
