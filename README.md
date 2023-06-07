# scorz

## Score Multi-Attribute Instruments

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![ready4](https://img.shields.io/badge/ready4-description-navy?style=flat&labelColor=black&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAFzUkdCAK7OHOkAAAAEZ0FNQQAAsY8L/GEFAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAIXRFWHRDcmVhdGlvbiBUaW1lADIwMjI6MDM6MDcgMTY6MTM6NTPZeG5UAAABa0lEQVQ4T4WT607CQBCFpyUi3qIR0eAfNfCi/vENfEgENIAIlcJ6vr1oLaZOerJzdst0zpklc49nznqHZs6ZfWwDem1xM1sqXwtXkb8rL4SuOLEoLXPPXWfD01Dg9dPsrTQbngQ+EZ+LDyIfiy/FHyIfFZbbTslWKOOqxx/uWBPSfp07FahGlqlNfWGqL9HNfBO+CAfwdO55WS8g4MFML834sfJVA9e7vwsg50aGohncdmRojV9XeL+jArRNmZxVSJ4Acj3NHqARdyeFJqC2KJiCfKE9zsfxnNYTl5TcCtmNMcwY/ZXf+3wdzzVza2vj4iCaq3d1R/bvwVSH6IPjNIUHx0FSNZA7WquDqOVb35+eiO8h7Oe+vRfp0a3yGtFMDuiAIg2R20YaVwJ3Hj+4kehO/J/I7VJ/jHtpvBP6mrHnR4EzdyQ0xI8HhM8jUiChxVpDK3iVuadzx43yRdI4E2d0gNtX74TCs419AR8YEST/cHPBAAAAAElFTkSuQmCC)](https://www.ready4-dev.com/toolkits/)
[![R-CMD-check](https://github.com/ready4-dev/scorz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ready4-dev/scorz/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5722708.svg)](https://doi.org/10.5281/zenodo.5722708)
<!-- badges: end -->

Tools to summarise multiple questionnaire responses in a
single index measure (e.g. calculating health utility from
multi-attribute utility instrument items) for use in models developed
with the ready4 framework (https://www.ready4-dev.com/).  The main
motivation for this package is to facilitate automated scoring, using
published publicly available scoring algorithms, of measures relevant
to mental health systems models.  This development version of the
scorz package has been made available as part of the process of
testing and documenting the package.  If you have any questions,
please contact the authors (matthew.hamilton@orygen.org.au).

To install a development version of this software, run the following commands in your R console:

```r
utils::install.packages("devtools")

devtools::install_github("ready4-dev/scorz")

```
