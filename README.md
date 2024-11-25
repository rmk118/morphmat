
# morphmat

\<img src=“man/figures/logo.png” alt=“Hex sticker logo for morphmat
package” align=“right” height=“139”, class=“pkgdown-hide”/\>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

A compilation of methods used to estimate size at (sexual) maturity
based on morphometric data, most commonly applied to crabs, lobsters,
and other crustaceans. Approaches include modeling approaches based on
piecewise/segmented linear regression as well as numerous
clustering-based methods.

`morphmat` is intended to help fisheries scientists and managers to
implement the multitude of computational methods that have been
developed for estimating crustacean size at maturity. This package is
being developed as part of my graduate work at the University of Maine.
**This is still a work in progress and should not yet be used for
research purposes.**

`morphmat` will include user-friendly functions for estimating size at
morphometric maturity, along with guidelines for choosing the optimal
method for a given dataset and correctly interpreting results. Both
novel and historically popular (e.g., broken-stick regression) methods
will be included, and users will be able to obtain SM50 estimates from
nearly a dozen different approaches with a single function call.
Crucially, by combining code for the various models within a single
package, morphmat will enable the application of multi-model inference
to SM50 estimation, avoiding the arbitrary selection of a single “best”
model ([Katsanevakis 2006](#ref-katsanevakis2006)). By increasing the
accessibility of modern statistical methods for estimating SM50, I hope
to facilitate the widespread adoption of improved SM50 modeling
procedures.

`morphmat` will include versions of the methods implemented in these
existing GitHub repositories:

|  |  |  |  |
|----|----|----|----|
| **Type** | **Authors** | **GitHub repository** | **Description/notes** |
| Package | Josymar Torrejon-Magallanes | [ejosymart/sizeMat](https://github.com/ejosymart/sizeMat) | [sizeMat: An R Package to Estimate Size at Sexual Maturity](https://cran.r-project.org/web/packages/sizeMat/vignettes/sizeMat.html) |
| Package | Rodrigo Sant’Ana, Fernando Mayer | [rodrigosantana/Regrans: Fits Segmented Linear Regression Models](https://github.com/rodrigosantana/Regrans) | Older repository: [fernandomayer/Regrans](https://github.com/fernandomayer/Regrans/blob/master/change.point.R) |
| Package | Derek Sonderegger | [siZer](https://cran.r-project.org/web/packages/SiZer/index.html) | [Conduct size at morphometric maturity analysis (SMM) and plot results using the SiZer extension package](https://zenodo.org/records/5002120) by Olson, Andrew P., Siddon, Chris E., and Eckert, Ginny L. |
| Package | Vito M. R. Muggeo | [segmented](https://cran.r-project.org/web/packages/segmented/index.html) | [package reference manual](https://cran.r-project.org/web/packages/segmented/segmented.pdf) |
| Package | Youyi Fong | [chngpt](https://doi.org/10.32614/CRAN.package.chngpt) | [Fitting Threshold Regression Models Using chngpt](https://cran.r-project.org/web/packages/chngpt/vignettes/chngpt-vignette.pdf) (vignette) |
| Package | Tobie Surette | [TobieSurette/gulf.stats](https://github.com/TobieSurette/gulf.stats) | See files [morphometric.maturity.R](https://github.com/TobieSurette/gulf.stats/blob/master/R/morphometric.maturity.R) and [morphometry.R](https://github.com/TobieSurette/gulf.stats/blob/master/R/morphometry.R) |
| R scripts | Bradley Stevens | [Crabman52/Crustacean_Sensation](https://github.com/Crabman52/Crustacean_Sensation) | Include three different methods for separating mature from immature crabs based on allometric growth of body parts, with code based on Somerton’s (1980) program MATURE |

The following scripts do not use morphometric data and require
individuals to already be classified as mature or immature. They provide
various examples of how to fit maturity~length binomial models to
estimate SM50 and obtain confidence intervals. These tools can also be
used to calculate size at maturity for non-crustacean fisheries. For a
comprehensive examination of this type of model, see ([Mainguy et al.
2024](#ref-mainguy2024)).

|  |  |  |  |
|----|----|----|----|
| **Type** | **Authors** | **GitHub repository** | **Description/notes** |
| R scripts | Lucas Rodrigues | [lvcasrodrigues/maturity_at_size](https://github.com/lvcasrodrigues/maturity_at_size) | Does not use morphometric data; takes size classes with known numbers of mature individuals per size class and fits a binomial model (frequentist or Bayesian). Finds SM50 by generating new data from the model rather than from ratio of parameters |
| R scripts | Mainguy et al. | [rafamoral/L50](https://github.com/rafamoral/L50): Monitoring reproduction in fish: Assessing the adequacy of ogives and the predicted uncertainty of their *L*<sub>50</sub> estimates for more reliable biological inferences | Over a dozen methods for estimating L50 values and obtaining confidence intervals from (frequentist or Bayesian) binomial models (Delta, Fieller, bootstrap resampling, profile-likelihood, etc.). See accompanying manuscript by Mainguy et al. ([2024](#ref-mainguy2024)). |
| R script | [Alastair V. Harry](https://scholar.google.com.au/citations?user=hb4nzPYAAAAJ&hl) | [Alharry/Maturity.ogive.R](https://gist.github.com/alharry/4576675) | Includes bootstrap resampling to obtain 95% CIs |

## Installation

You can install the development version of morphmat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rmk118/morphmat")
```

## Examples

``` r
library(morphmat)

set.seed(12) # for reproducibility when generating the simulated data

# Generate a simulated dataset with known size at maturity
fc <- fake_crustaceans(n = 100, L50 = 100, allo_params = c(1, 0.2, 1.1, 0.2))
```

### Broken-stick/piecewise regression methods

REGRANS:

``` r
regrans_fun(fc, "x", "y", verbose = FALSE)
#> [1] 89.43822
```

Two-line logistic:

``` r
two_line_logistic(fc, xvar = "x", yvar = "y", verbose = FALSE)
#>     SM50 
#> 104.7633
```

Two-line Stevens:

``` r
two_line_stevens(fc, xvar = "x", yvar = "y", verbose = FALSE)
#>   breakpoint intersection 
#>     106.0655    1383.9744
```

Broken-stick Stevens:

``` r
broken_stick_stevens(fc, xvar = "x", yvar = "y", verbose = FALSE)
#> [1] 91.10524
```

Other packages:

``` r
# segmented
# chngpt
```

Compare estimates from all regression methods:

``` r
broken_stick(fc, xvar = "x", yvar = "y", method = "all")
#>    chngpt segmented   REGRANS   Stevens 
#>  89.44561  88.71463  89.43822  91.10524
```

### Clustering methods

Somerton method:

``` r
out_df <- somerton_fun(fc, xvar = "x", yvar = "y")[[1]]
mod <- glm(data = out_df, pred_mat_num ~ x, family = binomial(link = "logit"))
unname(-coef(mod)[1] / coef(mod)[2])
#> [1] 102.37
```

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-katsanevakis2006" class="csl-entry">

Katsanevakis, Stelios. 2006. “Modelling Fish Growth: Model Selection,
Multi-Model Inference and Model Selection Uncertainty.” *Fisheries
Research* 81 (2): 229–35.
<https://doi.org/10.1016/j.fishres.2006.07.002>.

</div>

<div id="ref-mainguy2024" class="csl-entry">

Mainguy, Julien, Martin Bélanger, Geneviève Ouellet-Cauchon, and Rafael
de Andrade Moral. 2024. “Monitoring Reproduction in Fish: Assessing the
Adequacy of Ogives and the Predicted Uncertainty of Their *L*50
Estimates for More Reliable Biological Inferences.” *Fisheries Research*
269 (January): 106863. <https://doi.org/10.1016/j.fishres.2023.106863>.

</div>

</div>
