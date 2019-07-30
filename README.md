
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggnetwork

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub
tag](https://img.shields.io/github/tag/briatte/ggnetwork.svg?label=%22latest%20tag%22)](https://github.com/briatte/ggnetwork)
[![Travis-CI Build
Status](https://travis-ci.org/briatte/ggnetwork.svg?branch=master)](https://travis-ci.org/briatte/ggnetwork)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/briatte/ggnetwork?branch=master&svg=true)](https://ci.appveyor.com/project/briatte/ggnetwork)
[![Coverage Status
(codecov)](https://codecov.io/gh/briatte/ggnetwork/branch/master/graph/badge.svg)](https://codecov.io/gh/briatte/ggnetwork)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/ggnetwork)](https://cran.r-project.org/package=ggnetwork)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/ggnetwork)](https://cran.r-project.org/web/checks/check_results_ggnetwork.html)
[![CRAN\_Download\_total](http://cranlogs.r-pkg.org/badges/grand-total/ggnetwork)](https://cran.r-project.org/package=ggnetwork)
<!-- badges: end -->

This package allows to pass network objects to
[`ggplot2`](http://ggplot2.org/) and provides geometries to plot their
elements.

## Get started

You can install the released version of `ggnetwork` [from
CRAN](https://cran.r-project.org/package=ggnetwork) with:

``` r
install.packages("ggnetwork")
```

And the development version [from
GitHub](https://github.com/briatte/ggnetwork) with:

``` r
# install.packages("remotes")
remotes::install_github("briatte/ggnetwork")
```

**IMPORTANT:** the `ggnetwork` package depends on R 3.5+ and on
`ggplot2` version 2.0.0+.

## Documentation

The package vignette contains [detailed
examples](https://briatte.github.io/ggnetwork/) of how to use its
`fortify` method and each of its geometries.

For further examples that use `ggnetwork` with other packages to produce
animated graphs, see James Curley’s slides on “[Interactive and Dynamic
Network Visualization in
R](http://curleylab.psych.columbia.edu/netviz/)” (2016). For even more
options, see Katherine Ognyanova’s tutorial “[Network visualization with
R](https://kateto.net/network-visualization) (2019).

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
[on GitHub](https://github.com/briatte/ggnetwork/issues).

For questions and other discussion, please contact the package
maintainer.

## Citation

You can get a citation for the package from R:

``` r
citation("ggnetwork")
```

## See also

The `ggnetwork` package was written within a larger development effort
around network visualization with `ggplot2`, on which you can read the
following article:

> Sam Tyner, François Briatte and Heike Hofmann, “[Network Visualization
> with `ggplot2`](https://doi.org/10.32614/RJ-2017-023),” *The R
> Journal* 9(1): 27–59, 2017.

The article also covers the related packages
[`geomnet`](https://github.com/sctyner/geomnet) and
[`ggnet`](https://github.com/briatte/ggnet). It does not cover the more
recent [`ggraph`](https://github.com/thomasp85/ggraph) and
[`tidygraph`](https://github.com/thomasp85/tidygraph), although you
should turn to those if you need a highly extensive way to handle ‘tidy’
networks in `ggplot2`.

## Thanks

Thanks to [@achmurzy](https://github.com/achmurzy),
[@andrewd789](https://github.com/andrewd789),
[@ArtemSokolov](https://github.com/ArtemSokolov),
[@aterhorst](https://github.com/aterhorst),
[@emillykkejensen](https://github.com/emillykkejensen),
[@evinhas](https://github.com/evinhas),
[@FinScience](https://github.com/FinScience),
[@ghost](https://github.com/ghost),
[@instantkaffee](https://github.com/instantkaffee),
[@jalapic](https://github.com/jalapic),
[@jcfisher](https://github.com/jcfisher),
[@jfaganUK](https://github.com/jfaganUK),
[@kippjohnson](https://github.com/kippjohnson),
[@koheiw](https://github.com/koheiw),
[@mbojan](https://github.com/mbojan),
[@mcanouil](https://github.com/mcanouil),
[@mgagliol](https://github.com/mgagliol),
[@mhairi](https://github.com/mhairi),
[@minimaxir](https://github.com/minimaxir),
[@mkarikom](https://github.com/mkarikom),
[@nick-youngblut](https://github.com/nick-youngblut),
[@SantiFilippo](https://github.com/SantiFilippo),
[@sciabolazza](https://github.com/sciabolazza),
[@sctyner](https://github.com/sctyner),
[@trinker](https://github.com/trinker),
[@zachcp](https://github.com/zachcp) and two anonymous *[R
Journal](https://journal.r-project.org/)* reviewers.
[@heike](https://github.com/heike) and
[@ethen8181](https://github.com/ethen8181) also helped with the tricky
issue of having arrows on directed edges, while
[@sumtxt](https://github.com/sumtxt) inspired this package as well as
its predecessor, the `ggnet` package, which also benefitted from
discussions with [@pedroj](https://github.com/pedroj) and Bertrand
Sudre.

-----

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
