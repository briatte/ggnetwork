ggnetwork 0.5.6
===============

Due to a bugfix in `statnet.common` 4.2.0, `ggnetwork` now requires R ≥ 3.5 (#45).


ggnetwork 0.5.5
===============

Many thanks to [Mickaël Canouil](https://github.com/mcanouil), who contributed all changes below (#42).

## Repository changes

* Add `README.Rmd` (replace `README.md`), 
  + use badges.
  + list all GitHub contributors.
  + add Contributing, Code of Conduct Issue Template and Support markdown file (`usethis`).

* Improve `.travis.yml`,
  + use `pkgdown` for website deployment.
      - add `_pkgdown.yml` configuration file.
  + use `covr` for code coverage.
      - add default `codecov.yml` configuration file.
  
* Add `ggnetwork.Rproj`, for ease of use within Rstudio.

## Minor improvements and fixes

* In `DESCRIPTION`,
  + update RoxygenNote version.
  + remove `ggplot2` from `Enhances` field.
  + add `Collate` field to load sequentially the functions.
  + move `igraph` to `Imports` field.
  
* Remove `inst/doc/` directory, *i.e.*, the vignette is part of the `pkgdown` website.

* Use tidy code style.

* In `R/fortify-igraph.R` and `R/fortify-network.R`, 
  + use subsetting functions instead of `with` and `transform` (*i.e.*, intended to be use interactively).
  + fix issue from CRAN check with undefined global variables.
  + remove namespace loading for `sna` package (*i.e.*, `gplot.layout.*` functions).
  
* In `R/geom-nodes.R` and `R/geom-edges.R`, 
  + remove unnecessary "@importFom".
  + add missing function packages prefix.
  
* In `R/ggnetwork.R`, `switch` and `tryCatch` to make the `igraph` and `network` testing consistent.
  
* In `R/utilities.R`, reexport `fortify` and `unit` from `ggplot2`.


ggnetwork 0.5.4
===============

Added native support for `igraph`. Thanks to [Jake Fisher](https://github.com/jcfisher) (#25).

## Minor improvements and fixes

Fix for network layouts with constant coordinates. Thanks to [Kipp Johnson](https://github.com/kippjohnson) (#32).


ggnetwork 0.5.3
===============

## Repository changes

Added Travis CI. Thanks to [Kohei Watanabe](https://github.com/koheiw) (#26).

## Minor improvements and fixes

* Export `ggplot2::Stat` to enable loading `ggnetwork` first (#14). Thanks to [Tyler Rinker](https://github.com/trinker).

* Built with the new version of `ggproto` (#11).

* Safer calls to the `sna` package (#9). Thanks to [Michał Bojanowski](https://github.com/mbojan).


ggnetwork 0.5.2 (2016-05-01)
============================

## Minor improvements and fixes

* Fixed a bug that removed labels from strictly vertical or strictly horizontal edges (#5).

* Fixed a small documentation issue that was corrected in `roxygen` 5.0.2 (#4).

CHANGES

* Support for segment colors in all geoms using `ggrepel` 0.5.1 (#3).

* Added some acknowledgements to the README and links to the DESCRIPTION.


ggnetwork 0.5.1 (2016-03-25)
============================

First CRAN release.
