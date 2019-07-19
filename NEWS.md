ggnetwork 0.5.6
============================

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
  + Remove `ggplot2` from `Enhances` field.
  
* Remove `inst/doc/` directory, *i.e.*, the vignette is part of the `pkgdown` website.

* Use tidy code style.

* In `R/fortify-igraph.R` and `R/fortify-network.R`, 
  + use subsetting functions instead of `with` and `transform` (*i.e.*, intended to be use interactively).
  + fix issue from CRAN check with undefined global variables.



ggnetwork 0.5.5 (2017-08-XX)
============================
  

ggnetwork 0.5.4 (2017-07-XX)
============================

## Minor improvements and fixes

* Added native support for igraph.  Thanks to [Jake Fisher](www.src.isr.umich.edu/people/jake-fisher/).


ggnetwork 0.5.3 (2016-06-XX)
============================

## Repository changes

* Added Travis CI.

## Minor improvements and fixes

* Export ggplot2::Stat to enable loading ggnetwork first (#14). Thanks to Tyler Rinker.

* Built with the new version of ggproto (#11).

* Safer calls to the sna package (#9). Thanks to Michał Bojanowski.


ggnetwork 0.5.2 (2016-05-01)
============================

## Minor improvements and fixes

* Fixed a bug that removed labels from strictly vertical or strictly horizontal edges (#5).

* Fixed a small documentation issue that was corrected in roxygen 5.0.2 (#4).

* Support for segment colors in all geoms using ggrepel 0.5.1 (#3).

* Added some acknowledgements to the README and links to the DESCRIPTION.


ggnetwork 0.5.1 (2016-03-25)
============================

First CRAN release.
