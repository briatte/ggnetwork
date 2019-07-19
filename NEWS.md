# ggnetwork 0.5.6

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
  

ggnetwork 0.5.4 (2017-07-XX)
----------------------------

Changes

* Added native support for igraph.  Thanks to [Jake Fisher](www.src.isr.umich.edu/people/jake-fisher/).


ggnetwork 0.5.3 (2016-06-XX)
----------------------------

Added Travis CI.

FIXES

* Export ggplot2::Stat to enable loading ggnetwork first (#14). Thanks to Tyler Rinker.

* Built with the new version of ggproto (#11).

* Safer calls to the sna package (#9). Thanks to Michał Bojanowski.

ggnetwork 0.5.2 (2016-05-01)
----------------------------

FIXES

* Fixed a bug that removed labels from strictly vertical or strictly horizontal edges (#5).

* Fixed a small documentation issue that was corrected in roxygen 5.0.2 (#4).

CHANGES

* Support for segment colors in all geoms using ggrepel 0.5.1 (#3).

* Added some acknowledgements to the README and links to the DESCRIPTION.


ggnetwork 0.5.1 (2016-03-25)
----------------------------

First CRAN release.
