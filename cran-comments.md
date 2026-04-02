## Resubmission
This is a resubmission. In this version I have:

* fixed alpha compositing in `blend_colors()` so composite alpha is preserved
  correctly and fully transparent blends remain finite
* fixed `map_colors()` for zero-width intensity ranges, all-missing intensity
  vectors, and scalar or per-element alpha handling
* added regression tests covering the corrected blending and color-mapping edge
  cases
* refreshed package metadata and regenerated the Rd/pkgdown documentation to
  match the current API

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

The package was checked with:

* `env LC_ALL=en_CA.UTF-8 R CMD build .`
* `env LC_ALL=en_CA.UTF-8 R CMD check --no-manual colorplane_0.6.1.tar.gz`

## Downstream dependencies

No downstream dependencies.
