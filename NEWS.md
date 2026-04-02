# colorplane 0.6.1

* Fixed alpha compositing in `blend_colors()` so composite alpha is preserved
  and fully transparent blends remain finite.
* Fixed `map_colors()` for zero-width intensity ranges, all-missing intensity
  vectors, and explicit alpha application without thresholding.
* Added regression tests for alpha handling, degenerate mapping cases, and
  constructor validation.
* Refreshed package metadata and generated documentation for the current API.

# colorplane 0.5.0

* Added a `NEWS.md` file to track changes to the package.
