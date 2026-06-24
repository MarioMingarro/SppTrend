# SppTrend 0.5

## Breaking changes

* Functions `overall_trend()`, `spp_trend()`, and `spp_strategy()` have been
  **removed**. The new workflow uses `spp_trend_spatial_ecef()` and
  `spp_trend_environmental()` instead.

* Input column names for the new core functions are now **capitalised**:
  `Species`, `Year`, `Month`, `Latitude`, `Longitude`, `Temperature`,
  `Elevation`.

## New functions

* `spp_trend_spatial_ecef()`: Analyses temporal changes in species geographic
  position using Earth-Centred Earth-Fixed (ECEF) Cartesian vector analysis on
  the WGS84 ellipsoid. Computes monthly spatial centroids, fits a multivariate
  OLS regression, projects the resulting slope vector onto the local tangent
  plane, and classifies each species as SA (Spatial Adaptation), SD (Spatial
  Discordance), or SC (Spatial Conformance) relative to a global reference
  vector via Monte Carlo covariance-based simulations.

* `spp_trend_environmental()`: Models temporal trends in temperature and/or
  elevation for each species using species-specific OLS regressions compared
  against a pooled global slope via Welch–Satterthwaite t-tests. Applies
  Benjamini–Hochberg FDR correction and an optional sample-size-dependent
  significance penalty (α_eff = α_ref × √(100/N) for N > 100). Classifies
  species as TT (Thermal Tolerance), TA (Thermal Adjustment), TC (Thermal
  Conformance), SA, SD, or SC for elevation.

## Unchanged functions

* `get_elevation()`, `get_era5_tme()`, and `get_fast_info()` are unchanged.

## New dependencies

* `data.table` added to `Imports` (required by `spp_trend_environmental()`).

## Documentation

* README completely rewritten to describe the new two-function workflow and
  the ECEF spatial methodology.
* Package-level documentation updated in `SppTrend-package.R`.
* Roxygen documentation for both new functions aligned with the manuscript:
  Mingarro, García-Roselló and Lobo (2026), *Ecological Informatics*.

# SppTrend 0.4

* Initial CRAN release with `overall_trend()`, `spp_trend()`,
  `spp_strategy()`, `get_elevation()`, `get_era5_tme()`, and
  `get_fast_info()`.
* Methodology described in Lobo et al. (2023) <doi:10.1002/ece3.10674>.
