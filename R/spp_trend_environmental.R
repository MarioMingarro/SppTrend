#' Analyse Long-Term Environmental Niche Trends Across Species
#'
#' @description
#' `spp_trend_environmental()` models temporal trends in environmental
#' variables—Temperature and/or Elevation—for each species in a community
#' dataset and evaluates whether species-specific shifts differ from the
#' pooled community trend.
#'
#' For every requested response variable the function fits:
#' \enumerate{
#' \item A **global ordinary least-squares (OLS) regression** pooled across
#' all valid species, capturing the average community-level temporal
#' trend.
#' \item An **independent OLS regression per species**, yielding
#' species-specific temporal slopes together with their standard errors,
#' t-statistics, and 95 \% confidence intervals.
#' }
#'
#' Species-specific slopes are then compared against the global slope using
#' **Welch–Satterthwaite t-tests**. Raw p-values from both the individual
#' species regressions and the slope-difference tests are corrected for
#' multiple testing with the **Benjamini–Hochberg False Discovery Rate (FDR)**
#' procedure, applied separately for each response variable. An optional
#' sample-size correction penalises the significance threshold for
#' well-sampled species (N > 100).
#'
#' @section Classification system:
#' Each species is assigned an **environmental response class** reflecting the
#' direction and significance of its temporal trend (or its deviation from the
#' global trend, depending on `individual_lm_threshold_type`):
#'
#' \describe{
#' \item{`"TT"` — Thermal Tolerance}{Temperature response.The species is
#' moving toward warmer environments over time: its individual slope (or
#' its deviation from the global slope, under `"slopediff"`) is
#' positive, statistically significant, and exceeds the minimum
#' ecological threshold.}
#' \item{`"TA"` — Thermal Adjustment}{Temperature response.The species is
#' shifting toward cooler environments: slope (or deviation) is
#' negative, significant, and exceeds the threshold.}
#' \item{`"TC"` — Thermal Conformance}{Temperature response.Default class
#' assigned when the individual trend is non-significant or not
#' distinguishable from the global trend.}
#' \item{`"SA"` — Spatial Adaptation}{Elevation response.The species is
#' tracking higher elevations over time: slope (or deviation) is
#' positive, significant, and exceeds the threshold.}
#' \item{`"SD"` — Spatial Discordance}{Elevation response.The species is
#' shifting to lower elevations: slope (or deviation) is negative,
#' significant, and exceeds the threshold.}
#' \item{`"SC"` — Spatial Conformance}{Elevation response.Default class
#' when the elevation trend is non-significant or indistinguishable from
#' the global trend.}
#' }
#'
#' @section Time variable:
#' Internally the function constructs a continuous, mean-centred time
#' predictor (`time_cont_c`) from `Year` and `Month` as
#' `Year + (Month - 1) / 12 - mean(...)`. Slopes are therefore expressed in
#' **environmental units per year** (°C yr⁻¹ for Temperature, m yr⁻¹ for
#' Elevation).
#'
#' @param data A data frame or `data.table` containing the raw observational
#' records.Must include the following columns (exact names required):
#' \describe{
#' \item{`Species`}{Character or factor.Taxonomic name or identifier for
#' each observation.}
#' \item{`Year`}{Integer or numeric.Calendar year of the observation.}
#' \item{`Month`}{Integer or numeric.Calendar month (1–12) of the
#' observation.}
#' \item{`Temperature`}{Numeric.Environmental temperature associated with
#' the observation (e.g. mean monthly temperature in °C at the
#' sampling location).Required when `"Temperature"` is included in
#' `responses`.}
#' \item{`Elevation`}{Numeric.Elevation (m a.s.l.) associated with the
#' observation.Required when `"Elevation"` is included in
#' `responses`.}
#' }
#' Rows with `NA` in `Species`, `Year`, or `Month`, and rows where `Month`
#' is outside 1–12, are silently removed before analysis.
#'
#' @param spp Character vector or `NULL` (default `NULL`).Names of the
#' species to include in the analysis.If `NULL`, all unique species present
#' in `data` are used.Species named in `spp` that are absent from `data`
#' trigger a warning; if none of the requested species are found the function
#' stops with an error.
#'
#' @param responses Character vector specifying which environmental variables
#' to model.Allowed values are `"Temperature"`, `"Elevation"`, or both
#' (default `c("Temperature", "Elevation")`).The vector is matched against
#' the allowed set in the order `c("Temperature", "Elevation")`, so any
#' duplicate or out-of-order entries are normalised automatically.
#'
#' @param min_records Positive integer (default `20`).Minimum number of
#' non-missing records a species must have for a given response variable to
#' be retained in the analysis.Species below this threshold are excluded
#' and reported in `species_filter`.
#'
#' @param min_years Positive integer (default `5`).Minimum number of
#' distinct calendar years a species must have data in (for a given response
#' variable) to be retained.Acts alongside `min_records`; a species must
#' satisfy **both** criteria.
#'
#' @param alpha_ref Numeric scalar in (0, 1) (default `0.05`).Baseline
#' significance level used for all hypothesis tests and confidence-interval
#' calculations.This value is also the starting point for the
#' sample-size correction applied when `use_Ncorrected_alpha = TRUE`.
#'
#' @param use_Ncorrected_alpha Logical (default `TRUE`).When `TRUE`, the
#' effective significance threshold for any individual species with more
#' than 100 records is reduced according to
#' \deqn{\alpha_{\text{eff}} = \alpha_{\text{ref}} \times \sqrt{100 / N}}{
#' alpha_eff = alpha_ref * sqrt(100 / N)}
#' where \eqn{N} is the number of records for that species and response
#' variable.This penalises the inflated statistical power that comes with
#' large sample sizes, requiring a larger observed effect before a species is
#' classified as significant.When `FALSE`, `alpha_ref` is applied uniformly
#' to all species.
#'
#' @param individual_lm_min_thresholds Named numeric vector or `NULL`
#' (default `c(Temperature = 0.01, Elevation = 50)`).Minimum ecological
#' effect size that must be exceeded before a species can be assigned a
#' non-conformance class (TT, TA, SA, or SD).Names must be a subset of
#' `c("Temperature", "Elevation")` and all values must be positive.The
#' quantity being thresholded depends on `individual_lm_threshold_type`:
#' \describe{
#' \item{`"speciesslope"`}{ (default) The absolute value of the species-specific
#' temporal slope must exceed the threshold (e.g. |slope| > 0.01
#' °C yr⁻¹ for Temperature). Evaluates how much the species is shifting
#' in absolute terms.}
#' \item{`"slopediff"`}{The absolute value of the difference between the
#' species slope and the global slope must exceed the threshold.
#' Evaluates how much the species is shifting relative to the average
#' community trend. }
#' }
#' If `NULL`, no ecological threshold is applied and classification relies
#' entirely on statistical significance.Must be `NULL` jointly with
#' `individual_lm_threshold_type`, or non-`NULL` jointly; mixed states
#' produce an error.
#'
#' @param individual_lm_threshold_type Character scalar (default
#' `"speciesslope"`).Determines which quantity is compared against
#' `individual_lm_min_thresholds`.Must be one of:
#' \describe{
#' \item{`"speciesslope"`}{Threshold is applied to the absolute
#' species-specific slope.}
#' \item{`"slopediff"`}{Threshold is applied to the absolute difference
#' between the species slope and the global slope.}
#' }
#'
#' @param verbose Logical (default `TRUE`).When `TRUE`, the function prints
#' progress messages to the console via `message()`, including the number of
#' species retained or excluded at the filtering step for each response
#' variable.Set to `FALSE` to suppress all messages (useful inside loops
#' or parallel computations).
#'
#' @return A named list with the following elements:
#'
#' \describe{
#'
#' \item{`params`}{Named list storing the values of all function arguments
#' (except `data` and `spp`) as supplied by the caller.Useful for
#' reproducibility and logging.}
#'
#' \item{`species_filter`}{Data frame with one row per species × response
#' combination, summarising the filtering step.Columns:
#' \describe{
#' \item{`Species`}{Character.Species name.}
#' \item{`response`}{Character.Response variable (`"Temperature"` or
#' `"Elevation"`).}
#' \item{`n_records`}{Integer.Total number of non-missing records for
#' this species and response.}
#' \item{`n_years`}{Integer.Number of distinct calendar years with at
#' least one non-missing record.}
#' \item{`retained`}{Logical.`TRUE` if the species meets both
#' `min_records` and `min_years` thresholds and was therefore
#' included in the modelling step.}
#' }
#' }
#'
#' \item{`environmental_global_lm`}{Data frame with one row per response
#' variable containing statistics from the pooled (community-level) OLS
#' regression.Columns:
#' \describe{
#' \item{`response`}{Character.Response variable.}
#' \item{`model`}{Character.Always `"global_lm"`.}
#' \item{`global_lm_slope`}{Numeric.Estimated temporal slope of the
#' global model (units per year: °C yr⁻¹ or m yr⁻¹).}
#' \item{`global_lm_se`}{Numeric.Standard error of the global slope.}
#' \item{`global_lm_t`}{Numeric.t-statistic for the global slope
#' (H₀: slope = 0).}
#' \item{`global_lm_p_value`}{Numeric.Two-tailed p-value for the
#' global slope test.}
#' \item{`global_lm_conf_low`}{Numeric.Lower bound of the 95 \%
#' confidence interval for the global slope.}
#' \item{`global_lm_conf_high`}{Numeric.Upper bound of the 95 \%
#' confidence interval for the global slope.}
#' \item{`n_records`}{Integer.Total number of records used to fit the
#' global model (all valid records across retained species).}
#' \item{`n_years`}{Integer.Number of distinct calendar years
#' represented in the global model dataset.}
#' }
#' }
#'
#' \item{`environmental_individual_lm`}{Data frame with one row per species
#' × response combination for all species that passed the filtering step.
#' Contains species-level OLS statistics plus comparisons against the
#' global model.Columns:
#' \describe{
#' \item{`Species`}{Character.Species name.}
#' \item{`response`}{Character.Response variable.}
#' \item{`n_records`}{Integer.Number of records used to fit this
#' species model.}
#' \item{`n_years`}{Integer.Number of distinct years in the species
#' data for this response.}
#' \item{`individual_slope`}{Numeric.Estimated temporal slope for this
#' species (°C yr⁻¹ or m yr⁻¹).}
#' \item{`individual_slope_se`}{Numeric.Standard error of the
#' species-specific slope.}
#' \item{`individual_t`}{Numeric.t-statistic for the species slope
#' (H₀: slope = 0).}
#' \item{`individual_p_value`}{Numeric.Two-tailed p-value for the
#' species slope test (raw, before FDR correction).}
#' \item{`individual_conf_low`}{Numeric.Lower bound of the 95 \%
#' confidence interval for the species slope.}
#' \item{`individual_conf_high`}{Numeric.Upper bound of the 95 \%
#' confidence interval for the species slope.}
#' \item{`global_lm_slope`}{Numeric.Global slope for this response
#' variable (copied from `environmental_global_lm` for
#' convenience).}
#' \item{`slope_diff_signed`}{Numeric.Signed difference between the
#' species slope and the global slope
#' (`individual_slope − global_lm_slope`).Positive values
#' indicate the species trend is steeper than the community
#' average.}
#' \item{`slope_diff_direction`}{Character.Direction of
#' `slope_diff_signed`: `"positive"`, `"negative"`, or `"zero"`.}
#' \item{`slope_diff`}{Numeric.Absolute value of `slope_diff_signed`.}
#' \item{`slope_diff_t`}{Numeric.Welch–Satterthwaite t-statistic for
#' the test of equality between the species slope and the global
#' slope.}
#' \item{`slope_diff_df`}{Numeric.Welch–Satterthwaite degrees of
#' freedom for `slope_diff_t`.}
#' \item{`slope_diff_p_value`}{Numeric.Two-tailed p-value for the
#' slope-difference test (raw, before FDR correction).}
#' \item{`individual_direction`}{Character.Direction of the species
#' slope itself: `"positive"`, `"negative"`, or `"zero"`.}
#' \item{`individual_p_adj_fdr`}{Numeric.FDR-adjusted (BH method)
#' p-value for the species slope test, computed within each
#' response variable separately.}
#' \item{`slope_diff_p_adj_fdr`}{Numeric.FDR-adjusted (BH method)
#' p-value for the slope-difference test.}
#' \item{`alpha`}{Numeric.Effective significance threshold applied to
#' this species.Equals `alpha_ref` for species with ≤ 100
#' records, or the sample-size-corrected value when
#' `use_Ncorrected_alpha = TRUE` and N > 100.}
#' \item{`individual_significant`}{Logical.`TRUE` if
#' `individual_p_adj_fdr < alpha` for this species.}
#' \item{`slope_diff_significant`}{Logical.`TRUE` if
#' `slope_diff_p_adj_fdr < alpha` for this species.}
#' }
#' }
#'
#' \item{`environmental_comparison`}{Data frame extending
#' `environmental_individual_lm` with the classification columns added by
#' `classify_lm_individual_slope()`.Contains all the columns described
#' for `environmental_individual_lm` plus:
#' \describe{
#' \item{`individual_slope_above_threshold`}{Logical.`TRUE` if the
#' relevant quantity (species slope or slope difference, depending
#' on `individual_lm_threshold_type`) exceeds the minimum
#' ecological threshold defined in `individual_lm_min_thresholds`.
#' Always `TRUE` when no threshold is specified.Retained for
#' downstream quality audits.}
#' \item{`individual_response_class`}{Character.Final environmental
#' response classification for this species and response variable.
#' Possible values: `"TT"` (Thermal Tolerance), `"TA"` (Thermal
#' Adjustment), `"TC"` (Thermal Conformance) for Temperature;
#' `"SA"` (Spatial Adaptation), `"SD"` (Spatial Discordance),
#' `"SC"` (Spatial Conformance) for Elevation.A non-conformance
#' class (TT, TA, SA, SD) is assigned only when both
#' `individual_significant` and `slope_diff_significant` are
#' `TRUE` and `individual_slope_above_threshold` is `TRUE`.
#' Otherwise the species defaults to the conformance class (TC or
#' SC).}
#' }
#' }
#'
#' }
#'
#' @importFrom data.table data.table setkey uniqueN rbindlist :=
#' @importFrom stats lm.fit pt qt p.adjust
#'
#' @examples
#' # Minimal example with simulated data
#' set.seed(42)
#' n <- 600
#' sim_data <- data.frame(
#' Species = sample(paste0("Sp", 1:5), n, replace = TRUE),
#' Year = sample(2000:2020, n, replace = TRUE),
#' Month = sample(1:12, n, replace = TRUE),
#' Temperature = rnorm(n, mean = 15, sd = 3),
#' Elevation = rnorm(n, mean = 500, sd = 100))
#'
#' results <- spp_trend_environmental(
#' data = sim_data,
#' responses = c("Temperature", "Elevation"),
#' min_records = 20,
#' min_years = 5,
#' alpha_ref = 0.05,
#' use_Ncorrected_alpha = FALSE,
#' individual_lm_min_thresholds = c(Temperature = 0.01, Elevation = 50),
#' individual_lm_threshold_type = "speciesslope",
#' verbose = FALSE
#' )
#'
#' # Inspect the classification table
#' head(results$environmental_comparison)
#'
#' # Species retained per response
#' results$species_filter[results$species_filter$retained, ]
#'
#' # Global community trends
#' results$environmental_global_lm
#'
#' @export
spp_trend_environmental <- function(data,
                                    spp = NULL,
                                    responses = c("Temperature", "Elevation"),
                                    min_records = 20,
                                    min_years = 5,
                                    alpha_ref = 0.05,
                                    use_Ncorrected_alpha = TRUE,
                                    individual_lm_min_thresholds = c(Temperature = 0.01, Elevation = 50),
                                    individual_lm_threshold_type = "speciesslope",
                                    verbose = TRUE) {

        param_names <- names(formals())
        params_to_store <- setdiff(param_names, c("data", "spp"))
        param_values <- mget(params_to_store, sys.frame(sys.nframe()))

        required_cols <- c("Species", "Year", "Month")
        missing_required <- setdiff(required_cols, names(data))
        if (length(missing_required) > 0) {
                stop(
                        "Required column(s) not found: ",
                        paste(missing_required, collapse = ", ")
                )
        }

        allowed_thresholds <- c("Temperature", "Elevation")

        if (!is.null(individual_lm_min_thresholds) |
            !is.null(individual_lm_threshold_type)) {
                if (!is.null(individual_lm_min_thresholds) &
                    is.null(individual_lm_threshold_type)) {
                        stop(
                                "If param 'individual_lm_min_thresholds' is not null, param 'individual_lm_threshold_type' cannot be NULL."
                        )
                }
                if (is.null(individual_lm_min_thresholds) &
                    !is.null(individual_lm_threshold_type)) {
                        stop(
                                "If param 'individual_lm_threshold_type' is not null, param 'individual_lm_min_thresholds' cannot be NULL."
                        )
                }
                if (!is.null(individual_lm_min_thresholds) &
                    is.null(individual_lm_threshold_type)) {
                        stop(
                                "If param 'individual_lm_min_thresholds' is not null, param 'individual_lm_threshold_type' cannot be NULL."
                        )
                }
                if (!individual_lm_threshold_type %in% c("speciesslope", "slopediff")) {
                        stop(
                                "individual_lm_threshold_type should be 'speciesslope' or 'slopediff' "
                        )
                }
                if (is.null(names(individual_lm_min_thresholds))) {
                        stop(
                                "The values in 'individual_lm_min_thresholds' must be named as 'Temperature' and/or 'Elevation'"
                        )
                } else{
                        if (any(individual_lm_min_thresholds < 0)) {
                                stop(
                                        "The values in 'individual_lm_min_thresholds' must be positive"
                                )
                        }
                }
                not_allowed <- setdiff(names(individual_lm_min_thresholds),
                                       allowed_thresholds)
                if (length(not_allowed) > 0) {
                        stop(
                                "Not allowed parameter in'individual_lm_min_thresholds': ",
                                paste(not_allowed, collapse = ", ")
                        )
                }
        }

        Nref = 100

        allowed_responses <- c("Temperature", "Elevation")
        unknown_responses <- setdiff(responses, allowed_responses)
        if (is.null(responses) ||
            length(responses) == 0 || length(unknown_responses) > 0) {
                stop(
                        "Unknown response(s): ",
                        paste(unknown_responses, collapse = ", "),
                        ". Allowed responses are: ",
                        paste(allowed_responses, collapse = ", ")
                )
        }

        responses <- intersect(allowed_responses, responses)

        if (!requireNamespace("data.table", quietly = TRUE)) {
                stop("Package 'data.table' is required.")
        }

        dt <- data.table::data.table(
                Species = as.factor(as.character(data$Species)),
                Year = as.integer(data$Year),
                Month = as.integer(data$Month),
                Temperature = as.numeric(data$Temperature),
                Elevation = as.numeric(data$Elevation)
        )

        dt <- dt[!is.na(Species) & Species != "" &
                         !is.na(Year) & !is.na(Month) &
                         Month >= 1L & Month <= 12L]
        dt[, Species := droplevels(Species)]

        if (!is.null(spp)) {
                spp <- as.character(spp)
                found_spp <- intersect(spp, levels(dt$Species))
                if (length(found_spp) == 0)
                        stop("None of the species specified in 'spp' was found in the data.")
                if (length(found_spp) < length(spp))
                        warning("Some species specified in 'spp' were not found and will be ignored.")
                dt <- dt[Species %in% found_spp]
                dt[, Species := droplevels(Species)]
        }

        if (nrow(dt) == 0) {
                warning("No records remain after filtering Species, Year and Month.")
                return(
                        list(
                                species_filter = data.frame(),
                                environmental_global_lm = data.frame(),
                                environmental_individual_lm = data.frame(),
                                environmental_comparison = data.frame()
                        )
                )
        }

        time_cont_raw <- dt$Year + (dt$Month - 1L) / 12
        dt[, time_cont_c := time_cont_raw - mean(time_cont_raw, na.rm = TRUE)]
        dt[, Year_integer := as.integer(floor(time_cont_raw))]
        rm(time_cont_raw)

        dt[, c("Year", "Month") := NULL]

        data.table::setkey(dt, Species)
        finite_idx <- function(d, response) {
                is.finite(d$time_cont_c) & is.finite(d[[response]])
        }

        species_temporal_info <- function(d, response) {
                ok <- finite_idx(d, response)
                dd <- d[ok]
                if (nrow(dd) == 0)
                        return(data.frame())
                dd[, .(
                        response = response,
                        n_records = .N,
                        n_years = data.table::uniqueN(Year_integer)
                ), by = Species]
        }

        safe_rbind <- function(x) {
                x <- Filter(Negate(is.null), x)
                if (length(x) == 0)
                        return(data.frame())
                data.table::rbindlist(x, fill = TRUE) |> as.data.frame()
        }

        direction <- function(x) {
                ifelse(is.na(x),
                       NA_character_,
                       ifelse(
                               x > 0,
                               "positive",
                               ifelse(x < 0, "negative", "zero")
                       ))
        }

        fit_lm_stats_xy <- function(x, y) {
                n <- length(x)
                if (n < 3L || length(unique(x)) < 2L)
                        return(NULL)
                fit <- tryCatch(
                        stats::lm.fit(cbind(1, x), y),
                        error = function(e)
                                NULL
                )
                if (is.null(fit))
                        return(NULL)
                df <- n - 2L
                mse <- sum(fit$residuals^2) / df
                xc <- x - mean(x)
                se <- sqrt(mse / sum(xc^2))
                slope <- unname(fit$coefficients[2L])
                t_val <- slope / se
                p_val <- 2 * stats::pt(-abs(t_val), df = df)
                ci_hw <- stats::qt(0.975, df = df) * se
                list(
                        slope = slope,
                        se = se,
                        t = t_val,
                        p = p_val,
                        ci_low = slope - ci_hw,
                        ci_high = slope + ci_hw,
                        df = df,
                        n = n
                )
        }

        welch_slope_test <- function(slope_a,
                                     se_a,
                                     df_a,
                                     slope_b,
                                     se_b,
                                     df_b) {
                if (!all(is.finite(c(
                        slope_a, se_a, df_a, slope_b, se_b, df_b
                ))) ||
                se_a <= 0 || se_b <= 0)
                        return(list(
                                t = NA_real_,
                                df = NA_real_,
                                p = NA_real_
                        ))
                var_a <- se_a^2
                var_b <- se_b^2
                se_diff <- sqrt(var_a + var_b)
                if (!is.finite(se_diff) ||
                    se_diff <= 0)
                        return(list(
                                t = NA_real_,
                                df = NA_real_,
                                p = NA_real_
                        ))
                df_welch <- (var_a + var_b)^2 / ((var_a^2 / df_a) + (var_b^2 / df_b))
                t_val <- (slope_a - slope_b) / se_diff
                list(
                        t = t_val,
                        df = df_welch,
                        p = 2 * stats::pt(-abs(t_val), df = df_welch)
                )
        }

        classify_lm_individual_slope <- function(environmental_comparison,
                                                 individual_lm_threshold_type,
                                                 individual_lm_min_thresholds) {
                environmental_comparison$individual_response_class <- NA_character_

                t_idx <- environmental_comparison$response == "Temperature"
                t_idx[is.na(t_idx)] <- FALSE
                e_idx <- environmental_comparison$response == "Elevation"
                e_idx[is.na(e_idx)] <- FALSE

                environmental_comparison$individual_response_class[t_idx] <- "TC"
                environmental_comparison$individual_response_class[e_idx] <- "SC"

                sig_ind <- !is.na(environmental_comparison$individual_significant) &
                        environmental_comparison$individual_significant
                sig_diff <- !is.na(environmental_comparison$slope_diff_significant) &
                        environmental_comparison$slope_diff_significant
                diff_above <- !is.na(environmental_comparison$slope_diff_signed) &
                        environmental_comparison$slope_diff_signed > 0
                diff_below <- !is.na(environmental_comparison$slope_diff_signed) &
                        environmental_comparison$slope_diff_signed < 0
                diff_above_threshold <- rep(TRUE, nrow(environmental_comparison))

                if (!is.null(individual_lm_min_thresholds))
                {
                        if (!is.na(individual_lm_min_thresholds["Temperature"])) {
                                if (individual_lm_threshold_type == "speciesslope") {
                                        diff_above_threshold[t_idx] <- !is.na(
                                                environmental_comparison$individual_slope[t_idx]
                                        ) &
                                                abs(
                                                        environmental_comparison$individual_slope[t_idx]
                                                ) > individual_lm_min_thresholds["Temperature"]
                                        diff_above[t_idx] <- !is.na(
                                                environmental_comparison$individual_slope[t_idx]
                                        ) &
                                                environmental_comparison$individual_slope[t_idx] > individual_lm_min_thresholds["Temperature"]
                                        diff_below[t_idx] <- !is.na(
                                                environmental_comparison$individual_slope[t_idx]
                                        ) &
                                                environmental_comparison$individual_slope[t_idx] < -individual_lm_min_thresholds["Temperature"]
                                }
                                else if (individual_lm_threshold_type == "slopediff") {
                                        diff_above_threshold[t_idx] <- !is.na(environmental_comparison$slope_diff[t_idx]) &
                                                abs(environmental_comparison$slope_diff[t_idx]) > individual_lm_min_thresholds["Temperature"]
                                        diff_above[t_idx] <- !is.na(
                                                environmental_comparison$slope_diff_signed[t_idx]
                                        ) &
                                                environmental_comparison$slope_diff_signed[t_idx] > 0
                                        diff_below[t_idx] <- !is.na(
                                                environmental_comparison$slope_diff_signed[t_idx]
                                        ) &
                                                environmental_comparison$slope_diff_signed[t_idx] < 0
                                }
                        }
                        if (!is.na(individual_lm_min_thresholds["Elevation"])) {
                                if (individual_lm_threshold_type == "speciesslope") {
                                        diff_above_threshold[e_idx] <- !is.na(
                                                environmental_comparison$individual_slope[e_idx]
                                        ) &
                                                abs(
                                                        environmental_comparison$individual_slope[e_idx]
                                                ) > individual_lm_min_thresholds["Elevation"]
                                        diff_above[e_idx] <- !is.na(
                                                environmental_comparison$individual_slope[e_idx]
                                        ) &
                                                environmental_comparison$individual_slope[e_idx] > individual_lm_min_thresholds["Elevation"]
                                        diff_below[e_idx] <- !is.na(
                                                environmental_comparison$individual_slope[e_idx]
                                        ) &
                                                environmental_comparison$individual_slope[e_idx] < -individual_lm_min_thresholds["Elevation"]
                                }
                                else if (individual_lm_threshold_type == "slopediff") {
                                        diff_above_threshold[e_idx] <- !is.na(environmental_comparison$slope_diff[e_idx]) &
                                                abs(environmental_comparison$slope_diff[e_idx]) > individual_lm_min_thresholds["Elevation"]
                                        diff_above[e_idx] <- !is.na(
                                                environmental_comparison$slope_diff_signed[e_idx]
                                        ) &
                                                environmental_comparison$slope_diff_signed[e_idx] > 0
                                        diff_below[e_idx] <- !is.na(
                                                environmental_comparison$slope_diff_signed[e_idx]
                                        ) &
                                                environmental_comparison$slope_diff_signed[e_idx] < 0
                                }
                        }
                }

                environmental_comparison$individual_slope_above_threshold = diff_above_threshold

                environmental_comparison$individual_response_class[t_idx &
                                                                           sig_ind & sig_diff & diff_above_threshold & diff_above] <- "TT"
                environmental_comparison$individual_response_class[t_idx &
                                                                           sig_ind & sig_diff & diff_above_threshold & diff_below] <- "TA"
                environmental_comparison$individual_response_class[e_idx &
                                                                           sig_ind & sig_diff & diff_above_threshold & diff_above] <- "SA"
                environmental_comparison$individual_response_class[e_idx &
                                                                           sig_ind & sig_diff & diff_above_threshold & diff_below] <- "SD"
                return (environmental_comparison)
        }


        species_filter <- safe_rbind(lapply(responses, function(resp)
                species_temporal_info(dt, resp)))
        if (nrow(species_filter) > 0) {
                species_filter$retained <- species_filter$n_records >= min_records &
                        species_filter$n_years >= min_years
                species_filter$Species <- as.character(species_filter$Species)
        }

        if (verbose && nrow(species_filter) > 0) {
                message("Environmental response filtering summary:")
                for (resp in unique(species_filter$response)) {
                        x <- species_filter[species_filter$response == resp, , drop = FALSE]
                        message(
                                "- ",
                                resp,
                                ": retained ",
                                sum(x$retained),
                                " of ",
                                nrow(x),
                                " species using min_records = ",
                                min_records,
                                " and min_years = ",
                                min_years,
                                "."
                        )
                }
        }


        calc_individual_lm <- function()
        {
                global_lm_list <- list()
                individual_lm_list <- list()
                for (resp in responses) {
                        sf <- species_filter[species_filter$response == resp &
                                                     species_filter$retained == TRUE, , drop = FALSE]
                        valid_species <- sf$Species
                        if (length(valid_species) == 0)
                                next
                        ok_all <- dt$Species %in% valid_species & finite_idx(dt, resp)
                        x_all <- dt$time_cont_c[ok_all]
                        y_all <- dt[[resp]][ok_all]
                        yi_all <- dt$Year_integer[ok_all]
                        if (length(x_all) == 0)
                                next

                        g_stats <- fit_lm_stats_xy(x_all, y_all)
                        rm(x_all, y_all, yi_all, ok_all)
                        if (is.null(g_stats))
                                next

                        global_lm_list[[resp]] <- data.frame(
                                response = resp,
                                model = "global_lm",
                                global_lm_slope = g_stats$slope,
                                global_lm_se = g_stats$se,
                                global_lm_t = g_stats$t,
                                global_lm_p_value = g_stats$p,
                                global_lm_conf_low = g_stats$ci_low,
                                global_lm_conf_high = g_stats$ci_high,
                                n_records = g_stats$n,
                                n_years = {
                                        ok2 <- dt$Species %in% valid_species & finite_idx(dt, resp)
                                        data.table::uniqueN(dt$Year_integer[ok2])
                                },
                                stringsAsFactors = FALSE
                        )

                        sp_results <- lapply(valid_species, function(sp) {
                                ok_sp <- dt$Species == sp & finite_idx(dt, resp)
                                if (sum(ok_sp) < 3L)
                                        return(NULL)
                                x_sp <- dt$time_cont_c[ok_sp]
                                y_sp <- dt[[resp]][ok_sp]

                                s_stats <- fit_lm_stats_xy(x_sp, y_sp)
                                if (is.null(s_stats))
                                        return(NULL)

                                diff <- welch_slope_test(
                                        s_stats$slope,
                                        s_stats$se,
                                        s_stats$df,
                                        g_stats$slope,
                                        g_stats$se,
                                        g_stats$df
                                )
                                data.frame(
                                        Species = sp,
                                        response = resp,
                                        n_records = s_stats$n,
                                        n_years = data.table::uniqueN(dt$Year_integer[ok_sp]),
                                        individual_slope = s_stats$slope,
                                        individual_slope_se = s_stats$se,
                                        individual_t = s_stats$t,
                                        individual_p_value = s_stats$p,
                                        individual_conf_low = s_stats$ci_low,
                                        individual_conf_high = s_stats$ci_high,
                                        global_lm_slope = g_stats$slope,
                                        slope_diff_signed = s_stats$slope - g_stats$slope,
                                        slope_diff_direction = direction(
                                                s_stats$slope - g_stats$slope
                                        ),
                                        slope_diff = abs(
                                                s_stats$slope - g_stats$slope
                                        ),
                                        slope_diff_t = diff$t,
                                        slope_diff_df = diff$df,
                                        slope_diff_p_value = diff$p,
                                        individual_direction = direction(s_stats$slope),
                                        stringsAsFactors = FALSE
                                )
                        })
                        individual_lm_list[[resp]] <- sp_results
                }

                global_lm_results <- safe_rbind(global_lm_list)
                individual_lm_results <- safe_rbind(unlist(individual_lm_list, recursive = FALSE))
                rm(global_lm_list, individual_lm_list)

                if (nrow(individual_lm_results) > 0) {
                        individual_lm_results$individual_p_adj_fdr <- NA_real_
                        individual_lm_results$slope_diff_p_adj_fdr <- NA_real_
                        for (resp in unique(individual_lm_results$response)) {
                                idx <- individual_lm_results$response == resp
                                individual_lm_results$individual_p_adj_fdr[idx] <-
                                        stats::p.adjust(
                                                individual_lm_results$individual_p_value[idx],
                                                method = "BH"
                                        )
                                individual_lm_results$slope_diff_p_adj_fdr[idx] <-
                                        stats::p.adjust(
                                                individual_lm_results$slope_diff_p_value[idx],
                                                method = "BH"
                                        )
                                #calculamos el alpha correspondiente a la especie en funciÃ³n de N y de las opciones del usuario
                                individual_lm_results$alpha[idx] <- alpha_ref
                                if (use_Ncorrected_alpha) {
                                        mask <- idx & individual_lm_results$n_records > 100
                                        individual_lm_results$alpha[mask] <- alpha_ref * sqrt(Nref / individual_lm_results$n_records[mask])
                                }
                        }
                        individual_lm_results$individual_significant <-
                                !is.na(individual_lm_results$individual_p_adj_fdr) &
                                individual_lm_results$individual_p_adj_fdr < individual_lm_results$alpha
                        individual_lm_results$slope_diff_significant <-
                                !is.na(individual_lm_results$slope_diff_p_adj_fdr) &
                                individual_lm_results$slope_diff_p_adj_fdr < individual_lm_results$alpha
                }
                return (list(global_lm_results, individual_lm_results))
        }

        r_ind_lm = calc_individual_lm()
        global_lm_results = r_ind_lm[[1]]
        individual_lm_results = r_ind_lm[[2]]
        rm(r_ind_lm)

        rm(dt)

        if (!is.null(individual_lm_results) &&
            nrow(individual_lm_results) > 0) {
                environmental_comparison <- individual_lm_results
        } else {
                environmental_comparison <- data.frame()
        }

        if (nrow(environmental_comparison) > 0) {
                environmental_comparison = classify_lm_individual_slope(
                        environmental_comparison,
                        individual_lm_threshold_type,
                        individual_lm_min_thresholds
                )
        }

        list(
                params = param_values,
                species_filter = species_filter,
                environmental_global_lm = global_lm_results,
                environmental_individual_lm = individual_lm_results,
                environmental_comparison = environmental_comparison
        )
}
