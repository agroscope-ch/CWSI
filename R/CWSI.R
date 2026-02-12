#' R6 class for calculating the Crop Water Stress Index (CWSI)
#' @description
#' destination description This class allows users to load canopy temperature data and micrometeorological data, and then calculate the CWSI based on these inputs.

CWSI <- R6::R6Class(
  public = list(
    # canopy temperature data
    Tc = NULL,
    # micrometeorological data
    met_data = NULL,
    # intermediate results from CWSI calculation (e.g., upper and lower baselines)
    int_results = NULL,

    #' @description
    #' Load canopy temperature data.
    #' @param Tc Canopy temperature data (°C).
    #' @return The CWSI object with loaded canopy temperature data.
    load_canopyTemp = function(Tc, time_series = TRUE) {

      # Reset micrometeorological data (as n time points may change)
      micrometeo_data = NULL

      private$is_time_series <- time_series
      self$Tc <- as.array(Tc)
      dm <- dim(self$Tc)
      if (private$is_time_series) {
        private$time_dim <- dm[length(dm)]
        private$space_dim <- dm[-length(dm)]
        if (length(private$space_dim) == 0) {
          private$space_dim <- 1
        }
      } else if (!private$is_time_series) {
        private$time_dim <- 1
        private$space_dim <- dm
      }
      message(paste0("Tc data of dimensions c(",
                     paste(private$space_dim, collapse = ", "),
                     ") for ", private$time_dim, " time points loaded."))
      return(invisible(self))
    },

    #' @description
    #' Load micrometeorological data.
    #' @param Rs Incoming solar radiation (W/m^2)
    #' @param Ta Air temperature (°C)
    #' @param h Crop height (m)
    #' @param z Measurement height (m)
    #' @param u Wind speed (m/s)
    #' @param rH Relative humidity (\%)
    #' @return The CWSI object with loaded micrometeorological data.
    load_micrometeo = function(Rs = NULL,
                               Ta = NULL,
                               u = NULL,
                               rH = NULL,
                               h = NULL,
                               z = NULL,
                               data = NULL) {
      if (!is.null(data)) {
        list2env(data, environment())
      }
      if(is.null(private$time_dim)) {
        stop("Canopy temperature data must be loaded first using load_canopyTemp()",
             call. = FALSE)
      }

      assertInput <- checkmate::makeAssertCollection()

      if (!missing(Rs)) {
        checkmate::assertNumeric(Rs,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 len = private$time_dim,
                                 add = assertInput)
      }

      if (!missing(Ta)) {
        checkmate::assertNumeric(Ta,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 len = private$time_dim,
                                 add = assertInput)
      }

      if (!missing(u)) {
        checkmate::assertNumeric(u,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 len = private$time_dim,
                                 add = assertInput)
      }

      if (!missing(rH)) {
        checkmate::assertNumeric(rH,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 len = private$time_dim,
                                 add = assertInput)
      }

      if (!missing(h)) {
        checkmate::assertNumeric(h,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 add = assertInput)
        if(length(h) != 1 && length(h) != private$time_dim) {
          assertInput$push(
            paste0("h must be of length 1 or ", private$time_dim, ".")
          )
        }
      }

      if (!missing(z)) {
        checkmate::assertNumeric(z,
                                 null.ok = FALSE,
                                 finite = TRUE,
                                 any.missing = FALSE,
                                 add = assertInput)
        if(length(z) != 1 && length(z) != private$time_dim) {
          assertInput$push(
            paste0("z must be of length 1 or ", private$time_dim, ".")
          )
        }
      }

      checkmate::reportAssertions(assertInput)

      self$met_data <- list(Rs = Rs,
                            Ta = Ta,
                            u = u,
                            rH = rH,
                            h = h,
                            z = z)

    },

    #' @description
    #' Calculate the Crop Water Stress Index (CWSI) based on the loaded canopy temperature and micrometeorological data.
    #' @param method The method to calculate CWSI. Currently, only "theoretical" is implemented.
    #' @return A numeric vector of CWSI values for each time point.
    #' @details
    #' \deqn{
    #'   T_c - T_A =
    #'   \frac{r_a R_n}{\rho c_p} \cdot \frac{\gamma}{\Delta + \gamma}
    #'   - \frac{(e_A^* - e_A)}{\Delta + \gamma}
    #' }

    calc_CWSI = function(method = "theoretical") {

      last_dim <- length(dim(self$Tc))

      if (last_dim == 1) {
        Tc <- self$Tc
      } else {
        # split array by time dim (rest can be any dimension)
        Tc = asplit(self$Tc, last_dim)
      }

      CWSI_fun = switch(method,
        "theoretical" = calc_CWSI_theoretical,
        stop("Method not recognized. Currently, only 'theoretical' is implemented.",
             call. = FALSE))

      # apply CWSI calculation element-wise across time points, keeping spatial dimensions intact
      CWSI <- do.call(Map, c(CWSI_fun, list(Tc), self$met_data))

      # recreate original dimensions, with time as the last dimension
      CWSI <- array(do.call(c, CWSI), dim = c(dim(CWSI[[1]]), length(CWSI)))

      return(CWSI)
    }

  ),

  private = list(
    is_time_series = NULL,
    time_dim = NULL,
    space_dim = NULL
  )
)
