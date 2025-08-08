#' Create a CacheConnection Object
#'
#' `init_CacheConnection` initializes a `CacheConnection`.
#'
#' @return An instance of the `CacheConnection` class.
#'
#' @export
init_CacheConnection <- function() {
  CacheConnection$new()
}


#' CacheConnection Class
#'
#' @description
#' An R6 class that handles persistent or in-memory caching of data used in the Countdown 2030
#' analysis and reporting workflows. It supports tracking of various internal data objects,
#' reactive updates (for Shiny apps).
#'
#' @docType class
#' @name CacheConnection
#' @format An [R6::R6Class] generator object.
#' @keywords internal
CacheConnection <- R6::R6Class(
  'CacheConnection',
  public = list(
    #' @description Initialize a CacheConnection instance.
    initialize = function() {
      # Initialize in-memory data using the template
      private$.in_memory_data <- private$.data_template
    },

    #' @description Return a reactive wrapper (for Shiny).
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$.reactiveDep)) {
        private$.reactiveDep <- reactiveValues() # Initialize as an empty reactiveValues
        for (field_name in names(private$.data_template)) {
          private$.reactiveDep[[field_name]] <- 0 # Create a reactive tracker for each field
        }
      }
      reactive({
        private$depend_all()
        self
      })
    },

    #' @description Set county.
    #' @param value Character.
    set_county = function(value) private$setter('county', value, is_scalar_character),

    #' @description Set county.
    #' @param value Character.
    set_year_type = function(value) private$setter('year_type', value, is_scalar_character),

    #' @description Set county.
    #' @param value Character.
    set_year = function(value) private$setter('year', value, is_scalar_character),

    #' @description Set county.
    #' @param value Character.
    set_aggregation_unit = function(value) private$setter('aggregation_unit', value, ~ is.null(.x) || is_scalar_character(.x)),

    #' @description Set county.
    #' @param value Character.
    set_aggregation_level = function(value) private$setter('aggregation_level', value, is_scalar_character)
  ),
  active = list(
    #' @field county description
    county = function(value) private$getter('county', value),

    #' @field year_type description
    year_type = function(value) private$getter('year_type', value),

    #' @field year description
    year = function(value) private$getter('year', value),

    #' @field aggregation_unit description
    aggregation_unit = function(value) private$getter('aggregation_unit', value),

    #' @field aggregation_level description
    aggregation_level = function(value) private$getter('aggregation_level', value),

    #' @field k_factors description
    k_factors = function(value){
      if (is_missing(value)) {
        private$depend('k_factors')
        return(data_elements %>% distinct(column_name, k_factor))
      }

      cli::cli_abort(c('x' = '{.field field_name} is readonly'))
    },



    #' @field khis_data description
    khis_data = function(value) {
      if (is_missing(value)) {
        private$depend('k_factors')
        dt <- khis_data %>%
          adjust_data(self$k_factors)
        return(dt)
      }

      cli::cli_abort(c('x' = '{.field field_name} is readonly'))
    },

    #' @field summarised_data description
    summarised_data = function(value){
      if (is_missing(value)) {
        # private$depend('year_type')
        # private$depend('aggregation_level')
        # private$depend('county')
        # private$depend('k_factors')

        year_col <- self$year_type
        agg_val <- self$aggregation_level
        county <- self$county

        level <- if (county == 'Kenya') 'national' else 'county'
        period <- if (agg_val == 'year' && year_col == 'year') {
          'year'
        } else if (agg_val == 'year' && year_col == 'fiscal_year') {
          'fiscal_year'
        } else if (agg_val == 'quarter' && year_col == 'year') {
          'quarter'
        } else if (agg_val == 'quarter' && year_col == 'fiscal_year') {
          'fiscal_quarter'
        } else if (agg_val == 'month' && year_col == 'fiscal_year') {
          'fiscal_month'
        } else {
          'month'
        }

        data <- self$khis_data %>%
          generate_indicators(level = level, period = period)
        return(data)
      }

      cli::cli_abort(c('x' = '{.field field_name} is readonly'))
    },

    #' @field years description
    years = function(value) {
      if (is_missing(value)) {
        private$depend('year_type')
        dt <- self$khis_data %>%
          distinct(!!sym(self$year_type)) %>%
          arrange(desc(!!sym(self$year_type))) %>%
          pull(!!sym(self$year_type))
        return(dt)
      }

      cli::cli_abort(c('x' = '{.field field_name} is readonly'))
    }
  ),
  private = list(
    .data_template = list(
      county = 'Kenya',
      year_type = 'fiscal_year',
      year = NULL,
      aggregation_unit = NULL,
      aggregation_level = 'year'
    ),
    .in_memory_data = NULL,
    .has_changed = FALSE,
    .reactiveDep = NULL,
    #' Update a field (with change tracking)
    update_field = function(field_name, value) {
      if (!identical(private$.in_memory_data[[field_name]], value)) {
        private$.in_memory_data[[field_name]] <<- value
        private$.has_changed <<- TRUE
        private$trigger(field_name)
      }
    },
    getter = function(field_name, value) {
      if (is_missing(value)) {
        private$depend(field_name)
        return(private$.in_memory_data[[field_name]])
      }

      cli::cli_abort(c('x' = '{.field field_name} is readonly'))
    },
    setter = function(field_name, value, validation_exp = NULL) {
      check_required(field_name)
      check_required(value)

      validate_fn <- if (!is.null(validation_exp)) {
        if (rlang::is_formula(validation_exp)) {
          rlang::as_function(validation_exp)
        } else if (rlang::is_function(validation_exp)) {
          validation_exp
        } else {
          cli::cli_abort(c('x' = '{.arg validation} must be a function or a formula.'))
        }
      } else {
        function(x) TRUE
      }
      if (!validate_fn(value)) {
        cli::cli_abort(c('x' = 'Invalid value for field {.field field_name}.'))
      }
      private$update_field(field_name, value)
    },
    depend = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]]
      }
      invisible()
    },
    trigger = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]] <- isolate(private$.reactiveDep[[field_name]] + 1)
      }
    },
    depend_all = function() {
      for (field_name in names(private$.data_template)) {
        private$depend(field_name) # Establish dependency for each field
      }
      invisible()
    }
  )
)
