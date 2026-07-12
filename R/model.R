#' Create a GAMS model container
#'
#' `gams_model()` creates a mutable model context that owns symbols and stores
#' package-side metadata. It does not call GAMS.
#'
#' @param name Optional model name. When omitted, `"GAMSr_model"` is used.
#' @param system_directory Optional GAMS system directory for later execution.
#'
#' @return A `GamsModel` R6 object.
#' @importFrom R6 R6Class
#' @export
gams_model <- function(name = NULL, system_directory = NULL) {
  GamsModel$new(name = name, system_directory = system_directory)
}

#' @noRd
GamsModel <- R6::R6Class(
  "GamsModel",
  public = list(
    name = NULL,
    system_directory = NULL,
    options = NULL,
    diagnostics = NULL,

    initialize = function(name = NULL, system_directory = NULL) {
      self$name <- validate_model_name(name %||% "GAMSr_model")
      self$system_directory <- system_directory
      self$options <- list()
      self$diagnostics <- list()
      private$symbol_registry <- new_name_registry("symbol")
      private$generated_registry <- new_name_registry("generated name")
    },

    add_symbol = function(symbol) {
      if (!inherits(symbol, "gams_symbol")) {
        gamsr_abort(
          "`symbol` must be a GAMSr symbol object.",
          class = "gamsr_error_invalid_symbol"
        )
      }
      private$symbol_registry$add(symbol$name, symbol)
      invisible(symbol)
    },

    get_symbol = function(name) {
      private$symbol_registry$get(name)
    },

    symbols = function() {
      private$symbol_registry$values()
    },

    validate = function() {
      invisible(self)
    },

    print = function(...) {
      cat(format_gams_model(self), sep = "\n")
      invisible(self)
    }
  ),
  private = list(
    symbol_registry = NULL,
    generated_registry = NULL
  )
)

is_gams_model <- function(x) {
  inherits(x, "GamsModel")
}

format_gams_model <- function(x) {
  syms <- x$symbols()
  lines <- c(
    sprintf("<gams_model: %s>", x$name),
    sprintf("Symbols: %d", length(syms))
  )

  if (length(syms) > 0L) {
    kinds <- vapply(syms, function(sym) sym$kind, character(1L))
    counts <- table(kinds)
    lines <- c(
      lines,
      sprintf("  %s: %d", names(counts), as.integer(counts))
    )
  }

  lines
}
