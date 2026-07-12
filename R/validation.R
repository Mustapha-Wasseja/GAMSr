.gams_reserved_words <- c(
  "abort", "acronym", "acronyms", "alias", "all", "and", "binary",
  "break", "card", "continue", "diag", "display", "do", "else",
  "elseif", "endfor", "endif", "endloop", "endwhile", "eps", "equation",
  "equations", "execute", "execute_load", "execute_loaddc",
  "execute_loadhandle", "execute_loadpoint", "execute_unload",
  "execute_unloaddi", "execute_unloadidx", "file", "files", "for", "free",
  "function", "functions", "gdxload", "if", "inf", "integer", "logic",
  "loop", "model", "models", "na", "negative", "nonnegative", "no", "not",
  "option", "options", "or", "ord", "parameter", "parameters", "positive",
  "procedure", "procedures", "prod", "put", "put_utility", "put_utilities",
  "putclear", "putclose", "putfmcl", "puthd", "putheader", "putpage",
  "puttitle", "puttl", "repeat", "sameas", "sand", "scalar", "scalars",
  "semicont", "semiint", "set", "sets", "singleton", "smax", "smin",
  "solve", "sor", "sos1", "sos2", "sum", "system", "table", "tables",
  "then", "undf", "until", "variable", "variables", "while", "xor", "yes"
)

validate_symbol_name <- function(name, arg = "name", call = rlang::caller_env()) {
  if (!is_string(name)) {
    gamsr_abort(
      sprintf("`%s` must be a single non-missing character string.", arg),
      class = "gamsr_error_invalid_name",
      call = call
    )
  }

  if (!nzchar(name)) {
    gamsr_abort(
      sprintf("`%s` must not be empty.", arg),
      class = "gamsr_error_invalid_name",
      call = call
    )
  }

  if (nchar(name, type = "chars") > 63L) {
    gamsr_abort(
      sprintf("`%s` must be at most 63 characters for GAMS compatibility.", arg),
      class = "gamsr_error_invalid_name",
      call = call
    )
  }

  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name, perl = TRUE)) {
    gamsr_abort(
      sprintf(
        "`%s` must start with a letter and contain only letters, digits, or underscores.",
        arg
      ),
      class = "gamsr_error_invalid_name",
      call = call
    )
  }

  if (tolower(name) %in% .gams_reserved_words) {
    gamsr_abort(
      sprintf("`%s` must not be a GAMS reserved word.", arg),
      i = sprintf("Problematic name: `%s`.", name),
      class = "gamsr_error_reserved_name",
      call = call
    )
  }

  name
}

validate_model_name <- function(name, arg = "name", call = rlang::caller_env()) {
  validate_symbol_name(name, arg = arg, call = call)
}

validate_label <- function(label, arg = "label", call = rlang::caller_env()) {
  if (!is.character(label)) {
    gamsr_abort(
      sprintf("`%s` must be a character vector.", arg),
      class = "gamsr_error_invalid_label",
      call = call
    )
  }

  if (anyNA(label)) {
    gamsr_abort(
      sprintf("`%s` must not contain missing values.", arg),
      class = "gamsr_error_invalid_label",
      call = call
    )
  }

  too_long <- nchar(label, type = "chars", allowNA = FALSE) > 63L
  if (any(too_long)) {
    gamsr_abort(
      sprintf("Each `%s` value must be at most 63 characters.", arg),
      i = sprintf("First long label: `%s`.", label[which(too_long)[1L]]),
      class = "gamsr_error_invalid_label",
      call = call
    )
  }

  has_control <- grepl("[[:cntrl:]]", label, perl = TRUE)
  if (any(has_control)) {
    gamsr_abort(
      sprintf("`%s` must not contain control characters.", arg),
      i = sprintf("First invalid label: `%s`.", label[which(has_control)[1L]]),
      class = "gamsr_error_invalid_label",
      call = call
    )
  }

  trailing_blank <- grepl("[[:space:]]$", label, perl = TRUE)
  if (any(trailing_blank)) {
    gamsr_abort(
      sprintf("`%s` must not contain trailing whitespace.", arg),
      i = "GAMS trims trailing blanks in labels, which would make round-trips ambiguous.",
      class = "gamsr_error_invalid_label",
      call = call
    )
  }

  enc2utf8(label)
}

gams_identifier_key <- function(name) {
  tolower(validate_symbol_name(name))
}

label_key <- function(label) {
  tolower(validate_label(label))
}

check_unique_labels <- function(label, arg = "records", call = rlang::caller_env()) {
  keys <- label_key(label)
  duplicated_key <- duplicated(keys)
  if (any(duplicated_key)) {
    gamsr_abort(
      sprintf("`%s` must not contain duplicate GAMS labels.", arg),
      i = sprintf("First duplicate label: `%s`.", label[which(duplicated_key)[1L]]),
      class = "gamsr_error_duplicate_label",
      call = call
    )
  }
  invisible(label)
}

is_unquoted_gams_label <- function(label) {
  label <- validate_label(label)
  grepl("^[A-Za-z0-9][A-Za-z0-9_+-]*$", label, perl = TRUE) &
    !(tolower(label) %in% .gams_reserved_words)
}

format_gams_label <- function(label, call = rlang::caller_env()) {
  label <- validate_label(label, call = call)

  vapply(label, function(x) {
    if (is_unquoted_gams_label(x)) {
      return(x)
    }

    has_single <- grepl("'", x, fixed = TRUE)
    has_double <- grepl('"', x, fixed = TRUE)
    if (!has_single) {
      paste0("'", x, "'")
    } else if (!has_double) {
      paste0('"', x, '"')
    } else {
      gamsr_abort(
        "Labels containing both single and double quotes are not supported yet.",
        i = sprintf("Problematic label: `%s`.", x),
        class = "gamsr_error_invalid_label",
        call = call
      )
    }
  }, character(1L), USE.NAMES = FALSE)
}

new_name_registry <- function(kind = "name") {
  items <- new.env(parent = emptyenv())
  order <- character()

  add <- function(name, value = name, call = rlang::caller_env()) {
    key <- gams_identifier_key(name)
    if (exists(key, envir = items, inherits = FALSE)) {
      existing <- get(key, envir = items, inherits = FALSE)
      gamsr_abort(
        sprintf("A %s named `%s` already exists.", kind, existing$name),
        i = "GAMS identifiers are case-insensitive.",
        class = "gamsr_error_duplicate_name",
        call = call
      )
    }

    assign(key, list(name = name, value = value), envir = items)
    order <<- c(order, key)
    invisible(value)
  }

  has <- function(name) {
    key <- gams_identifier_key(name)
    exists(key, envir = items, inherits = FALSE)
  }

  get_value <- function(name, call = rlang::caller_env()) {
    key <- gams_identifier_key(name)
    if (!exists(key, envir = items, inherits = FALSE)) {
      gamsr_abort(
        sprintf("No %s named `%s` exists.", kind, name),
        class = "gamsr_error_unknown_name",
        call = call
      )
    }
    get(key, envir = items, inherits = FALSE)$value
  }

  replace_value <- function(name, value, call = rlang::caller_env()) {
    key <- gams_identifier_key(name)
    if (!exists(key, envir = items, inherits = FALSE)) {
      gamsr_abort(
        sprintf("No %s named `%s` exists.", kind, name),
        class = "gamsr_error_unknown_name",
        call = call
      )
    }

    existing <- get(key, envir = items, inherits = FALSE)
    assign(key, list(name = existing$name, value = value), envir = items)
    invisible(value)
  }

  names_value <- function() {
    vapply(order, function(key) {
      get(key, envir = items, inherits = FALSE)$name
    }, character(1L), USE.NAMES = FALSE)
  }

  values <- function() {
    if (length(order) == 0L) {
      return(list())
    }
    stats::setNames(
      lapply(order, function(key) get(key, envir = items, inherits = FALSE)$value),
      names_value()
    )
  }

  structure(
    list(
      add = add,
      exists = has,
      get = get_value,
      replace = replace_value,
      names = names_value,
      values = values
    ),
    class = "gamsr_name_registry"
  )
}
