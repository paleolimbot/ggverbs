
.onLoad <- function(libname, pkgname) {
  verbs_regexes <- c(
    "^aes_?$" = "update",
    "^aes_(string|q)$" = "update",
    "^layer$"  = "add",
    "^(geom|stat|annotation)_" = "add",
    "^scale_[a-z0-9]+_[a-z0-9]+$" = "set",
    "^theme_(?!get|set|update|replace)[a-z]+$" = "set",
    "^theme$" = "update",
    "^coord_(?!munch)[a-z]+$" = "set",
    "^facet_" = "set",
    "^labs$" = "update",
    "^guides$" = "update"
  )

  fun_list <- mapply(get_verb_functions, names(verbs_regexes), verbs_regexes)
  for(item in fun_list) {
    for(new_fun_name in names(item)) {
      assign(new_fun_name, item[[new_fun_name]], envir = pkg_env)
    }
  }
}

pkg_env <- environment()

get_verb_functions <- function(regex, verb) {
  function_matches <- stringr::str_subset(getNamespaceExports("ggplot2"), regex)
  new_function_names <- paste(verb, function_matches, sep = "_")
  functions <- lapply(function_matches, verbify)
  names(functions) <- new_function_names

  functions
}

verbify <- function(ggplot_function_name) {
  noun_fun <- getNamespace("ggplot2")[[ggplot_function_name]]
  verb_fun <- function(.plot, ...) {
    fun_call <- match.call()
    fun_call[[1]] <- noun_fun
    fun_call$.plot <- NULL
    .plot + eval(fun_call, envir = parent.frame())
  }
  formals(verb_fun) <- c(list(.plot = quote(ggplot2::last_plot())), formals(noun_fun))
  verb_fun
}
