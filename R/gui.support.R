.stylo_has_namespace <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

.stylo_namespace_function <- function(pkg, name) {
  if (!.stylo_has_namespace(pkg)) {
    stop(
      "Optional package `", pkg, "` is required for this stylo GUI operation.",
      call. = FALSE
    )
  }

  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

.stylo_tk_available <- function() {
  if (!.stylo_has_namespace("tcltk")) {
    return(FALSE)
  }

  if (.Platform$OS.type == "windows" || identical(.Platform$GUI, "AQUA")) {
    return(TRUE)
  }

  isTRUE(capabilities("tcltk")) &&
    isTRUE(capabilities("X11")) &&
    isTRUE(suppressWarnings(.stylo_namespace_function("tcltk", ".TkUp")))
}

.stylo_gui_available <- function() {
  .stylo_tk_available() && .stylo_has_namespace("tcltk2")
}

.stylo_require_gui <- function(function_name = "This function") {
  if (.stylo_gui_available()) {
    return(invisible(TRUE))
  }

  missing_packages <- character()

  if (!.stylo_has_namespace("tcltk")) {
    missing_packages <- c(missing_packages, "tcltk")
  }
  if (!.stylo_has_namespace("tcltk2")) {
    missing_packages <- c(missing_packages, "tcltk2")
  }

  if (length(missing_packages) > 0) {
    stop(
      function_name,
      " requires optional GUI support; missing packages: ",
      paste(missing_packages, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  stop(
    function_name,
    " requires an interactive Tcl/Tk session.",
    call. = FALSE
  )
}

.stylo_dialogs_available <- function() {
  interactive() && .stylo_tk_available()
}

.stylo_choose_dir <- function(caption, error_message) {
  if (!.stylo_dialogs_available()) {
    stop(error_message, call. = FALSE)
  }

  selected.path <- tk_choose.dir(caption = caption)

  if (!is.character(selected.path) || length(selected.path) == 0 ||
      is.na(selected.path) || !nzchar(selected.path)) {
    stop(error_message, call. = FALSE)
  }

  selected.path
}

.stylo_choose_files <- function(default = "", caption = "", multi = FALSE,
                                error_message) {
  if (!.stylo_dialogs_available()) {
    stop(error_message, call. = FALSE)
  }

  selected.files <- tk_choose.files(
    default = default,
    caption = caption,
    multi = multi
  )

  if (length(selected.files) == 0) {
    stop(error_message, call. = FALSE)
  }

  selected.files
}

.Tcl <- function(...) {
  .stylo_namespace_function("tcltk", ".Tcl")(...)
}

tclVar <- function(...) {
  .stylo_namespace_function("tcltk", "tclVar")(...)
}

tclvalue <- function(...) {
  .stylo_namespace_function("tcltk", "tclvalue")(...)
}

`tclvalue<-` <- function(x, value) {
  .stylo_namespace_function("tcltk", "tclvalue<-")(x, value)
}

tktoplevel <- function(...) {
  .stylo_namespace_function("tcltk", "tktoplevel")(...)
}

tktitle <- function(...) {
  .stylo_namespace_function("tcltk", "tktitle")(...)
}

`tktitle<-` <- function(x, value) {
  .stylo_namespace_function("tcltk", "tktitle<-")(x, value)
}

tkdestroy <- function(...) {
  .stylo_namespace_function("tcltk", "tkdestroy")(...)
}

tkradiobutton <- function(...) {
  .stylo_namespace_function("tcltk", "tkradiobutton")(...)
}

tkconfigure <- function(...) {
  .stylo_namespace_function("tcltk", "tkconfigure")(...)
}

tklabel <- function(...) {
  .stylo_namespace_function("tcltk", "tklabel")(...)
}

tkcheckbutton <- function(...) {
  .stylo_namespace_function("tcltk", "tkcheckbutton")(...)
}

tkentry <- function(...) {
  .stylo_namespace_function("tcltk", "tkentry")(...)
}

tkbutton <- function(...) {
  .stylo_namespace_function("tcltk", "tkbutton")(...)
}

tkbind <- function(...) {
  .stylo_namespace_function("tcltk", "tkbind")(...)
}

tkgrid <- function(...) {
  .stylo_namespace_function("tcltk", "tkgrid")(...)
}

`tkgrid.forget` <- function(...) {
  .stylo_namespace_function("tcltk", "tkgrid.forget")(...)
}

tkframe <- function(...) {
  .stylo_namespace_function("tcltk", "tkframe")(...)
}

`tkwait.window` <- function(...) {
  .stylo_namespace_function("tcltk", "tkwait.window")(...)
}

ttkcombobox <- function(...) {
  .stylo_namespace_function("tcltk", "ttkcombobox")(...)
}

`tk_choose.dir` <- function(...) {
  .stylo_namespace_function("tcltk", "tk_choose.dir")(...)
}

`tk_choose.files` <- function(...) {
  .stylo_namespace_function("tcltk", "tk_choose.files")(...)
}

tk2tip <- function(...) {
  .stylo_namespace_function("tcltk2", "tk2tip")(...)
}
