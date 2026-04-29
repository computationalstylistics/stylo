
# NOTE: integrate the following function with .stylo_require_gui("gui.stylo.R") 
#
#
detect_gui_capability = function() {

  # check if R is running in an interactive session
  is_interactive = interactive()

  # check if we are connected via SSH
  # test variables that SSH produces when connected
  is_remote = Sys.getenv("SSH_CLIENT") != "" ||
    Sys.getenv("SSH_TTY") != "" ||
    Sys.getenv("SSH_CONNECTION") != ""

  # we only want a GUI if we are interactive **and not** remote
  # this is to prevent launching GUI on my remote desktop :-)
  # but perhaps other users will also find it useful
  if (is_interactive && !is_remote) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

