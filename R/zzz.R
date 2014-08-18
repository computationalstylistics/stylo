
.onAttach <- function(lib, pkg) {
    packageStartupMessage("stylo version: ", utils::packageVersion("stylo"))
}
