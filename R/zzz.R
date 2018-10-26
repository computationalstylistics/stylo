
.onAttach <- function(lib, pkg) {
    packageStartupMessage("\n### stylo version: ", utils::packageVersion("stylo"), " ###\n", "\nIf you plan to cite this software (please do!), use the following reference:", "\n    Eder, M., Rybicki, J. and Kestemont, M. (2016). Stylometry with R:", "\n    a package for computational text analysis. R Journal 8(1): 107-121.", "\n    <https://journal.r-project.org/archive/2016/RJ-2016-007/index.html>\n", "\nTo get full BibTeX entry, type: citation(\"stylo\")")
}
