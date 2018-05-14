# stylo: R package for stylometric analyses

**Authors:** Maciej Eder, Mike Kestemont, Jan Rybicki<br/>
**License:** [GPL-3](https://opensource.org/licenses/GPL-3.0)


[![CRAN Version](http://www.r-pkg.org/badges/version/stylo)](https://CRAN.R-project.org/package=stylo)


![AppVeyor](https://img.shields.io/appveyor/ci/gruntjs/grunt.svg)


[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/computationalstylistics/stylo?branch=master&svg=true)](https://ci.appveyor.com/project/computationalstylistics/stylo) 

[![Travis-CI Build Status](https://travis-ci.org/computationalstylistics/stylo.svg?branch=master)](https://travis-ci.org/computationalstylistics/stylo) [![Coverage Status](https://img.shields.io/codecov/c/github/computationalstylistics/stylo/master.svg)](https://codecov.io/github/computationalstylistics/stylo?branch=master)
![Downloads](http://cranlogs.r-pkg.org/badges/stylo)


This package provides a number of functions, supplemented by a GUI, to perform various
analyses in the field of computational stylistics, authorship attribution, etc.

See: https://sites.google.com/site/computationalstylistics/ for further details.

If you find the package `stylo` useful and plan to publish your results, please consider citing the following paper:

**Eder, M., Rybicki, J. and Kestemont, M.** (2016). Stylometry with R: a package for computational text analysis. _R Journal_, 8(1): 107-21.  [https://journal.r-project.org/archive/2016/RJ-2016-007/index.html](https://journal.r-project.org/archive/2016/RJ-2016-007/index.html)



## Installation

There are three ways of installing `stylo`:

* from CRAN repository
* from the GitHub repository, via the package `devtools`
* from a locally downloaded file

###  Installing from CRAN

Launch R, make sure you are connected to the internet, type: 

```
install.packages("stylo")
```

choose your favorite CRAN mirror (a window will pop up), click OK.

**NOTE** (Mac OS users): the package "stylo" requires X11 support being installed. To quote "R for Mac OS X FAQ" (http://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html): "Each binary distribution of R available through CRAN is build to use the X11 implementation of Tcl/Tk. Of course a X windows server has to be started first: this should happen automatically on OS X, provided it has been installed (it needs a separate install on Mountain Lion or later). The first time things are done in the X server there can be a long delay whilst a font cache is constructed; starting the server can take several seconds." The newest versions of R (>3.1.0) seem to have Tcl/Tk support out-of-the-box, though.

**ANOTHER NOTE** (Mac Os users again): when you install a recent version of R on OS X (e.g. Mavericks), you might run into encoding errors when you start up R (e.g. "WARNING: You're using a non-UTF8 locale" etc.). In that case, you should close R, open a new window in Applications > Terminal and execute the following line:


```
defaults write org.R-project.R force.LANG en_US.UTF-8
```

Next, close the Terminal and start up R again.


### from the GitHub repository

A convenient way to install R packages directly from the GitHub repository is to use the package `devtools`. Unless you have already installed it, you should do it now:

```install.packages("devtools")```

Then, install the package `stylo`

```
library(devtools)
install_github("computationalstylistics/stylo")
```



### Installing from a local file

Download the package from **here**; save the file anywhere on your computer where you will be able to find it; launch R; set working directory to the folder where the downloaded file is (please keep it mind that the slashes might look different in different operating systems):

```
setwd("i/hope/i/can/remember/where/it/was/")
```

Install the package:

```
install.packages("stylo_0.6.5.tar.gz", repos = NULL, type = "source")
```

**NOTE**: the `stylo` package requires a few standard R packages to be installed. When installing from CRAN or from GitHub, the dependencies are downloaded automatically; otherwise, you have to install them manually. Type (or copy-paste) the following lines:

```
install.packages("tcltk2")
install.packages("ape")
install.packages("class")
install.packages("e1071")
install.packages("pamr")
install.packages("tsne")
```




