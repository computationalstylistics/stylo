# stylo: R package for stylometric analyses

**Authors:** Maciej Eder<sup>*</sup>, Mike Kestemont, Jan Rybicki, Steffen Pielström<br/>
**License:** [GPL-3](https://opensource.org/licenses/GPL-3.0)


[![CRAN Version](http://www.r-pkg.org/badges/version/stylo)](https://CRAN.R-project.org/package=stylo) [![Travis-CI Build Status](https://travis-ci.org/computationalstylistics/stylo.svg?branch=master)](https://travis-ci.org/computationalstylistics/stylo) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/stylo)](https://www.rpackages.io/package/stylo) ![Downloads](http://cranlogs.r-pkg.org/badges/stylo)


This package provides a number of functions, supplemented by a GUI, to perform various
analyses in the field of computational stylistics, authorship attribution, etc.

Refer to the [Computational Stylistics Group webpage](https://computationalstylistics.github.io/), especially the subpage [Projects](https://computationalstylistics.github.io/projects/), to get some ideas about possible applications of the package `stylo`.




## Citation

If you find the package `stylo` useful and plan to publish your results, please consider citing the following paper:

**Eder, M., Rybicki, J. and Kestemont, M.** (2016). Stylometry with R: a package for computational text analysis. _R Journal_, 8(1): 107-21.  [https://journal.r-project.org/archive/2016/RJ-2016-007/index.html](https://journal.r-project.org/archive/2016/RJ-2016-007/index.html)





## Installation

There are four ways of installing `stylo`:

1. from CRAN repository
2. from the GitHub repository, via the package `devtools`
3. from a locally downloaded file
4. building the package directly from source files





###  1. Installing from CRAN repository

This is the simplest way to install `stylo` (as well as any other R package). Launch R, make sure you are connected to the internet, type: 

```
install.packages("stylo")
```

choose your favorite CRAN mirror (a window will usually pop up), click OK.

If you are a MacOS user, please have a look below, at the **Installation issues** section.





### 2. Installing from the GitHub repository

A convenient way to install R packages directly from the GitHub repository is to use the package `devtools`. Unless you have already installed it, you should do it now:

```install.packages("devtools")```

Then, install the package `stylo`:

```
library(devtools)
install_github("computationalstylistics/stylo")
```

The remarks about possible issues on MacOS apply are valid also in this case.



### 3. Installing from a local file

This is an option for more advanced users. You need to obtain a so-called tarball file, which is a compressed version of the package (you can grab it from CRAN). It might be named `stylo_0.6.9.tar.gz`, depending of the current version of course. Then type in R console:

```
setwd("I/hope/I/can/remember/where/I/have/put/the/zipfile/")
install.packages("stylo_0.7.1.tar.gz", repos = NULL, type = "source")
```




### 4. Building a package from source files

This is something for real geeks. Clone this very repository, unpack it, and type the following lines at the command prompt:

```
R CMD build stylo
R CMD INSTALL stylo
```


## Installation issues


**NOTE** (Mac OS users): the package `stylo` requires X11 support being installed. To quote "R for Mac OS X FAQ" (http://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html): “Each binary distribution of R available through CRAN is build to use the X11 implementation of Tcl/Tk. Of course a X windows server has to be started first: this should happen automatically on OS X, provided it has been installed (it needs a separate install on Mountain Lion or later). The first time things are done in the X server there can be a long delay whilst a font cache is constructed; starting the server can take several seconds.”

You might also run into encoding errors when you start up R (e.g. “WARNING: You're using a non-UTF8 locale” etc.). In that case, you should close R, open a new window in Applications > Terminal and execute the following line:

```
defaults write org.R-project.R force.LANG en_US.UTF-8
```

Next, close the Terminal and start up R again.

**ANOTHER NOTE** A slightly different workaround of the above problem (Mac users again):

* Install XQuartz, restart Mac
* Open Terminal, type: `sudo ln -s /opt/X11 /usr/X11`
* Run XQuartz
* Run R, type: `system('defaults write org.R-project.R force.LANG en_US.UTF-8')`


**YET ANOTHER NOTE** On MacOS Mojave one usually faces the problem of not properly recognized tcltk support. Open your terminal and type the following command:

`xcode-select --install`

This will download and install xcode developer tools and fix the problem. The problem is that one needs to explicitly agree to the license agreement.





## Usage

This section is meant to give the users a general outline of what the package can do, rather than providing a comprehensive description of designing a stylometric test using the R package `stylo`. Refer to the following documents:

* for (real) beginners: a crush introduction in the form of a [slideshow](https://computationalstylistics.github.io/stylo_nutshell/)
* for (sort of) beginners: a concise [HOWTO](https://sites.google.com/site/computationalstylistics/stylo/stylo_howto.pdf)
* for advanced users: a paper in [R Journal](https://journal.r-project.org/archive/2016/RJ-2016-007/RJ-2016-007.pdf)
* full documentation at [CRAN](https://cran.r-project.org/web/packages/stylo/stylo.pdf)






## Docs on non-obvious functionalities


* [Authorship verification with the package 'stylo'](https://computationalstylistics.github.io/blog/imposters)
* [Cross-validation using the function `classify()`](https://computationalstylistics.github.io/blog/cross-validation)
* [Custom distance measures](https://computationalstylistics.github.io/blog/custom_distances)
* [Testing rolling stylometry](https://computationalstylistics.github.io/blog/rolling_stylometry)
* [Using ‘Stylo’ with languages other than English](https://computationalstylistics.github.io/blog/stylo_and_languages/)






## Other relevant resources


* Despite a black legend, R and Python are not necessarily in a deadly clash: here is [a great post](https://cligs.hypotheses.org/577) by José Calvo Tello on invoking the package `stylo` directly from Python!

* Using the package `stylo` with the TXM environment: see [this post](https://groupes.renater.fr/wiki/txm-users/public/tutorial_to_use_stylo_into_txm) by Serge Heiden.

* Probably not a  bad idea to check a comprehensive [Stylometry Bibliography](https://www.zotero.org/groups/643516/stylometry_bibliography) curated by Christof Schöch, before starting an experiment in text analysis.

* The package `stylo` has been created as a by-product of a few projects conducted by the Computational Stylistics Group. See [this website](https://computationalstylistics.github.io/) for further details. An [older version](https://sites.google.com/site/computationalstylistics/) of the webpage is also there, even if it has not been be updated for a while.

 






### 3. Installing from a local file

Download the package from **here**; save the file anywhere on your computer where you will be able to find it; launch R; set working directory to the folder where the downloaded file is (please keep it mind that the slashes might look different in different operating systems):

```
setwd("I/hope/I/remember/where/it/was/")
```

Install the package:

```
install.packages("stylo_0.7.1.tar.gz", repos = NULL, type = "source")
```

**NOTE**: the `stylo` package requires a few standard R packages to be installed. When installing from CRAN or from GitHub, the dependencies are downloaded automatically; otherwise, you have to install them manually. Type (or copy-paste) the following lines:

```
install.packages("tcltk2")
install.packages("ape")
install.packages("class")
install.packages("e1071")
install.packages("pamr")
install.packages("tsne")
install.packages("foreach")
```



## Usage

This section is meant to give the users a general outline of what the package can do, rather than providing a comprehensive description of designing a stylometric test using the R package `stylo`. Refer to the following documents:

* for (real) beginners: a crush introduction in the form of a [slideshow](https://computationalstylistics.github.io/stylo_nutshell/)
* for (sort of) beginners: a concise [HOWTO](https://sites.google.com/site/computationalstylistics/stylo/stylo_howto.pdf)
* for advanced users: a paper in [R Journal](https://journal.r-project.org/archive/2016/RJ-2016-007/RJ-2016-007.pdf)
* full documentation at [CRAN](https://cran.r-project.org/web/packages/stylo/stylo.pdf)






## Docs on non-obvious functions


* [Authorship verification with the package 'stylo'](https://computationalstylistics.github.io/blog/imposters)
* [Cross-validation using the function `classify()`](https://computationalstylistics.github.io/blog/cross_validation)
* [Custom distance measures](https://computationalstylistics.github.io/blog/custom_distances)
* [Testing rolling stylometry](https://computationalstylistics.github.io/blog/rolling_stylometry)







## Other relevant resources


* Despite a black legend, R and Python are not necessarily in a deadly clash: here is [a great post](https://cligs.hypotheses.org/577) by José Calvo Tello on invoking the package `stylo` directly from Python!

* Using the package `stylo` with the TXM environment: see [this post](https://groupes.renater.fr/wiki/txm-users/public/tutorial_to_use_stylo_into_txm) by Serge Heiden.

* Probably not a  bad idea to check a comprehensive [Stylometry Bibliography](https://www.zotero.org/groups/643516/stylometry_bibliography) curated by Christof Schöch, before starting an experiment in text analysis.

* The package `stylo` has been created as a by-product of a few projects conducted by the Computational Stylistics Group. See [this website](https://computationalstylistics.github.io/) for further details. An [older version](https://sites.google.com/site/computationalstylistics/) of the webpage is also there, even if it has not been be updated for a while.

 




