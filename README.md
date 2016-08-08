[![Build Status](https://travis-ci.org/ablejec/animatoR.svg?branch=master)](https://travis-ci.org/ablejec/animatoR)
[![Coverage Status](https://img.shields.io/codecov/c/github/ablejec/animatoR/master.svg)](https://codecov.io/github/ablejec/animatoR?branch=master)
[![codecov](https://codecov.io/gh/ablejec/animatoR/branch/master/graph/badge.svg)](https://codecov.io/gh/ablejec/animatoR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/animatoR)](http://cran.r-project.org/package=animatoR)
[![tag](https://img.shields.io/github/tag/ablejec/animatoR.svg)](https://github.com/ablejec/animatoR/releases)
# Package `animatoR`
Support for Animated Graphics in Base R Graphics

Enable plotting of graphics elements (e.g. points, lines, segments) in any position between two predefined positions. 
The central function is `animator()` which takes care about the repeting plotting of graphical elements in different positions. 
As a simple example, user can define the starting and ending position of a point and `animator()` 
will produce plots of the point between those positions. 
The result is an animated impression of the point moving along the screen.

## Installation

**Note:** package is under development, some things might not work or will be changed!

```R
install.packages("devtools")
devtools::install_github("ablejec/animatoR")
```

