
# Package `animatoR`
Support for Animated Graphics in Base R Graphics

This package enable plotting of graphics elements (e.g. points, lines, segments) in any position between two predefined positions. 
The central function is `animator()` which takes care about the repeting plotting of graphical elements in different positions. 
As a simple example, user can define the starting and ending position of a point and `animator()` 
will produce plots of the point between those positions. 
The result is an animated impression of the point moving along the screen.

## Installation


```R
# install.packages("devtools")`
devtools::install_github("ablejec/animatoR")
```

