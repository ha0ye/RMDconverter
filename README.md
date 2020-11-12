
# RMDconverter

<!-- badges: start -->
[![R build status](https://github.com/ha0ye/RMDconverter/workflows/R-CMD-check/badge.svg)](https://github.com/ha0ye/RMDconverter/actions)
<!-- badges: end -->

The goal of RMDconverter is to provide functions for converting the contents of various RMD files from one export-type to another.

Currently, the only supported conversion is from **`xaringan`**-based slides to a markdown set of notes.

## Installation

You can install the latest version of RMDconverter from [GitHub](https://github.com/ha0ye/RMDconverter) with:

``` r
# install.packages("remotes")
remotes::install_github("ha0ye/RMDconverter")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RMDconverter)

slides_file <- system.file("extdata/test_slides.Rmd", package = "RMDconverter")
md_file <- here::here("slides_temp.md")
out_file <- here::here("test_out.md")

process_slides(slides_file = slides_file, 
               md_file = md_file,
               out_file = out_file,
               clean = FALSE)
```


