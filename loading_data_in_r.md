<!--
title: Loading Data in R
-->

Getting your data into R is 

- Comma separated value (`.csv`) files

    - `read.csv` is the standard function included in R. However, it can be slow for very large datasets.
	- The [readr](https://github.com/hadley/readr) package is very new, but provides much faster reading of csv files.
	
- Stata (`.dta`):

    - `foreign` package function `read.dta` is the standard way to do this.
	  But this **does not** work with datasets created by Stata 13.
	- The package [haven](https://github.com/hadley/haven/) reads Stata `.dta` files from all versions (in addition to SAS and SPSS files).

- SPSS (`.por`, `.sav`). The function `read.spss` in the `foreign` library.
- Excel (`.xls`, `.xlsx` files). There are several packages that can read these. However, the newest and best is the [readxl](https://github.com/hadley/readxl) package. It is very new (not on CRAN), but provides fast reading *and* writing of xls fils. However, try to avoid dealing with Excel files if at all possible; while you may need to read data from one, you should not save your own data in them.
- PDF ... abandon all hope, nothing can save you.
- Increasingly data is available on the web through [APIs](http://en.wikipedia.org/wiki/Application_programming_interface). See the CRAN task view [Web Technologies](https://cran.r-project.org/web/views/WebTechnologies.html) for a list of packages that provide interfaces to web data. For example,

    - [WDI](https://cran.r-project.org/web/packages/WDI/index.html) World Bank's World Development Indicators.
	- The [rOpenGov](http://ropengov.github.io/) project maintains many government data related R packages.

- R has functions and packages to handle many more types of data and documents (json, HTML, XML, databases, ...) but you will probably not encounter them in this course. If you do, try finding it up yourself, and if you cannot, ask one of us and we'll point you to the right means to load it.

## References

A few other articles on this. They may suggest other functions (there are often many ways to do the same thing in R) or cover other types of files:

- Quick-R [Importing Data](http://www.statmethods.net/input/importingdata.html)
- http://www.r-tutor.com/r-introduction/data-frame/data-import
