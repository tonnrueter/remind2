# The REMIND R package (2nd generation)

R package **remind2**, version **1.9.1**

[![CRAN status](https://www.r-pkg.org/badges/version/remind2)](https://cran.r-project.org/package=remind2)    

## Purpose and Functionality

Contains the REMIND-specific routines for data and model output manipulation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remind2")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("remind_summary") # Adding plots to the REMIND_summary.pdf
```

## Automated Tests

When building the library, GDXs are downloaded to test the creation of the reporting. For requests to update the test specimen, please file an issue or contact the package maintainer. Note that you can also manually place one or multiple GDX files in the `tests/testgdxs` folder to have them tested instead of the default ones.


If you want to disable the GDX test, please uncomment the line following the comment `## uncomment to skip test` in `tests/testthat/test-convGDX2mif.R`.

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **remind2** in publications use:

Rodrigues R (2021). _remind2: The REMIND R package (2nd generation)_. R
package version 1.9.1.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remind2: The REMIND R package (2nd generation)},
  author = {Renato Rodrigues},
  year = {2021},
  note = {R package version 1.9.1},
}
```

