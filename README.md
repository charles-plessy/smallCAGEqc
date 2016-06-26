smallCAGEqc
===========

_smallCAGEqc is a R package collecting scripts for the analysis of transcriptome
libraries at RIKEN CLST/DGT ex-OSC.  It makes strong assumptions on our
infrastructure and hardcodes paths for key files, therefore, although it
shared on GitHub, there is really no guaranty that it will be useful to
you if we are not working together already.  It installs a Rmarkdown
[template][] for analyses on Rstudio._

  [template]: https://raw.githubusercontent.com/charles-plessy/smallCAGEqc/master/inst/rmarkdown/templates/nanoCAGE/skeleton/skeleton.Rmd

Installation on Linux computers
-------------------------------

### From GitHub in `R` (recommended)

```
devtools::install_github('charles-plessy/smallCAGEqc', upgrade_dependencies = FALSE)
```


### From a Git clone.

```
git clone https://github.com/charles-plessy/smallCAGEqc.git
R CMD INSTALL smallCAGEqc
```

In case of error `no permission to install to directory
‘/usr/local/lib/R/site-library’`, create a local R directory with the following
command.

```
/usr/bin/Rscript -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE)'
````
