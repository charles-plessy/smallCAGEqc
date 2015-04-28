smallCAGEqc
===========

_smallCAGEqc is a R package collecting scripts for the analysis of transcriptome
libraries at RIKEN CLST/DGT ex-OSC.  It makes strong assumptions on our
infrastructure and hardcodes paths for key files, therefore, although it
shared on GitHub, there is really no guaranty that it will be useful to
you if we are not working together already._


Installation on Linux computers
-------------------------------

### From GitHub (recommended)

```
devtools::install_github('smallCAGEqc','charles-plessy')
```


### From a Git clone.

```
git clone https://github.com/charles-plessy/oscR.git
R CMD INSTALL smallCAGEqc
```

In case of error `no permission to install to directory
‘/usr/local/lib/R/site-library’`, create a local R directory with the following
command.

```
/usr/bin/Rscript -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE)'
````
