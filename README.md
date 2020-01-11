# Screenmill R package

This package is designed for quantifying arrayed colony growth experiments Synthetic Genetic Arrays (SGA), Epistatic Mini-Array Profiling (E-MAP), Selective Ploidy Ablation (SPA), or a simple quantitative colony fitness assay. This software was designed to efficiently measure hourly growth measurements given images of colony grids captured on a flatbed scanner. Colonies are quantified using background subtracted pixel intensity. Image transformations and colony coordinates are saved in simple CSVs allowing one to individually crop colonies and stich them together programmatically to generate colony growth kymographs.

## Installation

This package can be installed from GitHub by following the instructions below.

**Step 1**: Install [R](https://cloud.r-project.org) (>= 3.3.0 recommended).

**Step 2**: Install R package developer tools. Why? This package contains some Rcpp code that must be compiled, so you will need a C++ compiler (e.g. [GCC](https://gcc.gnu.org), or [clang](http://clang.llvm.org)). For more help, checkout this guide for R's [package development prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites).

- **Mac OS X**: Install [Xcode](https://developer.apple.com/xcode/) developer tools (in terminal: `xcode-select --install`).
- **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
- **Debian/Ubuntu**: Install the build-essential package in shell with: `sudo apt-get install build-essential`

**Step 3 (*optional*)**: Install [RStudio](https://www.rstudio.com). Why? RStudio makes programming in R fun!

**Step 4**: Install the package by running the following commands in R

```r
# Install the latest version of Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite()

# Install the latest version of devtools
install.packages('devtools', dependencies = T)

# Install the latest version of screenmill
devtools::install_github('EricBryantPhD/screenmill')
```

# Usage


Rothstein lab members should install [rothfreezer](https://github.com/ericbryantphd/rothfreezer)
as the default annotation database (i.e. no need to change screenmill options)

Non Rothstein lab members should manage their own annotation tables which can
be used with screenmill by setting screenmill options. Expected fields 
for these tables are described in `?screenmill::annotate`.

Screenmill annotations, calibrations and measurements are saved as CSVs 
in the processed directory. Re-running this script on a directory that has
already been processed will synchronize the project with the latest annotations
while leaving everything else untouched. This is usefull if a key needs to 
be fixed for many projects.

```r
library(screenmill)

# Non Rothstein lab members should set these options. See ?screenmill::annotate for help
options(
  screenmill.queries                = custom_queries_dataframe,
  screenmill.strain_collections     = custom_collections_dataframe,
  screenmill.strain_collection_keys = custom_keys_dataframe,
  screenmill.media                  = custom_media_dataframe,
  screenmill.treatments             = custom_treatments_dataframe
)

dir = 'path/to/directory-of-plate-images'

annotate(dir)
calibrate(dir, grid_rows = 32, grid_cols = 48)
measure(dir)
review(dir)
```

After processing images, measurements can be read into an R session
for analysis and visualization with the following command:

```r
data <- read_screenmill(dir)
```

Want to combine results from multiple projects?

```r
library(tidyverse)
dirs <- c('path/to/project1', 'path/to/project2', 'path/to/project3')
data <- map_df(dirs, read_screenmill)
```

# Features

- Efficient quantification of time-series colony growth experiments.
- Individually crop colonies and generate colony growth kymographs.
- Supports measurement of muliple plates in a single image (e.g. multiple plates imaged on a large flatbed scanner).
- Tools for working with legacy [ScreenMill](http://www.rothsteinlab.com/tools/screen_mill/cm_engine) data.

