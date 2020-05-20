# wdl-fies

This is the repository for the World Data Lab's analysis of the Food Insecurity Experience Scale.

This repo is organized using the ProjectTemplate package.  [Read more about it](http://projecttemplate.net/) if you are unfamiliar.

Raw data is stored in the `data` folder, and is processed using the scripts in the `munge` folder.  The results of munging are then saved in the `cache` folder.  Then, drawing on those intermediate cached files, model scripts are stored in the `models` folder and scripts for visualization are stored in the `viz` folder, with graphic outputs stored in the `figures` folder.  Utility function are stored in the `lib` folder and documentation is stored in the `docs` folder.

# Getting started

First, copy the data into the `data` folder.  The readme in the data folder documents all existing files.  Keep it up to date if you add more data.

Then, to run the munging scripts and generate the cached files, run:

```
library(ProjectTemplate)
setwd(<your directory>)
load.project(list(data_loading=TRUE, munging=TRUE))
```

If you already have the cached files and do not need to run the munging scripts, simply run: 

```
library(ProjectTemplate)
setwd(<your directory>)
load.project()
```
