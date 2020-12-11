
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

The goal of `{labor}` is to keep your project folder structured, tidy
and to sync it easily to local folders.

The package works only on MacOs systems.

# Installation

Download the package in a local folder and then run the following code.

``` r
devtools::install_local("path_to_package")
```

Alternatively, you can install it directly from github.

``` r
library(devtools)
devtools::install_github("c1au6i0/labor")
```

# Usage

## Create a folder tree

To set up your lab folder, start running `create_labtree`.

![](inst/gifs_readme/create_labtree.gif)

The function generates the folder structure generally used for projects
in our lab, and relative `README` files

You can run the function multiple types and you will be prompt to decide
if overwrite or not particular folders.

![](inst/gifs_readme/create_labfolder2.gif)

Use `remove_labtree` to remove the folders just created.

## Sync with local folder

Under the hood, the package `{labor}` use `rsync` to sync folders Set up
the destination folder using `set_sync_lab`

![](inst/gifs_readme/set_destination.gif)

The destination folder is saved in a file file in the project directory
`.labor_destination`.

Then run `sync_lab` to sync the project directory.

![](inst/gifs_readme/sync.gif)

<span style="color:red; font-weight:bold;"> Convinient things about
`lab_sync` </span> :

1.  No need to retype origin and destination everytime in `rsync`.
2.  Files containing authentication information and `renv` packages are
    not synced. You can use `exclude_files` to decide to not sync other
    specific files.
3.  You can bidirectianl sync the folders setting the argument
    `direction`
4.  You can still decide to use any `rsync` flags setting the argument
    `rsync_flags` (default is `-avtuP`)

## Are you keeping things in the right place?

Run `check_lab` to see if you have messy directories.

![](inst/gifs_readme/check_folder.gif)

The report generated, indicates the absolute and over the total number
of files missplaced. It also lists the misplaced files.
