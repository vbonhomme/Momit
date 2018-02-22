Momit: a file format for morphometric data exchange and conversion
--------
![maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![Travis-CI Build Status](https://travis-ci.org/MomX/Momit.svg?branch=master)](https://travis-ci.org/MomX/Momit)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Momit)](http://cran.r-project.org/package=Momit)
[![Coverage Status](https://img.shields.io/codecov/c/github/vbonhomme/Momit/master.svg)](https://codecov.io/github/MomX/Momit?branch=master)
<!--
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/coo)](http://cran.r-project.org/package=coo)
![CRAN downloads last month](http://cranlogs.r-pkg.org/badges/coo) ![CRAN downloads grand total](http://cranlogs.r-pkg.org/badges/grand-total/coo)
-->
## Get it
```
devtools::install_github("vbonhomme/Momit", build_vignettes = TRUE)
```

This package is still in development and will be published on CRAN in spring 2018.

## Rationale
 * Morphometrics data essentially consist of coordinates and, if any, associated covariates.

* Existing morphometrics file format, eg `.tps`, `.nts`, `.xml`, etc. are not fully generic and/or explicitely defined/consistent, etc.

* Here is proposed the `.mom` (short for *mo*dern *m*orphometrics and to echo [`MomX`](https://github.com/MomX)) file format along with R utilities to import, parse, manipulate, export them.

* `.mom` files are easy to read, for humans, and to parse, for computers.

## Definition
`.mod` files are plain text files whose single line syntax fall within one of the five following rules:

Rule | Pattern                 | What                          | Examples
-----|-------------------------|-------------------------------|--------------
 1   | space-separated numbers | coordinates in each dimension | `-0.5 0.5`; `0 0 0`
 2   | word and word/number    | covariate name and its value  | `scale 56`
 3   | single word             | partition of coordinates      | `LM` ; `out`
 4   | tilde and word          | shape name when collated      |  `~iris150`  ; `~H. sapiens`
 5   | anything else           | ignored                       | `#a comment`
 
**TODO**: handle units, handle missing data (?)

## Supported formats

extension | software              | `from_*` function  | `to_*` function 
----------|-----------------------|--------------------------------------
`.tps`    | tps series and others | `from_tps`         | planned
`.nts`    | tps series and others | `from_nts`         | planned
`.lmk`    | meshtools             | `from_lmk`         | planned
`.stv`    | meshtools             | `from_stv`         | planned
`.txt`    | StereoMorph           | `from_StereoMorph` | 
`.asc`    | Optimas               | `from_Optimas`     | planned
`.txt`    | ImageJ xy (among ot.) | planned            | planned
`.xml`    | morphoJ               | planned            |
`.txt`    | morphologika          | planned            |
`.txt`    | PAST                  | planned            | planned

__Any suggestion of additional formats/softs, along with example datasets are more than welcome.__
 
## Examples

A single shape with one covariate:

```
name brahma
type beer
37  561
40  540
40  529
[...]
```
Two shapes with more covariates/cofactors:

```
~0001-cAglan_O10VD
var Aglan
domes cult
view VD
ind O10
-0.5000 0.00000
-0.4967 0.01035
-0.4935 0.02414
[...]
~0001-cAglan_O10VL 
var Aglan
domes cult
view VL
ind O10
-0.5000 0.00000
-0.4995 0.01018
-0.4957 0.02022
[...]
```

A single shape with landmarks and 2 partitions of semi-landmarks:

```
id 571
taxa T. mono
ldk
697  977
766  991
704 1046
[...]
sl1
541 962
542 965
543 967
[...]
sl2
541 949
542 952
542 954
[...]
```

A shape with nothing else but coordinates:

```
200   91
187   95
173  105
[...]
```
Examples adapted from [Momocs](https://github.com/vbonhomme/Momocs/): `bot[1]`, `olea[1]`, `charring[1]`, `shapes[1]`.

## Thanks
See `?Momit`.
