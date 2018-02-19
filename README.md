Modown (in dev.): a minimalist file format for morphometrics data
--------
[![Travis-CI Build Status](https://travis-ci.org/vbonhomme/coo.svg?branch=master)](https://travis-ci.org/vbonhomme/coo)
[![Coverage Status](https://img.shields.io/codecov/c/github/vbonhomme/coo/master.svg)](https://codecov.io/github/vbonhomme/coo?branch=master)
<!--
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/coo)](http://cran.r-project.org/package=coo)
![CRAN downloads last month](http://cranlogs.r-pkg.org/badges/coo) ![CRAN downloads grand total](http://cranlogs.r-pkg.org/badges/grand-total/coo)
-->

### Rationale
Morphometrics data essentially consist of coordinates and, if any, associated covariates.

Existing morphometrics, eg `.tps`, `.nts`, `.xml`, etc. are not fully generic and/or explicitely defined/consistent, etc.

I propose here the `.mod` (short for `Modown`) file format along with R utilities.

Just like markdown, `Modown` has minimalist syntax and is easy to read/manipulate/write both for humans and machines.

Modown name is derived from [markdown](https://daringfireball.net/projects/markdown/), sounds a bit like [Momocs](https://cran.r-project.org/web/packages/Momocs/index.html) and is a tribute to [Motown](https://en.wikipedia.org/wiki/Motown).

### Specifications
`.mod` files are plain text files whose single line syntax fall within one of the five following rules:

1. **space-separated numbers** stand for coordinates or other numeric data in each dimension: eg `-0.5 0.5`; `0 0 0`
2. **word and word/number** include a covariate name and its value: eg `LM`; `out`
3. **single word** introduces a partition of coordinates: eg `size 3.14` <br />`sp H. sapiens`
4. **tilde and word** names a shape when several are collated in the same file: eg `~iris150`
5. **anything else** is ignored: eg `#a comment`

### Examples

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

---------
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
