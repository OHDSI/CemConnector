CEMConnector
====================

[![Build Status](https://github.com/OHDSI/CEMConnector/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CEMConnector/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/CEMConnector/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CEMConnector?branch=master)

CEMConnector is part of [HADES](https://ohdsi.github.io/Hades).

Introduction
============
This is an API and client for the Common Evidence Model that takes in data from clinical trials, spontaneous reports,
 labels and literature.
 


Features
========
- Plumber API that sits on top of CEM database to allow access to common evidence model data
- Database and Web backends that have equivalent functionality
- Fast, programmatic way of getting negative controls from CEM
- Get all related data for sets of standard conceptsets

Example
========
```r
library(CEMConnector)



```

Technology
============
CEMConnector is an R package.

System Requirements
============
Requires R. Libraries used in CEMConnector require Java.

Getting Started
===============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. In R, use the following commands to download and install CEMConnector:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/CEMConnector")
  ```

User Documentation
==================


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CEMConnector/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.


License
=======
CEMConnector is licensed under Apache License 2.0

Development
===========
CEMConnector is being developed in R Studio and PyCharm (with R plugins).

### Development status

Beta
