CemConnector
====================

[![Build Status](https://github.com/OHDSI/CemConnector/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CemConnector/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/CemConnector/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CemConnector?branch=master)

CemConnector is built on top of [HADES](https://ohdsi.github.io/Hades).

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
library(CemConnector)
cemConnectorUrl <- "https://cem.ohdsi.org"
cemConnection <- CemWebApiBackend$new(apiUrl = cemConnectorUrl)

myExposureConceptSet <- data.frame(conceptId = 1201620, includeDescendants = 1, isExcluded = 0)

# Get 50 negative control concepts for this exposure
cemConnection$getSuggestedControlCondtion(myExposureConceptSet)

# Get any related evidence for this exposure and an outcome of interest
myOutcomeConceptSet <- data.frame(conceptId = 4149320, includeDescendants = 1, isExcluded = 0)
cemConnection$getRelationships(myExposureConceptSet, myOutcomeConceptSet, conditionSiblingLookupLevels = 1)

```
For instructions on building good concept sets see [PHEOBE](https://data.ohdsi.org/PHOEBE/).

Technology
============
CemConnector is an R package.

System Requirements
============
Requires R. Libraries used in CemConnector require Java.

Getting Started
===============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. In R, use the following commands to download and install CemConnector:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/CemConnector")
  ```

User Documentation
==================


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CemConnector/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.


License
=======
CemConnector is licensed under Apache License 2.0

Development
===========
CemConnector is being developed in R Studio and PyCharm (with R plugins).

### Development status

Early development - not intended for use
