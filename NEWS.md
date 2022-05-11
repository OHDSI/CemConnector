CemConnector 0.1.3
==================
Changes:
1. Changed use of `cemSchema`, `vocabularySchema`, `sourceSchema` to `cemDatabaseSchema`, `vocabularyDatabaseSchema`, 
`sourceDatabaseSchema` to be consistent across HADES packages

Bug fixes:
1. Fixed broken test cases

CemConnector 0.1.2
==================
Changes:
1. Search button in explorer, rather than changing on user input
2. Default suggested number of negative controls is now 100 instead of 50


CemConnector 0.1.1
==================
Changes:
1. Reformatted table evidence explorer tool to only include relevant columns
2. Evidence explorer now launches connecting to default cem.ohdsi.org
3. Added withProgress for queries in shiny to improve UX.
4. Allow submission of comma separated list and json inputs for concept sets in explorer

Bug Fixes:
1. Fixed display of error messages in evidence explorer
2. Fixed searches with backend throwing bad SQL error.
   Concept sets are now checked to see if all concepts are excluded, with an informative error message.
3. Fixed sql error when searching for negative controls with no descendants of concepts only
4. Fixed sql error when searching for evidence with no descendants included

CemConnector 0.1.0
==================
Release of first version