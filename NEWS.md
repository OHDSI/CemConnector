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