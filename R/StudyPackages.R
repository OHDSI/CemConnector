# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CemConnector
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Add controls to a SkeletonComparativeEffectStudy package
#' @description
#' See https:://github.com/ohdsi/SkeletonComparativeEffectStudy for details on creating a study
#' This function adds a set of negative controls to the study package to be automatically instantiated as cohorts.
#' Id's are determined by the study package, normally this is the same as the concept id
#' @param controlConcepts data.frame with at least 1 row (recommended > 10) containing conceptId and conceptName
#' @param targetId target cohort Id controls are for
#' @param comparatorId comparator cohort Id controls are for (optional in self controlled studies)
#' @param fileName path of existing negative control file or where file will be written
#' @param type "outcome" or "exposure" - only outcomes are currently supported by the study package
#' @importFrom utils read.csv write.csv
#' @export
addControlsToStudyPackage <- function(controlConcepts,
                                      targetId,
                                      comparatorId = NULL,
                                      fileName = "inst/settings/NegativeControls.csv",
                                      type = "outcome") {
  checkmate::assert_data_frame(controlConcepts, min.rows = 1)
  checkmate::assert_names(names(controlConcepts), must.include = c("conceptId", "conceptName"))
  checkmate::assert_numeric(targetId)
  checkmate::assert_numeric(comparatorId, null.ok = TRUE)
  checkmate::assert_string(fileName)

  if (nrow(controlConcepts) < 10) {
    warning("Small number of negative control concepts selected")
  }

  outputData <- data.frame(targetId = targetId,
                           comparatorId = comparatorId,
                           outcomeId = controlConcepts$conceptId,
                           outcomeName = controlConcepts$conceptName,
                           type = type)

  if (file.exists(fileName)) {
    # Read existing controls
    existingData <- utils::read.csv(fileName)

    if (nrow(existingData)) {
      message("Appending to existing controls")
      checkmate::assertNames(names(existingData),
                             must.include = c("targetId",
                                              "comparatorId",
                                              "outcomeId",
                                              "outcomeName",
                                              "type"))

      for (name in names(existingData)) {
        if (!(name %in% names(outputData))) {
          outputData[,name] <- ''
        }
      }
      outputData <- base::rbind(existingData, outputData)
    }
  }

  utils::write.csv(x = unique(outputData), file = fileName, row.names = FALSE, quote = FALSE)
  invisible(NULL)
}