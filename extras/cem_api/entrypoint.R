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

# To point at a live cem database instance:
# 1. Set envrionment variables in extra/DOCKER_ENV_FILE
# 2. build contianer docker build -t cemconnector .
# 3. Run docker run --rm -p8080:8080 --env-file extra/ENV_FILE cemconnector

dbms <- Sys.getenv("CEM_DATABASE_DBMS")
driverExists <- grepl(tolower(dbms), tolower(list.files(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))))

if (!any(driverExists))
  DatabaseConnector::downloadJdbcDrivers(dbms = dbms)

connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("CEM_DATABASE_SERVER"),
                                                                user = Sys.getenv("CEM_DATABASE_USER"),
                                                                password = Sys.getenv("CEM_DATABASE_PASSWORD"),
                                                                port = Sys.getenv("CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("CEM_DATABASE_EXTRA_SETTINGS"))

cemDatabaseSchema <- Sys.getenv("CEM_DATABASE_SCHEMA")
vocabularyDatabaseSchema <- Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA")
sourceDatabaseSchema <- Sys.getenv("CEM_DATABASE_INFO_SCHEMA")

api <- CemConnector::loadApi(connectionDetails,
                             cemDatabaseSchema = cemDatabaseSchema,
                             vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                             sourceDatabaseSchema = sourceDatabaseSchema)
