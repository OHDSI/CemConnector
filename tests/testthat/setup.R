connectionDetails <- DatabaseConnector::createConnectionDetails(server = "ohda-prod-1.cldcoxyrkflo.us-east-1.redshift.amazonaws.com/cem",
                                                                user = "jgilber2",
                                                                password = keyring::key_get("ohda-prod-1", username = "jgilber2"),
                                                                port = 5439,
                                                                dbms = "redshift",
                                                                extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

cemTestSchema <- "cem_v3_0_0_20210308_evidence"
vocabularySchema <- "cem_v3_0_0_20210308_staging_vocabulary"
sourceInfoSchema <- "cem_v3_0_0_20210308_translated"