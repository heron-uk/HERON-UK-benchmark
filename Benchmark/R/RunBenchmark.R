source("R/functions.R")

iterations <- 1
pkg_name <- "HDRUK-benchmark"
pkg_version <- "2.0.0"

# create log file
outputFolder <- here::here("Results")
if(isTRUE(logSql)){
options(omopgenerics.log_sql_path = here::here("Results", "sql_logs"))
}
if(isTRUE(logSqlExplain)){
options(omopgenerics.log_sql_explain_path = here::here("Results", "sql_explain"))
}

log_file <- file.path(outputFolder, paste0("/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"), ".txt"))
omopgenerics::createLogFile(logFile = log_file)

omopgenerics::logMessage("reading tables in write schema (initial)")
initialTables <- safe_run(quote(omopgenerics::listSourceTables(cdm = cdm)), "listSourceTables (initial)")

# Initialize result variables to NULL so subsequent code can reference them safely
general_benchmark <- omopConstructor_benchmark <- CodelistGenerator_benchmark <-
  cohortConstructor_benchmark <- incidencePrevalence_benchmark <-
  cohortCharacteristics_benchmark <- drugUtilisation_benchmark <- NULL

# Run benchmarks conditionally, safely
if (runGeneralBenchmark) {
  general_benchmark <- safe_run(quote(generalBenchmark(cdm = cdm, iterations = iterations)), task_name = "generalBenchmark")
}

if (runOmopConstructorBenchmark) {
  omopConstructor_benchmark <- safe_run(quote(omopConstructorBenchmark(cdm = cdm, iterations = iterations)), task_name = "omopConstructorBenchmark")
}

if (runCodelistGeneratorBenchmark) {
  CodelistGenerator_benchmark <- safe_run(quote(CodelistGeneratorBenchmark(cdm = cdm, iterations = iterations)), task_name = "CodelistGeneratorBenchmark")
}

if (runCohortConstructorBenchmark) {
  cohortConstructor_benchmark <- safe_run(quote(cohortConstructorBenchmark(cdm = cdm, iterations = iterations)), task_name = "cohortConstructorBenchmark")
}

if (runIncidencePrevalenceBenchmark) {
  incidencePrevalence_benchmark <- safe_run(quote(incidencePrevalenceBenchmark(cdm = cdm, iterations = iterations)), task_name = "incidencePrevalenceBenchmark")
}

if (runCohortCharacteristicsBenchmark) {
  cohortCharacteristics_benchmark <- safe_run(quote(cohortCharacteristicsBenchmark(cdm = cdm, iterations = iterations)), task_name = "cohortCharacteristicsBenchmark")
}

if (runDrugUtilisationBenchmark) {
  drugUtilisation_benchmark <- safe_run(quote(drugUtilisationBenchmark(cdm = cdm, iterations = iterations)), task_name = "drugUtilisationBenchmark")
}

# export results
omopgenerics::logMessage("Export results")

omopgenerics::exportSummarisedResult(
  general_benchmark,
  omopConstructor_benchmark,
  CodelistGenerator_benchmark,
  cohortConstructor_benchmark,
  incidencePrevalence_benchmark,
  cohortCharacteristics_benchmark,
  drugUtilisation_benchmark,
  minCellCount = minCellCount,
  path = outputFolder,
  fileName = "result_benchmark_{cdm_name}_{date}.csv",
  logSqlPath = NULL,
  logExplainPath = NULL
)

# reading tables in write schema
omopgenerics::logMessage("reading tables in write schema (final)")
finalTables <- omopgenerics::listSourceTables(cdm = cdm)
createdTables <- finalTables[!finalTables %in% initialTables]
if (length(createdTables) > 0) {
  createdTables <- paste0(createdTables, collapse = ", ")
  mes <- "the following tables where created in the write schema: {createdTables}" |>
    glue::glue()
  omopgenerics::logMessage(mes)
}

# Close connection
omopgenerics::logMessage("closing connection")
CDMConnector::cdmDisconnect(cdm)
