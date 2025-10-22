new_rows <- function(res, task_name, time, iteration) {
  res <- dplyr::bind_rows(res, tibble::tibble(
    group_level = task_name,
    estimate_value = as.character(time$toc - time$tic),
    estimate_name = "time_seconds",
    strata_level = as.character(iteration)
  ) |>
    tibble::add_row(
      group_level = task_name,
      estimate_value = as.character(round((time$toc - time$tic) / 60, 1)),
      estimate_name = "time_minutes",
      strata_level = as.character(iteration)
    ))
  return(res)
}
check_int64 <- function(druglist){
if (any(purrr::map_lgl(druglist, inherits, "integer64"))) {
  druglist <- purrr::map(druglist, as.integer)
}
  return(druglist)
}
generalBenchmark <- function(cdm, iterations) {

  res <- tibble::tibble()

  for (i in 1:iterations) {
    mes <- glue::glue("general benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    # Benchmarks relating to vocab tables (which should be similar size for dps)
    task_name <- "Collect concept table into memory"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    collected_concept <- cdm[["concept"]] |>
      dplyr::select("concept_id", "domain_id", "vocabulary_id") |> # to avoid problems with non utf8
      dplyr::collect()
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Insert concept table into database"
    cli::cli_inform("Running task: {task_name}")
    tmpName <- omopgenerics::uniqueTableName()
    tictoc::tic()
    cdm <- omopgenerics::insertTable(cdm = cdm, name = tmpName, table = collected_concept)
    collected_concept <- cdm[["concept"]] |>
      dplyr::collect()
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
    omopgenerics::dropSourceTable(cdm, tmpName)

    task_name <- "Compute concept table to temp table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm[["concept"]] |>
      dplyr::compute()
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Compute concept table to permanent table"
    cli::cli_inform("Running task: {task_name}")
    tmpName <- omopgenerics::uniqueTableName()
    tictoc::tic()
    cdm[[tmpName]] <- cdm[["concept"]] |>
      dplyr::compute(temporary = FALSE, name = tmpName)
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
    omopgenerics::dropSourceTable(cdm, tmpName)

    task_name <- "Count of concept relationship table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm[["concept_relationship"]] |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Count of different relationship IDs in concept relationship table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm[["concept_relationship"]] |>
      dplyr::group_by(.data$relationship_id) |>
      dplyr::tally() |>
      dplyr::collect()
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Distinct count of concept relationship table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm[["concept_relationship"]] |>
      dplyr::distinct() |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # Benchmarks relating to clinical records (which will be different size for dps)
    task_name <- "Count condition occurrence rows"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm$condition_occurrence |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Count individuals in condition occurrence table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm$condition_occurrence |>
      dplyr::group_by(person_id) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Count individuals in person but not in condition occurrence table"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm$person |>
      dplyr::left_join(
        cdm$condition_occurrence |>
          dplyr::group_by(person_id) |>
          dplyr::tally() |>
          dplyr::ungroup(),
        by = "person_id"
      ) |>
      dplyr::filter(is.na(n)) |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Compute person table to write schema"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm$person_ws <- cdm$person |>
      dplyr::compute(
        name = "person_ws",
        temporary = FALSE
      )|>
      suppressMessages()
    cdm$person_ws |>
      dplyr::tally() |>
      dplyr::pull("n")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Count individuals in person (in write schema) but not in condition occurrence table"
    tictoc::tic()
    cdm$person_ws |>
      dplyr::left_join(
        cdm$condition_occurrence |>
          dplyr::group_by(person_id) |>
          dplyr::tally() |>
          dplyr::ungroup(),
        by = "person_id"
      ) |>
      dplyr::filter(is.na(n)) |>
      dplyr::tally() |>
      dplyr::pull("n") |>
      suppressMessages()
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Get summary of cdm"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    snap <- summary(cdm)
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    # 14) Drop tables created
    task_name <- "Drop tables created"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm <- CDMConnector::dropSourceTable(
      cdm = cdm,
      name = dplyr::starts_with(c( "person_ws"
      ))
    )
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

  }

  omopgenerics::logMessage("Compile results for general benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::numberSubjects(cdm$person)
    ) |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "task",
      strata_name = "iteration",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric"
    ) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n"))

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_general_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)

  return(res)
}

incidencePrevalenceBenchmark <- function(cdm, iterations) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("IncidencePrevalence benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    x <- IncidencePrevalence::benchmarkIncidencePrevalence(cdm, analysisType = "all") |>
      suppressMessages()
    x <- x |>
      dplyr::mutate(
        strata_name = "iteration",
        strata_level = as.character(i),
        estimate_name = "time_minutes"
      )
    x <- dplyr::bind_rows(
      x,
      x |>
        dplyr::filter(estimate_name == "time_minutes") |>
        dplyr::mutate(
          estimate_name = "time_seconds",
          estimate_value = as.character(as.numeric(estimate_value) * 60)
        )
    )
    res <- dplyr::bind_rows(res, x)
  }

  omopgenerics::logMessage("Compile results for IncidencePrevalence benchmark")

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_incidence_prevalence_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}


cohortCharacteristicsBenchmark <- function(cdm, iterations) {
  res <- omopgenerics::emptySummarisedResult()

  for (i in 1:iterations) {
    mes <- glue::glue("CohortCharacteristics benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    cdm$my_cohort <- CohortConstructor::demographicsCohort(cdm, "my_cohort")

    x <- CohortCharacteristics::benchmarkCohortCharacteristics(cdm$my_cohort) |>
      suppressMessages()
    x <- x |>
    dplyr::mutate(
      strata_name = "iteration",
      strata_level = as.character(i),
      estimate_name = "time_seconds"
    )

    omopgenerics::dropSourceTable(cdm, "my_cohort")

  res <- dplyr::bind_rows(res, x)
  }

  omopgenerics::logMessage("Compile results for CohortCharacteristics benchmark")

  res <- dplyr::bind_rows(
    res,
    res |>
      dplyr::filter(estimate_name == "time_seconds") |>
      dplyr::mutate(
        estimate_name = "time_minutes",
        estimate_value = as.character(as.numeric(estimate_value) / 60)
      )
  ) |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::settings(x)$person_n
    ) |>
    dplyr::select(!c(additional_name, additional_level)) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n"))
  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_cohort_characteristics_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}



cohortConstructorBenchmark <- function(cdm, iterations) {

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("CohortConstructor benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)
    res <- dplyr::bind_rows(res, CohortConstructor::benchmarkCohortConstructor(cdm, runCIRCE = FALSE)  |>
                              suppressMessages() |>
                              omopgenerics::filterSettings(result_type %in% c("instanciation_time", "cohort_count")) |>
                              dplyr::mutate(iteration = as.character(i)))
  }
  omopgenerics::logMessage("Compile results for CohortConstructor benchmark")

  subj <- res |>
    omopgenerics::addSettings(settingsColumn = "result_type") |>
    dplyr::filter(.data$result_type == "cohort_count") |>
    omopgenerics::tidy() |>
    dplyr::filter(.data$variable_name == "number_subjects") |>
    dplyr::mutate(cohort_name = stringr::str_remove(.data$cohort_name, "^cc_")) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "person_n" = "count")


  inst <- res |>
    omopgenerics::addSettings(settingsColumn = "result_type") |>
    dplyr::filter(.data$result_type == "instanciation_time") |>
    omopgenerics::tidy() |>
    dplyr::mutate(
      cohort_name = dplyr::case_when(
        .data$variable_name == "Cohort set"                                     ~ paste0("Cohort set: ", .data$variable_level),
        .data$variable_name == "Acquired neutropenia or unspecified leukopenia" ~ "neutropenia_leukopenia",
        .data$variable_name == "Asthma without COPD"                            ~ "asthma_no_copd",
        .data$variable_name == "COVID-19"                                       ~ "covid",
        .data$variable_name == "COVID-19: female"                               ~ "covid_female",
        .data$variable_name == "COVID-19: female, 0 to 50"                      ~ "covid_female_0_to_50",
        .data$variable_name == "COVID-19: female, 51 to 150"                    ~ "covid_female_51_to_150",
        .data$variable_name == "COVID-19: male"                                 ~ "covid_male",
        .data$variable_name == "COVID-19: male, 0 to 50"                        ~ "covid_male_0_to_50",
        .data$variable_name == "COVID-19: male, 51 to 150"                      ~ "covid_male_51_to_150",
        .data$variable_name == "Endometriosis procedure"                        ~ "endometriosis_procedure",
        .data$variable_name == "Inpatient hospitalisation"                      ~ "hospitalisation",
        .data$variable_name == "Major non cardiac surgery"                      ~ "major_non_cardiac_surgery",
        .data$variable_name == "New fluoroquinolone users"                      ~ "new_fluoroquinolone",
        .data$variable_name == "New users of beta blockers nested in essential hypertension"
        ~ "beta_blockers_hypertension",
        .data$variable_name == "Transverse myelitis"                            ~ "transverse_myelitis",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "time_minutes" = "time")


  out <- inst |>
    dplyr::left_join(subj, by = c("cdm_name", "iteration", "cohort_name")) |>
    dplyr::select("cdm_name", "iteration", "cohort_name",
                  "person_n", "time_minutes")


  out <- out |>
    dplyr::mutate(time_seconds = as.numeric(.data$time_minutes * 60L),
                  dbms = omopgenerics::sourceType(cdm)
                  ) |>
    tidyr::pivot_longer(
      cols = c(time_seconds, time_minutes),
      names_to = "estimate_name",
      values_to = "estimate_value"
      ) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n")) |>
    omopgenerics::uniteStrata(cols = "iteration") |>
    dplyr::rename("group_level" = "cohort_name") |>
    dplyr::mutate(
      result_id = 1L,
      group_name = "task",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric",
      estimate_value = sprintf("%.3f", .data$estimate_value))


  settings <- dplyr::tibble(
    result_id = 1L,
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_cohort_constructor_benchmark"
  )
  out <- out |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(out)
}




drugUtilisationBenchmark <- function(cdm, iterations) {

  res <- list()

  for (i in 1:iterations) {
    mes <- glue::glue("DrugUtilisation benchmark iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)
    res <- dplyr::bind_rows(res, DrugUtilisation::benchmarkDrugUtilisation(cdm) |>
                              dplyr::mutate(strata_level = as.character(i)) |>
                              suppressMessages())
  }
  omopgenerics::logMessage("Compile results for DrugUtilisation benchmark")



  res <- res |>
    omopgenerics::pivotEstimates() |>
    dplyr::mutate(time_minutes = as.numeric(.data$time_seconds / 60L),
                  dbms = omopgenerics::sourceType(cdm),
                  person_n = omopgenerics::numberSubjects(cdm$person),
                  "strata_name" = "iteration") |>
    tidyr::pivot_longer(
      cols = c(time_seconds, time_minutes),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    omopgenerics::splitAdditional() |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n")) |>
    dplyr::mutate(
      estimate_type = "numeric",
      estimate_value = sprintf("%.2f", .data$estimate_value))


  settings <- dplyr::tibble(
    result_id = 1L,
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_drug_utilisation_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)
  return(res)
}


omopConstructorBenchmark <- function(cdm, iterations) {
  res <- tibble::tibble()
  for (i in 1:iterations) {
    mes <- glue::glue("OmopConstructor iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    task_name <- "OP: first record to data extraction"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm <- OmopConstructor::buildObservationPeriod(
      cdm,
      collapseDays = Inf,
      persistenceDays = Inf

    )
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "OP: Inpatient"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm <- OmopConstructor::buildObservationPeriod(
      cdm = cdm,
      collapseDays = 1,
      persistenceDays = 0,
      recordsFrom = "visit_occurrence"
    )
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "OP: Collapse+Persistence 365 - Age<60"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    cdm <- OmopConstructor::buildObservationPeriod(
      cdm = cdm,
      collapseDays = 365,
      persistenceDays = 364,
      censorAge = 60
    )
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)
  }

  omopgenerics::logMessage("Compile results for OmopConstructor benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::numberSubjects(cdm$person)
    ) |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "task",
      strata_name = "iteration",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric"
    ) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n"))

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_omop_constructor_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)

  omopgenerics::dropSourceTable(cdm = cdm, "observation_period")

  return(res)
}

CodelistGeneratorBenchmark <- function(cdm, iterations) {
  res <- tibble::tibble()
  for (i in 1:iterations) {
    mes <- glue::glue("CodelistGenerator iteration {i}/{iterations}")
    omopgenerics::logMessage(mes)

    task_name <- "Get codelist for warfarin"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    warfarin <- CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm, name = "warfarin")
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

    task_name <- "Get codelist for every drug ingredient"
    cli::cli_inform("Running task: {task_name}")
    tictoc::tic()
    druglist <- CodelistGenerator::getDrugIngredientCodes(
      cdm = cdm, name = NULL)
    t <- tictoc::toc()
    res <- new_rows(res, task_name = task_name, time = t, iteration = i)

  }

  omopgenerics::logMessage("Compile results for CodelistGenerator benchmark")

  res <- res |>
    dplyr::mutate(
      dbms = omopgenerics::sourceType(cdm),
      person_n = omopgenerics::numberSubjects(cdm$person)
    ) |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "task",
      strata_name = "iteration",
      variable_name = "overall",
      variable_level = "overall",
      estimate_type = "numeric"
    ) |>
    omopgenerics::uniteAdditional(cols = c("dbms", "person_n"))

  settings <- dplyr::tibble(
    result_id = unique(res$result_id),
    package_name = pkg_name,
    package_version = pkg_version,
    result_type = "summarise_codelist_generator_benchmark"
  )
  res <- res |>
    omopgenerics::newSummarisedResult(settings = settings)

  return(res)
}

safe_run <- function(expr, task_name = "task") {
  tryCatch(
    {
      omopgenerics::logMessage(glue::glue("START: {task_name}"))
      res <- eval(expr)
      omopgenerics::logMessage(glue::glue("SUCCESS: {task_name}"))
      return(res)
    },
    error = function(e) {
      # Log error message and, if available, the traceback (as text)
      msg <- paste0("ERROR in ", task_name, ": ", conditionMessage(e))
      omopgenerics::logMessage(msg)

      # capture and log a simple traceback if available
      tb <- try(utils::capture.output(traceback()), silent = TRUE)
      if (!inherits(tb, "try-error") && length(tb) > 0) {
        omopgenerics::logMessage(paste0("TRACEBACK for ", task_name, ":\n", paste(tb, collapse = "\n")))
      }
      return(omopgenerics::emptySummarisedResult())
    }
  )
}
