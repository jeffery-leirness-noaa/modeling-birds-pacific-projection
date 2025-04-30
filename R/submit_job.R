reticulate::py_require(
  c("azure.ai.ml", "azure.identity"),
  python_version = "3.13.2",
  exclude_newer = "2025-03-25",
  action = "set"
)

#' Submit a Job to Azure Machine Learning
#'
#' This function submits a command job to Azure Machine Learning.
#'
#' @param code Character string. Path to the code directory. Default is ".".
#' @param command Character string. The command to run.
#' @param inputs List or NULL. Input data for the job.
#' @param outputs List or NULL. Output data configuration for the job.
#' @param environment Character string or NULL. The environment to use.
#' @param compute Character string. The compute target to use.
#' @param experiment_name Character string. The name of the experiment.
#' @param display_name Character string. The display name for the job.
#' @param description Character string. Description of the job.
#' @param subscription_id Character string or NULL. Azure subscription ID.
#' @param resource_group Character string or NULL. Azure resource group.
#' @param workspace_name Character string or NULL. Azure ML workspace name.
#'
#' @return The submitted job object.
#'
#' @examples
#' # Submit a job to run an R script
#' job <- submit_job(
#'   command = "Rscript process_data.R --input ${{inputs.data}} --output ${{outputs.results}}",
#'   inputs = list(data = "azureml://datastores/mydata/paths/input"),
#'   outputs = list(results = "azureml://datastores/mydata/paths/output"),
#'   compute = "cpu-cluster",
#'   experiment_name = "data-processing",
#'   display_name = "Process 2023 Data",
#'   description = "Process environmental data for 2023"
#' )
submit_job <- function(code = ".", command, inputs = NULL, outputs = NULL,
                       environment = NULL, compute, experiment_name,
                       display_name, description, subscription_id = NULL,
                       resource_group = NULL, workspace_name = NULL) {

  # import required libraries
  azure_ai_ml <- reticulate::import("azure.ai.ml")
  azure_ai_ml_entities <- reticulate::import("azure.ai.ml.entities")
  azure_identity <- reticulate::import("azure.identity")

  # get azure machine learning configuration specs
  if (any(is.null(subscription_id), is.null(resource_group), is.null(workspace_name))) {
    aml_config_path <- "/config.json"
    if (fs::file_exists(aml_config_path)) {
      aml_config <- rjson::fromJSON(file = aml_config_path)
    }
    if (is.null(subscription_id)) {
      if (Sys.getenv("AML_SUBSCRIPTION_ID") != "") {
        subscription_id <- Sys.getenv("AML_SUBSCRIPTION_ID")
      } else if (exists("aml_config")) {
        subscription_id <- aml_config$subscription_id
      }
    }
    if (is.null(resource_group)) {
      if (Sys.getenv("AML_RESOURCE_GROUP") != "") {
        resource_group <- Sys.getenv("AML_RESOURCE_GROUP")
      } else if (exists("aml_config")) {
        resource_group <- aml_config$resource_group
      }
    }
    if (is.null(workspace_name)) {
      if (Sys.getenv("AML_WORKSPACE_NAME") != "") {
        workspace_name <- Sys.getenv("AML_WORKSPACE_NAME")
      } else if (exists("aml_config")) {
        workspace_name <- aml_config$workspace_name
      }
    }
  }

  # get a handle to the workspace
  ml_client <- azure_ai_ml$MLClient(credential = azure_identity$DefaultAzureCredential(),
                                    subscription_id = subscription_id,
                                    resource_group_name = resource_group,
                                    workspace_name = workspace_name)

  # configure the command
  if (!is.null(inputs)) {
    inputs <- purrr::map(inputs, .f = \(x) azure_ai_ml$Input(type = "uri_folder", path = x, mode = "ro_mount")) |>
      reticulate::dict()
  }
  if (!is.null(outputs)) {
    outputs <- purrr::map(outputs, .f = \(x) azure_ai_ml$Output(type = "uri_folder", path = x, mode = "rw_mount")) |>
      reticulate::dict()
  }
  job <- azure_ai_ml$command(code = code,
                             command = command,
                             inputs = inputs,
                             outputs = outputs,
                             environment = environment,
                             compute = compute,
                             experiment_name = experiment_name,
                             display_name = display_name,
                             description = description)

  # submit the command
  ml_client$create_or_update(job)

}

#' Submit an R File Job to Azure Machine Learning
#'
#' This function submits a job to run an R script file on Azure Machine Learning,
#' with automatic handling of input and output arguments.
#'
#' @param rfile Character string. The R script file to run.
#' @param additional_args Character string or NULL. Additional command line arguments.
#' @param inputs List or NULL. Input data for the job.
#' @param outputs List or NULL. Output data configuration for the job.
#' @param environment Character string or NULL. The environment to use.
#' @param compute Character string. The compute target to use.
#' @param experiment_name Character string. The name of the experiment.
#' @param display_name Character string. The display name for the job.
#' @param description Character string. Description of the job.
#' @param subscription_id Character string or NULL. Azure subscription ID.
#' @param resource_group Character string or NULL. Azure resource group.
#' @param workspace_name Character string or NULL. Azure ML workspace name.
#'
#' @return The submitted job object.
#'
#' @examples
#' # Submit an R script job
#' job <- submit_job_rfile(
#'   rfile = "process_environmental_data.R",
#'   additional_args = "--verbose",
#'   inputs = list(data = "azureml://datastores/mydata/paths/input"),
#'   outputs = list(results = "azureml://datastores/mydata/paths/output"),
#'   compute = "cpu-cluster",
#'   experiment_name = "env-data-processing",
#'   display_name = "Process 2023 Environmental Data",
#'   description = "Process daily environmental data for 2023"
#' )
submit_job_rfile <- function(rfile, additional_args = NULL,
                             inputs = NULL, outputs = NULL,
                             environment = NULL, compute, experiment_name,
                             display_name, description, subscription_id = NULL,
                             resource_group = NULL, workspace_name = NULL) {
  if (!is.null(inputs)) {
    arg_inputs <- purrr::map_chr(names(inputs),
                                 .f = \(x) paste0("--", x, "=${{inputs.", x,
                                                  "}}")) |>
      stringr::str_flatten(collapse = " ")

  } else {
    arg_inputs <- ""
  }
  if (!is.null(outputs)) {
    arg_outputs <- purrr::map_chr(names(outputs),
                                 .f = \(x) paste0("--", x, "=${{outputs.", x,
                                                  "}}")) |>
      stringr::str_flatten(collapse = " ")

  } else {
    arg_outputs <- ""
  }
  command <- paste("Rscript", rfile, arg_inputs, arg_outputs, additional_args) |>
    trimws()
  submit_job(command = command,
             inputs = inputs,
             outputs = outputs,
             environment = environment,
             compute = compute,
             experiment_name = experiment_name,
             display_name = display_name,
             description = description,
             subscription_id = subscription_id,
             resource_group = resource_group,
             workspace_name = workspace_name)
}
