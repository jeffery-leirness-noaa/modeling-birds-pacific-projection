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
