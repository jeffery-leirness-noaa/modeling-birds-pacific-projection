
submit_job <- function(code = ".", command, dir_in = NULL, dir_out = NULL,
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
  if (is.null(dir_in)) {
    inputs <- NULL
  } else {
    inputs <- reticulate::dict("dir_in" = azure_ai_ml$Input(type = "uri_folder", path = dir_in, mode = "ro_mount"))
  }
  if (is.null(dir_out)) {
    outputs <- NULL
  } else {
    outputs <- reticulate::dict("dir_out" = azure_ai_ml$Output(type = "uri_folder", path = dir_out, mode = "rw_mount"))
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
                             dir_in = NULL, dir_out = NULL,
                             environment = NULL, compute, experiment_name,
                             display_name, description, subscription_id = NULL,
                             resource_group = NULL, workspace_name = NULL) {
  arg_dir_in <- ifelse(!is.null(dir_in), "--dir_in=${{inputs.dir_in}}", "")
  arg_dir_out <- ifelse(!is.null(dir_out), "--dir_out=${{outputs.dir_out}}", "")
  command <- paste("Rscript", rfile, arg_dir_in, arg_dir_out, additional_args) |>
    trimws()
  submit_job(command = command,
             dir_in = dir_in,
             dir_out = dir_out,
             environment = environment,
             compute = compute,
             experiment_name = experiment_name,
             display_name = display_name,
             description = description,
             subscription_id = subscription_id,
             resource_group = resource_group,
             workspace_name = workspace_name)
}
