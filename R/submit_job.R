
submit_job <- function(code = ".", command, dir_out = NULL, environment = NULL,
                       compute, experiment_name, display_name, description,
                       subscription_id, resource_group_name, workspace_name) {

  # import required libraries
  azure_ai_ml <- reticulate::import("azure.ai.ml")
  azure_ai_ml_entities <- reticulate::import("azure.ai.ml.entities")
  azure_identity <- reticulate::import("azure.identity")

  # get a handle to the workspace
  ml_client <- azure_ai_ml$MLClient(credential = azure_identity$DefaultAzureCredential(),
                                    subscription_id = subscription_id,
                                    resource_group_name = resource_group_name,
                                    workspace_name = workspace_name)

  # configure the command
  if (is.null(dir_out)) {
    outputs <- NULL
  } else {
    outputs <- {"dir_out": azure_ai_ml$Output(type = "uri_folder", path = dir_out, mode = "rw_mount")}
  }
  job <- azure_ai_ml$command(code = code,
                             command = command,
                             outputs = outputs,
                             environment = environment,
                             compute = compute,
                             experiment_name = experiment_name,
                             display_name = display_name,
                             description = description)

  # submit the command
  ml_client$create_or_update(job)

}
