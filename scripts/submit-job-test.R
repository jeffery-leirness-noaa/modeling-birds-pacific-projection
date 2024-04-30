# set testing parameters
code <- "./scripts"
rscript_command <- "Rscript test.R --dir_out=${{outputs.dir_out}}"
dir_out <- "azureml://datastores/datastor_processing/paths"
dir_env <- "./env"
compute <- "nccos-vm-leirness-e4dsv4"
experiment_name <- "r-data-access-testing"
display_name <- "r-data-access-testing-exp-1"
description <- "Test data access from RStudio on compute instance."

# import required libraries
azure_ai_ml <- reticulate::import("azure.ai.ml")
azure_ai_ml_entities <- reticulate::import("azure.ai.ml.entities")
azure_identity <- reticulate::import("azure.identity")

# Enter details of your AML workspace
subscription_id <- "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c"
resource_group <- "nccos-mse-biogeo-seabirds-rg"
workspace <- "nccos-mse-biogeo-seabird-ml"

# get a handle to the workspace
ml_client <- azure_ai_ml$MLClient(credential = azure_identity$DefaultAzureCredential(),
                                  subscription_id = subscription_id,
                                  resource_group_name = resource_group,
                                  workspace_name = workspace)

# configure the command
if (is.null(dir_out)) {
  outputs <- NULL
} else {
  outputs <- {"dir_out": azure_ai_ml$Output(type = "uri_folder", path = dir_out, mode = "rw_mount")}
}
if (is.null(dir_env)) {
  environment <- NULL
} else {
  # environment <- azure_ai_ml_entities$Environment(build = azure_ai_ml_entities$BuildContext(path = dir_env))
  environment <- "azureml://registries/nccos-registry-ml/environments/nccos-leirness-modeling-birds-pacific-projection/versions/1"
}
job <- azure_ai_ml$command(code = code,
                           command = rscript_command,
                           outputs = outputs,
                           environment = environment,
                           compute = compute,
                           experiment_name = experiment_name,
                           display_name = display_name,
                           description = description)

# submit the command
# ml_client$create_or_update(job)
source("R/submit_job.R")
submit_job(code = code,
           command = rscript_command,
           dir_out = dir_out,
           environment = environment,
           compute = compute,
           experiment_name = experiment_name,
           display_name = display_name,
           description = description,
           subscription_id = subscription_id,
           resource_group_name = resource_group,
           workspace_name = workspace)
