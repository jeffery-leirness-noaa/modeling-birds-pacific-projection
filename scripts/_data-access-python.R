# mount to compute target -------------------------------------------------
azureml_core <- reticulate::import("azureml.core")

subscription_id <- "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c"
resource_group <- "nccos-mse-biogeo-seabirds-rg"
workspace_name <- "nccos-mse-biogeo-seabird-ml"

workspace <- azureml_core$Workspace(subscription_id, resource_group, workspace_name)

datastore <- azureml_core$Datastore$get(workspace, "datastor_processing")
dataset <- azureml_core$Dataset$File$from_files(path = reticulate::tuple(datastore, ""))
mounted_path <- dataset$mount()



# test job submission -----------------------------------------------------

# import required libraries
azure_ai_ml <- reticulate::import("azure.ai.ml")
azure_ai_ml_entities <- reticulate::import("azure.ai.ml.entities")
azure_identity <- reticulate::import("azure.identity")

# enter details of your AML workspace
subscription_id <- "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c"
resource_group <- "nccos-mse-biogeo-seabirds-rg"
workspace <- "nccos-mse-biogeo-seabird-ml"

# get a handle to the workspace
ml_client <- azure_ai_ml$MLClient(azure_identity$DefaultAzureCredential(),
                                  subscription_id, resource_group, workspace)

# configure the command
# custom_path = "azureml://datastores/datastor_processing/paths/output/"
job1 = azure_ai_ml$command(
  code="./scripts",
  command='Rscript test.R',
  # command='Rscript test.R --dir_out=${{outputs.dir_out}}',
  # outputs={"dir_out": azure_ai_ml$Output(type="uri_folder", path=custom_path, mode="rw_mount")},
  environment=azure_ai_ml_entities$Environment(build=azure_ai_ml_entities$BuildContext(path="./env")),
  compute="nccos-vm-leirness-e4dsv4",
  display_name="r-data-access-testing",
  experiment_name="r-data-access-testing-exp-1",
  description="Test data access from RStudio on compute instance.",
)

# submit the command
returned_job1 <- ml_client$create_or_update(job1)
