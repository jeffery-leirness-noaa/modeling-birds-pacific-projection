# set testing parameters
code = "./scripts"
# rscript_command = "Rscript test.R"
rscript_command = "Rscript test.R --dir_out=${{outputs.dir_out}}"
# dir_out = None
dir_out = "azureml://datastores/workspaceblobstore/paths/"
# dir_env = "./env"
environment = "azureml://registries/nccos-registry-ml/environments/nccos-leirness-modeling-birds-pacific-projection/versions/2"
compute = "nccos-vm-leirness-e4dsv4"
experiment_name = "r-data-access-testing"
display_name = "r-data-access-testing-exp-1"
description = "Test data access from RStudio on compute instance."

# import required libraries
from azure.ai.ml import MLClient
from azure.ai.ml import command, Input, Output
from azure.ai.ml.entities import Environment, Data, BuildContext
from azure.identity import DefaultAzureCredential

# Enter details of your AML workspace
subscription_id = "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c"
resource_group_name = "nccos-mse-biogeo-seabirds-rg"
workspace_name = "nccos-mse-biogeo-seabird-ml"

# get a handle to the workspace
ml_client = MLClient(
  credential = DefaultAzureCredential(), 
  subscription_id = subscription_id, 
  resource_group_name = resource_group_name, 
  workspace_name = workspace_name
)

# configure the command
if dir_out is None:
  outputs = None
else:
  outputs = {"dir_out": Output(type = "uri_folder", path = dir_out, mode = "rw_mount")}
# if dir_env is None:
#   environment = None
# else:
#   # environment = Environment(build = BuildContext(path = dir_env))
#   environment = "azureml://registries/nccos-registry-ml/environments/nccos-leirness-modeling-birds-pacific-projection/versions/1"
job = command(
  code = code,
  command = rscript_command,
  outputs = outputs,
  environment = environment,
  compute = compute,
  experiment_name = experiment_name,
  display_name = display_name,
  description = description
)

# submit the command
ml_client.create_or_update(job)
