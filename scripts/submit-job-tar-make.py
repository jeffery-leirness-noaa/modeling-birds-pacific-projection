# set parameters
code = "."
rscript_command = "Rscript tar_make.R --dir_in=${{inputs.dir_in}} --dir_out=${{outputs.dir_out}}"
dir_in = "azureml://datastores/datastor_raw/paths/"
dir_out = "azureml://datastores/datastor_processing/paths/"
environment = "azureml://registries/nccos-registry-ml/environments/nccos-leirness-modeling-birds-pacific-projection/versions/4"
# compute = "nccos-vm-leirness-e4dsv4"
compute = "nccos-vm-cluster-ds2"
experiment_name = "tar-make"
display_name = "tar-make-run-1"
description = "Run target pipeline (see .Renviron for specified targets project)."

# import required libraries
from azure.ai.ml import MLClient
from azure.ai.ml import command, Input, Output
from azure.ai.ml.entities import Environment, Data, BuildContext
from azure.identity import DefaultAzureCredential

# get a handle to the workspace
ml_client = MLClient.from_config(credential = DefaultAzureCredential())

# configure the command
if dir_in is None:
  inputs = None
else:
  inputs = {"dir_in": Input(type = "uri_folder", path = dir_in, mode = "ro_mount")}
if dir_out is None:
  outputs = None
else:
  outputs = {"dir_out": Output(type = "uri_folder", path = dir_out, mode = "rw_mount")}
job = command(
  code = code,
  command = rscript_command,
  inputs = inputs,
  outputs = outputs,
  environment = environment,
  compute = compute,
  experiment_name = experiment_name,
  display_name = display_name,
  description = description
)

# submit the command
ml_client.create_or_update(job)
