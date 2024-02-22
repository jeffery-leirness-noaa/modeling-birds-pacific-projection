
# import mltable
# from azure.ai.ml import MLClient
# from azure.identity import DefaultAzureCredential
# 
# ml_client = MLClient.from_config(credential=DefaultAzureCredential())
# data_asset = ml_client.data.get("dummy_test", version="1")
# 
# path = {
#   'folder': data_asset.path
# }
# 
# tbl = mltable.from_delimited_files(paths=[path])
# df = tbl.to_pandas_dataframe()
# df



# mount to compute target
from azureml.core import Workspace, Dataset, Datastore

subscription_id = '737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c'
resource_group = 'nccos-mse-biogeo-seabirds-rg'
workspace_name = 'nccos-mse-biogeo-seabird-ml'

workspace = Workspace(subscription_id, resource_group, workspace_name)

datastore = Datastore.get(workspace, "datastor_processing")
dataset = Dataset.File.from_files(path=(datastore, ''))
mounted_path = dataset.mount()
