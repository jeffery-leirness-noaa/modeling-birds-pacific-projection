
key <- "vw9J+qa1sijEq6Ai6S62+sT/FkbCNFCm6tgpm/Ffh5bpXQikEifablcsgRmccHggJtl0R0Opy8ps+ASteCAt3A=="

# AzureAuth::get_azure_token()
# 
# bl <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net", key = key)
# fl <- AzureStor::storage_endpoint("https://nccosseabirdmlstor.file.core.windows.net", key = key)

fl <- AzureStor::storage_endpoint("https://nccoswsdevstor.file.core.windows.net", key = key)
AzureStor::list_storage_containers(fl)
cont <- AzureStor::storage_container(fl, "code-391ff5ac-6576-460f-ba4d-7e03433c68b6")
AzureStor::list_storage_files(cont)

AzureStor::list_storage_files(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home")
temp <- AzureStor::storage_read_delim(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home/.gitignore")

# AzureStor::storage_load_rds()
