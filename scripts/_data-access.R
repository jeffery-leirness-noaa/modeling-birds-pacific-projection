# create azure login client to authenticate with Azure Resource Manager
# not sure this is necessary
# AzureRMR::create_azure_login(auth_type = "device_code")
# AzureRMR::list_azure_logins()

# # test out azure login client functionality
# az <- AzureRMR::get_azure_login()
# az$list_subscriptions()
# sub <- az$get_subscription_by_name("nos-nccos-adfssub-6301")
# sub$get_resource_group("nccos-mse-biogeo-seabirds-rg")

# create/get azure active directory (AAD) token
AzureRMR::list_azure_tokens()
token <- AzureRMR::get_azure_token("https://storage.azure.com", tenant = "common",
                                   app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
                                   auth_type = "device_code")

# test out file access using token
endp <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                    token = token)
AzureStor::list_storage_containers(endp)
cont <- AzureStor::storage_container(endp, "processing")
AzureStor::list_storage_files(cont)
dat <- AzureStor::storage_read_csv(cont, "segmented-data.csv")



# explore using shared access signature (SAS) -----------------------------
# user delegation key valid for 24 hours
endp <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                    token = token)
userkey <- AzureStor::get_user_delegation_key(endp, start = Sys.Date(),
                                              expiry = Sys.Date() + 1)

# user delegation SAS for a container
# is the `resource` specified correctly?
usersas <- AzureStor::get_user_delegation_sas(endp, key = userkey,
                                              resource = "processing")

# test file access using SAS
# no success so far with this method!
r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif",
                       usersas, sep = "?"))

# user delegation SAS for a specific file, read/write/create/delete access
# (order of permissions is important!)
usersas <- AzureStor::get_user_delegation_sas(endp, key = userkey,
                                              resource = "processing/chla-fall.tif",
                                              resource_type = "b",
                                              permissions = "r")

# test file access using SAS
# no success so far with this method!
r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif",
                       usersas, sep = "?"))



# hard code container-level SAS obtained via Microsoft Azure Storage Explorer
# this method is successful!
usersas <- "sv=2023-01-03&st=2024-02-21T15%3A59%3A20Z&se=2024-02-22T15%3A59%3A20Z&skoid=8b019ad4-f16e-4b34-a2ee-d46e2ef407f4&sktid=abc10057-d592-4f3c-a901-c5e4c87b50cf&skt=2024-02-21T15%3A59%3A20Z&ske=2024-02-22T15%3A59%3A20Z&sks=b&skv=2023-01-03&sr=c&sp=rl&sig=0zH6idpWHUqw1hRV8UujmVKGDxFCwWt8G%2FhVFaDq7EQ%3D"
r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif",
                       usersas, sep = "?"))
sa <- sf::read_sf(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/study-area.gpkg",
                        usersas, sep = "?"))



# additional explorations using key access on nccoswsdevstor storage
# key <- "vw9J+qa1sijEq6Ai6S62+sT/FkbCNFCm6tgpm/Ffh5bpXQikEifablcsgRmccHggJtl0R0Opy8ps+ASteCAt3A=="
# endp <- AzureStor::storage_endpoint("https://nccoswsdevstor.file.core.windows.net",
#                                     key = key)
# AzureStor::list_storage_containers(endp)
# cont <- AzureStor::storage_container(endp, "code-391ff5ac-6576-460f-ba4d-7e03433c68b6")
# AzureStor::list_storage_files(cont)
#
# AzureStor::list_storage_files(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home")
# temp <- AzureStor::storage_read_delim(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home/.gitignore")
#
# # AzureStor::storage_load_rds()
