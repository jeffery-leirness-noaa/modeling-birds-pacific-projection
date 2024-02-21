
# create azure login client to authenticate with Azure Resource Manager
AzureRMR::create_azure_login(auth_type = "device_code")

# test out azure login client functionality
az <- AzureRMR::get_azure_login()
az$list_subscriptions()
sub <- az$get_subscription_by_name("nos-nccos-adfssub-6301")
sub$get_resource_group("nccos-mse-biogeo-seabirds-rg")

AzureRMR::list_azure_logins()
AzureRMR::list_azure_tokens()
AzureRMR::is_azure_token(AzureRMR::list_azure_tokens()[[2]])
token <- AzureRMR::get_azure_token("https://storage.azure.com", tenant = "common",
                                   app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
                                   auth_type = "device_code")

fl <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net", token = token)
AzureStor::list_storage_containers(fl)
cont <- AzureStor::storage_container(fl, "processing")
AzureStor::list_storage_files(cont)
dat <- AzureStor::storage_read_csv(cont, "segmented-data.csv") |>
  tibble::as_tibble()


AzureStor::get_user_delegation_key()
AzureStor::get_user_delegation_sas()

# user delegation key valid for 24 hours
endp <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net", token = token)
userkey <- AzureStor::get_user_delegation_key(endp, start = Sys.Date(), expiry = Sys.Date() + 1)

# user delegation SAS for a container
usersas <- AzureStor::get_user_delegation_sas(endp, userkey, resource = "processing")

r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif", usersas, sep = "?"))

# user delegation SAS for a specific file, read/write/create/delete access
# (order of permissions is important!)
usersas <- AzureStor::get_user_delegation_sas(endp, userkey, resource = "processing/chla-fall.tif",
                                              resource_types = "b", permissions = "r")

r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif", usersas, sep = "?"))



key <- "vw9J+qa1sijEq6Ai6S62+sT/FkbCNFCm6tgpm/Ffh5bpXQikEifablcsgRmccHggJtl0R0Opy8ps+ASteCAt3A=="
fl <- AzureStor::storage_endpoint("https://nccoswsdevstor.file.core.windows.net", key = key)
AzureStor::list_storage_containers(fl)
cont <- AzureStor::storage_container(fl, "code-391ff5ac-6576-460f-ba4d-7e03433c68b6")
AzureStor::list_storage_files(cont)

AzureStor::list_storage_files(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home")
temp <- AzureStor::storage_read_delim(cont, "Users/jeffery.leirness/_env/rocker-geospatial-4-3-2/home/.gitignore")

# AzureStor::storage_load_rds()
