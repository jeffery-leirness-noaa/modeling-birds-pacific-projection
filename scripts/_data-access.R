# create azure login client to authenticate with Azure Resource Manager
# not sure this is necessary
# AzureRMR::create_azure_login(auth_type = "device_code")
# AzureRMR::list_azure_logins()

# # test out azure login client functionality
# az <- AzureRMR::get_azure_login()
# az$list_subscriptions()
# sub <- az$get_subscription_by_name("nos-nccos-adfssub-6301")
# sub$get_resource_group("nccos-mse-biogeo-seabirds-rg")

AzureRMR::list_azure_logins()
AzureRMR::create_azure_login(auth_type = "device_code")
ck <- AzureRMR::get_azure_login()
ck$list_subscriptions()[[1]]$list_storage_accounts()

rg <- ck$list_subscriptions()[[1]]$get_resource_group("nccos-mse-biogeo-seabirds-rg")
rg$create_key_vault("mykeyvault")


vault <- AzureKeyVault::key_vault("https://mykeyvault.vault.azure.net", auth_type = "device_code")


# perhaps easier method
token <- AzureRMR::get_azure_login()
endp <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                    token = token)
AzureStor::list_storage_containers(endp)


AzureRMR::list_azure_tokens()

# if token exists, use it
token <- AzureRMR::list_azure_tokens()[[1]]

AzureAuth::get_managed_token("https://storage.azure.com")
AzureAuth::get_managed_token("https://management.azure.com/")

# else, create/get azure active directory (AAD) token
token <- AzureRMR::get_azure_token("https://storage.azure.com", tenant = "common",
                                   app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
                                   auth_type = "device_code")
token <- AzureAuth::get_azure_token("https://storage.azure.com", tenant = "common",
                                    app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
                                    auth_type = "device_code")

# test out file access using token
endp <- AzureStor::storage_endpoint("https://nccospacificsbdatastor.blob.core.windows.net",
                                    token = token)
AzureStor::list_storage_containers(endp)
cont <- AzureStor::storage_container(endp, "processing")
AzureStor::list_storage_files(cont)
dat <- AzureStor::storage_read_csv(cont, "segmented-data.csv")
m <- AzureStor::storage_load_rds(cont, "bfal.rds")

cont <- AzureStor::storage_container(endp, "raw")
con <- rawConnection(raw(0), "r+")
AzureStor::storage_download(cont, "study-area.gpkg", con)
sf::st_read(con)
close(con)

cont <- AzureStor::storage_container(endp, "raw")
dir_temp <- tempdir()
AzureStor::storage_download(cont, src = "study-area.gpkg", dest = fs::path(dir_temp, "study-area.gpkg"))
sf::st_read(fs::path(dir_temp, "study-area.gpkg"))

# custom function to load sf file
storage_read_sf <- function(container, file) {
  # con <- rawConnection(raw(0), "r+")
  # on.exit(try(close(con), silent=TRUE))
  # AzureStor::storage_download(container, file, con)
  # sf::read_sf(con)
  dir_temp <- tempdir()
  on.exit(try(fs::dir_delete(dir_temp), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fs::path(dir_temp, file))
  sf::read_sf(fs::path(dir_temp, file))
}
storage_read_sf(cont, "study-area.gpkg") |> plot()


cont <- AzureStor::storage_container(endp, "processing")
dir_temp <- tempdir()
conn <- AzureStor::storage_download(cont, src = "_targets_modeling/objects/data", dest = NULL)


storage_read_qs <- function(container, file) {
  dir_temp <- tempdir()
  on.exit(try(fs::dir_delete(dir_temp), silent = TRUE))
  AzureStor::storage_download(container, src = file, dest = fs::path(dir_temp, file))
  qs::qread(fs::path(dir_temp, file))
}
storage_read_qs(cont, file = "_targets_modeling/objects/data")

AzureStor::storage_container(endp, "processing") |>
  AzureStor::list_storage_files()

# # test downloading entire directory
# # this was unsuccessful
# AzureStor::storage_container(endp, "processing") |>
#   AzureStor::storage_download(src = "_targets_modeling", dest = "data/_targets_modeling")

AzureStor::storage_container(endp, "processing") |>
  AzureStor::storage_multidownload(src = "_targets_modeling", dest = "data/_targets_modeling", recursive = TRUE)

# custom function to download entire directory
storage_download_dir <- function(container, src, dest) {
  is_dir <- AzureStor::list_storage_files(container) |>
    tibble::as_tibble() |>
    dplyr::filter(name == src) |>
    dplyr::pull(isdir)
  if (is_dir) {
    src <- AzureStor::list_storage_files(container, src) |>
      tibble::as_tibble() |>
      dplyr::filter(!isdir) |>
      dplyr::pull(name)
    dest <- fs::path(dest, src)
    AzureStor::storage_multidownload(container, src = src, dest = dest, recursive = TRUE)
  }
}




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
# this method is successful for reading files!
usersas <- "?sv=2023-01-03&st=2024-03-19T15%3A25%3A29Z&se=2024-03-26T15%3A25%3A00Z&skoid=8b019ad4-f16e-4b34-a2ee-d46e2ef407f4&sktid=abc10057-d592-4f3c-a901-c5e4c87b50cf&skt=2024-03-19T15%3A25%3A29Z&ske=2024-03-26T15%3A25%3A00Z&sks=b&skv=2023-01-03&sr=c&sp=rwl&sig=9d2V9YIsWFGvwVaSsw7ZUJhkaS1XATjn9CVgnF4xs0M%3D"
r <- terra::rast(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall.tif",
                       usersas, sep = "?"))
sa <- sf::read_sf(paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/study-area.gpkg",
                        usersas, sep = "?"))

# but not successful for writing files
fn <- paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/chla-fall-test1.tif",
            usersas, sep = "?")
terra::writeRaster(r, filename = fn, filetype = "GTiff", overwrite = TRUE)
fn <- paste("https://nccospacificsbdatastor.blob.core.windows.net/processing/mtcars-test1.csv",
            usersas, sep = "?")
data(mtcars)
write.csv(mtcars, fn)



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
