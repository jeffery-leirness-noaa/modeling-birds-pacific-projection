
# needed to install azure cli to docker image for this to work
# cannot get `az login` to work; instead must use `az login --use-device-code`

file_transfer <- reticulate::import_from_path("file_transfer", path = "../File-Transfer-Solution")

account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "raw"
local_folder <- "data"
cloud_folder <- "wcnrt"

ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)

# transfer files from blob to compute
ftc$transfer_from_blob_to_compute()

# transfer files from blob to compute
account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "processing"
local_folder <- "_targets"
cloud_folder <- "_targets"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$transfer_from_blob_to_compute()

# transfer files from blob to compute
account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "output"
local_folder <- "output"
cloud_folder <- "wcra31"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$transfer_from_blob_to_compute()

# transfer files from compute to blob
account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "output"
local_folder <- "output/wcra31"
cloud_folder <- "wcra31"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)



# create month-year summary rasters ---------------------------------------
# targets::tar_destroy()
targets::tar_make()
r <- targets::tar_read(test) |>
  # purrr::map(terra::unwrap) |>
  terra::rast()
terra::plot(r)

# transfer files from compute to blob
account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "output"
local_folder <- "output"
cloud_folder <- ""
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)

# delete files from compute
fl <- fs::dir_ls("output")
fs::file_delete(fl)

# transfer {targets} files from compute to blob
account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "processing"
local_folder <- "_targets/meta"
cloud_folder <- "_targets/meta"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)
local_folder <- "_targets/objects"
cloud_folder <- "_targets/objects"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)
local_folder <- "_targets/user"
cloud_folder <- "_targets/user"
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)

# delete files from compute
fl <- fs::dir_ls("_targets", recurse = TRUE)
fs::file_delete(fl)


# another method using (outdated) azuremlsdk package
ws <- azuremlsdk::get_workspace(name = "nccos-mse-biogeo-seabird-ml",
                                subscription_id = "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c",
                                resource_group = "nccos-mse-biogeo-seabirds-rg")

ds <- azuremlsdk::get_datastore(ws, datastore_name = "datastor_raw")
azuremlsdk::download_from_datastore(ds, target_path = "output", prefix = "species-codes.csv")

ds <- azuremlsdk::get_datastore(ws, datastore_name = "workspacefilestore")
azuremlsdk::mount_file_dataset(ds)

azuremlsdk::data_path(ds)

azuremlsdk::get_default_datastore(ws)
