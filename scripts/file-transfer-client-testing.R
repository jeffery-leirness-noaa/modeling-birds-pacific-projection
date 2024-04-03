
# needed to install azure cli to docker image for this to work
# cannot get `az login` to work; instead must use `az login --use-device-code`

file_transfer <- reticulate::import_from_path("file_transfer", path = "../File-Transfer-Solution")

account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "raw"
local_folder <- "data"
cloud_folder <- ""

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
cloud_folder <- ""
ftc <- file_transfer$FileTransferClient(account_url,
                                        container_name = container_name,
                                        local_folder = local_folder,
                                        cloud_folder = cloud_folder)
ftc$transfer_from_blob_to_compute()


# transfer files from compute to blob
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
