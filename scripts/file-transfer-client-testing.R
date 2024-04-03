
# needed to install azure cli to docker image for this to work
# cannot get `az login` to work; instead must use `az login --use-device-code`

reticulate::source_python("../File-Transfer-Solution/file_transfer.py")

account_url <- "https://nccospacificsbdatastor.blob.core.windows.net"
container_name <- "raw"
local_folder <- "data"
cloud_folder <- ""

ftc <- FileTransferClient(account_url,
                          container_name = container_name,
                          local_folder = local_folder,
                          cloud_folder = cloud_folder)

# transfer files from blob to compute
ftc$transfer_from_blob_to_compute()

# transfer files from compute to blob
ftc$upload_folder_to_blob(source_folder = local_folder)



# create month-year summary rasters ---------------------------------------
targets::tar_make()
r <- targets::tar_read(test) |>
  # purrr::map(terra::unwrap) |>
  terra::rast()
terra::plot(r)

# transfer files from compute to blob
ftc$upload_folder_to_blob(source_folder = "output")
