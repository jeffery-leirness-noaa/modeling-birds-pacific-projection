#' Transfer Files to Azure Blob Storage
#'
#' This function transfers files to Azure Blob Storage.
#'
#' @return Invisible. The function is called for its side effects.
transfer_files_blob <- function(container_name, local_folder, cloud_folder, type, account_url = "https://nccospacificsbdatastor.blob.core.windows.net") {
  file_transfer <- reticulate::import_from_path("file_transfer")
  ftc <- file_transfer$FileTransferClient(account_url,
                                          container_name = container_name,
                                          local_folder = local_folder,
                                          cloud_folder = cloud_folder)
  if (type == "download") {
    # transfer files from blob to compute
    ftc$transfer_from_blob_to_compute()
  } else if (type == "upload") {
    # transfer files from compute to blob
    ftc$upload_folder_to_blob(source_folder = local_folder, destination_folder = cloud_folder)
  }
}
