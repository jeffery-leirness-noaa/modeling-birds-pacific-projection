# azure_auth_token() is not accessible from the other utility functions,
# which can lead to an issue when a token expires while a target is running. A
# possible solution is to put these utility functions into a separate R package
# that can be installed via github and referenced via
# <package-name>::azure_auth_token().

#' Get Azure Authentication Token
#'
#' This function obtains an authentication token for Azure services. It first tries
#' to get a managed token, and if that fails, it falls back to interactive authentication.
#'
#' @param resource Character string. The Azure resource to authenticate against.
#'        Default is "https://storage.azure.com".
#' @param tenant Character string. The Azure tenant. Default is "common".
#' @param app Character string. The application ID. If NULL, it will try to get the
#'        client ID from an existing login, or create a new login.
#' @param auth_type Character string. Authentication type. Default is "device_code".
#'
#' @return An Azure authentication token object.
azure_auth_token <- function(
    resource = "https://storage.azure.com",
    tenant = "common",
    app = NULL,
    auth_type = "device_code"
) {
  tryCatch(
    AzureAuth::get_managed_token(resource),
    error = function(err) {
      if (is.null(app)) {
        app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                   silent = TRUE)
        if (inherits(app, "try-error")) {
          AzureRMR::create_azure_login(auth_type = auth_type)
          app <- AzureRMR::get_azure_login()$token$client$client_id
        }
      }
      tokens <- AzureRMR::list_azure_tokens()
      resources <- purrr::map(tokens, \(x) x$resource)
      token_use <- match(resource, resources)[1]
      if (!is.na(token_use)) {
        tokens[[token_use]]
      } else {
        AzureRMR::get_azure_token(resource,
                                  tenant = tenant,
                                  app = app,
                                  auth_type = auth_type)
      }
    }
  )
}

#' Upload a File to Azure Blob Storage
#'
#' This function uploads a file to Azure Blob Storage using the endpoints specified
#' in the environment variables TARGETS_ENDPOINT and TARGETS_CONTAINER.
#'
#' @param key Character string. The key (path within the container) where the file will be stored.
#' @param path Character string. The local path to the file to be uploaded.
#'
#' @return Invisible. The function is called for its side effects.
azure_upload <- function(key, path) {
  if (fs::is_dir(path)) {
    stop("This CAS repository does not support directory outputs.")
  }
  azure_auth_token <- function(
    resource = "https://storage.azure.com",
    tenant = "common",
    app = NULL,
    auth_type = "device_code"
  ) {
    tryCatch(
      AzureAuth::get_managed_token(resource),
      error = function(err) {
        if (is.null(app)) {
          app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                     silent = TRUE)
          if (inherits(app, "try-error")) {
            AzureRMR::create_azure_login()
            app <- AzureRMR::get_azure_login()$token$client$client_id
          }
        }
        tokens <- AzureRMR::list_azure_tokens()
        resources <- purrr::map(tokens, \(x) x$resource)
        token_use <- match(resource, resources)[1]
        if (!is.na(token_use)) {
          tokens[[token_use]]
        } else {
          AzureRMR::get_azure_token(resource,
                                    tenant = tenant,
                                    app = app,
                                    auth_type = auth_type)
        }
      }
    )
  }
  AzureStor::upload_to_url(
    path,
    dest = paste(Sys.getenv("TARGETS_ENDPOINT"),
                 Sys.getenv("TARGETS_CONTAINER"),
                 paste0("_targets_", gert::git_info()$shorthand),
                 key,
                 sep = "/"),
    token = azure_auth_token()
  )
}

#' Download a File from Azure Blob Storage
#'
#' This function downloads a file from Azure Blob Storage using the endpoints specified
#' in the environment variables TARGETS_ENDPOINT and TARGETS_CONTAINER.
#'
#' @param key Character string. The key (path within the container) of the file to download.
#' @param path Character string. The local path where the downloaded file will be saved.
#'
#' @return Invisible. The function is called for its side effects.
azure_download <- function(key, path) {
  azure_auth_token <- function(
    resource = "https://storage.azure.com",
    tenant = "common",
    app = NULL,
    auth_type = "device_code"
  ) {
    tryCatch(
      AzureAuth::get_managed_token(resource),
      error = function(err) {
        if (is.null(app)) {
          app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                     silent = TRUE)
          if (inherits(app, "try-error")) {
            AzureRMR::create_azure_login()
            app <- AzureRMR::get_azure_login()$token$client$client_id
          }
        }
        tokens <- AzureRMR::list_azure_tokens()
        resources <- purrr::map(tokens, \(x) x$resource)
        token_use <- match(resource, resources)[1]
        if (!is.na(token_use)) {
          tokens[[token_use]]
        } else {
          AzureRMR::get_azure_token(resource,
                                    tenant = tenant,
                                    app = app,
                                    auth_type = auth_type)
        }
      }
    )
  }
  AzureStor::download_from_url(
    paste(Sys.getenv("TARGETS_ENDPOINT"), Sys.getenv("TARGETS_CONTAINER"),
          paste0("_targets_", gert::git_info()$shorthand), key, sep = "/"),
    dest = path,
    token = azure_auth_token(),
    overwrite = TRUE
  )
}

#' Check if a File Exists in Azure Blob Storage
#'
#' This function checks if a file exists in Azure Blob Storage using the endpoints
#' specified in the environment variables TARGETS_ENDPOINT and TARGETS_CONTAINER.
#'
#' @param key Character string. The key (path within the container) to check.
#'
#' @return Logical. TRUE if the file exists, FALSE otherwise.
azure_exists <- function(key) {
  azure_auth_token <- function(
    resource = "https://storage.azure.com",
    tenant = "common",
    app = NULL,
    auth_type = "device_code"
  ) {
    tryCatch(
      AzureAuth::get_managed_token(resource),
      error = function(err) {
        if (is.null(app)) {
          app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                     silent = TRUE)
          if (inherits(app, "try-error")) {
            AzureRMR::create_azure_login()
            app <- AzureRMR::get_azure_login()$token$client$client_id
          }
        }
        tokens <- AzureRMR::list_azure_tokens()
        resources <- purrr::map(tokens, \(x) x$resource)
        token_use <- match(resource, resources)[1]
        if (!is.na(token_use)) {
          tokens[[token_use]]
        } else {
          AzureRMR::get_azure_token(resource,
                                    tenant = tenant,
                                    app = app,
                                    auth_type = auth_type)
        }
      }
    )
  }
  AzureStor::storage_endpoint(Sys.getenv("TARGETS_ENDPOINT"),
                              token = azure_auth_token()) |>
    AzureStor::storage_container(name = Sys.getenv("TARGETS_CONTAINER")) |>
    AzureStor::storage_file_exists(
      fs::path(paste0("_targets_", gert::git_info()$shorthand), key)
    )
}
