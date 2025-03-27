# azure_auth_token() is not accessible from the other utility functions,
# which can lead to an issue when a token expires while a target is running. A
# possible solution is to put these utility functions into a separate R package
# that can be installed via github and referenced via
# <package-name>::azure_auth_token().
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
