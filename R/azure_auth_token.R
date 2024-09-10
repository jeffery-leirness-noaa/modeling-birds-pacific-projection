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
