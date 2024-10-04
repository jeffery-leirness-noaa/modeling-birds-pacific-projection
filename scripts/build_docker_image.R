tag <- lubridate::now() |>
  lubridate::as_date() |>
  stringr::str_replace_all(pattern = "-", replacement = ".")

# stringr::str_c("docker build github.com/jeffery-leirness-noaa/modeling-birds-pacific-projection:", tag)
#
# build_docker_image <- function(repo, tag = "latest") {
#   if (tag == "date") {
#     tag <- lubridate::now() |>
#       lubridate::as_date() |>
#       stringr::str_replace_all(pattern = "-", replacement = ".")
#   }
#   stringr::str_c("docker build -t ", repo, ":", tag, " .")
# }
#
# build_docker_image(repo = "jefferyleirnessnoaa/modeling-birds-pacific-projection",
#                    tag = "date") |>
#   system2()
# # system2("docker push jefferyleirnessnoaa/modeling-birds-pacific-projection")

# pracpac::use_docker()
# pracpac::create_docker_dir()
# pracpac::renv_deps()

docker_dir <- "docker"
fs::file_copy(renv::paths$lockfile(),
              new_path = fs::path(docker_dir, "renv.lock"),
              overwrite = TRUE)
image_tag <- tag
image_user <- "jeffery-leirness-noaa"
image_repo <- "modeling-birds-pacific-projection"
buildcmd <- glue::glue("docker build --tag ghcr.io/{image_user}/{image_repo}:{image_tag} {docker_dir}")
# system(buildcmd)
pushcmd <- glue::glue("docker push ghcr.io/{image_user}/{image_repo}:{image_tag}")
# system(pushcmd)
