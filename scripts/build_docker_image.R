tag <- lubridate::now() |>
  lubridate::as_date() |>
  stringr::str_replace_all(pattern = "-", replacement = ".")

stringr::str_c("docker build github.com/jeffery-leirness-noaa/modeling-birds-pacific-projection:", tag)

build_docker_image <- function(repo, tag = "latest") {
  if (tag == "date") {
    tag <- lubridate::now() |>
      lubridate::as_date() |>
      stringr::str_replace_all(pattern = "-", replacement = ".")
  }
  stringr::str_c("docker build -t ", repo, ":", tag, " .")
}

build_docker_image(repo = "jefferyleirnessnoaa/modeling-birds-pacific-projection",
                   tag = "date") |>
  system2()
# system2("docker push jefferyleirnessnoaa/modeling-birds-pacific-projection")
