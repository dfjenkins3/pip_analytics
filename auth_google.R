readRenviron(".Renviron")

library(googlesheets4)

if (!interactive()) {
  stop(
    paste(
      "Google OAuth needs an interactive R session.",
      "Run `R --vanilla` from this folder, then run `source(\"auth_google.R\")`.",
      "You can also open auth_google.R in RStudio and source it.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

auth_cache <- Sys.getenv("GARGLE_OAUTH_CACHE", unset = ".secrets/gargle")
auth_email <- Sys.getenv("PIP_ANALYTICS_GOOGLE_EMAIL", unset = "")

if (!dir.exists(auth_cache)) {
  dir.create(auth_cache, recursive = TRUE, showWarnings = FALSE)
}

if (!nzchar(auth_email)) {
  stop(
    "Set PIP_ANALYTICS_GOOGLE_EMAIL in .Renviron before authenticating.",
    call. = FALSE
  )
}

gs4_auth(
  email = auth_email,
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
  cache = auth_cache
)

message("Google Sheets credentials cached in: ", normalizePath(auth_cache))
