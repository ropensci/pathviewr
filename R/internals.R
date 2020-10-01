skip_on_gh_actions <- function() {
  if (!identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("On GitHub Actions")
}
