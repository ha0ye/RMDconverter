test_that("is_title_slide works", {
    title_slide <- c("# Slide Title",
                     "## Author",
                     "## Affiliation",
                     "## Date")
  expect_true(is_title_slide(title_slide))

  not_title_slide <- c("# References (usage)",
                       "",
                       "```",
                       "Sometimes whelks eat lobsters [@Barkai_1998].",
                       "```",
                       "",
                       '## Sorry this feature currently cannot be used without the "bibtex" package installed.',
                       '## Please install from GitHub using the "remotes" (or ;devtools") package:',
                       "## ",
                       '## remotes::install_github("ROpenSci/bibtex")')

  expect_false(is_title_slide(not_title_slide))
  expect_true(is_title_slide(not_title_slide, max_other_lines = 100))
})
