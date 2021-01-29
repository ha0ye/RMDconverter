test_dir <- tempdir()

test_that("demo file 1 works", {
    slides_file <- system.file("extdata/test_slides.Rmd", package = "RMDconverter")
    md_file <- file.path(test_dir, "test_slides.md")
    out_file <- file.path(test_dir, "test_out.md")

    expect_error(out <- process_slides(slides_file = slides_file,
                          md_file = md_file,
                          out_file = out_file,
                          clean = FALSE),
                 NA)

    md_contents <- readLines(md_file)

    expected_md_file <- system.file("extdata/test_slides.md", package = "RMDconverter")
    expected_md_contents <- readLines(expected_md_file)
    expect_equal(md_contents, expected_md_contents)

    out_contents <- readLines(out_file)

    expected_out_file <- system.file("extdata/test_out.md", package = "RMDconverter")
    expected_out_contents <- readLines(expected_out_file)
    expect_equal(out_contents, expected_out_contents)
})

test_that("demo file 2 works", {
    slides_file <- system.file("extdata/test_slides_2.Rmd", package = "RMDconverter")
    md_file <- file.path(test_dir, "test_slides_2.md")
    out_file <- file.path(test_dir, "test_out_2.md")

    expect_error(out <- process_slides(slides_file = slides_file,
                                       md_file = md_file,
                                       out_file = out_file,
                                       clean = FALSE),
                 NA)

    md_contents <- readLines(md_file)

    expected_md_file <- system.file("extdata/test_slides_2.md", package = "RMDconverter")
    expected_md_contents <- readLines(expected_md_file)
    expect_equal(md_contents, expected_md_contents)

    out_contents <- readLines(out_file)

    expected_out_file <- system.file("extdata/test_out_2.md", package = "RMDconverter")
    expected_out_contents <- readLines(expected_out_file)
    expect_equal(out_contents, expected_out_contents)
})
