test_dir <- tempdir()

test_that("demo files work", {
    slides_file <- system.file("extdata/test_slides.Rmd", package = "RMDconverter")
    md_file <- file.path(test_dir, "slides_temp.md")
    out_file <- file.path(test_dir, "test_out.md")

    expect_error(out <- process_slides(slides_file = slides_file,
                          md_file = md_file,
                          out_file = out_file,
                          clean = FALSE),
                 NA)

    md_contents <- readLines(md_file)
    expect_length(md_contents, 366)
    expect_known_hash(md_contents, "5689d7cf49")

    out_contents <- readLines(out_file)
    expect_length(out_contents, 125)
    expect_known_hash(out_contents, "fc4893fe2f")
})
