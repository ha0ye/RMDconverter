#'
#'
#'
#' @importFrom utils tail
#' @export
process_slides <- function(slides_file = here::here("slides", "slides.Rmd"),
                           md_file = here::here("slides_temp.md"),
                           out_file = here::here("syllabus.md"),
                           clean = TRUE,
                           fix_image_paths = FALSE)
{
    # convert slides into raw markdown
    stopifnot(file.exists(slides_file))
    rmarkdown::render(slides_file, output_file = md_file)

    dat <- readLines(md_file)
    lines_to_write <- c()

    # identify slides
    slide_starts <- c(grep("^# ", dat)[1],
                      grep("^\\s*---\\s*", dat))
    slide_ends <- c(tail(slide_starts, -1),
                    grep("</textarea>", dat)) - 1
    stopifnot(length(slide_starts) == length(slide_ends))

    # process slides
    for (i in seq(NROW(slide_starts)))
    {
        curr_slide <- dat[seq(slide_starts[i], slide_ends[i])]
        lines_to_write <- c(lines_to_write,
                            process_slide(curr_slide))
    }

    # write notes out
    lines_to_write <- remove_extra_blank_lines(lines_to_write)
    writeLines(lines_to_write, out_file)

    if (clean)
    {
        unlink(md_file)
    }
    return()
}

process_slide <- function(curr_slide, fix_image_paths = FALSE)
{
    if (is_title_slide(curr_slide))
    {
        return(c(process_title_slide(curr_slide), "## Intro"))
    } else if (is_ending_slide(curr_slide)) {
        return(process_ending_slide(curr_slide))
    } else if (is_section_slide(curr_slide)) {
        return(process_section_slide(curr_slide))
    }
    # else
    process_content_slide(curr_slide,
                          fix_image_paths = fix_image_paths)
}

is_title_slide <- function(curr_slide, min_subheaders = 3, max_other_lines = 2)
{
    if (min_subheaders > length(curr_slide))
        return(FALSE)
    num_subheaders <- sum(grepl("^#{2,4}", curr_slide))
    return(num_subheaders >= min_subheaders &&
               num_subheaders+max_other_lines >= length(curr_slide))
}

process_title_slide <- function(curr_slide)
{
    curr_slide <- gsub("^# .+", "# Syllabus", curr_slide)
    curr_slide <- gsub("^#{2,4} ", "  ", curr_slide)
    return(curr_slide)
}

is_ending_slide <- function(curr_slide)
{
    any(grepl("^# Thanks", curr_slide))
}

process_ending_slide <- function(curr_slide)
{
    curr_slide %>%
        remove_slide_break() %>%
        convert_slide_title("## ")
}

is_section_slide <- function(curr_slide)
{
    2 %in% grep("^class:\\s*.*(center.*inverse|inverse.*center).*", curr_slide)
}

process_section_slide <- function(curr_slide)
{
    curr_slide %>%
        remove_slide_break() %>%
        remove_slide_formatting() %>%
        increment_header_depth() %>%
        replace_html_line_break()
}

process_content_slide <- function(curr_slide, fix_image_paths = FALSE)
{
    curr_slide %>%
        remove_slide_break() %>%
        remove_slide_formatting() %>%
        indent_content() %>%
        convert_slide_title() %>%
        remove_blank_lines() %>%
        modify_image_paths(run = fix_image_paths) %>%
        fix_html_chars() %>%
        remove_extra_formatting() %>%
        replace_html_line_break() %>%
        remove_extra_p_tags()
}
