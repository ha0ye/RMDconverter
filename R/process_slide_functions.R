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

        if (is_title_slide(curr_slide))
        {
            lines_to_write <- c(process_title_slide(curr_slide),
                                "## Intro")
        } else if (is_ending_slide(curr_slide)) {
            lines_to_write <- c(lines_to_write, process_ending_slide(curr_slide))
        } else if (is_section_slide(curr_slide)) {
            lines_to_write <- c(lines_to_write, process_section_slide(curr_slide))
        } else {
            lines_to_write <- c(lines_to_write, process_content_slide(curr_slide,
                                                                      fix_image_paths = fix_image_paths))
        }
    }

    # write notes out
    writeLines(lines_to_write, out_file)

    if (clean)
    {
        unlink(md_file)
    }
    return()
}

is_title_slide <- function(curr_slide, min_subheaders = 3)
{
    if (min_subheaders > length(curr_slide))
        return(FALSE)
    num_subheaders <- sum(grepl("^#{2,4}", curr_slide))
    return(num_subheaders >= min_subheaders)
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
        convert_slide_title("## ")
}

process_content_slide <- function(curr_slide, fix_image_paths = FALSE)
{
    curr_slide %>%
        remove_slide_break() %>%
        remove_slide_formatting() %>%
        indent_content() %>%
        convert_slide_title() %>%
        modify_image_paths(run = fix_image_paths) %>%
        fix_html_chars() %>%
        remove_extra_formatting()
}

#---- helper functions ----
convert_slide_title <- function(curr_slide, repl = "* ")
{
    return(gsub("^# ", repl, curr_slide))
}

fix_html_chars <- function(curr_slide)
{
    curr_slide <- gsub("&lt;", "<", curr_slide)
    curr_slide <- gsub("&gt;", ">", curr_slide)
    return(curr_slide)
}

modify_image_paths <- function(curr_slide, run = FALSE, prepend = "slides/")
{
    if (!run)
        return(curr_slide)
    pattern <- "^(.*\\[.+\\]\\()(.+)(\\).*)$"
    pattern_alt <- "^(.*src\\s*=\\s*\\\")([^\\\"]+)(\\\".*)$"
    for(i in seq(curr_slide))
    {
        if (grepl(pattern, curr_slide[i]))
        {
            img_path <- gsub(pattern, "\\2", curr_slide[i])
            fixed_path <- paste0(prepend, img_path)
            if (file.exists(fixed_path))
            {
                curr_slide[i] <- gsub(pattern, paste0("\\1", fixed_path, "\\3"), curr_slide[i])
            }
        } else if (grepl(pattern_alt, curr_slide[i])) {
            img_path <- gsub(pattern_alt, "\\2", curr_slide[i])
            fixed_path <- paste0(prepend, img_path)
            if (file.exists(fixed_path))
            {
                curr_slide[i] <- gsub(pattern_alt, paste0("\\1", fixed_path, "\\3"), curr_slide[i])
            }
        }
    }
    return(curr_slide)
}

indent_content <- function(curr_slide)
{
    header_lines <- grep("^#", curr_slide)
    blank_lines <- grep("^$", curr_slide)
    content_idx <- setdiff(seq(curr_slide), c(header_lines, blank_lines))
    curr_slide[content_idx] <- paste("  ", curr_slide[content_idx])
    return(curr_slide)
}

remove_slide_break <- function(curr_slide)
{
    to_remove <- grep("^\\s*---\\s*", curr_slide, perl = TRUE)
    return(curr_slide[setdiff(seq(curr_slide), to_remove)])
}

remove_slide_formatting <- function(curr_slide)
{
    to_remove <- grep("^class:\\s*.*", curr_slide)
    return(curr_slide[setdiff(seq(curr_slide), to_remove)])
}

remove_extra_formatting <- function(curr_slide)
{
    return(gsub("\\.\\w+\\[\\[(.+)\\]\\]", "\\1", curr_slide))
}

remove_extra_blank_lines <- function(text)
{
    blank_lines <- grep("^$", text)
    blank_lines_to_keep <- setdiff(blank_lines, blank_lines-1)
    blank_lines_to_keep <- setdiff(blank_lines, blank_lines+1)
    content_lines <- setdiff(seq(text), blank_lines)
    to_keep <- sort(content_lines, blank_lines_to_keep)
    return(text[to_keep])
}
