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

indent_content <- function(curr_slide, num_spaces = 2)
{
    indent <- paste0(rep_len(" ", num_spaces), collapse = "")
    header_lines <- grep("^#", curr_slide)
    blank_lines <- grep("^$", curr_slide)
    content_idx <- setdiff(seq(curr_slide), c(header_lines, blank_lines))
    curr_slide[content_idx] <- paste0(indent, curr_slide[content_idx])
    return(curr_slide)
}

filter_out_pattern <- function(curr_slide, pattern, ...)
{
    to_remove <- grep(pattern, curr_slide, ...)
    return(curr_slide[setdiff(seq(curr_slide), to_remove)])
}

remove_slide_break <- function(curr_slide)
{
    filter_out_pattern(curr_slide, "^\\s*---\\s*")
}

remove_slide_formatting <- function(curr_slide)
{
    filter_out_pattern(curr_slide, "^class:\\s*.*")
}

remove_extra_formatting <- function(curr_slide)
{
    return(gsub("\\.\\w+\\[\\[(.+)\\]\\]", "\\1", curr_slide))
}

remove_extra_blank_lines <- function(text)
{
    to_keep <- integer(0)
    while(length(text) > length(to_keep))
    {
        blank_lines <- grep("^\\s*$", text)
        blank_lines_to_keep <- setdiff(blank_lines, blank_lines-1)
        blank_lines_to_keep <- setdiff(blank_lines, blank_lines+1)
        content_lines <- setdiff(seq(text), blank_lines)
        to_keep <- sort(c(content_lines, blank_lines_to_keep))
        text <- text[to_keep]
    }

    return(text)
}

remove_blank_line_after_primary_bullet <- function(curr_slide)
{
    primary_bullet_line <- grep("^\\* ", curr_slide)
    if (length(primary_bullet_line) == 0)
        return(curr_slide)

    target_blank_line <- primary_bullet_line[1] + 1
    if (target_blank_line %in% grep("^\\s*$", curr_slide))
    {
        curr_slide <- curr_slide[-target_blank_line]
    }
    return(curr_slide)
}

remove_blank_lines <- function(curr_slide)
{
    filter_out_pattern(curr_slide, "^\\s*$")
}
