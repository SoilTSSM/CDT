
rm(list = ls())
library(knitr)
library(markdown)
library(rmarkdown)
library(stringr)

getIndexTOC <- function(fileRmd){
	input_lines <- readLines(fileRmd, warn = FALSE)
	input_lines <- iconv(input_lines, from = "", to = "UTF-8")
	partitions <- partition_yaml_front_matter(input_lines)
	if(is.null(partitions$front_matter)) return(NULL)

	front_matter <- partitions$front_matter
	if(length(front_matter) < 3) return(NULL)

	front_matter <- front_matter[2:(length(front_matter)-1)]
	front_matter <- paste(front_matter, collapse="\n")
	validate_front_matter(front_matter)
	parsed_yaml <- yaml_load_utf8(front_matter)
	if(!is.list(parsed_yaml)) return(NULL)

	toTOC <- list()
	html.file <- paste(tools::file_path_sans_ext(basename(fileRmd)), '.html', sep = '')
	if(!is.null(parsed_yaml$title)) toTOC <- c(toTOC, list(list(toc = paste('#', parsed_yaml$title), html = html.file)))

	if(parsed_yaml$output$html_document$toc){
		len.toc <- parsed_yaml$output$html_document$toc_depth
		len.toc <- if(is.null(len.toc)) 3 else len.toc
		ihead <- grep("^#", str_trim(partitions$body))
		ichunk <- grep("^```", str_trim(partitions$body))
		if(length(ichunk) >= 2){
			mchunk <- matrix(ichunk, ncol = 2, byrow = TRUE)
			ichunk <- sapply(seq_along(ihead), function(i) any(mchunk[, 1] <= ihead[i] & mchunk[, 2] >= ihead[i]))
			ihead <- ihead[!ichunk]
		}
		if(length(ihead) > 0){
			md.head <- partitions$body[ihead]
			md.head <- strsplit(md.head, ' ')
			tree.toc <- nchar(sapply(md.head, '[[', 1))
			tree.toc <- tree.toc <= len.toc
			if(any(tree.toc)){
				md.head <- md.head[tree.toc]
				tmp.toc <- vector(mode = 'list', length = length(md.head))
				isname <- lapply(md.head, function(x) grep("anchor.point", x))
				isname0 <- lapply(md.head, function(x) grep("`r", x))
				ix <- sapply(isname, length)
				i0 <- which(ix == 0)
				i1 <- which(ix == 1)
				if(length(i0) > 0){
					x0 <- lapply(md.head[i0], paste, collapse = ' ')
					tmp.toc[i0] <- lapply(x0, function(x) list(toc = x, html = html.file))
				}
				if(length(i1) > 0){
					r0 <- unlist(isname0[i1])
					r1 <- unlist(isname[i1])
					x1 <- md.head[i1]
					tmp.toc[i1] <- lapply(seq_along(x1), function(j){
						toc <- paste(x1[[j]][1:(r0[j]-1)], collapse = ' ')
						xref <- gsub(".*\\((.*)\\).*", "\\1", x1[[j]][r1[j]])
						xref <- gsub('^.|.$', '', xref)
						html <- paste(html.file, '#', xref, sep = '')
						list(toc = toc, html = html)
					})
				}
				toTOC <- c(toTOC, tmp.toc)
			}
		}
	}
	toTOC <- toTOC[sapply(toTOC, function(x) !is.null(x$toc))]
	if(length(toTOC) > 0) toTOC else NULL
}

## from rmarkdown packages
partition_yaml_front_matter <- function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}

trim_trailing_ws <- function (x) {
  sub("\\s+$", "", x)
}

mark_utf8 <- function(x) {
  if (is.character(x)) {
    Encoding(x) <- 'UTF-8'
    return(x)
  }
  if (!is.list(x)) return(x)
  attrs <- attributes(x)
  res <- lapply(x, mark_utf8)
  attributes(res) <- attrs
  res
}

yaml_load_utf8 <- function(string, ...) {
  string <- paste(string, collapse = '\n')
  mark_utf8(yaml::yaml.load(enc2utf8(string), ...))
}

validate_front_matter <- function(front_matter) {
  front_matter <- trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter))
    stop("Invalid YAML front matter (ends with ':')", call. = FALSE)
}


####################################################################

help_dir <- '/Users/rijaf/Desktop/ECHANGE/github/CDT/help/html'
mathjax_dir <- '/Users/rijaf/Desktop/ECHANGE/MathJax'
sources_dir <- '../sources'
r_images <- 'images/R_images'

setwd(help_dir)
Sys.setenv(RMARKDOWN_MATHJAX_PATH = mathjax_dir)

## arrange files for toc
toc.order <- c("index.Rmd", "cdt_file_menu.Rmd", "cdt_data_input.Rmd", "format_cdt_data.Rmd")

#####
all_src <- list.files(path = sources_dir, pattern = '.Rmd', full.names = TRUE, recursive = TRUE)
all_src <- all_src[match(toc.order, basename(all_src))]
rmd_files <- basename(all_src)

#####
parent_dir <- sapply(strsplit(all_src, '/'), function(x) x[3])
all_files <- gsub('.Rmd', '', rmd_files)
html_files <- paste(all_files, '.html', sep = '')
figure_dir <- paste(all_files, '_files', sep = '')
figure_files <- list.files(r_images, recursive = TRUE, full.names = TRUE)

## Test change 
# git_modifed_added <- do.call('rbind',lapply(strsplit(system('git status -s',intern = TRUE),' '), function(x) x[x != ""]))
# is_change <- grep('help',git_modifed_added[,2])
# if(length(is_change)>0)
# changed_files <- git_modifed_added[is_change,2]
# # and do build else do nothing/ ifif(length(is_change)>0){buil}

# # search all Rmd files 
# # search in images and get files corresponding
# # if any change in common_dir re-build all

## replace parent_dir, rmd_files, all_files, html_files and figure_dir to the new changed files

# all_files <- c("index", "cdt_data_input", "cdt_file_menu")
# parent_dir <- c("index", "cdt_file", "cdt_file")
# rmd_files <- paste(all_files, '.Rmd', sep = '')
# html_files <- paste(all_files, '.html', sep = '')
# figure_dir <- paste(all_files, '_files', sep = '')

# all_files <- "cdt_file_menu"
# rmd_files <- "cdt_file_menu.Rmd"
# parent_dir <- "cdt_file"
# html_files <- "cdt_file_menu.html"
# figure_dir <- "cdt_file_menu_files"

# html_files <- c("index.html", "format_cdt_data.html")

######
exist_Rimage_fig <- sapply(strsplit(list.files(r_images), '\\.'), '[[', 1) %in% all_files
if(any(exist_Rimage_fig)) unlink(figure_files[exist_Rimage_fig])

exist_html_fig <- dir.exists(figure_dir)
if(any(exist_html_fig)) unlink(figure_dir[exist_html_fig], recursive = TRUE, force = TRUE)

exist_html_files <- file.exists(html_files)
if(any(exist_html_files)) unlink(html_files[exist_html_files])

exist_rmd_wd_files <- file.exists(rmd_files)
if(any(exist_rmd_wd_files)) unlink(rmd_files[exist_rmd_wd_files])

###############
# remove index
index.path <- file.path(sources_dir, parent_dir[1], rmd_files[1])
parent_dir <- parent_dir[-1]
rmd_files <- rmd_files[-1]
file.copy(file.path(sources_dir, parent_dir, rmd_files), '.')

###############
cat(paste('---\n',
				'output:', '\n',
				' html_document:', '\n',
				'  toc: true', '\n',
				'  toc_depth: 1', '\n',
				'  toc_float:', '\n',
				'   collapsed: false', '\n',
				'---\n', sep = ''), file = index.path)
cat("\n<div id='toc-index' class='toc-index-class'>\n\n", file = index.path, append = TRUE)
toTOC <- do.call('c',
	lapply(seq_along(rmd_files), function(j){
		X <- getIndexTOC(rmd_files[j])
		if(is.null(X)) return(NULL)
		sapply(X, function(x){
			toc <- strsplit(x$toc, ' ')[[1]]
			toc <- paste(toc[1], ' [', paste(toc[2:length(toc)], collapse = ' '), ']', sep = '')
			htm <- paste('(', x$html, ')', sep = '')
			paste(toc, htm, sep = '')
		})
	})
)

cat(paste(toTOC, collapse = '\n\n'), file = index.path, append = TRUE)
cat("\n\n</div>\n", file = index.path, append = TRUE)
cat("\n```{r, child = '_generated_date.Rmd'}\n```\n", file = index.path, append = TRUE)
file.copy(index.path, '.')

rmd_files <- c("index.Rmd", rmd_files)

###############

# rmd_files <- c("index.Rmd", "format_cdt_data.Rmd")

###############

for(jj in seq_along(rmd_files)) render(rmd_files[jj])

exist_rmd_wd_files <- file.exists(rmd_files)
if(any(exist_rmd_wd_files)) unlink(rmd_files[exist_rmd_wd_files])

# unlink('../index.html')
# file.symlink(file.path(getwd(),'index.html'), '../index.html')
# system('ln -s "$PWD"/index.html ../index.html')

