
# ---------------------------------------
# UPDATE ME when you start a new project: 
project_name = 'P015_NCHRP_17-102'
db_name_src = 'p015_nchrp_17_102'
# ---------------------------------------

# Sys.info()["user"]
# Sys.getenv("LOGNAME")

# These should not change: 
# Project Paths for exporting data and importing data
ba_company_path <- '/Users/balmdale/Dropbox/Company/'
rs_company_path <- '/Users/rebecca/Library/CloudStorage/Dropbox/professional/Consulting/SafeStreetsResearch/Company/'
js_company_path <- '/Users/jessica/Library/CloudStorage/Dropbox/SafeStreetsResearch/Company/'

ba_proj_path <- paste0(ba_company_path, 'Projects/')
rs_proj_path <- paste0(rs_company_path, 'Projects/')
js_proj_path <- paste0(js_company_path, 'Projects/')

r_data_dir <- '/Data/r_data/'

ba_code_path <- '/Users/balmdale/code/'
js_code_path <- '/Users/jessica/github/'
rs_code_path <- '/Users/rebecca/Desktop/GitHubProject/'

graphic_dir <- '/graphics/'

# Function to export Rdata
export_rdata <- function(in_file, out_filename) {
  if (file.exists(ba_proj_path)) {
    message("Exporting using BA path")
    try(
      saveRDS(
        in_file, 
        paste0(ba_proj_path, project_name, r_data_dir, out_filename)), 
      silent=TRUE)
    message(paste0("Exported: ", out_filename))
  } else if (file.exists(js_proj_path)) {
    print("Exporting using JS path")
    try(
      saveRDS(
        in_file, 
        paste0(js_proj_path, project_name, r_data_dir, out_filename)), 
      silent=TRUE)
    message(paste0("Exported: ", out_filename))
  } else if (file.exists(rs_proj_path)) {
    message("Exporting using RS path")
    try(
      saveRDS(
        in_file, 
        paste0(rs_proj_path, project_name, r_data_dir, out_filename)), 
      silent=TRUE)
    message(paste0("Exported: ", out_filename))
  } else{
    message('Okay, the data did not export...')
  }
} 

project_path <- function(path) {
  if (file.exists(ba_proj_path)) {
    message("Reading data using BA path")
    tmp_dir <- try(
        paste0(ba_proj_path, project_name, path), 
      silent=TRUE)
  } else if (file.exists(js_proj_path)) {
    message("Reading data using JS path")
    tmp_dir <- try(
      paste0(js_proj_path, project_name, path), 
      silent=TRUE)
  } else if (file.exists(rs_proj_path)) {
    message("Reading data using RS path")
    tmp_dir <- try(
        paste0(rs_proj_path, project_name, path), 
      silent=TRUE)
  } else{
    message('Okay, the data did not load')
  }
  message(paste0('here is your path: ', tmp_dir))
  return(tmp_dir)
} 

# Function to read_data
read_rdata <- function(in_filename) {
  if (file.exists(ba_proj_path)) {
    message("Reading data using BA path")
    df <- try(
      readRDS(
        paste0(ba_proj_path, project_name, r_data_dir, in_filename)), 
      silent=TRUE)
  } else if (file.exists(js_proj_path)) {
    message("Reading data using JS path")
    df <- try(
      readRDS(
        paste0(js_proj_path, project_name, r_data_dir, in_filename)), 
      silent=TRUE)
  } else if (file.exists(rs_proj_path)) {
    message("Reading data using RS path")
    df <- try(
      readRDS(
        paste0(rs_proj_path, project_name, r_data_dir, in_filename)), 
      silent=TRUE)
  } else{
    message('Okay, the data did not load')
  }
  message(paste0('Loaded: ', in_filename))
  return(df)
} 

# Function get file path to graphic
read_graphic_md <- function(in_filename, graphic_dir) {
  if (file.exists(ba_proj_path)) {
    message("Reading data using BA path")
    graphic_path <- try(
      paste0(ba_proj_path, project_name, graphic_dir, in_filename), 
      silent=TRUE)
  } else if (file.exists(js_proj_path)) {
    message("Reading data using JS path")
    graphic_path <- try(
      paste0(js_proj_path, project_name, graphic_dir, in_filename), 
      silent=TRUE)
  } else if (file.exists(rs_proj_path)) {
    message("Reading data using RS path")
    graphic_path <- try(
      paste0(rs_proj_path, project_name, graphic_dir, in_filename), 
      silent=TRUE)
  } else{
    message('Okay, the data did not load')
  }
  message(paste0('Loaded: ', in_filename))
  return(graphic_path)
} 

# https://github.com/harrelfe/rscripts
#
#
# Figures -----


addCap <- function(label = NULL, cap = NULL, short_cap = NULL) {
  get_chunk_opt <- function(tag1, tag2, tag3) {
    r <- knitr::opts_current$get(tag1)
    if (!length(r)) {
      r <- knitr::opts_current$get(tag2)
    }
    if (!length(r)) {
      r <- knitr::opts_current$get(tag3)
    }
    
    r
  }
  
  alter_label <- function() {
    lab <- get_chunk_opt("label")
    # if chunk label exists and it does NOT start with "fig"
    if (length(lab) && (!grepl("^fig-", lab) && !grepl("^tbl-", lab))) {
      # assign lab
      lab <- paste0("fig-", lab)
    }
    lab
  }
  
  # if no label, alter label
  if (!length(label)) {
    label <- alter_label()
  }
  
  deb <- .Options$debugaddCap
  deb <- length(deb) && deb
  
  if (deb) {
    cat("label:", label, "\n", file = "/tmp/z", append = TRUE)
  }
  
  # if there is no label, return invisible list
  if (!length(label)) {
    return(invisible(list(NULL, NULL, NULL)))
  }
  
  # if label is logical and is false, return invisible list
  if (is.logical(label) && !label) {
    return(invisible(list(NULL, NULL, NULL)))
  }
  
  # if there is no cap , get cap from chunk
  if (!length(cap)) {
    cap <- get_chunk_opt("fig.cap", "tbl.cap", "cap", "tbl-cap")
  }
  
  # if there is no short_cap, get short_cap from chunk
  if (!length(short_cap)) {
    short_cap <- get_chunk_opt("fig.short_cap", "short_cap")
  }
  
  # if there is no cap but there is a short cap, re-assign cap
  if (!length(cap) && length(short_cap)) {
    cap <- short_cap
  }
  
  # if there is no short_cap but there is a cap, re-assign short_cap
  if (!length(short_cap) && length(cap)) {
    short_cap <- cap
  }
  
  # if global global_caption_index does NOT exist, create it
  if (!exists("global_caption_index")) {
    global_caption_index <<- NULL
  }
  
  # create data frame with label, caption, and short caption
  info <- data.frame(label = label, cap = cap, short_cap = short_cap)
  
  
  if (deb) {
    prn(info, fi = "/tmp/z")
  }
  
  # if there is nothing in global_caption_index OR
  # label is NOT in global_caption_index$label
  # if (!length(global_caption_index) || !label %in% global_caption_index$label) {
  # bind new info table to global captions
  global_caption_index <<- rbind(global_caption_index, info)
  # }
  invisible(list(label = label, cap = cap, short_cap = short_cap))
}


saveCap <- function(basename) {
  if (exists("global_caption_index")) {
    # cli::cli_inform(c("saving global index for ", basename))
    saveRDS(global_caption_index, file = paste0(basename, "-captions.rds"), compress = "xz")
  }
}


printCap <- function(book = FALSE) {
  if (book) {
    files <- list.files(pattern = "*-captions.rds")
    global_caption_index <- NULL
    for (f in files) global_caption_index <- rbind(global_caption_index, readRDS(f))
  }
  cap <- global_caption_index[c("label", "short_cap")]
  
  # use '@' to cross reference exact figure
  cap$label <- paste0("@", cap$label)
  names(cap) <- c("Figure", "Short Caption")
  if (book) {
    cap
  } # knitr::kable(cap, row.names=FALSE)
  else {
    knitr::kable(cap, row.names = FALSE, format = "html")
  }
}

cli::cli_inform(
  c("v" = "loaded file paths\n")
)
