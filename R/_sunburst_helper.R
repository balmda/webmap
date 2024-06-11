
as.sunburstdf <- function(df, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesdf <- names(df)
  
  if(is.data.table(df)){
    dt <- copy(df)
  } else {
    dt <- data.table(df, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    dt[, root := "Total"]  
  }
  
  col_names_dt <- names(dt)
  hierarchy_columns <- setdiff(col_names_dt, value_column)
  dt[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(dt, c("root", colNamesdf))
  } else if(!is.null(value_column) && !add_root) {
    setnames(dt, value_column, "values", skip_absent=TRUE)
    setcolorder(dt, c(setdiff(colNamesdf, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(dt, value_column, "values", skip_absent=TRUE)
    setcolorder(dt, c("root", setdiff(colNamesdf, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- col_names_dt[1:i]
    if(is.null(value_column)){
      currentdt <- unique(dt[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentdt <- dt[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentdt, length(current_columns), "labels")
    hierarchyList[[i]] <- currentdt
  }
  
  hierarchydt <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchydt), c("labels", "values", value_column))
  hierarchydt[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchydt[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchydt[, c(parent_columns) := NULL]
  return(hierarchydt)
}


cli::cli_inform(
  c("v" = "loaded sunburst helpers\n")
)