source('R/_project_directory.R')

db_connect <- function(db_name = db_name_src) {
    if (file.exists(paste0(ba_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'))) {
      message("Connecting using BA path")
      try(aws <- read.table(
        paste0(ba_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'),
        sep = '\t', header = TRUE), 
        silent=TRUE)
    } else if (file.exists(paste0(js_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'))) {
      message("Connecting using JS path")
      try(aws <- read.table(
        paste0(js_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'),
        sep = '\t', header = TRUE), 
        silent=TRUE)
    } else if (file.exists(paste0(rs_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'))) {
      message("Connecting using JS path")
      try(aws <- read.table(
        paste0(rs_company_path,'Admin_Gen_Mktg/Software/AWS/aws_credentials.txt'),
        sep = '\t', header = TRUE), 
        silent=TRUE)
    } else {
      message('okay, well, you are not connecting to the database')
    }
  
    con = dbConnect(drv=PostgreSQL(), 
                    user=aws$val[aws$fieldname=="user"], 
                    password=aws$val[aws$fieldname=="pw"],
                    host=aws$val[aws$fieldname=="host"], 
                    port=aws$val[aws$fieldname=="port"], 
                    dbname=db_name)
    
    message(paste0("you're connected to: ", db_name))
    
    return(con)
  }


cli::cli_inform(
  c("v" = "loaded db connection\n")
)