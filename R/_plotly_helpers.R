# rm(plotly_layout)
# if (exists("plotly_layout") == FALSE) {

plotly_margin <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 80,
  pad = 0.5
)

  # table_styling <- c("striped", "hover")
  
  plotly_layout <- function(a_plotly,
                            x_title = "",
                            y_title = "",
                            main_title = "",
                            subtitle = "",
                            legend_title = "") {
    a_plotly %>%
      plotly::layout(
        margin = plotly_margin,
        barmode = "group",
        # title -----
        title = list(
          text = main_title,
          font = list(
            family = "Arial Narrow",
            size = 24
          ),
          align = "left",
          x = 0,
          y = 1.15,
          xref = "paper",
          yref = "paper"
        ),
        annotations = list(
          # subtitle -----
          list(
            text = subtitle,
            font = list(
              family = "Arial Narrow",
              size = 16
            ),
            align = "left",
            x = 0,
            y = 1.08,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          )
        ),
        # legend -----
        legend = list(
          title = list(
            text = legend_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          ),
          font = list(
            family = "Arial Narrow",
            size = 14
          )
        ),
        # yaxis -----
        yaxis = list(
          title = list(
            text = y_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          ),
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          )
        ),
        ## yaxis2 -----
        yaxis2 = list(
          title = list(
            text = y_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          ),
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          )
        ),
        ## yaxis3 ----
        yaxis3 = list(
          title = list(
            text = y_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          ),
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          )
        ),
        ## yaxis4 ----
        yaxis4 = list(
          title = list(
            text = y_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          ),
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          )
        ),
        # xaxis -----
        xaxis = list(
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          ),
          title = list(
            text = x_title,
            font = list(
              family = "Arial Narrow",
              size = 20
            )
          )
        ),
        ## xaxis2 -----
        xaxis2 = list(
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          ),
          title = list(
            text = x_title,
            font = list(
              family = "Arial Narrow",
              size = 24
            )
          )
        ),
        ## xaxis3 -----
        xaxis3 = list(
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          ),
          title = list(
            text = x_title,
            font = list(
              family = "Arial Narrow",
              size = 24
            )
          )
        ),
        ## xaxis4 -----
        xaxis4 = list(
          tickfont = list(
            family = "Arial Narrow",
            size = 16
          ),
          title = list(
            text = x_title,
            font = list(
              family = "Arial Narrow",
              size = 24
            )
          )
        ),
        
        # hover mode ----
        hovermode = "closest",
        hoverdistance = "10",
        hoverlabel = list(
          font = list(
            size = 12,
            family = "Arial Narrow",
            color = 'white'
          ),
          # bgcolor = "white",
          stroke = list(
            'grey',
            'grey',
            'grey',
            'grey'
          ),
          padding = list(l = 5, r = 5, b = 5, t = 5)
        )
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "drawopenpath", "lasso",
          "editInChartStudio", "sendDataToCloud",
          "zoom2d", "pan2d"
        )
      )
  }
  
  
  # save_plotly <- function(a_plotly, fmt = c("png", "pdf"),
  #                         file_title = c("source", "title")) {
  #   pl_titles <-
  #     purrr::map(
  #       1:length(a_plotly$x$layoutAttrs),
  #       function(x) {
  #         a_plotly$x$layoutAttrs[[x]]$title$text
  #       }
  #     )
  #   
  #   pl_title <- pl_titles %>%
  #     rlist::list.clean() %>%
  #     rlist::list.filter(. != "") %>%
  #     unlist() %>%
  #     stringr::str_replace_all("[^[:alnum:]]", " ")
  #   
  #   # browser()
  #   pl_source <-
  #     purrr::map(
  #       1:length(a_plotly$x$layoutAttrs),
  #       function(x) {
  #         a_plotly$x$source
  #       }
  #     ) %>%
  #     unique() %>%
  #     unlist()
  #   
  #   file_name <- if (file_title == "source") {
  #     paste0(
  #       "plotly_", fmt, "/",
  #       pl_source, ".", fmt
  #     )
  #   } else if (file_title == "title") {
  #     paste0(
  #       "plotly_", fmt, "/",
  #       pl_title, ".", fmt
  #     )
  #   }
  #   
  #   scope <- kaleido()
  #   
  #   scope$transform(
  #     p = a_plotly,
  #     file = file_name
  #   )
  #   
  #   if (fmt == "pdf") {
  #     Sys.sleep(1)
  #     scope$transform(
  #       p = a_plotly,
  #       file = file_name
  #     )
  #   }
  # }
  
  
  cli::cli_inform(
    c("v" = "loaded plotly layout\n")
  )

