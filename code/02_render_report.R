here::i_am("code/02_render_report.R")

library(rmarkdown)
render(
  here::here("final_report.Rmd"),
  knit_root_dir = here::here()
)