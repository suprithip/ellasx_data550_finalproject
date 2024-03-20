# DATA550 Final Project: Diabetes Predictors

------------------------------------------------------------------------

## File descriptions

`code/01_make_output1.R`

  - generates regression model table and boxplots
  - saves the table as `.rds` object and boxplots as `.png` in `output/` folder

`code/02_render_report.R`

  - renders `final_report.Rmd`

`final_report.Rmd`

  - reads regression table generated by `code/01_make_output1.R`
  - reads boxplots generated by `code/01_make_output1.R`

`makefile`

  - contains rules for building the report
  - `make .outputs` will generate the `.rds` and `.png` files needed to compile the report
  - `make report.html` will generate the final report
  - `make clean` will clean all outputs




