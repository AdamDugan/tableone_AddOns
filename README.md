# tableone_AddOns
Two functions I use with the tableone package in R and a Microsoft Word template that allows for page breaks with "#####".

A summary of the files:

* combine_overall_and_stratified_tables.R
  * Creates a list that contains a data.frame ("$Table") that combines the overall and stratified results.
  * Also includes other items in the list regarding missing observations.
  * Currently does not work if you do not include a "strata" argument. If you want just an overall table, then maybe just use the tableone functions.

* summarize_regression_models.R
  * Creates a data.frame containing the results of an lm or lme model object.
  * Requires the input of a dataframe containg the names of the variables included in the model along with the corresponding labels you want to display for those variables.
  * Does not work with model objects from the lme4 package.

* RMarkdown_WordTemplate_WideMargins.docx
  * A Microsoft Word document that was setup so that "#####" in the RMarkdown file creates a pagebreak when markdowned to Word.
  * It also has narrower margins than the Word default.




