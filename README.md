# tableone_AddOns
Two functions I use with the tableone package in R along with a Microsoft Word template that allows for page breaks with "#####".

A summary of the files:

* combine_overall_and_stratified_tables.R
  * Creates a list that contains a data.frame ("$Table") that combines the overall and stratified results.
  * Also includes other items in the list regarding missing observations and statistical methods.
  * Currently does not work if you do not include a "strata" argument. If you want just the overall or just the stratified table, then you can create the combined table and remove the unwanted columns.
  * Currently does not include the ability to conduct paired analyses (paired t-test, McNemar's, etc.) if the observations are correlated across the strata variable. I hope to add this soon.

* summarize_regression_models.R
  * Creates a data.frame containing the results of an lm, glm, or lme model object.
  * Requires the input of a data.frame containg the names of the variables included in the model along with the corresponding labels you want to display for those variables.
  * For categorical/factor variables with more than 2 levels, it will, by default, list all levels including the reference level. For 2-level categorical variables, it will only list the non-reference level so adjust the variable labels accordingly.
  * Currently does not work with model objects from the lme4 or survival packages.

* RMarkdown_WordTemplate_WideMargins.docx
  * A Microsoft Word document that was setup so that "#####" in the RMarkdown file creates a page break when markdowned to Word.
  * It also has narrower margins than what Word has by default and a non-blue theme.
