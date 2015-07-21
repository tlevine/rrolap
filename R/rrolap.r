#' Query a star schema.

#' @param fact.table Fact table name without the "fact_" prefix
cube <- function(fact.table)
  paste0('select %s from "fact_', fact.table, '" \nwhere')

#' Dice a query on the values in a dimension table
dice <- function(sql, dimension, subdimension, values) {
  labels <- paste(values, collapse = "', '")
  if (grepl('where$', sql))
    where <- ' "%s_id" in (select "id" from "dim_%s" where label in (\'%s\'))'
  else
    where <- '\nand "%s_id" in (select "id" from "dim_%s" where label in (\'%s\'))'
  paste0(sql, sprintf(where, dimension, dimension, labels))
}

#' @param columns List of columns to select. This can include functions.
#' @example
#'   select('select %s from "fact_iris', c('count(*)', 'sum(Sepal.Length)'))
select <- function(sql, columns) {
  columns.sql <- paste(columns, collapse = ', ')
  sub.sql <- sprintf(sql, columns)
  root.sql <- 'select %s "a" from (%s) join "dim_%s" "b" on a.%s_id = b.id
, ';\n'
  paste0(sprintf(root, table.a))
}

example <- function() {
  cube('iris') %>%
    dice('species', 'label', c('virginica')) %>%
    select('Sepal.Length') -> subcube
  subcube
}
