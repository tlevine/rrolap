#' Query a star schema.

#' @param fact.table Fact table name without the "fact_" prefix
cube <- function(fact.table)
  paste0('select %s from "fact_', fact.table, '" \nwhere')

#' Dice a query on the values in a dimension table
dice <- function(sql, dimension, subdimension, values) {
  labels <- paste(values, collapse = "', '")
  if (grepl('where$', sql))
    where <- ' "%s_id" in (select "id" from "dim_%s" where label in (\'%s\'))\n'
  else
    where <- '  and "%s_id" in (select "id" from "dim_%s" where label in (\'%s\'))\n'
  paste0(sql, sprintf(where, dimension, dimension, labels))
}

#' @param columns List of columns to select. This can include functions.
#' @example
#'   select('select %s from "fact_iris', c('count(*)', 'sum(Sepal.Length)'))
select <- function(sql, columns)
  paste0(sprintf(sql, paste(columns, collapse = ', ')), ';\n')


example <- function() {
  cube('iris') %>%
    dice('species', c('label', 'virginica')) %>%
    select('Sepal.Length') -> subcube
  subcube
}
