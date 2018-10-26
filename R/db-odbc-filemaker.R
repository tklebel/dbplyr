# sql_ generics --------------------------------------------

#' @export
sql_select.FileMaker <- function(con, select, from,
                              where = NULL,  group_by = NULL,
                              having = NULL, order_by = NULL,
                              limit = NULL,  distinct = FALSE, ...) {

  out <- vector("list", 7)
  names(out) <- c("select",  "from",   "where",
                  "group_by","having", "order_by", "limit")

  assert_that(is.character(select), length(select) > 0L)

  out$select <- build_sql(

    "SELECT ",

    if (distinct) sql("DISTINCT "),

    # Todo: limiting the query works differently than in base SQL

    escape(select, collapse = ", ", con = con)
  )

  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)


  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @export
sql_translate_env.FileMaker <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,

                   as.character  = sql_prefix("STRVAL"),
                   as.Date       = sql_prefix("DATEVAL"),
                   as.numeric    = sql_prefix("NUMVAL"),

                   Sys.Date      = sql_prefix("DATE"),

                   ceiling       = sql_prefix("CEILING"),
                   exp           = sql_prefix("EXP"),
                   floor         = sql_prefix("FLOOR"),
                   log           = sql_prefix("LOG"),
                   sqrt          = sql_prefix("SQRT"),
                   abs           = sql_prefix("ABS")),


    sql_translator(.parent = base_odbc_agg,

                   mean          = sql_prefix("AVG"),
                   sd            = sql_prefix("STDEV"),
                   var           = sql_prefix("VAR"),
                   max           = sql_prefix("MAX"),
                   min           = sql_prefix("MIN"),
                   sum           = sql_prefix("SUM")),

    # Window functions not supported in Filemaker
    sql_translator(.parent = base_no_win)

  )}

# db_ generics -----------------------------------

#' @export
db_analyze.FileMaker <- function(con, table, ...) {
  # Do nothing. Filemaker doesn't support an analyze / update statistics function
}

db_explain.FileMaker <- function(con, sql, ...) {
  # Do nothing
}

# Util -------------------------------------------
