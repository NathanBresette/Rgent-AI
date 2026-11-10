\name{rstudioai-internal}
\alias{rstudioai-internal}
\alias{ai_insert_code}
\alias{ai_replace_code}
\alias{ai_smart_insert}
\alias{execute_r_function}
\alias{assemble_intelligent_context_safe}
\alias{capture_context}
\alias{detect_context_changes}
\alias{auto_capture_error}
\alias{capture_last_error}
\alias{get_console_context}
\alias{remove_error_monitoring}
\alias{test_auto_capture}
\alias{trigger_auto_capture}
\alias{execute_code}
\alias{execute_with_capture}
\alias{get_available_dataframes}
\alias{get_available_objects}
\alias{get_dataframe_info}
\alias{get_dataframe_explorer_data}
\alias{get_dataframe_chunk_filtered}
\alias{get_object_explorer_data}
\alias{convert_explorer_view_to_code}
\title{Functions the Rgent chat pane calls}
\description{
These functions are exported so the chat pane and its websocket server can call
them. They are not meant to be called directly; use \code{\link{run_rgent}}.
}
\usage{
ai_insert_code(code, context_context = NULL)
ai_replace_code(old_code_pattern, new_code)
ai_smart_insert(code, location_hint = NULL)
execute_r_function(function_name, param1, param2 = NULL)

assemble_intelligent_context_safe(query)
capture_context()
detect_context_changes(old_context, new_context)

auto_capture_error()
capture_last_error()
get_console_context()
remove_error_monitoring()
test_auto_capture()
trigger_auto_capture()

execute_code(code)
execute_with_capture(code)

get_available_dataframes()
get_available_objects()
get_dataframe_info(dataframe_name)
get_dataframe_explorer_data(df_name)
get_dataframe_chunk_filtered(df_name, page = 1, page_size = 100, filters = NULL,
  sorts = NULL, pinned_cols = NULL, search = NULL)
get_object_explorer_data(obj_name)
convert_explorer_view_to_code(df_name, filters = NULL, sorts = NULL, search = NULL,
  style = c("auto", "dplyr", "base"))
}
\arguments{
\item{code}{R code, as a character string.}
\item{context_context}{Optional text describing the code; when it mentions a
function, the code goes after the last function definition in the document,
otherwise at the top.}
\item{old_code_pattern}{A regular expression matching the line to replace in the
open document.}
\item{new_code}{The replacement code, as a character string.}
\item{location_hint}{Optional text such as \code{"top"}, \code{"end"},
\code{"after function"} or \code{"cursor"} saying where to insert the code; by
default it goes at the top.}
\item{function_name}{One of \code{"ai_insert_code"}, \code{"ai_replace_code"} or
\code{"ai_smart_insert"}.}
\item{param1, param2}{The first and second arguments passed to
\code{function_name}.}
\item{query}{The user's question, used to choose which workspace objects to describe.}
\item{old_context, new_context}{Two context lists as returned by
\code{capture_context()}, to compare.}
\item{dataframe_name, df_name, obj_name}{The name of an object in the global
environment, as a character string.}
\item{page, page_size}{Which page of rows to return, and how many rows a page has.}
\item{filters, sorts, search}{The Data Explorer's current filters, sort columns and
search text (lists as sent by the chat pane, or \code{NULL} for none).}
\item{pinned_cols}{Names of columns pinned to the left of the grid.}
\item{style}{The code to write: \code{"dplyr"}, \code{"base"}, or \code{"auto"} to
use dplyr when it is installed.}
}
\value{Lists and values for the chat pane, or \code{TRUE}/\code{FALSE} for whether
an action worked.}
\keyword{internal}
