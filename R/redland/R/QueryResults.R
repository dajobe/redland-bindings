#
#   This work was created by the National Center for Ecological Analysis and Synthesis.
#
#     Copyright 2015 Regents of the University of California
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

#' @title A Redland QueryResults object is used to inspect query results from a Query object.
#' @description The QueryResults object contains the RDF statements that were returned from
#' a query on an RDF model.
#' @slot librdf_query_results A redland query object
#' @rdname QueryResults-class
#' @aliases QueryResults
#' @include redland.R
#' @keywords classes
#' @export
#' @section Methods:
#' \itemize{
#'   \item{\code{\link{QueryResults-initialize}}}{: Initialize a QueryResults object.}
#'   \item{\code{\link{freeQueryResults}}}{: Free memory used by a librdf query result.}
#' }
#' @seealso \code{\link{redland}}{: redland package}
setClass("QueryResults", slots = c(librdf_query_results = "_p_librdf_query_results"))

#' Initialize the QueryResults object.
#' @description The QueryResults object is initialized with the librdf query result from
#' return value of \code{'Query.execute()'}.
#' @details A QueryResults object is returned by the \code{Query.executeQuery()} method, so typically a user
#' does not initialize a QueryResult object by calling \code{new("QueryResult", ...)}
#' @rdname QueryResults-initialize
#' @aliases QueryResults-initialize
#' @param .Object the QueryResults object.
#' @param results a librdf query result
#' @return the QueryResults object
#' @export
setMethod("initialize", signature = "QueryResults", definition = function(.Object, results) {
  .Object@librdf_query_results <- results     
  return(.Object)
})

#' Free memory used by a librdf query results
#' @description After this method is called, the QueryResults object is no longer usable and should
#' be deleted with \code{"rm(query)"}.
#' @rdname freeQueryResults
#' @param .Object a QueryResults object
#' @export
setGeneric("freeQueryResults", function(.Object) {
  standardGeneric("freeQueryResults")
})

#' @rdname freeQueryResults
setMethod("freeQueryResults", signature("QueryResults"), function(.Object) {
  # Have to free all of the nodes that were created by the query result that
  # hold the bound node values
  if (!is.null(.Object@librdf_query_results) && librdf_query_results_finished(.Object@librdf_query_results) == 0) {
    num_nodes <- librdf_query_results_get_bindings_count(.Object@librdf_query_results)
    for (i in 1:num_nodes-1) {
      binding_name <- librdf_query_results_get_binding_name(.Object@librdf_query_results, i)
      val = librdf_query_results_get_binding_value(.Object@librdf_query_results, i)    
      if (!is.null.externalptr(val@ref)) {
        librdf_free_node(val)
      } 
    }
  }
  
  librdf_free_query_results(.Object@librdf_query_results)
})
