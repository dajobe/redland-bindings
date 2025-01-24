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

#' @title Query an RDF model
#' @description The Query class is used to execute a query on a Model object using the default query
#' language SPARQL. For more information, please refer to \url{http://librdf.org/rasqal/} for details on 
#' supported query languages.
#' @details A Query is executed using the executeQuery method, which returns a QueryResults object that
#' can be iterated over the query solution sequence.
#' @slot librdf_query A redland query object
#' @slot librdf_world A redland world object
#' @rdname Query-class
#' @aliases Query
#' @include redland.R
#' @keywords classes
#' @export
#' @references www.example.com
#' @section Methods:
#' \itemize{
#'   \item{\code{\link{Query-initialize}}}{: Initialize a Query object.}
#'   \item{\code{\link{executeQuery}}}{: Execute a query.}
#'   \item{\code{\link{setQueryResultLimit}}}{: Set limit on returned query results.}
#'   \item{\code{\link{getQueryResultLimit}}}{: Get the query result limit.}
#'   \item{\code{\link{getResults}}}{: Return all query results.}
#'   \item{\code{\link{writeResults}}}{: Write query results to a file.}
#'   \item{\code{\link{freeParser}}}{: Free memory used by a librdf query.}
#' }
#' @seealso \code{\link{redland}}{: redland package}
#' @examples
#' world <- new("World")
#' storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
#' model <- new("Model", world, storage, options="")
#' stmt <- new("Statement", world=world, 
#'   subject="https://cn.dataone.org/cn/v1/resolve/urn:uuid:274a0c5c-3082-4562-bbd3-2b1288768cac",
#'   predicate="http://www.w3.org/ns/prov#hadPlan",
#'   object="https://cn.dataone.org/cn/v1/resolve/urn:uuid:01305f45-f22b-40c8-8d27-00357d01e4a5")
#' status <- addStatement(model, stmt)
#' stmt <- new("Statement", world=world, 
#'            subject="https://orcid.org/0000-0002-2192-403X",
#'            predicate="http://www.w3.org/ns/prov#Agent",
#'            object="slaughter", 
#'            objectType="literal", 
#'            datatype_uri="http://www.w3.org/2001/XMLSchema#string")
#' status <- addStatement(model, stmt)
#' queryString <- 
#'     paste("PREFIX orcid: <https://orcid.org/>",
#'           "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
#'           "PREFIX prov: <http://www.w3.org/ns/prov#>",
#'           "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
#' query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
#' # Return all results as a string
#' results <- getResults(query, model, "rdfxml")
#' 
setClass("Query", slots = c(librdf_query = "_p_librdf_query", librdf_world = "_p_librdf_world_s"))

#' Initialize the Query object.
#' @rdname Query-initialize
#' @aliases Query-initialize
#' @param .Object the Query object
#' @param world a World object
#' @param querystring a query string for the language specified in 'query_language'
#' @param base_uri a URI to prepend to relative URI in the document
#' @param query_language the query language to execute the querystring with
#' @param query_uri a URI to prepend to terms in the query
#' @return the Query object
#' @export
setMethod("initialize",
          signature = "Query",
          definition = function(.Object, world, querystring, base_uri=NULL, query_language="sparql", query_uri=NULL) {

  stopifnot(!is.null(querystring))
  .Object@librdf_query <- librdf_new_query(world@librdf_world, query_language, base_uri, querystring, query_uri)
  .Object@librdf_world <- world@librdf_world
  
  return(.Object)
})

#' Execute a query
#' @description The initialize query is executed and the result is returned as a QueryResult object
#' @rdname executeQuery
#' @param .Object a Query object
#' @param model a Model object containing the statements to query
#' @return a QueryResults object
#' @export
setGeneric("executeQuery", function(.Object, model) {
  standardGeneric("executeQuery")
})

#' @rdname executeQuery
setMethod("executeQuery", signature("Query"), function(.Object, model) {
  results <- librdf_query_execute(.Object@librdf_query, model@librdf_model) 
  queryResultObj <- new("QueryResults", results)
  return(queryResultObj)
})

#' Set limit on returned query results
#' @rdname setQueryResultsLimit
#' @aliases setQueryResultsLimit
#' @param .Object a Query object
#' @param limit the result set limit. Specify a value >= to have a limit, or a value < 0 to have no limit.
#' @export
setGeneric("setQueryResultLimit", function(.Object, limit) {
  standardGeneric("setQueryResultLimit")
})

#' @rdname setQueryResultsLimit
setMethod("setQueryResultLimit", signature("Query"), function(.Object, limit) {
  librdf_query_set_limit(.Object@librdf_query, as.integer(limit))
})

#' Get the query result limit
#' @rdname getQueryResultsLimit
#' @param .Object a Query object
#' @return the query result limit. If a limit is set then the value will be >= 0. If the value is < 0, no limit is set
#' @export
setGeneric("getQueryResultLimit", function(.Object) {
  standardGeneric("getQueryResultLimit")
})

#' @rdname getQueryResultsLimit
setMethod("getQueryResultLimit", signature("Query"), function(.Object) {
  return(librdf_query_get_limit (.Object@librdf_query))
})

#' Return all query results
#' @details After this method is called, the Query object is no longer usable and should
#' be deleted \code{"rm(query)"} and a new object created.
#' @rdname getResults
#' @param .Object a Query object
#' @param model a Model object
#' @param formatName a string specifying the RDF format name. Currently the supported formats are "rdfxml" ,"turtle", "json", "csv"
#' @examples
#' world <- new("World")
#' storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
#' model <- new("Model", world, storage, options="")
#' stmt <- new("Statement", world=world, 
#'   subject="https://orcid.org/0000-0002-2192-403X",
#'   predicate="http://www.w3.org/ns/prov#Agent",
#'   object="slaughter", 
#'   objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
#'   #objectType="literal", language="en")
#' status <- addStatement(model, stmt)
#' queryString <- paste("PREFIX orcid: <https://orcid.org/>",
#'                      "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
#'                      "PREFIX prov: <http://www.w3.org/ns/prov#>",
#'                      "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
#' query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
#' # Return all results as a string
#' results <- getResults(query, model, "rdfxml")
#' results <- getResults(query, model, "turtle")
#' results <- getResults(query, model, "json")
#' 
#' # When the query object is no longer needed, the resources it had allocated can be freed.
#' freeQuery(query)
#' rm(query)
#' @export
#' @param ... additional parameters
setGeneric("getResults", function(.Object, model, ...) {
  standardGeneric("getResults")
})

#' @rdname getResults
setMethod("getResults", signature("Query"), function(.Object, model, formatName="rdfxml") {
  queryResult <- executeQuery(.Object, model)
  if (!is.null(queryResult@librdf_query_results) && librdf_query_results_finished(queryResult@librdf_query_results) == 0) {
    # It appears that this redland C library function doesn't allow specifying 'formatName' as "" or NULL, so the next two
    # arguments can never be used. Also, a text value has to be specified for the 3rd argument (mimeType) otherwise the function crashes.
    if(is.null(formatName) || formatName == "") formatName="rdfxml"
    results <- librdf_query_results_to_string2(queryResult@librdf_query_results, formatName, "application/rdf+xml", NULL, NULL);
    return(results)
  } else {
    return(NULL)
  }
})

#' Write query results to a file.
#' @details After this method is called, the Query object is no longer usable and should
#' be deleted \code{"rm(query)"} and a new object created.
#' @rdname writeResults
#' @param .Object a Query object
#' @param model a Model object
#' @param ... additional parameters
#' @examples
#' world <- new("World")
#' storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
#' model <- new("Model", world, storage, options="")
#' stmt <- new("Statement", world=world, 
#'   subject="https://orcid.org/0000-0002-2192-403X",
#'   predicate="http://www.w3.org/ns/prov#Agent",
#'   object="slaughter", 
#'   objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
#' status <- addStatement(model, stmt)
#' queryString <- paste("PREFIX orcid: <https://orcid.org/>",
#'                      "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
#'                      "PREFIX prov: <http://www.w3.org/ns/prov#>",
#'                      "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
#' query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
#' # Return all results as a string
#' tf <- tempfile()
#' writeResults(query, model, file=tf, mimeType="application/x-turtle")
#' 
#' # When the query object is no longer needed, the resources it had allocated can be freed.
#' freeQuery(query)
#' rm(query)
#' @export
setGeneric("writeResults", function(.Object, model, ...) {
  standardGeneric("writeResults")
})

#' @rdname writeResults
#' @param file a string specifying the output file
#' @param mimeType a string specifying the mimeType of the output file. Currently supported values are "application/x-turtle", "text/plain", "application/json", "text/html"
#' @param format_uri (not currently used)
#' @param base_uri (not currently used)
setMethod("writeResults", signature("Query"), function(.Object, model, file, mimeType="application/x-turtle", format_uri=NULL, base_uri=NULL) {
  queryResult <- executeQuery(.Object, model)
  if (!is.null(queryResult@librdf_query_results) && librdf_query_results_finished(queryResult@librdf_query_results) == 0) {
    results <- librdf_query_results_to_file2(queryResult@librdf_query_results, file, mimeType, format_uri, base_uri);
    return(results)
  } else {
    return(NULL)
  }
})

#' Free memory used by a librdf query
#' @details After this method is called, the Query object is no longer usable and should
#' be deleted \code{"rm(query)"} and a new object created.
#' @rdname freeQuery
#' @param .Object a Query object
#' @examples
#' world <- new("World")
#' storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
#' model <- new("Model", world, storage, options="")
#' stmt <- new("Statement", world=world, 
#'   subject="https://orcid.org/0000-0002-2192-403X",
#'   predicate="http://www.w3.org/ns/prov#Agent",
#'   object="slaughter", 
#'   objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
#' status <- addStatement(model, stmt)
#' queryString <- paste("PREFIX orcid: <https://orcid.org/>",
#'                      "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
#'                      "PREFIX prov: <http://www.w3.org/ns/prov#>",
#'                      "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
#' query <- new("Query", world, queryString, base_uri=NULL, 
#'   query_language="sparql", query_uri=NULL)
#' # Return all results as a string
#' results <- getResults(query, model, "rdfxml")
#' 
#' # When the query object is no longer needed, the resources it had allocated can be freed.
#' freeQuery(query)
#' rm(query)
#' @export
setGeneric("freeQuery", function(.Object) {
  standardGeneric("freeQuery")
})

#' @rdname freeQuery
setMethod("freeQuery", signature("Query"), function(.Object) {
  librdf_free_query(.Object@librdf_query)
})
