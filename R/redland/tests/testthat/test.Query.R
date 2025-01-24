context("Query tests")
test_that("redland library loads", {
  library(redland)
  # Add a line to this test to prevent 'Empty test' (skipped) msg.
  expect_true(require(redland))
})
test_that("Query works", {
  library(redland)
  world <- new("World")
  expect_false(is.null(world))
  
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world, storage, options="")

  # Literal for object node has RDF language tag
  stmt <- new("Statement", 
            world = world,  
            subject="", 
            predicate="http://schema.org/name", 
            object="Maëlle Salmon",
            language="fr")
  addStatement(model, stmt)

  queryString <-'SELECT ?s ?p ?o WHERE { ?s ?p ?o}'
  query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
  results <- getResults(query, model, "turtle")
  expect_true(grepl("http://schema.org/name", results))

  # The Redlands library can return UTF characters that are outside the
  # ASCII range as the unicode literal string, i.e.  https://en.wikipedia.org/wiki/List_of_Unicode_characters.
  # If 'stringi' is available, then check the original representation, otherwise check the escaped codepoint
  # representation.
  if(require("stringi", character.only = TRUE)) {
      expect_match(stringi::stri_unescape_unicode(results), "Maëlle Salmon")
  } else {
      expect_match(results$o, '"Ma\\\\u00EBlle Salmon"@fr')
  }
  
  freeQuery(query)
  rm(query)
  freeStorage(storage)
  rm(storage)
  freeModel(model)
  rm(model)
  freeWorld(world)
  rm(world)
    
})


test_that("getResults, writeResults work", {
  library(redland)
  world <- new("World")
  expect_false(is.null(world))
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world, storage, options="")
  stmt <- new("Statement", world=world, 
     subject="https://orcid.org/0000-0002-2192-403X",
     predicate="http://www.w3.org/ns/prov#Agent",
     object="slaughter", 
     objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
  status <- addStatement(model, stmt)
  queryString <- paste("PREFIX orcid: <https://orcid.org/>",
                        "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
                        "PREFIX prov: <http://www.w3.org/ns/prov#>",
                        "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
  query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
  # Return all results as a string
  results <- getResults(query, model, "rdfxml")
  expect_true(grepl("0000-0002-2192-403X", results))
  expect_true(grepl("slaughter", results))
  
  results <- getResults(query, model, "turtle")
  expect_true(grepl("slaughter", results))
  expect_true(grepl("0000-0002-2192-403X", results))
  
  results <- getResults(query, model, "json")
  expect_true(grepl("slaughter", results))
  expect_true(grepl("0000-0002-2192-403X", results))
   
  # When the query object is no longer needed, the resources it had allocated can be freed.
  freeQuery(query)
  rm(query)
  
  # Test writeResults
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world, storage, options="")
  stmt <- new("Statement", world=world, 
     subject="https://orcid.org/0000-0002-2192-403X",
     predicate="http://www.w3.org/ns/prov#Agent",
     object="slaughter", 
     objectType="literal", datatype_uri="http://www.w3.org/2001/XMLSchema#string")
  status <- addStatement(model, stmt)
  queryString <- paste("PREFIX orcid: <https://orcid.org/>",
                        "PREFIX dataone: <https://cn.dataone.org/cn/v1/resolve/>",
                        "PREFIX prov: <http://www.w3.org/ns/prov#>",
                        "SELECT ?a ?c WHERE { ?a prov:Agent ?c . }", sep=" ")
  query <- new("Query", world, queryString, base_uri=NULL, query_language="sparql", query_uri=NULL)
  # Return all results as a string
  tf <- tempfile()
  writeResults(query, model, file=tf, mimeType="application/x-turtle")
  inbuf <- readLines(tf)
  expect_true(any(grepl("slaughter", inbuf)))
  expect_true(any(grepl("0000-0002-2192-403X", inbuf)))
  
  # Ntriples
  tf <- tempfile()
  writeResults(query, model, file=tf, mimeType="text/plain")
  inbuf <- readLines(tf)
  expect_true(any(grepl("slaughter", inbuf)))
  expect_true(any(grepl("0000-0002-2192-403X", inbuf)))
  
  tf <- tempfile()
  writeResults(query, model, file=tf, mimeType="application/json")
  inbuf <- readLines(tf)
  expect_true(any(grepl("slaughter", inbuf)))
  expect_true(any(grepl("0000-0002-2192-403X", inbuf)))
  
  tf <- tempfile()
  writeResults(query, model, file=tf, mimeType="text/html")
  inbuf <- readLines(tf)
  expect_true(any(grepl("slaughter", inbuf)))
  expect_true(any(grepl("0000-0002-2192-403X", inbuf)))
  
  # When the query object is no longer needed, the resources it had allocated can be freed.
  freeQuery(query)
  rm(query)
  freeStorage(storage)
  rm(storage)
  freeModel(model)
  rm(model)
  freeWorld(world)
  rm(world) 
})