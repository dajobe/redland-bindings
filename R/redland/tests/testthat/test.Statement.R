context("Statement tests")
test_that("redland library loads", {
  library(redland)
  # Add a line to this test to prevent 'Empty test' (skipped) msg.
  expect_true(require(redland))
})
test_that("Statement constructor", {
  library(redland)
  world <- new("World")
  expect_false(is.null(world))
  
  # Test creating Subject, predicate, and object Nodes
  subject <- new("Node", world, literal="subject")
  expect_match(class(subject@librdf_node), "_p_librdf_node_s")
  predicate <- new("Node", world, literal="subject")
  expect_match(class(predicate@librdf_node), "_p_librdf_node_s")
  object <- new("Node", world, literal="subject")
  expect_match(class(object@librdf_node), "_p_librdf_node_s")
  
  # Test creating the Statement
  stmt <- new("Statement", world, subject, predicate, object)
  expect_false(is.null(stmt))
  expect_match(class(stmt@librdf_statement), "_p_librdf_statement_s")
  
  # Test that statement creation fails if world is not provided or is null
  err <- try(stmt <- new("Statement", world=NULL, subject=subject, predicate=predicate, object=object), silent=TRUE)
  expect_match(class(err), "try-error")
  
  # Test that statement creation fails if subject, predicate, or object is not provided or is null
  err <- try(stmt <- new("Statement", world=world, subject=NULL, predicate=predicate, object=object), silent=TRUE)
  expect_match(class(err), "try-error")
  err <- try(stmt <- new("Statement", world=world, subject=subject, predicate=NULL, object=object), silent=TRUE)
  expect_match(class(err), "try-error")
  err <- try(stmt <- new("Statement", world=world, subject=subject, predicate=predicate, object=NULL), silent=TRUE)
  expect_match(class(err), "try-error")
  
  # Test statement creation when subject, predicate, object are passed in as character and RDF type is not specified
  stmt <- new("Statement", world=world, 
              subject="https://cn.dataone.org/cn/v1/resolve/resourceMap_5cbcdecd-6b0e-4b24-a0be-20291b2e49a7#aggregation",
              predicate="http://purl.org/dc/terms/identifier",
              object="resourceMap_5cbcdecd-6b0e-4b24-a0be-20291b2e49a7^^xsd:string")
              
  expect_match(getTermType(stmt, "subject"), "resource")
  expect_match(getTermType(stmt, "predicate"), "resource")
  expect_match(getTermType(stmt, "object"), "literal")
  
  # Test 
  stmt <- new("Statement", world=world, 
              subject="_:foo1",
              predicate="http://purl.org/dc/terms/identifier",
              object=NULL)
  
  expect_match(getTermType(stmt, "subject"), "blank")
  expect_match(getTermType(stmt, "predicate"), "resource")
  expect_match(getTermType(stmt, "object"), "blank")
  
  err <- try(freeStatement(stmt), silent=TRUE)
  expect_false(class(err) == "try-error")
  
  stmt <- new("Statement", world=world, 
              subject=NULL,
              predicate="http://purl.org/dc/terms/identifier",
              object="id1234")
  
  expect_match(getTermType(stmt, "subject"), "blank")
  expect_match(getTermType(stmt, "predicate"), "resource")
  expect_match(getTermType(stmt, "object"), "literal")
  
  err <- try(freeStatement(stmt), silent=TRUE)
  expect_false(class(err) == "try-error")
  
  # Test statement creation when subject, predicate and object are passed in as charater and RDF types are specified
  stmt <- new("Statement", world=world, 
              subject="http://www.exmaple.com/subject",
              predicate="http://purl.org/dc/terms/identifier",
              object="http://www.exmaple.com/object", subjectType="blank", objectType="literal")
  
  expect_match(getTermType(stmt, "subject"), "blank")
  expect_match(getTermType(stmt, "predicate"), "resource")
  expect_match(getTermType(stmt, "object"), "literal")
  
  err <- try(freeStatement(stmt), silent=TRUE)
  expect_false(class(err) == "try-error")
  err <- try(freeWorld(world), silent=TRUE)
  expect_false(class(err) == "try-error")
})