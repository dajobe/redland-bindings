context("Parser tests")
test_that("redland library loads", {
  library(redland)
  # Add a line to this test to prevent 'Empty test' (skipped) msg.
  expect_true(require(redland))
})
test_that("Parser constructor", {
  library(redland)
  world <- new("World")
  expect_false(is.null(world))

  # Test creating the Storage system
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  expect_false(is.null(storage))
  expect_match(class(storage@librdf_storage), "_p_librdf_storage_s")

  # Test creating the Model
  model <- new("Model", world, storage, options="")
  expect_false(is.null(model))
  expect_match(class(model@librdf_model), "_p_librdf_model_s")

  # Test that model creation fails if world is not provided or is null
  err <- try(model <- new("Model", world=NULL, storage, options=""), silent=TRUE)
  expect_match(class(err), "try-error")

  expect_false(is.null(model))
  expect_match(class(model@librdf_model), "_p_librdf_model_s")

  # Test parsing an RDF document into a Model
  parser <- new("Parser", world)
  expect_false(is.null(parser))
  expect_match(class(parser@librdf_parser), "_p_librdf_parser_s")

  parseFileIntoModel(parser, world, system.file('extdata/example.rdf', package='redland'), model)

  # Test creating a Serializer and serializing the content just parsed into the model
  serializer <- new("Serializer", world)
  expect_false(is.null(serializer))
  expect_match(class(serializer@librdf_serializer), "_p_librdf_serializer_s")

  # Test performing a serialization on an RDF model
  rdf <- serializeToCharacter(serializer, world, model)
  expect_match(rdf, "John Smith")

  err <- try(freeModel(model), silent=TRUE)
  expect_false(class(err) ==  "try-error")

  err <- try(freeStorage(storage), silent=TRUE)
  expect_false(class(err) == "try-error")

  err <- try(freeParser(parser), silent=TRUE)
  expect_false(class(err) == "try-error")

  err <- try(freeSerializer(serializer), silent=TRUE)
  expect_false(class(err) == "try-error")

  err <- try(freeWorld(world), silent=TRUE)
  expect_false(class(err) == "try-error")
})
