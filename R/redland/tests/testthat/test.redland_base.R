context("librdf base API tests")
test_that("redland library loads", {
    library(redland)
    # Add a line to this test to prevent 'Empty test' (skipped) msg.
    expect_true(require(redland))
})
test_that("librdf basic functions", {
    library(redland)
    world <- librdf_new_world();
    expect_match(class(world), "_p_librdf_world_s")
    storage <- librdf_new_storage(world,'hashes','dummy',"new=yes,hash-type='memory'")
    expect_match(class(storage), "_p_librdf_storage_s")

    model <- librdf_new_model(world,storage,'')
    expect_match(class(model), "_p_librdf_model_s")

    parser <- librdf_new_parser(world,'rdfxml','application/rdf+xml',NULL)
    expect_match(class(parser), "_p_librdf_parser_s")
    uri <- librdf_new_uri(world,paste0('file:',system.file('extdata/dc.rdf', package='redland')))
    expect_match(class(uri), "_p_librdf_uri_s")
    rv <- librdf_parser_parse_into_model(parser,uri,uri,model)
    expect_that(rv, equals(0))
    librdf_free_uri(uri);
    librdf_free_parser(parser);

    query <- librdf_new_query(world, 'sparql', NULL, "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?a ?c ?d WHERE { ?a dc:title ?c . OPTIONAL { ?a dc:related ?d } }", NULL)
    results <- librdf_model_query_execute(model, query);
    expect_false(is.null(results))
    expect_match(class(results), "_p_librdf_query_results")

    # Convert the whole sparql result to a string and check its value
    qstr <- librdf_query_results_to_string2(results, "rdfxml", "application/rdf+xml", NULL, NULL)
    expect_match(qstr, "http://www.dajobe.org/")
    expect_match(qstr, "Beckett")

    # Test adding a new Statement to the model
    about <- "http://matt.magisa.org/"
    subject <- librdf_new_node_from_uri_string(world, about)
    expect_false(is.null(subject))
    expect_match(class(subject), "_p_librdf_node_s")
    pred <- librdf_new_node_from_uri_string(world, "http://purl.org/dc/elements/1.1/title")
    expect_false(is.null(pred))
    expect_match(class(pred), "_p_librdf_node_s")
    object <- librdf_new_node_from_literal(world, "Matt Jones' Home Page", "", 0)
    expect_false(is.null(object))
    expect_match(class(object), "_p_librdf_node_s")
    statement <- librdf_new_statement_from_nodes(world, subject, pred, object)
    expect_false(is.null(statement))
    expect_match(class(statement), "_p_librdf_statement_s")

    rc <- librdf_model_add_statement(model, statement)
    expect_that(rc, equals(0))

    # Don't need to call librdf_free_node, just librdf_free_statement.
    librdf_free_statement(statement)

    # Test serialization of the model to a text file
    serializer <- librdf_new_serializer(world, "rdfxml", "", NULL);
    expect_false(is.null(serializer))
    expect_match(class(serializer), "_p_librdf_serializer")
    base = librdf_new_uri(world, "http://example.org/base.rdf");
    filePath <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".rdf")

    librdf_serializer_serialize_model_to_file(serializer,filePath,base,model);
    expect_true(file.exists(filePath))
    unlink(filePath)

    # Free resources
    librdf_free_serializer(serializer);
    librdf_free_uri(base);
    librdf_free_model(model);
    librdf_free_storage(storage);
    expect_that("Reached end.", equals("Reached end."))
})
