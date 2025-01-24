context("World tests")
test_that("redland library loads", {
    library(redland)
    # Add a line to this test to prevent 'Empty test' (skipped) msg.
    expect_true(require(redland))
})
test_that("World constructor", {
    library(redland)
    world <- new("World");
    expect_match(class(world), "World")
    expect_match(class(world@librdf_world), "_p_librdf_world_s")
    err <- try(freeWorld(world), silent=TRUE)
    expect_false(class(err) == "try-error")
    
})