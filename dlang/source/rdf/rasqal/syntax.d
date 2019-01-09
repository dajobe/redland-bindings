module rdf.rasqal.syntax;

import std.string;
import rdf.rasqal.world;
import rdf.raptor.syntax;

private extern extern(C) {
    const(SyntaxDescription*) rasqal_world_get_query_language_description(
        RasqalWorldHandle* world,
        uint counter);
    const(SyntaxDescription*) rasqal_world_get_query_results_format_description(
        RasqalWorldHandle* world,
        uint counter);
}

ref const(SyntaxDescription) getQueryLanguageDescription(RasqalWorldWithoutFinalize world,
                                                        uint counter)
{
    return *rasqal_world_get_query_language_description(world.handle, counter);
}

ref const(SyntaxDescription) getQueryResultsFormatDescription(RasqalWorldWithoutFinalize world,
                                                             uint counter)
{
    return *rasqal_world_get_query_results_format_description(world.handle, counter);
}

// TODO: Stopped at Query_Language_Description_Cursor
