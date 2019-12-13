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
    int rasqal_language_name_check(RasqalWorldHandle* world, const char *name);
}

const(SyntaxDescription*) getQueryLanguageDescription(const RasqalWorldWithoutFinalize world,
                                                      uint counter)
{
    return rasqal_world_get_query_language_description(world.handle, counter);
}

const(SyntaxDescription*) getQueryResultsFormatDescription(const RasqalWorldWithoutFinalize world,
                                                           uint counter)
{
    return rasqal_world_get_query_results_format_description(world.handle, counter);
}

struct QueryLanguageIterator {
private:
    RasqalWorldWithoutFinalize _world;
    uint _counter = 0;
public:
    this(RasqalWorldWithoutFinalize world) {
        _world = world;
    }
    bool empty() const {
        return !getQueryLanguageDescription(_world, _counter);
    }
    ref const(SyntaxDescription) front() const {
        return *getQueryLanguageDescription(_world, _counter);
    }
    void popFront() {
        ++_counter;
    }
}

struct QueryResultsFormatIterator {
private:
    RasqalWorldWithoutFinalize _world;
    uint _counter = 0;
public:
    this(RasqalWorldWithoutFinalize world) {
        _world = world;
    }
    bool empty() const {
        return !getQueryResultsFormatDescription(_world, _counter);
    }
    ref const(SyntaxDescription) front() const {
        return *getQueryResultsFormatDescription(_world, _counter);
    }
    void popFront() {
        ++_counter;
    }
}

bool languageNameCheck(RasqalWorldWithoutFinalize world, string name) {
    return rasqal_language_name_check(world.handle, name.toStringz) != 0;
}

unittest {
    RasqalWorld world = RasqalWorld.createAndOpen();
    for(auto i = QueryLanguageIterator(world); !i.empty; i.popFront()) { }
    for(auto i = QueryResultsFormatIterator(world); !i.empty; i.popFront()) { }
}

