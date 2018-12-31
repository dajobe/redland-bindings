import std.stdio;
//import rdf.auxiliary.handled_record;

void main() {
    import rdf.raptor.world;
    RaptorWorld defaultWorld = RaptorWorld.create();
    defaultWorld.open();
//     RaptorWorld defaultWorld = RaptorWorld.create_and_open();
//     RaptorWorld worldWithSomeFlags =
//         RaptorWorld.create_and_open([FlagAndValue(RaptorFlagType.URIInterning, false)]);

    writeln("Tests passed.");
}
