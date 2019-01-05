module rdf.raptor.syntax;

import std.string;
import rdf.raptor.world;

enum SyntaxBitflags { Need_Base_URI = 1 }

// FIXME: D style IDs

extern(C) struct Mime_Type_Q {
private:
    char* _mimeType;
    size_t _mimeTypeLen;
    char _Q;
public:
    @property string mimeType() { return _mimeType[0.._mimeTypeLen].idup; }
    @property byte Q() { return _Q; }
}

extern(C) struct Syntax_Description_Record {
private:
    char** Names;
    uint Names_Count;
    char* Label;
    Mime_Type_Q* Mime_Types;
    uint Mime_Types_Count;
    char** URI_Strings;
    uint URI_Strings_Count;
    SyntaxBitflags Flags;
public:
    @disable this(this);
    string name(uint index)
        in { assert(index < namesCount); }
        do {
            return (*(Names + index)).fromStringz.idup;
        }
    @property uint namesCount() { return Names_Count; }
    @property string label() { return Label.fromStringz.idup; }
    ref const mimeTypeInfo(uint index)
        in { assert(index < Mime_Types_Count); }
        do {
            return *(Mime_Types + index); 
        }
    @property uint mimeTypesCount() { return Mime_Types_Count; }
    string uri(uint index)
        in { assert(index < URI_Strings_Count); }
        do {
            return (*(URI_Strings + index)).fromStringz.idup;
        }
    @property uint urisCount() { return URI_Strings_Count; }
    @property SyntaxBitflags flags() { return Flags; }
}

struct ParserDescriptionIterator {
private:
    RaptorWorldWithoutFinalize _world;
    uint _pos;
}

struct SerializerDescriptionIterator {
private:
    RaptorWorldWithoutFinalize _world;
    uint _pos;
}


// TODOL Stopped at Parser_Description_Cursor
