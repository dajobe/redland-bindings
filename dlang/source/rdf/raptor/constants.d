module rdf.raptor.constants;

import std.string;

private extern extern(C) immutable __gshared uint raptor_version_major;
private extern extern(C) immutable __gshared uint raptor_version_minor;
private extern extern(C) immutable __gshared uint raptor_version_release;
private extern extern(C) immutable __gshared uint raptor_version_decimal;

alias version_major   = raptor_version_major;
alias version_minor   = raptor_version_minor;
alias version_release = raptor_version_release;
alias version_decimal = raptor_version_decimal;

private extern extern(C) {
    immutable char* raptor_copyright_string;
    immutable char* raptor_home_url_string;
    immutable char* raptor_license_string;
    immutable char* raptor_short_copyright_string;
    immutable char* raptor_version_string;
    immutable char* raptor_owl_namespace_uri;
    immutable char* raptor_rdf_namespace_uri;
    immutable char* raptor_rdf_schema_namespace_uri;
    immutable char* raptor_xml_literal_datatype_uri_string;
    immutable char* raptor_xml_namespace_uri;
    immutable char* raptor_xmlschema_datatypes_namespace_uri;
}

string copyright_string() { return raptor_copyright_string.fromStringz; }
string home_url_string() { return raptor_home_url_string.fromStringz; }
string license_string() { return raptor_license_string.fromStringz; }
string short_copyright_string() { return raptor_short_copyright_string.fromStringz; }
string version_string() { return raptor_version_string.fromStringz; }
string owl_namespace_uri() { return raptor_owl_namespace_uri.fromStringz; }
string rdf_namespace_uri() { return raptor_rdf_namespace_uri.fromStringz; }
string rdf_schema_namespace_uri() { return raptor_rdf_schema_namespace_uri.fromStringz; }
string xml_literal_datatype_uri_string() { return raptor_xml_literal_datatype_uri_string.fromStringz; }
string xml_namespace_uri() { return raptor_xml_namespace_uri.fromStringz; }
string xmlschema_datatypes_namespace_uri() { return raptor_xmlschema_datatypes_namespace_uri.fromStringz; }

extern extern(C) uint rdf_namespace_uri_len;
extern extern(C) uint xml_literal_datatype_uri_string_len;

enum DomainType : ubyte { None = 0,
                          IOStream = 1,
                          Namespace = 2,
                          Parser = 3,
                          Qname = 4,
                          Sax2 = 5,
                          Serializer = 6,
                          Term = 7,
                          Turtle_Writer = 8,
                          URI = 9,
                          World = 10,
                          WWW = 11,
                          XML_Writer = 12 }

unittest {
    import std.conv;

    string combinedString =
        to!string(version_major) ~ '.' ~ to!string(version_minor) ~ '.' ~ to!string(version_release);
    uint combinedDecimal =
        version_major * 10000 + version_minor * 100 + version_release;
    assert(combinedString == version_string, "Combined version string");
    assert(combinedDecimal == version_decimal, "Combined decimal version");
}

