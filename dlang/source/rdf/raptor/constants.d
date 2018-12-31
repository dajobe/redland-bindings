module rdf.raptor.constants;

private extern extern(C) immutable __gshared uint raptor_version_major;
private extern extern(C) immutable __gshared uint raptor_version_minor;
private extern extern(C) immutable __gshared uint raptor_version_release;
private extern extern(C) immutable __gshared uint raptor_version_decimal;

alias version_major   = raptor_version_major;
alias version_minor   = raptor_version_minor;
alias version_release = raptor_version_release;
alias version_decimal = raptor_version_decimal;

private extern extern(C) {
    string raptor_copyright_string;
    string raptor_home_url_string;
    string raptor_license_string;
    string raptor_short_copyright_string;
    string raptor_version_string;
    string raptor_owl_namespace_uri;
    string raptor_rdf_namespace_uri;
    string raptor_rdf_schema_namespace_uri;
    string raptor_xml_literal_datatype_uri_string;
    string raptor_xml_namespace_uri;
    string raptor_xmlschema_datatypes_namespace_uri;
}

alias copyright_string = raptor_copyright_string;
alias home_url_string = raptor_home_url_string;
alias license_string = raptor_license_string;
alias short_copyright_string = raptor_short_copyright_string;
alias version_string = raptor_version_string;
alias owl_namespace_uri = raptor_owl_namespace_uri;
alias rdf_namespace_uri = raptor_rdf_namespace_uri;
alias rdf_schema_namespace_uri = raptor_rdf_schema_namespace_uri;
alias xml_literal_datatype_uri_string = raptor_xml_literal_datatype_uri_string;
alias xml_namespace_uri = raptor_xml_namespace_uri;
alias xmlschema_datatypes_namespace_uri = raptor_xmlschema_datatypes_namespace_uri;

extern extern(C) uint rdf_namespace_uri_len;
extern extern(C) uint xml_literal_datatype_uri_string_len;

enum DomainType : ubyte {None_Domain = 0,
                         IOStream_Domain = 1,
                         Namespace_Domain = 2,
                         Parser_Domain = 3,
                         Qname_Domain = 4,
                         Sax2_Domain = 5,
                         Serializer_Domain = 6,
                         Term_Domain = 7,
                         Turtle_Writer_Domain = 8,
                         URI_Domain = 9,
                         World_Domain = 10,
                         WWW_Domain = 11,
                         XML_Writer_Domain = 12 }
