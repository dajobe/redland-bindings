module rdf.raptor.options;

enum RaptorOption : short {
    SCANNING = 0,
    ALLOW_NON_NS_ATTRIBUTES = 1,
    ALLOW_OTHER_PARSETYPES = 2,
    ALLOW_BAGID = 3,
    ALLOW_RDF_TYPE_RDF_LIST = 4,
    NORMALIZE_LANGUAGE = 5,
    NON_NFC_FATAL = 6,
    WARN_OTHER_PARSETYPES = 7,
    CHECK_RDF_ID = 8,
    RELATIVE_URIS = 9,
    WRITER_AUTO_INDENT = 10,
    WRITER_AUTO_EMPTY = 11,
    WRITER_INDENT_WIDTH = 12,
    WRITER_XML_VERSION = 13,
    WRITER_XML_DECLARATION = 14,
    NO_NET = 15,
    RESOURCE_BORDER = 16,
    LITERAL_BORDER = 17,
    BNODE_BORDER = 18,
    RESOURCE_FILL = 19,
    LITERAL_FILL = 20,
    BNODE_FILL = 21,
    HTML_TAG_SOUP = 22,
    MICROFORMATS = 23,
    HTML_LINK = 24,
    WWW_TIMEOUT = 25,
    WRITE_BASE_URI = 26,
    WWW_HTTP_CACHE_CONTROL = 27,
    WWW_HTTP_USER_AGENT = 28,
    JSON_CALLBACK = 29,
    JSON_EXTRA_DATA = 30,
    RSS_TRIPLES = 31,
    ATOM_ENTRY_URI = 32,
    PREFIX_ELEMENTS = 33,
    STRICT = 34,
    WWW_CERT_FILENAME = 35,
    WWW_CERT_TYPE = 36,
    WWW_CERT_PASSPHRASE = 37,
    NO_FILE = 38,
    WWW_SSL_VERIFY_PEER = 39,
    WWW_SSL_VERIFY_HOST = 40,
    LOAD_EXTERNAL_ENTITIES = 41,
}

enum ValueType { BOOL, INT, STRING, URI }

/+
extern(C)
struct OptionDescription {
    Domain domain;
    RaptorOption option;
    Value_Type valueType;
    char* Name;
    size_t len;
    char* label;
    Dummy* uri;
}
+/

// TODO
