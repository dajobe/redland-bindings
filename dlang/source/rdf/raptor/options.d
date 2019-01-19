module rdf.raptor.options;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.world;
import rdf.raptor.uri;

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

enum Domain {
  RAPTOR_DOMAIN_NONE,
  RAPTOR_DOMAIN_IOSTREAM,
  RAPTOR_DOMAIN_NAMESPACE,
  RAPTOR_DOMAIN_PARSER,
  RAPTOR_DOMAIN_QNAME,
  RAPTOR_DOMAIN_SAX2,
  RAPTOR_DOMAIN_SERIALIZER,
  RAPTOR_DOMAIN_TERM,
  RAPTOR_DOMAIN_TURTLE_WRITER,
  RAPTOR_DOMAIN_URI,
  RAPTOR_DOMAIN_WORLD,
  RAPTOR_DOMAIN_WWW,
  RAPTOR_DOMAIN_XML_WRITER,
}

struct OptionDescriptionHandle {
private:
    Domain _domain;
    RaptorOption _option;
    ValueType _valueType;
    char* _name;
    size_t _len;
    char* _label;
    URIHandle* _uri;
}

private extern extern(C) {
    uint raptor_option_get_count();
    const(char*) raptor_option_get_value_type_label(ValueType type);
    void raptor_free_option_description(OptionDescriptionHandle* option_description);
    OptionDescriptionHandle* raptor_world_get_option_description(RaptorWorldHandle* world,
                                                                 Domain domain,
                                                                 RaptorOption option);
    RaptorOption raptor_world_get_option_from_uri(RaptorWorldHandle* world, URIHandle* uri);
}

struct OptionDescriptionWithoutFinalize {
    mixin WithoutFinalize!(OptionDescriptionHandle,
                           OptionDescriptionWithoutFinalize,
                           OptionDescription);
    @property Domain domain() { return handle._domain; }
    @property RaptorOption option() { return handle._option; }
    @property ValueType option() { return handle._valueType; }
    @property string name() { return handle._name[0..handle._len].idup; }
    @property string label() { return handle._label.fromStringz.idup; }
    @property URIWithoutFinalize uri() { return URIWithoutFinalize.fromHandle(handle._uri); }
}

struct OptionDescription {
    mixin WithFinalize!(OptionDescriptionHandle,
                        OptionDescriptionWithoutFinalize,
                        OptionDescription,
                        raptor_free_option_description);
}

ushort optionsCount() {
    return cast(ushort)raptor_option_get_count();
}

string valueTypeLabel(ValueType type) {
    const char* ptr = raptor_option_get_value_type_label(type);
    if(!ptr) throw new RDFException();
    return ptr.fromStringz.idup;
}

OptionDescription getOptionDescription(RaptorWorldWithoutFinalize world,
                                Domain domain,
                                RaptorOption option)
{
    return OptionDescription.fromNonnullHandle(
        raptor_world_get_option_description(world.handle, domain, option));
}

RaptorOption optionFromURI (RaptorWorldWithoutFinalize world, URIWithoutFinalize uri) {
    return raptor_world_get_option_from_uri(world.handle, uri.handle);
}
