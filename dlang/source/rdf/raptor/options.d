module rdf.raptor.options;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.world;
import rdf.raptor.uri;

enum RaptorOption : short {
    scanning = 0,
    allowNonNSAttributes = 1,
    allowOtherParsetypes = 2,
    allowBagid = 3,
    allowRDFTypeRDFList = 4,
    normalizeLanguage = 5,
    nonNFCFatal = 6,
    warnOtherParsetypes = 7,
    checkRDFId = 8,
    relativeUris = 9,
    writerAutoIndent = 10,
    writerAutoEmpty = 11,
    writerIndentWidth = 12,
    writerXMLVersion = 13,
    writerXMLDeclaration = 14,
    noNet = 15,
    resourceBorder = 16,
    literalBorder = 17,
    bnodeBorder = 18,
    resourceFill = 19,
    literalFill = 20,
    bnodeFill = 21,
    htmlTagSoup = 22,
    microformats = 23,
    htmlLink = 24,
    wwwTimeout = 25,
    writeBaseURI = 26,
    wwwHTTPCacheControl = 27,
    wwwHTTPUserAgent = 28,
    jsonCallback = 29,
    jsonExtraData = 30,
    rssTriples = 31,
    atomEntryURI = 32,
    prefixElements = 33,
    strict = 34,
    wwwCertFilename = 35,
    wwwCertType = 36,
    wwwCertPassphrase = 37,
    noFile = 38,
    wwwSSLVerifyPeer = 39,
    wwwSSLVerifyHost = 40,
    loadExternalEntities = 41,
}

enum ValueType { bool_, int_, string_, uri_ }

enum Domain {
  none,
  ioStream,
  namespace,
  parser,
  qname,
  sax2,
  serializer,
  term,
  turtleWriter,
  uri,
  world,
  www,
  xmlWriter,
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
    @property Domain domain() const { return handle._domain; }
    @property RaptorOption option() const { return handle._option; }
    @property ValueType option() const { return handle._valueType; }
    @property string name() const { return handle._name[0..handle._len].idup; }
    @property string label() const { return handle._label.fromStringz.idup; }
    @property URIWithoutFinalize uri() const { return URIWithoutFinalize.fromHandle(handle._uri); }
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
