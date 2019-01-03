module rdf.raptor.log;

import rdf.auxiliary.handled_record;
import rdf.raptor.constants;
import rdf.raptor.memory;
import rdf.raptor.uri;

enum LogLevel { none = 0,
                trace = 1,
                debug_ = 2,
                info = 3,
                warn = 4,
                error = 5,
                fatal = 6 }

extern(C)
struct LocatorHandle {
private:
    URIHandle* _uri;
    char* _file;
    int _line, _column, _byte;
}

extern(C)
struct LogMessage {
private:
    int _code;
    DomainType _domain;
    LogLevel _logLevel;
    LocatorHandle _locator;
    char* _text;
}

private void free_locator(LocatorHandle* handle) {
    raptor_free_uri(handle._uri);
    raptor_free_memory(handle._file);
    raptor_free_memory(cast(char*)handle);
}

private LocatorHandle* locator_copy(LocatorHandle* handle) {
    void* Result2 = raptor_alloc_memory(LocatorHandle.sizeof);
    LocatorHandle* result = cast(LocatorHandle*)Result2;
    *result = *handle;
    result._uri = raptor_uri_copy(handle._uri);
    result._file = copy_c_string(handle._file);
    return result;
}

private extern extern(C) {
    void raptor_free_uri(URIHandle* uri);
    URIHandle* raptor_uri_copy(URIHandle* uri);
}

struct LocatorWithoutFinalize {
    mixin WithoutFinalize!(LocatorHandle,
                           LocatorWithoutFinalize,
                           Locator,
                           locator_copy);
}

struct Locator {
    mixin WithFinalize!(LocatorHandle,
                        LocatorWithoutFinalize,
                        Locator,
                        free_locator);
}

// TODO: Stopped at Finalize_Handle (Object: Log_Message_Type; Handle: Log_Message_Handle);
