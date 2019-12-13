module rdf.raptor.log;

import std.string;
import std.stdio : FILE, File;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.constants;
import rdf.raptor.world;
import rdf.raptor.memory;
import rdf.raptor.uri;
import rdf.rasqal.world; // this dependency should probably be eliminated

enum LogLevel { none = 0,
                trace = 1,
                debug_ = 2,
                info = 3,
                warn = 4,
                error = 5,
                fatal = 6 }

string getLabel(LogLevel level) {
    return raptor_log_level_get_label(level).fromStringz.idup;
}

string getLabel(DomainType level) {
    return raptor_domain_get_label(level).fromStringz.idup;
}

struct LocatorHandle {
private:
    URIHandle* _uri;
    char* _file;
    int _line, _column, _byte;
}

struct LogMessageHandle {
private:
    int _code;
    DomainType _domain;
    LogLevel _logLevel;
    LocatorHandle* _locator;
    char* _text;
}

private void free_locator(LocatorHandle* handle) {
    raptor_free_uri(handle._uri);
    raptor_free_memory(handle._file);
    raptor_free_memory(cast(char*)handle);
}

private LocatorHandle* locator_copy(LocatorHandle* handle) {
    LocatorHandle* result = cast(LocatorHandle*)raptor_alloc_memory(LocatorHandle.sizeof);
    if(!result) return null;
    *result = *handle;
    result._uri = raptor_uri_copy(handle._uri);
    if(!result._uri) {
        raptor_free_memory(cast(char*)result);
        return null;
    }
    result._file = raptor_copy_c_string(handle._file);
    if(!result._file) {
        raptor_free_memory(cast(char*)result._uri);
        raptor_free_memory(cast(char*)result);
        return null;
    }
    return result;
}

private void free_log_message(LogMessageHandle* handle) {
    raptor_free_memory(handle._text);
    free_locator(handle._locator);
    raptor_free_memory(cast(char*)handle);
}

private LogMessageHandle* log_message_copy(LogMessageHandle* handle) {
    LogMessageHandle* result = cast(LogMessageHandle*)raptor_alloc_memory(LogMessageHandle.sizeof);
    if(!result) return null;
    *result = *handle;
    result._text = raptor_copy_c_string(handle._text);
    if(!result._text) {
        raptor_free_memory(cast(char*)result);
    }
    result._locator = locator_copy(handle._locator);
    if(!result._locator) {
        raptor_free_memory(cast(char*)result._text);
        raptor_free_memory(cast(char*)result);
        return null;
    }
    return result;
}

private extern extern(C) {
    void raptor_free_uri(URIHandle* uri);
    URIHandle* raptor_uri_copy(URIHandle* uri);
    int raptor_locator_print(LocatorHandle* locator, FILE *stream);
    int raptor_locator_format(char *buffer, size_t length, LocatorHandle* locator);
    alias raptor_log_handler = void function(void *user_data, LogMessageHandle* message);
    int raptor_world_set_log_handler(RaptorWorldHandle* world, void *user_data, raptor_log_handler handler);
    const(char*) raptor_log_level_get_label(LogLevel level);
    const(char*) raptor_domain_get_label(DomainType domain);
    // Experimental code:
    void rasqal_world_set_log_handler(RasqalWorldHandle* world,
                                      void *user_data,
                                      raptor_log_handler handler);
}

struct LocatorWithoutFinalize {
    mixin WithoutFinalize!(LocatorHandle,
                           LocatorWithoutFinalize,
                           Locator,
                           locator_copy);
    @property URIWithoutFinalize uri() const {
        return URIWithoutFinalize.fromHandle(handle._uri);
    }
    @property string file() const {
        return handle._file.fromStringz.idup;
    }
    @property uint line() const {
        return handle._line;
    }
    @property uint column() const {
        return handle._column;
    }
    @property uint byte_() const {
        return handle._byte;
    }
    void print(File file) const {
        if(raptor_locator_print(handle, file.getFP) != 0)
            throw new RDFException();
    }
    string format() const {
        immutable int res = raptor_locator_format(null, 0, handle);
        if(res < 0) throw new RDFException();
        char[] buffer = new char[res];
        if(raptor_locator_format(buffer.ptr, 0, handle) < 0) throw new RDFException();
        return buffer.idup;
    }
}

struct Locator {
    mixin WithFinalize!(LocatorHandle,
                        LocatorWithoutFinalize,
                        Locator,
                        free_locator);
}

struct LogMessageWithoutFinalize {
    mixin WithoutFinalize!(LogMessageHandle,
                           LogMessageWithoutFinalize,
                           LogMessage,
                           log_message_copy);
    @property int errorCode() const {
        return handle._code;
    }
    @property DomainType domain() const {
        return handle._domain;
    }
    @property LogLevel logLevel() const {
        return handle._logLevel;
    }
    @property string text() const {
        return handle._text.fromStringz.idup;
    }
    @property LocatorWithoutFinalize locator() const {
        return LocatorWithoutFinalize.fromNonnullHandle(handle._locator);
    }
}

struct LogMessage {
    mixin WithFinalize!(LogMessageHandle,
                        LogMessageWithoutFinalize,
                        LogMessage,
                        free_log_message);
}

class LogHandler : UnmovableObject {
    abstract void logMessage(LogMessageWithoutFinalize info);
    private static extern(C) void handleImpl(void* data, LogMessageHandle* msg) {
        return (cast(LogHandler)data).logMessage(LogMessageWithoutFinalize.fromHandle(msg));
    }
    final void set(RaptorWorldWithoutFinalize world) {
        if(raptor_world_set_log_handler(world.handle, cast(void*)this, &handleImpl) != 0)
            throw new RDFException();
    }
    /// Experimental API
    final void set(RasqalWorldWithoutFinalize world) {
        rasqal_world_set_log_handler(world.handle, cast(void*)this, &handleImpl);
    }
}

