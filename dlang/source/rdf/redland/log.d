module rdf.redland.log;

import std.string;
import rdf.raptor.log;

struct LogMessageHandle;

enum LogLevel { None  = 0,
                Debug = 1,
                Info  = 2,
                Warn  = 3,
                Error = 4,
                Fatal = 5 }

enum LogFacility { None,
                   Concepts,
                   Digest,
                   Files,
                   Hash,
                   Init,
                   Iterator,
                   List,
                   Model,
                   Node,
                   Parser,
                   Query,
                   Serializer,
                   Statement,
                   Storage,
                   Stream,
                   URI,
                   UTF8,
                   Memory,
                   Raptor }

private extern extern(C) {
    int librdf_log_message_code(LogMessageHandle* message);
    LogLevel librdf_log_message_level(LogMessageHandle* message);
    LogFacility librdf_log_message_facility(LogMessageHandle* message);
    const(char*) librdf_log_message_message(LogMessageHandle* message);
    LocatorHandle* librdf_log_message_locator(LogMessageHandle* message);
}

struct LogMessageType {
private:
    LogMessageHandle* handle;
public:
    @property int code() { return librdf_log_message_code(handle); }
    @property int level() { return librdf_log_message_level(handle); }
    @property LogFacility level() { return librdf_log_message_facility(handle); }
    @property string message() {
        return librdf_log_message_message(handle).fromStringz.idup;
    }

}

// TODO: Stopped at Get_Locator

