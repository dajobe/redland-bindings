module rdf.redland.log;

import std.string;
import rdf.auxiliary.user;
import rdf.raptor.log;
import rdf.redland.world;

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
    alias librdf_log_func = int function(void *user_data, LogMessageHandle* message);

    void librdf_world_set_logger(RedlandWorldHandle* world, void *user_data,
                                                         librdf_log_func log_handler);
}

struct LogMessage {
private:
    LogMessageHandle* handle;
public:
    @property int code() { return librdf_log_message_code(handle); }
    @property int level() { return librdf_log_message_level(handle); }
    @property LogFacility level() { return librdf_log_message_facility(handle); }
    @property string message() {
        return librdf_log_message_message(handle).fromStringz.idup;
    }
    @property Locator locator() {
        return Locator.fromHandle(librdf_log_message_locator(handle));
    }
}

class LogHandler : UnmovableObject {
    this(RedlandWorldWithoutFinalize world) {
        librdf_world_set_logger(world.handle, cast(void*)this, &our_handler);
    }
    abstract void handle(LogMessage message);
    private static extern(C) int our_handler(void *user_data, LogMessageHandle* message) {
        (cast(LogHandler*)user_data).handle(LogMessage(message));
        return 1;
    }
}

// TODO: Stopped at Log_Handler

