module rdf.redland.log;

struct LogMessage;

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

// TODO: Stopped at Get_Code

