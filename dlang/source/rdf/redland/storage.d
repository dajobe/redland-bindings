module rdf.redland.storage;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.redland.world;

struct StorageHandle;

private extern extern(C) {
    void librdf_free_storage(StorageHandle* storage);
    StorageHandle* librdf_new_storage_from_storage(StorageHandle* old_storage);
    int librdf_storage_enumerate(RedlandWorldHandle* world,
                                 const int counter,
                                 const char **name,
                                 const char **label);
}

struct StorageInfo {
    string name, label;
}

struct StorageWithoutFinalize {
    mixin WithoutFinalize!(StorageHandle,
                           StorageWithoutFinalize,
                           Storage,
                           librdf_new_storage_from_storage);
}

struct Storage {
    mixin WithFinalize!(StorageHandle,
                        StorageWithoutFinalize,
                        Storage,
                        librdf_free_storage);
}

Nullable!StorageInfo enumerateStorages(RedlandWorldWithoutFinalize world, uint counter) {
    char* name, label;
    int Result = librdf_storage_enumerate(world.handle, counter, &name, &label);
    return Nullable!StorageInfo(StorageInfo(name.fromStringz.idup, label.fromStringz.idup));
}

struct StoragesEnumerate {
private:
    RedlandWorldWithoutFinalize _world;
    uint counter = 0;
public:
    @property bool empty() {
        return librdf_storage_enumerate(_world.handle, counter, null, null) != 0;
    }
    @property StorageInfo front()
        in(!empty)
    {
        return enumerateStorages(_world, counter);
    }
    void popFront()
        in(!empty)
    {
        ++counter;
    }
}

// TODO: Stopped at Sync

