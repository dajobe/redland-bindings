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
    int librdf_storage_sync(StorageHandle* storage);
    RedlandWorldHandle* librdf_storage_get_world(StorageHandle* storage);
    StorageHandle* librdf_new_storage(RedlandWorldHandle* world,
                                      const char *storage_name,
                                      const char *name,
                                      const char *options_string);
}

struct StorageInfo {
    string name, label;
}

struct StorageWithoutFinalize {
    mixin WithoutFinalize!(StorageHandle,
                           StorageWithoutFinalize,
                           Storage,
                           librdf_new_storage_from_storage);
    void sync() {
        if(librdf_storage_sync(handle) != 0)
            throw new RDFException();
    }
    // Will implement after http://bugs.librdf.org/mantis/view.php?id=636 bug fix
//     not overriding function Get_Feature (Storage: Storage_Type_Without_Finalize;
//                                         Feature: URI_Type_Without_Finalize'Class)
//                                          return Node_Type_Without_Finalize;
//     not overriding procedure Set_Feature (Storage: Storage_Type_Without_Finalize;
//                                           Feature: URI_Type_Without_Finalize'Class;
//                                           Value: Node_Type_Without_Finalize'Class);
    @property RedlandWorldWithoutFinalize world() {
        // Or just fromHandle?
        return RedlandWorldWithoutFinalize.fromNonnullHandle(librdf_storage_get_world(handle));
    }
}

struct Storage {
    mixin WithFinalize!(StorageHandle,
                        StorageWithoutFinalize,
                        Storage,
                        librdf_free_storage);
    static Storage create(RedlandWorldWithoutFinalize world,
                          string factoryName,
                          string name,
                          string options = "")
    {
        StorageHandle* h = librdf_new_storage(world.handle,
                                              factoryName.toStringz,
                                              name.toStringz,
                                              options.toStringz);
        return Storage.fromNonnullHandle(h);
    }
}

Nullable!StorageInfo enumerateStorages(RedlandWorldWithoutFinalize world, uint counter) {
    char* name, label;
    int result = librdf_storage_enumerate(world.handle, counter, &name, &label);
    if(result != 0) return Nullable!StorageInfo();
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

