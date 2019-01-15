module rdf.redland.storage;

import rdf.auxiliary.handled_record;

struct StorageHandle;

private extern extern(C) {
    void librdf_free_storage(StorageHandle* storage);
    StorageHandle* librdf_new_storage_from_storage(StorageHandle* old_storage);
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

// TODO: Stopped at Storage_Info

