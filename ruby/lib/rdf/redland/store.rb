require 'rdf/redland'

module Redland

  # A class for storing RDF triples
  class TripleStore
    attr_accessor :store_type, :store

    # Creates a store with the given type, name, and options
    def initialize(store_type='memory',name='',options='')
      case store_type
      when "memory";
      when "hashes";
      when "file";
      when "uri";
      when "mysql";
      when "sqlite";
      when "postgresql";
      else
        raise RedlandError.new("Failed to initialize storage, unknown storage type [#{store_type}]")
      end
      @name = name
      @store_type = store_type
      @store=Redland.librdf_new_storage($world.world,store_type,name,options)
      unless @store
        raise RedlandError.new("Creating Storage Failed")
      end

      ObjectSpace.define_finalizer(self,TripleStore.create_finalizer(@store))
    end

    # You shouldn't use this. Used internally for cleanup.
    def TripleStore.create_finalizer(store)
      proc {|id| "Finalizer on #{id}"
        $log_final.info "closing store"
        Redland::librdf_free_storage(store) if store
      }
    end


  end

  # Store the triples in memory
  class MemoryStore < TripleStore
    def initialize(mem_name="",options_string="")
      super('memory',mem_name,options_string)
    end
  end

  # Store the triples in a hash. Can use memory or bdb
  class HashStore < TripleStore

    attr_accessor :hash_type

    # hash_type either memory or bdb. If it's bdb, you must specify a name
    def initialize(hash_type='memory',name='',dir='.', want_new=true,write=true,contexts=true)
      @hash_type = hash_type
      @dir = dir
      unless ( (hash_type == 'memory') || (hash_type == 'bdb'))
        raise RedlandError.new('Hash must be memory or bdb')
      end

      if hash_type=='bdb' then
        unless (name != '')
          raise RedlandError.new('bdb must have a filename')
        end
      end
      if (( want_new == true)|| (want_new == 'yes') || (want_new == 'Yes'))
        want_new = 'yes'
      elsif
        ( (want_new == false) || (want_new == 'no') || (want_new == 'No'))
        want_new = 'no'
      end
      if ((write == true) || (write == 'yes') || (write == 'Yes'))
        write = 'yes'
      elsif
        ( (write == false) || (write == 'no') || (write == 'No'))
        write = 'no'
      end
      if ((contexts == true) || (contexts == 'yes') || (contexts == 'Yes'))
        contexts = 'yes'
      elsif ((contexts == false) || (contexts == 'no') || (contexts == 'No'))
        contexts = 'no'
      end
      
      options = "hash-type='#{hash_type}',new='#{want_new}', dir='#{dir}', write='#{write}',contexts='#{contexts}'"
      super('hashes',name,options)
    end

    def HashStore.read_store(name,dir='.',write=true)
      return HashStore.new('bdb',name,dir,false,write)
    end



  end

  # Store the triples in a hash using bdb in the current directory
  class HashOpen < TripleStore

    def initialize(name)
      @store = Redland::librdf_new_storage $world.world, "hashes",name, "new='no',hash-type='bdb',dir='.'"
    end

  end

  # Store the triples in a file
  class FileStore < TripleStore

    def initialize(mem_name,options)
      super("file",mem_name,options)
    end

  end


end
#end

