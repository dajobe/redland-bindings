require 'redland'
require 'singleton'
#require 'rubygems'
#require_gem 'log4r'
require 'rdf/model'
require 'rdf/store'
require 'rdf/statement'
require 'rdf/node'
require 'rdf/parser'
require 'rdf/serializer'
require 'rdf/uri'
require 'rdf/resource'


module Redland

  #  include Redland

  class RedlandError < RuntimeError
  end

  class NodeTypeError < RedlandError
  end

  class World
    #include Singleton
    attr_accessor :world,:uri_hash

    # Create new RDF World object (constructor)
    def initialize(digest_name="",uri_hash=nil)
      @uri_hash = uri_hash
      @world = Redland::librdf_new_world()
      Redland::librdf_world_open(@world)
      ObjectSpace.define_finalizer(self,World.create_finalizer(@world))
    end

    def World.create_finalizer(world)
      proc {|id| "Finalizer on #{id}"
        puts "closing world"
        #Redland::librdf_free_world world
      }
    end
  end

  $world = Redland::World.new()

end


if $0 == __FILE__
  
  world = Redland::World.new()
  world = nil
  puts "listing instances of World: "
  ObjectSpace.each_object(Redland::World){|obj|
    p obj
  }
  puts "DONE"
  puts "Running the garbage collector"
  GC.start
  puts "Listing remaining instances of World:"
  ObjectSpace.each_object(Redland::World){|obj|
    p obj
  }
  puts "DONE"
end






