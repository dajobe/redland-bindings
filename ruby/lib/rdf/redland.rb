require 'rdf/redland/model'
require 'rdf/redland/store'
require 'rdf/redland/statement'
require 'rdf/redland/node'
require 'rdf/redland/parser'
#require 'rdf/redland/query'
require 'rdf/redland/resource'
require 'rdf/redland/serializer'
require 'rdf/redland/uri'
require 'rdf/redland/resource'
#require 'log4r'
#require 'rdf/redland/constants'

module Redland

  

  #  include Redland

  class RedlandError < RuntimeError
  end

  class NodeTypeError < RedlandError
  end

  class World
     #include Singleton
    attr_accessor :world

    # Create new RDF World object (constructor)
    def initialize()
      @world = Redland::librdf_new_world()
      Redland::librdf_world_open(@world)
      ObjectSpace.define_finalizer(self,World.create_finalizer(@world))
    end

    def World.create_finalizer(world)
      proc {|id| "Finalizer on #{id}"
#        log_final.info "closing world"
        #Redland::librdf_free_world world
      }
    end
  end

# Initialize Globals
  $world = Redland::World.new()
#  $log_final = Log4r::Logger.new('log_final')
#  outfile = Log4r::FileOutputter.new('final.log',:filename=>"final.log")
#  $log_final.outputters = outfile
#  $log_final.level = Log4r::DEBUG

end


if $0 == __FILE__
  
#  world = Redland::World.new()
#  world = nil
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






