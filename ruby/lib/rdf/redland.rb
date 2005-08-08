require 'rdf/redland/model'
require 'rdf/redland/store'
require 'rdf/redland/stream'
require 'rdf/redland/statement'
require 'rdf/redland/util'
require 'rdf/redland/node'
require 'rdf/redland/parser'
require 'rdf/redland/queryresults'
require 'rdf/redland/query'
require 'rdf/redland/resource'
require 'rdf/redland/serializer'
require 'rdf/redland/uri'
require 'rdf/redland/resource'
require 'rdf/redland/util'

# This module initialises the Redland library and references all
# resources from it.

module Redland

  
  # Load the interface to the C shared library
  require 'redland'

  include Redland
  
  # Error class specific to this package
  
  class RedlandError < RuntimeError
  end

  # Incorrect node type
  
  class NodeTypeError < RedlandError
  end

  # The world object is the single global variable in Redland that 
  # all the classes, their factories and implementations reference.
  #
  # In most programs there should be only one Redland world, using 
  # multiple models although multiple worlds are possible.

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

require 'rdf/redland/constants'

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






