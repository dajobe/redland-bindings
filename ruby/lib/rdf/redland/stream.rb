require 'rdf/redland'
require 'rdf/redland/util'

module Redland

  # This module provides a method to generate a stream of statements, 
  # suitable for outputing from RDF/XML parsers, returning as the results 
  # of queries and serialising models in order to manipulate them or 
  # transform into another syntax.
  class Stream
    include Util
    attr_accessor :stream

    # Create a wrapper for a librdf_stream object (?? confirm)
    def initialize(object,model)
      @stream = object
      @model = model
      ObjectSpace.define_finalizer(self,Stream.create_finalizer(@stream))
    end

    # You shouldn't use this. Used internally for cleanup.
    def Stream.create_finalizer(stream)
      proc {|id| "Finalizer on #{id}"
        #puts "closing stream"
        Redland::librdf_free_stream(stream)
      }
    end

    # Test if the stream has ended
    def end?
      if not @stream
        return true
      else
        return (Redland.librdf_stream_end(self.stream) != 0)
      end
    end

    # Get the current Statement in the stram
    def current
      if not self.stream
        return nil
      end
      my_statement = Redland.librdf_stream_get_object(self.stream)
      
      unless my_statement
        return nil
      else
        return Statement.new(:from_object=>my_statement,:model=>@model)
      end
    end

    # Move to the next Statement in the stream
    def next()
      if not self.stream
        return true
      else
        return Redland.librdf_stream_next(self.stream)
      end
    end

    # Get the context of the current Statement in the stream
    def context()
      if not self.stream
        return true
      end
      my_node = Redland.librdf_stream_get_context(@stream)
      if not my_node
        return nil
      else
        return Node.new(:from_object=>my_node)
      end
    end

    
  end

  # The iterator provides a generic way to receive a sequence of values
  # (order may or may not be import) from objects, usually generated on demand.
  class NodeIterator
    include Util

    def initialize(object,model=nil,creator2=nil,creator3=nil)
      @iterator = object
      @model = model
      @creator2 = creator2
      @creator3 = creator3
      
      # Test if the iterator has finished
      def end?()
        return (Redland.librdf_iterator_end(@iterator) != 0)
      end

      # Get the current object from the iterator
      def current
        my_node = Redland.librdf_iterator_get_object(@iterator)
        if my_node == "NULL" or my_node == nil
          return nil
        elsif is_literal?(my_node)
          return Literal.from_node(my_node)
        else
          return Resource.new(my_node,@model)
        end
      end

      # Move to the next iterator element
      def next()
        my_node=Redland.librdf_iterator_next(@iterator)
      end

    end
  end
end
