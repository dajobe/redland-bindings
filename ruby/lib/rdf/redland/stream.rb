require 'rdf/redland'

module Redland

  class Stream
    include Util
    attr_accessor :stream

    def initialize(object,model)
      @stream = object
      @model = model
      ObjectSpace.define_finalizer(self,Stream.create_finalizer(@stream))
    end

    def Stream.create_finalizer(stream)
      proc {|id| "Finalizer on #{id}"
        #      puts "closing model"
        Redland::librdf_free_model(stream)
      }
    end

    def end?
      if not @stream
        return true
      else
        return (Redland.librdf_stream_end(self.stream) != 0)
      end
    end

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

    def next()
      if not self.stream
        return true
      else
        return Redland.librdf_stream_next(self.stream)
      end
    end

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

  class NodeIterator
    include Util

    def initialize(object,model=nil,creator2=nil,creator3=nil)
      @iterator = object
      @model = model
      @creator2 = creator2
      @creator3 = creator3
      
      def end?()
        return (Redland.librdf_iterator_end(@iterator) != 0)
      end

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

      def next()
        my_node=Redland.librdf_iterator_next(@iterator)
      end

    end
  end
end
