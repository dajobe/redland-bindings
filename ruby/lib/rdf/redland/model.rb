#require 'rdf/redland'
require 'rdf/redland/store'
require 'rdf/redland/stream'
require 'rdf/redland/statement'
require 'rdf/redland/util'

#module RDF
module Redland
  
  class Model
    include Util
    
    attr_reader :model
    
    # Constructor - takes an optional argument store, which should 
    # be one of the storage classes (all derived from TripleStore)
    def initialize(store=nil)
      @statements = []
      store = MemoryStore.new() if not store
      @model = Redland.librdf_new_model($world.world,store.store,"")
      raise RedlandError.new("Creating new Model failed") if not @model
      @store = store
      ObjectSpace.define_finalizer(self,Model.create_finalizer(@model))
    end

    # You shouldn't use this. Used internally for cleanup.
    def Model.create_finalizer(model)
      proc {|id| "Finalizer on #{id}"
        #$log_final.info "closing model"
        Redland::librdf_free_model(model)
      }
    end

    # create a resource for this model.  The str can be a String, or a
    # Redland::Uri
    def create_resource(str=nil,context=nil)
      resource = Resource.new(str,self,context)
      if context
        resource.set_context(Resource.new(context))
      end
      return resource
    end

    # get a resource given by str.  If arg is a string it will create a
    # Resource from it.
    def get_resource(arg)
      res = self.find(Resource.new(arg),nil,nil)
      return (res.nil? || res[0].nil?) ? nil : res[0].subject
    end

    # Return the size of the Storage Model.  Raises a redland error if the model
    # has non countable storage
    def size()
      s = get_size()
      raise RedlandError.new("Attempt to get size when using non-countable storage") if s < 0
      return s
    end

    # Add the Statement to the Model with optional context Node
    def add_statement(statement,context=nil)
      if context
        context = Node.ensure(context)
        raise RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
        return Redland.librdf_model_context_add_statement(self.model,context,statement.statement)
      else
        return Redland.librdf_model_add_statement(self.model,statement.statement)
      end
    end

    # Add the triples (s,p,o) to the model with the optional context
    def add(subject,predicate,object,context=nil)
      my_statement = Statement.new(subject,predicate,object)
      self.add_statement(my_statement,context)
    end
    
    # Add the Stream of Statements to the Model with optional
    # context Node
    def add_statements(statement_stream,context=nil)
      if context
        context = Node.ensure(context)
        raise RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
        Redland.librdf_model_context_add_statements(self.model,context,statement_stream.stream)
      else
        return Redland.librdf_model_add_statements(self.model,statement_stream.stream)
      end
    end

    # Effectively removes a Predicate from the Model by replacing all
    # occurrences with a blank node (?? confirm)
    def smush(pred)
      identifying_hash = {}
      canonical = {}
      mapme = []
      transactions = []
      self.find(nil,pred,nil){|sub,pred,obj|
        if not identifying_hash.has_key?(obj.to_s)
          identifying_hash[obj.to_s] = sub
        elsif identifying_hash[obj.to_s] != sub
          subject_string = sub.to_s
          if not canonical.has_key?(subject_string)
            # print "amodule Rdf dding mapping: ",subject_string,"--->",identifying_hash[obj.to_s]
            canonical[subject_string] = identifying_hash[obj.to_s]
            mapme << Node.new(sub)
          end
        end
      }
      
      self.triples(){ |sub,pred,obj| 
        ss = sub.to_s
        os = obj.to_s
        new_statement = nil
        replace = false

        if canonical.has_key?(ss)
          if not replace
            new_statement = Statement.new(sub,pred,obj)
            replace = 1
          end
          new_statement.subject = canonical[ss]
        end
        if canonical.has_key?(os)
          if not replace
            new_statement = Statement.new(sub,pred,obj)
            replace = 1
            new_statement.object = canonical[os]
          end
        end
        
        if replace
          
          transactions << [Statement.new(sub,pred,obj),new_statement]
        end
      }

      
      transactions.each{ |pair|
        self.delete_statement(pair[0])
        self.add_statement(pair[1])
      }
    end
    

    
    # Remove the Statement from the Model with the optional context Node
    def delete_statement(statement,context=nil)
      if context
        context = Node.ensure(context)
        RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
        return Redland.librdf_model_context_remove_statement(self.model,context,statement.statement)
      else
        return Redland.librdf_model_remove_statement(self.model,statement.statement)
      end
    end

    # Remove the statement made of the triples (s,p,o) with the optional context Node
    def delete(s,p,o,context=nil)
      statement = Statement.new(s,p,o)
      self.delete_statement(statement,context)
    end

    # Remove the Statements from the Model with the given context Node
    def delete_context(context)
      context = Node.ensure(context)
      raise RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
      return Redland.librdf_model_context_remove_statements(self.model,context)
    end

    # Returns true if the Statement is in the Model
    def include_statement?(statement)
      return (Redland.librdf_model_contains_statement(self.model,statement.statement) != 0)
    end

    # Returns true if the given Triple is in the model
    def include?(s,p,o)
      statement = Statement.new(s,p,o)
      self.include_statement?(statement)
    end

    # list the model contents as a stream of statements
    def as_stream(context=nil)
      if context
        context = Node.ensure(context)
        raise RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
        my_stream = Redland.librdf_model_context_as_stream(self.model,context)
      else
        my_stream = Redland.librdf_model_as_stream(self.model)
      end
      return Stream.new(my_stream,self)
    end

    
    # Yield the triples subject, predicate, and object if a block is given.  
    # Otherwise return an array of the statements
    def triples(context=nil)
      stream = self.as_stream(context)
      return nil if not stream
      if block_given?
        while not stream.end?
          yield stream.current.subject ,stream.current.predicate,stream.current.object
          stream.next
        end
      else
        return get_statement_array(stream)
      end
    end

    # Yield the statements if a block is given.
    def statements(context = nil)
      stream = self.as_stream(context)
      while not stream.end?
        yield stream.current
        stream.next
      end
    end

    # Yield the triples as well as the context
    def triples_with_context(context=nil)
      mystream = self.as_stream(context)
      stream = Stream.new(mystream,self)
      while not stream.end?
        my_statement = mystream.current
        yield my_statement.subject,my_statement.predicate,my_statement.object,mystream.context
        stream.next
      end
    end
    
    # find the statements matching the given triple. The triples can be nil
    # 
    # Find all triples with FOAF['firstName'] of Dominic
    #   model.find(nil,FOAF['firstName'],'Dominic Sisneros'){|s,p,o ...}
    def find(s=nil,p=nil,o=nil,context=nil)
      statement = Statement.new(s,p,o,self)
      if context
        context = Node.ensure(context)
        raise RedlandError.new("Cannot make a Node from an object of #{context.class}") if not context
        my_stream = Redland.librdf_model_find_statements_in_context(self.model,statement.statement,context)
      else
        my_stream = Redland.librdf_model_find_statements(self.model,statement.statement)
      end
      return nil if not my_stream      
      stream = Stream.new(my_stream,self)
      if block_given?
        while not stream.end?
          my_statement = stream.current
          yield my_statement.subject,my_statement.predicate,my_statement.object
          stream.next
        end
      else #block not given
        return get_statement_array(stream)
      end
    end

    def statement_array_or_yield(stream)
      
    end

    # Return one Node in the Model matching (source, predicate,?)
    # The source and predicate can be a Node or Uri
    def object(source,predicate)
      if source.class == Uri then source = Node.new(source) end
      if predicate.class == Uri then predicate = Node.new(predicate) end
      my_node = Redland.librdf_model_get_target(self.model,source.node,predicate.node)
      return nil if not my_node
      return Literal.from_node(my_node) if is_literal?(my_node)
      return Resource.new(my_node,self,false)
    end
    
    # Return an array of Subject Nodes in the Model matching (?,predicate,target)
    # The predicate and target can be a Node or Uri
    # If block_given? yields the Subject Node
    def subjects(predicate,target)
      if predicate.class == Uri
        predicate = Node.new(predicate)
      end
      if target.class == Uri or target.class == String
        target = Node.new(target)
      end
      my_iterator = Redland.librdf_model_get_sources(self.model,predicate.node,target.node)
      raise RedlandError.new("Unable to create iterator") if !my_iterator
      iterator = NodeIterator.new(my_iterator,self,predicate,target)
      if block_given?
        while not iterator.end?
          yield iterator.current
          iterator.next
        end
      else
        return get_node_array(iterator)
      end

    end
    
    # Return one Node in the Model matching (?,predicate,target)
    # The source and predicate can be a Node or Uri
    def subject(predicate,target)
      if predicate.class == Uri
        predicate = Node.new(predicate)
      end
      if target.class == Uri or target.class == String
        target = Node.new(target)
      end
      my_node = Redland.librdf_model_get_source(self.model,predicate.node,target.node)
      return nil if !my_node
      return Literal.from_node(my_node) if is_literal?(my_node)
      return Resource.new(my_node,self,false)
    end
    
    # Return an array of Predicate Nodes in the Model matching (subject,?,target)
    # The subject and target can be a Node or Uri
    # If block_given? yields the Subject Node
    def predicates(source,target)
      if source.class == Uri
        source = Node.new(source)
      end
      if target.class == Uri or target.class == String
        target =Node.new(target)
      end
      my_iterator = Redland.librdf_model_get_arcs(self.model,source.node,target.node)
      raise RedlandError.new("unable to create iterator") if !my_iterator
      iterator = NodeIterator.new(my_iterator,self,source,target)
      if block_given?    
        while not iterator.end?
          yield iterator.current
          iterator.next
        end
      else 
        return get_node_array(iterator)
      end
    end

    def yield_node_or_array(iterator)
      iterator = NodeIterator.new(my_iterator,self,source,target)
      if block_given?    
        while not iterator.end?
          yield iterator.current
          iterator.next
        end
      else 
        return get_node_array(iterator)
      end
    end

    # yield the contexts defined in the model.  If the store wasn't set up for 
    # contexts raises a RedlandError.  If no block_given returns an array of the
    # contexts
    def contexts()
      my_iterator = Redland.librdf_model_get_contexts(@model)
      raise RedlandError.new("Unable to create iterator for contexts") if !my_iterator
      iterator= NodeIterator.new(my_iterator,self)
      if block_given?
        while not iterator.end?
          yield iterator.current
          iterator.next
        end
      else
        context_array = []
        while not iterator.end?
          context_array << iterator.current
          iterator.next
        end
        return context_array
      end
    end
    

    # saves this model to the specified filename
    def save(filename)
      serializer = Serializer.new()
      serializer.to_file(filename,self)
    end

    # a screen dump of the triples, if context is included a screen dump
    # of the triples for the given context
    def dump_model(context=nil)
      self.triples(context){|s,p,o| puts "#{s}:#{p}:#{o}"}
    end

    # parser the file with the given parser and uri and smushes
    # the file with the model on the statements  with the given
    # predlist 
    def parse_and_merge(parser,uri,predlist,context=uri,st=nil)
      context = Uri.new(context) if context == uri
      temp_model = MergedModel.new(self)
      if st
        temp_model.add_statement(st)
      else
        parser.parse_as_stream(uri){|s|temp_model.add_statement(s)}
      end
      temp_model.find_canonical(predlist)
      temp_model.rewrite(context)
      if temp_model.global_rewrites.size > 0
        print "***** GLOBAL REWRITES UNDERWAY ***"
        #self.rewrite(temp_model.global_rewrites)
      end
    end
    
    # clears the transactions (?? confirm)
    def rewrite(change_hash)
      transactions = []
    end

    # Get the number of statements in the model    
    def get_size()
      return Redland.librdf_model_size(@model)
    end

    private :get_size

    # execute a query
    def query_execute(query)
      results=Redland.librdf_model_query_execute(@model, query.query)
      if not results
        return nil
      else
        return QueryResults.new(results)
      end
    end

    # Serialize the Model to a syntax
    def to_string(name="", base_uri=nil, mime_type="", type_uri=nil)
      rbase_uri = base_uri.nil? ? nil : base_uri.uri
      rtype_uri = type_uri.nil? ? nil : type_uri.uri

      return Redland.librdf_model_to_string(@model, rbase_uri, name, mime_type, rtype_uri)
    end
    
  end

  # A non-context-aware model used for the purpose of merging
  class MergedModel < Model

    attr_reader :global_rewrites,:canonical

    # Constructor - needs a master model to use
    def initialize(master_model)
      @hooks = {}
      @count = {}
      @inverse_functional_properties = []
      @identifiers = [] # list of inverse functional properties to smush
      @canonical = {} #rewrites to perform on local level
      @global_rewrites = {} #rewrites to apply globally
      @transactions = []
      @master = master_model
      @pred_identifiers = {}
      super()
      
    end
    
    # Return one Node in the Model matching (?,predicate,target)
    # The source and predicate can be a Node or Uri
    def find_subject_in_master(pred,obj)
      return @master.subject(pred,obj)
    end
    
    # Effectively removes the listes Predicates from the Model by replacing all
    # occurrences with a blank node (?? confirm)
    def smush(predlist)
      self.find_canonical(predlist)
      self.rewrite()
    end

    # Locate predicates for smushing
    def find_canonical(predlist)
      predlist.each do |pred|
        identifier = {}
        self.find(nil,pred,nil) do |subj,pred,obj|
          master_id = self.find_subject_in_master(pred,obj)
          if not master_id
            if not identifier.has_key?(obj.to_s)
              identifier[obj.to_s] = subj 
            elsif identifier[obj.to_s] != subj #already an identifying URI
              new_value = identifier[obj.to_s]
              if not @canonical.has_key?(subj.to_s)
                @canonical[subj.to_s] = new_value
              end
            end
          else # master_id
            if identifier.has_key?(obj.to_s)
              if identifier[obj.to_s] != master_id
                @canonical[subj.to_s] = master_id
                identifier[obj.to_s] = master_id
              end
            else # master_id not in identifier
              identifier[obj.to_s] = master_id
              if @canonical.has_key?(subj.to_s)
                if @canonical[subj.to_s] != master_id
                  @global_rewrites[master_id] = @canonical[subj.to_s]
                end
              else
                if subj != master_id
                  @canonical[subj.to_s] = master_id
                end
              end
            end
          end
        end # self.find
        @pred_identifiers[pred] = identifier
      end #predlist.each
    end

    # actually do the smush
    def rewrite(context=nil)
      self.triples() do |sub,pred,obj|
        sub = @canonical[sub.to_s] if @canonical.key?(sub.to_s)
        obj = @canonical[obj.to_s] if @canonical.key?(obj.to_s)
        
        
        @master.add(sub,pred,obj,context)
      end
    end
    
  end #class
end
#end RDF module



