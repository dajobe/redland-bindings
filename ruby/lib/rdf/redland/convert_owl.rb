require 'rubyrdf'
require 'constants'

class Rdf2Owl
  
  attr_accessor :model

  def initialize(model)
    @model = model
    
  end

  def convert_classes()
    todelete = []
    @model.find(nil,TYPE,RDFS_CLASS) {|s,p,o|
      #log("found class #{s}:#{p}:#{o}")
      @model.add(s,p,OWL_CLASS)
      todelete << Statement.new(s,p,o)
log("Converted rdfs:Class #{s} into owl:Class")
    }
    todelete.each{|st| @model.delete_statement(st)}
    
    
  end

  def convert_properties()
    todelete = []
    @model.find(nil,TYPE,PROPERTY) {|s,p,o|
      type = property_type(s)
      log("type is #{type}")
      @model.add(s,TYPE,type)
      todelete << Statement.new(s,p,o)
log("Converted rdf:Property #{s} into #{type}")
    }
    todelete.each{|st| @model.delete_statement(st)}
    
    
  end

  def property_type(prop)
    owl_object = false
    @model.find(prop,RDFS_RANGE,nil){|s,p,o|
      if o.resource?
        return OWL_OBJECT_PROPERTY
      end
    }
    return OWL_DATATYPE_PROPERTY
  end

  def convert_RDFResource()
    todelete = []
    @model.find(nil,nil,RDFS_RESOURCE){|s,p,o|
      @model.add(s,p,OWL_THING)
      todelete << Statement.new(s,p,o)
      log("Replaced triple #{s}:#{p}:#{o} with (x,x owl:Thing)")
    }
    todelete.each{|st| @model.delete_statement(st)}
    
    
  end

  def convert()
     self.convert_RDFResource()
     self.convert_properties()
    self.convert_classes()
  end

  def log(message)
    puts "[RDF2OWL]" + message
  end

end

if $0 ==  __FILE__


  model = Model.new()
  parser = Parser.new()
  uri = Uri.new("http://www.faa.gov")
  #parser.parse_into_model(model,"file:/home/dsisnero/programming/rdf-files/ani_people.rdf","http://example.com/foo.rdf")
   parser.parse_into_model(model,"file:/home/dsisnero/programming/rdf-schemas/foaf.rdfs")
  
   convert = Rdf2Owl.new(model)
   convert.convert
  serializer = Serializer.new()
   serializer.to_file('/home/dsisnero/programming/rdf-schemas/foaf.owl',convert.model)

end
