require 'rdf/redland'

module Redland::OWL
  #OWL

  OWL = Namespace.new("http://www.w3.org/2002/07/owl#")
  OWLNS = Redland::Namespace.new("http://www.w3.org/2002/07/owl#")
  OWL_CLASS = OWLNS['Class']
  OWL_OBJECT_PROPERTY = OWLNS['ObjectProperty']
  OWL_DATATYPE_PROPERTY = OWLNS['DatatypeProperty']
  OWL_THING = OWLNS['OwlThing']

  def add_datatype(prop)
    
  end


end
