require 'rubyrdf'

require 'convert_owl'


model = Model.new()
parser = Parser.new()
parser.parse_into_model(model,"file:/home/dsisnero/programming/rdf-schemas/foaf.rdfs")
convert = Rdf2Owl.new(model)
serializer = Serializer.new()
serializer.to_file('/home/dsisnero/programming/rdf-schemas/foaf.owl',convert.model)
