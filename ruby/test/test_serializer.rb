require 'test/unit'
require 'rdf/redland'

class TestSerializer < Test::Unit::TestCase
  include Redland

  def test_serializer
    model = Model.new()
    foaf = Namespace.new('http://xmlns.com/foaf/0.1/')
    faa = Namespace.new('http://www.faa.gov/people#')
    model.add_statement(Statement.new(faa['dom@some.gov'],foaf['name'],"Dominic"))
    model.add_statement Statement.new(faa['kris'],foaf['firstName'],"Kris")
    model.add_statement Statement.new(faa['kris'],foaf['phone'],"333-123-2387")
    model.add(faa['kris'],foaf['name'],"Kris Frame")
    model.add(faa['dom'],faa['project'],'2334')
    domnode = model.subject(foaf['name'],"Dominic")
    assert_equal(faa['dom@some.gov'], domnode)
    assert_equal('333-123-2387',model.object(faa['kris'],foaf['phone']).to_s)
    

    #model.predicates(faa['kris'],"Kris"){|p| puts p }
    #model.triples{|s,p,o| puts "subject: #{s}, pred: #{p}, object: #{o}"}
    #model.find( nil,foaf['name'],nil){|s,p,o| puts o }
    
    model.save('out3.rdf')

    serializer = Serializer.new()
    #To Do This doesn't work.
    assert(serializer.set_namespace('foaf', 'http://xmlns.com/foaf/0.1/')) #returns true if succeeds
    assert(serializer.set_namespace('faa', Uri.new('http://www.faa.gov/people#'))) #returns true if succeeds
    serializer.to_file('out_namespace.rdf',model,Uri.new("http://www.faa.gov/people#"))
    #  serializer.namespace(foaf,'foaf')
    string = serializer.model_to_string(Uri.new("http://www.faa.gov/people#"), model)
    fdata = IO::read('out_namespace.rdf')
    assert(fdata == string)
  end
  

end
