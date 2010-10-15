require 'test/unit'
require 'rdf/redland'

class TestParser < Test::Unit::TestCase
  include Redland

  def setup
    world = $world
    @foaf = Namespace.new("http://xmlns.com/foaf/0.1/")
    @faa = Namespace.new("http://http://www.faa.gov/ontology/people#")
    @data_rdf_string =<<EOF
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns="http://www.faa.gov/ontology/people#"
  xmlns:site="http://www.faa.gov/ontology/site#"
  xmlns:foaf="http://xmlns.com/foaf/0.1/"
  xml:base="http://www.faa.gov/ontology/people">

<foaf:Person>
    <foaf:name>Pat Richardson</foaf:name>
    <foaf:title>NISC Program Manager</foaf:title>
    <foaf:firstName>Pat</foaf:firstName>
    <foaf:surname>Richardson</foaf:surname>
    <foaf:mbox rdf:resource="mailto:pat.ctr.richardson@faa.gov" />
    <foaf:phone>202-646-2352</foaf:phone>
    <worksFor rdf:resource="http://www.faa.gov/people#NISC" />
</foaf:Person>

<foaf:Person>
    <foaf:name>Jack Nager</foaf:name>
    <foaf:title>Director of NAS Implementation Program</foaf:title>
    <foaf:firstName>Jack</foaf:firstName>
    <foaf:surname>Nager</foaf:surname>
    <foaf:mbox rdf:resource="mailto:jack.nager@bogus.com" />
   
</foaf:Person>

<foaf:Person>
    <foaf:name>Dominic Sisneros</foaf:name>
    <foaf:title>Electronic Engineer</foaf:title>
    <foaf:firstName>Dominic</foaf:firstName>
    <foaf:surname>Sisneros</foaf:surname>
    <foaf:mbox rdf:resource="mailto:dominic@bogus.com" />
</foaf:Person>

</rdf:RDF>

EOF

    @data_triples_string=<<TRIPLES
_:r1081452401r1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/name> "Pat Richardson" .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/title> "NISC Program Manager" .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/firstName> "Pat" .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/surname> "Richardson" .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/mbox> <mailto:pat.ctr.richardson@bogus.com> .
_:r1081452401r1 <http://xmlns.com/foaf/0.1/phone> "202-646-2352" .
_:r1081452401r1 <http://www.faa.gov/ontology/people#worksFor> <http://www.faa.gov/people#NISC> .
_:r1081452401r2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
_:r1081452401r2 <http://xmlns.com/foaf/0.1/name> "Jack Nager" .
_:r1081452401r2 <http://xmlns.com/foaf/0.1/title> "Director of NAS Implementation Program" .
_:r1081452401r2 <http://xmlns.com/foaf/0.1/firstName> "Jack" .
_:r1081452401r2 <http://xmlns.com/foaf/0.1/surname> "Nager" .
_:r1081452401r2 <http://xmlns.com/foaf/0.1/mbox> <mailto:jack.nager@bogus.com> .
_:r1081452401r3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
_:r1081452401r3 <http://xmlns.com/foaf/0.1/name> "Dominic Sisneros" .
_:r1081452401r3 <http://xmlns.com/foaf/0.1/title> "Electronic Engineer" .
_:r1081452401r3 <http://xmlns.com/foaf/0.1/firstName> "Dominic" .
_:r1081452401r3 <http://xmlns.com/foaf/0.1/surname> "Sisneros" .
_:r1081452401r3 <http://xmlns.com/foaf/0.1/mbox> <mailto:dominic@bogus.com> .
TRIPLES
    
  end
  
  def test_parse
    model = Model.new()
    foaf = Namespace.new("http://xmlns.com/foaf/0.1/")
    faa = Namespace.new("http://http://www.faa.gov/ontology/people#")
    parser = Parser.new()
    parser.parse_into_model(model,"file:./ical.rdf")
    #model.triples(){|s,p,o| puts "#{s}:#{p}:#{o}"}
  end

  def test_stream
    parser = Parser.new()
    model = Model.new()
    parser.parse_as_stream("file:./ical.rdf"){|s| model.add_statement(s)}
  end

  def test_parser_with_contexts()
    store = HashStore.new('bdb','thestore')
    model = Model.new(store)
    context = Node.new(Uri.new('http://www.faa.gov'))
    parser = Parser.new()
    parser.parse_into_model(model,"file:./ical.rdf",nil,context)
    contexts = model.contexts
    assert(1,contexts.size)
    assert_equal(contexts[0],Node.new(Uri.new('http://www.faa.gov')))
  end

  def test_foaf_model(model)
    jack = model.subject(@foaf['mbox'],Node.new(:uri_string=>'mailto:jack.nager@bogus.com'))
    assert_equal('Jack',model.object(jack,@foaf['firstName']).to_s)
    assert_equal('Nager',model.object(jack,@foaf['surname']).to_s)
    assert_equal('Director of NAS Implementation Program', model.object(jack,@foaf['title']).to_s)
  end

  def test_parse_string_into_model
    model = Model.new()
    parser = Parser.new()
    parser.parse_string_into_model(model,@data_rdf_string,'http://xml.com')
    test_foaf_model(model)
  end

  def test_parse_string_into_model_from_triples
    model = Model.new()
    parser = Parser.ntriples()
    parser.parse_string_into_model(model,@data_triples_string,'http://xml.com')
    test_foaf_model(model)
  end

  def test_parse_string_as_stream
    model = Model.new()
    parser = Parser.new()
    parser.parse_string_as_stream(@data_rdf_string,'http://xml.com'){|s| model.add_statement(s)}
    test_foaf_model(model)
  end

  def smush_tester(model,dom)
    assert_equal('Electronic Engineer',model.object(dom,@foaf['title']).to_s)
    assert_equal('Sisneros',model.object(dom,@foaf['surname']).to_s)
  end  

  def no_test_string_smush
    model = Model.new()
    dom = BNode.new('dom')
    model.add(dom,@foaf['mbox'],Node.new(:uri_string=>'mailto:dominic@bogus.com'))
    model.add(dom,@foaf['name'],'Dominic Sisneros')
    #puts "Dom title before = #{model.object(dom,@foaf['title'])}"
    assert_nil(model.object(dom,@foaf['title']))
    parser = Parser.ntriples()
    parser.add_ident(@foaf['mbox'])
    parser.smush_string(@data_triples_string,model)
    smush_tester(model,dom)
  end

  def test_file_smush()
    #temp_model = Model.new()
    #parser = Parser.new()
    #parser.parse_string_into_model(temp_model,@data_rdf_string,"http://www.faa.gov")
    #serializer = Serializer.new()
    #serializer.to_file('triples.rdf',temp_model)
    model = Model.new()
    dom = BNode.new('dom')
    model.add(dom,@foaf['mbox'],Node.new(:uri_string=>'mailto:dominic@bogus.com'))
    model.add(dom,@foaf['name'],'Dominic Sisneros')
    parser = Parser.new()
    parser.add_ident(@foaf['mbox'])
    parser.smush_file(model,'file:./triples.rdf')
    smush_tester(model,dom)
    #model.statements{|s| puts s}
  end
end
