require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/constants'
require 'rdf/redland/schemas/foaf'

class TestModel < Test::Unit::TestCase
  include Redland
 
  def setup
    @foaf = Namespace.new('http://xmlns.com/foaf/0.1/')
    @faa = Namespace.new("http://www.faa.gov/faa#")
  end

  def test_initialize()
    model = Model.new()
    assert_equal(0,model.size)
    st = Statement.new(Uri.new("http://example.org/foo"),Uri.new("http://example.org/bar"),"baz")
    st2 = Statement.new(Uri.new("http://example.org/ns/"),Uri.new("http://example.org/name"),"bar")
    model.add_statement(st)
    model.add_statement(st2)
    assert_equal(2,model.size)
  end

  def test_api()
    model = Model.new()
    foaf = Namespace.new('http://xmlns.com/foaf/0.1/')
    faa = Namespace.new('http://www.faa.gov/people#')
    model.add_statement(Statement.new(@faa['dom@some.gov'],@foaf['name'],"Dominic"))
    model.add_statement Statement.new(@faa['kris'],@foaf['firstName'],"Kris")
    model.add_statement Statement.new(@faa['kris'],@foaf['phone'],"333-123-2387")
    model.add(@faa['kris'],@foaf['name'],"Kris Frame")
    model.add( Uri.new('http://example.org/dom'),Uri.new('http://example.org/project'),'2334')
    domnode = model.subject(@foaf['name'],"Dominic")
    assert_equal(@faa['dom@some.gov'], domnode)
    assert_equal('333-123-2387',model.object(@faa['kris'],@foaf['phone']).to_s)

    #model.predicates(@faa['kris'],"Kris"){|p| puts p }
    #model.triples{|s,p,o| puts "subject: #{s}, pred: #{p}, object: #{o}"}
    #model.find( nil,@foaf['name'],nil){|s,p,o| puts o }
  end

  def test_delete()
    model = Model.new()
    dom = BNode.new('dom')
    model.add(dom,@foaf['firstName'],'Dominic')
    model.add(dom,@foaf['surname'],'Sisneros')
    assert_equal(2,model.size)
    st = Statement.new(dom,@foaf['firstName'],'Dominic')
    assert(model.include_statement?(st))
    model.delete_statement(st)
    assert(!model.include_statement?(st),"model should not include statement")
    assert_equal(1,model.size)
    model.delete(dom,@foaf['surname'],'Bogus')
    assert_equal(1,model.size)
    model.delete(dom,@foaf['surname'],'Sisneros')
    assert_equal(0,model.size)
  end

  def test_add_statements(model)
    dom = BNode.new('dom')
    kris = BNode.new('kris')
    model.add(dom,@foaf['firstName'],'Dominic')
    model.add(dom,@foaf['surname'],'Sisneros')
    model.add_statement Statement.new(kris,@foaf['firstName'],"Kris")
    model.add_statement Statement.new(kris,@foaf['phone'],"425-227-2387")
  end
  
  def test_delete()
    model = Model.new
    dom = BNode.new('dom')
    kris = BNode.new('kris')
    model.add(dom,@foaf['firstName'],'Dominic')
    model.add(dom,@foaf['surname'],'Sisneros')
    model.add_statement Statement.new(kris,@foaf['firstName'],"Kris")
    model.add_statement Statement.new(kris,@foaf['phone'],"192-192-192")
    assert_equal(4,model.size)
    to_delete =model.find(dom,nil,nil) # delete all dom nodes
    to_delete.each{|st| model.delete_statement(st)}
    assert_equal(2,model.size)
    assert_equal(0, model.find(dom,nil,nil).size)
    assert_equal(2, model.find(kris,nil,nil).size)
  end
  

  def testcontext()
    store = HashStore.new('bdb','thestore')
    model = Model.new(store)
    model.add_statement(Statement.new(Uri.new('http://example.org/dom'),Uri.new('http://example.org/project'),'10892'),Node.new(Uri.new('http://example.org/mycontext/dom')))
    model.add(Uri.new('http://example.org/dom'),Uri.new('http://example.org/name'),'Dominic',Uri.new('http://example.org/mycontext/dom'))
    #model.statements(){|s| puts s}
    contexts = []
    model.contexts{|c| contexts << c}
    assert_equal(1,contexts.size)
    #assert_equal(contexts[0],model.create_resource('http://mycontext/dom'))
    #model.add(Uri.new('http://example.org/dom'),Uri.new('http://example.org/surname'),'Sisneros',Uri.new('http://newcontext/dom'))
    #contexts = model.contexts()
    #puts "## CONTEXTS ARE #{contexts}"
    #assert_equal(2,contexts.size)    
  end

  def test_smush()
    model = Model.new()
    foaf = Namespace.new("http://xmlns.com/foaf/0.1/")
    rdfs = Namespace.new("http://www.23.org/2000/01/rdf-schema#")
    dom = Node.new(:blank=>'dom')
    model.add(dom,rdfs['type'],foaf['Person'])
    model.add(dom,foaf['mbox'],Uri.new('mailto:dom@sisna.com'))
    model.add(dom,foaf['firstName'],'Dominic')
    dom2 = Node.new(:blank=>'dom2')
    model.add_statement(Statement.new(dom2,rdfs['type'],foaf['Person']))
    model.add_statement(Statement.new(dom2,foaf['mbox'],Uri.new('mailto:dom@sisna.com')))
    model.add_statement(Statement.new(dom2,foaf['surname'],'Sisneros'))
    assert_equal(6,model.size)
    subjects = []
    model.subjects(foaf['mbox'],Uri.new('mailto:dom@sisna.com')){|s|
      subjects << s
    }
    assert_equal(2,subjects.size)
    model.smush(foaf['mbox'])
    assert_equal(1,model.subjects(foaf['mbox'],Uri.new('mailto:dom@sisna.com')).size)
    
  end

  def test_parse_and_merge()
    store = HashStore.new(hash_type='memory', options="contexts='yes'")
    model = Model.new(store)
    dom = BNode.new('dom')
    model.add(dom,FOAF::MBOX,Node.new(:uri_string=>'mailto:dominic@bogus.com'))
    model.add(dom,FOAF::NAME,'Dominic Sisneros')
    parser = Parser.new()
    model.parse_and_merge(parser,'file:triples.rdf',[FOAF::MBOX])

    domnode = model.subject(FOAF::MBOX,Node.new(:uri_string=>'mailto:dominic@bogus.com'))
    assert_equal('Electronic Engineer',model.object(domnode,FOAF::TITLE).to_s)
    assert_equal('Sisneros',model.object(domnode,FOAF::SURNAME).to_s)
  end
end

