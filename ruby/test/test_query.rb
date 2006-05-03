require 'test/unit'
require 'rdf/redland'
require 'rdf/redland/constants'
require 'rdf/redland/schemas/foaf'

class TestQuery < Test::Unit::TestCase
  include Redland
 
  def setup
    @foaf = Namespace.new('http://xmlns.com/foaf/0.1/')
    @exns = Namespace.new('http://example.org/')
  end

  def test_model_query_api()
    model = Model.new()

    query = Query.new("SELECT ?a ?b ?c WHERE (?a ?b ?c)", "rdql", nil, nil)
    results = query.execute(model)
  end

  def test_query_api()
    model = Model.new()

    query = Query.new("SELECT ?a ?b ?c WHERE (?a ?b ?c)", "rdql", nil, nil)
    results = model.query_execute(query)
  end

  def test_model_query_results()
    model = Model.new()

    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("SELECT ?a ?b ?c WHERE (?a ?b ?c)", "rdql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    # Result should be a single variable binding result with three values

    assert(results.is_bindings?)
    assert_equal(results.binding_value(0), @exns['subject'])
    assert_equal(results.binding_value(1), @exns['pred'])
    assert_equal(results.binding_value(2), lit)

    values = [@exns['subject'], @exns['pred'], lit]
    assert_equal(results.binding_values(), values)

    assert_equal(results.binding_name(0), "a")
    assert_equal(results.binding_name(1), "b")
    assert_equal(results.binding_name(2), "c")

    assert_equal(results.binding_names(), ["a", "b", "c"])

    results.next()
    assert(results.finished?)
  end

  def test_model_query_ask()
    model = Model.new()

    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("ASK WHERE { ?a ?b ?c }", "sparql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    # Result should be a true boolean

    assert(results.is_boolean?)
    assert(results.get_boolean?)
  end

  def test_model_query_construct()
    model = Model.new()

    @exns = Namespace.new('http://example.org/')
    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("CONSTRUCT { ?a ?b ?c . ?b ?a ?c } WHERE { ?a ?b ?c }", "sparql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    # Result should be a graph of two triples

    assert(results.is_graph?)
    stream = results.as_stream()
    assert(stream != nil)

    statement = stream.current()
    assert_equal(statement, st)
    stream.next()

    statement = stream.current()
    st2 = Statement.new(@exns['pred'], @exns['subject'], lit)
    assert_equal(statement, st2)
    stream.next()

    assert(stream.end?)
  end

  def test_model_query_serialize_bindings()
    model = Model.new()

    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("SELECT ?a ?b ?c WHERE (?a ?b ?c)", "rdql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    string = results.to_string()

    # length of a SPARQL results format string - might change
    assert_equal(string.length(), 482)
  end

  def test_model_query_serialize_ask()
    model = Model.new()

    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("ASK WHERE { ?a ?b ?c }", "sparql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    string = results.to_string()

    # length of a SPARQL results format string - might change
    assert_equal(string.length(), 150)
  end

  def test_model_query_serialize_construct()
    model = Model.new()

    @exns = Namespace.new('http://example.org/')
    lit = Node.new("baz")
    st = Statement.new(@exns['subject'], @exns['pred'], lit)
    model.add_statement(st)

    query = Query.new("CONSTRUCT { ?a ?b ?c . ?b ?a ?c } WHERE { ?a ?b ?c }", "sparql", nil, nil)
    results = query.execute(model)
    assert(results != nil)

    string = results.to_string()

    # length of an RDF/XML string - might change
    assert_equal(string.length(), 401)
  end


end

