require 'rdf/redland'
require 'test/unit'

class TestStore < Test::Unit::TestCase
  include Redland

  def test_hash_store()
    store = HashStore.new()
    assert_equal('memory',store.hash_type)
    model = Model.new(store)
    model = nil
    store = nil
  end

  def test_hash_bdb()
    store = HashStore.new('bdb','mystore')
    assert_equal('bdb',store.hash_type)
    model = Model.new(store)
    model.add(
      Uri.new("http://xmlns.com"),
      Uri.new("http://predicate"),
      "baz"
    )
    st = Statement.new(
      Uri.new("http://xmlns.com"),
      Uri.new("http://predicate"),
      "baz"
    )
    assert(model.include_statement?(st))
  end

  def test_hash_raises()
    assert_raises(RedlandError){
    store = HashStore.new('bdb')
    } # bdb needs a name
  end

  def test_read_store()
    store = HashStore.new('bdb','thestore')
    model = Model.new(store)
    parser = Parser.new()
    parser.parse_into_model(model,"file:./out2.rdf")
    st = Statement.new(
      Uri.new("http://example.org/dom"),
      Uri.new("http://example.org/project"),
      "2334"
    )
    assert(model.include_statement?(st))
    #model.triples(){|s,p,o| puts "#{s}:#{p}:#{o}"}
  end
end
