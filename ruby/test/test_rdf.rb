require 'test/unit'
require 'rdf/redland'

include Redland


class TestStorage < Test::Unit::TestCase
  def test_initialize()
    store = TripleStore.new('memory','mystore')
    assert_instance_of(TripleStore,store)
    assert_raises(RedlandError,'Should raise exception'){
      store2 = TripleStore.new('bogus','thestore')
    }
    assert_raises(RedlandError){
      store3 = TripleStore.new('hash','thefile')
    }
    
  end

  
end

class TestHash < Test::Unit::TestCase
  def test_initialize
    store = HashStore.new()
    assert_instance_of(HashStore, store)
    assert_equal('hashes',store.store_type)
    assert_equal('memory',store.hash_type)
    assert_raises(RedlandError,'should raise exception'){
      store = HashStore.new('bdb')
    }
  end

  def test_bdb_hash
    store = HashStore.new('bdb','thestore')
    
  end


end












