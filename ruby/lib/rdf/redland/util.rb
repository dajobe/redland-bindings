module Redland

  module Util
    # Return true if node is a literal
    def is_literal?(node)
      return (Redland.librdf_node_is_literal(node) !=0)
    end

    # Return true if node is a resource with a URI
    def is_resource?(node)
      return (Redland.librdf_node_is_resource(node) !=0)
    end

    # Return true if node is a blank node
    def is_blank?(node)
      return (Redland.librdf_node_is_blank(node) !=0)

    end

    # Given an Iterator, returns an array of all Nodes beyond it
    # including this one
    def get_node_array(node_iterator)
      node_array = []
      while not node_iterator.end?
        node_array << node_iterator.current
        node_iterator.next
      end
      return node_array
    end

    # Given an Stream, returns an array of all Statements beyond it
    # including this one
    def get_statement_array(stream)
      statement_array = []
      while not stream.end?
        statement_array << stream.current
        stream.next
      end
      return statement_array
    end

  end
end








