#!/usr/bin/python
# 
# redlandtest.py - Redland Python RDF model unit tests
#
# $Id$
#
# Unit tests provided by Matt Biddulph 
# 
#

import unittest
from RDF import *

class RedlandStreamsCase(unittest.TestCase):
    def setUp(self):
        self.storage = MemoryStorage(options_string="contexts='yes'")
        self.model = Model(self.storage)
        self.parser = Parser()

    def testParseIntoModel(self):
        a = "file:../perl/dc.rdf"
        self.parser.parse_into_model(self.model,a)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testParseAsStream(self):
        a = "file:../perl/dc.rdf"
        for s in self.parser.parse_as_stream(a):
            self.model.add_statement(s)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testFindStatements1(self):
        self.parser.parse_into_model(self.model,"file:../perl/dc.rdf")
        statement = Statement(subject=Node(uri_string="http://purl.org/net/dajobe/"),
                predicate=None,
                object=None)

        count = 0
        found_stmts = self.model.find_statements(statement)
        for i in found_stmts:
            count += 1
        self.assert_(count == 3,"Should have found_stmts 3 statements")

    def testFindStatementsWithContext(self):
        a = "file:../perl/dc.rdf"
        intended_context = Node(uri_string="http://example.org/")
        for s in self.parser.parse_as_stream(a):
            self.model.add_statement(s,intended_context)

        subject = Node(uri_string="http://purl.org/net/dajobe/")
        statement = Statement(subject=subject,
                predicate=None,
                object=None)

        count = 0
        found_stmts = self.model.find_statements(statement)
        for s,c in found_stmts.context_iter():
            self.failIf(c is None, "Didn't get context back from find_statements")
            self.assert_(c == intended_context, "Didn't get the right context back from find_statements")
            self.assert_(statement.subject == subject,"find_statements didn't find statements with the right subject")
            count += 1
        self.assert_(count == 3,"Should have found_stmts 3 statements")

if __name__ == '__main__':
    unittest.main()


