#!/usr/bin/python
# 
# redlandtest.py - Redland Python RDF model unit tests
#
# $Id$
#
# Authors:
#   Matt Biddulph <mb@picdiary.com>
#   Edd Dumbill <edd@usefulinc.com>

import unittest
from RDF import *

class RedlandModelCase (unittest.TestCase):
    def setUp (self):
        self.storage_no_context = Storage (storage_name="hashes",
            name="redland_testcase_nc",
            options_string="hash-type='bdb',contexts='no',dir='/tmp',new='yes'")

        self.storage_context = Storage (storage_name="hashes",
            name="redland_testcase_c",
            options_string="hash-type='bdb',contexts='yes',dir='/tmp',new='yes'")

        self.context_node = Node(Uri("http://mycontext/"))
        self.alien_context_node = Node(Uri("http://mycontext/alien"))

    def make_statement (self, n):
        return Statement (
                Uri ("http://subject/%d" % n),
                Uri ("http://pred/%d" % n),
                Uri ("http://object/%d" % n))

    def testListnessNoContext (self):
        model = Model (self.storage_no_context)
        for i in range(0,4):
            s = self.make_statement (i)
            model.append (s)

        for i in range(0,4):
            s = self.make_statement (i)
            self.assert_(s in model,
                "statement %s should be in model" % (str(s)))
            
        s_two = self.make_statement (2)

        del model[s_two]

        self.assert_(s_two not in model,
                "del failed; statement %s should not be in model" % \
                        (str(s_two)))

    def testListnessContext (self):
        model = Model (self.storage_context)
        for i in range(0,4):
            s = self.make_statement (i)
            model.append (s, self.context_node)

        for i in range(0,4):
            s = self.make_statement (i)
            self.assert_((s, self.context_node) in model,
                "statement %s context %s should be in model" % (
                    str(s), str(self.context_node)))
            self.assert_((s, self.alien_context_node) not in model,
                "statement %s context %s should be in model" % (
                    str(s), str(self.alien_context_node)))
            
        s_two = self.make_statement (2)

        del model[s_two, self.context_node]

        self.assert_((s_two, self.context_node) not in model,
                "del failed; statement %s context %s should not be in model" % \
                        (str(s_two), str(self.context_node)))

class RedlandSimpleGetsCase(unittest.TestCase):
    def setUp(self):
        self.storage = MemoryStorage()
        self.model = Model(self.storage)
        self.node1 = Node(Uri("http://node1/"))
        self.node2 = Node(Uri("http://node2/"))
        self.node3 = Node(Uri("http://node3/"))
        self.model.append (Statement(self.node1, self.node2, self.node3))

    def testSimpleGets(self):
        self.assert_(
                self.model.get_source(self.node2, self.node3) == self.node1,
                "Source should be node1")
        self.assert_(
                self.model.get_target(self.node1, self.node2) == self.node3,
                "Target should be node3")
        self.assert_(
                self.model.get_predicate(self.node1, self.node3) == self.node2,
                "Predicate should be node2")

class RedlandGetsCase(unittest.TestCase):
    def setUp(self):
        self.storage = MemoryStorage()
        self.model = Model(self.storage)
        self.node1 = Node(Uri("http://node1/"))
        self.node2 = Node(Uri("http://node2/"))
        self.node3 = Node(Uri("http://node3/"))
        self.node4 = Node(Uri("http://node4/"))
        self.node5 = Node(Uri("http://node5/"))
        self.node6 = Node(Uri("http://node6/"))
        self.nodeb = Node()
        self.model.append(Statement(self.node1, self.node2, self.node3))
        self.model.append(Statement(self.node1, self.node2, self.node4))
        self.model.append(Statement(self.node6, self.node2, self.node4))
        self.model.append(Statement(self.node5, self.node3, self.node6))
        self.model.append(Statement(self.node5, self.node1, self.node6))
        # and now, with context
        self.storageC = MemoryStorage(options_string="contexts='yes'")
        self.modelC = Model(self.storageC)
        self.context_node = Node(Uri("http://mycontext/"))

        self.modelC.append(Statement(self.node1, self.node2, self.node3),
                self.context_node)
        self.modelC.append(Statement(self.node1, self.node2, self.node4),
                self.context_node)
        self.modelC.append(Statement(self.node6, self.node2, self.node4),
                self.context_node)
        self.modelC.append(Statement(self.node5, self.node3, self.node6),
                self.context_node)
        self.modelC.append(Statement(self.node5, self.node1, self.node6),
                self.context_node)

    def testGets(self):
        sources = list(self.model.get_sources(self.node2, self.node4))
        self.assert_( (self.node1 in sources) and (self.node6 in sources),
                "sources should contain node1 and node6")
    
        targets = list(self.model.get_targets(self.node1, self.node2))
        self.assert_( (self.node3 in targets) and (self.node4 in targets),
                "targets should contain node3 and node4")

        predicates = list(self.model.get_predicates(self.node5, self.node6))
        self.assert_( (self.node3 in predicates) and (self.node1 in predicates),
                "predicates should contain node3 and node1")

    def testGetsC(self):
        sources = list(self.modelC.get_sources_context(self.node2, self.node4))
        self.assert_( ((self.node1, self.context_node) in sources) and
                ((self.node6, self.context_node) in sources),
                "sources should contain (node1,c) and (node6,c)")

        targets = list(self.modelC.get_targets_context(self.node1, self.node2))
        self.assert_( ((self.node3, self.context_node) in targets) and
                ((self.node4, self.context_node) in targets),
                "targets should contain (node3,c) and (node4,c)")

        predicates = list(self.modelC.get_predicates_context(self.node5,
            self.node6))
        self.assert_( ((self.node3, self.context_node) in predicates) and
                ((self.node1, self.context_node) in predicates),
                "predicates should contain (node3,c) and (node1,c)")

class RedlandStreamsCase(unittest.TestCase):
    def setUp(self):
        self.storage = MemoryStorage(options_string="contexts='yes'")
        self.model = Model(self.storage)
        self.parser = Parser()

    def testParseIntoModel(self):
        a = "file:../perl/dc.rdf"
        self.parser.parse_into_model(self.model,a)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testIterateModel(self):
        a = "file:../perl/dc.rdf"
        self.parser.parse_into_model(self.model,a)
        counter = 0
        for s in self.model:
            counter += 1
        self.assert_(counter == 3,"dc.rdf should have 3 statements")

    def testParseAsStream(self):
        a = "file:../perl/dc.rdf"
        for s in self.parser.parse_as_stream(a):
            self.model.append(s)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testFindStatements1(self):
        self.parser.parse_into_model(self.model,"file:../perl/dc.rdf")
        statement = Statement(
                Uri("http://purl.org/net/dajobe/"),
                None, None)

        count = 0
        found_stmts = self.model.find_statements(statement)
        for i in found_stmts:
            count += 1
        self.assert_(count == 3, "Should have found_stmts 3 statements")

    def testContains(self):
        self.parser.parse_into_model(self.model,"file:../perl/dc.rdf")
        statement = Statement(
                Uri("http://purl.org/net/dajobe/"),
                Uri("http://purl.org/dc/elements/1.1/creator"), 
                "Dave Beckett")

        count = 0
        found_stmts = self.model.find_statements(statement)
        for i in found_stmts:
            count += 1
        self.assert_(count == 1, "Should have found_stmts 1 statement") # test we can find it at all, for comparison
        self.assert_(statement in self.model, "Should have found a statement via __contains__")

    def testFindStatementsWithContext(self):
        a = "file:../perl/dc.rdf"
        intended_context = Node(Uri("http://example.org/"))
        for s in self.parser.parse_as_stream(a):
            self.model.append (s, intended_context)

        subject = Node(Uri("http://purl.org/net/dajobe/"))
        statement = Statement(subject, None, None)

        count = 0

        for s,c in self.model.find_statements_context(statement):
            self.failIf(c is None,
                    "Didn't get context back from find_statements")
            self.assert_(c == intended_context,
                    "Didn't get the right context back from find_statements")
            self.assert_(statement.subject == subject,
                    "find_statements_context didn't find statements with the right subject")
            count += 1

        self.assert_(count == 3, "Should have found 3 statements")

class RedlandModelGeneral (unittest.TestCase):
    def setUp (self):
        self.storage = HashStorage("test", options="hash-type='bdb'")
        self.model = Model(self.storage)

    def testSync(self):
        self.model.sync()
        

if __name__ == '__main__':
    unittest.main()


