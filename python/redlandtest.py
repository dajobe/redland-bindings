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

        s_two_again = self.make_statement (2)

        self.assert_(s_two == s_two_again,
                "equality; statement %s should equal statement %s" % \
                        (str(s_two), str(s_two_again)))

        s_three = self.make_statement (3)

        self.assert_(s_two != s_three,
                "inequality; statement %s should not equal statement %s" % \
                        (str(s_two), str(s_three)))

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

    def testParseNTriplesIntoModel(self):
        a = "file:../data/dc.nt"
        p = NTriplesParser()
        p.parse_into_model(self.model,a)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testParseTurtleIntoModel(self):
        a = "file:../data/dc.ttl"
        p = TurtleParser()
        p.parse_into_model(self.model,a)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testParseIntoModel(self):
        a = "file:../data/dc.rdf"
        self.parser.parse_into_model(self.model,a)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testIterateModel(self):
        a = "file:../data/dc.rdf"
        self.parser.parse_into_model(self.model,a)
        counter = 0
        for s in self.model:
            counter += 1
        self.assert_(counter == 3,"dc.rdf should have 3 statements")

    def testParseAsStream(self):
        a = "file:../data/dc.rdf"
        for s in self.parser.parse_as_stream(a):
            self.model.append(s)
        self.assert_(len(self.model) == 3,"dc.rdf should have 3 statements")

    def testFindStatements1(self):
        self.parser.parse_into_model(self.model,"file:../data/dc.rdf")
        statement = Statement(
                Uri("http://purl.org/net/dajobe/"),
                None, None)

        count = 0
        found_stmts = self.model.find_statements(statement)
        for i in found_stmts:
            count += 1
        self.assert_(count == 3, "Should have found_stmts 3 statements")

    def testContains(self):
        self.parser.parse_into_model(self.model,"file:../data/dc.rdf")
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
        a = "file:../data/dc.rdf"
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

    def testParseStringAsStream(self):
        content = """<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/"><rdf:Description rdf:about="http://librdf.org/"><dc:title>Redland RDF</dc:title></rdf:Description></rdf:RDF>"""
        base_uri = Uri("http://example.org/base#")
        s=self.parser.parse_string_as_stream(content, base_uri)
        self.assert_(s is not None, "parse_string_as_stream should have succeeded")
        rc=self.model.add_statements(s)
        self.assert_(rc == 0, "add statements should have succeeded")

class RedlandModelGeneral (unittest.TestCase):
    def setUp (self):
        self.storage = HashStorage("test", options="hash-type='bdb'")
        self.model = Model(self.storage)

    def testSync(self):
        self.model.sync()
        
class RasqalQueryTestCase (unittest.TestCase):
    def setUp(self):
        self.model = Model()
        self.parser = Parser()
        a = "file:../data/dc.rdf"
        self.parser.parse_into_model(self.model,a)

    def testCreate(self):
        q = RDQLQuery("SELECT ?a ?b ?c WHERE (?a ?b ?c)")
        self.assert_(q is not None)
        q = Query("- - -",query_language="triples")
        self.assert_(q is not None)

    def testRDQLQueryCount(self):
        q = RDQLQuery("SELECT ?x ?y ?z WHERE (?x ?y ?z)")
        results = q.execute(self.model)
        for result in results:
            pass
        self.assert_(len(results) == 3, "Query count should be 3 after query finished running")

    def testRDQLQueryRun(self):
        q = RDQLQuery("SELECT ?x ?y ?z WHERE (?x ?y ?z)")
        results = q.execute(self.model)
        self.assert_(results is not None,"Query eval not OK")

        count = 0
        for result in results:
            count += 1
        self.assert_(count == 3, "Should have found three results in query")

    def testTripleQueryAsBindingsDontWork(self):
        q = Query("- - -",query_language="triples")
        results = q.execute(self.model)
	first = results.get_binding_name(0)
        self.assert_(first is None,"Triples queries shouldn't work as bindings - None should be returned from get_binding_name(0)")

    def testRDQLQueryAsStreamDontWork(self):
        q = RDQLQuery("SELECT ?a ?c WHERE (?a dc:title ?c) USING dc FOR <http://purl.org/dc/elements/1.1/>")
	failed = False
	try:
          stream = q.execute(self.model).as_stream()
	except:
          failed = True
        self.assert_(failed is True,"RDQL queries shouldn't work as streams - None should be returned from as_stream")

    #def testRDQLParseError(self):
        #q = RDQLQuery("SELECT WHERE")
        #results = q.run_as_bindings(self.model)
        # TODO: This needs to fail somehow

    def testTripleQuery(self):
        q = Query("- - -",query_language="triples")
        results = q.execute(self.model)
        stream = results.as_stream()
        self.assert_(stream is not None,"Query eval not OK")

        count = 0
        for result in stream:
            count += 1
        self.assert_(count == 3, "Should have found three results in query")

    def testRDQLQueryRunOnModel(self):
        q = RDQLQuery("SELECT ?x ?y ?z WHERE (?x ?y ?z)")
        bindings = self.model.execute(q)
        self.assert_(bindings is not None,"Query eval not OK")

        count = 0
        for result in bindings:
            count += 1
        self.assert_(count == 3, "Should have found three results in query")

    def testTripleQueryOnModel(self):
        q = Query("- - -",query_language="triples")
        results = self.model.execute(q)
        stream = results.as_stream()
        self.assert_(results is not None,"Query eval not OK")

        count = 0
        for result in stream:
            count += 1
        self.assert_(count == 3, "Should have found three results in query")

    def testSPARQLQueryAsStream(self):
        q = SPARQLQuery("PREFIX dc: <http://purl.org/dc/elements/1.1/> CONSTRUCT * WHERE {?a dc:title ?c}")
        s = q.execute(self.model).as_stream()
        self.assert_(s is not None, "execute as_stream should have succeeded")
        rc=self.model.add_statements(s)
        self.assert_(rc == 0, "add statements should have succeeded")

    def testSPARQLQueryAsk(self):
        q = SPARQLQuery("PREFIX dc: <http://purl.org/dc/elements/1.1/> ASK WHERE {?x ?y ?z}")
        rc = q.execute(self.model).get_boolean()
        self.assert_(rc is True, "execute get_boolean should have returned True")

    def testSPARQLQueryAsString(self):
        q = SPARQLQuery("PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT * WHERE {?a dc:title ?c}")

        r = q.execute(self.model)
        expect = 377
        self.assert_(r is not None, "execute to_string should have succeeded")
        s = r.to_string()
        self.assert_(len(s) == expect, "to_string should have returned "+str(expect)+" not " + str(len(s)))


    def testSPARQLQueryTwice(self):
        q = SPARQLQuery("SELECT ?x ?y ?z WHERE {?x ?y ?z}")
        results1 = q.execute(self.model)
        count1 = 0
        for result in results1:
            count1 += 1
        self.assert_(count1 == 3, "Should have found three results in execution 1")

        results2 = q.execute(self.model)
        count2 = 0
        for result2 in results2:
            count2 += 1
        self.assert_(count2 == 3, "Should have found three results in execution 2")


#    def testSPARQLQueryTwiceOverwriteVar(self):
#        q = SPARQLQuery("SELECT ?x ?y ?z WHERE {?x ?y ?z}")
#        results = q.execute(self.model)
#        count = 0
#        for result in results:
#	    print "result 1>",count,result
#            count += 1
#        self.assert_(count == 3, "Should have found three results in execution 1 not "+str(count))
#
#        results = q.execute(self.model)
#        count = 0
#        for result in results:
#	    print "result 2>",count,result
#            count += 1
#        self.assert_(count == 3, "Should have found three results in execution 2 not "+str(count))


if __name__ == '__main__':
    unittest.main()
