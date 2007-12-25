# GRDDL parsing with URI filtering
# This file is in the public domain

import RDF

source="http://www.w3.org/People/Connolly/"
#source="http://www.w3.org/2001/sw/grddl-wg/td/titleauthor.html"

model=RDF.Model()
uri=RDF.Uri(string=source)
parser=RDF.GRDDLParser()

def reject_uri(uri):
  print "********** reject_uri(",uri,")"
  if "http://purl.org/NET/erdf/profile" == uri:
    return True
  return False

parser.set_uri_filter(reject_uri)

parser.parse_into_model(model, uri,uri)
parser.set_feature(RDF.Uri("http://feature.librdf.org/raptor-htmlTagSoup"), "0")
parser.set_feature(RDF.Uri("http://feature.librdf.org/raptor-microformats"), "0")
parser.set_feature(RDF.Uri("http://feature.librdf.org/raptor-htmlLink"), "0")


print "Parsing ",source,"added",model.size(),"statements"
