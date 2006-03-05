#!/usr/bin/perl -Tw
#
# query.pl - Redland RDF Query demo
#
# $Id$
#
# Copyright (C) 2004-2006, David Beckett http://purl.org/net/dajobe/
# Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 
# 
#

# CHANGE THIS FOR YOUR CONFIGURATION
$::ROOT_DIR='/somewhere';

use strict;

# Helps with broken web requests (missing headers)
$ENV{'Content-Length'}||=0;

# Tainting, dontcha know
$ENV{'PATH'}="/bin:/usr/bin:/usr/local/bin";

# Standard perl modules
use CGI;
use LWP::Simple;
use URI::URL;

# Configuration

my(@query_languages)=qw(rdql sparql),
my(%query_language_labels)=('rdql'  =>'RDQL',
			    'sparql' =>'SPARQL');
my $default_query_language='sparql';

my $example_foaf_uri='http://purl.org/net/dajobe/webwho.xrdf';
my $example_rss_uri='http://www.w3.org/2000/08/w3c-synd/home.rss';

my(%query_examples)=(
  'rdql' => [
{ Q => <<EOT
select ?name
where
  (?x rdf:type foaf:Person)
  (?x foaf:name ?name)
using foaf for <http://xmlns.com/foaf/0.1/>
EOT
 , D => $example_foaf_uri
 , T => 'A FOAF query that finds the names of all the people in the graph'
}  , 
{ Q => <<EOT
select ?nick, ?name where 
  (?x rdf:type foaf:Person) (?x foaf:nick ?nick) (?x foaf:name ?name)
using foaf for <http://xmlns.com/foaf/0.1/>
EOT
 , D => $example_foaf_uri
 , T => 'A FOAF query that finds all people with a name and an IRC nick'
},
{ Q => <<'EEEEE'
select ?name, ?archives
where
  (?list rdf:type doaml:MailingList)
  (?list doaml:name ?name)
  (?list doaml:archives ?archives)
and ?name =~ /p3p/
using doaml for <http://ns.balbinus.net/doaml#>
EEEEE
 , D => 'http://www.doaml.net/doaml/w3ml/Lists.rdf'
 , T => 'Print the name and archive URIs of W3C mailing lists about P3P as described by <a href="http://www.doaml.net/">DOAML</a>'
},
  ],

  'sparql' => [
{ Q => <<EOT
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT DISTINCT ?name
WHERE {
  ?x rdf:type foaf:Person .
  ?x foaf:name ?name
}
ORDER BY ?name
EOT
 , D => $example_foaf_uri
 , T => 'A FOAF query that finds the names of all the people in the graph'
}
  , 
{ Q => <<EOT
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?nick, ?name
WHERE { ?x rdf:type foaf:Person . ?x foaf:nick ?nick . ?x foaf:name ?name }
EOT
 , D => $example_foaf_uri
 , T => 'A FOAF query that finds all people with a name and an IRC nick'
}
  , 
{ Q => <<AAAAA
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX rss: <http://purl.org/rss/1.0/>
SELECT ?title ?description
WHERE { ?item rdf:type rss:item .
       ?item rss:title ?title .
       ?item rss:description ?description }
AAAAA
 , D => $example_rss_uri
 , T => 'An RSS 1.0 query that finds all the items in the feed'
},
{ Q => <<'CCCCC'
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX iemsr: <http://www.ukoln.ac.uk/projects/iemsr/terms/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT $number $name $description
WHERE {
  $r rdf:type iemsr:RootDataElement .
  $n iemsr:isChildOf $r .
  $n iemsr:refNumber $number .
  $n rdfs:label $name .
  $n rdfs:comment $description
}
CCCCC
 , D => 'http://www.ukoln.ac.uk/projects/iemsr/terms/LOM/'
 , T => 'Find all LOM root elements in the LOM encoded for the <a href="http://www.ukoln.ac.uk/projects/iemsr/">JISC IE Schema Registry</a> (my current project)'
},
{ Q => <<'DDDDD'
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX doap: <http://usefulinc.com/ns/doap#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT $description $maintainerName
WHERE {
  $project rdf:type doap:Project .
  $project doap:description $description .
  $project doap:maintainer $m .
  $m foaf:name $maintainerName
}
DDDDD
 , D => 'http://svn.usefulinc.com/svn/repos/trunk/doap/examples/gnome-bluetooth-doap.rdf'
 , T => 'Print the description of a project and maintainer(s) using <a href="http://usefulinc.com/doap">DOAP</a>'
},
{ Q => <<'EEEEE'
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX doaml: <http://ns.balbinus.net/doaml#>

SELECT ?name ?archives
WHERE {
  ?list rdf:type doaml:MailingList .
  ?list doaml:name ?name .
  ?list doaml:archives ?archives .
  FILTER REGEX(?name, "p3p")
}
EEEEE
 , D => 'http://www.doaml.net/doaml/w3ml/Lists.rdf'
 , T => 'Print the name and archive URIs of W3C mailing lists about P3P as described by <a href="http://www.doaml.net/">DOAML</a>'
},
{ Q => <<'optional-example1'
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?nick
WHERE {
  ?x rdf:type foaf:Person .
  ?x foaf:name ?name .
  OPTIONAL { ?x foaf:nick ?nick }
}
optional-example1
 , D => $example_foaf_uri,
 , T => 'Print the names and optional nicks of people in my FOAF file where available'
},
{ Q => <<'podcasts',
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX rss: <http://purl.org/rss/1.0/>
PREFIX enc: <http://purl.oclc.org/net/rss_2.0/enc#>
SELECT ?title ?enc ?len
WHERE {
      ?item rdf:type rss:item .
      ?item rss:title ?title .
      ?enclosure rdf:type enc:Enclosure .
      ?item enc:enclosure ?enclosure .
      ?enclosure enc:url ?enc .
      ?enclosure enc:type ?type .
      ?enclosure enc:length ?len .
      FILTER regex(?type, "audio/mpeg")
     }
podcasts
,
, D => 'http://B4mad.Net/datenbrei/feed/rdf',
, T => 'What podcasts have you got in your RSS feed?  (you will need an RSS feed using the enclosures vocab) ',
},
{ Q => <<'PERIODIC',
PREFIX table: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
PREFIX xsd:   <http://www.w3.org/2001/XMLSchema#>
SELECT ?name ?symbol ?weight ?number
WHERE {
 ?element table:group ?group .
 ?group table:name "Noble gas"^^xsd:string .
 ?element table:name ?name .
 ?element table:symbol ?symbol .
 ?element table:atomicWeight ?weight .
 ?element table:atomicNumber ?number
}
ORDER BY ASC(?name)
PERIODIC
, D => 'http://www.daml.org/2003/01/periodictable/PeriodicTable.owl',
, T => 'What are the Noble Gases?',
},
{ Q => <<'DAWG',
PREFIX collab: <http://www.w3.org/2000/10/swap/pim/collab@@#>
SELECT ?desc ?R
WHERE {
  ?issue collab:shortDesc ?desc;
  collab:resolveRecord ?R
}
DAWG
, D => 'http://www.w3.org/2000/06/webdata/xslt?xslfile=http%3A%2F%2Fwww.w3.org%2F2003%2F11%2Frdf-in-xhtml-processor&xmlfile=http%3A%2F%2Fwww.w3.org%2F2001%2Fsw%2FDataAccess%2Fissues',
, T => 'What are the RDF DAWG issues?',
},
{ Q => <<'ATOM',
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX rss: <http://purl.org/rss/1.0/>
PREFIX atom: <http://www.w3.org/2005/Atom>
SELECT ?item ?title ?date
WHERE { ?item rdf:type rss:item .
        ?item rss:title ?title .
        ?item atom:updated ?date }
ORDER BY DESC(?date)
LIMIT 10
ATOM
, D => 'http://www.tbray.org/ongoing/ongoing.atom',
, T => 'What are the last 10 updated items in an atom feed?',
},
{ Q => <<'BRUNEL',
PREFIX : <http://www.commonobjects.example.org/gmlrss>
PREFIX bio: <http://purl.org/vocab/bio/0.1/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?name ?birthDate ?deathDate
WHERE {
  ?bridge a :Bridge;
    foaf:maker ?person [
      foaf:name ?name;
      bio:event [
	a bio:Birth;
	bio:date ?birthDate
      ];
      bio:event [
	a bio:Death;
	bio:date ?deathDate
      ]
    ]
}
BRUNEL
, D => 'http://www.w3.org/2003/01/geo/rdfgml/tests/mixing-eg1.xml',
, T => 'Who made a bridge in Bristol and what birth/death dates did they have?',
},
{ Q => <<'REDLAND-NEWS',
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX rss: <http://purl.org/rss/1.0/>
SELECT ?item ?title ?date
WHERE {
  ?item a rss:item ;
        rss:title ?title ;
        dc:date ?date
}
ORDER BY DESC(?date)
LIMIT 10
REDLAND-NEWS
  D => 'http://librdf.org/NEWS.rdf http://librdf.org/raptor/NEWS.rdf http://librdf.org/rasqal/NEWS.rdf http://librdf.org/bindings/NEWS.rdf',
  T => 'Find the most recent 10 news items about Redland from multiple RSS 1.0 feeds',
},
  ]
);

my(%query_blurbs)=(
  'rdql' => <<EOT
  <p>Documentation on RDQL is available in the <a href="http://www.w3.org/Submission/2004/SUBM-RDQL-20040109/">specification</a> and the <a href="http://www.hpl.hp.com/semweb/doc/tutorial/RDQL/">Jena RDQL tutorial</a></p>

  <p>See the <a href="http://librdf.org/rasqal/TODO.html#rdql">status of RDQL support in Rasqal</a></p>
EOT
  ,
  'sparql' => <<EOT
  <p>Documentation on SPARQL is available in the <a href="http://www.w3.org/TR/rdf-sparql-query/">SPARQL Query Language for RDF</a>, W3C Working Draft, 12 October 2004</p>

  <p><b>NOTE</b> Not all of SPARQL is implemented. See the <a href="http://librdf.org/rasqal/TODO.html#sparql">status of SPARQL support in Rasqal</a>.  Current unimplemented features include <code>UNION</code> and full XSD datatype comparisons (dates, decimal) and promotions, <code>GRAPH</code> and deeply grouped graph patterns <code>{</code>...<code>}</code>.</p>
EOT
);



my $log_file="$::ROOT_DIR/logs/query.log";



my $max_stream_size=200;
my $max_result_size=200;

my(%namespaces)=(
  'rdf' => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs' => 'http://www.w3.org/2000/01/rdf-schema#',
  'dc' => 'http://purl.org/dc/elements/1.1/',
  'owl' => 'http://www.w3.org/2002/07/owl#',
#  'xsd' => 'http://www.w3.org/2001/XMLSchema#',
  'foaf' => 'http://xmlns.com/foaf/0.1/',
  'dcterms' => 'http://purl.org/dc/terms/',
  'bot' => 'http://www.w3.org/2001/sw/Europe/200401/bot/terms\#',
);


# Redland perl modules

use RDF::Redland;

#  $RDF::Debug=1;


######################################################################
# Subroutines

sub log_action ($$$;$) {
  my($host, $db, $message, $now)=@_;
  $now ||= time;
  return unless open (LOG, ">>$log_file");
  my($sec,$min,$hour,$mday,$mon,$year)=gmtime $now;
  my $date=sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",1900+$year,$mon+1,$mday,$hour,$min,$sec);
  $message =~ s/[\n\s]+/ /gs
    if $message;
  print LOG "$host $date $db $message\n";
  close(LOG);
}

sub end_page($) {
  my $q=shift;

  print <<'EOT';
<p>The source code of this demonstration is available in the Redland
bindings distribution as <tt>demos/query.pl</tt> or from the
<a href="http://librdf.org/">Redland</a> website</p>
EOT

  print qq{<hr />\n\n<p class="copyright"><a href="http://purl.org/net/dajobe/">Dave Beckett</a></p>\n\n</div></body>\n</html>\n};
}

sub html_escape ($) {
  my $str=shift;
  $str =~ s/\&/\&amp;/g;
  $str =~ s/</\&lt;/g;
  $str =~ s/</\&gt;/g;
  $str;
}

sub format_node($) {
  my $node=shift;
  my $node_label;

  if(!defined $node) {
    $node_label="&nbsp;";
  } elsif($node->is_resource) {
    $node_label=$node->uri->as_string;
    $node_label=qq{<a href="$node_label">$node_label</a>};
  } elsif ($node->is_literal) {
    $node_label=$node->literal_value_as_latin1;
    if($node->literal_value_language) {
      $node_label.="@".$node->literal_value_language;
    }
    if($node->literal_datatype && 
       !$node->literal_value_is_wf_xml) {
      $node_label.="^^&lt;".$node->literal_datatype->as_string."&gt;";
    }
  } elsif ($node->is_blank) {
    $node_label="blank node ".$node->blank_identifier;
  } else {
    $node_label=$node ? $node->as_string : "undef";
  } 
  $node_label;
}

sub print_bindings_results($) {
  my $results=shift;

  print qq{<p>Variable bindings result format</p>\n\n};

  my $width=$results->bindings_count;
  my $count=0;
  my(@variables)=();
  for(my $i=0; $i < $width; $i++) {
    my $name=$results->binding_name($i);
    push(@variables, $name) if $name;
  }
  
  if(@variables) {
    my $t=join('</th> <th>', @variables);
    print <<"EOT";
<center>
<table align="center" border="1">
<tr>
<th>Count</th>
<th>$t</th>

</tr>
EOT

    while(!$results->finished) {
      $count++;
      
      print qq{<tr><td>$count</td>\n};
      for(my $i=0; $i < $results->bindings_count; $i++) {
	my $label=format_node($results->binding_value($i));
	print "<td>$label</td>";
      }
      print "</tr>\n";
      $results->next_result;
      
      if ($count >= $max_result_size) {
	my $w=$width+1;
	print << "EOT";
<tr>
<td colspan="$w">Truncated at $max_result_size items.</td>
</tr>
EOT
        last;
      }
    }
    $results=undef;

    print <<"EOT";
</table>
</center>
EOT
  } # end if variables

  my $pl=($count != 1) ? 's' : '';
  print "\n\n<p>Found $count result$pl</p>\n";
}


sub print_graph_results($) {
  my $results=shift;
  my $stream=$results->as_stream;

  print qq{<p>Graph result format</p>\n\n};

  print <<"EOT";
<center>
<table align="center" border="1">
<tr>
<th>Subject</th>
<th>Predicate</th>
<th>Object</th>
</tr>
EOT

  my $count=0;
  for(;!$stream->end ;  $stream->next) {
    my $statement=$stream->current;
    
    last if !$statement;
    
    my $subject_label=format_node($statement->subject);
    my $predicate_label=format_node($statement->predicate);
    my $object_label=format_node($statement->object);
    
    print << "EOT";
<tr>
<td>$subject_label</td>
<td>$predicate_label</td>
<td>$object_label</td>
</tr>
EOT

    $count++;
    if ($count >= $max_stream_size) {
      print << "EOT";
<tr>
<td colspan="3">Truncated at $max_stream_size items - sorry, this is just a demonstration.</td>
</tr>
EOT
      last;
    }

  } # while


  print <<"EOT";
</table>
</center>
EOT

  my $pl=($count != 1) ? 's' : '';
  print "\n\n<p>Found $count triple$pl</p>\n";

}


sub print_boolean_results($) {
  my $results=shift;

  print qq{<p>Boolean result format</p>\n\n};

  my $bresult=$results->get_boolean;
  print "<p>Result: ",$bresult ? "True" : "False","</p>\n\n";
}    


######################################################################
# Main code

my $q = new CGI;

# CGI parameter paranoia
my $val;

my $uri_string;
$val=$q->param('uri');
if(defined $val && $val =~ /^([ -~]+)$/) {
  $uri_string=$1;
} else {
  $uri_string=undef;
}

my $query_string='';
$val=$q->param('query');
if(defined $val && $val =~ /^(.*)$/s) {
  $query_string=$1;
}

my $query_language=$default_query_language;
$val=$q->param('language');
if(defined $val && $val =~ /^(rdql|sparql)$/s) {
  $query_language=$1;
}

my $raw=0;
$val=$q->param('raw');
if(defined $val && $val =~ /^(0|1)$/s) {
  $raw=$1;
}

my $qname=1;

# End of parameter decoding


my $execute='yes';
$val=$q->param('execute');
if(defined $val && $val eq 'no') {
  $execute='no';
}

# Used in logging
my $host=$q->remote_host;


######################################################################
# Emit content

print $q->header(-type  =>  'text/html', -charset=>'utf-8');


# Always print header
print <<"EOT";
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <title>Redland Rasqal RDF Query Demonstration</title>
  <!-- HTML STYLE -->
</head>
<body>

<!-- LOGO START -->
<h1>Redland Rasqal RDF Query Demonstration</h1>
<!-- LOGO END -->

<p>This is a demonstration of using
<a href="http://librdf.org/rasqal/">Rasqal</a>
to perform queries in either of the RDQL or SPARQL languages over RDF data.
The data is loaded into a <a href="/">Redland</a> model and then 
queried and results accessed via the
<a href="http://librdf.org/docs/perl.html">Redland Perl</a>
language binding.
</p>

EOT

# use q->url() to get URL of this script without any query parameters
# since we are using a POST here and don't want them added to the
# submission URL.
#my $action_url="/".$q->url(-relative=>1);
my $action_url="/query";

my $query_language_label=$query_language_labels{$query_language};

if($execute eq 'no') {
  print <<"EOT";
<div>
<table border="0" cellspacing="0" cellpadding="0" summary="Main navigation" width="100%" summary="" class="headingbox">
<tr>
<td class="mainnavback"><a class="mainnavlink" href="/query?language=rdql">Query with RDQL</a></td>
<td class="mainnavback"><a class="mainnavlink" href="/query?language=sparql">Query with SPARQL</a></td>
</tr>
</table>
</div>
EOT
}

print <<"EOT";
<p>Firstly choose whether to query in
<a href="/query?language=rdql">RDQL</a> or
<a href="/query?language=sparql">SPARQL</a>, then
enter a URI of some RDF content to query and run it.
The example query is for FOAF and finds the
names of all the people in the graph.  There are other example
queries further down the page.</p>

<p>Some RDF content you can use could be my FOAF file at:
$example_foaf_uri<br />
or the W3C\'s RSS 1.0 file at:
$example_rss_uri</p>

EOT
print $q->start_form(-method=>'GET', -action => $action_url, -name => 'myform'),"\n";
print "<p><em>RDF content URIs</em><br/>\n";
print $q->textfield(-name=>'uri',
		      -default=>'',
		      -size=>100,
		      -maxlength=>1024);
my $examples=$query_examples{$query_language};

print "</p>\n\n<p><em>Query</em><br/>\n";
print $q->textarea(-name=>'query',
		    -default=>$examples->[0]->{Q},
		    -columns=>100,
		    -rows=>10);

print "</p>\n\n<p>in query language:\n";
print $q->radio_group(-name=>'language', 
			-values=>\@query_languages,
			-default=>$default_query_language, 
			-labels=>\%query_language_labels);

print "&nbsp;&nbsp;";
print $q->checkbox(-name=>'raw',
		   -checked=>0,
		   -value=>'1',
		   -label=>'raw syntax output');

print "</p>\n\n<p>";

print $q->submit('Run Query'),"\n";
print qq{<input type="button" value="Clear Query" onclick="document.myform.query.value=''" />\n};

print "</p>\n";
print $q->endform,"\n";


if($query_string && $uri_string) {
  print <<"EOT";
<p>See example queries at the
<a href="$action_url">Rasqal Query Demonstration home</a></p>
EOT
} else {
  shift(@{$examples});

  my $query_blurb=$query_blurbs{$query_language};

  print <<"EOT";
$query_blurb
  
  <p>Some other $query_language_label example queries:</p>
<ol>
EOT
  for my $qe (@{$examples}) {
    my $t=$qe->{T} || '';
    my $d=$qe->{D};
    $t="<em>$t</em><br />\n" if $t;
    print "<li>${t}Data: $d<br />\nQuery:";
    print "<pre>\n". html_escape($qe->{Q}) ."</pre>\n";

    $q->param('uri', $d);
    $q->param('query', $qe->{Q});    
    $q->param('language', $query_language);
    my $query_uri=$q->self_url;
    print qq{<a href="$query_uri">Run this query</a>\n};

    print "</li>\n\n";
  }

  print <<"EOT";
</ol>
EOT

}


# Any parameters?

if(!$q->param) {
  end_page($q);
  exit 0;
}

if(!$query_string || !$uri_string) {
  if ($query_string || $uri_string) {
    print "<hr />\n\n<h2>Error</h2>\n";
    print "\n\n<p>Got a query string but no RDF content URIs.</p>\n" 
      if $query_string;
    print "\n\n<p>Got RDF content URIs but no query string.</p>\n" 
      if $uri_string;
  }
  end_page($q);
  exit 0;
}

print "<hr />\n\n<h2>Results</h2>\n";

######################################################################

# Validate me
if($execute eq 'no') {
  end_page($q);
  exit 0;
}

my $model=undef;
my $storage=undef;

my $db='temp';
my $store_type="hashes";
my $options=join(',' , "contexts='yes'", "hash-type='memory'");

my(@errors)=();
my(@warnings)=();

my $handler=sub {
  my($code, $level, $facility, $message, $line, $column, $byte, $file, $uri)=@_;
  if($level < 4) {
    push(@warnings, [$message, $line]);
  } else {
    push(@errors, [$message, $line]);
  }

  #print "code $code\n";
  #print "level $level\n";
  #print "facility $facility\n";
  #print "message: $message\n" if defined $message;
  #print "line $line\n";
  #print "column $column\n";
  #print "byte $byte\n";
  #print "file $file\n" if defined $file;
  #print "uri $uri\n" if defined $uri;
};

RDF::Redland::set_log_handler($handler);


$storage=new RDF::Redland::Storage($store_type, $db, $options);
if($storage) {
  $model=new RDF::Redland::Model($storage, "");
}
if(!$storage && !$model) {
  log_action($host, $db, "Failed to open database");
  print "\n\n<p>Sorry - failed to open RDF Database.  This problem has been recorded.</p>\n";
  end_page($q);
  exit 0;
}


my $uri=undef;
if($uri_string) {
  for my $u (split(/ /, $uri_string)) {
    if($u !~ /^(ftp|http):/i) {
      push(@warnings, ["Ignored non-ftp/http URI $u", 0]);
      next;
    }

    $uri=new RDF::Redland::URI($u);
    my $parser=new RDF::Redland::Parser("guess");
    eval { $parser->parse_into_model($uri, $uri, $model); };
    if($@ || @errors) {
      my $err=join("<br />", map {$_->[0]} @errors);
      print "\n\n<p><b>Loading URI $u failed with errors:</b><br />\n$err</p>\n";
      end_page($q);
      exit 0;
    }
    $parser=undef;
  }
}

my $statement=undef;
my $stream=undef;

my(@context_nodes);

if(!$query_string || !$uri_string) {
  end_page($q);
  exit 0;
}

log_action($host, $db, "Data $uri_string with query '$query_string' in language $query_language");
if($query_string =~ m%\s+(from)\s+%i) {
  my $fr=$1;
  log_action($host, $db, "Query '$query_string' contains 'from' - ignoring it");
  print "\n\n<p>The query string contains '$fr'; ignoring it</p>\n";
  end_page($q);
  exit 0;
}


@errors=();

my $query=undef;
my $base_uri=new RDF::Redland::URI("http://librdf.org/query");
eval '$query=new RDF::Redland::Query($query_string, $base_uri, undef, $query_language);';
if($@ || @errors || !$query) {
  my $err=join("<br />", map {$_->[1].":".$_->[0]} @errors);
  print "\n\n<p><b>$query_language_label query construction failed with errors:</b><br />\n$err</p>\n";
  end_page($q);
  exit 0;
}
$base_uri=undef;

$q->delete('command');
$q->delete('query');
$q->delete('language');

my $results=undef;
eval '$results=$model->query_execute($query);';
if($@ || @errors || !$results) {
  my $err=join("<br />", map {$_->[1].":".$_->[0]} @errors);
  print "\n\n<p><b>$query_language_label query execution failed with errors:</b><br />\n$err</p>\n";
  end_page($q);
  exit 0;
}

if(@warnings) {
  my $w=join("<br />", map {$_->[1].":".$_->[0]} @warnings);
  print "<p><b>$query_language_label warnings:</b><br />\n$w</p>\n";
}


if($raw) {
  my $base_uri=new RDF::Redland::URI('http://librdf.org/query/');
  my $str=$results->to_string;

  if(defined $str) {
    print qq{<pre>\n} . html_escape($str) .qq{\n</pre>\n};
  } else {
    print qq{<p>No raw format returned\n</p>};
  }

} else {

  if($results->is_bindings) {
    print_bindings_results($results);
  } elsif($results->is_graph) {
    print_graph_results($results);
  } elsif($results->is_boolean) {
    print_boolean_results($results);
  } else {
    print qq{<p>Unknown results format - cannot display</p>\n\n};
  }
  
}

end_page($q);
exit 0;
