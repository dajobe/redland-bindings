# -*- Mode: Perl -*-
#
# RSS.pm - Redland Perl RSS 1.0 module
#
# Copyright (C) 2000-2003 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2003 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL)
#   2. GNU General Public License (GPL)
#   3. Mozilla Public License (MPL)
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 
#

package RDF::Redland::RSS;

use strict;


use RDF::Redland;

use vars qw(@ISA $NS_URL $DC_NS_URL);

@ISA=qw(RDF::Redland::Model);

$NS_URL="http://purl.org/rss/1.0/";
$DC_NS_URL="http://purl.org/dc/elements/1.1/";

=pod

=head1 NAME

RDF::Redland::RSS - Redland RSS 1.0 Class

=head1 SYNOPSIS

  use RDF::Redland::RSS;

  ...
  my $rss=RDF::Redland::RSS->new_from_model($model);

  my $rss2=new RDF::Redland::RSS("http://example.com/test.rdf");
  ...

  for my $channel ($rss->channels) {
    ...
   print "channel title is ",$channel->title->as_string,"\n"; # UTF-8
  }

  my(@items)=$channel->items;
  # Print channel items (URI, title)
  for my $item (@items) {
    print "item ",$item->uri->as_string, " ", $item->title->as_string, "\n";
  }

  ...

  print $rss->as_xhtml(width => 320, align => 'right');

=head1 DESCRIPTION

A class for processing RSS 1.0 as RDF, traversing the resulting
graph using RSS propertiiies and formatting the output as XHTML.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

=over

=item new SOURCE_URI_STRING [BASE_URI_STRING]

Process RSS 1.0 at source URI I<SOURCE_URI_STRING>. If the 
I<BASE_URI_STRING> string is given then use that as the base URI
rather than the source URI.

=cut

# Do it all here, create storage (in memory), model, parse it.
sub new ($$;$) {
  my($proto,$source_uri_string,$base_uri_string)=@_;
  my $class = ref($proto) || $proto;

  $base_uri_string ||= $source_uri_string;

  my $base_uri=new RDF::Redland::URI $base_uri_string;
  return undef if !$base_uri;

  my $source_uri=new RDF::Redland::URI $source_uri_string;
  return undef if !$source_uri;

  my $storage=new RDF::Redland::Storage("hashes", "rss", 
			       "new='yes',write='yes',hash-type='memory'");
  return undef if !$storage;
  
  my $self=new RDF::Redland::Model($storage, "");
  return undef if !$self;

  my $parser=new RDF::Redland::Parser('raptor');
  return undef if !$parser;

  $parser->parse_into_model($source_uri, $base_uri, $self);

  bless ($self, $class); # reconsecrate as RDF::Redland::RSS
  return $self;
}


=item new MODEL

Process RSS 1.0 from content stored in RDF::Redland::Model I<MODEL>.

=cut

sub new_from_model ($$) {
  my($proto,$model)=@_;
  my $class = ref($proto) || $proto;
  my $self  = $model;

  bless ($self, $class); # reconsecrate as RDF::Redland::RSS
  return $self;
}

=pod

=back

=cut

# DESTRUCTOR
sub DESTROY ($) {
  my $self=shift;
  $self->SUPER::DESTROY();
  warn "RDF::Redland::RSS DESTROY\n" if $RDF::Redland::Debug;
}



sub _find_by_type ($$) {
  my($self,$type_value)=@_;

  my $rdf_type=RDF::Redland::Node->new_from_uri_string("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
  return () if !$rdf_type;

  my $object=RDF::Redland::Node->new_from_uri_string($type_value);
  return () if !$object;

  my(@results)=$self->sources($rdf_type, $object);

  # Turn the nodes into RDF::Redland::RSS:Node-s
  @results=map { RDF::Redland::RSS::Node->new($self,$_) } @results;

  return(@results);
}


=head1 METHODS

=over

=item channels

Return the RSS channels (E<lt>channelE<gt> tags) as a list of
RDF::Redland::RSS::Node objects.

=cut

sub channels ($) {
  shift->_find_by_type($NS_URL.'channel');
}

=item items

Return the RSS items (E<lt>itemE<gt> tags) as a list of
RDF::Redland::RSS::Node objects.

=cut

sub items ($) {
  shift->_find_by_type($NS_URL.'item');
}

=item image

Return the RSS 1.0 image (E<lt>imageE<gt> tag) as an
RDF::Redland::RSS::Node object.

=cut

sub image ($) {
  shift->_find_by_type($NS_URL.'image');
}

=item textinput

Return the RSS 1.0 textinput (E<lt>textinputE<gt> tag) as an
RDF::Redland::RSS::Node object.

=cut

sub textinput ($) {
  shift->_find_by_type($NS_URL.'textinput');
}

=item as_xhtml (key1 => value1, key2 => value2, ...)

Return a formatted XHTML string (or full XHTML document) representing
the RSS 1.0 content with various options set as listed in the section
below.

The parameters to this method are mostly from the specification of
the viewRssBox macro at http://macros.userland.com/viewRssBox

=back

=head1 AS_XHTML OPTIONS

=over

=item boxTitle

A string, is the text displayed in the title of the box. It defaults
to the title element of the channel.

=item align

A string, has three possible values, left, right or the empty
string. The HTML table is either left-aligned, right-aligned or not
aligned. It defaults to the empty string.

=item width

A number, is the width of the box, in pixels. It defaults to
125. Note that the title bar determines the minimum width of the box,
if you're having trouble getting it to be narrower, try shortening
boxTitle.

=item frameColor

A string, is the hex browser color for the frame of the box. Defaults
to "#000000".

=item titleBarTextColor

A string, is the hex browser color for the text in the title bar of
the box. Defalults to "#000000".

=item titleBarColor

A string, is the hex browser color for the title bar of the
box. Defaults to "#ADD8E6".

=item boxFillColor

A string, is the hex browser color for the main part of the
box. Defaults to "#FFFFFF".

=item time

A string, is text that's displayed as the time to the right of the
box title. Defaults to "".

=item hspace

A number, is the number of pixels to the left and right of the
box. Defaults to 0.

=item vspace

A number, is the number of pixels above and below the box. Defaults
to 0.

=item full

If set to any value, returns a full XHTML document.  Defaults to
returning an HTML fragment.

=item imageAlign

A string, has two possible values, left or right. The channel image
is either left-aligned or right-aligned. It defaults to right
aligned.


=back

=cut

sub as_xhtml ($%) {
  my($self,%opts)=@_;
  my $url=$opts{url};

  sub format_literal ($) {
    my $string=shift;
    return 'UNDEFINED' if !$string;
    $string=$string->literal_value;
    return '' if !defined $string || !length $string;
    # No need for HTML::Entities here for four things
    $string =~ s/\&/\&amp;/g;
    $string =~ s/</\&lt;/g;
    $string =~ s/>/\&gt;/g;
    $string =~ s/"/\&quot;/g;
    $string;
  }

  sub format_url($) { shift->as_string; }


  my($channel)=$self->channels;
  return '' unless $channel;

  my(@items)=$channel->items;
  return '' unless @items;

  my $boxTitle=$opts{boxTitle} || ($channel->title && format_literal($channel->title)) || '';
  my $align=$opts{align} || '';
  my $image_align=$opts{imageAlign} || 'right';
  my $width=$opts{width} || '125';
  my $frameColor=$opts{frameColor} || '#000000';
  my $titleBarTextColor=$opts{titleBarTextColor} || '#000000';
  my $titleBarColor=$opts{titleBarColor} || '#ADD8E6';
  my $boxFillColor=$opts{boxFillColor} || '#FFFFFF';
  my $time=$opts{'time'} || '';
  my $hspace=$opts{hspace} || 0;
  my $vspace=$opts{vspace} || 0;
  my $full=$opts{full};

  my $dc_date=RDF::Redland::Node->new_from_uri_string('http://purl.org/dc/elements/1.1/date');
  if(!$time) {
    $time=$channel->property($dc_date);
    $time=$time ? format_literal($time) : '';
  }

  my $width_opt=($width eq 'infinity' ? '' : qq{ width="$width"});
  my $align_opt=$align ? qq{ align="$align"} : '';
  my $time_fmt=($time eq '') ? '&nbsp;' : qq{<font color="$titleBarTextColor" size="-1">$time</font>};
  my $channel_title_fmt=qq{<b><font color="$titleBarTextColor">$boxTitle</font></b>};


  my $ch_uri=$channel->uri ? format_url($channel->uri) : 'No URI';
  my $ch_link=$channel->link ? format_url($channel->link) : 'No Link';
  my $ch_desc=$channel->description ? $channel->description->literal_value : undef;

  my $ch_desc_fmt=$ch_desc ? qq{<p>$ch_desc</p>\n\n} : '';


  my $output='';

  $output.=<<"EOT" if $full;
<html>
<head>
<title>$boxTitle</title>
</head>
<body bgcolor='#FFFFFF' fgcolor='#000000'>

<h1>$boxTitle</h1>

<p>Converted from RSS 1.0 content at $ch_uri by 
<a href="http://purl.org/net/redland/">Redland</a> <tt>RDF::Redland::RSS</tt>
module <em>as_xhtml</em> method</p>

EOT


  $output.=<<"EOT";

<!-- 
RSS 1.0 content at $ch_uri rendered to XHTML by
Redland - http://purl.org/net/redland/
RDF::Redland::RSS module as_xhtml method
-->
EOT

  if($vspace >=0 || $hspace >=0) {
    $output.=<<"EOT";
<table summary="RSS 1.0 content from $boxTitle" "cellpadding="0" cellspacing="0" border="0" $align_opt>
  <!-- Top of frame -->
  <tr>
     <td height="$vspace" colspan="3">&nbsp;</td>
  </tr>
  <tr>
     <!-- Left of frame -->
     <td width="$hspace">&nbsp;</td>
     <td><table$width_opt cellspacing="0" cellpadding="1" border="0">
        <tr bgcolor="$frameColor">
          <td$width_opt>
            <!-- Start of main content frame -->
EOT
    $align_opt='';
  }

  $output.=<<"EOT";
<table$width_opt cellspacing="0" cellpadding="7" border="0"$align_opt>
EOT

  my $image=$channel->image;
  my $image_string='';
  if($image) {
    my $image_title=$image->title ? format_literal($image->title) : undef;
    my $image_link=$image->link ? format_url($image->link) : undef;
    my $image_url=$image->image_url ? format_url($image->image_url) : undef;

    if ($image_title && $image_link && $image_url) {
      $image_string=qq{<a href="$image_link"><img src="$image_url" alt="$image_title" align="$image_align" /></a>\n       }
    } else {
      $output.=qq{  <-- image missing some fields -->\n};
    }
  }

  $output.= <<"EOT";
  <!-- Title Bar -->
  <tr bgcolor="$titleBarColor">
    <td$width_opt nowrap="nowrap">
       $image_string<a href="$ch_uri">$channel_title_fmt</a>
    </td>
    <td>&nbsp;</td>
    <td nowrap="nowrap" align="right">$time_fmt</td>
  </tr>

  <!-- Title Bar Frame  -->
  <tr bgcolor="$frameColor">
     <td$width_opt colspan="3" height="1"></td>
  </tr>

  <!-- Content -->
  <tr bgcolor="$boxFillColor">
    <td$width_opt colspan="3">

      $ch_desc_fmt

      <ul>
EOT

  my $dc_source=RDF::Redland::Node->new_from_uri_string('http://purl.org/dc/elements/1.1/source');
  my $dc_subject=RDF::Redland::Node->new_from_uri_string('http://purl.org/dc/elements/1.1/subject');
  for my $item (@items) {
    my $item_title=$item->title ? format_literal($item->title) : undef;
    my $item_link=$item->link ? format_url($item->link) : undef;
    my $item_desc=$item->description ? $item->description->literal_value : '';

    if(my $date=$item->property($dc_date)) {
      $item_desc.="<br />" if $item_desc;
      $item_desc.="<em>".format_literal($date)."</em>";
    }

    if(my $source=$item->property($dc_source)) {
      $item_desc.="<br />" if $item_desc;
      $item_desc.="<small>Source: ".format_literal($source)."</small>";
    }
    
    if(my $subject=$item->property($dc_subject)) {
      $item_desc.="<br />" if $item_desc;
      my $fmt_subject;
      if($subject->type eq $RDF::Redland::Node::Type_Literal) {
        $fmt_subject=format_literal($subject);
      } else {
        $fmt_subject=format_url($subject);
        $fmt_subject=qq{<a href="$fmt_subject">$fmt_subject</a>};
      }
      $item_desc.="<small>Subject: $fmt_subject</small>";
    }

    if($item_link && $item_title) {
      $output.=qq{        <li><a href="$item_link">$item_title</a>};
      $output.=": $item_desc" if $item_desc;
      $output.=qq{</li>\n};
    } else {
      $output.=qq{        <-- item missing some fields -->\n};
    }
  }


  $output.=qq{      </ul>\n\n};



  my $textinput=$channel->textinput;
  if($textinput) {
    my $t_uri=$textinput->uri;
    my $t_title=$textinput->title;
    my $t_link=$textinput->link;
    my $t_desc=$textinput->description;
    my $t_name=$textinput->name;

    if($t_uri && $t_title && $t_link && $t_desc && $t_name) {
      my $t_uri_string=format_url($t_uri);
      my $t_name_string=format_literal($t_name);
      my $t_desc_string=$t_desc ? $t_desc->literal_value : '';
      my $t_title_string=format_literal($t_title);

      $output.= <<"EOT";
      <form method="get" action="$t_uri_string">
        <b>$t_title_string</b><br />
        $t_desc_string
        <input type="text" name="$t_name_string" />
        <input type="submit" name="Go" value="Go" />
      </form>
EOT
    } else {
      $output .= qq{      <!-- textinput missing some fields -->\n};
    }
  }


$output.=<<"EOT";
    </td>
  </tr>
</table>
EOT

  if($vspace >=0 || $hspace >=0) {
    $output.=<<"EOT";
            <!-- End of main content frame -->
        </td>
      </tr>
    </table></td>
     <!-- Right of frame -->
    <td width="$hspace">&nbsp;</td>
  </tr>
  <!-- Bottom of frame -->
  <tr>
    <td height="$vspace" colspan="3">&nbsp;</td>
  </tr>
</table>
EOT
  }

  $output.=<<"EOT" if $full;

</html>
</body>
EOT
 

  $output;
}



package RDF::Redland::RSS::Node;

use RDF::Redland::Node;

use vars qw(@ISA);

@ISA=qw(RDF::Redland::Node);

=head1 NAME

RDF::Redland::RSS::Node - Redland RSS 1.0 Node Class

=head1 DESCRIPTION

Class representing concepts in an RSS 1.0 RDF graph.

=cut

######################################################################

=pod

=head1 CONSTRUCTORS

No public constructors.  Nodes are created either by methods of
this class or RDF::Redland::RSS.

=cut

sub new ($$$) {
  my($proto,$model,$node)=@_;
  my $class = ref($proto) || $proto;

  return undef if !$model || !$node;

  warn "RDF::Redland::RSS::Node::new in model $model with node $node\n" if $RDF::Redland::Debug;

  my $self=$node;

  $self->{MODEL}=$model;

  bless ($self, $class); # reconsecrate as RDF::Redland::RSS::Node
  return $self;
}

sub DESTROY ($) {
  my $self=shift;
  $self->SUPER::DESTROY();
  warn "RDF::Redland::RSS::Node DESTROY\n" if $RDF::Redland::Debug;
}


sub _find_targets_by_predicate ($$) {
  my($self,$uri_string)=@_;
  warn "RDF::Redland::RSS::_find_targets_by_predicate from $self with predicate $uri_string\n" if $RDF::Redland::Debug;

  my $predicate=RDF::Redland::Node->new_from_uri_string($uri_string);
  return () if !$predicate;

  my(@targets)=$self->{MODEL}->targets($self,$predicate);
  warn "RDF::Redland::RSS::_find_targets_by_predicate returned ",scalar @targets, " results\n" if $RDF::Redland::Debug;

  # Convert list of RDF::Redland::Node-s into list of RDF::Redland::RSS::Node-s
  return map { RDF::Redland::RSS::Node->new($self->{MODEL}, $_) } @targets;
}

=head1 METHODS

=over

=item title

Get the RSS titles for channel, image, item or textinput.
Returns either a list or first one found depending on calling context.

=cut

# Convienience accessors
# for all (channel, image, item, textinput) resources
sub title ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'title');
  return wantarray ? @r : $r[0];
}

=item link

Get the RSS link for channel, image, item or textinput.
Returns either a list or first one found depending on calling context.

=cut

sub link ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'link');
  return wantarray ? @r : $r[0];
}

=item description

Get the Dublin Core description element or RSS description for
channel, item or textinput.  Returns either a list or first one found
depending on calling context.

=cut

# for channel, item, textinput resources
sub description ($) {
  my $node=shift;
  my(@r);
  @r=$node->_find_targets_by_predicate($RDF::Redland::RSS::DC_NS_URL.'description');
  return (wantarray ? @r : $r[0]) if @r;

  @r=$node->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'description');
  return wantarray ? @r : $r[0];
}

=item image_url

Get the RSS image URL string for an image.
Returns either a list or first one found depending on calling context.

=cut

# for image resources
sub image_url ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'url');
  return wantarray ? @r : $r[0];
}

=item name

Get the RSS name for a textinput.
Returns either a list or first one found depending on calling context.

=cut

# for textinput resources
sub name ($) {
  my(@r)=shift->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'name');
  return wantarray ? @r : $r[0];
}


# -------------------------
# Methods for channels only

=item items

Get the RSS items in a channel as a list of RDF::Redland::RSS::Node objects.

=cut

# always returns a list of nodes
sub items ($) {
  my $self=shift;

  my $items_predicate=RDF::Redland::Node->new_from_uri_string($RDF::Redland::RSS::NS_URL.'items');
  return () if !$items_predicate;

  # Get 1st resource inside <items> - i.e. the 1st rdf:Seq
  my $seq_resource=$self->{MODEL}->target($self, $items_predicate);
  return () if !$seq_resource;

  $seq_resource=RDF::Redland::RSS::Node->new($self->{MODEL}, $seq_resource);

  # Find all rdf:_<n> properties from <rdf:Seq>
  my(@resources);
  for my $prop ($seq_resource->properties) {
    if($prop->uri->as_string =~ m%^http://www.w3.org/1999/02/22-rdf-syntax-ns#_(\d+)$%) {
       push(@resources,
	    [$1, $seq_resource->{MODEL}->target($seq_resource, $prop)]
	    );
     }
  }

  # In order - sort them by ordinal, convert to RDF::Redland::RSS::Node objects
  # and return
  return map {RDF::Redland::RSS::Node->new($self->{MODEL}, $_->[1])}
              sort {$a->[0] <=> $b->[0]} @resources;
}

=item image

Get the image of a channel as an RDF::Redland::RSS::Node object or undef
if not present.

=cut

# for channel (0 or 1 allowed)
sub image ($) {
  my $self=shift;

  my($image)=$self->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'image');
  return undef if !$image;

  return RDF::Redland::RSS::Node->new($self->{MODEL}, $image);
}


=item textinput

Get the textinput of a channel as an RDF::Redland::RSS::Node object or undef
if not present.

=cut

# for channel (0 or 1 allowed)
sub textinput ($) {
  my $self=shift;

  my($textinput)=$self->_find_targets_by_predicate($RDF::Redland::RSS::NS_URL.'textinput');
  return undef if !$textinput;

  return RDF::Redland::RSS::Node->new($self->{MODEL}, $textinput);
}


=item property PROPERTY

Get the value of the named property off an RDF::Redland::RSS::Node where
I<PROPERTY> is an RDF::Redland::Node or RDF::Redland::RSS::Node.  Returns a list of
RDF::Redland::RSS::Node objects or first one found depending on calling
context.

=cut

# general property
sub property ($$) {
  my($self,$property)=@_;

  if(wantarray) {
    my(@targets)=$self->{MODEL}->targets($self,$property);
    
    # Convert list of RDF::Redland::Node-s into list of RDF::Redland::RSS:Node-s
    return map { RDF::Redland::RSS::Node->new($self->{MODEL}, $_) } @targets;
  } else {
    my $target=$self->{MODEL}->target($self,$property);
    return RDF::Redland::RSS::Node->new($self->{MODEL}, $target);
  }
}

=item properties

Get all properties off the RDF::Redland::RSS::Node.  Returns a list of
RDF::Redland::RSS::Node objects.

=cut

sub properties ($) {
  my($self)=@_;

  my $statement=new RDF::Redland::Statement($self, undef, undef);
  my(@arcs_out_statements)=$self->{MODEL}->find_statements($statement);

  # Convert list of RDF::Redland::Statement-s into a list of
  # RDF::Redland::RSS:Node-s predicates.

  return map { new RDF::Redland::RSS::Node($self->{MODEL}, $_->predicate) }
           @arcs_out_statements;
}

=item properties_with_ns_prefix NS_PREFIX

Get all unique properties of the RDF::Redland::RSS::Node which have
namespace URI prefix I<NS_PREFIX>.  Returns a list of
the properties as RDF::Redland::RSS::Node objects.

=cut

sub properties_with_ns_prefix ($$) {
  my($self,$ns_prefix)=@_;

  my(@arcs);
  my(%arcs_uris_seen);
  for my $arc ($self->properties) {
    my $uri=$arc->uri->as_string;
    next unless $uri =~ m%^$ns_prefix(.*)$% && !$arcs_uris_seen{$uri};
    push(@arcs, $arc);
    $arcs_uris_seen{$uri}=1;
  }
  @arcs;
}

=pod

=back

=head1 SEE ALSO

L<RDF::Redland::Model> and RSS 1.0 web pages at http://purl.org/rss/1.0/

=head1 AUTHOR

Dave Beckett - http://www.dajobe.org/

=cut

1;
