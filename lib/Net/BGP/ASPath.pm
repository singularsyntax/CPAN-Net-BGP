#!/usr/bin/perl

package Net::BGP::ASPath::AS;
use bytes;

use strict;
use Carp;
use Exporter;
use vars qw(
    $VERSION @ISA
);

use overload
	'<=>' => \&compare,
	'""'  => \&asstring,
	'fallback' => 1;
	# DO NOT OVERLOAD @{} - it's an array - we need this!

$VERSION = '0.07';

use Net::BGP::Notification qw( :errors );

@Net::BGP::ASPath::AS_SEQUENCE::ISA     = qw( Exporter );

## BGP AS_PATH Path Attribute Type Classes ##

my @BGP_PATH_ATTR_CLASS = (
	undef,					# unused
	'Net::BGP::ASPath::AS_SET',		# BGP_PATH_ATTR_AS_SET
	'Net::BGP::ASPath::AS_SEQUENCE',	# BGP_PATH_ATTR_AS_SEQUENCE
	'Net::BGP::ASPath::AS_CONFED_SEQUENCE',	# BGP_PATH_ATTR_AS_CONFED_SEQUENCE
	'Net::BGP::ASPath::AS_CONFED_SET'	# BGP_PATH_ATTR_AS_CONFED_SET
	);

## Public Class Methods ##

sub new
{
    my ($class,$value) = (shift,shift);

    return $value->clone if (ref $value) =~ /^Net::BGP::ASPath::AS_/;

    my ($this,$realclass);

    $value = '' unless defined($value);

    if (ref $value eq 'HASH')
     {
      # Construct SET from HASH
      croak "Hash argument given for a non-set AS_PATH element" unless $class =~ /_SET$/;
      $this->{keys %{$value}} = values(%{$value});
      bless($this,$class);
      return $this;
     };

    if (ref $value eq 'ARRAY')
     {
      # Construct SET from HASH
      if ($class =~ /_SEQUENCE$/)
       {
        push(@{$this},@{$value});
       }
      else
       {
        $this = {};
        foreach my $a (@{$value}) { $this->{$a} = 1; };
       };
      bless($this,$class);
      return $this;
     };

    croak "Unknown argument type (" . (ref $value) . ") parsed as argument to AS_PATH construtor."
	if (ref $value);

    # Only a scalar left - Parse string!
    my $confed = '';
    if (($value =~ /^\((.*)\)$/) ||
        ($value eq '' && $class =~ /_CONFED_/))
     {
      $value = $1 if defined($1);
      $confed = '_CONFED';
     };
    if (($value =~ /^\{([0-9,]*)\}$/) ||
        ($value eq '' && $class =~ /_SET$/))
     {
      my $set = defined $1 ? $1 : $value;
      $realclass = 'Net::BGP::ASPath::AS' . $confed . '_SET';
      $this = {};
      foreach my $a (split(/,/,$set)) { $this->{$a} = 1; };
     }
    elsif ($value =~ /^[0-9 ]*$/)
     {
      $realclass = 'Net::BGP::ASPath::AS' . $confed . '_SEQUENCE';
      $this = [split(' ',$value)];
     }
    else
     {
      croak "$value is not a valid AS_PATH segment";
     }; 

    croak "AS_PATH segment is a $realclass but was constructed as $class"
	if $class !~ /::AS$/ && $class ne $realclass;

    bless($this,$realclass);
    return ( $this );
}

sub _new_from_msg
# Constructor - returns object AND buffer with data removed
{
  my ($class,$buffer) = @_;
  my ($type,$len,@list) = unpack('CC',$buffer);

  if ($len*2+2 > length($buffer))
  {
      Net::BGP::Notification->throw(
          ErrorCode    => BGP_ERROR_CODE_UPDATE_MESSAGE,
          ErrorSubCode => BGP_ERROR_SUBCODE_BAD_AS_PATH
      );
  }

  ($type,$len,@list) = unpack('CCn*',substr($buffer,0,(2*$len)+2,''));
  $class = $BGP_PATH_ATTR_CLASS[$type];
  return ($class->new(\@list),$buffer);
}

sub _encode
{
  my $this = shift;
  my $list = $this->asarray;
  my $len = scalar @{$list};
  my $type = $this->type;
  my $msg = pack('CCn*',$type,$len,@{$list});
}

sub compare
{
    my ($this,$other) = @_;
    return undef unless defined($other);
    return $this->length <=> $other->length;
}

sub clone
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    $proto = shift unless ref $proto;

    my $clone;
    if ($class =~ /_SET$/)
     {
      return $class->new([keys %{$proto}]);
     }
    else
     {
      return $class->new([@{$proto}]); # Unblessed!
     };
}

sub asstring
{
  my $this = shift;
  croak 'Instance of ASPath::AS should not exist!' if (ref $this eq 'Net::BGP::ASPath::AS');
  return $this->asstring;
}

sub asarray
{
  my $this = shift;
  croak 'Instance of ASPath::AS should not exist!' if (ref $this eq 'Net::BGP::ASPath::AS');
  return $this->asarray;
}

## End Of Net::BGP::ASPath::AS ##

package Net::BGP::ASPath::AS_SEQUENCE;

use strict;

@Net::BGP::ASPath::AS_SEQUENCE::ISA     = qw( Net::BGP::ASPath::AS );

sub type
{
 return 2;
}

sub length
{
 my $this = shift;
 return scalar @{$this};
}

sub asstring
{
 my $this = shift;
 return join(' ',@{$this});
};
 
sub asarray
{
 my $this = shift;
 return [@{$this}]; # Unblessed version of list!
}

## End Of Net::BGP::ASPath::AS_SEQUENCE ##

package Net::BGP::ASPath::AS_SET;

use strict;

@Net::BGP::ASPath::AS_SET::ISA     = qw( Net::BGP::ASPath::AS );

sub type
{
 return 1;
}

sub length
{
 my $this = shift;
 return scalar keys %{$this};
}

sub asstring
{
 my $this = shift;
 return '{'.join(',',sort {$a <=> $b} keys %{$this}).'}';
};

sub asarray
{
 my $this = shift;
 return [sort {$a <=> $b} keys %{$this}];
}

sub merge
{
 my $this = shift;
 foreach my $obj (@_)
  {
   foreach my $as (@{$obj})
    {
     $this->{$as} = 1;
    };
  }; 
 return $this;
}
 
## End Of Net::BGP::ASPath::AS_SET ##

package Net::BGP::ASPath::AS_CONFED_SEQUENCE;

use strict;

@Net::BGP::ASPath::AS_CONFED_SEQUENCE::ISA     = qw( Net::BGP::ASPath::AS_SEQUENCE );

sub type
{
 return 3;
}

sub length
{
 return 0;
}

sub asstring
{
 return '('.shift->SUPER::asstring.')';
};

## End Of Net::BGP::ASPath::AS_CONFED_SEQUENCE ##

package Net::BGP::ASPath::AS_CONFED_SET;

use strict;

@Net::BGP::ASPath::AS_CONFED_SET::ISA     = qw( Net::BGP::ASPath::AS_SET );

sub type
{
 return 4;
}

sub length
{
 return 0;
}

sub asstring
{
 return '('.shift->SUPER::asstring.')';
}

## End Of Net::BGP::ASPath::AS_CONFED_SET ##

package Net::BGP::ASPath;

use strict;
use vars qw(
    $VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS @PATHTYPES
    @BGP_PATH_ATTR_COUNTS
);

## Inheritance and Versioning ##

@ISA     = qw( Exporter );
$VERSION = '0.07';

## Module Imports ##

use Carp;
use IO::Socket;
use overload
	'<=>'  => \&len_compare,
	'<'  => \&len_lessthen,
	'>'  => \&len_greaterthen,
	'==' => \&len_equal,
	'!=' => \&len_notequal,
	'""' => \&asstring,
	'+'  => sub { my $x = shift->clone; $x->prepend(shift); },
	'+=' => \&prepend,
	'eq' => \&equal,
	'ne' => \&notequal,
	'@{}' => \&asarray,
	'fallback' => 1;

## Public Class Methods ##

sub new
{
    my $class = shift();
    my $value = shift;

    return clone Net::BGP::ASPath($value) if (ref $value eq 'Net::BGP::ASPath');

    my $this = {
        _as_path      => []
    };

    bless($this, $class);

    if (defined($value)) {
      croak "Unknown ASPath constructor argument type: " . ref $value
	if (ref $value);

      # Scalar/string
      $this->_setfromstring($value);
    };

    return ( $this );
}

sub _setfromstring
{
 my ($this,$value) = @_;
 $this->{_as_path} = [];
 $value =~ s/[ \t]+/ /g;
 $value =~ s/^ //;
 $value =~ s/ $//;
 $value =~ s/ ?, ?/,/g;
 while ($value ne '')
  {
   confess 'Invalid path segments for path object: >>' . $value . '<<'
     unless (($value =~ /^(\([^\)]*\))( (.*))?$/) ||  # AS_CONFED_* segment
             ($value =~ /^(\{[^\}]*\})( (.*))?$/) ||  # AS_SET segment
             ($value =~ /^([0-9][0-9 ]*)( (.*))?$/)); # AS_SEQUENCE seqment
   $value = $3 || '';
   my $segment = Net::BGP::ASPath::AS->new($1);
   push(@{$this->{_as_path}},$segment);
   # push(@{$this->{_as_path}}, Net::BGP::ASPath::AS->new($1));
  };
 return $this;
}

sub clone
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    $proto = shift unless ref $proto;

    my $clone = { _as_path => [] };

    foreach my $p (@{$proto->{_as_path}})
     {
      push(@{$clone->{_as_path}},$p->clone);
     };
    return ( bless($clone, $class) );
}

sub _new_from_msg
{
  my ($class,$buffer) = @_;
  my $this = $class->new;
  my $segment;
  while ($buffer ne '')
   {
    ($segment,$buffer) = _new_from_msg Net::BGP::ASPath::AS($buffer);
    return undef unless (defined $segment) && (length($buffer) != 1); # Error in message
    push(@{$this->{_as_path}},$segment);
   };
  return $this;
}

## Public Object Methods ##

sub _encode
{
  my $this = shift;
  my $msg = '';
  foreach my $segment (@{$this->{_as_path}})
   {
    $msg .= $segment->_encode;
   };
  return $msg;
}

sub prepend
{
 my $this = shift;
 my $value = shift;
 return $this->prepend_confed($value) if ($value =~ /^\(/);
 $this->strip;

 my @list = ($value);
 @list = @{$value} if (ref $value eq 'ARRAY');
 @list = split(' ',$list[0]) if $list[0] =~ / /;

 # Ugly - slow - but simple! Should be improved later!
 return $this->_setfromstring(join(' ',@list).' '.$this)->cleanup;
};

sub prepend_confed
{
 my $this = shift;

 my $value = shift;
 $value =~ s/^\((.*)\)$/$1/ unless ref $value;

 my @list = ($value);
 @list = @{$value} if (ref $value eq 'ARRAY');
 @list = split(' ',$list[0]) if $list[0] =~ / /;

 # Ugly - slow - but simple! Should be improved later!
 return $this->_setfromstring('('.join(' ',@list).') '.$this)->cleanup;
}

sub cleanup
{
 my $this = shift;

 # Ugly - slow - but simple! Should be improved later!
 my $str = $this->asstring;
 $str =~ s/\{\}//g;
 $str =~ s/\(\)//g;
 $str =~ s/(\d)\) +\((\d)/$1 $2/g;
 return $this->_setfromstring($str);
}

sub _confed
{
 my $this = shift->clone;
 @{$this->{_as_path}} = grep { (ref $_) =~ /_CONFED_/ } @{$this->{_as_path}};
 return $this;
}

sub strip
{
 my $this = shift;
 @{$this->{_as_path}} = grep { (ref $_) !~ /_CONFED_/ } @{$this->{_as_path}};
 return $this;
}

sub striped
{
 return shift->clone->strip(@_);
}

sub aggregate
{
 my @olist = @_;
 shift(@olist) unless ref $olist[0];

 # Sets
 my $cset = Net::BGP::ASPath::AS_CONFED_SET->new;
 my $nset  = Net::BGP::ASPath::AS_SET->new;

 # Lists of confed / normal part of paths
 my @clist = map { $_->_confed } @olist;
 my @nlist = map { $_->striped } @olist;

 my $res = '';
 foreach my $pair ([\@clist,$cset], [\@nlist,$nset])
  {
   my ($list,$set) = @{$pair};
   # Find common head
   my $head = $list->[0]->_head;
   foreach my $obj (@{$list}[1..@{$list}-1])
    {
     my $s = $obj->_head;
     $head = _longest_common_head($head,$s);  
    }; 

   # Find tail set
   foreach my $obj (@{$list})
    {
     my $tail = $obj->_tail($head);
     $tail = '(' . $tail if $tail =~ /^[^\(]*\).*$/; # Fix tail
     $obj = Net::BGP::ASPath->new($tail);
     $set->merge($obj);
    };
   $head .= ')' if $head =~ /^\([^\)]+$/; # Fix head
   $res .= "$head $set ";
  };

 # Construct result
 return Net::BGP::ASPath->new($res)->cleanup;
}

## Utility functions (not methods!) ##
sub _longest_common_head
{
 my ($s1,$s2) = @_;
 my $pos = 0;
 $s1 .= ' ';
 $s2 .= ' ';
 for my $i (0..length($s1)-1)
  {
   last unless substr($s1,$i,1) eq substr($s2,$i,1);
   $pos = $i if substr($s1,$i,1) eq ' ';
  };
 return substr($s1,0,$pos);
}

sub _head
# Head means the leading non-set part of the path
{
 my $this = shift->clone;
 my $ok = 1;
 $this->{_as_path} = [ grep { $ok &&= (ref $_) =~ /_SEQUENCE$/; $_ = undef unless $ok; } @{$this->{_as_path}} ];
 return $this;
}

sub _tail
# Tail means everything after the "head" given as argument.
# The tail is returned as a string. Returns undef if "head" is invalid.
{
 my $thisstr = shift() . " ";
 my $head = shift() . " ";
 $head =~ s/\(/\\(/g;
 $head =~ s/\)/\\)/g;
 return undef unless $thisstr =~ s/^$head//;
 $thisstr =~ s/ $//;
 return $thisstr;
}

sub asstring
{
 my $this = shift;
 return join(' ',map { $_->asstring; } @{$this->{_as_path}});
}

sub asarray
{
 my $this = shift;
 my @res;
 foreach my $s (@{$this->{_as_path}})
  {
   push(@res,@{$s->asarray});
  };
 return \@res;
}

sub len_equal
{
 my ($this,$other) = @_;
 return 0 unless defined($other);
 return ($this->length == $other->length) ? 1 : 0;
}

sub len_notequal
{
 my ($this,$other) = @_;
 return 1 unless defined($other);
 return ($this->length != $other->length) ? 1 : 0;
}

sub len_lessthen
{
 my ($this,$other) = @_;
 return 0 unless defined($other);
 return ($this->length < $other->length) ? 1 : 0;
}

sub len_greaterthen
{
 my ($this,$other) = @_;
 return 1 unless defined($other);
 return ($this->length > $other->length) ? 1 : 0;
}

sub len_compare
{
 my ($this,$other) = @_;
 return 1 unless defined($other);
 return $this->length <=> $other->length;
}

sub equal
{
 my ($this,$other) = @_;
 return 0 unless defined($other);
 confess "Cannot compare " . (ref $this) . " with a " . (ref $other) . "\n"
	unless ref $other eq ref $this;
 return $this->asstring eq $other->asstring ? 1 : 0;
}

sub notequal
{
 my ($this,$other) = @_;
 return 1 unless defined($other);
 return $this->asstring ne $other->asstring ? 1 : 0;
}

sub length
{
 my ($this) = @_;
 my $res = 0;
 foreach my $p (@{$this->{_as_path}})
  {
   $res += $p->length;
  };
 return $res;
}


## POD ##

=pod

=head1 NAME

Net::BGP::ASPath - Class encapsulating BGP-4 AS Path information

=head1 SYNOPSIS

    use Net::BGP::ASPath;

    # Constructor
    $aspath  = Net::BGP::ASPath->new();
    $aspath2 = Net::BGP::ASPath->new([65001,65002]);
    $aspath3 = Net::BGP::ASPath->new("(65001 65002) 65010");
    $aspath4 = Net::BGP::ASPath->new("65001 {65011,65010}");

    # Object Copy
    $clone   = $aspath->clone();

    # Modifiers;
    $aspath  = $aspath->prepend(64999);
    $aspath  = $aspath->prepend("64999 65998");
    $aspath  = $aspath->prepend([64999,65998]);

    $aspath  = $aspath->prepend("(64999 65998)");
    $aspath  = $aspath->prepend_confed("64999 65998");

    $aspath += "65001 65002";    # Same as $aspath->prepend("65001 65002")

    $aspath5 = $aspath->striped; # New object
    $aspath  = $aspath->strip;   # Same modified

    $aspath  = $aspath->cleanup  # Same modified

    # Aggregation
    $aspath  = $aspath1->aggregate($aspath2,$aspath3);
    $aspath  = Net::BGP::ASPath->aggregate($aspath1,$aspath2,$aspath3);


    # Accessor Methods
    $length    = $aspath->length;
    $string    = $aspath->asstring;
    $array_ref = $aspath->asarray

    # In context
    $string    = "The AS path is: " . $aspath;
    $firstas   = $aspath[0];

    # Length comparisons
    if ($aspath < $aspath2) { ... };
    if ($aspath > $aspath2) { ... };
    if ($aspath == $aspath2) { ... };
    if ($aspath != $aspath2) { ... };
    @sorted = sort { $a <=> $b } ($aspath, $aspath2, $aspath3, $aspath4);

    # Path comparisons
    if ($aspath eq $aspath2) { ... };
    if ($aspath ne $aspath2) { ... };

=head1 DESCRIPTION

This module encapsulates the data contained in a BGP-4 AS_PATH, inluding
confederation extentions.

=head1 CONSTRUCTOR

=over 4

=item new() - create a new Net::BGP::ASPath object

    $aspath = Net::BGP::ASPath->new( PATHDATA );

This is the constructor for Net::BGP::ASPath objects. It returns a
reference to the newly created object. The parameter may be either:

=over 4

=item ARRAY

An array of AS numbers inteperted as an AS_PATH_SEQUENCE.

=item SCALAR

A string with AS numbers seperated by spaces (AS_PATH_SEQUANCE).
AS_PATH_SETs is written using "{}" with "," to seperate AS numbers. 
AS_PATH_CONFED_* is writen equally, but encapsulated in "()".

=item Net::BGP::ASPath

Another ASPath object, in which case a clone is constructed.

=back

=back

=head1 OBJECT COPY

=over 4

=item clone() - clone a Net::BGP::ASPath object

    $clone = $aspath->clone();

This method creates an exact copy of the Net::BGP::ASPath object.

=back

=head1 ACCESSOR METHODS

=over 4

=item length()

Return the path-length used in BGP path selection. This is the sum
of the lengths of all AS_PATH elements. This does however not include
AS_PATH_CONFED_* elements.

=item asstring()

Returns the path as a string in same notation as the constructor accept.

=item cleanup()

Reduce the path by removing meaningless AS_PATH elements (empty sets or
sequences) and joining neighbour elements of same _SET type.

=item strip()

Strips AS_CONFED_* segments from the path.

=item striped()

Returns a strip() 'ed clone() of the path.

=item prepend(ARRAY)

=item prepend(SCALAR)

Strips AS_CONFED_* segments from the path and prepends one or more AS numbers
to the path as given as arguments, either as an array of AS numbers or as a
string with space seperated AS numbers. If string has "()" arround, prepend_confed
will be used instead.

=item prepend_confed(ARRAY)

=item prepend_confed(SCALAR)

Prepends one or more confederation AS numbers to the path as given as
arguments, either as an array of AS numbers or as a string with space
seperated AS numbers. "()" arround the string is ignored.

=item aggregate(ASPath)

=item aggregate(ARRAY)

Aggregates the current ASPath with the ASPath(s) given as argument.
If invoked as class method, aggregate all ASPaths given as argument.

To aggregate means to find the longest common substring (of the paths of all
objects that should be aggregated) and keep them, but
replacing the non-common substrings with AS_SET segments. Currently only
the longest common normal and confederation head will be found and the remaing
will be left as an AS_SET and AS_CONFED_SET.

Returns the aggregated object. The objects self are not modified.

=back

=head1 SEE ALSO

B<RFC 1771>, B<RFC 1997>, Net::BGP, Net::BGP::Process, Net::BGP::Peer,
Net::BGP::Notification, Net::BGP::NLRI, Net::BGP::Update

=head1 AUTHOR

Martin Lorensen <bgp@martin.lorensen.dk>

=cut

## End Package Net::BGP::ASPath ##
