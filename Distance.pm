package Geo::Distance;

use 5.006;
use strict;
use warnings;
use Carp;
use Math::Trig qw( great_circle_distance deg2rad );

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [ qw(
	&geo_distance
	&geo_distance_dirty
) ] );
our @EXPORT_OK = (
	@{ $EXPORT_TAGS{'all'} },
	'&geo_distance',
	'&geo_distance_dirty'
);
our $VERSION = '0.02';


# See Math::Trig for what $rho is.
our(%rho);
$rho{kilometer} = 6378; # Derived from the Math::Trig POD on the 'great_circle_distance'.
$rho{meter} = $rho{kilometer}*1000; # 1000 meters in one kilometer.
$rho{centimeter} = $rho{meter}*100; # 100 centimeters in one meter.
$rho{yard} = $rho{meter}*1.0936; # 1.0936 yards in one meter.
$rho{foot} = $rho{yard}*3; # 3 feet in a yard.
$rho{inch} = $rho{foot}*12; # 12 inches in a foot.
$rho{light_second} = $rho{kilometer}/298000; # 298,000 kilometers in one light second.
$rho{mile} = $rho{kilometer}/0.6214; # 0.6214 miles in one kilometer.


# New Object Constructor
sub new {
	my $class = shift;
	return bless {}, $class;
}

# Wrapper for object oriented access to geo_distance().
sub distance {
	if(!ref($_[0])){ croak('The routine distance() called without an object reference'); }
	shift;
	geo_distance(@_);
}

# Wrapper for object oriented access to geo_distance_dirty().
sub distance_dirty {
	if(!ref($_[0])){ croak('The routine distance_dirty() called without an object reference'); }
	shift;
	geo_distance_dirty(@_);
}

# Checks input and passes input to distance_dirty().
sub geo_distance {
	if(ref($_[0])){ croak('The routine geo_distance() called with an object reference'); }
	my %args = @_;
	$args{unit}='mile' if(!$args{unit});
	if(!$rho{$args{unit}}){ croak('Unkown unit'); }
	if(!(_is_decimal($args{lon1}) and _is_decimal($args{lat1}) and _is_decimal($args{lon2}) and _is_decimal($args{lat2}))){ croak('You did not provide two complete sets of longitude and latitude'); }
	return geo_distance_dirty($args{unit},$args{lon1},$args{lat1},$args{lon2},$args{lat2});
}

# Retrieves the distance between two sets of longitude and latitude.
# Does not check validity of input.
sub geo_distance_dirty {
	if(ref($_[0])){ croak('The routine geo_distance_dirty() called with an object reference'); }
	my($unit,$lon1,$lat1,$lon2,$lat2) = splice(@_,0,5);
	return great_circle_distance(deg2rad($lon1), deg2rad(90 - $lat1), deg2rad($lon2), deg2rad(90 - $lat2), $rho{$unit});
}

# Checks if a value is a decimal.
sub _is_decimal {
	return(defined($_[0]) and $_[0] =~ /^[0-9]+(\.[0-9]+)?$/s);
}


1;
__END__

=head1 NAME

Geo::Distance - Compute the Distances between Two Geographic Locations

=head1 SYNOPSIS

  use Geo::Distance;
  my $geo = new Get::Distance;
  my $dist1 = $geo->distance( unit=>'mile', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = $geo->distance_dirty('light_second',$lon1,$lat1,$lon2,$lat2);

or

  use Geo::Distance qw{ :all };
  my $dist1 = geo_distance( unit=>'meter', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = geo_distance_dirty('inch',$lon1,$lat1,$lon2,$lat2);

=head1 DESCRIPTION

This perl library provides the ability to calculate the distance between two geographic points.  
There is both a function oriented interface, and an object oriented interface.  Both have the 
same capabilities.  Your choice of interface depends on your programming style and needs.  
Geo::Distance only recognizes standard decimal based longitude and latitude measurements.  If you 
have other coordinate systems that you would like to use, then take a peak at the Geo::Coordinates 
module and the Geo::Coordinates::* family of conversion modules.

The latest version of the Geo::Distance (and Geo::Coordinates) modules can be found at:

  http://www.bluefeet.net/perl/modules/geo/distance/

=head1 OO VERSUS POLLUTION

This module provides two styles of programming, the first of which being an elegant object oriented 
interface and the second being a namespace polluting function interface.  Personally, I like to 
pollute my namespace, but an object oriented interface does come in handy at times.

Both the object oriented interface and the function interface have the same functions, just with 
slightly different names.

At the moment there are several ways to import functions.  The most common being the B<:all> export 
tag.  This will export the geo_distance() and geo_distance_dirty() functions.  You may also export 
the functions individually, as follows:

  # Export just geo_distance().
  use Geo::Distance qw{ &geo_distance };
  
  # Export just geo_distance_dirty().
  use Geo::Distance qw{ &geo_distance_dirty };
  
  # Export both functions.
  use Geo::Distance qw{ &geo_distance &geo_distance_dirty };
  # or
  use Geo::Distance qw{ :all };

=head1 METHODS

Currently there are two methods supplied by this module, both with function oriented and object 
oriented interface.  Whatever the interface, these methods take the same arguments and 
return the same results.

=head2 DISTANCE

Takes a name value pairs in a hash style.  All arguments will be validated.  Returns the distance 
between the two locations in the unit type passed to it.

  # unit => mile|light_second|kilometer|meter|centimeter|yard|foot|inches
  # lon1,lat1 => Latitude and longitude in decimal format for the first location.
  # lon2,lat2 => Ditto, but for the second location.
  
  my $dist1 = geo_distance( unit=>'meter', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = $geo->distance( unit=>'mile', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );

=head2 DISTANCE_DIRTY

This method is used internally by distance() to do the actual distance 
calculation.  The benefit of using distance_dirty() is that it is faster than its counterpart 
because it does not check the input and does not accept a hash style argument list.

This method takes 5 arguments. They are unit, lon1, lat1, lon2, and lat2.  See the distance method 
for a description of these arguments.

  my $dist1 = $geo->distance_dirty('light_second',$lon1,$lat1,$lon2,$lat2);
  my $dist2 = geo_distance_dirty('inch',$lon1,$lat1,$lon2,$lat2);

=head1 TODO

=over 4

=item *

Detect coordinate types and automatically send them through Geo::Coordinates to 
be converted to decimal.

=back

=head1 BUGS

None known right now, but by the time you read this, who knows?
Check http://www.bluefeet.net/perl/modules/geo/distance/ for the latest bugs and 
enhancements.

=head1 NOTES

Geo::Distance is currently in its alpha stage.  The interface may, and probably will change, 
features will be added and removed without notice, and good things will hopefully happen before 
the bad.  So, until this module reaches beta stage, don't be too peeved if something doesn't 
work when you upgrade.

This module relies on Math::Trig (great_circle_distance) for most of its computations.  Math::Trig 
is a core Perl module.

=head1 AUTHOR

Copyright (C) 2003 Aran Clary Deltac (CPAN: BLUEFEET)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

Address bug reports and comments to: E<lt>geo_distance@bluefeet.netE<gt>. When sending bug reports, 
please provide the version of Geo::Distance, the version of Perl, and the name and version of the 
operating system you are using.  Patches are welcome if you are brave!

=head1 SEE ALSO

L<Geo::Coordinates> - A wrapper for converting between various geographic coordinates.

L<Math::Trig> - Inverse and hyperbolic trigonemetric Functions.
