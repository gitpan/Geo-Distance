package Geo::Distance;

use 5.006;
use strict;
use warnings;
use Carp;
use Math::Trig qw( great_circle_distance deg2rad );

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [ qw(
	&distance
	&distance_calc
	&find_closest
	&reg_unit
) ] );
our @EXPORT_OK = (
	@{ $EXPORT_TAGS{'all'} },
	'&distance',
	'&distance_calc',
	'&find_closest',
	'&reg_unit'
);
our $VERSION = '0.05';


# See Math::Trig for what $rho is.
our(%rho);
$rho{kilometer} = 6378; # Derived from the Math::Trig POD on the 'great_circle_distance'.
$rho{meter} = $rho{kilometer}*1000; # 1000 meters in one kilometer.
$rho{centimeter} = $rho{meter}*100; # 100 centimeters in one meter.
$rho{yard} = $rho{meter}*1.0936; # 1.0936 yards in one meter.
$rho{foot} = $rho{yard}*3; # 3 feet in a yard.
$rho{inch} = $rho{foot}*12; # 12 inches in a foot.
$rho{light_second} = $rho{kilometer}/298000; # 298,000 kilometers in one light second.
$rho{mile} = $rho{kilometer}*0.6214; # 0.6214 miles in one kilometer.

# Number of units in a single degree (lat or lon) at the equator.
# Derived from doing dirty_distance('kilometer',10,0,11,0) = 111.317099692185
# Then dividing that by $unit{kilometer} = 6378
our $deg_ratio = 0.01745329252;


# New Object Constructor
sub new {
	my $class = shift;
	return bless {}, $class;
}

# Register a unit.
sub reg_unit {
	shift() if(ref($_[0]));
	my($unit,$amount) = splice(@_,0,2);
	if(@_){
		# Make a new unit based off an existing one.
		my $parent = shift;
		$rho{$unit} = $rho{$parent}*$amount;
	}else{
		# Make a new unit, or update an existing one.
		$rho{$unit} = $amount;
	}
}


# Checks input and passes input to distance_calc().
sub distance {
	shift() if(ref($_[0]));
	my %args = @_;
	$args{unit}='mile' if(!$args{unit});
	if(!$rho{$args{unit}}){ croak('Unkown unit'); }
	return distance_calc($args{unit},[$args{lon1},$args{lat1}],[$args{lon2},$args{lat2}]);
}

# Retrieves the distance between two sets of longitude and latitude.
# Does not check validity of input.
sub distance_calc {
	shift() if(ref($_[0]));
	my $unit = shift;
	my($ary1,$ary2);
	if(ref($_[0])){ $ary1=shift; $ary2=shift; }
	else{ $ary1=[]; $ary2=[]; ($$ary1[0],$$ary1[1],$$ary2[0],$$ary2[1]) = splice(@_,0,4); }
	return great_circle_distance(deg2rad($$ary1[0]), deg2rad(90 - $$ary1[1]), deg2rad($$ary2[0]), deg2rad(90 - $$ary2[1]), $rho{$unit});
}

# Finds the closest set of locations.
sub find_closest {
	shift() if(ref($_[0]));
	my %args = @_;
	croak('A distance was not provided') if(! defined $args{distance} );
	croak('No valid unit type was passed') if(! (defined($args{unit}) and defined($rho{$args{unit}})) );
	croak('No valid longitude was passed') if(! defined $args{lon} );
	croak('No valid latitude was passed') if(! defined $args{lat} );
	# Check for what we're going to search with.
	if($args{dbh}){
		my $degrees = $args{distance}/($deg_ratio*$rho{$args{unit}});
		croak('Not a DBI connection') if(ref($args{dbh}) !~ /DBI/);
		croak('DBI connection accepted, but no table was provided') if(!$args{table});
		if(!$args{field}){ $args{field}='id'; }
		$args{array} = $args{dbh}->selectall_arrayref('SELECT lon,lat,'.$args{field}.' FROM '.$args{table}.' WHERE lon>='.($args{lon}-$degrees).' AND lat>='.($args{lat}-$degrees).' AND lon<='.($args{lon}+$degrees).' AND lat<='.($args{lat}+$degrees));
	}elsif($args{array}){
		croak('Not an array reference') if(ref($args{array}) !~ /ARRAY/);
		# TODO: Need to do the simpler calculation like we do with the 
		# dbh to take out the obviously too far away locations.
	}else{
		croak('Unkown data retrieval method');
	}
	# Weed out places farther away than we want.
	my($i,$location,$distance,@locations,@distances);
	for($i=0; $i<@{$args{array}}; $i++){
		$location = ${$args{array}}[$i];
		$distance = distance_calc($args{unit},$args{lon},$args{lat},$$location[0],$$location[1]);
		if($distance<=$args{distance}){
			$locations[@locations] = $location;
			$distances[@distances] = $distance;
		}
	}
	if(defined($args{count})){
		for(my $i=@distances-1; $i>=0; $i--){
			for(my $j=$i-1; $j>=0; $j--){
				if($distances[$i] < $distances[$j]){
					# Move Location Up
					$location = $locations[$i];
					$locations[$i] = $locations[$j];
					$locations[$j] = $location;
					# Move Distance Up
					$distance = $distances[$i];
					$distances[$i] = $distances[$j];
					$distances[$j] = $distance;
				}
			}
		}
		if($args{count}>0){
			splice(@locations,$args{count});
			splice(@distances,$args{count});
		}
	}
	return( wantarray() ? (\@locations,\@distances) : \@locations );
}


1;
__END__

=head1 NAME

Geo::Distance - Calculate Distances and Closest Locations

=head1 SYNOPSIS

  use Geo::Distance;
  my $geo = new Geo::Distance;
  $geo->reg_unit('foobar',390);
  my $dist1 = $geo->distance( unit=>'foobar', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = $geo->distance_calc('light_second',$lon1,$lat1,$lon2,$lat2);
  my $dist3 = $geo->distance_calc('centimeter',$ary_ref1,$ary_ref2);
  my $locations = $geo->find_closest(
  	lon=>$lon, lat=>$lat,
  	distance=>$dist, unit=>$unt,
  	dbh=>$dbh, table=>$tbl, field=>$fld
  );

or

  use Geo::Distance qw{ :all };
  reg_unit('dinosaur_step',44560);
  my $dist1 = distance( unit=>'meter', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = distance_calc('dinosaur_step',$lon1,$lat1,$lon2,$lat2);
  my $dist3 = distance_calc('foot',$ary_ref1,$ary_ref2);
  my $locations = find_closest(
  	lon=>$lon, lat=>$lat,
  	distance=>$dist, unit=>$unt,
  	dbh=>$dbh, table=>$tbl, field=>$fld
  );

=head1 DESCRIPTION

This perl library aims to provide as many tools to make it as simple as possible to calculate 
distances between geographic points, and anything that can be derived from that.  Currently 
there is support for finding the closest locations within a specified distance, to find the 
closest number of points to a specified point, and to do basic point-to-point distance 
calculations.

The latest version of the Geo::Distance module can be found here:

  http://www.bluefeet.net/perl/modules/geo-distance/
    and of course at,
  http://www.cpan.org/ (or your closest CPAN mirror)

Soon to be available will be some free to download and use data sets for use with find_closest().

=head1 OO VS. POLLUTION

There are two styles of programming available.  The first of which being an elegant object oriented 
interface and the second being a namespace polluting function interface.  Personally, I like to 
pollute my namespace, but an object oriented interface does come in handy at times.

Both the object oriented interface and the function interface have the same functions, just a 
different context under which to call them with.

At the moment there are several ways to import functions.  The most common being the B<:all> export 
tag.  This will export the distance(), distance_calc(), and find_closest() functions.  
You may also export the functions individually, as follows:

  # Export just distance().
  use Geo::Distance qw( distance );
  
  # Export just find_closest().
  use Geo::Distance qw( find_closest );
  
  # Export all functions.
  use Geo::Distance qw( distance distance_calc find_closest );
  # or
  use Geo::Distance qw( :all );

=head1 PROPERTIES

=head2 UNITS

All functions accept a unit type to do the computations of distance with.  There are several 
of the most common unit types pre-defined.

  kilometer
  meter
  centimeter
  mile
  yard
  foot
  inches
  light_second

If you want to use a different unit type than is available use the reg_unit() function.

=head2 LATITUDE AND LONGITUDE

When a function needs a lon and lat they must always be in decimal format.  If you have a 
different coordinate system and need to convert head on over to your local CPAN and do a look 
up for any Geo::Coordinates::* modules.

  # DMS to Decimal
  my $decimal = $degrees + ($minutes/60) + ($seconds/3600);
  
  # Precision Six Integer to Decimal
  my $decimal = $integer * .000001;

=head1 METHODS

Currently there are three methods supplied by this module, each with function oriented and object 
oriented interface.  Whatever the interface, these methods take the same arguments and 
return the same results.

=head2 DISTANCE

Takes a name value pairs in a hash style.  All arguments will be validated.  Returns the distance 
between the two locations in the unit type passed to it.

  my $dist1 = geo_distance( unit=>'meter', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );
  my $dist2 = $geo->distance( unit=>'mile', lon1=>$lon1, lat1=>$lat1, lon2=>$lon2, lat2=>$lat2 );

=head2 DISTANCE_CALC

This method is used internally by distance() and find_closest() to do the actual distance 
calculation.  The benefit of using distance_calc() above distance() is that it is faster, 
it does not verify the input, and does not accept a hash style argument list.

This method takes arguments in two forms, as five arguments or as three. The five arguments 
are unit, lon1, lat1, lon2, and lat2.  See the distance method for a description of these arguments.  
The three arguments are unit, array_ref1, and array_ref2.  The array_refs are array references each 
with two entries, the first being lon, and the second being lat.

  my $dist1 = $geo->distance_calc('light_second',$lon1,$lat1,$lon2,$lat2);
  my $dist2 = distance_calc('inch',$lon1,$lat1,$lon2,$lat2);
  
  my $dist3 = $geo->distance_calc('centimeter',$ary_ref1,$ary_ref2);
  my $dist4 = distance_calc('foot',$ary_ref1,$ary_ref2);

=head2 FIND_CLOSEST

This method finds the closest locations within a certain distance and returns an array reference of 
arrays of locations, and optionally an array reference of distances, depending on if you called 
with a scalar context or array context.

  my $locations = $geo->find_closest(...);
  my ($locations,$distances) = $geo->find_closest(...);

The locations to search can be provided as either a database connection or as an array ref.  

B<DATABASE SEARCH>

This method uses some very simplistic calculations to SQL select out of the $dbh.  This 
means that the SQL should work fine on almost any database (only tested on MySQL so far) and 
this also means that it is B<very> fast.  Once this sub set of locations has been retrieved 
then more precise calculations are made to narrow down the result set.  Remember, though, that 
the farther out your distance is, and the more locations in the table, the slower your searches will be.

When searching a database you must also provide a table name to search and at least one field to return.  
The table that you want to search in I<must> have lon and lat fields, both being of the type float.

  # Database connection example.
  my $dbh = DBI->connect(...);
  
  # Find all zip codes within 50 miles of the county Wilbarger, TX, US.
  my($lon,$lat) = $dbh->selectrow_array(
    'SELECT lon,lat FROM counties WHERE fips=48487'
  );
  my($zips,$distances) = geo_find_closest(
    lon => $lon,
    lat => $lat,
    distance => 50,
    unit => 'mile',
    dbh => $dbh,
    table => 'zipcodes',
    field => 'id,state'
  );
  
  # Internally an SQL select like this is created:
  #   SELECT lon,lat,id,state FROM counties WHERE ...
  
  # Print out each Zip.
  for(my $i=0; $i<@$zips; $i++){
  	# @$zips = [lon,lat,id,state]
  	my $zip = $$zips[$i];
  	print "The Zip $$zip[2], $$zip[3], ($$zip[0] x $$zip[1]) was $$distances[$i] miles away from $lon x $lat.\n";
  }

B<ARRAY REFERENCE SEARCH>

You may also pass an array reference as the data to search. While not regarded as an 
effecient method of finding closest locations, it is still useful at times especially 
for testing.

  my $locations_db = load_zips();
  $locations = find_closest(
    array=>$locations_db,
    lon=>$lon,
    lat=>$lat,
    unit=>'mile',
    distance=>'50'
  );

B<COUNT ARGUMENT>

find_closest() has an optional 'count' field that provides the ability to only retrieve a certain number 
of locations.

  # Retrieve the 5 closest locations.
  $locations = find_closest(count=>5, ...);

Coincidentally, if you request a zero count (count=>0), you will get all locations as if you didn't specify 
a count except they will be ordered by distance.

  # Sort locations by their distance.
  ($locations,$distances) = find_closest(count=>0, ...);

=head2 REG_UNIT

This method is used to create custom unit types.  There are two ways of calling it, 
depending on if you are defining the unit from scratch, or if you are basing it off 
of an existing unit (such as saying inches = feet / 12 ).  When defining a unit from 
scratch you pass the name and rho (radius of the earth in that unit) value.

So, if you wanted to do your calculations in human adult steps you would have to have an 
average human adult walk around the earth at the equator (ignore the fact that if you did 
this you would first have to make bridges that span the oceans around the earth).  So, 
assuming we did this and we came up with 43,200 steps to make it all the way around the 
equator of the earth, you'd do something like the following.

  # Create adult_step unit.
  $geo->reg_unit('adult_step',43200);

Now, if you also wanted to do distances in baby steps you might think "well, now I 
gotta get a baby to walk around the earth".  But, you don't have to!  If you do some 
research you'll find (no research was actually conducted) that there are, on average, 
4.7 baby steps in each adult step.

  # Create baby_step unit based off adult_step unit.
  $geo->reg_unit('baby_step',4.7,'adult_step');

And if we were doing this in reverse and already had the baby step unit but not 
the adult step...

  # Create adult_step unit based off baby_step unit.
  $geo->reg_unit('adult_step',1/4.7,'baby_step');

=head1 TODO

=over 4

=item *

Need a more accurate way of calculating the distance that does not rely upon Math::Trig.  Will 
probably be math intensive, so Math::Trig should still remain the default for the sake of speed.

=item *

Berkely DB would be a nice alternative to DBI and Array find_closest() searching.

=item *

Array searching by find_closest() needs to do a first pass using the more simplistic outer radius 
calculation, like it does with a database connection.

=item *

A second pass should be done in find_closest before distance calculations are made that does an inner 
radius simplistic calculation.

=back

=head1 BUGS

None known right now, but by the time you read this, who knows?
Check http://www.bluefeet.net/perl/modules/geo-distance/ for the latest bugs and 
enhancements.

=head1 NOTES

Geo::Distance is currently in its alpha stage.  The interface may, and probably will change, 
features will be added and removed without notice, and good things will hopefully happen before 
the bad.  So, until this module reaches beta stage, don't be too peeved if something doesn't 
work when you upgrade.

This module relies on Math::Trig (great_circle_distance) for most of its computations.  Math::Trig 
is a core Perl module.  Be aware that Math::Trig states:

  "The answers may be off by few percentages because of the irregular (slightly aspherical) form of the Earth."

also

  "The formula used for grear circle distances is also somewhat unreliable for small distances (for locations separated less than about five degrees) because it uses arc cosine which is rather ill-conditioned for values close to zero."

=head1 CHEERS

Thanks!

=over 4

=item *

I<Michael R. Meuser>

=item *

I<Jack D.>

=item *

I<Bryce Nesbitt>

=back

=head1 AUTHOR

Copyright (C) 2003 Aran Clary Deltac (CPAN: BLUEFEET)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

Address bug reports and comments to: E<lt>aran@bluefeet.netE<gt>. When sending bug reports, 
please provide the version of Geo::Distance, the version of Perl, and the name and version of the 
operating system you are using.  Patches are welcome if you are brave!

=head1 SEE ALSO

L<Math::Trig> - Inverse and hyperbolic trigonemetric Functions.
