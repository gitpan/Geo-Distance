package Geo::Distance;
#-------------------------------------------------------------------------------

=head1 NAME

Geo::Distance - Calculate Distances and Closest Locations

=head1 SYNOPSIS

  use Geo::Distance;
  my $geo = new Geo::Distance;
  $geo->formula('hsin');
  $geo->reg_unit( 'toad_hop', 200120 );
  $geo->reg_unit( 'frog_hop' => 6 => 'toad_hop' );
  my $distance = $geo->distance( 'unit_type', $lon1,$lat1 => $lon2,$lat2 );
  my $locations = $geo->closest( $unit, $unit_count, $lon, $lat, $source, $options);

=head1 DESCRIPTION

This perl library aims to provide as many tools to make it as simple as possible to calculate 
distances between geographic points, and anything that can be derived from that.  Currently 
there is support for finding the closest locations within a specified distance, to find the 
closest number of points to a specified point, and to do basic point-to-point distance 
calculations.

=head1 STABILITY

This is the first version of Geo::Distance to be considered to have a stable interface.  
You can now rely on the interface to be backwards compatible to version 0.07 and newer.

=cut

#-------------------------------------------------------------------------------
use 5.006;
use strict;
use warnings;
use Carp;
use Math::Trig qw( great_circle_distance deg2rad rad2deg acos pi );
our $VERSION = '0.08';
use constant KILOMETER_RHO => 6371.64;
#-------------------------------------------------------------------------------

=head1 PROPERTIES

=head2 UNITS

All functions accept a unit type to do the computations of distance with.  By default no units 
are defined in a Geo::Distance object.  You can add units with reg_unit() or create some default 
units with default_units().

=head2 LATITUDE AND LONGITUDE

When a function needs a lon and lat they must always be in decimal degree format.  Here is some sample 
code for converting from other formats to decimal:

  # DMS to Decimal
  my $decimal = $degrees + ($minutes/60) + ($seconds/3600);
  
  # Precision Six Integer to Decimal
  my $decimal = $integer * .000001;

If you want to convert from decimal radians to degrees you can use Math::Trig's rad2deg function.

=head1 METHODS

=head2 new

  my $geo = new Geo::Distance;
  my $geo = new Geo::Distance( no_units=>1 );

Returns a blessed Geo::Distance object.  The new constructor accepts one optional 
argument.

  no_unit - Whether or not to load the default units. Defaults to 0 (false).
            kilometer, meter, centimeter, yard, foot, inch, light_second, mile

=cut

#-------------------------------------------------------------------------------
sub new {
	my $class = shift;
	my $self = bless {}, $class;
	my %args = @_;
	
	$self->{formula} = 'hsin';
	$self->{units} = {};
	if(!$args{no_units}){
		$self->reg_unit( 'kilometer', KILOMETER_RHO );
		$self->reg_unit( 'meter' => 1000 => 'kilometer' );
		$self->reg_unit( 'centimeter' => 100 => 'meter' );
		$self->reg_unit( 'yard' => 1.0936 => 'meter' );
		$self->reg_unit( 'foot' => 3 => 'yard' );
		$self->reg_unit( 'inch' => 12 => 'foot' );
		$self->reg_unit( 'light_second' => (1/298000) => 'kilometer' );
		$self->reg_unit( 'mile' => 0.6214 => 'kilometer' );
		$self->reg_unit( 'nautical mile' => 1.852 => 'kilometer' );
	}
	
	# Number of units in a single degree (lat or lon) at the equator.
	# Derived from: $geo->distance( 'kilometer', 10,0, 11,0 ) / $geo->{units}->{kilometer}
	$self->{deg_ratio} = 0.0174532925199433;

	return $self;
}
#-------------------------------------------------------------------------------

=head2 formula

  if($geo->formula eq 'hsin'){ ... }
  $geo->formula('cos');

Allows you to retrieve and set the formula that is currently being used to 
calculate distances.  The availabel formulas are hsin, polar, cos, and mt.  hsin 
is the default and mt/cos are depreciated in favor of hsin.  polar should be 
used when calculating coordinates near the poles.

=cut

#-------------------------------------------------------------------------------
sub formula {
	my $self = shift;
	my $formula = shift;
	if($formula ne 'mt' and $formula ne 'cos' and $formula ne 'hsin' and $formula ne 'polar'){
		croak('Invalid formula (only gcd, cos, hsin, and polar are supported)');
	}else{
		$self->{formula} = $formula;
	}
	return $formula;
}
#-------------------------------------------------------------------------------

=head2 reg_unit

  # Register 200,120 frog hops to travel the radius of the earth.
  $geo->reg_unit( 'toad_hop', 200120 );
  
  # For every toad hop there are 6 frog hops.
  $geo->reg_unit( 'frog_hop' => 6 => 'toad_hop' );

This method is used to create custom unit types.  There are two ways of calling it, 
depending on if you are defining the unit from scratch, or if you are basing it off 
of an existing unit (such as saying inches = feet / 12 ).  When defining a unit from 
scratch you pass the name and rho (radius of the earth in that unit) value.

So, if you wanted to do your calculations in human adult steps you would have to have an 
average human adult walk from the crust of the earth to the core (ignore the fact that 
this is impossible).  So, assuming we did this and we came up with 43,200 steps, you'd 
do something like the following.

  # Create adult_step unit.
  $geo->reg_unit( 'adult_step', 43200 );

Now, if you also wanted to do distances in baby steps you might think "well, now I 
gotta get a baby to walk to the center of the earth".  But, you don't have to!  If you do some 
research you'll find (no research was actually conducted) that there are, on average, 
4.7 baby steps in each adult step.

  # Create baby_step unit based off adult_step unit.
  $geo->reg_unit( 'baby_step' => 4.7 => 'adult_step' );

And if we were doing this in reverse and already had the baby step unit but not 
the adult step...

  # Create adult_step unit based off baby_step unit.
  $geo->reg_unit( 'adult_step', 1/4.7, 'baby_step' );

=cut

#-------------------------------------------------------------------------------
sub reg_unit {
	my $self = shift;
	my($unit,$amount) = splice(@_,0,2);
	if(@_){
		# Make a new unit based off an existing one.
		my $parent = shift;
		if(!defined($self->{units}->{$parent})){ croak('The unit "'.$parent.'" is not defined'); }
		$self->{units}->{$unit} = $self->{units}->{$parent} * $amount;
	}else{
		# Make a new unit, or update an existing one.
		$self->{units}->{$unit} = $amount;
	}
}
#-------------------------------------------------------------------------------

=head2 distance

  my $distance = $geo->distance( 'unit_type', $lon1,$lat1 => $lon2,$lat2 );

Calculates the distance between two lon/lat points.

=cut

#-------------------------------------------------------------------------------
sub distance {
	my($self,$unit,$lon1,$lat1,$lon2,$lat2) = @_;
	croak('Unkown unit type "'.$unit.'"') unless($unit = $self->{units}->{$unit});
	if($self->{formula} eq 'mt'){
		return great_circle_distance(
			deg2rad($lon1), 
			deg2rad(90 - $lat1), 
			deg2rad($lon2), 
			deg2rad(90 - $lat2), 
			$unit
		);
	}else{
		$lon1 = deg2rad($lon1); $lat1 = deg2rad($lat1);
		$lon2 = deg2rad($lon2); $lat2 = deg2rad($lat2);
		my $c;
		if($self->{formula} eq 'cos'){
			my $a = sin($lat1) * sin($lat2);
			my $b = cos($lat1) * cos($lat2) * cos($lon2 - $lon1);
			$c = acos($a + $b);
		}
		elsif($self->{formula} eq 'hsin'){
			my $dlon = $lon2 - $lon1;
			my $dlat = $lat2 - $lat1;
			my $a = (sin($dlat/2)) ** 2 + cos($lat1) * cos($lat2) * (sin($dlon/2)) ** 2;
			$c = 2 * atan2(sqrt($a), sqrt(1-$a));
		}
		elsif($self->{formula} eq 'polar'){
			my $a = pi/2 - $lat1;
			my $b = pi/2 - $lat2;
			$c = sqrt( $a ** 2 + $b ** 2 - 2 * $a * $b * cos($lon2 - $lon1) );
		}
		else{
			croak('Unkown distance formula "'.$self->{formula}.'"');
		}
		return $unit * $c;
	}
}
#-------------------------------------------------------------------------------

=head2 closest

This method finds the closest locations within a certain distance and returns an hash reference of 
locations each with at least it's lon, lat, and the distance.

  my $locations = $geo->closest( $unit, $distance, $lon, $lat, $source, $options);

  $unit - The name of the unit that you want the distances measured by.
  $distance - The number units out that you want to search.
  $lon, $lat - The longitutde and latitude.
  $source - The data source (either a DBI handle or an array ref).
  $options - A hashref of options.
    table - The name of the table to search in.
    fields - Any custom fields to return.
    lon_field - The name of the longitude field, defaults to "lon".
    lat_field - The name of the latitude field, defaults to "lat".
    count - The maximum number of locations to return.
    sort - A boolean of whether or not to sort the resulting locations 
           by their distance.  Defaults to 0 (false).
    where - Any additional SQL where clause that you would like to limit the search by.
    bind - Bind vars to use with your where clause.

B<DATABASE SEARCH>

This method uses some very simplistic calculations to SQL select out of the $dbh.  This 
means that the SQL should work fine on almost any database (only tested on MySQL so far) and 
this also means that it is B<very> fast.  Once this sub set of locations has been retrieved 
then more precise calculations are made to narrow down the result set.  Remember, though, that 
the farther out your distance is, and the more locations in the table, the slower your searches will be.

When searching a database you must also provide a table name to search and, optionally, one or more fields 
(seperated by commas) to return.  The table that you want to search in I<must> have lon and lat fields.

  # Database connection example.
  my $dbh = DBI->connect(...);
  
  # Find all zip codes within 50 miles of the county Wilbarger, TX, US.
  my($lon,$lat) = county_lonlat( fips=>48487 );
  my $zips = $geo->closest(
    50 => 'mile' => $lon,$lat,
    $dbh => { table=>'zipcodes', fields=>'id AS code,state' }
  );
  
  # Internally an SQL select like this is created:
  #   SELECT lon,lat,id,state FROM zipcodes WHERE ...
  
  # Print out each Zip.
  foreach my $zip (@$zips){
    print 
      "The Zip $zip->{code}, $zip->{state}, ($zip->{lon} x $zip->{lat}) was ".
      int($zip->{distance}*10)/10. " miles away from the county at $lon x $lat.\n";
  }

B<ARRAY REFERENCE SEARCH>

You may also pass an array reference as the data to search. While not regarded as an 
effecient method of finding closest locations, it is still useful at times especially 
for testing.

  my $zips_ary = load_zips();
  my $zips = $geo->closest(
    50 => 'mile' => $lon,$lat,
    $zips_ary
  );

The array should contain a hash ref for each location to search.  Each hash_ref should have a 
lon field and a lat field.

=cut

#-------------------------------------------------------------------------------
sub closest {
	my($self,$distance,$unit,$lon,$lat,$source,$options) = @_;

	# Default options.
	$options ||= {};
	$options->{lon_field} ||= 'lon';
	$options->{lat_field} ||= 'lat';
	$options->{sort}=1 if($options->{count});

	# Retrieve locations.
	my $locations;
	if(ref($source) eq 'DBI'){
		my $degrees = $distance / ($self->{deg_ratio}*$unit);
		$options->{fields} .= ',' if($options->{fields});
		$options->{fields} .= $options->{lon_field}.','.$options->{lat_field};
		my $sth = $source->prepare("
			SELECT $options->{fields} 
			FROM $options->{table} 
			WHERE lon>=".($lon-$degrees).' 
			AND lat>='.($lat-$degrees).' 
			AND lon<='.($lon+$degrees).' 
			AND lat<='.($lat+$degrees).
			( $options->{where} ? "AND ($options->{where})" : '' )
		);
		$sth->execute( $options->{bind} || () );
		$locations = [];
		while(my $location = $sth->fetchrow_hashref){
			push @$locations, $location;
		}
	}elsif(ref($source) eq 'ARRAY'){
		$locations = $source;
	}else{
		croak('Unkown data source');
	}

	# Calculate distances.
	my $closest = [];
	foreach my $location (@$locations){
		$location->{distance} = $self->distance(
			$unit, $lon, $lat, 
			$location->{$options->{lon_field}}, 
			$location->{$options->{lat_field}}
		);
		if( $location->{distance} <= $distance ){
			push @$closest, $location;
		}
	}
	$locations = $closest;

	# Sort.
	if( $options->{sort} ){
		my $location;
		for(my $i=@$locations-1; $i>=0; $i--){
			for(my $j=$i-1; $j>=0; $j--){
				if($locations->[$i]->{distance} < $locations->[$j]->{distance}){
					$location = $locations->[$i];
					$locations->[$i] = $locations->[$j];
					$locations->[$j] = $location;
				}
			}
		}
	}

	# Split for count.
	if( $options->{count} ){
		splice @$locations, $options->{count};
	}
	
	return $locations;
}
#-------------------------------------------------------------------------------

=head1 FORMULAS

=head2 hsin: Haversine Formula

  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c 

The hsin formula is the new standard formula for Geo::Distance because 
of it's improved accuracy over the cos formula.

=head2 polar: Polar Coordinate Flat-Earth Formula

  a = pi/2 - lat1
  b = pi/2 - lat2
  c = sqrt( a^2 + b^2 - 2 * a * b * cos(lon2 - lon1) )
  d = R * c 

While implimented, this formula has not been tested much.  If you use it 
PLEASE share your results with the author!

=head2 cos: Law of Cosines for Spherical Trigonometry

  a = sin(lat1) * sin(lat2)
  b = cos(lat1) * cos(lat2) * cos(lon2 - lon1)
  c = arccos(a + b)
  d = R * c

Although this formula is mathematically exact, it is unreliable for 
small distances because the inverse cosine is ill-conditioned.

=head2 mt: Math::Trig great_circle_distance

This formula uses Meth::Trig's great_circle_distance function which at this time uses math almost 
exactly the same as the cos formula.  If you want to use the cos formula you may find 
that mt will calculate faster (untested assumption).  For some reason mt and cos return 
slight differences at very close distances. The mt formula has the same drawbacks as the cos formula.

This is the same formula that was previously the only one used by 
Geo::Distance (ending at version 0.06) and was wrongly called the "gcd" formula.

Math::Trig states that the formula that it uses is:

  lat0 = 90 degrees - phi0
  lat1 = 90 degrees - phi1
  d = R * arccos(cos(lat0) * cos(lat1) * cos(lon1 - lon01) + sin(lat0) * sin(lat1))

=head1 TODO

=over 4

=item *

Test the polar formula.

=item *

Test the closest() function.  I've modified it since the last version but haven't had a chance to test.

=item *

Berkely DB would be a nice alternative to DBI and Array closest() searching.

=item *

A second pass should be done in closest before distance calculations are made that does an inner 
radius simplistic calculation to find the locations that are obviously within the distance needed.

=item *

Tests!  We need tests!

=back

=head1 BUGS

Its probable since several of the parts mentioned in the TODO section have not been tested.

Otherwise, none known right now, but by the time you read this, who knows?

=head1 CHEERS

Thanks!

=over 4

=item *

I<Dean Scott>

=item *

I<Michael R. Meuser>

=item *

I<Jack D.>

=item *

I<Bryce Nesbitt>

=back

=head1 AUTHOR

Copyright (C) 2003-2005 Aran Clary Deltac (CPAN: BLUEFEET)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

Address bug reports and comments to: E<lt>aran@arandeltac.comE<gt>. When sending bug reports, 
please provide the version of Geo::Distance, the version of Perl, and the name and version of the 
operating system you are using.  Patches are welcome!

=head1 SEE ALSO

L<Math::Trig> - Inverse and hyperbolic trigonemetric Functions.

L<http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1> - A overview of calculating distances.

=cut

#-------------------------------------------------------------------------------

1;
