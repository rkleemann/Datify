use v5.14;
use warnings;

package Datify::Path;
# ABSTRACT: Describe structures like filesystem paths.
# VERSION

use Carp            ();    #qw( carp croak );
use Datify          ();    #qw( self );
use Scalar::Util    ();    #qw( blessed refaddr reftype );
use String::Tools qw( subst );

our %SETTINGS = ();

### Public methods ###


=method C<< new( name => value, name => value, ... ) >>

Create a C<Datify::Path> object with the following options.

See L</OPTIONS> for a description of the options and their default values.

=cut

sub new {
    my $class = shift || __PACKAGE__;

    my %self = ();
    if ( defined( my $blessed = Scalar::Util::blessed($class) ) ) {
        %self  = %$class;    # shallow copy
        $class = $blessed;
    }
    return @_ ? bless( \%self, $class )->set(@_) : bless( \%self, $class );
}


=method C<get( name, name, ... )>

Get one or more existing values for one or more settings.
If passed no names, returns all parameters and values.

Can be called as a class method or an object method.

=cut

sub get {
    my $self = shift;
    my $count = scalar(@_);

    if ( defined( Scalar::Util::blessed($self) ) ) {
        return
              $count == 0 ? %{ { %SETTINGS, %$self } }
            : $count == 1 ?
                exists  $self->{ $_[0] }
                    ?   $self->{ $_[0] }
                    : $SETTINGS{ $_[0] }
            : map { exists $self->{$_} ? $self->{$_} : $SETTINGS{$_} } @_;
    } else {
        return
              $count == 0 ? %SETTINGS
            : $count == 1 ? $SETTINGS{ $_[0] }
            :               @SETTINGS{@_};
    }
}


=method C<< set( name => value, name => value, ... ) >>

Change the L</OPTIONS> settings.
When called as a class method, changes default options.
When called as an object method, changes the settings and returns a
new object.

See L</OPTIONS> for a description of the options and their default values.

B<NOTE:> When called as a object method, this returns a new instance
with the values set, so you will need to capture the return if you'd like to
persist the change:

 $datify = $datify->set( ... );

=cut

sub set {
    my $self = shift;
    return $self unless @_;
    my %set  = @_;

    my $return;
    my $class;
    if ( defined( $class = Scalar::Util::blessed($self) ) ) {
        # Make a shallow copy
        $self   = bless { %$self }, $class;
        $return = 0;
    } else {
        $class  = $self;
        $self   = \%SETTINGS;
        $return = 1;
    }




    my $internal = $class->isa( scalar caller );
    while ( my ( $k, $v ) = each %set ) {
        Carp::carp( 'Unknown key ', $k )
            unless $internal
            || exists $self->{$k}
            || exists $SETTINGS{$k};
        $self->{$k} = $v;
    }

    return ( $self, $class )[$return];
}


=method exists( name, name, ... )

Determine if values exists for one or more settings.

Can be called as a class method or an object method.

=cut

sub exists {
    my $self = shift;
    return unless my $count = scalar(@_);

    if ( Scalar::Util::blessed($self) ) {
        return $count == 1
            ? do {  exists $self->{ $_[0] } || exists $SETTINGS{ $_[0] } }
            : map { exists $self->{ $_ }    || exists $SETTINGS{ $_ } } @_;
    } else {
        return
            $count == 1 ? exists $SETTINGS{ $_[0] }
            :       map { exists $SETTINGS{ $_ } } @_;
    }
}


=method pathify( ... )

=cut

sub pathify {
    return unless defined( my $wantarray = wantarray );
    my $self = &Datify::self;
    local $_ = @_ == 0 ? $_ : @_ == 1 ? shift : \@_;

    my $values = $self->_cache_get($_) // [ $self->_scalar($_) ];
    if ( $self->isa( scalar caller ) ) {
        $self->_cache_add( $_ => $values );
    } else {
        $values = [ map $self->_flatten, @$values ];
        $self->_cache_reset();
    }
    return $wantarray ? @$values : $values;
}

### Private methods ###

__PACKAGE__->set(
    datify_options => {},
);

sub _datify {
    my $self = &Datify::self;
    my $datify = $self->get('_datify');
    if ( not $datify ) {
        $datify = Datify->new( %{ $self->get('datify_options') // {} } );
        $self->set( _datify => $datify );
    }
    return $datify;
}

__PACKAGE__->set(
    statement      => '$key = $value',
);

sub _flatten {
    my $self = &Datify::self;
    local $_ = shift if @_;
    my $ref = Scalar::Util::reftype($_);
    my ( $key, $value ) = $ref && $ref eq 'ARRAY' ? @$_ : ($_);

    if ( defined $value ) {
        $ref = Scalar::Util::reftype($value);
        my $statement = $self->get('statement');
        if ( not $ref ) {
            return subst(
                $statement,
                key   => $key,
                value => $self->_datify->keyify($value)
            );
        } elsif ( $ref eq 'ARRAY' ) {
            return $key . $self->_flatten($value);
        #} elsif ( $ref eq 'HASH' ) {
        #    return subst(
        #        $self->get('object'),
        #        class => $value->{class},
        #        key   => $key,
        #        value => $value->{value}
        #    );
        } elsif ( $ref eq 'SCALAR' ) {
            return subst(
                $statement,
                key   => $key,
                value => $$value
            );
        } else {
            die 'Unsure of how to handle ', $ref;
        }
    } else {
        return $key;
    }
}

__PACKAGE__->set(
    list_count     => '[$i/$n]',
);

sub _array {
    my $self = &Datify::self;
    local $_ = shift if @_;

    my $datify     = $self->_datify;
    my $list_count = $self->get('list_count');
    my $size       = $datify->numify( scalar @$_ );
    return [ subst( $list_count, i => 0, n => 0 ), undef ]
        if ( $size eq '0' );

    my $format = subst(
        $list_count,
        i => '%' . length($size) . 's',
        n => $size
    );

    my @structure;
    while ( my ( $i, $v ) = each @$_ ) {
        my $key = sprintf( $format, $datify->numify( 1 + $i ) );
        $self->_push_position($key);
        push @structure, map { [ $key, $_ ] } $self->pathify($v);
        $self->_pop_position();
    }
    return @structure;
}

__PACKAGE__->set(
    path_separator => '/',
);

sub _hash {
    my $self = &Datify::self;
    local $_ = shift if @_;

    my $path_separator = $self->get('path_separator');
    return [ $path_separator, undef ]
        if ( 0 == scalar keys %$_ );

    my $datify = $self->_datify;
    my @structure;
    foreach my $k ( $datify->hashkeys($_) ) {
        my $key = $path_separator . $datify->keyify($k);
        $self->_push_position($key);
        push @structure, map { [ $key, $_ ] } $self->pathify( $_->{$k} );
        $self->_pop_position();
    }
    return @structure;
}

# TODO:
#{
#    foo => bless(
#        {
#            alpha   => {},
#            bravo   => [],
#            charlie => 123,
#        },
#        'Foo::Bar'
#    )
#}
#   /foo/Foo::Bar=alpha/
#   /foo/Foo::Bar=bravo[0/0]
#   /foo/Foo::Bar=charlie = 123
#sub _object {
#}

sub _scalar {
    my $self = &Datify::self;
    local $_ = shift if @_;

    return undef unless defined;

    #if ( defined( my $blessed = Scalar::Util::blessed($_) ) ) {
    #    return $blessed eq 'Regexp' ? $self->_scalar("$_")
    #                                : $self->_object($_);
    #}

    my $ref = Scalar::Util::reftype $_;
    return
          not($ref)        ? $_
        : $ref eq 'ARRAY'  ? $self->_array($_)
        : $ref eq 'HASH'   ? $self->_hash($_)
        : $ref eq 'REGEXP' ? $self->_scalarify("$_")
        : $ref eq 'SCALAR' ? $self->_scalarify($$_)
        : $ref eq 'REF' && 'REF' ne Scalar::Util::reftype($$_)
                           ? $self->pathify($$_)
        :                    die 'Cannot handle ', $ref;
}

__PACKAGE__->set(
    _cache_hit => 1,
    nested     => '$key$subkey',
);

sub _push_position {
    my $self     = shift;
    my $position = shift;
    push @{ $self->{_position} //= [] }, $position;
    return $self;
}
sub _pop_position {
    my $self = shift;
    return pop @{ $self->{_position} };
}
sub _position {
    my $self = shift;

    my $nest = $self->get('nested');
    my $pos  = List::Util::reduce(
        sub { subst( $nest, key => $a, subkey => $b ) },
            @{ $self->{_position} //= [] }
    );
    return $pos // '';
}
sub _cache_add {
    my $self  = shift;
    my $ref   = shift;
    my $value = shift;

    return $self unless my $refaddr = Scalar::Util::refaddr $ref;
    my $_cache = $self->{_cache} //= {};
    my $entry = $_cache->{$refaddr} //= [ [ \$self->_position ] ];
    push @$entry, $value if @$entry == $self->get('_cache_hit');

    return $self;
}
sub _cache_get {
    my $self = shift;
    my $item = shift;

    return unless my $refaddr = Scalar::Util::refaddr $item;

    my $_cache = $self->{_cache} //= {};
    if ( my $entry = $_cache->{$refaddr} ) {
        my $repr = $self->get('_cache_hit');
        return $entry->[$repr]
            // Carp::croak 'Recursive structures not allowed at ',
                           $self->_position;
    } else {
        # Pre-populate the cache, so that we can check for loops
        $_cache->{$refaddr} = [ [ \$self->_position ] ];
        return;
    }
}
sub _cache_reset {
    my $self = shift;
    %{ $self->{_cache} //= {} } = ();
    delete $self->{_datify};
    return $self;
}



1;

__DATA__

=head1 SYNOPSIS

 use Datify::Path;

 my $pathify = Datify::Path->new();

 say foreach $pathify->pathify( [ qw( this that the-other ) ] );
 # [1/3] = this
 # [2/3] = that
 # [3/3] = 'the-other'

 say foreach $pathify->pathify( { a => 100, b => 1024, c => 102030 } );
 # /a = 100
 # /b = 1_024
 # /c = 102_030

 say foreach $pathify->pathify(
     {
         array  => [ 1, 10, 100, 10000, 100_000_000 ],
         hash   => { a => 'alpha', b => 'bravo', c => undef },
         nested => [
            { '!@#$%^&*()' => [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ] },
            { '' => 'empty string' },
            { " \t\n" => 'space, tab, & newline' },
            { 'empty array' => [] },
            { 'empty hash'  => {} },
         ],
     }
 );
 # /array[1/5] = 1
 # /array[2/5] = 10
 # /array[3/5] = 100
 # /array[4/5] = 10_000
 # /array[5/5] = 100_000_000
 # /hash/a = alpha
 # /hash/b = bravo
 # /hash/c
 # /nested[1/4]/'!@#$%^&*()'[ 1/10] = 1
 # /nested[1/4]/'!@#$%^&*()'[ 2/10] = 2
 # /nested[1/4]/'!@#$%^&*()'[ 3/10] = 3
 # /nested[1/4]/'!@#$%^&*()'[ 4/10] = 4
 # /nested[1/4]/'!@#$%^&*()'[ 5/10] = 5
 # /nested[1/4]/'!@#$%^&*()'[ 6/10] = 6
 # /nested[1/4]/'!@#$%^&*()'[ 7/10] = 7
 # /nested[1/4]/'!@#$%^&*()'[ 8/10] = 8
 # /nested[1/4]/'!@#$%^&*()'[ 9/10] = 9
 # /nested[1/4]/'!@#$%^&*()'[10/10] = 0
 # /nested[2/4]/'' = 'empty string'
 # /nested[3/4]/" \t\n" = 'space, tab, & newline'
 # /nested[4/4]/'empty array'[0/0]
 # /nested[4/4]/'empty hash'/

=head1 DESCRIPTION

Datify::Path will convert a data structure consisting of arrays, hashes,
and scalars into a form similar to a path listing.  This can be useful when
searching for a particular value, then finding the "path" that leads to it.

=head1 TODO

=head1 SEE ALSO

