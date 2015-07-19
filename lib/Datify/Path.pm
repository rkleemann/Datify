use v5.18;
use warnings;

package Datify::Path;

use Datify;

# Hack to use Datify::keysort
( *a, *b ) = ( *Datify::a, *Datify::b );

sub flatten {
    my ( $key, $value ) = @{ @_ ? $_[0] : $_ };
    if ( defined $value ) {
        if ( ref $value ) {
            # Assuming it's an ARRAY
            return $key . flatten($value);
        } else {
            return $key . ' = ' . Datify->keyify($value);
        }
    } else {
        return $key;
    }
}

sub pathify {
    return wantarray
        ?   map flatten, _pathify(@_)
        : [ map flatten, _pathify(@_) ];
}
sub _pathify {
    my ( $class, $s ) = @_;
    return undef unless defined $s;
    my $ref = ref $s;

    my @structure;
    if      ( 'ARRAY'  eq $ref ) {
        my $size = Datify->numify( scalar @$s );
        if ( 0 < $size ) {
            my $format = '[%' . length($size) . 's/' . $size . ']';
            while ( my ( $i, $v ) = each @$s ) {
                my $key = sprintf $format, Datify->numify( 1 + $i );
                push @structure,
                    map { [ $key, $_ ] }
                        Datify::Path->_pathify($v);
            }
        } else {
            push @structure, [ '[0/0]', undef ];
        }
    } elsif ( 'HASH'   eq $ref ) {
        if ( 0 < keys %$s ) {
            foreach my $k ( sort Datify::keysort keys(%$s) ) {
                my $key = '/' . Datify->keyify($k);
                push @structure,
                    map { [ $key, $_ ] }
                        Datify::Path->_pathify( $s->{$k} );
            }
        } else {
            push @structure, [ '/', undef ];
        }
    #} elsif ( 'REF'    eq $ref ) {
    #   TODO
    #} elsif ( 'SCALAR' eq $ref ) {
    #   TODO
    } elsif ( not $ref ) {
        return $s;
    } else {
        return "$s";
        #die 'Cannot handle ' . $ref;
    }

    return @structure;
}

1;
