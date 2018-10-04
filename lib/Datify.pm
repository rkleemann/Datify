use v5.14;

package Datify;
# ABSTRACT: Simple stringification of data.
# VERSION

=head1 SYNOPSIS

 use Datify;

 my $datify = Datify->new( ... );   # See OPTIONS below
 $datify = $datify->set( ... );     # See OPTIONS below

 print $datify->varify( data => [...] ), "\n";  # "@data = (...);\n"

 # Or

 print Datify->varify( data => [...] ), "\n";
 # "@data = (...);\n"

=head1 DESCRIPTION

C<Datify> is very similar to L<Data::Dumper>, except that it's
easier to use, and has better formatting and options.

=cut

use overload ();
use warnings;

use Carp               ();#qw( croak );
use List::Util    1.40 ();#qw( reduce sum );
use Scalar::Util  1.40 ();#qw( blessed looks_like_number refaddr reftype );
use String::Tools    qw( subst );
use Sub::Util     1.40 ();#qw( set_subname );

my %SETTINGS;

=method C<< add_handler( 'Class::Name' => \&code_ref ) >>

Add a handler to handle an object of type C<'Class::Name'>.  C<\&code_ref>
should take two parameters, a reference to Datify, and the object to be
Datify'ed.  It should return a representation of the object.

 # Set URI's to stringify as "URI->new('http://example.com')"
 # instead of "bless(\'http://example.com', 'URI')"
 Datify->add_handler( 'URI' => sub {
     my ( $datify, $uri ) = @_;
     my $s = $datify->stringify("$uri");
     return "URI->new($s)";
 } );

=cut

sub add_handler {
    no strict 'refs';
    my $pkg  = shift || __PACKAGE__; $pkg = ref $pkg || $pkg;
    my $name = _nameify(shift);
    *{$name} = Sub::Util::set_subname $name => shift;
}



### Constructor ###

=method C<< new( name => value, name => value, ... ) >>

Create a C<Datify> object with the following options.

See L</OPTIONS> for a description of the options and their default values.

=cut

sub new {
    my $self = shift || __PACKAGE__;
    if ( my $class = ref $self ) {
        return bless { %$self,    @_ }, $class;
    } else {
        return bless { %SETTINGS, @_ }, $self;
    }
}



### Setter ###

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
    my %set  = @_;

    my $return;
    my $class;
    if ( $class = ref $self ) {
        # Make a copy
        $self   = bless { %$self }, $class;
        $return = 0;
    } else {
        $class  = $self;
        $self   = \%SETTINGS;
        $return = 1;
    }

    delete $self->{keyword_set} if ( $set{keywords} );
    delete $self->{"tr$_"} for grep { exists $set{"quote$_"} } ( 1, 2, 3 );

    %$self = ( %$self, %set );

    return ( $self, $class )[$return];
}



### Accessor ###

=method C<get( name, name, ... )>

Get one or more existing values for one or more settings.
If passed no names, returns all parameters and values.

Can be called as a class method or an object method.

=cut

sub get {
    my $self = shift; $self = \%SETTINGS unless ref $self;
    my $count = scalar @_;
    if    ( $count == 0 ) { return %$self }
    elsif ( $count == 1 ) { return $self->{ +shift } }
    else                  { return @{$self}{@_} }
}



=option Varify options

=over

=item I<name>       => B<'$self'>

The name of the default variable.
This is also set as the first parameter to C<varify>.

=item I<assign>     => B<'$var = $value;'>

What an assignment statement should look like.  If the generated code
is to be run under C<use strict;>, then you may want to change this to
C<'my $var = $value;'>.

=item I<list>       => B<'($_)'>

The delimiters for a list.

=item I<beautify>   => B<undef>

Set this to a C<CODE> reference that you would like to use to beautify
the code.  It should accept the code as the first parameter, process it,
and return the code after all the beauty modifications have been completed.

An example:

 use Datify;
 use Perl::Tidy;

 sub beautify {
     my $source = shift;

     my ($dest, $stderr);
     Perl::Tidy::perltidy(
         argv => [ qw(
             --noprofile
             --nostandard-output
             --standard-error-output
             --nopass-version-line
         ) ],
         source      => \$source,
         destination => \$dest,
         stderr      => \$stderr,
         errorfile   => \$stderr,
     ) && die $stderr;

     return $dest;
 }

 Datify->set( beautify => \&beautify );

 say Datify->varify( var => $var );

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Var options
    name        => '$self',
    assign      => '$var = $value;',
    list        => '($_)',
    beautify    => undef,
);

# Name can be any of the following:
# * package name (optional) followed by:
#   * normal word
#   * ::
# * Perl special variable:
#   * numbers
#   * punctuation
#   * control character
#   * control word
my $sigils     = '[\\x24\\x25\\x40]'; # $%@
my $package    = '[[:alpha:]]\w*(?:\::\w+)*';
my $word       = '[[:alpha:]_]\w*';
my $digits     = '\d+';
my $punct      = '[[:punct:]]';
my $cntrl      = '(?:[[:cntrl:]]|\^[[:upper:]])';
my $cntrl_word = "$cntrl$word";
my $varname
    = '(?:' . join( '|', $word, $digits, $punct, $cntrl, $cntrl_word ) . ')';
$varname .= "|\\{\\s*$varname\\s*\\}";
$varname  = "(?:$varname)";

=method C<< varify( name => value, value, ... ) >>

Returns an assignment statement for the values.  If C<name> does not begin
with a sigil (C<$>, C<@>, or C<%>), will determine which sigil to use based
on C<values>.

Some examples:

Common case, determine the type and add the correct sigil to 'foo'.

 Datify->varify(   foo  => $foo );

Specify the type.

 Datify->varify( '$foo' => $foo );

Handle a list: C<@foo = (1, 2, 3);>

 Datify->varify( '@foo' =>   1, 2, 3   );
 Datify->varify( '@foo' => [ 1, 2, 3 ] );
 Datify->varify(   foo  =>   1, 2, 3   );
 Datify->varify(   foo  => [ 1, 2, 3 ] );

Handle a hash: C<< %foo = (a => 1, b => 2, c => 3); >>
(B<Note>: Order may be rearranged.)

 Datify->varify( '%foo' =>   a => 1, b => 2, c => 3   );
 Datify->varify( '%foo' => { a => 1, b => 2, c => 3 } );
 Datify->varify(   foo  => { a => 1, b => 2, c => 3 } );

Keep in mind that without proper hints, this would be interpretted as a list,
not a hash:

 Datify->varify(   foo  =>   a => 1, b => 2, c => 3   );
 # "@foo = ('a', 1, 'b', 2, 'c', 3);"

=cut

sub varify {
    my $self = &self;
    my ($sigil, $name);
    if ( defined $_[0] && !ref $_[0] ) {
        ( $sigil, $name )
            = $_[0] =~ /^($sigils)?((?:$package\::)?$varname|$package\::)$/;
        shift if defined $name && length $name;
    }
    my $value = 1 == @_ ? shift : \@_;

    if ( defined $name && length $name ) {
        if ( $name =~ /[[:cntrl:]]/ ) {
            $name =~ s/([[:cntrl:]])/'^' . chr(64 + ord($1) % 64)/e;
            $name =~ s/($cntrl_word)(?!\s*\})/\{$1\}/;
        }
    } else {
        if ( my $ref = ref $value ) {
            $name = _nameify($ref);
        } else {
            $name = $self->{name};
        }
    }
    Carp::croak "Missing name" unless ( defined $name && length $name );

    unless ($sigil) {
        my $ref = ref $value;
        if    ( $ref eq 'ARRAY' ) { $sigil = '@'; }
        elsif ( $ref eq 'HASH' )  { $sigil = '%'; }
        else                      { $sigil = '$'; }
    }
    $name = $sigil . $name;
    $self = $self->set( name => $name );

    if    ( $sigil eq '$' ) { $value = $self->scalarify($value); }
    elsif ( $sigil eq '@' ) {
        $value = subst( $self->{list}, $self->listify($value) );
    }
    elsif ( $sigil eq '%' ) {
        $value = subst( $self->{list}, $self->pairify($value) );
    }
    else    { $value = $self->scalarify($value); }

    $value = subst( $self->{assign}, var => $name, value => $value );
    if ( $self->{beautify} ) {
        return $self->{beautify}->($value);
    } else {
        return $value;
    }
}



### Scalar: undef ###

=option Undefify options

=over

=item I<null> => B<'undef'>

What to use as the null value.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Undef options
    null => 'undef',
);

=method C<undefify>

Returns the string that should be used for an undef value.

=cut

sub undefify {
    my $self = &self;
    return $self->scalarify(shift) if @_ and defined($_[0]);
    return $self->{null};
}



### Scalar: boolean ###

=option Booleanify options

=over

=item I<true>    => B<1>

=item I<false>   => B<"''">

What to use as the values for C<true> and C<false>, respectively.
Since Perl does not have native boolean values, these are placeholders.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Boolean options
    true    => 1,
    false   => "''",
);

=method C<booleanify( value )>

Returns the string that represents the C<true> or C<false> interpretation
of value.

=cut

sub booleanify {
    my $self = &self;
    local $_ = shift;
    return $self->undefify unless defined;
    return $_ ? $self->{true} : $self->{false};
}



### Scalar: single-quoted string ###

=method C<stringify1( value I<, delimiters> )>

Returns the string that represents value as a single-quoted string.
The delimiters parameter is optional.

=cut

sub stringify1 {
    my $self = &self;
    local $_ = shift;
    $_ = "$_" if ref;
    return $self->undefify unless defined;
    my ( $open, $close ) = $self->_get_delim( shift // $self->{quote1} );

    # single-quote and backslash.
    s/([$open$close\x5c])/\\$1/g;

    if ( $self->{quote1} ne $open ) {
        if ( $open =~ /\w/ ) {
            $open  = ' ' . $open;
            $close = ' ' . $close;
        }
        $open = $self->{q1} . $open;
    }

    return sprintf '%s%s%s', $open, $_, $close;
}



### Scalar: double-quoted string ###

=method C<stringify2( value I<, delimiters> )>

Returns the string that represents value as a double-quoted string.
The delimiters parameter is optional.

=cut

sub stringify2 {
    my $self = &self;
    local $_ = shift;
    $_ = "$_" if ref;
    return $self->undefify unless defined;
    my ( $open, $close ) = $self->_get_delim( shift // $self->{quote2} );

    my $sigils = $self->{sigils} =~ s/(.)/$self->_encode($1)/egsr;

    # quote char(s), sigils, and backslash.
    s/([$open$close$sigils\x5c])/\\$1/g;
    s/([[:cntrl:]])/$self->_encode($1)/eg;

    if ( $self->{quote2} ne $open ) {
        if ( $open =~ /\w/ ) {
            $open  = ' ' . $open;
            $close = ' ' . $close;
        }
        $open = $self->{q2} . $open;
    }

    return sprintf '%s%s%s', $open, $_, $close;
}



### Scalar: string ###

=option Stringify options

=over

=item I<quote>   => B<undef>

What to use as the default quote character.
If set to a false value, then use the best guess.
See L</stringify( value )>.

=item I<quote1>  => B<"'">

The default single-quoting character.

=item I<quote2>  => B<'"'>

The default double-quoting character.

=item I<q1>      => B<'q'>

The special single-quoting character starter.

=item I<q2>      => B<'qq'>

The special double-quoting character starter.

=item I<sigils>  => B<'$@'>

The characters in a double quoted sting that need to be quoted,
or they may be interpreted as variable interpolation.

=item I<longstr> => B<1_000>

How long a string needs to be before it's considered long.
See L</stringify( value )>.
Change to a false value to indicate no string is long.
Change to a negative value to indicate every string is long.

=item I<encode>  => B<<
{
0 => '\0',
7 => '\a',
9 => '\t',
10 => '\n',
12 => '\f',
13 => '\r',
27 => '\e',
byte => '\x%02x',
wide => '\x{%04x}'
}
>>

How to encode characters that need encoding.

=item I<qpairs>  => B<< [ qw\ () <> [] {} \ ] >>

=item I<qquotes> => B<[ qw\ ! # % & * + , - . / : ; = ? ^ | ~ $ @ ` \ ]>

When determining the quote character to use, go through these lists to see
which character would work best.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # String options
    quote   => undef,   # Auto
    quote1  => "'",
    #tr1     => q!tr\\'\\'\\!,
    quote2  => '"',
    #tr2     => q!tr\\"\\"\\!,
    q1      => 'q',
    q2      => 'qq',
    sigils  => '$@',
    longstr => 1_000,
    encode  => {
        map( { ord( eval qq!"$_"! ) => $_ } qw( \0 \a \t \n \f \r \e ) ),
        #0x00 => '\\0',
        #0x07 => '\\a',
        ##0x08 => '\\b',   # Does \b mean backspace or word-boundary?
        #0x09 => '\\t',
        #0x0a => '\\n',
        #0x0c => '\\f',
        #0x0d => '\\r',
        #0x1b => '\\e',
        byte => '\\x%02x',
        wide => '\\x{%04x}',
    },

    do {
        no warnings 'qw';
        # To silence the warnings:
        # Possible attempt to put comments in qw() list
        # Possible attempt to separate words with commas

        qpairs  => [ qw\ () <> [] {} \ ],
        qquotes => [
            # Punctuation, excluding ", ', \, and _
            qw\ ! # % & * + , - . / : ; = ? ^ | ~ $ @ ` \
        ],
    },
);

=method C<stringify( value )>

Returns the string the represents value.  It will be a double-quoted string
if it is longer than the C<longstr> option or contains control characters.
It will be a single-quoted string unless there are single-quotes within the
string, then it will be a double-quoted string, unless it also contains
double-quotes within the string, then it will attempt to find the best quote
character.

=cut

sub stringify {
    my $self = &self;
    local $_ = shift;
    return $self->undefify unless defined;
    $_ = "$_" if ref;
    local $@ = undef;

    if ( $self->{quote} ) {
        return $self->stringify1($_) if $self->{quote1} eq $self->{quote};
        return $self->stringify2($_) if $self->{quote2} eq $self->{quote};
        Carp::croak("Bad setting for quote: $self->{quote}");
    }

    # Long strings or strings with control characters
    my $longstr = $self->{longstr};
    return $self->stringify2($_)
        if ( $longstr && $longstr < length() || /[[:cntrl:]]/ );

    my $quote1 = $self->{quote1};
    $self->{tr1} ||= "tr\\$quote1\\$quote1\\";
    my $single_quotes  = eval $self->{tr1} // die $@;
    return $self->stringify1($_) unless $single_quotes;

    my $quote2 = $self->{quote2};
    my $sigils = $self->{sigils};
    #$self->{tr2} ||= "tr\\$quote2\\$quote2\\";
    #my $double_quotes = eval $self->{tr2} // die $@;
    #my $sigil_count   = eval "tr\\$sigils\\$sigils\\" // die $@;
    #return $self->stringify2($_) unless $double_quotes + $sigil_count;
    $self->{tr2} ||= "tr\\$quote2$sigils\\$quote2$sigils\\";
    my $double_quotes = eval $self->{tr2} // die $@;
    return $self->stringify2($_) unless $double_quotes;

    return $self->stringify1( $_, $self->_find_q($_) );
}



### Scalar: number ###
# Adapted from Perl FAQ "How can I output my numbers with commas added?"

=option Numify options

=over

=item I<infinite>  => B<"'inf'">

What to use to indicate infinity.

=item I<-infinite>  => B<"'-inf'">

What to use to indicate negative infinity.

=item I<nonnumber> => B<"'nan'">

What to use to indicate this is not a number.

=item I<num_sep>   => B<'_'>

What character to use to seperate sets of numbers.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Number options
    infinite  => "'inf'",
    -infinite => "'-inf'",
    nonnumber => "'nan'",
    num_sep   => '_',
);

=method C<is_numeric( value )>

Returns true  if value is can be numeric,
returns false if the value is not numeric (including inf and nan),
returns undef if the value is undefined.

 Datify->is_numeric(1234.5678901);       #          true
 Datify->is_numeric("inf");              #          false
 Datify->is_numeric( "inf" / "inf" );    # "nan" => false
 Datify->is_numeric(undef);              #          undef

=cut

sub is_numeric {
    my $self = &self;
    local $_ = shift;

    return undef unless defined;

    if (ref) {
        if ( my $method = $self->overloaded($_) ) {
            $_ = $_->$method();
        }
        else {
            return '';
        }
    }

    # The "defined" and Regexp ensure that we're not
    # considering inf, nan, and their variants as numeric
    # (even though looks_like_number does)
    return Scalar::Util::looks_like_number($_)
        && defined( $_ <=> 0 )
        && !/^\s*[+-]?(?i:inf(?:inity)?|nan)\s*$/;
}

=method C<numify( value )>

Returns value with seperators between the hundreds and thousands,
hundred-thousands and millions, etc.  Similarly for the fractional parts.

 Datify->numify(1234.5678901);    # "1_234.56_789_01"

Also returns the string that should be used for the C<infinite>,
C<-infinite>, and C<nonnumber> values,
the C<null> value for undefined values,
and C<nonnumber> value for all not-a-numbers.

 Datify->numify('inf');              # 'inf'
 Datify->numify( 'inf' / 'inf' );    # 'nan'
 Datify->numify(undef);              # undef
 Datify->numify('apple');            # 'nan'

=cut

sub numify {
    my $self = &self;
    local $_ = shift;

    return $self->undefify unless defined;

    if ( $self->is_numeric($_) ) {
        return $_ unless my $sep = $self->{num_sep};

        # Fractional portion
                s{^(\s*[-+]?\d*\.\d\d)(\d+)}              [${1}$sep${2}];
        1 while s{^(\s*[-+]?\d*\.(?:\d+$sep)+\d\d\d)(\d+)}[${1}$sep${2}];

        # Whole portion
        1 while s{^(\s*[-+]?\d+)(\d{3})}                  [${1}$sep${2}];

        return $_;
    }
    elsif ( Scalar::Util::looks_like_number($_) ) {
        if ( not defined( $_ <=> 0 ) ) { return $self->{nonnumber}  }
        elsif ( $_ ==  'inf' )         { return $self->{infinite}   }
        elsif ( $_ == -'inf' )         { return $self->{-infinite}  }
    }

    return $self->{nonnumber};
}



### Scalar ###

=method C<scalarify( value )>

Returns value as a scalar.  If value is not a reference, performs some magic
to correctly print vstrings and numbers, otherwise assumes it's a string.
If value is a reference, hands off to the correct function to create
the string.

Handles reference loops.

=cut

sub scalarify {
    my $self = &self;
    my $s = shift;

    return $self->undefify unless defined $s;

    my $ref = ref $s;
    if ( $ref eq '' ) {
        # Handle GLOB, LVALUE, and VSTRING
        my $ref2 = ref \$s;
        if    ( $ref2 eq 'GLOB' )    { return $self->globify($s);    }
        elsif ( $ref2 eq 'LVALUE' )  { return $self->lvalueify($s);  }
        elsif ( $ref2 eq 'VSTRING' ) { return $self->vstringify($s); }

        # All other non-ref types.
        return $self->numify($s)
            if ( Scalar::Util::looks_like_number($s) );
        return $self->stringify($s);
    }

    if ( my $_cache = $self->_cache($s) ) {
        return $_cache;
    }

    if    ( $ref eq 'ARRAY' )   { return $self->arrayify($s);  }
    elsif ( $ref eq 'CODE' )    { return $self->codeify($s);   }
    elsif ( $ref eq 'FORMAT' )  { return $self->formatify($s); }
    elsif ( $ref eq 'HASH' )    { return $self->hashify($s);   }
    elsif ( $ref eq 'IO' )      { return $self->ioify($s);     }
    elsif ( $ref eq 'REF' )     { return $self->refify($s);    }
    elsif ( $ref eq 'Regexp' )  { return $self->regexpify($s); }

    elsif ( $ref eq 'GLOB' )    {
        return subst( $self->{reference}, $self->globify($$s) );
    }
    elsif ( $ref eq 'LVALUE' )  {
        return subst( $self->{reference}, $self->lvalueify($$s) );
    }
    elsif ( $ref eq 'SCALAR' )  {
        return subst( $self->{reference}, $self->scalarify($$s) );
    }
    elsif ( $ref eq 'VSTRING' ) {
        return subst( $self->{reference}, $self->vstringify($$s) );
    }
    else {
        if ( my $code = $self->_find_handler($ref) ) {
            return $self->$code($s);
        } else {
            return $self->objectify($s);
        }
    }
}



### Scalar: LValue ###

=option LValueify options

=over

=item I<lvalue>  => B<'substr($lvalue, 0)'>

How to generate a LValue.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # LValue options
    lvalue  => 'substr($lvalue, 0)',
);

=method C<lvalueify( value )>

Returns an approximate representation of what the lvalue is.

=cut

sub lvalueify {
    my $self = &self;
    return subst( $self->{lvalue}, lvalue => $self->stringify(shift) );
}



### Scalar: VString ###

=option VStringify options

=over

=item I<vformat> => B<'v%vd'>

=item I<vsep>    => B<undef>

The formatting string to use.  If I<vsep> is set, then I<vformat> should use
the C<*> format to inidicate what I<vsep> will be:
C<< vformat => 'v%*vd', vsep => '.' >>.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # VString options
    vformat => 'v%vd',
    #vformat => 'v%*vd',
    #vsep    => '.',
);

=method C<vstringify( value )>

A representation of the VString, in dotted notation.

=cut

sub vstringify {
    my $self = &self;
    if ( defined $self->{vsep} ) {
        return sprintf $self->{vformat}, $self->{vsep}, shift;
    } else {
        return sprintf $self->{vformat}, shift;
    }
}



### Regexp ###

=option Regexpify options

=over

=item I<quote3>  => B<'/'>

=item I<q3>      => B<'qr'>

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Regexp options
    quote3  => '/',
    #tr3     => q!tr\\/\\/\\!,
    q3      => 'qr',
);

=method C<regexpify( value, delimiters )>

A representation of the C<Regexp> in C<value>.

=cut

sub regexpify {
    my $self = &self;
    local $_ = shift;
    local $@ = undef;

    $self->{tr3} ||= "tr\\$self->{quote3}\\$self->{quote3}\\";
    my $quoter = eval $self->{tr3} // die $@;
    my ( $open, $close )
        = $self->_get_delim(
            shift // $quoter ? $self->_find_q($_) : $self->{quote3} );

    # Everything but the quotes should be escaped already.
    s/([$open$close])/\\$1/g;
    s/([[:cntrl:]])/$self->_encode($1)/eg;

    if ( $open =~ /\w/ ) {
        $open  = ' ' . $open;
        $close = ' ' . $close;
    }

    $open = $self->{q3} . $open;

    return sprintf '%s%s%s', $open, $_, $close;
}



### List/Array ###

=method C<listify( value, value, ... )>

Returns value(s) as a list.

 Datify->listify( 1, 2, 3 );    # '1, 2, 3'

=cut

sub listify {
    my $self = &self;
    if (1 == @_) {
        my $ref = Scalar::Util::reftype $_[0];
        if    ( $ref eq 'HASH' )  { @_ = %{ +shift } }
        elsif ( $ref eq 'ARRAY' ) {
            my $array = shift;
            $self->{_cache}{ +Scalar::Util::refaddr $array }
                = $self->_name_and_position;
            @_ = @$array;
        }
    }
    my @values;
    for ( my $i = 0; $i < @_; $i++ ) {
        my $value = $_[$i];
        push @{ $self->{_position} //= [] }, "[$i]";
        push @values, $self->scalarify($value);
        pop  @{ $self->{_position} };
    }
    return join($self->{list_sep}, @values);
}



=option Arrayify options

=over

=item I<array_ref>   => B<'[$_]'>

The representation of an array reference.

=item I<list_sep>    => B<', '>

The representation of the separator between list elements.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Array options
    array_ref   => '[$_]',
    list_sep    => ', ',
);

=method C<arrayify( value, value, ... )>

Returns value(s) as an array.

 Datify->arrayify( 1, 2, 3 );    # '[1, 2, 3]'

=cut

sub arrayify {
    my $self = &self;
    return subst( $self->{array_ref}, $self->listify(@_) );
}



### Hash ###

=method C<keyify( value )>

Returns value as a key.  If value does not need to be quoted, it will not be.
Verifies that value is not a keyword.

=cut

sub keyify {
    my $self = &self;
    local $_ = shift;

    return $self->undefify unless defined;
    return $_ if ref;

    $self->{keyword_set} = { map { $_ => 1 } @{ $self->{keywords} } }
        unless $self->{keyword_set};

    if ( Scalar::Util::looks_like_number($_) ) {
        return $self->numify($_);
    } elsif ( /^-?[[:alpha:]_]\w*$/ && ! $self->{keyword_set}{$_} ) {
        # If the key would be autoquoted by the fat-comma (=>),
        # then there is no need to quote it.

        return "$_"; # Make sure it's stringified.
    }
    return $self->stringify($_);
}



=func C<keysort>

Not a method, but a sorting routine that sorts numbers (using C<< <=> >>)
before strings (using C<cmp>).

=cut

sub keysort($$);
BEGIN {
    if ( $^V >= v5.16.0 ) {
        *keysort = Sub::Util::set_subname(
            keysort => sub($$) {
                my $na = Scalar::Util::looks_like_number( $_[0] )
                    && defined( $_[0] <=> 0 );
                my $nb = Scalar::Util::looks_like_number( $_[1] )
                    && defined( $_[1] <=> 0 );
                if ( $na && $nb ) { return $_[0] <=> $_[1] }
                elsif ($na) { return -1 }
                elsif ($nb) { return +1 }
                else {
                    return CORE::fc( $_[0] ) cmp CORE::fc( $_[1] )
                        ||           $_[0]   cmp           $_[1];
                }
            }
        );
    } else {
        *keysort = Sub::Util::set_subname(
            keysort => sub($$) {
                my $na = Scalar::Util::looks_like_number( $_[0] )
                    && defined( $_[0] <=> 0 );
                my $nb = Scalar::Util::looks_like_number( $_[1] )
                    && defined( $_[1] <=> 0 );
                if ( $na && $nb ) { return $_[0] <=> $_[1] }
                elsif ($na) { return -1 }
                elsif ($nb) { return +1 }
                else        { return $_[0] cmp $_[1] }
            }
        );
    }
}


=method C<hashkeys( $hash )>

Returns the keys of a hash,
filtered (see L<< /I<keyfilter>        => B<undef> >>),
and sorted (see L</keysort>).

=cut

sub hashkeys {
    my $self = shift;
    my $hash = shift;

    my @keys = keys %$hash;
    if ( my $ref = ref( my $keyfilter = $self->{keyfilter} ) ) {
        my $keyfilternot     = !$self->{keyfilterdefault};
        my $keyfilterdefault = !$keyfilternot;
        if ( $ref eq 'ARRAY' || $ref eq 'HASH' ) {
            my %keyfilterhash
                = $ref eq 'ARRAY'
                ? ( map { $_ => $keyfilternot } @$keyfilter )
                : %$keyfilter;
            $self->{keyfilter} = $keyfilter = sub {
                exists $keyfilterhash{$_}
                    ? $keyfilterhash{$_}
                    : $keyfilterdefault;
            };
        } elsif ( $ref eq 'CODE' ) {
            # No-op, just use the code provided
        } elsif ( $ref eq 'Regexp' ) {
            my $keyfilterregexp = $keyfilter;
            $self->{keyfilter} = $keyfilter = sub {
                m/$keyfilterregexp/ ? $keyfilternot : $keyfilterdefault;
            };
        } elsif ( $ref eq 'SCALAR' ) {
            my $keyfiltervalue = $$keyfilter;
            $self->{keyfilter} = $keyfilter = sub { $keyfiltervalue };
        }
        @keys = grep { $keyfilter->() } @keys;
    }
    if ( my $keysort = $self->{keysort} ) {
        @keys = sort $keysort @keys;
    }
    return @keys;
}

sub hashkeyvals {
    my $self = shift;
    my $hash = shift;

    return map { $_ => $hash->{$_} } $self->hashkeys($hash);
}

=method C<< pairify( value => value, ... ) >>

Returns value(s) as a pair.

 Datify->pairify( a => 1, b => 2 );    # 'a => 1, b => 2'

=cut

sub pairify {
    my $self = &self;
    if (1 == @_) {
        my $ref = Scalar::Util::reftype $_[0];
        if    ( $ref eq 'ARRAY' ) { @_ = @{ +shift } }
        elsif ( $ref eq 'HASH' )  {
            my $hash = shift;
            $self->{_cache}{ +Scalar::Util::refaddr $hash }
                = $self->_name_and_position;

            @_ = $self->hashkeyvals($hash);
        }
    }
    # Use for loop in order to preserve the order of @_,
    # rather than each %{ { @_ } }, which would mix-up the order.
    my @list;
    for ( my $i = 0; $i < @_ - 1; $i += 2 ) {
        my ( $k, $v ) = @_[ $i, $i + 1 ];
        my $key = $self->keyify($k);
        push @{ $self->{_position} //= [] }, "{$key}";
        my $val = $self->scalarify($v);
        pop  @{ $self->{_position} };
        push @list, subst( $self->{pair}, key => $key, value => $val );
    }
    return join($self->{list_sep}, @list);
}



=option Hashify options

=over

=item I<hash_ref>         => B<'{$_}'>

The representation of a hash reference.

=item I<pair>             => B<< '$key => $value' >>

The representation of a pair.

=item I<keyfilter>        => B<undef>

A reference to an C<ARRAY>, C<CODE>, C<HASH>, C<Regexp>, or C<SCALAR>,
which will be converted to a the appropriate code, and used to filter
the keys in a hash via C<grep>.

C<ARRAY> entries are changed into a C<HASH>,
with the entries set to be the inverse of C<keyfilterdefault>.

C<CODE> entires should look for the key name in C<$_>,
and return a boolean value.

C<HASH> entries should have a true or false value,
to indicate if the entry should be included.

C<Regexp> entries are matched, and if true, then return the inverse of
C<$keyfilterdefault>.

C<SCALAR> entries treat all values according to the boolean evaluation.

=item I<keyfilterdefault> => B<1>

When filtering keys in a hash, if the key is not found in the C<keyfilter>
C<HASH> or C<ARRAY>, should it pass through or not?

=item I<keysort>          => B<\&Datify::keysort>

How to sort the keys in a hash.  This has a performance hit,
but it makes the output much more readable.  See the description of
L</keysort>.

=item I<keywords>         => B<[qw(undef)]>

Any keywords that should be quoted, even though they may not need to be.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Hash options
    hash_ref         => '{$_}',
    pair             => '$key => $value',
    keysort          => \&Datify::keysort,
    keyfilter        => undef,
    keyfilterdefault => 1,
    keywords         => [qw(undef)],
    #keyword_set      => { 'undef' => 1 },
);

=method C<hashify( value, value, ... )>

Returns value(s) as a hash.

 Datify->hashify( a => 1, b => 2 );    # '{a => 1, b => 2}'

=cut

sub hashify  {
    my $self = &self;
    return subst( $self->{hash_ref}, $self->pairify(@_) );
}



### Objects ###

=method C<overloaded( $object )>

Returns the first method from the C<overloads> list that $object
has overloaded.  If nothing is overloaded, then return nothing.

=cut

sub overloaded {
    my $self   = shift; $self = $self->new() unless ref $self;
    my $object = shift;

    return unless Scalar::Util::blessed($object)
        && overload::Overloaded($object);

    foreach my $overload ( @{ $self->{overloads} } ) {
        if ( my $method = overload::Method( $object => $overload ) ) {
            return $method;
        }
    }
    return;
}



=option Objectify options

=over

=item I<overloads>  => B<[ '""', '0+' ]>

The list of overloads to check for before deconstructing the object.
See L<overload> for more information on overloading.

=item I<object>     => B<'bless($data, $class_str)'>

The representation of an object.  Other possibilities include
C<'$class($data)'> or C<< '$class->new($data)' >>.

=item I<io>         => B<'*UNKNOWN{IO}'>

The representation of unknown IO objects.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Object options
    overloads => [ '""', '0+' ],
    object    => 'bless($data, $class_str)',
    #object    => '$class($data)',
    io        => '*UNKNOWN{IO}',
);

=method C<objectify( value )>

Returns value as an object.

 Datify->objectify($object);    # "bless({}, 'Object')"

=cut

sub objectify {
    my $self   = shift; $self = $self->new() unless ref $self;
    my $object = shift;

    return $self->scalarify($object)
        unless defined( my $class = Scalar::Util::blessed $object );

    my $data;
    if ( my $method = $self->overloaded($object) ) {
        $data = $self->scalarify( $object->$method() );
    } elsif ( my $keyvals = $object->can('_attrkeyvals') ) {
        $data = subst( $self->{hash_ref},
            $self->pairify( $object->$keyvals() ) );
    } else {
        $data = Scalar::Util::reftype $object;

        # Huh?!
        #if ( $data eq '' ) { return $self->scalarify( $object ) }

        if    ( $data eq 'HASH' )   {
            # TODO: Look this up via meta-objects and such.
            if ( my $attrkeys = $object->can('_attrkeys') ) {
                my @attrs = $object->$attrkeys();
                $data = subst( $self->{hash_ref},
                    $self->pairify( map { $_ => $object->{$_} } @attrs ) );
            } else {
                                      $data = $self->hashify(  {%$object} )
            }
        }
        elsif ( $data eq 'ARRAY' )  { $data = $self->arrayify( [@$object] ) }
        elsif ( $data eq 'CODE' )   { $data = $self->codeify(    $object  ) }
        elsif ( $data eq 'FORMAT' ) { $data = $self->formatify(  $object  ) }
        elsif ( $data eq 'GLOB' )   { $data = $self->globify(    $object  ) }
        elsif ( $data eq 'IO' )     { $data = $self->ioify(      $object  ) }
        elsif ( $data eq 'REF' )    { $data = $self->refify(     $object  ) }
        elsif ( $data eq 'Regexp' ) { $data = $self->regexpify(  $object  ) }
        elsif ( $data eq 'SCALAR' ) { $data = $self->refify(    $$object  ) }

        else { $data = "*UNKNOWN{$data}" } # ???
    }

    return subst(
        $self->{object},
        class_str => $self->stringify($class),
        class     => $class,
        data      => $data
    );
}



### Objects: IO ###

=method C<ioify( value )>

Returns a representation of value that is accurate if C<value> is
STDIN, STDOUT, or STDERR.  Otherwise, returns the C<io> setting.

=cut

sub ioify {
    my $self = &self;
    my $io   = shift;
    foreach my $ioe (qw(IN OUT ERR)) {
        no strict 'refs';
        if ( *{"main::STD$ioe"}{IO} == $io ) {
            return "*STD$ioe\{IO}";
        }
    }
    # TODO
    #while ( my ( $name, $glob ) = each %main:: ) {
    #    no strict 'refs';
    #    if ( defined( *{$glob}{IO} ) && *{$glob}{IO} == $io ) {
    #        keys %main::; # We're done, so reset each()
    #        return "*$name\{IO}";
    #    }
    #}
    return $self->{io};
}



### Other ###

=option Codeify options

=over

=item I<code>    => B<'sub {$_}'>

The representation of a code reference.  This module does not currently
support decompiling code to make a complete representation, but if passed
a representation, can wrap it in this.

=item I<body>    => B<'...'>

The representation of the body to a code reference.
This module does not currently support decompiling code to make a
complete representation.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Code options
    code    => 'sub {$_}',
    body    => '...',
);

=method C<codeify( value )>

Returns a subroutine definition that is not likely to encode value.

 Datify->codeify( \&subroutine );    # 'sub {...}'

However,
if C<value> is a string, then wrap that string with C<code>,
or
if C<value> is a reference to something other than C<CODE>,
represent that reference by wrapping it with C<code>.

=cut

sub codeify   {
    my $self = &self;
    if ( ! @_ || 'CODE' eq ref $_[0] ) {
        return subst $self->{code}, $self->{body};
    } else {
        my $code = shift;
        if ( ! defined $code || ref $code ) {
            $code = $self->scalarify($code);
        }
        return subst $self->{code}, $code;
    }
}



=option Refify options

=over

=item I<reference>   => B<'\\$_'>

The representation of a reference.

=item I<dereference> => B<< '$referent->$place' >>

The representation of dereferencing.

=item I<nested>      => B<'$referent$place'>

The representation of dereferencing a nested reference.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Reference options
    reference   => '\\$_',
    dereference => '$referent->$place',
    nested      => '$referent$place',
);

=method C<refify( value )>

Returns value as reference.

=cut

sub refify    {
    my $self = &self;
    local $_ = shift;
    $_ = $$_ if ref;
    return subst( $self->{reference}, $self->scalarify($_) );
}



=option Formatify options

=over

=item I<format>  => B<"format UNKNOWN =\n.\n">

The representation of a format.  This module does not currently support
showing the acutal representation.

=back

=cut

%SETTINGS = (
    %SETTINGS,

    # Format options
    format  => "format UNKNOWN =\n.\n",
);

=method C<formatify( value )>

Returns a value that is not completely unlike value.

=cut

sub formatify {
    my $self = &self;
    #Carp::croak "Unhandled type: ", ref shift;
    return $self->{format};
}



=method C<globify( value )>

Returns a representation of value.
For normal values, remove the leading C<main::>.

=cut

sub globify   {
    my $self = &self;
    my $name = '' . shift;
    if ( $name =~ /^\*$package\::(?:$word|$digits)?$/ ) {
        $name =~ s/^\*main::/*::/;
    } else {
        $name =~ s/^\*($package\::.+)/'*{' . $self->stringify($1) . '}'/e;
    }
    return $name;
}

### Internal ###
### Do not use these methods outside of this package,
### they are subject to change or disappear at any time.
sub self {
    my $self = shift;
    return ref $self ? $self : $self->new();
}
sub _nameify {
    local $_ = shift;
    s/::/_/g;
    return lc() . 'ify';
}
sub _find_handler {
    my $self = shift;
    return $self->can( _nameify(shift) );
}

sub _get_delim {
    my $self = shift;
    my $open = shift;

    my $close;
    if ( 1 < length $open ) {
        my %qpairs = map { $_ => 1 } @{ $self->{qpairs} };
        if ( $qpairs{$open} ) {
            ( $open, $close ) = split //, $open, 2;
        } else {
            ( $open ) = split //, $open, 1
        }
    }
    $close = $open unless $close;

    return $open, $close;
}

sub _encode {
    my $self = shift;
    my $ord  = ord shift;

    my $encoding;
    my $encodings = $self->{encode};
    if ( exists $encodings->{$ord} ) {
        return $encodings->{$ord};
    } elsif ( $ord <= 255 ) {
        $encoding = $encodings->{byte};
    } else {
        $encoding = $encodings->{wide} // $encodings->{byte};
    }

    return sprintf $encoding, $ord;
}

# Find a good character to use for delimiting q or qq.
sub _find_q {
    my $self = shift;
    local $_ = shift;

    my %counts;
    $counts{$_}++ foreach /([[:punct:]])/g;
    #$counts{$_}++ foreach grep /[[:punct:]]/, split //;
    foreach my $pair ( @{ $self->{qpairs} } ) {
        $counts{$pair}
            = List::Util::sum 0,
                grep defined,
                    map { $counts{$_} }
                        split //, $pair;
    }

    return List::Util::reduce {
        ( ( $counts{$a} //= 0 ) <= ( $counts{$b} //= 0 ) ) ? $a : $b
    } @{ $self->{qpairs} }, @{ $self->{qquotes} };
}
sub _name_and_position {
    my $self = shift;

    my $nest = $self->{nested} // $self->{dereference};
    my $pos  = List::Util::reduce(
        sub { subst( $nest, referent => $a, place => $b ) },
            @{ $self->{_position} //= [] }
    ) // '';

    my $var = $self->{name};
    my $sigil = substr $var, 0, 1;
    if ( $sigil eq '@' || $sigil eq '%' ) {
        if ($pos) {
            $var = sprintf '$%s%s', substr($var, 1), $pos;
        } else {
            $var = subst( $self->{reference}, $var );
        }
    } elsif ($pos) {
        $var = subst(
            $self->{dereference},
            referent => $var,
            place    => $pos
        );
    }

    return $var;
}
sub _cache {
    my $self = shift;
    my $item = shift;
    return $self->scalarify($item) unless ref $item;

    my $refaddr = Scalar::Util::refaddr $item;
    if ( my $cache = $self->{_cache}{$refaddr} ) {
        return $cache;
    }

    $self->{_cache}{$refaddr} = $self->_name_and_position;

    return;
}

1;

__END__

=head1 TODO

=over

=item *

Handle formats better.

=back

=head1 SEE ALSO

L<Data::Dumper>

