use v5.14;

package Datify;


use overload ();
use warnings;

use Carp            ();#qw(croak);
use List::Util      ();#qw(reduce sum);
use Scalar::Util    ();#qw(blessed looks_like_number refaddr);
use String::Tools   qw(subst);
use Sub::Name       ();#qw(subname);

my %SETTINGS = (
    # Var options
    name        => '$self',
    assign      => '$var = $value;',
    list        => '($_)',
    beautify    => undef,

    # Undef options
    null => 'undef',

    # Boolean options
    true    => 1,
    false   => "''",

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
        0x00    => '\\0',
        0x07    => '\\a',
        #0x08    => '\\b',   # Does \b mean backspace or word-boundary?
        0x09    => '\\t',
        0x0a    => '\\n',
        0x0c    => '\\f',
        0x0d    => '\\r',
        0x1b    => '\\e',
        0xff    => '\\x%02x',
        0xffff  => '\\x{%04x}',
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

    # LValue options
    lvalue  => 'substr($lvalue, 0)',

    # VString options
    vformat => 'v%vd',
    #vformat => 'v%*vd',
    #vsep    => '.',

    # Number options
    num_sep => '_',

    # Reference options
    reference   => '\\$_',
    dereference => '$referent->$place',
    nested      => '$referent$place',

    # Regexp options
    quote3  => '/',
    #tr3     => q!tr\\/\\/\\!,
    q3      => 'qr',

    # Hash options
    hash_ref    => '{$_}',
    pair        => '$key => $value',
    keysort     => \&Datify::keysort,
    keywords    => [qw(undef)],
    #keyword_set => { 'undef' => 1 },

    # Array options
    array_ref   => '[$_]',
    list_sep    => ', ',

    # Object options
    overloads => [ '""', '0+' ],
    object    => 'bless($data, $class_str)',
    #object    => '$class($data)',
    io        => '*UNKNOWN{IO}',

    # Code options
    code    => 'sub {...}',
);

sub keysort($$) {
    my $n0 = Scalar::Util::looks_like_number($_[0]);
    my $n1 = Scalar::Util::looks_like_number($_[1]);
    if ( $n0 && $n1 ) { return $_[0] <=> $_[1] }
    elsif ( $n0 )     { return -1 }
    elsif ( $n1 )     { return +1 }
    else              { return $_[0] cmp $_[1] }
}

sub add_handler {
    no strict 'refs';
    my $pkg   = shift || __PACKAGE__; $pkg = ref $pkg || $pkg;
    my $name  = _nameify(shift);
    *{ $name } = Sub::Name::subname $name => shift;
}

# Constructor
sub new {
    my $class = shift || __PACKAGE__; $class = ref $class || $class;
    my $self  = { %SETTINGS, @_ };

    return bless $self, $class;
}

# Setter
sub set {
    my $self = my $class = shift;
    if ( ref $self ) {
        # Make a copy
        $self = bless { %$self }, $class = ref $self;
    } else {
        $self = \%SETTINGS;
    }

    my %set = @_;
    delete $self->{keyword_set} if ( $set{keywords} );
    delete $self->{"tr$_"} for grep { exists $set{"quote$_"} } ( 1, 2, 3 );

    %$self = ( %$self, %set );

    return 'HASH' eq ref $self ? $class : $self;
}

# Accessor
sub get {
    my $self = shift; $self = \%SETTINGS unless ref $self;
    my $count = scalar @_;
    if    ( $count == 0 ) { return %$self }
    elsif ( $count == 1 ) { return $self->{ +shift } }
    else                  { return @{$self}{@_} }
}

my $sigils     = '[\@\%\$]';
my $pkg        = '[[:alpha:]]\w*(?:\::\w+)*';
my $word       = '[[:alpha:]_]\w*';
my $digits     = '\d+';
my $cntrl      = '(?:[[:cntrl:]]|\^[[:upper:]])';
my $punct      = '[[:punct:]]';
my $cntrl_word = "\\{$cntrl$word\\}";
my ($name_re)  = map qr/^$_$/, join('',
    "$sigils?",
    "(?:$pkg\::)?",
    '(?:' . join('|', $word, $digits, $cntrl, $punct, $cntrl_word ) . ')'
);
sub varify {
    my $self  = shift; $self = $self->new() unless ref $self;
    my $name  = shift if ( ! ref $_[0] && $_[0] =~ $name_re );
    my $value = 1 == @_ ? shift : \@_;

    if ( ! defined $name ) {
        if ( my $ref = ref $value ) {
            $name = _nameify($ref);
        } else {
            $name = $self->{name};
        }
    }
    Carp::croak "Missing name" unless ( defined $name && length $name );

    my $sigil;
    unless ( ($sigil) = $name =~ /^($sigils)/ ) {
        my $ref = ref $value;
        if    ( $ref eq 'ARRAY' ) { $sigil = '@'; }
        elsif ( $ref eq 'HASH' )  { $sigil = '%'; }
        else                      { $sigil = '$'; }
        $name = $sigil . $name;
    }
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
        return $self->{beautify}->( $value );
    } else {
        return $value;
    }
}

# Scalar: undef
sub undefify {
    my $self = shift; $self = $self->new() unless ref $self;
    return $self->{null};
}

# Scalar: boolean
sub booleanify {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
    return $self->undefify unless defined;
    return $_ ? $self->{true} : $self->{false};
}

# Scalar: single-quoted string
sub stringify1 {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
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

# Scalar: double-quoted string
sub stringify2 {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
    my ( $open, $close ) = $self->_get_delim( shift // $self->{quote2} );

    # quote char(s), dollar-sign, at-sign, and backslash.
    s/([$open$close\x24\x40\x5c])/\\$1/g;
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

# Scalar: string
sub stringify {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;

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

# Scalar: number
# Adapted from Perl FAQ "How can I output my numbers with commas added?"
sub numify {
    my $self = shift; $self = $self->new() unless ref $self;
    return $_[0] unless my $sep = $self->{num_sep};
    local $_ = shift;

    # Fractional portion
            s{^([-+]?\d*\.\d\d)(\d+)}              [${1}$sep${2}];
    1 while s{^([-+]?\d*\.(?:\d+$sep)+\d\d\d)(\d+)}[${1}$sep${2}];

    # Whole portion
    1 while s{^([-+]?\d+)(\d{3})}                  [${1}$sep${2}];

    return $_;
}

# Scalar
sub scalarify {
    my $self = shift; $self = $self->new() unless ref $self;
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

sub lvalueify {
    my $self = shift; $self = $self->new() unless ref $self;
    return subst( $self->{lvalue}, lvalue => $self->stringify(shift) );
}

# Scalar: VString
sub vstringify {
    my $self = shift; $self = $self->new() unless ref $self;
    if ( defined $self->{vsep} ) {
        return sprintf $self->{vformat}, $self->{vsep}, shift;
    } else {
        return sprintf $self->{vformat}, shift;
    }
}

# Regexp
sub regexpify {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
    $self->{tr3} ||= "tr\\$self->{quote3}\\$self->{quote3}\\";
    my $quoter = eval $self->{tr3} // die $@;
    my ( $open, $close )
        = $self->_get_delim( shift // $quoter ? $self->_find_q($_) : $self->{quote3} );

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

# List/Array
sub listify {
    my $self = shift; $self = $self->new() unless ref $self;
    if (1 == @_) {
        my $ref = ref $_[0];
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

sub arrayify {
    my $self = shift; $self = $self->new() unless ref $self;
    return subst( $self->{array_ref}, $self->listify(@_) );
}

# Hash
sub keyify {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;

    return $self->undefify unless defined;
    return $_ if ref;

    $self->{keyword_set} = { map { $_ => 1 } @{ $self->{keywords} } }
        unless $self->{keyword_set};

    if ( Scalar::Util::looks_like_number($_)
        || ( /^-?[[:alpha:]_]\w*$/ && ! $self->{keyword_set}{$_} ) )
    {
        # If the key would be autoquoted by the fat-comma (=>),
        # then there is no need to quote it.

        return "$_"; # Make sure it's stringified.
    }
    return $self->stringify($_);
}

sub pairify {
    my $self = shift; $self = $self->new() unless ref $self;
    if (1 == @_) {
        my $ref = ref $_[0];
        if    ( $ref eq 'ARRAY' ) { @_ = @{ +shift } }
        elsif ( $ref eq 'HASH' )  {
            my $hash = shift;
            $self->{_cache}{ +Scalar::Util::refaddr $hash }
                = $self->_name_and_position;

            my $keysort = $self->{keysort};
            @_ = $keysort
                ? map { $_ => $hash->{$_} } sort $keysort keys %$hash
                : %$hash;
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

sub hashify  {
    my $self = shift; $self = $self->new() unless ref $self;
    return subst( $self->{hash_ref}, $self->pairify(@_) );
}

# Objects
sub overloaded {
    my $self   = shift; $self = $self->new() unless ref $self;
    my $object = shift;

    return unless overload::Overloaded($object);

    foreach my $overload ( @{ $self->{overloads} } ) {
        if ( my $method = overload::Method( $object => $overload ) ) {
            return $method;
        }
    }
    return;
}

sub objectify {
    my $self   = shift; $self = $self->new() unless ref $self;
    my $object = shift;

    return $self->scalarify($object)
        unless my $class = Scalar::Util::blessed $object;

    my $data;
    if ( my $method = $self->overloaded($object) ) {
        $data = $self->scalarify( $object->$method() );
    } else {
        $data = Scalar::Util::reftype $object;

        # Huh?!
        #if ( $data eq '' ) { return $self->scalarify( $object ) }

        if    ( $data eq 'ARRAY' )  { $data = $self->arrayify( [@$object] ) }
        elsif ( $data eq 'CODE' )   { $data = $self->codeify(    $object  ) }
        elsif ( $data eq 'FORMAT' ) { $data = $self->formatify(  $object  ) }
        elsif ( $data eq 'GLOB' )   { $data = $self->refify(     $object  ) }
        elsif ( $data eq 'HASH' )   { $data = $self->hashify(  {%$object} ) }
        elsif ( $data eq 'IO' )     { $data = $self->ioify(      $object  ) }
        elsif ( $data eq 'REF' )    { $data = $self->refify(     $object  ) }
        elsif ( $data eq 'Regexp' ) { $data = $self->regexpify(  $object  ) }
        elsif ( $data eq 'SCALAR' ) { $data = $self->scalarify( $$object  ) }

        else { $data = "*UNKNOWN{$data}" } # ???
    }

    return subst(
        $self->{object},
        class_str => $self->stringify($class),
        class     => $class,
        data      => $data
    );
}

# Objects: IO
sub ioify {
    my $self = shift; $self = $self->new() unless ref $self;
    return $self->{io};
}

# Other
sub codeify   {
    my $self = shift; $self = $self->new() unless ref $self;
    return $self->{code};
}

sub refify    {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
    return subst( $self->{reference}, $self->scalarify($$_) );
}

sub formatify {
    my $self = shift; $self = $self->new() unless ref $self;
    Carp::croak "Unhandled type: ", ref shift;
}

sub globify   {
    my $self = shift; $self = $self->new() unless ref $self;
    local $_ = shift;
    ( my $name = "$_" ) =~ s/^\*main\::/*::/;
    return $name;
}

### Internal ###
### Do not use these methods outside of this package,
### they are subject to change or disappear at any time.
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
    my $o    = ord shift;

    my $e;
    my $encodings = $self->{encode};
    if ( exists $encodings->{$o} ) {
        $e = $encodings->{$o};
    } elsif ( $o <= 255 ) {
        $e = $encodings->{255};
    } else {
        $e = $encodings->{65535} // $encodings->{255};
    }

    return sprintf $e, $o;
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
        if ( $pos ) {
            $var = sprintf '$%s%s', substr($var, 1), $pos;
        } else {
            $var = subst( $self->{reference}, $var );
        }
    }
    else {
        $var = subst(
            $self->{dereference},
            referent => $var,
            place    => $pos
        ) if $pos;
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

__DATA__

=head1 NAME

Datify - Simple stringification of data.

=head1 SYNOPSIS

 use Datify;

 my $datify = Datify->new( ... );   # See Options below
 $datify = $datify->set( ... );     # See Options below

 print $datify->varify( data => [...] ), "\n";  # "@data = (...);\n"

 # Or

 print Datify->varify( data => [...] ), "\n";
 # "@data = (...);\n"

=head1 DESCRIPTION

C<Datify> is very similar to L<Data::Dumper>, except that it's
easier to use, has better formatting and options, and is invented here.

=head2 Options

The following options can be set as part of C<new> or with C<set>.
The default values are listed below.

=over

=item Varify options

=over

=item I<name>       => B<'$self'>

The neame of the default variable.
This is also set as the first parameter to C<varify>.

=item I<assign>     => B<'$var = $value;'>

What an assignment statement should look like.  If the generated code
is to be run under C<use strict;>, then you may want to change this to
C<'my $var = $value;'>.

=item I<list>       => B<'($_)'>

The delimiters for a list.

=item I<beautify>   => B<undef>

Set this to a sub reference that you would like to use to beautify the code.
It should accept the code as the first parameter, process it, and return
the code after all the beauty modifications have been completed.

An example:

 use Datify;
 use Perl::Tidy;

 sub beautify {
     my $source = shift;

     Perl::Tidy::perltidy(
         source      => \$source,
         destination => \my $dest,
         stderr      => \my $stderr,
         errorfile   => \$stderr,
     ) && die $stderr;

     return $dest;
 }

 Datify->set( beautify => \&beautify );

 say Datify->varify( var => $var );

=back

=item Undefify options

=over

=item I<null> => B<'undef'>

What to use as the null value.

=back

=item Booleanify options

=over

=item I<true>    => B<1>

=item I<false>   => B<"''">

What to use as the values for C<true> and C<false>, respectively.  Since Perl
does not have native boolean values, these are placeholders.

=back

=item Stringify options

=over

=item I<quote>   => B<undef>

What to use as the default quote character.
If set to a false value, then use the best guess.
See L<stringify> below.

=item I<quote1>  => B<"'">

The default single-quoting character.

=item I<quote2>  => B<'"'>

The default double-quoting character.

=item I<q1>      => B<'q'>

The special single-quoting character starter.

=item I<q2>      => B<'qq'>

The special double-quoting character starter.

=item I<sigils>  => B<'$@'>

TODO

=item I<longstr> => B<1_000>

How long a string needs to be before it's considered long.
See L<stringify> below.
Change to a false value to mean no string is long.
Change to a negative value to mean every string is long.

=item I<encode>  => B<<
{
0 => '\0',
7 => '\a',
9 => '\t',
10 => '\n',
12 => '\f',
13 => '\r',
27 => '\e',
255 => '\x%02x',
65535 => '\x{%04x}'
}
>>

How to encode characters that need encoding.

=item I<qpairs>  => B<< [ qw\ () <> [] {} \ ] >>

=item I<qquotes> => B<[ qw\ ! # % & * + , - . / : ; = ? ^ | ~ $ @ ` \ ]>

When determining the quote character to use, go through these lists to see
which character would work best.

=back

=item LValueify options

=over

=item I<lvalue>  => B<'substr($lvalue, 0)'>

How to generate a LValue.

=back

=item VStringify options

=over

=item I<vformat> => B<'v%vd'>

=item I<vsep>    => B<undef>

The formatting string to use.  If I<vsep> is set, then I<vformat> should use
the C<*> format to inidicate what I<vsep> will be:
C<< vformat => 'v%*vd', vsep => '.' >>.

=back

=item Numify options

=over

=item I<num_sep> => B<'_'>

What character to use to seperate sets of numbers.

=back

=item Refify options

=over

=item I<reference>   => B<'\\$_'>

=item I<dereference> => B<< '$referent->$place' >>

=item I<nested>      => B<'$referent$place'>

=back

=item Regexpify options

=over

=item I<quote3>  => B<'/'>

=item I<q3>      => B<'qr'>

=back

=item Hashify options

=over

=item I<hash_ref>    => B<'{$_}'>

=item I<pair>        => B<< '$key => $value' >>

=item I<keysort>     => B<\&Datify::keysort>

=item I<keywords>    => B<[qw(undef)]>

=back

=item Arrayify options

=over

=item I<array_ref>   => B<'[$_]'>

=item I<list_sep>    => B<', '>

=back

=item Objectify options

=over

=item I<overloads>  => B<[ '""', '0+' ]>

The list of overloads to check for before deconstructing the object.
See L<overload> for more information on overloading.

=item I<object>     => B<'bless($data, $class_str)'>

=item I<io>         => B<'*UNKNOWN{IO}'>

=back

=item Codeify options

=over

=item I<code>    => B<'sub {...}'>

=back

=back

=head2 Methods

=over

=item C<< new( name => value, name => value, ... ) >>

Create a C<Datify> object with the following options.

=item C<< set( name => value, name => value, ... ) >>

Change the L<Options> settings.
When called as a class method, changes default options.
When called as an object method, changes the settings and returns a
new object.

B<NOTE:> When called as a method on an object, this returns a new instance
with the values set, so you will need to capture the return if you'd like to
persist the change:

 $datify = $datify->set( ... );

=item C<get( name, name, ... )>

Get one or more existing values for one or more settings.
If passed no names, returns all values.

=item C<< varify( name => value, value, ... ) >>

Returns an assignment statement for the values.  If C<name> does not begin
with a sigil (C<$>, C<@>, or C<%>), will determine which sigil to use based
on C<values>.

Some examples:

Common case, determine the type and add the correct sigil to 'foo'.

 Datify->varify(   foo  => $foo )

Specify the type.

 Datify->varify( '$foo' => $foo )

Handle a list: C<@foo = (1, 2, 3);>

 Datify->varify( '@foo' =>   1, 2, 3   )
 Datify->varify( '@foo' => [ 1, 2, 3 ] )
 Datify->varify(   foo  =>   1, 2, 3   )
 Datify->varify(   foo  => [ 1, 2, 3 ] )

Handle a hash: C<< %foo = (a => 1, b => 2, c => 3); >>
(B<Note>: Order may be rearranged.)

 Datify->varify( '%foo' =>   a => 1, b => 2, c => 3   )
 Datify->varify( '%foo' => { a => 1, b => 2, c => 3 } )
 Datify->varify(   foo  => { a => 1, b => 2, c => 3 } )

Keep in mind that without proper hints, this would be interpretted as a list,
not a hash:

 Datify->varify(   foo  =>   a => 1, b => 2, c => 3   )
 # "@foo = ('a', 1, 'b', 2, 'c', 3);"

=item C<undefify>

Returns the string that should be used for an undef value.

=item C<booleanify( value )>

Returns the string that represents the C<true> or C<false> interpretation
of value.

=item C<stringify1( value, delimiters )>

Returns the string that represents value as a single-quoted string.
The delimiters parameter is optional.

=item C<stringify2( value, delimiters )>

Returns the string that represents value as a double-quoted string.
The delimiters parameter is optional.

=item C<stringify( value )>

Returns the string the represents value.  It will be a double-quoted string
if it is longer than the C<longstr> option or contains control characters.
It will be a single-quoted string unless there are single-quotes within the
string, then it will be a double-quoted string, unless it also contains
double-quotes within the string, then it will attempt to find the best quote
character.

=item C<numify( value )>

Returns value with seperators between the hundreds and thousands,
hundred-thousands and millions, etc.  Similarly for the fractional parts.

 Datify->numify(1234.5678901) # "1_234.56_789_01"

=item C<scalarify( value )>

Returns value as a scalar.  If value is not a reference, performs some magic
to correctly print vstrings and numbers, otherwise assumes it's a string.
If value is a reference, hands off to the correct function to create
the string.

Handles reference loops.

=item C<lvalueify( value )>

=item C<vstringify( value )>

=item C<regexpify( value, delimiters )>

=item C<listify( value, value, ... )>

Returns value(s) as a list.

 Datify->listify( 1, 2, 3 ) # '1, 2, 3'

=item C<arrayify( value, value, ... )>

Returns value(s) as an array.

 Datify->arrayify( 1, 2, 3 ) # '[1, 2, 3]'

=item C<keyify( value )>

Returns value as a key.  If value does not need to be quoted, it will not be.
Verifies that value is not a keyword.

=item C<pairify( value, value, ... )>

Returns value(s) as a pair.

 Datify->pairify( a => 1, b => 2 ) # 'a => 1, b => 2'

=item C<hashify( value, value, ... )>

Returns value(s) as a hash.

 Datify->hashify( a => 1, b => 2 ) # '{a => 1, b => 2}'

=item C<objectify( value )>

Returns value as an object.

 Datify->objectify( $object ) # "bless({}, 'Object')"

=item C<codeify( value )>

Returns a subroutine definition that is not likely to encode value.

 Datify->codeify( \&subroutine ) # 'sub {...}'

=item C<refify( value )>

Returns value as reference.

=item C<formatify( value )>

TODO

=item C<globify( value )>

TODO

=back

=head1 BUGS

No known bugs.

=head1 TODO

=over

=item *

Handle formats.

=item *

Handle globs

=back

=head1 AUTHOR

Bob Kleemann

=head1 SEE ALSO

L<Data::Dumper>

=cut

