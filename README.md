Datify
======

Simple stingification of Perl data

NAME
    Datify - Simple stringification of data.

SYNOPSIS
     use Datify;

     my $datify = Datify->new( ... );   # See Options below
     $datify = $datify->set( ... );     # See Options below

     print $datify->varify( data => [...] ), "\n";  # "@data = (...);\n"

     # Or

     print Datify->varify( data => [...] ), "\n";
     # "@data = (...);\n"

DESCRIPTION
    "Datify" is very similar to Data::Dumper, except that it's easier to
    use, has better formatting and options, and is invented here.

  Options
    The following options can be set as part of "new" or with "set". The
    default values are listed below.

    Varify options

        *name* => '$self'
            The neame of the default variable. This is also set as the first
            parameter to "varify".

        *assign* => '$var = $value;'
            What an assignment statement should look like. If the generated
            code is to be run under "use strict;", then you may want to
            change this to 'my $var = $value;'.

        *list* => '($_)'
            The delimiters for a list.

        *beautify* => undef
            Set this to a sub reference that you would like to use to
            beautify the code. It should accept the code as the first
            parameter, process it, and return the code after all the beauty
            modifications have been completed.

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

    Undefify options

        *null* => 'undef'
            What to use as the null value.

    Booleanify options

        *true* => 1
        *false* => "''"
            What to use as the values for "true" and "false", respectively.
            Since Perl does not have native boolean values, these are
            placeholders.

    Stringify options

        *quote* => undef
            What to use as the default quote character. If set to a false
            value, then use the best guess. See stringify below.

        *quote1* => "'"
            The default single-quoting character.

        *quote2* => '"'
            The default double-quoting character.

        *q1* => 'q'
            The special single-quoting character starter.

        *q2* => 'qq'
            The special double-quoting character starter.

        *sigils* => '$@'
            TODO

        *longstr* => 1_000
            How long a string needs to be before it's considered long. See
            stringify below. Change to a false value to mean no string is
            long. Change to a negative value to mean every string is long.

        *encode* => { 0 => '\0', 7 => '\a', 9 => '\t', 10 => '\n', 12 =>
        '\f', 13 => '\r', 27 => '\e', 255 => '\x%02x', 65535 => '\x{%04x}' }
            How to encode characters that need encoding.

        *qpairs* => [ qw\ () <> [] {} \ ]
        *qquotes* => [ qw\ ! # % & * + , - . / : ; = ? ^ | ~ $ @ ` \ ]
            When determining the quote character to use, go through these
            lists to see which character would work best.

    LValueify options

        *lvalue* => 'substr($lvalue, 0)'
            How to generate a LValue.

    VStringify options

        *vformat* => 'v%vd'
        *vsep* => undef
            The formatting string to use. If *vsep* is set, then *vformat*
            should use the "*" format to inidicate what *vsep* will be:
            "vformat => 'v%*vd', vsep => '.'".

    Numify options

        *num_sep* => '_'
            What character to use to seperate sets of numbers.

    Refify options

        *reference* => '\\$_'
        *dereference* => '$referent->$place'
        *nested* => '$referent$place'

    Regexpify options

        *quote3* => '/'
        *q3* => 'qr'

    Hashify options

        *hash_ref* => '{$_}'
        *pair* => '$key => $value'
        *keysort* => \&Datify::keysort
        *keywords* => [qw(undef)]

    Arrayify options

        *array_ref* => '[$_]'
        *list_sep* => ', '

    Objectify options

        *object* => 'bless($data, $class_str)'
        *io* => '*UNKNOWN{IO}'

    Codeify options

        *code* => 'sub {...}'

  Methods
    "new( name => value, name => value, ... )"
        Create a "Datify" object with the following options.

    "set( name => value, name => value, ... )"
        Change the Options settings. When called as a class method, changes
        default options. When called as an object method, changes the
        settings and returns a new object.

        NOTE: When called as a method on an object, this returns a new
        instance with the values set, so you will need to capture the return
        if you'd like to persist the change:

         $datify = $datify->set( ... );

    "get( name, name, ... )"
        Get one or more existing values for one or more settings. If passed
        no names, returns all values.

    "varify( name => value, value, ... )"
        Returns an assignment statement for the values. If "name" does not
        begin with a sigil ("$", "@", or "%"), will determine which sigil to
        use based on "values".

        Some examples:

        Common case, determine the type and add the correct sigil to 'foo'.

         Datify->varify(   foo  => $foo )

        Specify the type.

         Datify->varify( '$foo' => $foo )

        Handle a list: "@foo = (1, 2, 3);"

         Datify->varify( '@foo' =>   1, 2, 3   )
         Datify->varify( '@foo' => [ 1, 2, 3 ] )
         Datify->varify(   foo  =>   1, 2, 3   )
         Datify->varify(   foo  => [ 1, 2, 3 ] )

        Handle a hash: "%foo = (a => 1, b => 2, c => 3);" (Note: Order may
        be rearranged.)

         Datify->varify( '%foo' =>   a => 1, b => 2, c => 3   )
         Datify->varify( '%foo' => { a => 1, b => 2, c => 3 } )
         Datify->varify(   foo  => { a => 1, b => 2, c => 3 } )

        Keep in mind that without proper hints, this would be interpretted
        as a list, not a hash:

         Datify->varify(   foo  =>   a => 1, b => 2, c => 3   )
         # "@foo = ('a', 1, 'b', 2, 'c', 3);"

    "undefify"
        Returns the string that should be used for an undef value.

    "booleanify( value )"
        Returns the string that represents the "true" or "false"
        interpretation of value.

    "stringify1( value, delimiters )"
        Returns the string that represents value as a single-quoted string.
        The delimiters parameter is optional.

    "stringify2( value, delimiters )"
        Returns the string that represents value as a double-quoted string.
        The delimiters parameter is optional.

    "stringify( value )"
        Returns the string the represents value. It will be a double-quoted
        string if it is longer than the "longstr" option or contains control
        characters. It will be a single-quoted string unless there are
        single-quotes within the string, then it will be a double-quoted
        string, unless it also contains double-quotes within the string,
        then it will attempt to find the best quote character.

    "numify( value )"
        Returns value with seperators between the hundreds and thousands,
        hundred-thousands and millions, etc. Similarly for the fractional
        parts.

         Datify->numify(1234.5678901) # "1_234.56_789_01"

    "scalarify( value )"
        Returns value as a scalar. If value is not a reference, performs
        some magic to correctly print vstrings and numbers, otherwise
        assumes it's a string. If value is a reference, hands off to the
        correct function to create the string.

        Handles reference loops.

    "lvalueify( value )"
    "vstringify( value )"
    "regexpify( value, delimiters )"
    "listify( value, value, ... )"
        Returns value(s) as a list.

         Datify->listify( 1, 2, 3 ) # '1, 2, 3'

    "arrayify( value, value, ... )"
        Returns value(s) as an array.

         Datify->arrayify( 1, 2, 3 ) # '[1, 2, 3]'

    "keyify( value )"
        Returns value as a key. If value does not need to be quoted, it will
        not be. Verifies that value is not a keyword.

    "pairify( value, value, ... )"
        Returns value(s) as a pair.

         Datify->pairify( a => 1, b => 2 ) # 'a => 1, b => 2'

    "hashify( value, value, ... )"
        Returns value(s) as a hash.

         Datify->hashify( a => 1, b => 2 ) # '{a => 1, b => 2}'

    "objectify( value )"
        Returns value as an object.

         Datify->objectify( $object ) # "bless({}, 'Object')"

    "codeify( value )"
        Returns a subroutine definition that is not likely to encode value.

         Datify->codeify( \&subroutine ) # 'sub {...}'

    "refify( value )"
        Returns value as reference.

    "formatify( value )"
        TODO

    "globify( value )"
        TODO

BUGS
    No known bugs.

TODO
    *   Handle formats.

    *   Handle globs

AUTHOR
    Bob Kleemann

SEE ALSO
    Data::Dumper

