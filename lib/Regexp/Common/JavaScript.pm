package Regexp::Common::JavaScript;

=head1 NAME

Regexp::Common::JavaScript - regular expressions matching JavaScript tokens

=head1 SYNOPSIS

  use Regexp::Common qw /JavaScript/;

  my $alert = 'Window.alert("heffalump");';
  my $math = 'Math.sin(3.14159) + Math.log(6.022e23)/Math.log(10);';

  my $string_literal = $RE{JavaScript}{StringLiteral}{-keep};
  my $decimal_literal = $RE{JavaScript}{DecimalLiteral}{-keep};
  
  my ($string)            = $alert =~ m/$string_literal/g;
  my ($pi,$avogadro,$ten) = $math =~ m/$decimal_literal/g;

=head1 DESCRIPTION

Please consult the manual of L<Regexp::Common> for a general description
of the works of this interface.

Do not load this module directly, but load it via L<Regexp::Common>.

This module provides regular expressions that match various JavaScript
token types, as defined in I<ECMA-262>, the
I<ECMAScript Language Specification>.  This module was originally intended
for use by L<JavaScript::Tokenizer>, but has since been made more general.

=cut

use 5.006;
use strict;
use warnings;
no warnings qw| qw |;  # heh!

our $VERSION = '0.01';

use Regexp::Common qw| pattern clean no_defaults |;
use constant OPTION_REGEX => qr/^(-\w+)/;

my ($NAMESPACE) = __PACKAGE__ =~ m/:([^:]+)$/;

=head2 OPTIONS

The expressions in Regexp::Common::JavaScript all accept the
following options:

=over

=item B<-unicode>

Force the regular expression to strictly adhere to the I<ECMA-262>
specification in its requirement for Unicode support.  Note that this
may result in substantially slower regular expressions.

=item B<-head> and B<-tail>

Require that the regular expression come at the very beginning and
ending of the string, respectively.  These options may be used together
or individually.

=item B<-cont>

Permit the regular expression's use in a tokenizer, by including the
C<\G> assertion at the beginning of the regex.  This option should not be
used with either B<-head> or B<-tail>, but Regexp::Common::JavaScript
will neither prevent you from doing so, nor complain if you do.

=item B<-keep>

Save the entire matched string to C<$1>.  In this version, no other
capturing takes place.  Further capturing will be added in a future
version.

=item B<-i>

Perform a case-insensitive match.  This will, of course, cause certain
patterns to match strings that would not ordinarily be considered to be
of the given token type.

=back

=cut

my @OPTIONS = qw(
        -unicode=0
        -head=0
        -tail=0
        -cont=0
);

=head2 TOKENS

Expressions are provided for the token types (and fragments of tokens)
listed below.  The regexen are available as
C<$RE{JavaScript}{TOKEN_TYPE}>, where I<TOKEN_TYPE> is one of the token
(or fragment) names listed.

=over

=item WhiteSpace

Used to separate tokens from each other, and to provide visibility.
Otherwise insignificant.

=item LineTerminator

Like WhiteSpace, separates tokens from each other, and improves
visibility.  Important in automatic semicolon insertion.  Not legal
between certain pairs of tokens.

=item MultiLineComment

C-style comments, capable of spanning multiple lines, and incapable of
nesting.

=item SingleLineComment

C++-style comments, limited to a single line each.

=item ReservedWord

Words that are not permitted as Identifier.  Identical to the union of
the sets Keyword, FutureReservedWord, NullLiteral, and BooleanLiteral.

=item Keyword

Keywords for control structures and the like, and therefore not available
as identifiers.

=item FutureReservedWord

Words reserved as keywords for proposed extentions to the langauge.

=item Identifier

Words permitted for variable and function names, and the like.
B<Note:>  This will also match ReservedWord.

=item IdentifierStart I<(Token Fragment)>

Characters legal as the first character of an Identifier.

=item IdentifierPart I<(Token Fragment)>

Characters legal as subsequent characters in an Identifier.

=item Punctuator

Various non-alphanumerics of the language:  operators, delimiters,
I<et cetera>.  Note that members of DivPunctuator are explicitly not
included among these.

=item DivPunctuator

Punctuators that begin with a slash (C</>).

=item NullLiteral

The literal signifying the null value, namely C<null>.

=item BooleanLiteral

Literals signifying truth and falsehood, namely C<true> and C<false>.

=item DecimalLiteral

Literals signifying integer and floating-point numbers in base-10.

=item HexIntegerLiteral

Literals signifying integers in hexidecimal base-16.

=item StringLiteral

Literals signifying string values.

=item EscapeSequence  I<(Token Fragment)>

Representations of non-printing or otherwise inconvenient characters
in an alternate form.

=item RegularExpressionLiteral

Literals representing JavaScript C<RegExp> (regular expression) objects.

=back

=cut

my %REGEXEN;            # the regexen in their original, pristine state
my %DECOMMENTED;        # the regexen post-pre-processing
my %INTERPOLATED;       # after runtime interpolation

# Notice, if you will, the similarity this bears to the layout of the
#   spec itself.  This was not easy.  The numbers off to the
#   right (7.2, 7.3, etc.) correspond to the section of I<ECMA-262> in
#   which that token is described in detail.
%REGEXEN = (
        WhiteSpace =>                                                   # 7.2
        q(
                [\t\ck\f\_]+
        ),
        WhiteSpace_Unicode =>                                           # 7.2
        q(
                (?:
                        \x{0009}                # \N{HORIZONTAL TABULATION}
                |       \x{000B}                # \N{VERTICAL TABULATION}
                |       \x{000C}                # \N{FORM FEED}
                |       \x{0020}                # \N{SPACE}
                |       \x{00A0}                # \N{NO-BREAK SPACE}
                |       \p{Zs}                  # Other spaces
                )+
        ),
        LineTerminator =>                                               # 7.3
        q(
                \n
        |       \r
        ),
        LineTerminator_Unicode =>                                       # 7.3
        q(
                \x{000A}                        # \N{LINE FEED}
        |       \x{000D}                        # \N{CARRIAGE RETURN}
        |       \x{2028}                        # \N{LINE SEPARATOR}
        |       \x{2029}                        # \N{PARAGRAPH SEPARATOR}
        ),
        MultiLineComment =>                                             # 7.3
        q(
                \/\*
                (?:(?!\*\/)[\s\S])*?
                \*\/
        ),
        SingleLineComment =>                                            # 7.3
        q(
                (?: // | <!-- )  # that second one is non-ECMA but common
                (?: (?!%{LineTerminator}) [\s\S] )*
        ),
        ReservedWord => [                                               # 7.5.1
        qw(
                %{Keyword}
                %{FutureReservedWord}
                %{NullLiteral}
                %{BooleanLiteral}
        )],
        Keyword => [                                                    # 7.5.2
        qw(
                break           else            new             var
                case            finally         return          void
                catch           for             switch          while
                continue        function        this            with
                default         if              throw
                delete          in              try
                do              instanceof      typeof
        )],
        FutureReservedWord => [                                         # 7.5.3
        qw(
                abstract        enum            int             short
                boolean         export          interface       static
                byte            extends         long            super
                char            final           native          synchronized
                class           float           package         throws
                const           goto            private         transient
                debugger        implements      protected       volatile
                double          import          public
        )],
        Identifier =>                                                   # 7.6
        q(
                %{IdentifierStart} (?:%{IdentifierPart})*
        ),
        IdentifierStart =>                                              # 7.6
        q(
                [$_[:alpha:]]
        |       \\\\u[0-9a-fA-F]{4}
        ),
        IdentifierStart_Unicode =>                                      # 7.6
        q(
                [$_]
        |       \p{Lu}                          # Uppercase letter
        |       \p{Ll}                          # Lowercase letter
        |       \p{Lt}                          # Titlecase letter
        |       \p{Lm}                          # Modifier letter
        |       \p{Lo}                          # Other letter
        |       \p{Nl}                          # Letter number
        |       \\\\u[0-9a-fA-F]{4}
        ),
        IdentifierPart =>                                               # 7.6
        q(
                %{IdentifierStart}
        |       \d
        ),
        IdentifierPart_Unicode =>                                       # 7.6
        q(
                %{IdentifierStart}
        |       \p{Mn}                         # Non-spacing mark
        |       \p{Mc}                         # Combining spacing mark
        |       \p{Nd}                         # Decimal number
        |       \p{Pc}                         # Connector punctuation
        ),
        Punctuator => [                                                 # 7.7
        qw#
                {       }       (       )       [       ]
                .       ;       ,       <       >       <=
                >=      ==      !=      ===     !==
                +       -       *       %       ++      --
                <<      >>      >>>     &       |       ^
                !       ~       &&      ||      ?       :
                =       +=      -=      *=      %=      <<=
                >>=     >>>=    &=      |=      ^=
        #],
        DivPunctuator => [                                              # 7.7
        qw#
                /       /=
        #],
        NullLiteral => [                                                # 7.8.1
        qw(
                null
        )],
        BooleanLiteral => [                                             # 7.8.2
        qw(
                true            false
        )],
        DecimalLiteral =>                                               # 7.8.3
        q(
                (?: 0 | [1-9][0-9]* ) \. (?: [0-9]* ) (?: [eE] [+-]? [0-9]+ )?
        |                             \. (?: [0-9]+ ) (?: [eE] [+-]? [0-9]+ )?
        |       (?: 0 | [1-9][0-9]* )                 (?: [eE] [+-]? [0-9]+ )?
        ),
        HexIntegerLiteral =>                                            # 7.8.3
        q(
                0 [xX] [0-9a-fA-F]+
        ),
        StringLiteral =>                                                # 7.8.4
        q(
                "
                (?:
                        [^\n\r"\\\\]
                |       \\\\ %{EscapeSequence}
                )*?
                "
        |       '
                (?:
                        [^\n\r'\\\\]
                |       \\\\ %{EscapeSequence}
                )*?
                '
        ),
        EscapeSequence =>                                               # 7.8.4
        q(
                ['"\\\\bfnrtv]
        |       [^\n\r'"\\\\bfnrtv0-9xu]
        |       0(?![0-9])
        |       x[0-9a-fA-F]{2}
        |       u[0-9a-fA-F]{4}
        ),
        RegularExpressionLiteral =>                                     # 7.8.5
        q(
                # regular_expression_body
                /      
                [^\n\r\*\\\\/]
                (?:  
                        [^\n\r\\\\/]
                |       \\\\ [^\n\r]
                )*?
                /
                # regular_expression_flags
                # note that the spec says that regular_expression_flags is made
                #   up of zero or more of identifier_part.  Both SpiderMonkey
                #   and IE's JS engine seem to require it to be the following.
                #   Since it impacts tokenization, I'm obeying the will of the
                #   crowd rather than that of the spec.
                [A-Za-z]*
        ),
);

for my $name (keys %REGEXEN) {
        # Preprocess
        if (ref $REGEXEN{$name} eq "ARRAY") {
                _array_substitute($REGEXEN{$name});
                $DECOMMENTED{$name} =
                    _list_to_regex(@{$REGEXEN{$name}});
        } else {
                $DECOMMENTED{$name} = $REGEXEN{$name};
                for ($DECOMMENTED{$name}) {
                        s/^([^#]*)#.*$/$1/mg; # remove #comments
                        s/\s+//g;  # remove whitespace
                        s/\\_/ /g; # backslash-underscore is a space (eew!)
                }
        }

        # do everything except make a special pattern for unicode regexen
        next if $name =~ /_Unicode$/;
        pattern (
                name => [$NAMESPACE, $name, @OPTIONS],
                create => \&create,
                match => \&matches,
        );
}

sub create
{
        my ($self, $flags, $name) = @_;

        _flag_default($flags);

        my $requested = $name->[1];
        $requested .= "_Unicode" if $flags->{-unicode};

        unless ($INTERPOLATED{$requested}) {
                $INTERPOLATED{$requested} =
                    $DECOMMENTED{$requested} || $DECOMMENTED{$name->[1]};
                unless (ref $INTERPOLATED{$requested} eq "ARRAY") {
                        # This while loop is my interpretive dance on
                        #   the subject of s///eg.  Thanks to Avi Finkel
                        #   for the idea.
                        while (
                                $INTERPOLATED{$requested}
                                =~ m/^(.*?)%\{(\w+)\}(.*)$/s
                        ) {
                                my $pre = $1;
                                my $this = $2;
                                my $post = $3;
                                $INTERPOLATED{$requested} = 
                                    $pre
                                    . ($flags->{-unicode}
                                       ? $RE{$NAMESPACE}{$this}{-unicode}
                                       : $RE{$NAMESPACE}{$this})
                                    . $post;
                        }
                }
        }

        my $regex = $INTERPOLATED{$requested};

        $regex = "(?k:$regex)"; # to -keep entire match
        $regex = "\\A$regex" if $flags->{-head};
        $regex = "$regex\\z" if $flags->{-tail};
        $regex = "\\G$regex" if $flags->{-cont};

        return $regex;
}

=head2 ALTERNATE MATCHING OF ENUMERATED TOKEN TYPES

Certain token types in the ECMAScript specification are defined by a list
of all possible tokens of that type.  These types are:

=over

=item *

ReservedWord

=item *

Keyword

=item *

FutureReservedWord

=item *

Punctuator

=item *

DivPunctuator

=item *

NullLiteral

=item *

BooleanLiteral

=back

These types get special treatment from L<Regexp::Common>'s
C<< $re->matches() >> interface.  If the B<-head> and B<-tail> flags are
B<both> set, the string being tested is compared (with C<eq>) in turn to
each element of the enumeration.  The string matched is returned, or the
empty string if no match.

As an additional abuse of a mostly worthless feature of Regexp::Common,
if B<-head> and B<-tail> are both set, and this method is called in list
context, then a list is returned, containing each element in the
enumeration.

=cut

sub matches
{
        my $self = shift;
        my $str = shift;

        my $name = $self->{name}[1];

        _flag_default($self->{flags});

        unless (
                ref $REGEXEN{$name} eq "ARRAY"
                and $self->{flags}{-head}
                and $self->{flags}{-tail}
        ) {
                return $str =~ /$self/;
        }

        if (wantarray) {  # and now, we're really starting to abuse the system
                return @{$REGEXEN{$name}};
        } else {
                for (@{$REGEXEN{$name}}) { return $_ if $str eq $_ }
                return "";
        }
}

=head1 STANDARDS COMPLIANCE

=over

=item *

I<ECMA-262>, Section 7, specifies its tokens using a "Lexical Grammar"
with two start symbols, B<InputElementDiv> and B<InputElementRegExp>.
This module does not use a grammar, but regular expressions, to
represent the tokens.  Hopefully, I've correctly converted between these
two means of representing a token.

=item *

The standard requires that B<Identifier> not be a B<ReservedWord>.  In
practice, this means that every Identifier be checked against a list of
B<ReservedWord>s.  This module does not do that check for you, though
perhaps in the future it will, with a flag.

=item *

Nowhere in the standard is the the HTML-style B<SingleLineComment>
delmiter, C<< <!-- >>, listed.  Every implementation of JavaScript that
I have tested has supported this style of B<SingleLineComment>, however,
and so I have included support for it.

=item *

I<ECMA-262>, Section 7.8.5, specifies that the flags at the end of a
regular expression (the C<ms> in S<C</x+/ms>>) be zero or more of
B<IdentifierPart>.  Both SpiderMonkey (Mozilla's JavaScript engine) and
Microsoft Internet Explorer's JavaScript engine seem to require that
B<RegularExpressionFlags> be zero or more of S<C</[A-Za-z]/>>.  I have
opted to follow the herd, for now.

=item *

I<ECMA-262>, Section 7.8.4, seems to indicate that incomplete or
incorrect B<UnicodeEscapeSequence>s and B<HexEscapeSequence>s are not
allowed within a B<StringLiteral>.  Browsers seem not to choke on this
input, however.  Therefore this module will not, either.

=item *

I<ECMA-262>, Sections 6 and 7 are quite explicit that JavaScript source
code is comprised of Unicode B<SourceCharacter>s.  Unfortunately, the
Unicode-aware regular expressions that this requires can take some forty
times as long to run.  This is obviously impractical, and so Unicode
support is not the default.

=back

=head1 BUGS

See the CPAN Request Tracker for JavaScript::Parser, at
L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JavaScript-Parser>.

=head1 SEE ALSO

L<Regexp::Common>.

L<JavaScript::Parser>, L<JavaScript::Tokenizer>, L<perlre>, and
L<perlop/"Regexp Quote-Like Operators">.

I<ECMA-262:  ECMAScript Language Specification>,
L<http://www.ecma.ch/ecma1/STAND/ECMA-262.HTM>

L<http://www.mozilla.org/js/language/>

=head1 AUTHOR

David "cogent" Hand, L<mailto:cogent@cpan.org>.

Copyright (c) 2003.  All rights reserved.  This module is free software;
you may restribute and/or modify it under the same terms as Perl itself.

This module includes code from POE::Preprocessor, by Rocco Caputo.  Used
with permission.  It may be redistributed under the same terms as Perl
itself.

=cut

#=============================================================================
# Helper functions
#-----------------------------------------------------------------------------

# Sing, Muse, of the horrors of Regexp::Common.  Of pattern(name=>'...')
#   and its villainously bad system of flag defauls.  Defaults which
#   apply when the flag is omitted, but not when the flag is specified
#   without a particular value.  Sing, O Muse, of the method by which
#   the Disambiguation occurred:  that the default had to be set to
#   false, and that undef meant true, and black was white and up was
#   down.  And that the Truth of undef would not Be without Heroic
#   Action:
sub _flag_default
{
        my $flags = shift;  # hashref

        for my $option (@OPTIONS) {
                my ($realoption) = $option =~ OPTION_REGEX;
                if(not defined $flags->{$realoption}) {
                        $flags->{$realoption} = 1;
                }
        }
}

sub _array_substitute
{
        my $arrayref = shift;

        for (my $i=0; $i<@{$arrayref}; ++$i) {
                if ($arrayref->[$i] =~ /^%{(\w+)}$/) {
                        my $name = $1;

                        _array_substitute($REGEXEN{$1});
                        splice @{$arrayref}, $i, 1, @{$REGEXEN{$1}};
                }
        }
}

sub _list_to_regex
{
        _text_trie_as_regexp(_text_trie_trie(@_));
}


# Taken from POE::Preprocessor with permission from the author.  I was
# going to use a module, Regex::PreSuf, but `make test` had run for an
# hour and a half, and estimated 12 more hours to go....
#
# POE::Preprocessor is Copyright 2000-2003 by Rocco Caputo.  All
# rights reserved.  POE::Preprocessor is free software; you may
# redistribute it and/or modify it under the same terms as Perl
# itself.

### Start of regexp optimizer.

# _text_trie_trie is virtually identical to code in Ilya Zakharevich's
# Text::Trie::Trie function.  The minor differences involve hardcoding
# the minimum substring length to 1 and sorting the output.

sub _text_trie_trie
{
  my @list = @_;
  return shift if @_ == 1;
  my (@trie, %first);

  foreach (@list) {
    my $c = substr $_, 0, 1;
    if (exists $first{$c}) {
      push @{$first{$c}}, $_;
    }
    else {
      $first{$c} = [ $_ ];
    }
  }

  foreach (sort keys %first) {
    # Find common substring
    my $substr = $first{$_}->[0];
    (push @trie, $substr), next if @{$first{$_}} == 1;
    my $l = length($substr);
    foreach (@{$first{$_}}) {
      $l-- while substr($_, 0, $l) ne substr($substr, 0, $l);
    }
    $substr = substr $substr, 0, $l;

    # Feed the trie.
    @list = map {substr $_, $l} @{$first{$_}};
    push @trie, [$substr, _text_trie_trie(@list)];
  }

  @trie;
}

# This is basically Text::Trie::walkTrie, but it's hardcoded to build
# regular expressions.

sub _text_trie_as_regexp
{
  my @trie   = @_;
  my $num    = 0;
  my $regexp = '';

  foreach (@trie) {
    $regexp .= '|' if $num++;
    if (ref $_ eq 'ARRAY') {
      $regexp .= quotemeta($_->[0]) . '(?:';

      # If the first tail is empty, make the whole group optional.
      my ($tail, $first);
      if (length $_->[1]) {
        $tail  = ')';
        $first = 1;
      }
      else {
        $tail  = ')?';
        $first = 2;
      }

      # Recurse into the group of tails.
      if ($#$_ > 1) {
        $regexp .= _text_trie_as_regexp( @{$_}[$first .. $#$_] );
      }
      $regexp .= $tail;
    }
    else {
      $regexp .= quotemeta($_);
    }
  }

  $regexp;
}

### End of regexp optimizer.

1;
