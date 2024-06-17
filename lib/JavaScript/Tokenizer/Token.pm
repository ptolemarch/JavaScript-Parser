package JavaScript::Tokenizer::Token;

=head1 NAME

JavaScript::Tokenizer::Token - one JavaScript token

=head1 SYNOPSIS

  use JavaScript::Tokenizer::Token;

  my @lexemes = qw( x = 2 + y );
  my @tokens;

  for my $lexeme (@lexemes) {
          my $token = JavaScript::Tokenizer::Token->new();
          $token->lexeme($lexeme);

          push @tokens, $token;
  }

  print join('', @tokens), "\n";

=head1 DESCRIPTION

Represents a single token in a L<JavaScript::Tokenizer> token stream.
Basically a struct with limited utility apart from
L<JavaScript::Tokenizer>, and only then in its grandchild classes, each
of which represents a single JavaScript token type.

=cut

use 5.006;
use strict;
use warnings;

our $VERSION = '0.01';

use Carp;

use overload
        '""'    => sub { $_[0]->lexeme },
        'bool'  => sub { 1 },
        fallback=> 1;

=head1 METHODS

=head2 CONSTRUCTOR

There is only one constructor:

=over

=item B<< C<< $token = JavaScript::Tokenizer::Token->new(); >> >>

Constructs a new Token, essentially an empty struct.

=back

=head2 ACCESSORS

A Token's properties are get and set in the typical Perlish
way:

        $var->accessor($string);
        $string = $token->accessor;

The following properties can be accessed in this way:

=over

=item B<lexeme>

string represented by the Token

=item B<pos>

character position within the program at which the Token begins

=item B<newlines>

number of newlines in the Token

=back

=cut

use Class::Struct 'JavaScript::Tokenizer::Token' => {
        lexeme  => '$',
        pos     => '$',
        newlines=> '$',
};

=head2 OTHER METHODS

These are sorta like the accessors above, but read-only:

=over

=item B<< C<< $length = $token->length; >> >>

Get the length in characters of the token.  Of course, you could just use

        length $token->lexeme

or even

        length $token

TMTOWTDI.

=cut

sub length
{
        my $NAME = 'length';
        my $self = shift;

        if (@_) {
                carp "$NAME cannot be set"
        }
        return length $self->lexeme;
}

=item B<< C<< $string = $token->terminal; >> >>

Abstract method to represent the token to a JavaScript parser,
L<JavaScript::Parser> for example.  Returns a string that the parser
will use as a terminal symbol.  See L<the next section|/INHERITANCE> for
details.

=cut

sub terminal
{
        my $class = shift;  $class = ref($class) || $class;
        croak "${class}::terminal not defined";
}

=back

=head1 INHERITANCE

JavaScript::Tokenizer::Token defines an entire family of classes, based
mostly on how each class represents itself to a JavaScript parser (see
the C<terminal()> method, above).

(Below, each class is listed as the last part of its name.  In each case,
prepend C<JavaScript::Tokenizer::Token::> to the name.  For example,
C<_AsLexeme> becomes C<JavaScript::Tokenizer::Token::_AsLexeme> and
C<Identifier> becomes C<JavaScript::Tokenizer::Token::Identifier>.)

The children of JavaScript::Tokenizer::Token define three behaviors for
C<terminal()>:

=over

=item *

B<_AsType>

Each of these tokens represents itself to a parser as its type, for example
C<NullLiteral>:

=over

=item *

B<NullLiteral>

=item *

B<BooleanLiteral>

=item *

B<Identifier>

=item *

B<DecimalLiteral>

=item *

B<HexIntegerLiteral>

=item *

B<StringLiteral>

=item *

B<RegularExpressionLiteral>

=back

=item B<_AsLexeme>

Each of these tokens represents itself to a parser as its lexeme, for example
C<< >>>= >>.

=over

=item *

B<ReservedWord>

=item *

B<Keyword>

=item *

B<FutureReservedWord>

=item *

B<Punctuator>

=item *

B<DivPunctuator>

=back

=item *

B<_AsNothing>

These tokens are not to be given to the parser.  The C<terminal()> method
of each of these classes returns B<undef>.

=over

=item *

B<WhiteSpace>

=item *

B<LineTerminator>

=item *

B<MultiLineComment>

=item *

B<SingleLineComment>

=back

=cut

BEGIN {
        my %hierarchy = (
                _AsType => {
                        terminal => sub 
                        {
                                my $class = shift;
                                $class = ref($class) || $class;
                                $class =~ /([^:]+)$/;
                                return $1;
                        },
                        subclasses => [qw(
                                NullLiteral
                                BooleanLiteral
                                Identifier
                                DecimalLiteral
                                HexIntegerLiteral
                                StringLiteral
                                RegularExpressionLiteral
                        )],
                },
                _AsLexeme => {
                        terminal => sub
                        {
                                my $self = shift;
                                return $self->lexeme;
                        },
                        subclasses => [qw(
                                ReservedWord
                                Keyword
                                FutureReservedWord
                                Punctuator
                                DivPunctuator
                        )],
                },
                _AsNothing => {
                        terminal => sub
                        {
                                return undef;
                        },
                        subclasses => [qw(
                                WhiteSpace
                                LineTerminator
                                MultiLineComment
                                SingleLineComment
                        )],
                },
        );
        for my $as_what (keys %hierarchy) {
                no strict 'refs';
                @{__PACKAGE__ . "::${as_what}::ISA"} =
                    qw| JavaScript::Tokenizer::Token |;
                *{__PACKAGE__ . "::${as_what}::terminal"} =
                    $hierarchy{$as_what}{terminal};
                for my $subclass (@{$hierarchy{$as_what}{subclasses}}) {
                        @{__PACKAGE__ . "::${subclass}::ISA"} =
                            (__PACKAGE__ . "::${as_what}");

                }

        }
}

=head1 BUGS

See the CPAN Request Tracker for JavaScript::Parser, at
L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JavaScript-Parser>.

=head1 SEE ALSO

L<JavaScript::Parser>, L<JavaScript::Tokenizer>.

=head1 AUTHOR

David "cogent" Hand, L<mailto:cogent@cpan.org>.

Copyright (c) 2003.  All rights reserved.  This module is free software;
you may restribute and/or modify it under the same terms as Perl itself.

=cut

1;
