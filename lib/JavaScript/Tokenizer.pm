package JavaScript::Tokenizer;

=head1 NAME

JavaScript::Tokenizer - JavaScript token stream

=head1 SYNOPSIS

  use JavaScript::Tokenizer;

  my $code = "for (x = 3; x < 99; x++) { window.alert(x) };";

  my $tokens = JavaScript::Tokenizer->from_string($code);
  my @tokens;

  while (defined(my $token = $tokens->token)) {
        push @tokens, $token;
  }

  print join('', @tokens), "\n";

=head1 DESCRIPTION

Separates JavaScript code into its component tokens.

B<Warning:>  This module needs to be controlled by full parser in order
to correctly tokenize JavaScript code in the general case.  See caveats
in the documentation for the C<state> method, below.

=cut

use 5.006;
use strict;
use warnings;

our $VERSION = '0.01';

use Carp;
use Regexp::Common qw( JavaScript clean no_defaults );
use JavaScript::Tokenizer::Token;

use constant STATES => {
        # Order of tokens matters within each state.  The regexen don't
        #   always prevent matching tokens other than their own type.
        InputElementDiv => [qw(
                WhiteSpace
                LineTerminator
                MultiLineComment
                SingleLineComment
                DecimalLiteral
                HexIntegerLiteral
                Punctuator
                DivPunctuator
                Identifier
                StringLiteral
        )],
        InputElementRegExp => [qw(
                WhiteSpace
                LineTerminator
                MultiLineComment
                SingleLineComment
                DecimalLiteral
                HexIntegerLiteral
                Punctuator
                RegularExpressionLiteral
                Identifier
                StringLiteral
        )],
};
use constant NOT_IDENTIFIERS => [qw(
        NullLiteral
        BooleanLiteral
        Keyword
        FutureReservedWord
)];

=head1 METHODS

=head2 CONSTRUCTOR

=over

=item B<< C<< $tokens = JavaScript::Tokenizer->new($code) >> >>

=item B<< C<< $tokens = JavaScript::Tokenizer->from_string($code) >> >>

Create a new Tokenizer, and begin tokenizing at the beginning of
I<C<$code>>.  A reference to I<C<$code>> is taken, so beware of modifying
I<C<$code>> while tokenization is still in progress.

=back

=cut

sub new { goto &from_string; }
sub from_string
{
        my $class = shift;  $class = ref($class) || $class;

        my $strref;
        if (ref $_[0] eq "SCALAR") {
                $strref = $_[0];
        } else {
                $strref = \$_[0];
        }

        my $self = bless { strref => $strref }, $class;
        $self->{state}          = 'InputElementDiv';
        $self->{strpos}         = 0;

        $self->unicode(0);

        $self->notoken(\&_default_notoken);

        return $self;
}

=head2 TOKENIZATION

=over

=item B<< C<< $token = $tokens->token; >> >>

Get the next token, respecting the current state.  Returns an instance of
L<JavaScript::Tokenizer::Token>.

=cut

sub token
{
        my $self = shift;
        my $strref = $self->{strref};

        # end of input
        return undef if $self->{strpos} == length($$strref);

        my ($lexeme, $type, $token);
        for (@{STATES->{$self->{state}}}) {
                my $regex = $self->{regexen}{$_};

                pos($$strref) = $self->{strpos};
                if ($$strref =~ m/$regex/g) {
                        $lexeme = $1;
                        $type = $_;
                        last;
                }
        }

        unless (defined $lexeme) {
                $self->{'notoken'}->();

                return undef;
        }

        $self->{strpos} = pos $$strref;

        my $newlines = 0;
        if ($type eq "Identifier" && $lexeme =~ /\w+/) {
                # all ReservedWords are first recognized as Identifiers
                for my $not (@{&NOT_IDENTIFIERS}) {
                        for (@{$self->{not_identifiers}{$not}}) {
                                $type = $not, last if $lexeme eq $_;
                        }
                }
        } elsif ($type eq "MultiLineComment") {
                # one of two possible token types to span a line
                my $nlre = $self->{regexen}{LineTerminator};
                $newlines = (() = $lexeme =~ m/$nlre/g);
        } elsif ($type eq "LineTerminator") {
                $newlines = length $lexeme;
        }

        # create a JavaScript::Tokenizer::Token
        $token = "JavaScript::Tokenizer::Token::$type"->new;
        $token->lexeme($lexeme);
        $token->pos($self->{strpos});
        $token->newlines($newlines);

        return $token;
}

=back

=head2 ACCESSORS

=over

=item B<< C<< $tokens->state($state); >> >>

=item B<< C<< $state = $tokens->state; >> >>

Set or get the Tokenizer's state.  The valid values of I<$state> are:

=over

=item *

C<"InputElementDiv"> I<(starting state)>

=item *

C<"InputElementRegExp">

=back

JavaScript tokenization happens in two
distinct states, B<InputElementDiv> and B<InputElementRegExp>, corresponding
respectively to positions in the code where B<DivPunctuator> is a valid
token, and positions where B<RegularExpresisonLiteral> is valid.  Resolving
this ambiguity requires a full JavaScript L<parser|JavaScript::Parser>.

This means that, for any code which contains both division operators
(B<DivPunctuator>, C</> and C</=>) and regular expression literals
(B<RegularExpressionLiteral>, C</cogent/i> for example), you will B<not>
get correct tokenization using JavaScript::Tokenizer alone.

By default JavaScript::Tokenizer looks for an B<InputElementDiv>.

=cut

sub state
{
        my $self = shift;
        if (@_) {
                my $state = shift;
                if (STATES->{$state}) {
                        $self->{'state'} = $state;
                } else {
                        carp "invalid token state $state";
                }
        }
        return $self->{'state'}
}

=item B<< C<< $tokens->unicode($unicodeyness); >> >>

=item B<< C<< $unicodeyness = $tokens->unicode; >> >>

If I<$unicodeyness> is true, this tokenizer will require that all of its
regular expressions adhere strictly to the I<ECMA-262> standard's
requirements for Unicode support.  This will very likely slow tokenization by
quite a lot.

=cut

sub unicode
{
        my $self = shift;

        if (@_) {
                my $unicode = shift;

                $self->{unicode} = $unicode;
                $self->_init_regexen
                    unless defined $self->{unicode}
                    and $self->{unicode}!=$unicode;
        }

        return $self->{unicode};
}

sub _init_regexen
{
        my $self = shift;
        my $unicode = $self->{unicode};

        $self->{regexen} = {};

        for my $state (keys %{&STATES}) {
                for my $type (@{STATES->{$state}}) {
                        my $regex = $RE{JavaScript}{$type}
                            {-keep}{-cont}
                            {"-unicode=$unicode"};

                        $self->{regexen}{$type} = qr/$regex/;
                }
        }

        for my $not (@{&NOT_IDENTIFIERS}) {
                $self->{not_identifiers}{$not} = [
                    $RE{JavaScript}{$not}
                    {-head}{-tail}
                    {"-unicode=$unicode"}->matches()
                ];
        }
}

=item B<< C<< $pos = $tokens->pos; >> >>

Get the position within the code at which the tokenizer will try to find
the next token.  

=cut

sub pos
{
        my $self = shift;

        return $self->{'strpos'}
}

=item B<< C<< $tokens->notoken(sub { ... }); >> >>

Give the Tokenizer an error-handling subroutine to call when there is
still input left, but no more valid tokens.  The subroutine given is
called with no arguments.

=cut

sub notoken
{
        my $self = shift;

        if (@_) {
                my $ref = shift;
                if (ref($ref) eq "CODE") {
                        $self->{'notoken'} = $ref;
                } else {
                        carp "not a CODE ref";
                }
        }
        return;
}

sub _default_notoken
{
        carp "no valid token before end of input";
}

=back

=head1 STANDARDS COMPLIANCE

=over

=item *

I<ECMA-262>, Section 7, specifies tokenization using a "Lexical Grammar"
with two start symbols, B<InputElementDiv> and B<InputElementRegExp>.
This module does not use a grammar, but two ordered lists of regular
expressions, to match tokens.

=item *

I<ECMA-262>, Section 7, specifies that each time a token is to be
matched, the token type matching "the longest possible sequence of
characters" is to be chosen.  This module does not attempt to match each
token type, choosing the longest match.  Instead, each token is tried
in a particular order (so as to prevent erroneous matches), and the
first successful match is used.  B<Identifier> is handled as a special
case and changed to the appropriate B<ReservedWord> type if necessary.

=back

=head1 BUGS

See the CPAN Request Tracker for JavaScript::Parser, at
L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JavaScript-Parser>.

=head1 SEE ALSO

L<JavaScript::Parser>, L<JavaScript::Tokenizer::Token>,
L<Regexp::Common::JavaScript>.

I<ECMA-262>:  I<ECMAScript Language Specification>,
L<http://www.ecma.ch/ecma1/STAND/ECMA-262.HTM>

L<http://www.mozilla.org/js/language/>

=head1 AUTHOR

David "cogent" Hand, L<mailto:cogent@cpan.org>.

Copyright (c) 2003.  All rights reserved.  This module is free software;
you may restribute and/or modify it under the same terms as Perl itself.

=cut

1;
