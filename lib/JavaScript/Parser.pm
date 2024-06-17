package JavaScript::Parser;

use 5.006;
use strict;
use warnings;

our $VERSION = '0.01';

use Carp;
use JavaScript::Grammar;
use JavaScript::Tokenizer;

sub new { goto &from_string; }
sub from_string
{
        my $class = shift;  $class = ref($class) || $class;

        my $strref = shift;
        my $tokenizer = JavaScript::Tokenizer->from_string($strref);

        my $self = bless {
                tokenizer       => $tokenizer,
                tokens          => [],  # other tokens
        }, $class;
}

sub _init_grammar
{
        my $self = shift;
        my $tokenizer = $self->{tokenizer};

        my $ied = sub { $tokenizer->state("InputElementDiv"); };
        my $iere = sub { $tokenizer->state("InputElementRegExp"); };
        my $lexer = sub {
                my $token;

                if (@{$self->{tokens}}) {
                        $token = pop $self->{tokens};
                } else {
                        $token = $tokenizer->token;
                }

                if (not defined $token) {
                        # this particular return value is what Parse::Yapp wants
                        #   for end-of-input
                        return '', undef;
                } elsif (defined $token->terminal) {
                        # token that should be sent to the parser
                        return $token->terminal, $token;
                } else {
                        # whitespace or something else in which the parser isn't
                        #   interested
                        return $self->_lexer;
                }
        };
        my $ison = sub {
                my $self = shift;

                my $tokenizer = $self->{tokenizer}->token;
                my $state = $tokenizer->state;
                $tokenizer->state("InputElementRegExp");  # in every 

                $tokenizer->state($state);
        };
}

return 1;
