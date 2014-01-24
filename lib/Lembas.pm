package Lembas;

use strict;
use warnings;
use 5.010;
use Carp;

use Test::Builder;
use Params::Validate qw/:types validate/;
use IPC::Run qw/start/;
use List::Util qw/sum/;
use Text::ParseWords;

use Moo::Lax;

our $VERSION = 0.001;

has 'shell' => (is => 'ro',
                required => 1,
                isa => sub { ref $_[0]
                                 and ref $_[0] eq 'ARRAY'
                                 or die 'shell parameter must be an arrayref' },
                coerce => sub { ref $_[0] ? $_[0] : [ $_[0] ] });

has 'debug' => (is => 'ro',
                default => sub { 0 });

has 'input' => (is => 'ro',
                init_arg => undef,
                default => sub { my $anonymous = '';
                                 \$anonymous });
has 'output' => (is => 'ro',
                 init_arg => undef,
                 default => sub { my $anonymous = '';
                                  \$anonymous });
has 'errput' => (is => 'ro',
                 init_arg => undef,
                 default => sub { my $anonymous = '';
                                  \$anonymous });

has 'subprocess' => (is => 'ro',
                     lazy => 1,
                     builder => '_build_subprocess');

has 'commands' => (is => 'ro',
                   required => 1,
                   isa => sub { ref $_[0]
                                    and ref $_[0] eq 'ARRAY'
                                    or die 'shell parameter must be an arrayref' });

has 'test_builder' => (is => 'ro',
                       default => sub { Test::Builder->new },
                       handles => [ qw/is_passing/ ]);

has 'plan_size' => (is => 'ro');

has '_ansi_escape' => (is => 'ro',
                       default => sub { qr/\x{1B}\[([0-9]{1,3}((;[0-9]{1,3})*)?)?[m|K]/ });

sub _build_subprocess {

    my $self = shift;
    my $subprocess = start($self->shell,
                           '<', $self->input,
                           '1>', $self->output,
                           '2>', $self->errput)
        or croak(sprintf(q{Could not start subprocess with '%s': return code %d},
                         join(' ', @{$self->shell}), $?));

    return $subprocess;

}

sub ansi_escape {

    my ($self, $string) = @_;
    my $ansi_escape = $self->_ansi_escape;
    $string =~ s/$ansi_escape//g;
    return $string;

}

sub new_from_test_spec {

    my $class = shift;
    my %params = validate(@_,
                          { shell => { type => SCALAR | ARRAYREF, optional => 1 },
                            handle => { isa => 'IO::Handle' },
                            debug => { type => SCALAR, default => 0 }});

    my ($shell, $commands, $plan_size) = $class->_parse_test_spec($params{handle});

    # shell given from params takes precedence on shell guessed from
    # shebang
    return $class->new(shell => $params{shell} || $shell,
                       commands => $commands,
                       debug => $params{debug},
                       plan_size => $plan_size);

}

sub _pump_one_line {

    my $self = shift;

    my %params = validate(@_,
                          { blocking => { type => SCALAR, default => 1 } });

    my $old_output = ${$self->output};

    while (${$self->output} !~ /\n/) {

        if (${$self->errput}) {

            $self->test_builder->diag('STDERR: '.${$self->errput});
            ${$self->errput} = '';

        }

        if ($params{blocking}) {

            $self->subprocess->pump;

        } else {

            $self->subprocess->pump_nb;

            if ($old_output eq ${$self->output}) {

                # give up, nothing seems to be in the pipe
                last;

            }

        }

    }

    return 1;

}

sub run {

    my $self = shift;

    foreach my $command (@{$self->commands}) {

        if (not defined $command->{shell}) {

            # called command "preamble", need to start matching output
            # *before* sending input

            $self->test_builder->note('Matching preamble output...');

        } else {

            $self->test_builder->note($command->{shell});
            ${$self->input} .= $command->{shell} . "\n";

        }

        my $fastforwarding = 0;
        my $fastforwarding_buffer;
        my $fastforwarding_sink;
        my $fastforwarding_output_control = { current_output => $self->test_builder->output,
                                              current_failure_output => $self->test_builder->failure_output,
                                              current_todo_output => $self->test_builder->todo_output };

        while (my $expected_output = shift @{$command->{outputs}}) {

            if (exists $expected_output->{command}) {

                my @parameters = @{$expected_output->{parameters}};

                if ($expected_output->{command} eq 'fastforward') {
                    # handle params someday, for now assume "some"
                    $self->test_builder->note('Fastforwarding...');
                    $fastforwarding = 1;
                    $self->test_builder->output(\$fastforwarding_buffer);
                    $self->test_builder->failure_output(\$fastforwarding_sink);
                    $self->test_builder->todo_output(\$fastforwarding_buffer);
                } elsif ($expected_output->{command} eq 'wait_less_than') {
                    alarm $parameters[0];
                } else {
                    croak(sprintf(q{unknown command '%s'},
                                  $expected_output->{command}));
                }

                next;

            }

            $self->test_builder->note(sprintf(q{Waiting for a %s match of %s%s},
                                              $expected_output->{match_type},
                                              $expected_output->{output},
                                              $fastforwarding ? ' (fastforward mode)' : ''))
                if $self->{debug};

            my $had_timeout = eval {
                local $SIG{ALRM} = sub { die "alarm\n" };
                $self->_pump_one_line;
                alarm 0;
            };

            if (my $error = $@) {
                die unless $error eq "alarm\n";
                # timed out
                $self->test_builder->ok(0, "timed out");
                $self->test_builder->BAIL_OUT('Dangerous to continue after a time out');
            } else {
                if ($had_timeout) {
                    $self->test_builder->ok(1, "output was present with $had_timeout seconds left before timeout");
                }
            }

            ${$self->output} =~ s/^([^\n]*?)\r?\n//;
            my $output = $1;
            $output = $self->ansi_escape($output);

            if ($expected_output->{match_type} eq 'literal') {

                $self->test_builder->is_eq($output, $expected_output->{output},
                                           sprintf(q{literal match of '%s'}, $expected_output->{output}));

            } elsif ($expected_output->{match_type} eq 'regex') {

                my $regex = $expected_output->{output};
                use re 'eval'; # allow delayed interpolation trickery
                $self->test_builder->like($output, qr/$regex/,
                                          sprintf(q{regex match of '%s'}, $expected_output->{output}));

            } else {

                croak(sprintf(q{unknown match type '%s'},
                              $expected_output->{match_type}));

            }

            if ($fastforwarding) {

                my $tb = $self->test_builder;
                my $current_test = $tb->current_test;
                my @details = $tb->details;
                if ($details[$current_test - 1]->{ok}) {
                    # a test passed.  stop fastforwarding
                    $fastforwarding = 0;
                    $tb->output($fastforwarding_output_control->{current_output});
                    $tb->failure_output($fastforwarding_output_control->{current_failure_output});
                    $tb->todo_output($fastforwarding_output_control->{current_todo_output});
                    # and belatedly output the results of the passing
                    # test, since it's been suppressed with the rest
                    $tb->output->print($fastforwarding_buffer);
                    $fastforwarding_buffer = '';
                    $fastforwarding_sink = '';
                } else {
                    # didn't pass, but we're fastforwarding.  it may
                    # still pass in the future

                    # rewrite TB's history
                    $tb->current_test($current_test - 1);
                    # put the test back in the queue
                    unshift @{$command->{outputs}}, $expected_output;
                    # forget about the output of the failing test
                    $fastforwarding_buffer = '';
                    $fastforwarding_sink = '';
                }

            }

        }

        $self->_pump_one_line(blocking => 0);

        if (${$self->output}) {

            $self->test_builder->ok(0, sprintf(q{extra unmatched output for '%s'},
                                               defined($command->{shell}) ? $command->{shell} : '<preamble>'));
            $self->test_builder->diag(map { "'$_'" } split(/\r?\n/, ${$self->output}));

        } else {

            $self->test_builder->ok(1, sprintf(q{all output tested for '%s'},
                                               defined($command->{shell}) ? $command->{shell} : '<preamble>'));

        }

        # cleanup output to make room for the next command
        ${$self->output} = '';

    }

    $self->subprocess->finish;
    return $self;

}

sub _parse_test_spec {

    my ($class, $handle) = @_;

    my $shell;
    my $commands = [];
    my $plan_size = 0;
    my @errors;

    my %valid_output_prefixes = ('    ' => 'literal',
                                 're: ' => 'regex');
    my $output_prefix_re = join('|', keys %valid_output_prefixes);

    my %valid_commands = ('fastforward' => 1,
                          'wait_less_than' => 1,
                          'preamble' => 1);
    my $commands_re = join('|', keys %valid_commands, '\w+');

    while (defined(my $line = $handle->getline)) {

        chomp($line);

        if ($line =~ /^#/) {

            # Comment, skip it... unless it's a shebang

            if ($handle->input_line_number <= 1
                and $line =~ m/^#!/) {

                $shell = $line;
                $shell =~ s/^#!//;
                $shell = [ shellwords($shell) ];

            }

        } elsif ($line =~/^$/) {

            # Empty line (no starting whitespace) is only for clarity

        } elsif ($line =~ /^ {4}\$ (.*)$/) {

            # Shell command, push into the arrayref to create a new
            # command/output couple

            my $command = $1;
            push @{$commands}, { shell => $command,
                                 outputs => [] };
            $plan_size++; # "no leftover output" test

        } elsif ($line =~ /^($output_prefix_re)(.*)$/) {

            # Output line with optional match type

            my ($match_type, $output) = ($valid_output_prefixes{$1}, $2);

            unless (grep { exists $_->{shell} } @{$commands}) {

                push @errors, { line_number => $handle->input_line_number,
                                error => 'Output before any shell commands' };
                next;

            }

            push @{$commands->[-1]->{outputs}}, { match_type => $match_type,
                                                  output => $output };
            $plan_size++; # a literal or regex match test

        } elsif ($line =~ /^($commands_re)\s*(.*)$/) {

            # Lembas command
            my ($command, $parameters) = ($1, $2);

            unless (exists $valid_commands{$command}) {

                push @errors, { line_number => $handle->input_line_number,
                                error => sprintf(q{Call to unknown command '%s'},
                                                 $command) };
                next;

            }

            unless ($command eq 'preamble'
                    or grep { exists $_->{shell} } @{$commands}) {

                push @errors, { line_number => $handle->input_line_number,
                                error => 'Call to command before any shell commands' };
                next;

            }

            my @parameters = quotewords('\s+', 0, $parameters);

            if ($parameters
                and not @parameters) {

                # parsing into quoted words failed, probably
                # unbalanced delimiters.
                push @errors, { line_number => $handle->input_line_number,
                                error => 'Parameter list appears to contain unbalanced delimiters' };
                next;

            }

            if ($command eq 'preamble') {
                if (@{$commands}) {

                    push @errors, { line_number => $handle->input_line_number,
                                    error => 'Call to "preamble" command after shell commands' };
                    next;

                }

                push @{$commands}, { shell => undef,
                                     outputs => [] };
                # preamble command also ends up generating a "no
                # leftover output" test
                $plan_size++;

            } elsif ($command eq 'wait_less_than') {

                if (@parameters > 2
                    or @parameters < 1) {
                    push @errors, { line_number => $handle->input_line_number,
                                    error => 'wait_less_than command accepts 1 or 2 parameters' };
                    next;
                } elsif (@parameters == 2) {
                    my ($value, $unit) = @parameters;
                    if ($unit =~ /second(?:s)?/) {
                        @parameters = ($value);
                    } elsif ($unit =~ /minute(?:s)?/) {
                        @parameters = ($value * 60);
                    } else {
                        push @errors, { line_number => $handle->input_line_number,
                                        error => "unknown time unit $unit" };
                        next;
                    }
                }

                push @{$commands->[-1]->{outputs}}, { command => $command,
                                                      parameters => \@parameters };
                # wait_less_than command generates a failure if the
                # shell command timed out, then a BAIL_OUT occurs; or
                # a success if there was no timeout
                $plan_size++;

            } else {

                push @{$commands->[-1]->{outputs}}, { command => $command,
                                                      parameters => \@parameters };

            }

        } else {

            push @errors, { line_number => $handle->input_line_number,
                            error => 'Syntax error' };

        }

    }

    if (@errors) {

        croak "Errors were found while processing a file:\n"
            .join("\n", map { sprintf(q{l.%d: %s},
                                      $_->{line_number},
                                      $_->{error}) } @errors);

    }

    return ($shell, $commands, $plan_size);

}

1;

=pod

=head1 NAME

Lembas -- Testing framework for command line applications inspired by Cram

=head1 SYNOPSIS

  use Test::More;
  use Lembas;
  
  open my $specs, '<', 'hg-for-dummies.lembas'
      or BAILOUT("can't open Mercurial session test specs: $!");
  
  my $lembas = Lembas->new_from_test_spec(handle => $specs);
  plan tests => $lembas->plan_size;
  $lembas->run;

=head1 DESCRIPTION

=head2 WHAT IS LEMBAS

It's better than cram :)

In short, you write down shell sessions verbatim, allowing for
variance such as "this part here should match this regex" or "then
there's some output nobody really cares about" or even "this output
should be printed within N seconds".  The markup is really very simple
so you can almost copy-paste real shell sessions and have it work.

Then Lembas will spawn a shell process of your choice and pass it the
commands and test if the output matches what's expected, thereby
turning your shell session into a test suite!

You can get the number of tests run for a suite through your Lembas
object, so you can plan your tests as usual (or just let
C<done_testing> handle it).  A design point is that the number of
tests should be constant for a given script, no matter how many lines
fail to match.

An automatic, free test tells you if you had any extra unmatched
output after your last matched line for each command.  This is not
entirely reliable though and may produce false positives.  Tuits and
patches welcome.

=head2 I DON'T GET IT, GIVE ME A REAL USE-CASE

This is a simple script suitable to test the very first chapter of
your Mercurial Course for Dummkopfs.  Whitespace is important.

  #!/bin/bash
  
  # Do all the work in /tmp
      $ cd /tmp
      $ pwd
      /tmp
  
  # Testing presence of Mercurial
      $ hg --version
  re: Mercurial Distributed SCM \(version [\d.]+\)
      (see http://mercurial.selenic.com for more information)
      
  re: Copyright \(C\) [\d-]+ Matt Mackall and others
      This is free software; see the source for copying conditions. There is NO
      warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
  # Creating a repository
      $ mkdir foo
      $ cd foo
      $ pwd
      /tmp/foo
      $ hg init
      $ echo "this is a file" > content
      $ cat content
      this is a file
      $ hg add content
      $ hg st
      A content
      $ hg commit -m "created repo and added a file"
  
  # Checking that everything looks good
      $ hg log
  re: changeset:   0:[a-f0-9]{12}
      tag:         tip
      user:        Fabrice Gabolde <fabrice.gabolde@gmail.com>
  re: date:        .*
      summary:     created repo and added a file
      
  
  # Cleanup
      $ rm -Rf /tmp/foo

Lines with content in the first four characters are assumed to be
Lembas commands (or comments).  The rest are shell commands (if they
start with "$ ") or shell output (otherwise).  The syntax for "this
line of output should be matched as a regex" is problematic; Cram puts
" re" at the end of a line but it seems ugly to me.

The shebang-looking line at the top is almost exactly that.  It's a
user-friendly way of specifying "run this command with these
arguments".

You'll notice that this works with or without the C<color> extension
for Mercurial; Lembas removes ANSI terminal escape characters before
matching output.

=head1 METHODS

=head2 new

  my $lembas = Lembas->new(shell => [ '/bin/bash' ],
                           commands => [ { shell => 'whoami',
                                           outputs => [ 'fgabolde' ] } ]);

Creates a new Lembas object with the corresponding settings and
expected outputs.

=head2 new_from_test_spec

  my $lembas = Lembas->new_from_test_spec(shell => [ '/bin/bash' ],
                                          handle => $iohandle);

Same as C<new>, but parses a spec file first and uses it to set the
command/outputs list.  The C<shell> parameter is optional if you have
a shebang in the spec file, but still useful in case you want to force
a different program.

=head2 plan_size

  plan tests => $lembas->plan_size + $the_rest;

If you dislike C<done_testing>, you can use this method to obtain the
number of tests that will be run by this Lembas instance.

=head2 run

  $lembas->run;

Uses L<Test::Builder> to run all the tests specified, reporting
success, failure and diagnostics in the usual manner.

=head1 SEE ALSO

L<Test::Builder>

The original inspiration for Lembas: L<Cram|https://bitheap.org/cram/>

If you're looking for something more complex and low-level, you
probably want L<Expect.pm> instead.

=head1 AUTHOR

Fabrice Gabolde <fabrice.gabolde@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 Fabrice Gabolde

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
