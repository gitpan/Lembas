use strict;
use warnings;
use 5.010;
use Carp;

use Test::More;
use Test::Exception;
use Test::Builder;

use_ok 'Lembas';

new_ok 'Lembas', [ test_builder => Test::Builder->create,
                   shell => 'examples/ush',
                   commands => [ { command => 'preamble' },
                                 { outputs => { match_type => 're',
                                                output => '' } } ] ];

done_testing;
