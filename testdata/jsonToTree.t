#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;
use JSON::PP;
use File::Slurp;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin";
use jsonToTree;

# jsonToTree
{
    my $text = read_file('json00');
    my $want = read_file('tree00');
    my $has = jsonToTree::jsonToTree($text);
    is($has, $want, 'base case');
}
{
    my $text = read_file('json01');
    my $want = read_file('tree01');
    my $has = jsonToTree::jsonToTree($text);
    is($has, $want, 'case 01');
}
{
    my $text = read_file('json02');
    my $want = read_file('tree02');
    my $has = jsonToTree::jsonToTree($text);
    is($has, $want, 'case 02');
}
# {
#     my $text = read_file('json06');
#     my $want = read_file('tree06');
#     my $has = jsonToTree::jsonToTree($text);
#     is($has, $want, 'case 06');
# }

done_testing();

1;

