#!/usr/bin/perl -w
package jsonToTree;
use v5.30;
use Data::Dumper;
use JSON::PP;
use File::Slurp;

sub jsonToTree {
    my ($text) = @_;
    my $tree = decode_json($text);
    return walkTree($tree, 2) . "\n";
}

sub walkTree {
    my ($tree, $len) = @_;
    my $ret = '';
    for (keys $tree->%*) {
        $ret .= '─' x $len . " $_";
        if (defined $tree->{$_}) {
            for ($tree->{$_}->@*) {
                $ret .= ' ' . walkTree($_, $len + 1);
            }
        }
    }
    return "$ret";
}

1;
