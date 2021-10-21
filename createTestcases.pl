#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

my @inputs = (
    "+",
    "+/",
    ">./",
    "% >./",
    "+/ % # ",
    "32&+@(1.8&*)",
    "+/\"1",
    "1.04&*@:+",
    "+/@:*\"1",
    "/:~",
    "10*i.3 3",
    "(,,.~)^:4 ,1",
    "_1 0 0, 0 1 0,:_0.5 0.5 1",
    "3 3 <;._3 i. 5 5",
    "<.@(+/ % #)@(,/)",
    "&.>",
    "<\"",
    "/\\",
    "~/~",
);

my $i = 0;
for (@inputs) {
    my $src = "nub =: $_
x =: 5!:4 <'nub'
smoutput x
exit ''
";
    open my $f_inp, '>', sprintf "inp%02d", $i;
    say {$f_inp} $_;
    close $f_inp;
    {
        open my $f_out, '>', 'out.ijs';
        say {$f_out} $src;
        close $f_out;
    }
    open my $f_tree, '>', sprintf "tree%02d", $i;
    print {$f_tree} `jconsole out.ijs`;
    close $f_tree;
    {
        open my $f_out, '>', 'out.ijs';
        $src =~ s/5!:4/5!:6/;
        say {$f_out} $src;
        close $f_out;
    }
    open my $f_paren, '>', sprintf "paren%02d", $i;
    print {$f_paren} `jconsole out.ijs`;
    close $f_paren;
    $i++;
}
