#!/usr/bin/env perl

use strict;
use warnings;

use MarpaX::Languages::Perl::PackUnpack ':constants';

use Test::More;

# -------------

my($parser) = MarpaX::Languages::Perl::PackUnpack -> new(options => nothing_is_fatal);
my(@text)   =
(
	q|j|,
	q|s!10|,
	q|s![*]|,
	q|C0 U4|,
	q|j<![10]|,
	q|a3/A A*|,
	q|( s l )<|,
	q|n/a* w/a2|,
	q|d[ x![ d]]|,
	q|@1 A ( ( @2 A ) @3 A )|,
);
my($count) = 0;

my($result);

for my $text (@text)
{
	$count += 2;

	$result = $parser -> parse($text);

	ok($result == 0, "Parsed:    $text");
	ok($text eq $parser -> template_report, "Retrieved: $text");
}

done_testing($count);
