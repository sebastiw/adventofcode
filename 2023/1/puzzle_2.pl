#!/bin/perl -n

$rev = reverse($_);

my $first = $2 if (/^(.*?([0-9]|zero|one|two|three|four|five|six|seven|eight|nine))/);
my $second = $2 if ($rev =~ /^\n(.*?([0-9]|orez|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin))/);

$first = $first =~ s/zero/0/r =~ s/one/1/r =~ s/two/2/r =~ s/three/3/r =~ s/four/4/r =~ s/five/5/r =~ s/six/6/r =~ s/seven/7/r =~ s/eight/8/r =~ s/nine/9/r;
$second = $second =~ s/orez/0/r =~ s/eno/1/r =~ s/owt/2/r =~ s/eerht/3/r =~ s/ruof/4/r =~ s/evif/5/r =~ s/xis/6/r =~ s/neves/7/r =~ s/thgie/8/r =~ s/enin/9/r;

print $first . $second . "\n";

# Then pipe through `awk '{ sum += $1 } END { print sum }'`
