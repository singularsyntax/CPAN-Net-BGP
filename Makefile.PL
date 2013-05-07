use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Net::BGP',
    'ABSTRACT'		=> 'Object-oriented API to the BGP protocol',
    'AUTHOR'		=> 'Stephen J. Scheck <code@neurosphere.com>',
    'VERSION_FROM'	=> 'lib/Net/BGP.pm', # finds $VERSION
    'SIGN'		=> 1,
    'PREREQ_PM'		=> {
                                'List::Util'    => 1.01, # Actully Scalar::Util - but no version there!
                                'Test::Harness' => '2.00', # ?
                                'Test::More'    => '0.47'

	}
);
