#!/usr/bin/perl -wT

use strict;

use Test::More tests => 22;

# Use
use_ok('Net::BGP');
use_ok('Net::BGP::ASPath');
use_ok('Net::BGP::NLRI');
use_ok('Net::BGP::Update');
use_ok('Net::BGP::Notification');
use_ok('Net::BGP::Refresh');
use_ok('Net::BGP::Peer');
use_ok('Net::BGP::Process');

# What's happening here?
# ======================
# Two peers in same process connects - one is passive and listning
# the other active and not listning.
#
# On recieving the 2nd keepalive message the listner sends an UPDATE
# message and a REFRESH-message.
#
# On recieving the REFRESH-message, the active peer stops the
# connection and remove it self from the BGP process.
#
# The Cease notification also makes the listning peer remove it self
# from the BGP process.
#
# Along with this, the listning peer has a user timer that is called
# 2 times with atleast 1 second between.

my $lh1 = '127.0.0.1';
my $lh2 = '127.0.0.1'; # Windows don't know of 127.0.0.2!
my $port = 10179; # > 1024 to allow non-root execution
#my $port = 179; # 179 for debuging!
my $keepalive = 5; # To allow faster execution. Must long enough to allow code to execute!

# Construction
my $bgp = new Net::BGP::Process(
	Port	=> $port
	);

ok(ref $bgp eq 'Net::BGP::Process','process construction');


my $listen = new Net::BGP::Peer(
        Start   => 1,
	Listen  => 1,
	Passive => 1,
        ThisID  => $lh1,
        ThisAS  => 65001,
        PeerID  => $lh2,
        PeerAS  => 65002,
	PeerPort=> $port,
	Refresh	=> 1,
	KeepAliveTime	=> $keepalive
	);
ok(ref $listen eq 'Net::BGP::Peer','listing peer construction');

my $active = new Net::BGP::Peer(
        Start   => 1,
	Listen  => 0,
	Passive => 0,
        ThisID  => $lh2,
        ThisAS  => 65002,
        PeerID  => $lh1,
        PeerAS  => 65001,
	PeerPort=> $port,
	Refresh	=> 1,
	KeepaliveTime	=> $keepalive
	);
ok(ref $active eq 'Net::BGP::Peer','active peer construction');

my $msg = join('',map { pack('H2',$_); } qw (
	00 00 00 14  40 01 01 00  40 02 06 02  02 FD EB FD
        EA 40 03 04  0A FF 67 01  18 0A 02 01
	));
my $update = Net::BGP::Update->_new_from_msg($msg);
$listen->add_timer(\&timer,1);
$listen->set_open_callback(\&opencallback);
$listen->set_keepalive_callback(\&keepcallback);
$active->set_update_callback(\&updatecallback);
$active->set_refresh_callback(\&refreshcallback);
$listen->set_reset_callback(\&resetcallback);
$active->set_reset_callback(\&resetcallback);
$listen->set_notification_callback(\&notifcallback);
$active->set_notification_callback(\&notifcallback);
$listen->set_error_callback(\&errorcallback);
$active->set_error_callback(\&errorcallback);

$bgp->add_peer($listen);
$bgp->add_peer($active);

my ($openok,$keepok,$updateok,$estabok,$refreshok,$shutlok,$shutaok,$timerok,$resetaok,$resetlok) = (0,0,0,0,0,0,0,0,0,0);

my $keepseq = 0;

print "# Main BGP test may take between $keepalive and " . $keepalive*3 . " seconds\n";

eval
 {
  local $SIG{ALRM} = sub
   {
    $bgp->remove_peer($listen);
    $bgp->remove_peer($active);
    die "time-out\n";
   };
  alarm $keepalive*3-1;
  $bgp->event_loop();
  alarm 0;
 };

if ($@)
 {
  die unless $@ eq "time-out\n";   # propagate unexpected errors
  fail('Time-out! BGP test failed!');
 }
else
 {
  pass('event_loop finished');
 };

ok($openok,      'Open callback');
ok($keepok,      'Keepalive callback');
ok($updateok,    'Update callback');
ok($estabok,     'Refresh callback (established)');
ok($refreshok,   'Refresh callback (refresh request)');
ok($resetlok,    'Reset listner');
ok($resetaok,    'Reset active');
ok($shutlok == 2,'Shutdown listner');
ok($shutaok == 2,'Shutdown active');
ok($timerok == 2,'Timer');


## End of test script - Functions below ##

sub opencallback
{
 my ($peer) = @_;
 $openok = ref $peer eq 'Net::BGP::Peer';
};

sub keepcallback
{
 my ($peer) = @_;
 if ($keepseq == 1)
  {
   $keepok = ref $peer eq 'Net::BGP::Peer';
  };
 if ($timerok == 2)
  {
   $peer->update($update);
   $peer->refresh();
   $peer->set_keepalive_callback(undef);
  };
 $keepseq += 1 unless $keepseq == 2;
}

sub updatecallback
{
 my ($peer,$update) = @_;
 $updateok = ref $peer eq 'Net::BGP::Peer';
 $updateok &&= $update->_encode_message eq $msg;
}

sub refreshcallback
{
 my ($peer,$refresh) = @_;
 unless (defined $refresh)
  {
   $estabok = 1;
   return;
  };
 $refreshok = ref $peer eq 'Net::BGP::Peer';
 $refreshok &&= ref $refresh eq 'Net::BGP::Refresh';
 $peer->stop;
}

sub resetcallback
{
 my ($peer) = @_;
 if ($peer eq $listen)
  {
   $resetlok = 1;
  }
 elsif ($peer eq $active)
  {
   $resetaok = 1;
  }
 else
  {
   fail('Unknown peer in reset-callback');
  }
}

sub notifcallback
{
 my ($peer,$error) = @_;
 if ($peer eq $listen)
  {
   $shutlok = $error->error_code() == 6;
   $shutlok &&= $error->error_subcode() == 0;
   $bgp->remove_peer($peer);
  }
 elsif ($peer eq $active)
  {
   fail('Acrive peer in notification-callback');
  }
 else
  {
   fail('Unknown peer in notification-callback');
  };
};

sub errorcallback
{
 my ($peer,$error) = @_;
 if ($peer eq $listen)
  {
   $shutlok &&= $error->error_code() == 6;
   $shutlok &&= $error->error_subcode() == 0;
   $shutlok = 2 if $shutlok;
  }
 elsif ($peer eq $active)
  {
   $shutaok = $error->error_code() == 6;
   $shutaok &&= $error->error_subcode() == 0;
   $shutaok = 2 if $shutaok;
   $peer->set_error_callback(undef);
   $bgp->remove_peer($peer);
  }
 else
  {
   fail('Unknown peer in error-callback');
  };
};

sub timer
{
 my ($peer) = @_;
 $timerok += 1;
 $timerok = 0 unless $peer eq $listen;
 $peer->remove_timer(\&timer) if ($timerok == 2);
};

__END__

