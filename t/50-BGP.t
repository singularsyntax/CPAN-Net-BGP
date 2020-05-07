#!/usr/bin/perl -wT

use strict;

use Test::More tests => 29;

# Use
use_ok('Net::BGP');
use_ok('Net::BGP::ASPath');
use_ok('Net::BGP::NLRI');
use_ok('Net::BGP::Update');
use_ok('Net::BGP::Notification');
use_ok('Net::BGP::Refresh');
use_ok('Net::BGP::Peer');
use_ok('Net::BGP::Process');

use_ok('IO::Async::Loop');
use_ok('IO::Async::Timer::Countdown');

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

my $port = random_high_port();

my $keepalive = 5; # To allow faster execution. Must long enough to allow code to execute!

# Construction
my $event_loop = IO::Async::Loop->new();
my $bgp = new Net::BGP::Process(
    Port      => $port,
    EventLoop => $event_loop
);

ok(defined($event_loop), 'event loop construction');
ok(ref $bgp eq 'Net::BGP::Process','process construction');


my $listen = new Net::BGP::Peer(
        Start           => 1,
        Listen          => 1,
        Passive         => 1,
        ThisID          => $lh1,
        ThisAS          => 65001,
        PeerID          => $lh2,
        PeerAS          => 65002,
        PeerPort        => $port,
        AnnounceRefresh => 1,
        SupportMBGP     => 1,
        KeepAliveTime   => $keepalive
);
ok(ref $listen eq 'Net::BGP::Peer','listing peer construction');

my $active = new Net::BGP::Peer(
        Start           => 1,
        Listen          => 0,
        Passive         => 0,
        ThisID          => $lh2,
        ThisAS          => 65002,
        PeerID          => $lh1,
        PeerAS          => 65001,
        PeerPort        => $port,
        AnnounceRefresh => 1,
        SupportMBGP     => 1,
        KeepaliveTime   => $keepalive
);
ok(ref $active eq 'Net::BGP::Peer','active peer construction');

my $msg = join('',map { pack('H2',$_); } qw (
	00 00 00 14  40 01 01 00  40 02 06 02  02 FD EB FD
        EA 40 03 04  0A FF 67 01  18 0A 02 01
	));
my $update = Net::BGP::Update->_new_from_msg($msg);

my $msg2 = join('',map { pack('H2',$_); } qw (
	00 00 00 14  40 01 01 00  40 02 06 02  02 FD EB FD
        EA 40 03 04  0A FF 67 01  18 0A 02 02  18 0A 02 03
	));
my $update2 = Net::BGP::Update->_new_from_msg($msg2);

$listen->add_timer(\&timer,1);
$listen->set_open_callback(\&opencallback);
$listen->set_keepalive_callback(\&keepcallback);
$active->set_update_callback(\&updatecallback);
$listen->set_established_callback(\&establishedcallback);
$active->set_refresh_callback(\&refreshcallback);
$listen->set_reset_callback(\&resetcallback);
$active->set_reset_callback(\&resetcallback);
$listen->set_notification_callback(\&notifcallback);
$active->set_notification_callback(\&notifcallback);
$listen->set_error_callback(\&errorcallback);
$active->set_error_callback(\&errorcallback);

$bgp->add_peer($listen);
$bgp->add_peer($active);

my ($openok,$keepok,$establishok,$updateok,$estabok,$refreshok,$shutlok,$shutaok,$timerok,$resetaok,$resetlok) = (0,0,0,0,0,0,0,0,0,0);

my $estseq = 0;
my $keepseq = 0;
my $updateseq = 0;

print "# Main BGP test may take between $keepalive and " . $keepalive*3 . " seconds\n";

my $timer = IO::Async::Timer::Countdown->new(
    delay => ($keepalive * 3 - 1),
    on_expire => sub {
        $bgp->remove_peer($listen);
        $bgp->remove_peer($active);
        die "time-out\n";
    },
    remove_on_expire => 1
);

$timer->start();
$event_loop->add($timer);

eval {
    $bgp->event_loop();
    $timer->stop();
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
ok($establishok, 'Established callback');
ok($updateok,    'Update callback');
ok($estabok,     'Refresh callback (established)');
ok($refreshok,   'Refresh callback (refresh request)');
ok($resetlok,    'Reset listner');
ok($resetaok,    'Reset active');
ok($shutlok == 2,'Shutdown listner');
ok($shutaok == 2,'Shutdown active');
ok($timerok == 2,'Timer');


$bgp->add_peer($active);
$bgp->add_peer($listen);
ok(scalar($bgp->peers()) == 2, 'Two peers');

$bgp->remove_peer($active);
$bgp->remove_peer($listen);
ok(scalar($bgp->peers()) == 0, 'No peers');


## End of test script - Functions below ##

##
# Return a random TCP port number in the range [32768, 65536). If a parameter is
# provided specifying another port, ensure the port returned is different.
#
sub random_high_port
{
    my $unique_port = shift();
    my $random_port = int(rand(65536 - 32768)) + 32768;

    if (defined($unique_port))
    {
        if ($random_port == $unique_port)
        {
            return random_high_port($unique_port);
        }
    }

    return $random_port;
}

sub opencallback
{
    my ($peer) = @_;

    $openok = $peer->peer_can_mbgp;
    $openok &&= $peer->support_mbgp;
    $openok &&= ref $peer eq 'Net::BGP::Peer';
    
    # We're also going to validate open encoding here
    # Format is:
    # Octet 0-15 : Marker
    # Octet 16-17: Length (starting at marker)
    # Octet 18   : Type (01 = Open)
    # Octet 19   : Version (04 = BGP 4)
    # Octet 20-21: My ASN (FD E9 = 65001)
    # Octet 22-23: Hold Time (00 5A = 90)
    # Octet 24-27: BGP ID (7F 00 00 01 = 127.0.0.1)
    # Octet 28   : Optional Param Length (10 = 16 bytes)
    # Octet 29   : Opt. Param Type (02 = Capability)
    # Octet 30   :  Opt Param Len (06 = 6 bytes)
    # Octet 31   :  Capability Code (01 = MBGP)
    # Octet 32   :   Capability Length (04 = 4 bytes)
    # Octet 33   :   Address Family (01 = IPv4)
    # Octet 34   :   Reserved Bit (00)
    # Octet 35   :   Address Type (01 = Unicast)
    # Octet 36   : Opt. Param Type (02 = Capability)
    # Octet 37   :  Opt Param Len (02 = 2 bytes)
    # Octet 38   :  Capability Code (02 = Refresh)
    # Octet 39   :   Capability Length (00 = 0 bytes)
    # Octet 40   : Opt. Param Type (02 = Capability)
    # Octet 41   :  Opt Param Len (02 = 2 bytes)
    # Octet 42   :  Capability Code (80 = Refresh Old/Cisco)
    # Octet 43   :   Capability Length (00 = 0 bytes)
    my $desired_open = join('',map { pack('H2',$_); } qw (
        FF FF FF FF  FF FF FF FF  FF FF FF FF  FF FF FF FF
        00 2D 01 04  FD E9 00 5A  7F 00 00 01  10 02 06 01
        04 00 01 00  01 02 02 02  00 02 02 80  00
    ));
    my $sample_open = $peer->transport->_encode_bgp_open_message();
    ok ($sample_open eq $desired_open, "Proper open message encoding");
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

sub establishedcallback
{
    my ($peer) = @_;
    if ($estseq == 0) {
        $establishok = ref $peer eq 'Net::BGP::Peer';
    } else {
        $establishok = undef;
    };
    $peer->update($update2);
    $estseq += 1 unless $estseq == 2;
}

# First call will be for the ESTABLISHED callback.
# Second call will be for the second KEEPALIVE callback
sub updatecallback
{
    my ($peer,$update) = @_;
    if (ref $peer eq 'Net::BGP::Peer') {
        if (ref $update eq 'Net::BGP::Update') {
            $updateseq++;
        } else {
            $updateseq = 3000;
        }
    } else {
        $updateseq = 4000;
    }

    if ($updateseq == 1) {
        if ($update->_encode_message ne $msg2) {
            $updateseq = 1000;
        }
    } elsif ($updateseq == 2) {
        if ($update->_encode_message ne $msg) {
            $updateseq = 2000;
        } else {
            $updateok = 1;
        }
    }
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
   $timer->stop();
   $event_loop->remove($timer);
   $bgp->remove_peer($peer);
  }
 elsif ($peer eq $active)
  {
   fail('Active peer in notification-callback');
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

