#!/usr/bin/perl

package Net::BGP::Process;

use strict;
use vars qw( $VERSION );

## Inheritance and Versioning ##

$VERSION = '0.17';

## Module Imports ##

use Carp;
use IO::Async::Listener;
use IO::Async::Loop;
use IO::Async::Signal;
use IO::Async::Timer::Periodic;
use List::Util qw( any );
use Net::BGP::Peer qw( BGP_PORT TRUE FALSE );

## Socket Constants ##

sub INADDR_ANY { '0.0.0.0' }
sub LISTEN_QUEUE_SIZE { 5 }

## Public Methods ##

sub new
{
    my $class = shift();
    my ($arg, $value);

    my $this = {
        _event_loop     => undef,
        _listener       => undef,
        _listen_addr    => INADDR_ANY,
        _listen_port    => BGP_PORT,
        _peer_addr      => {},
        _peer_list      => {},
        _sig_pipe       => undef
    };

    while ( defined($arg = shift()) ) {
        $value = shift();
        if ( $arg =~ /port/i ) {
            $this->{_listen_port} = $value;
        }
	elsif ( $arg =~ /listenaddr/i ) {
            $this->{_listen_addr} = inet_aton($value);
        }
        elsif ( $arg =~ /eventloop/i ) {
            $this->{_event_loop} = $value;
        }
	else {
	    croak "Unknown argument '$arg'";
	}
    }

    bless($this, $class);

    $this->_io_async_init_event_loop();

    return ( $this );
}

sub add_peer
{
    my ($this, $peer) = @_;

    $this->{_peer_addr}->{$peer->this_id}->{$peer->peer_id} = $peer if $peer->is_listener;
    $this->{_peer_list}->{$peer} = $peer;

    if ( defined($this->{_event_loop}) ) {
        $this->_io_async_init_listen_socket();
        $this->_attach_transport($peer->transport());

        $peer->{_event_loop} = $this->{_event_loop};
        foreach my $timer ( values(%{ $peer->{_user_timers} }) ) {
            $timer->start();
            $this->{_event_loop}->add($timer);
        }
    }
}

sub remove_peer
{
    my ($this, $peer) = @_;

    if ( defined($this->{_peer_list}->{$peer}) ) {
        $peer->stop();
        delete $this->{_peer_addr}->{$peer->this_id}->{$peer->peer_id};
        delete $this->{_peer_list}->{$peer};
    }

    foreach my $timer ( values(%{ $peer->{_user_timers} }) ) {
        $timer->stop();
        $this->{_event_loop}->remove($timer);
    }

    $peer->{_event_loop} = undef;
    $this->_detach_transport($peer->transport());

    # Return from event loop when there are no more peers
    if ( scalar(keys(%{$this->{_peer_list}})) == 0 ) {
        $this->_cleanup();
        if ( scalar($this->{_event_loop}->notifiers()) == 0 ) {
            $this->{_event_loop}->stop();
        }
    }
}

sub peers
{
    return values(%{shift()->{_peer_list}});
}

sub event_loop
{
    my $this = shift();

    $this->{_event_loop}->run();
    $this->_cleanup();
}

## Private Methods ##

sub _io_async_init_event_loop
{
    my $this = shift();

    if ( ! defined($this->{_event_loop})) {
        $this->{_event_loop} = IO::Async::Loop->new();
    }

    # Ignore SIGPIPE
    my $sigpipe_handler = IO::Async::Signal->new(
        name => "PIPE",
        on_receipt => sub {
            ## IGNORED ##
        }
    );

    $this->{_sig_pipe} = $sigpipe_handler;
    $this->{_event_loop}->add($sigpipe_handler);
}

sub _io_async_init_listen_socket
{
    my $this = shift();

    if ( ! defined($this->{_listener}) ) {
        # Poll each peer and create listen socket if any is a listener
        foreach my $peer ( values(%{$this->{_peer_list}}) ) {
            if ( $peer->is_listener() ) {
                my $listener = IO::Async::Listener->new(

                    on_accept => sub {
                        my ($listener, $socket) = @_;
                        my $transport = $this->_get_peer_transport($socket);

                        if ( (defined($transport)) && (! $transport->{_parent}->is_passive()) ) {
                            $this->_attach_transport($transport);
                        }
                    }
                );

                $this->{_listener} = $listener;
                $this->{_event_loop}->add($listener);

                $listener->listen(
                    addr => {
                        family   => "inet",
                        socktype => "stream",
                        port     => $this->{_listen_port},
                        ip       => $this->{_listen_addr}
                    },

                    queuesize => LISTEN_QUEUE_SIZE,
                    reuseaddr => TRUE,

                    on_listen_error => sub {
                        croak("on_listen_error(): ", shift());
                    }
                );

                last;
            }
        }
    }
}

# Socket Init
#
# - set non-blocking

# Event Loop
#
# - remove SIGPIPE handler on event loop exit

sub _cleanup
{
    my $this = shift();

    if ( defined($this->{_listener}) ) {
        $this->{_listener}->close();
        $this->{_listener} = undef;
    }

    if ( defined($this->{_sig_pipe}) ) {
        $this->{_event_loop}->remove($this->{_sig_pipe});
        $this->{_sig_pipe} = undef;

        # When we add our SIGPIPE handler, IO::Async adds its own
        # for some reason. Clean it up as well...

        foreach my $notifier ( $this->{_event_loop}->notifiers() ) {
            if ( $notifier->notifier_name() eq 'sigpipe' ) {
                $this->{_event_loop}->remove($notifier);
                last;
            }
        }
    }
}

sub _get_peer_transport
{
    my ($this, $socket) = @_;
    my $sock_host = $socket->sockhost();
    my $peer_host = $socket->peerhost();
    my $peer = $this->{_peer_addr}->{$sock_host}->{$peer_host};
    my $transport = undef;

    if ( ! defined($peer) ) {
        carp("Ignored incoming connection from unknown peer ($peer_host => $sock_host)");
        $socket->close();
    } elsif ( ! $peer->is_listener() ) {
        carp("Ignored incoming connection for non-listening peer ($peer_host => $sock_host)");
        $socket->close();
    } else {
        $transport = $peer->_passive_transport();
        $transport->_connected($socket);
    }

    # TODO: handle Mikrotik patch case???
    # $transport->{_sibling}->_handle_collision_selfdestuct;

    return $transport;
}

sub _attach_transport
{
    my ($this, $transport) = @_;

    $this->{_event_loop}->add($transport);
    $this->{_event_loop}->add($transport->{_connect_retry_timer});
    $this->{_event_loop}->add($transport->{_hold_timer});
    $this->{_event_loop}->add($transport->{_keep_alive_timer});

    $transport->_handle_pending_events();
}

sub _detach_transport
{
    my ($this, $transport) = @_;

    # TODO: none of this may not be necessary if all event loop cleanup is handled
    #       cleanly within Transport.pm

    if ( any { $_ eq $transport->{_connect_retry_timer} } $this->{_event_loop}->notifiers() ) {
        $this->{_event_loop}->remove($transport->{_connect_retry_timer});
    }

    if ( any { $_ eq $transport->{_hold_timer} } $this->{_event_loop}->notifiers() ) {
        $this->{_event_loop}->remove($transport->{_hold_timer});
    }

    if ( any { $_ eq $transport->{_keep_alive_timer} } $this->{_event_loop}->notifiers() ) {
        $this->{_event_loop}->remove($transport->{_keep_alive_timer});
    }

    if ( any { $_ eq $transport } $this->{_event_loop}->notifiers() ) {
        $this->{_event_loop}->remove($transport);
    }
}

## POD ##

=pod

=head1 NAME

Net::BGP::Process - Class encapsulating BGP session multiplexing functionality

=head1 SYNOPSIS

    use Net::BGP::Process;

    $bgp = Net::BGP::Process->new(
        Port       => $port,
        ListenAddr => '1.2.3.4',
        EventLoop  => IO::Async::Loop->new()
    );

    $bgp->add_peer($peer);
    $bgp->remove_peer($peer);

    @peers = $bgp->peers();

    $bgp->event_loop();

=head1 DESCRIPTION

This module encapsulates the functionality necessary to multiplex multiple
BGP peering sessions. While individual B<Net::BGP::Peer> objects contain
the state of each peering session, it is the B<Net::BGP::Process> object
which monitors each peer's transport-layer connection and timers and signals
the peer whenever messages are available for processing or timers expire.
A B<Net::BGP::Process> object must be instantiated, even if a program only
intends to establish a session with a single peer.

=head1 METHODS

I<new()> - create a new Net::BGP::Process object

    $bgp = Net::BGP::Process->new(
        Port       => $port,
        ListenAddr => '1.2.3.4',
        EventLoop  => IO::Async::Loop->new()
    );

This is the constructor for Net::BGP::Process objects. It returns a
reference to the newly created object. The following named parameters may
be passed to the constructor.

=head2 Port

This parameter sets the TCP port the BGP process listens on. It may be
omitted, in which case it defaults to the well-known BGP port TCP/179.
If the program cannot run with root priviliges, it is necessary to set
this parameter to a value greater than or equal to 1024. Note that some
BGP implementations may not allow the specification of an alternate port
and may be unable to establish a connection to the B<Net::BGP::Process>.

=head2 ListenAddr

This parameter sets the IP address the BGP process listens on. Defaults
to INADDR_ANY.

=head2 EventLoop

This parameter sets an B<IO::Async::Loop> object to use as the event loop
for internal event processing. If omitted, defaults to the legacy B<IO::Select>
internal event loop. See L<"event_loop()"> method for details on use.

I<add_peer()> - add a new peer to the BGP process

    $bgp->add_peer($peer);

Each B<Net::BGP::Peer> object, which corresponds to a distinct peering
session, must be registered with the B<Net::BGP::Process> object via this
method. It is typically called immediately after a new peer object is created
to add the peer to the BGP process. The method accepts a single parameter,
which is a reference to a B<Net::BGP::Peer> object.

I<remove_peer()> - remove a peer from the BGP process

    $bgp->remove_peer($peer);

This method should be called if a peer should no longer be managed by the
BGP process, for example, if the session is broken or closed and will not
be re-established. The method accepts a single parameter, which is a
reference to a Net::BGP::Peer object which has previously been registered
with the process object with the add_peer() method.

I<peers()> - list peers registered with the BGP process

This method returns an array of the B<Net::BGP::Peer> objects registered
with the BGP process.

I<event_loop()> - start the process event loop

    $bgp->event_loop();

This method must called after all peers are instantiated and added to the
BGP process and any other necessary initialization has occured. Once it
is called, it takes over program control flow, and control will
only return to user code when one of the event callback functions is
invoked upon receipt of a BGP protocol message or a user
established timer expires (see L<Net::BGP::Peer> for details
on how to establish timers and callback functions). The method takes
no parameters. It will only return when there are no Net::BGP::Peer
objects remaining under its management, which can only occur if they
are explicitly removed with the remove_peer() method (perhaps called
in one of the callback or timer functions).

If an external IO::Async::Loop object has been passed into the constructor,
calling this method simply delegates to the loop's run() method. However,
the typical use for an external event loop would be to multiplex several
asynchonrous, event-driven services, and common practice would be to call
its run() method directly at some other point in the program after all the
other services have been added, instead of calling the B<Net::BGP::Process>
event_loop() method directly.

=head1 SEE ALSO

Net::BGP, Net::BGP::Peer, Net::BGP::Transport, Net::BGP::Update,
Net::BGP::Refresh, Net::BGP::Notification, IO::Async

=head1 AUTHOR

Stephen J. Scheck <sscheck@cpan.org>

=cut

## End Package Net::BGP::Process ##

1;
