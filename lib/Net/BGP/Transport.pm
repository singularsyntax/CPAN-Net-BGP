#!/usr/bin/perl

package Net::BGP::Transport;

use base qw( IO::Async::Protocol::Stream );
use bytes;
use strict;
use Errno qw( EAGAIN );
use Log::Any qw( $log );

use vars qw(
    $VERSION @ISA @BGP @BGP_EVENT_MESSAGE_MAP @BGP_EVENTS @BGP_FSM @BGP_STATES
);

## Inheritance and Versioning ##

$VERSION = '0.17';

## General Definitions ##

sub TRUE  { 1 }
sub FALSE { 0 }

## BGP Network Constants ##

sub BGP_PORT      { 179 }
sub BGP_VERSION_4 {   4 }

## BGP General Constant Definitions ##

sub BGP_MESSAGE_HEADER_LENGTH { 19 }
sub BGP_MAX_MESSAGE_LENGTH    { 4096 }
sub BGP_CONNECT_RETRY_TIME    { 120 }
sub BGP_HOLD_TIME             { 90 }
sub BGP_KEEPALIVE_TIME        { 30 }

## BGP Finite State Machine State Enumerations ##

sub BGP_STATE_IDLE         { 1 }
sub BGP_STATE_CONNECT      { 2 }
sub BGP_STATE_ACTIVE       { 3 }
sub BGP_STATE_OPEN_SENT    { 4 }
sub BGP_STATE_OPEN_CONFIRM { 5 }
sub BGP_STATE_ESTABLISHED  { 6 }

## BGP State Names ##

@BGP_STATES = qw( Null Idle Connect Active OpenSent OpenConfirm Established );

## BGP Event Enumerations ##

sub BGP_EVENT_START                        { 1 }
sub BGP_EVENT_STOP                         { 2 }
sub BGP_EVENT_TRANSPORT_CONN_OPEN          { 3 }
sub BGP_EVENT_TRANSPORT_CONN_CLOSED        { 4 }
sub BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED   { 5 }
sub BGP_EVENT_TRANSPORT_FATAL_ERROR        { 6 }
sub BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED  { 7 }
sub BGP_EVENT_HOLD_TIMER_EXPIRED           { 8 }
sub BGP_EVENT_KEEPALIVE_TIMER_EXPIRED      { 9 }
sub BGP_EVENT_RECEIVE_OPEN_MESSAGE         { 10 }
sub BGP_EVENT_RECEIVE_KEEP_ALIVE_MESSAGE   { 11 }
sub BGP_EVENT_RECEIVE_UPDATE_MESSAGE       { 12 }
sub BGP_EVENT_RECEIVE_NOTIFICATION_MESSAGE { 13 }
sub BGP_EVENT_RECEIVE_REFRESH_MESSAGE      { 14 }

## BGP Event Names ##

@BGP_EVENTS = (
    'Null',
    'BGP Start',
    'BGP Stop',
    'BGP Transport connection open',
    'BGP Transport connection closed',
    'BGP Transport connection open failed',
    'BGP Transport fatal error',
    'ConnectRetry timer expired',
    'Hold Timer expired',
    'KeepAlive timer expired',
    'Receive OPEN message',
    'Receive KEEPALIVE message',
    'Receive UPDATE message',
    'Receive NOTIFICATION message',
    'Receive REFRESH message'
);

## BGP Protocol Message Type Enumerations ##

sub BGP_MESSAGE_OPEN         { 1 }
sub BGP_MESSAGE_UPDATE       { 2 }
sub BGP_MESSAGE_NOTIFICATION { 3 }
sub BGP_MESSAGE_KEEPALIVE    { 4 }
sub BGP_MESSAGE_REFRESH      { 5 }

## BGP Open Optional Parameter Types ##

sub BGP_OPTION_AUTH          { 1 }
sub BGP_OPTION_CAPABILITIES  { 2 }

## BGP Open Capabilities Parameter Types
sub BGP_CAPABILITY_MBGP        {   1 }
sub BGP_CAPABILITY_REFRESH     {   2 }
sub BGP_CAPABILITY_AS4         {  65 }
sub BGP_CAPABILITY_REFRESH_OLD { 128 }

## Event-Message Type Correlation ##

@BGP_EVENT_MESSAGE_MAP = (
    undef,
    BGP_EVENT_RECEIVE_OPEN_MESSAGE,
    BGP_EVENT_RECEIVE_UPDATE_MESSAGE,
    BGP_EVENT_RECEIVE_NOTIFICATION_MESSAGE,
    BGP_EVENT_RECEIVE_KEEP_ALIVE_MESSAGE,
    BGP_EVENT_RECEIVE_REFRESH_MESSAGE
);

## BGP FSM State Transition Table ##

@BGP_FSM = (
    undef,                                     # Null (zero placeholder)

    [                                          # Idle
        \&_close_session,                      # Default transition
        \&_handle_bgp_start_event              # BGP_EVENT_START
    ],
    [                                          # Connect
        \&_close_session,                      # Default transition
        \&_ignore_start_event,                 # BGP_EVENT_START
        undef,                                 # BGP_EVENT_STOP
        \&_handle_bgp_conn_open,               # BGP_EVENT_TRANSPORT_CONN_OPEN
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_CLOSED
        \&_handle_connect_retry_restart,       # BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED
        undef,                                 # BGP_EVENT_TRANSPORT_FATAL_ERROR
        \&_handle_bgp_start_event              # BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED
    ],
    [                                          # Active
        \&_close_session,                      # Default transition
        \&_ignore_start_event,                 # BGP_EVENT_START
        undef,                                 # BGP_EVENT_STOP
        \&_handle_bgp_conn_open,               # BGP_EVENT_TRANSPORT_CONN_OPEN
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_CLOSED
        \&_handle_connect_retry_restart,       # BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED
        undef,                                 # BGP_EVENT_TRANSPORT_FATAL_ERROR
        \&_handle_bgp_start_event              # BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED
    ],
    [                                          # OpenSent
        \&_handle_bgp_fsm_error,               # Default transition
        \&_ignore_start_event,                 # BGP_EVENT_START
        \&_cease,                              # BGP_EVENT_STOP
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN
        \&_handle_open_sent_disconnect,        # BGP_EVENT_TRANSPORT_CONN_CLOSED
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED
        \&_close_session,                      # BGP_EVENT_TRANSPORT_FATAL_ERROR
        undef,                                 # BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED
        \&_handle_hold_timer_expired,          # BGP_EVENT_HOLD_TIMER_EXPIRED
        undef,                                 # BGP_EVENT_KEEPALIVE_TIMER_EXPIRED
        \&_handle_bgp_open_received,           # BGP_EVENT_RECEIVE_OPEN_MESSAGE
        undef,                                 # BGP_EVENT_RECEIVE_KEEP_ALIVE_MESSAGE
        undef,                                 # BGP_EVENT_RECEIVE_UPDATE_MESSAGE
        \&_handle_receive_notification_message,# BGP_EVENT_RECEIVE_NOTIFICATION_MESSAGE
    ],
    [                                          # OpenConfirm
        \&_handle_bgp_fsm_error,               # Default transition
        \&_ignore_start_event,                 # BGP_EVENT_START
        \&_cease,                              # BGP_EVENT_STOP
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN
        \&_close_session,                      # BGP_EVENT_TRANSPORT_CONN_CLOSED
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED
        \&_close_session,                      # BGP_EVENT_TRANSPORT_FATAL_ERROR
        undef,                                 # BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED
        \&_handle_hold_timer_expired,          # BGP_EVENT_HOLD_TIMER_EXPIRED
        \&_handle_keepalive_expired,           # BGP_EVENT_KEEPALIVE_TIMER_EXPIRED
        undef,                                 # BGP_EVENT_RECEIVE_OPEN_MESSAGE
        \&_handle_receive_keepalive_message,   # BGP_EVENT_RECEIVE_KEEP_ALIVE_MESSAGE
        undef,                                 # BGP_EVENT_RECEIVE_UPDATE_MESSAGE
        \&_handle_receive_notification_message,# BGP_EVENT_RECEIVE_NOTIFICATION_MESSAGE
        \&_handle_receive_refresh_message      # BGP_EVENT_RECEIVE_REFRESH_MESSAGE
    ],
    [                                          # Established
        \&_handle_bgp_fsm_error,               # Default transition
        \&_ignore_start_event,                 # BGP_EVENT_START
        \&_cease,                              # BGP_EVENT_STOP
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN
        \&_close_session,                      # BGP_EVENT_TRANSPORT_CONN_CLOSED
        undef,                                 # BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED
        \&_close_session,                      # BGP_EVENT_TRANSPORT_FATAL_ERROR
        undef,                                 # BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED
        \&_handle_hold_timer_expired,          # BGP_EVENT_HOLD_TIMER_EXPIRED
        \&_handle_keepalive_expired,           # BGP_EVENT_KEEPALIVE_TIMER_EXPIRED
        undef,                                 # BGP_EVENT_RECEIVE_OPEN_MESSAGE
        \&_handle_receive_keepalive_message,   # BGP_EVENT_RECEIVE_KEEP_ALIVE_MESSAGE
        \&_handle_receive_update_message,      # BGP_EVENT_RECEIVE_UPDATE_MESSAGE
        \&_handle_receive_notification_message,# BGP_EVENT_RECEIVE_NOTIFICATION_MESSAGE
        \&_handle_receive_refresh_message      # BGP_EVENT_RECEIVE_REFRESH_MESSAGE
    ]
);

## Module Imports ##

use Scalar::Util qw( weaken );
use Errno qw( EINPROGRESS ENOTCONN );
use IO::Async::Handle;
use IO::Async::Timer::Countdown;
use IO::Socket;
use Carp qw( cluck croak );
use Net::BGP::Notification qw( :errors );
use Net::BGP::Refresh;
use Net::BGP::Update;

## Generic Subroutines ##

# This subroutine was snicked from David Town's excellent Net::SNMP
# module and renamed as dump_hex(). Removed class dependence and made
# into standalone subroutine.

sub dump_hex
{
   my $data = shift();
   my ($length, $offset, $line, $hex) = (0, 0, '', '');
   my $string;

   $string = '';
   $length = length($data);

   while ($length > 0) {
      if ($length >= 16) {
         $line = substr($data, $offset, 16);
      } else {
         $line = substr($data, $offset, $length);
      }
      $hex  = unpack('H*', $line);
      $hex .= ' ' x (32 - length($hex));
      $hex  = sprintf("%s %s %s %s  " x 4, unpack('a2' x 16, $hex));
      $line =~ s/[\x00-\x1f\x7f-\xff]/./g;
      $string .= sprintf("[%03d]  %s %s\n", $offset, uc($hex), $line);
      $offset += 16;
      $length -= 16;
   }

   return ( $string );
}

## Public Class Methods ##

sub new
{
    my $class = shift();
    my ($arg, $value);

    my $this = {
	_parent                => undef,
        _sibling               => undef,
        _bgp_version           => BGP_VERSION_4,
        _fsm_state             => BGP_STATE_IDLE,
	_peer_refresh          => FALSE,
        _peer_as4              => FALSE,
        _peer_mbgp             => FALSE,
        _peer_announced_id     => undef,
        _event_queue           => [],
        _message_queue         => [],
        _hold_time             => BGP_HOLD_TIME,
        _hold_timer            => undef,
        _keep_alive_time       => BGP_KEEPALIVE_TIME,
        _keep_alive_timer      => undef,
        _connect_retry_time    => BGP_CONNECT_RETRY_TIME,
        _connect_retry_timer   => undef
    };

    $class->SUPER::new();
    bless($this, $class);

    while ( defined($arg = shift()) ) {
        $value = shift();

        if ( $arg =~ /start/i ) {
            $this->_start();
        }
        elsif ( $arg =~ /parent/i ) {
            $this->{_parent} = $value;
        }
        elsif ( $arg =~ /holdtime/i ) {
            $this->{_hold_time} = $value;
        }
        elsif ( $arg =~ /connectretrytime/i ) {
            $this->{_connect_retry_time} = $value;
        }
        elsif ( $arg =~ /keepalivetime/i ) {
            $this->{_keep_alive_time} = $value;
        }
        else {
            croak "unrecognized argument $arg\n";
        }
    }

    $this->{_connect_retry_timer} = $this->_init_timer("_connect_retry_timer", $this->{_connect_retry_time}, BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED);
    $this->{_hold_timer} = $this->_init_timer("_hold_timer", $this->{_hold_time}, BGP_EVENT_HOLD_TIMER_EXPIRED);
    $this->{_keep_alive_timer} = $this->_init_timer("_keep_alive_timer", $this->{_keep_alive_time}, BGP_EVENT_KEEPALIVE_TIMER_EXPIRED);

    return ( $this );
}

## Public Object Methods ##

sub _start
{
    my $this = shift();
    $this->_enqueue_event(BGP_EVENT_START);
#    $this->_handle_pending_events();
}

sub stop
{
    my $this = shift();
    $this->_enqueue_event(BGP_EVENT_STOP);
    $this->_handle_pending_events();
}

sub version
{
    return shift->{_bgp_version};
}

sub is_established
{
    return ( (shift->{_fsm_state} == BGP_STATE_ESTABLISHED) ? 1 : 0 );
}

sub can_refresh
{
    return shift->{_peer_refresh};
}

sub can_as4
{
    return shift->{_peer_as4};
}

sub can_mbgp
{
    return shift->{_peer_mbgp};
}

sub update
{
    my ($this, $update) = @_;

    my $result = FALSE;
    if ( $this->{_fsm_state} == BGP_STATE_ESTABLISHED ) {

        my $encoded = $update->_encode_message( { as4 => $this->{_peer_as4} } );

        my $buffer = $this->_encode_bgp_update_message($encoded);
        $this->write($buffer);
        $result = TRUE;
    }

    return $result;
}

sub refresh
{
    my $this = shift;

    my ($refresh) = @_;
    $refresh = Net::BGP::Refresh->new(@_) unless ref $refresh eq 'Net::BGP::Refresh';

    my $result = FALSE;
    if (( $this->{_fsm_state} == BGP_STATE_ESTABLISHED ) && $this->{_peer_refresh}) {
        my $buffer = $this->_encode_bgp_refresh_message($refresh->_encode_message());
        $this->write($buffer);
        $result = TRUE;
    }

    return $result;
}

sub sibling
{
    my $this = shift();
    return undef unless defined $this->{_sibling};
    return undef unless $this->_parent->transport eq $this;
    return $this->{_sibling};
}

## Overrides ##

sub on_read
{
    my ($this, $buffer_ref, $conn_closed) = @_;
    my ($type, $message, $read_complete_message);
    my $buffer = ${ $buffer_ref };

    $this->_debug("on_read", sprintf("buffer-bytes: %d", length($buffer)));

    if ( length($buffer) >= BGP_MESSAGE_HEADER_LENGTH ) {
        ($type, $message) = $this->_decode_bgp_message_header($buffer);

        if ( defined($type) ) {
            substr(${ $buffer_ref }, 0, length($message) + BGP_MESSAGE_HEADER_LENGTH, "");
            $this->_enqueue_message($message);
            $this->_enqueue_event($BGP_EVENT_MESSAGE_MAP[$type]);
            $this->_handle_pending_events();
            $read_complete_message = TRUE;
        }
        else {
            $read_complete_message = FALSE;
        }
    }
    else {
        $read_complete_message = FALSE;
    }

    if ( $conn_closed ) {
        $this->_enqueue_event(BGP_EVENT_TRANSPORT_CONN_CLOSED);
        $this->_handle_pending_events();
    }

    return $read_complete_message;
}

sub on_read_eof
{
    my $this = shift();
    $this->_debug("on_read_eof");
}

sub on_write_eof
{
    my $this = shift();
    $this->_debug("on_write_eof");
}

sub on_closed
{
    my $this = shift();
    $this->_debug("on_closed");
    $this->loop()->remove($this);
}

## Private Class Methods ##

sub _init_timer
{
    my ($this, $timer, $timeout, $event) = @_;

    return IO::Async::Timer::Countdown->new(
        delay => $timeout,
        on_expire => sub {
            $this->{$timer}->reset();
            $this->{$timer}->stop();
            $this->_enqueue_event($event);
            $this->_handle_pending_events();
        }
    );
}

sub _restart_timer
{
    my ($this, $timer) = @_;

    $this->{$timer}->reset();
    $this->{$timer}->stop();
    $this->{$timer}->start();
}

sub _parent
{
    return shift->{_parent};
}

sub _clone
{
    my $this = shift();

    $this->_debug("_clone");

    my $transport = Net::BGP::Transport->new(
        Start            => ($this->{_fsm_state} != BGP_STATE_IDLE),
        Parent           => $this->{_parent},
        ConnectRetryTime => $this->{_connect_retry_time},
        HoldTime         => $this->{_hold_time},
        KeepAliveTime    => $this->{_keep_alive_time}
    );

    $this->{_connect_retry_timer} = $this->_init_timer("_connect_retry_timer", $this->{_connect_retry_time}, BGP_EVENT_CONNECT_RETRY_TIMER_EXPIRED);
    $this->{_hold_timer} = $this->_init_timer("_hold_timer", $this->{_hold_time}, BGP_EVENT_HOLD_TIMER_EXPIRED);
    $this->{_keep_alive_timer} = $this->_init_timer("_keep_alive_timer", $this->{_keep_alive_time}, BGP_EVENT_KEEPALIVE_TIMER_EXPIRED);

    $this->{_sibling} = $transport;
    $transport->{_sibling} = $this;

    return ( $transport );
}

## Private Object Methods ##

sub _log_this
{
    my $this = shift();
    return sprintf("peer: %s, transport: %s", $this->{_parent}, $this);
}

sub _info
{
    my ($this, $log_line) = @_;
    $log->info(sprintf("{ %s, message: \"%s\" }", $this->_log_this(), $log_line));
}

sub _debug
{
    my ($this, $function, $log_line) = @_;
    $log->debug(sprintf("%s(%s): { %s }", $function, $this->_log_this(), defined($log_line) ? $log_line : ""));
}

## This creates AND throws a ::Notification object.
sub _error
{
    my $this = shift();

    Net::BGP::Notification->throw(
        ErrorCode    => shift(),
        ErrorSubCode => shift() || BGP_ERROR_SUBCODE_NULL,
        ErrorData    => shift()
    );
}

sub _connected
{
    my ($this, $stream) = @_;

    $this->configure(transport => $stream);
    $this->_enqueue_event(BGP_EVENT_TRANSPORT_CONN_OPEN);
    $this->_handle_pending_events();
}

sub _is_connected
{
    my $this = shift();
    return ( $this->read_handle()->connected() );
}

sub _enqueue_event
{
    my $this = shift();
    unshift(@{ $this->{_event_queue} }, shift());
}

sub _dequeue_event
{
    my $this = shift();
    return ( shift(@{ $this->{_event_queue} }) );
}

sub _enqueue_message
{
    my $this = shift();
    unshift(@{ $this->{_message_queue} }, shift());
}

sub _dequeue_message
{
    my $this = shift();
    return ( shift(@{ $this->{_message_queue} }) );
}

sub _handle_event
{
    my ($this, $event) = @_;

    my $state = my $next_state = $this->{_fsm_state};
    my $callback;

    $this->_debug("_handle_event", sprintf("event: \"%s\", this-state: \"%s\"", $BGP_EVENTS[$event], $BGP_STATES[$state]));

    my $action =
           $BGP_FSM[$state]->[$event]
        || $BGP_FSM[$state]->[0] ## default action
        || undef ;

    eval {
        ($next_state, $callback) = $action->($this) if defined $action;
        $this->_debug("_handle_event", sprintf("next-state: \"%s\"", $BGP_STATES[$next_state]));
    };
    if (my $oops = $@)
    {
        if (UNIVERSAL::isa($oops, 'Net::BGP::Notification'))
        {
            ($next_state, $callback) = $this->_kill_session($oops);
        }
        else
        {
            die $oops;
        }
    }

    # transition to next state
    $this->{_fsm_state} = $next_state if defined $next_state;
    &{ $callback } if defined $callback;

    ## trigger callbacks if we changed states
    if ($next_state != $state)
    {
        if ( $state == BGP_STATE_ESTABLISHED )
        {
            ## session has terminated
            ##
            # It is ok to invoke a callback directly here because we are outside of the FSM at this point
            $this->_parent->reset_callback(undef)
        }
        elsif ( $next_state == BGP_STATE_ESTABLISHED )
        {
            ## newly established session
            ##
            # It is ok to invoke a callback directly here because we are outside of the FSM at this point
            $this->_parent->refresh_callback(undef);
        }

        # trigger post-transition actions
        $this->_trigger_post_transition_action($state, $next_state);
    }
}

sub _trigger_post_transition_action
{
    my ($this, $pre_state, $pos_state) = @_;

    # TODO:
    #
    # This needs to be broken out into a separate table similar to $BGP_FSM
    # which triggers actions prior to state transition. Or, alternately,
    # $BGP_FSM could be augmented to have an array of subrefs, one each for the
    # pre- and post- transition action, rather than the current scalar subref.
    # But I'm too lazy to refactor the entire table right now, so just handle
    # the single current use case of firing the ESTABLISHED callback...

    if (($pre_state == BGP_STATE_OPEN_CONFIRM) && ($pos_state == BGP_STATE_ESTABLISHED)) {
        # It is ok to invoke a callback directly here because we are outside of the FSM at this point
        $this->_parent->established_callback();
    }
}

sub _handle_pending_events
{
    my $this = shift();
    my $event;

    while ( defined($event = $this->_dequeue_event()) ) {
        $this->_handle_event($event);
    }
}

sub _close_session
{
    my $this = shift();

    $this->_debug("_close_session");

    if ( defined($this->transport()) ) {
        $this->transport()->close_when_empty();
    }

    $this->{_hold_timer}->stop();
    $this->{_keep_alive_timer}->stop();
    $this->{_connect_retry_timer}->stop();
    $this->{_message_queue} = [];

    return ( BGP_STATE_IDLE );
}

sub _kill_session
{
    my ($this, $error) = @_;
    my $buffer;

    $buffer = $this->_encode_bgp_notification_message(
        $error->error_code(),
        $error->error_subcode(),
        $error->error_data()
    );

    $this->write($buffer);

    # invoke user callback function on return
    return ( $this->_close_session(), sub { $this->_parent->error_callback($error); } );
}

sub _ignore_start_event
{
    my $this = shift();
    return ( $this->{_fsm_state} );
}

sub _handle_receive_keepalive_message
{
    my $this = shift();

    $this->_debug("_handle_receive_keepalive_message");

    # restart Hold Timer
    if ( $this->{_hold_time} != 0 ) {
        $this->{_hold_timer}->reset();
    }

    # invoke user callback function on return
    return ( BGP_STATE_ESTABLISHED, sub { $this->_parent->keepalive_callback(); } );
}

sub _handle_receive_update_message
{
    my $this = shift();
    my ($buffer, $update);

    # restart Hold Timer
    if ( $this->{_hold_time} != 0 ) {
        $this->{_hold_timer}->reset();
    }

    $buffer = $this->_dequeue_message();
    $update = Net::BGP::Update->_new_from_msg(
        $buffer,
        { as4 => $this->{_peer_as4} }
    );

    # invoke user callback function on return
    return ( BGP_STATE_ESTABLISHED, sub { $this->_parent->update_callback($update); } );
}

sub _handle_receive_refresh_message
{
    my $this = shift();
    my ($buffer, $refresh);

    # restart Hold Timer
    if ( $this->{_hold_time} != 0 ) {
        $this->{_hold_timer}->reset();
    }

    $buffer = $this->_dequeue_message();
    $refresh = Net::BGP::Refresh->_new_from_msg($buffer);

    unless ( $this->_parent->this_can_refresh ) {
        $log->error("Received REFRESH message and can_refresh is false!");
        Net::BGP::Notification->throw(
            ErrorCode => BGP_ERROR_CODE_FINITE_STATE_MACHINE
        );
    }

    # invoke user callback function on return
    return ( BGP_STATE_ESTABLISHED, sub { $this->_parent->refresh_callback($refresh); } );
}

sub _handle_receive_notification_message
{
    my $this = shift();
    my $error;

    $error = $this->_decode_bgp_notification_message($this->_dequeue_message());
    $this->_close_session();

    # invoke user callback function on return
    return ( BGP_STATE_IDLE, sub { $this->_parent->notification_callback($error); } );
}

sub _handle_keepalive_expired
{
    my $this = shift();
    my $buffer;

    # send KEEPALIVE message to peer
    $buffer = $this->_encode_bgp_keepalive_message();
    $this->write($buffer);

    # restart KeepAlive timer
    $this->{_keep_alive_timer}->start();

    return ( $this->{_fsm_state} );
}

sub _handle_hold_timer_expired
{
    my $this = shift();

    $this->_error(BGP_ERROR_CODE_HOLD_TIMER_EXPIRED);
}

sub _handle_bgp_fsm_error
{
    my $this = shift();

    cluck("_handle_bgp_fsm_error()");
    $this->_error(BGP_ERROR_CODE_FINITE_STATE_MACHINE);
}

sub _handle_bgp_conn_open
{
    my $this = shift();
    my $buffer;

    # clear ConnectRetry timer
    $this->{_connect_retry_timer}->reset();
    $this->{_connect_retry_timer}->stop();

    # send OPEN message to peer
    $buffer = $this->_encode_bgp_open_message();
    $this->write($buffer);

    return ( BGP_STATE_OPEN_SENT );
}

sub _handle_collision_selfdestuct
{
    my ($this, $not_connected) = @_;
    $log->debug("Connection collision ... closing this connection: $this");

    if ( $not_connected ) {
        $this->_close_session();
    }
    else {
        $this->stop();
    }

    $this->_parent->transport($this->{_sibling});
    $this->{_sibling}->{_sibling} = undef;
}

sub _handle_bgp_open_received
{
    my $this = shift();
    my ($buffer, $this_id, $peer_id);

    $this->_debug("_handle_bgp_open_received");

    if ( ! $this->_decode_bgp_open_message($this->_dequeue_message()) ) {
        ; # do failure stuff
        return ( BGP_STATE_IDLE );
    }

    # check for connection collision
    if ( defined($this->{_sibling}) ) {
        if ( ($this->{_sibling}->{_fsm_state} == BGP_STATE_OPEN_SENT) ||
             ($this->{_sibling}->{_fsm_state} == BGP_STATE_OPEN_CONFIRM) ) {

            $this_id = unpack('N', inet_aton($this->_parent->this_id));
            $peer_id = unpack('N', inet_aton($this->_parent->peer_id));

            if ( $this_id < $peer_id ) {
		$this->_handle_collision_selfdestuct;
                return ( BGP_STATE_IDLE );
            }
            else {
                $this->{_sibling}->_handle_collision_selfdestuct;
            }
        }
        elsif ( ($this->{_sibling}->{_fsm_state} == BGP_STATE_ESTABLISHED) ) {
            $this->_handle_collision_selfdestuct;
            return ( BGP_STATE_IDLE );
        }
	else { # Other in Idle, conect, active
          $this->{_sibling}->_handle_collision_selfdestuct(TRUE);
	}
    }

    # clear the message buffer after decoding and validation
    $this->{_message} = undef;

    # send KEEPALIVE message to peer
    $buffer = $this->_encode_bgp_keepalive_message();
    $this->write($buffer);

    # set Hold Time and KeepAlive timers
    if ( $this->{_hold_time} != 0 ) {
        $this->{_hold_timer}->start();
        $this->{_keep_alive_timer}->start();
    }

    # transition to state OpenConfirm and invoke user callback function on return
    return ( BGP_STATE_OPEN_CONFIRM, sub { $this->_parent->open_callback(); } );
}

sub _handle_open_sent_disconnect
{
    my $this = shift();

    $this->_close_session();
    return ( $this->_handle_connect_retry_restart() );
}

sub _handle_connect_retry_restart
{
    my $this = shift();

    $this->_debug("_handle_connect_retry_restart");

    # restart ConnectRetry timer
    $this->_restart_timer("_connect_retry_timer");

    return ( BGP_STATE_ACTIVE );
}

sub _handle_bgp_start_event
{
    my $this = shift();

    $this->_debug("_handle_bgp_start_event");

    # initialize the TCP transport connection and ConnectRetry timer
    if ( ! $this->_parent()->is_passive() ) {

        $this->{_connect_retry_timer}->start();

        $this->connect(

            addr => {
                family   => 'inet',
                socktype => 'stream',
                port     => $this->_parent()->peer_port(),
                ip       => $this->_parent()->peer_id()
            },

            on_connected => sub {
                $this->_info(sprintf("Active connection established with %s:%d",
                    $this->transport()->read_handle()->peerhost(),
                    $this->transport()->read_handle()->peerport()));
                $this->_enqueue_event(BGP_EVENT_TRANSPORT_CONN_OPEN);
                $this->_handle_pending_events();
            },

            on_connect_error => sub {
                $this->_enqueue_event(BGP_EVENT_TRANSPORT_CONN_OPEN_FAILED);
                $this->_handle_pending_events();
            }

        );
    }

    return ( BGP_STATE_CONNECT );
}

sub _min
{
    my ($a, $b) = @_;
    return ( ($a < $b) ? $a : $b );
}

sub _cease
{
    my $this = shift();
    return $this->_kill_session(Net::BGP::Notification->new( ErrorCode => BGP_ERROR_CODE_CEASE ));
}

sub _encode_bgp_message
{
    my ($this, $type, $payload) = @_;
    my ($buffer, $length);

    $buffer = '';
    $length = BGP_MESSAGE_HEADER_LENGTH;

    if ( defined($payload) ) {
       $length += length($payload);
       $buffer = $payload;
    }

    # encode the type field
    $buffer = pack('C', $type) . $buffer;

    # encode the length field
    $buffer = pack('n', $length) . $buffer;

    # encode the marker field
    if ( defined($this->{_auth_data}) ) {
        $buffer = $this->{_auth_data} . $buffer;
    }
    else {
        $buffer = (pack('C', 0xFF) x 16) . $buffer;
    }

    return ( $buffer );
}

sub _decode_bgp_message_header
{
    my ($this, $header) = @_;
    my ($marker, $length, $type);

    # validate the BGP message header length
    if ( length($header) < BGP_MESSAGE_HEADER_LENGTH ) {
        $this->_error(
            BGP_ERROR_CODE_MESSAGE_HEADER,
            BGP_ERROR_SUBCODE_BAD_MSG_LENGTH,
            pack('n', length($header))
        );
    }

    # decode and validate the message header Marker field
    $marker = substr($header, 0, 16);
    if ( $marker ne (pack('C', 0xFF) x 16) ) {
        $this->_error(BGP_ERROR_CODE_MESSAGE_HEADER,
                      BGP_ERROR_SUBCODE_CONN_NOT_SYNC);
    }

    # decode and validate the message header Length field
    $length = unpack('n', substr($header, 16, 2));
    if ( ($length < BGP_MESSAGE_HEADER_LENGTH) || ($length > BGP_MAX_MESSAGE_LENGTH) ) {
        $this->_error(
            BGP_ERROR_CODE_MESSAGE_HEADER,
            BGP_ERROR_SUBCODE_BAD_MSG_LENGTH,
            pack('n', $length)
        );
    }

    # decode and validate the message header Type field
    $type = unpack('C', substr($header, 18, 1));
    if ( ($type < BGP_MESSAGE_OPEN) || ($type > BGP_MESSAGE_REFRESH) ) {
        $this->_error(
            BGP_ERROR_CODE_MESSAGE_HEADER,
            BGP_ERROR_SUBCODE_BAD_MSG_TYPE,
            pack('C', $type)
        );
    }

    if ( length($header) >= $length ) {
        return ($type, substr($header, BGP_MESSAGE_HEADER_LENGTH, $length - BGP_MESSAGE_HEADER_LENGTH));
    }

    # More data needed for complete message
    return undef;
}

sub _encode_bgp_open_message
{
    my $this = shift();
    my ($buffer, $length);

    # encode optional parameters and length
    my $opt = '';

    if ($this->_parent->support_capabilities) {

        if ( defined($this->{_peer_announced_id}) ) {
            # We received an open from the other end

            if ($this->{_peer_mbgp}) {
                $opt .= $this->_encode_capability_mbgp();
            }

            if ($this->{_peer_as4}) {
                $opt .= $this->_encode_capability_as4();
            }

        }  else {
            # We are sending the open

            if ( $this->_parent->support_mbgp ) {
                $opt .= $this->_encode_capability_mbgp();
            }
            if ( $this->_parent->this_can_as4 ) {
                $opt .= $this->_encode_capability_as4();
            }

        }

        # Both the standard (2) and Cisco (128) capabilities are sent
        if ($this->_parent->this_can_refresh) {
            $opt .= $this->_encode_capability(BGP_CAPABILITY_REFRESH, '');
            $opt .= $this->_encode_capability(BGP_CAPABILITY_REFRESH_OLD, '');
        }
    }

    $buffer = pack('C', length($opt)) . $opt;

    # encode BGP Identifier field
    $buffer = inet_aton($this->_parent->this_id) . $buffer;

    # encode Hold Time
    $buffer = pack('n', $this->{_hold_time}) . $buffer;

    # encode local Autonomous System number
    if ($this->_parent->this_as > 65535) {
        $buffer = pack('n', 23456) . $buffer;
    } else {
        $buffer = pack('n', $this->_parent->this_as) . $buffer;
    }

    # encode BGP version
    $buffer = pack('C', $this->{_bgp_version}) . $buffer;

    return ( $this->_encode_bgp_message(BGP_MESSAGE_OPEN, $buffer) );
}

sub _encode_capability_mbgp
{
    my $this = shift;

    # Capability 1 with data of:
    # Address family 1 (IPv4), reserved bit 0, type 1 (unicast)
    my $cap = pack('ncc', 1, 0, 1);
    my $opt = $this->_encode_capability(BGP_CAPABILITY_MBGP, $cap);

    return $opt;
}

sub _encode_capability_as4
{
    my $this = shift;

    # Capability 65 with data of the ASN
    my $cap = pack('N', $this->_parent->this_as());
    my $opt = $this->_encode_capability(BGP_CAPABILITY_AS4, $cap);

    return $opt;
}

# Encodes a capability (inside the capability option)
# RFC5492
# Format is <2> <capability_len> <cap_code> <data_len>
sub _encode_capability
{
    my ($this, $type, $data) = @_;

    my $opt = '';
    $opt .= pack('C', BGP_OPTION_CAPABILITIES);    # Option Type

    my $cap = '';
    $cap .= pack('C', $type);                       # Capability Type
    $cap .= pack('C', length($data));               # Capability Data Len
    $cap .= $data;                                  # Capability data

    $opt .= pack('C', length($cap));                # Option Data Len
    $opt .= $cap;

    return $opt;
}

sub _decode_bgp_open_message
{
    my ($this, $buffer) = @_;
    my ($version, $as, $hold_time, $bgp_id);

    # decode and validate BGP version
    $version = unpack('C', substr($buffer, 0, 1));
    if ( $version != BGP_VERSION_4 ) {
        $this->_error(
            BGP_ERROR_CODE_OPEN_MESSAGE,
            BGP_ERROR_SUBCODE_BAD_VERSION_NUM,
            pack('n', BGP_VERSION_4)
        );
    }

    # decode and validate remote Autonomous System number
    $as = unpack('n', substr($buffer, 1, 2));
    if ( $as != $this->_parent->peer_as ) {
        if ($this->_parent->peer_as < 65536) {
            $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
                          BGP_ERROR_SUBCODE_BAD_PEER_AS);
        } elsif ($as != 23456) {
            $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
                          BGP_ERROR_SUBCODE_BAD_PEER_AS);
        }
    }

    # decode and validate received Hold Time
    $hold_time = _min(unpack('n', substr($buffer, 3, 2)), $this->{_hold_time});
    if ( ($hold_time < 3) && ($hold_time != 0) ) {
        $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
            BGP_ERROR_SUBCODE_BAD_HOLD_TIME);
    }

    # decode received BGP Identifier
    # Spelling error is retained for compatibility.
    $this->{_peer_annonced_id} = inet_ntoa(substr($buffer, 5, 4));
    $this->{_peer_announced_id} = inet_ntoa(substr($buffer, 5, 4));

    # decode known Optional Parameters
    my $opt_length = unpack('c', substr($buffer, 9, 1));
    my $opt = substr($buffer, 10, $opt_length);
    while ($opt ne '')
     {
      my ($type, $length) = unpack('cc', substr($opt, 0, 2));
      my $value = substr($opt, 2, $length);
      if ($type eq BGP_OPTION_CAPABILITIES)
       {
        $this->_decode_capabilities($value);
       }
      else
       { # Unknown optional parameter!
         # XXX We should send a notify here.
       }
      $opt = substr($opt, 2+$length);
     };

    # set Hold Time to negotiated value
    $this->{_hold_time} = $hold_time;

    # indicate decoding and validation success
    return ( TRUE );
}

# Capabilities we don't understand get ignored.
sub _decode_capabilities
{
    my ($this, $value) = @_;

    $this->{'_peer_refresh'} = TRUE;

    while (length($value) > 0) {

        if (length($value) < 2) {
            $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
                BGP_ERROR_SUBCODE_BAD_OPT_PARAMETER);
            return;
        }

        my ($type, $len) = unpack('cc', substr($value, 0, 2));
        my $data = substr($value, 2, $len);

        $this->_decode_one_capability($type, $len, $data);

        $value = substr($value, 2+$len);
    }

}

sub _decode_one_capability {
    my ($this, $type, $len, $data) = @_;

    if (length($data) != $len) {
        $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
            BGP_ERROR_SUBCODE_BAD_OPT_PARAMETER);
    }

    if ($type == BGP_CAPABILITY_MBGP) {
        $this->{_peer_mbgp} = TRUE;
    }

    if ($type == BGP_CAPABILITY_REFRESH) {
        $this->{_peer_refresh} = TRUE;
    }
    if ($type == BGP_CAPABILITY_REFRESH_OLD) {
        $this->{_peer_refresh} = TRUE;
    }

    if ($type == BGP_CAPABILITY_AS4) {

        if ($len != 4) {
            $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
                BGP_ERROR_SUBCODE_BAD_OPT_PARAMETER);
        }
   
        my $as = unpack('N', $data); 
        if ($as != $this->_parent->peer_as) {
            $this->_error(BGP_ERROR_CODE_OPEN_MESSAGE,
                BGP_ERROR_SUBCODE_BAD_PEER_AS);
        }

        # Both ends must support this. 
        if ( $this->_parent->this_can_as4 ) {
            $this->{_peer_as4} = TRUE;
        }
    }

}

sub _decode_bgp_notification_message
{
    my ($this, $buffer) = @_;
    my ($error, $error_code, $error_subcode, $data);

    # decode and validate Error code
    $error_code = unpack('C', substr($buffer, 0, 1));
    if ( ($error_code < 1) || ($error_code > 6) ) {
        die("_decode_bgp_notification_message(): invalid error code = $error_code\n");
    }

    # decode and validate Error subcode
    $error_subcode = unpack('C', substr($buffer, 1, 1));
    if ( ($error_subcode < 0) || ($error_subcode > 11) ) {
        die("_decode_bgp_notification_message(): invalid error subcode = $error_subcode\n");
    }

    # decode Data field
    $data = substr($buffer, 2, length($buffer) - 2);

    return Net::BGP::Notification->new(
        ErrorCode => $error_code,
        ErrorSubcode => $error_subcode,
        ErrorData => $data);
}

sub _encode_bgp_keepalive_message
{
    my $this = shift();
    return ( $this->_encode_bgp_message(BGP_MESSAGE_KEEPALIVE) );
}

sub _encode_bgp_update_message
{
    my ($this, $buffer) = @_;
    return ( $this->_encode_bgp_message(BGP_MESSAGE_UPDATE, $buffer) );
}

sub _encode_bgp_refresh_message
{
    my ($this, $buffer) = @_;
    return ( $this->_encode_bgp_message(BGP_MESSAGE_REFRESH, $buffer) );
}

sub _encode_bgp_notification_message
{
    my ($this, $error_code, $error_subcode, $data) = @_;
    my $buffer;

    # encode the Data field
    $buffer = $data ? $data : '';

    # encode the Error Subcode field
    $buffer = pack('C', $error_subcode) . $buffer;

    # encode the Error Code field
    $buffer = pack('C', $error_code) . $buffer;

    return ( $this->_encode_bgp_message(BGP_MESSAGE_NOTIFICATION, $buffer) );
}

## POD ##

=pod

=head1 NAME

Net::BGP::Transport - Class encapsulating BGP-4 transport session state and functionality

=head1 SYNOPSIS

    use Net::BGP::Transport;

    $trans = Net::BGP::Transport->new(
        Start                => 1,
        Parent               => Net::BGP::Peer->new(),
        ConnectRetryTime     => 300,
        HoldTime             => 60,
        KeepAliveTime        => 20
    );

    $version = $trans->version();
    $established = $trans->is_established();

    $trans->start();
    $trans->stop();

    $trans->update($update);
    $trans->refresh($refresh);

=head1 DESCRIPTION

This module encapsulates the state and functionality associated with a BGP
transport connection. Each instance of a Net::BGP::Transport object
corresponds to a TCP session with a distinct peer. It should not be used by
itself, but encapsulated in a Net::BGP::Peer object.

=head1 CONSTRUCTOR

=over 4

=item new() - create a new Net::BGP::Transport object

This is the constructor for Net::BGP::Transport objects. It returns a
reference to the newly created object. The following named parameters may
be passed to the constructor. Once the object is created, the information
can not be changed.

=over 4

=item Start

=item ConnectRetryTime

=item HoldTime

=item KeepAliveTime

Has the same meaning as the equivalent named argument for Net::BGP::Peer.

=item Parent

The parent Net::BGP::Peer object.

=back

=back

=head1 ACCESSOR METHODS

=over 4

=item version()

=item is_established()

=item start()

=item stop()

=item update()

=item refresh()

These methods do the actual I<work> for the methods of the same name in
Net::BGP::Peer.

=back

=head1 SEE ALSO

Net::BGP::Peer, Net::BGP, Net::BGP::Update, Net::BGP::Refresh

=head1 AUTHOR

Stephen J. Scheck <sscheck@cpan.org> in original Peer.pm form
Martin Lorensen <lorensen@cpan.org> separated into Transport.pm

=cut

## End Package Net::BGP::Transport ##

1;
