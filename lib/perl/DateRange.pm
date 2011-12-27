##############################################################################
#
#   File Name    - DateRange.pm
#
#   Description  - The date range module for the mtn-browse application. This
#                  module contains all the routines for implementing the
#                  assorted date/time range widgets used in a number of
#                  windows.
#
#   Author       - A.E.Cooper.
#
#   Legal Stuff  - Copyright (c) 2007 Anthony Edward Cooper
#                  <aecooper@coosoft.plus.com>.
#
#                  This program is free software; you can redistribute it
#                  and/or modify it under the terms of the GNU General Public
#                  License as published by the Free Software Foundation;
#                  either version 3 of the License, or (at your option) any
#                  later version.
#
#                  This program is distributed in the hope that it will be
#                  useful, but WITHOUT ANY WARRANTY; without even the implied
#                  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#                  PURPOSE. See the GNU General Public License for more
#                  details.
#
#                  You should have received a copy of the GNU General Public
#                  License along with this software; if not, write to the Free
#                  Software Foundation, Inc., 59 Temple Place - Suite 330,
#                  Boston, MA 02111-1307 USA.
#
##############################################################################
#
##############################################################################
#
#   Global Data For This Module
#
##############################################################################



# ***** DIRECTIVES *****

require 5.008005;

use locale;
use strict;
use warnings;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub get_date_range($$$);
sub set_date_range($$$);
sub setup_date_range_widgets($);

# Private routines.

sub between_range_radiobutton_toggled_cb($$);
sub date_range_checkbutton_toggled_cb($$);
#
##############################################################################
#
#   Routine      - date_range_checkbutton_toggled_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the date range check button in the window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub date_range_checkbutton_toggled_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    # Simply enable and disable the appropriate widgets.

    if ($instance->{date_range_checkbutton}->get_active())
    {
	for my $widget (@{$instance->{date_sensitive_group}})
	{
	    $widget->set_sensitive(TRUE);
	}
	if ($instance->{between_range_radiobutton}->get_active())
	{
	    for my $widget (@{$instance->{during_dates_sensitive_group}})
	    {
		$widget->set_sensitive(FALSE);
	    }
	}
	else
	{
	    for my $widget (@{$instance->{between_dates_sensitive_group}})
	    {
		$widget->set_sensitive(FALSE);
	    }
	}
    }
    else
    {
	for my $widget (@{$instance->{date_sensitive_group}})
	{
	    $widget->set_sensitive(FALSE);
	}
    }

}
#
##############################################################################
#
#   Routine      - between_range_radiobutton_toggled_cb
#
#   Description  - Callback routine called when the user changes the value of
#                  the between radio button in the window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub between_range_radiobutton_toggled_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    # Simply enable and disable the appropriate widgets.

    if ($instance->{between_range_radiobutton}->get_active())
    {
	for my $widget (@{$instance->{between_dates_sensitive_group}})
	{
	    $widget->set_sensitive(TRUE);
	}
	for my $widget (@{$instance->{during_dates_sensitive_group}})
	{
	    $widget->set_sensitive(FALSE);
	}
    }
    else
    {
	for my $widget (@{$instance->{during_dates_sensitive_group}})
	{
	    $widget->set_sensitive(TRUE);
	}
	for my $widget (@{$instance->{between_dates_sensitive_group}})
	{
	    $widget->set_sensitive(FALSE);
	}
    }

}
#
##############################################################################
#
#   Routine      - get_date_range
#
#   Description  - Retrieves, validates and converts the values specified in
#                  the date range widgets into two Monotone style date time
#                  strings.
#
#   Data         - $instance    : The window instance that is associated with
#                                 these date range widgets.
#                  $from_date   : A reference to a variable that is to contain
#                                 either the Monotone style from date/time
#                                 string or undef if no date range was
#                                 specified.
#                  $to_date     : A reference to a variable that is to contain
#                                 either the Monotone style to date/time
#                                 string or undef in the case of a `during the
#                                 last ...' style date/time range or no date
#                                 range at all.
#                  Return Value : True on success, otherwise false on failure.
#
##############################################################################



sub get_date_range($$$)
{

    my ($instance, $from_date, $to_date) = @_;

    # If no date range was specified then return undef for both dates.

    if (! $instance->{date_range_checkbutton}->get_active())
    {
	$$from_date = $$to_date = undef;
	return 1;
    }

    # Get the user entered date range.

    if ($instance->{between_range_radiobutton}->get_active())
    {

	my ($older_date,
	    $younger_date);

	# Simple `from - to' style date range.

	$older_date = strftime(MTN_TIME_STRING,
			       localtime($instance->{older_date_dateedit}->
					 get_time()));
	$younger_date = strftime(MTN_TIME_STRING,
				 localtime($instance->{younger_date_dateedit}->
					   get_time()));

	# Check that any date range is the right way around.

	if ($older_date ge $younger_date)
	{
	    my $dialog = Gtk2::MessageDialog->new
		($instance->{window},
		 ["modal"],
		 "warning",
		 "close",
		 __("The `between' dates are either\n"
		    . "the same or the wrong way round."));
	    busy_dialog_run($dialog);
	    $dialog->destroy();
	    return;
	}

	# Sort out the return values.

	$$from_date = $older_date;
	$$to_date = $younger_date;

    }
    else
    {

	my ($month,
	    $period,
	    $period_units,
	    @time_val,
	    $year);

	# `In the last ...' style date range.

	# Please note that the update method needs to be called on the
	# spinbutton so as to make sure that it's internal state is completely
	# up to date (the user might have entered a value directly into the
	# entry field). Updates are usually done when it looses the focus,
	# however the parent window may not make use of any focus stealing
	# buttons.

	$instance->{time_spinbutton}->update();
	$period = $instance->{time_spinbutton}->get_value_as_int();
	$period_units = $instance->{time_units_combobox}->get_active();

	# Check that the duration period is not too large.

	@time_val = localtime();
	($month, $year) = (@time_val)[4, 5];
	if ($period_units == DURATION_MONTHS
	    && $period > ((($year - 70) * 12) + $month))
	{
	    my $dialog = Gtk2::MessageDialog->new
		($instance->{window},
		 ["modal"],
		 "warning",
		 "close",
		 __x("A duration of {months} months is too long.",
		     months => $period));
	    busy_dialog_run($dialog);
	    $dialog->destroy();
	    return;
	}
	elsif ($period_units == DURATION_YEARS && $period > ($year - 70))
	{
	    my $dialog = Gtk2::MessageDialog->new
		($instance->{window},
		 ["modal"],
		 "warning",
		 "close",
		 __x("A duration of {years} years is too long.",
		     years => $period));
	    busy_dialog_run($dialog);
	    $dialog->destroy();
	    return;
	}

	# Now work out the actual start date.

	adjust_time(\@time_val, $period, $period_units);

	# Sort out the return values.

	$$from_date = strftime(MTN_TIME_STRING, @time_val);
	$$to_date = undef;

    }

    return 1;

}
#
##############################################################################
#
#   Routine      - set_date_range
#
#   Description  - Sets the values of the date range widgets to the specified
#                  values.
#
#   Data         - $instance    : The window instance that is associated with
#                                 these date range widgets.
#                  $from_date   : The time() style time code for the from
#                                 date.
#                  $to_date     : The time() style time code for the to date.
#
##############################################################################



sub set_date_range($$$)
{

    my ($instance, $from_date, $to_date) = @_;

    # Enable and disable the appropriate widgets.

    $instance->{date_range_checkbutton}->set_active(TRUE);
    $instance->{between_range_radiobutton}->set_active(TRUE);
    for my $widget (@{$instance->{date_sensitive_group}})
    {
	$widget->set_sensitive(TRUE);
    }
    for my $widget (@{$instance->{during_dates_sensitive_group}})
    {
	$widget->set_sensitive(FALSE);
    }

    # Now load in the date/time values into teh dateedit widgets.

    $instance->{older_date_dateedit}->set_time($from_date);
    $instance->{younger_date_dateedit}->set_time($to_date);

}
#
##############################################################################
#
#   Routine      - setup_date_range_widgets
#
#   Description  - Does any window instance initialisation necessary for the
#                  date range widgets.
#
#   Data         - $instance : The window instance that is associated with
#                              these date range widgets.
#
##############################################################################



sub setup_date_range_widgets($)
{

    my $instance = $_[0];

    # Setup widget sensitivity groups.

    $instance->{date_sensitive_group} = [];
    foreach my $widget ("between_range_radiobutton",
			"older_date_dateedit",
			"and_label",
			"younger_date_dateedit",
			"during_range_radiobutton",
			"time_spinbutton",
			"time_units_combobox")
    {
	push(@{$instance->{date_sensitive_group}}, $instance->{$widget});
    }
    $instance->{between_dates_sensitive_group} = [];
    foreach my $widget ("older_date_dateedit",
			"and_label",
			"younger_date_dateedit")
    {
	push(@{$instance->{between_dates_sensitive_group}},
	     $instance->{$widget});
    }
    $instance->{during_dates_sensitive_group} = [];
    foreach my $widget ("time_spinbutton", "time_units_combobox")
    {
	push(@{$instance->{during_dates_sensitive_group}},
	     $instance->{$widget});
    }

    # Setup the comboboxes.

    $instance->{time_units_combobox}->set_active(DURATION_DAYS);

    # Disable the appropriate widgets by default.

    for my $widget (@{$instance->{date_sensitive_group}})
    {
	$widget->set_sensitive(FALSE);
    }

}

1;
