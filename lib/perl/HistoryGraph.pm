##############################################################################
#
#   File Name    - HistoryGraph.pm
#
#   Description  - The history graphing module for the mtn-browse application.
#                  This module contains all the routines for implementing the
#                  history graphing windows.
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
no warnings qw(recursion);

# ***** GLOBAL DATA DECLARATIONS *****

# Constants for the border, scale and size of items in a graph.

use constant CANVAS_BORDER    => 5;
use constant DPI              => 72;
use constant FONT_SIZE        => 8;
use constant HEIGHT           => 28;
use constant LINE_WIDTH       => 2;
use constant SELECTION_BORDER => 5;
use constant TEXT_BORDER      => 7;
use constant WIDTH            => 72;

# Constant for the number of characters displayed for a hexadecimal id.

use constant HEX_ID_LENGTH => 8;

# Constants representing the graph node flags that can be set.

use constant CIRCULAR_NODE => 0x01;
use constant NO_PARENTS    => 0x02;
use constant SELECTED_NODE => 0x04;

# Constants representing certain colours.

use constant NOT_SELECTED_BORDER_COLOUR => "Gray";
use constant SELECTED_BORDER_COLOUR     => "Black";
use constant SELECTION_COLOUR           => "Orange";

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_history_graph($;$$$);

# Private routines.

sub canvas_item_event_cb($$$);
sub default_zoom_button_clicked_cb($$);
sub dot_input_handler_cb($$);
sub draw_graph($);
sub generate_ancestry_graph($$;$$$);
sub get_history_graph_window();
sub get_node_colour($$);
sub graph_reconnect_helper($$);
sub hsv_to_rgb($$$$$$);
sub layout_graph($);
sub populate_revision_details($$);
sub scale_canvas($);
sub scroll_to_node($$);
sub select_node($$);
sub zoom_in_button_clicked_cb($$);
sub zoom_out_button_clicked_cb($$);
#
##############################################################################
#
#   Routine      - display_history_graph
#
#   Description  - Display a history graph for the specified branches within
#                  the specified date range.
#
#   Data         - $mtn       : The Monotone::AutomateStdio object that is to
#                               be used to generate the history graph.
#                  $branches  : A reference to a list of branches to generate
#                               a graph from. This parameter can be undef or
#                               an empty list if all branches are to be
#                               selected.
#                  $from_date : The earliest date from which revisions will be
#                               selected for the history graph. This parameter
#                               can be undef or an empty string if no such age
#                               restriction is required.
#                  $to_date   : The latest date from which revisions will be
#                               selected for the history graph. This parameter
#                               can be undef or an empty string if no such age
#                               restriction is required.
#
##############################################################################



sub display_history_graph($;$$$)
{

    my ($mtn, $branches, $from_date, $to_date) = @_;

    my ($counter,
	$instance,
	@revision_ids);
    my $wm = WindowManager->instance();

    $instance = get_history_graph_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $wm->update_gui();

    foreach my $item (@{$instance->{revision_sensitive_group}})
    {
	$item->set_sensitive(FALSE);
    }
    $instance->{stop_button}->set_sensitive(TRUE);
    $wm->update_gui();

    # Get the list of file change revisions. Remember to include the current
    # revision in the history.

    generate_ancestry_graph($instance, 1, $branches, $from_date, $to_date);

    # Populate all of the graphed nodes with essential revision information.

    if (! $instance->{stop})
    {
	$instance->{appbar}->set_progress_percentage(0);
	$instance->{appbar}->set_status(__("Getting revision information"));
	$wm->update_gui();
	$counter = 1;
	@revision_ids = keys(%{$instance->{graph_data}->{child_graph}});
	foreach my $revision_id (@revision_ids)
	{
	    populate_revision_details($instance, $revision_id);
	    if (($counter % 100) == 0)
	    {
		$instance->{appbar}->set_progress_percentage
		    ($counter / scalar(@revision_ids));
		$wm->update_gui();
	    }
	    ++ $counter;

	    # Stop if the user wants to.

	    last if ($instance->{stop});
	}
	$instance->{appbar}->set_progress_percentage(1);
	$wm->update_gui();
    }

    # Get dot to lay out the history graph.

    if (! $instance->{stop})
    {
	$instance->{appbar}->set_progress_percentage(0);
	$instance->{appbar}->set_status(__("Laying out graph with dot"));
	$wm->update_gui();
	layout_graph($instance);
    }

    # Now draw it in our canvas.

    draw_graph($instance) unless ($instance->{stop});

    # Cleanup any data if we were asked to stop.

    if ($instance->{stop})
    {
	$instance->{colour_db} = {};
	$instance->{graph_data} = undef;
    }

    $instance->{stop_button}->set_sensitive(FALSE);
    $instance->{stop} = 0;
    $instance->{appbar}->set_progress_percentage(0);
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - zoom_in_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the zoom in
#                  button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub zoom_in_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = $instance->{scale} * sqrt(2);
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - zoom_out_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the zoom
#                  out button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub zoom_out_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = $instance->{scale} / sqrt(2);
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - default_zoom_button_clicked_cb
#
#   Description  - Callback routine called when the user clicks on the default
#                  zoom button in the history graph window.
#
#   Data         - $widget   : The widget object that received the signal.
#                  $instance : The window instance that is associated with
#                              this widget.
#
##############################################################################



sub default_zoom_button_clicked_cb($$)
{

    my ($widget, $instance) = @_;

    return if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    $instance->{scale} = 1;
    scale_canvas($instance);

}
#
##############################################################################
#
#   Routine      - canvas_item_event_cb
#
#   Description  - Callback routine called when an event it delivered to a
#                  canvas item widget.
#
#   Data         - $widget      : The canvas widget object that received the
#                                 signal.
#                  $event       : A Gtk2::Gdk::Event object describing the
#                                 event that has occurred.
#                  $details     : A reference to an anonymous hash containing
#                                 the window instance and revision id that is
#                                 associated with this widget.
#                  Return Value : TRUE if the event has been handled and needs
#                                 no further handling, otherwise FALSE if the
#                                 event should carry on through the remaining
#                                 event handling.
#
##############################################################################



sub canvas_item_event_cb($$$)
{

    my ($widget, $event, $details) = @_;

    my $instance = $details->{instance};
    my $revision_id = $details->{revision_id};

    return FALSE if ($instance->{in_cb});
    local $instance->{in_cb} = 1;

    my $type = $event->type();

    if ($type eq "button-press")
    {

	my $button = $event->button();

	if ($button == 1)
	{
	    select_node($instance, $revision_id);
	}

	return TRUE;

    }
    elsif ($type eq "2button-press")
    {
	print("Double click\n");
	return TRUE;
    }

    return FALSE;

}
#
##############################################################################
#
#   Routine      - generate_ancestry_graph
#
#   Description  - Generate the ancestry graph database from the specified
#                  selection criteria.
#
#   Data         - $instance                 : The history graph window
#                                              instance.
#                  $show_all_propagate_nodes : True if al propagation nodes
#                                              are to be shown in the graph,
#                                              otherwise false if only parent
#                                              propagate nodes are to be
#                                              shown.
#                  $branches                 : A reference to a list of
#                                              branches to generate a graph
#                                              from. This parameter can be
#                                              undef or an empty list if all
#                                              branches are to be selected.
#                  $from_date                : The earliest date from which
#                                              revisions will be selected for
#                                              the history graph. This
#                                              parameter can be undef or an
#                                              empty string if no such age
#                                              restriction is required.
#                  $to_date                  : The latest date from which
#                                              revisions will be selected for
#                                              the history graph. This
#                                              parameter can be undef or an
#                                              empty string if no such age
#                                              restriction is required.
#
##############################################################################



sub generate_ancestry_graph($$;$$$)
{

    my ($instance, $show_all_propagate_nodes, $branches, $from_date, $to_date)
	= @_;

    my (%branches_set,
	$branch_selector,
	$counter,
	%date_set,
	$date_range_selector,
	$date_selector,
	@graph,
	%graph_db,
	@head_revisions,
	%nr_of_parents,
	$selected_set,
	$update_interval);
    my $wm = WindowManager->instance();

    $instance->{appbar}->set_status(__("Building ancestry graph"));
    $wm->update_gui();

    $instance->{graph_data}->{child_graph} = {};
    $instance->{graph_data}->{head_revisions} = [];

    # First build up revision hit lists based upon the selection criteria. This
    # will be then used when scanning the graph to weed out unwanted revisions.

    if (defined($from_date) && $from_date ne "")
    {
	$date_range_selector = "l:" . $from_date;
    }
    else
    {
	$date_range_selector = "";
    }
    if (defined($to_date) && $to_date ne "")
    {
	if ($date_range_selector eq "")
	{
	    $date_range_selector = "e:" . $to_date;
	}
	else
	{
	    $date_range_selector .= "/e:" . $to_date;
	}
    }

    # Remember that this is a user generated query and so may contain invalid
    # data. So protect ourselves.

    CachingAutomateStdio->register_error_handler
	(MTN_SEVERITY_ALL,
	 sub {
	     my ($severity, $message) = @_;
	     my $dialog;
	     $dialog = Gtk2::MessageDialog->new_with_markup
		 ($instance->{window},
		  ["modal"],
		  "warning",
		  "close",
		  __x("There is a problem with an internal query, Monotone "
			  . "gave:\n<b><i>{error_message}</i></b>",
		      error_message =>
			  Glib::Markup::escape_text($message)));
	     WindowManager->instance()->allow_input(sub { $dialog->run(); });
	     $dialog->destroy();
	     die("Bad query");
	 });
    eval
    {

	# Do the branches hit list.

	if (defined($branches) && scalar(@$branches) > 0)
	{
	    my $date_range = ($date_range_selector ne "")
		? ("/" . $date_range_selector) : "";
	    foreach my $branch (@$branches)
	    {
		my @revision_ids;
		$instance->{mtn}->select(\@revision_ids,
					 "b:" . $branch . $date_range);
		foreach my $revision_id (@revision_ids)
		{
		    $branches_set{$revision_id} = undef;
		}
	    }
	}
	$wm->update_gui();

	# Now do the date hit list.

	if ($date_range_selector ne "")
	{
	    my @revision_ids;
	    $instance->{mtn}->select(\@revision_ids, $date_range_selector);
	    foreach my $revision_id (@revision_ids)
	    {
		$date_set{$revision_id} = undef;
	    }
	}
	$wm->update_gui();

    };
    CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
						 \&mtn_error_handler);
    $branch_selector = 1 if (scalar(keys(%branches_set)) > 0);
    $date_selector = 1 if (scalar(keys(%date_set)) > 0);

    # Set the selected_set variable to point to which ever selector set should
    # be used to determine whether a revision exactly matches the caller's
    # search criteria. If there are no selectors then this variable will be
    # left undefined. If we have both a branch and date range selector then
    # although the branch selector is date limited, we still need the date
    # range selector for propagation revisions (which should be included if
    # they are in the date range but not necessarily in the selected set of
    # branches).

    if ($branch_selector)
    {
	$selected_set = \%branches_set;
    }
    elsif ($date_selector)
    {
	$selected_set = \%date_set;
    }

    # Get the revision graph from Monotone.

    $instance->{mtn}->graph(\@graph);

    $wm->update_gui();

    # Build up a revision child graph indexed on revision id with only the
    # revisions that we are interested in within.

    $instance->{appbar}->set_progress_percentage(0);
    $wm->update_gui();
    $update_interval = calculate_update_interval(\@graph);
    $counter = 1;
    foreach my $entry (@graph)
    {

	my ($current_selected,
	    $selected);

	# A revision is selected if:
	#     1) There are no selectors, i.e. everything is selected.
	#     2) Not all propagation nodes are to be shown and the revision is
	#        within the specified date range and on one of the selected
	#        branches.
	#     3) All propagation nodes are to be shown and the revision is
	#        within the specified date range and it or one of its parents
	#        are on one of the selected branches.
	#     4) There is no branch selector and the revision is within the
	#        specified date range.

	if (! defined($selected_set)
	    || exists($selected_set->{$entry->{revision_id}}))
	{
	    $current_selected = $selected = 1;
	}
	elsif ($show_all_propagate_nodes && $branch_selector
	       && (! $date_selector
		   || exists($date_set{$entry->{revision_id}})))
	{
	    foreach my $parent_id (@{$entry->{parent_ids}})
	    {
		if (exists($selected_set->{$parent_id}))
		{
		    $selected = 1;
		    last;
		}
	    }
	}

	# If the revision has been selected then process it.

	if ($selected)
	{

	    # File the revision. Remember it may already exist as it might be a
	    # parent of a revision that has already been processed.

	    if (! exists($graph_db{$entry->{revision_id}}))
	    {
		$graph_db{$entry->{revision_id}} = {children => [],
						    flags    => 0};
	    }

	    # If the current revision is selected itself then all parents
	    # should be included if they are in the date range (need to show
	    # propagation revisions), otherwise only those parents that are
	    # within the date range and on the right branch(es) should be
	    # included.

	    foreach my $parent_id (@{$entry->{parent_ids}})
	    {
		if (($current_selected
		     && (! $date_selector || exists($date_set{$parent_id})))
		    || (! $current_selected
			&& (! defined($selected_set)
			    || exists($selected_set->{$parent_id}))))
		{
		    if (exists($graph_db{$parent_id}))
		    {
			push(@{$graph_db{$parent_id}->{children}},
			     $entry->{revision_id});
		    }
		    else
		    {
			$graph_db{$parent_id} =
			    {children => [$entry->{revision_id}],
			     flags    => 0};
		    }
		}
	    }

	    # If this node was actually selected then mark it as such.

	    $graph_db{$entry->{revision_id}}->{flags} |= SELECTED_NODE
		if ($current_selected);

	}

	if (($counter % $update_interval) == 0)
	{
	    $instance->{appbar}->set_progress_percentage
		($counter / scalar(@graph));
	    $wm->update_gui();
	}
	++ $counter;

	# Stop if the user wants to.

	last if ($instance->{stop});

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Now find out how many head revisions we have.

    if (! $instance->{stop})
    {
	foreach my $revision_id (keys(%graph_db))
	{
	    if (scalar(@{$graph_db{$revision_id}->{children}}) == 0)
	    {
		push(@head_revisions, $revision_id);
	    }
	}
    }

    # If we have more than one head revision and we have restricted the graph
    # to a number of branches then we better make sure that as many head and
    # tail revisions are joined up (may happen due to intermediate revisions
    # being excluded because of branch selectors). For this we need a parent
    # database but limited by date if possible.

    if (! $instance->{stop} && $branch_selector && scalar(@head_revisions) > 1)
    {
	my ($context,
	    %parent_db);
	if ($date_selector)
	{
	    foreach my $entry (@graph)
	    {
		if (exists($date_set{$entry->{revision_id}}))
		{
		    my @parent_ids;
		    foreach my $parent_id (@{$entry->{parent_ids}})
		    {
			push(@parent_ids, $parent_id)
			    if (exists($date_set{$parent_id}));
		    }
		    $parent_db{$entry->{revision_id}} = \@parent_ids;
		}
	    }
	}
	else
	{
	    foreach my $entry (@graph)
	    {
		$parent_db{$entry->{revision_id}} = $entry->{parent_ids};
	    }
	}
	$context = {selected_set      => $selected_set,
		    graph_db          => \%graph_db,
		    outside_selection => undef,
		    parent_db         => \%parent_db,
		    parents           => [],
		    processed_set     => {},
		    aggressive_search => undef};
	$instance->{appbar}->set_progress_percentage(0);
	$wm->update_gui();
	$update_interval = calculate_update_interval(\@head_revisions);
	$counter = 1;
	foreach my $head_id (@head_revisions)
	{

	    # Try and join up gaps starting from this head revision.

	    $context->{outside_selection} = undef;
	    $context->{parents} = [];
	    graph_reconnect_helper($context, $head_id);

	    if (($counter % $update_interval) == 0)
	    {
		$instance->{appbar}->set_progress_percentage
		    ($counter / scalar(@head_revisions));
		$wm->update_gui();
	    }
	    ++ $counter;

	    # Stop if the user wants to.

	    last if ($instance->{stop});

	}
	$instance->{appbar}->set_progress_percentage(1);
	$wm->update_gui();
    }

    if (! $instance->{stop})
    {

	# Create a hash indexed by revision id that gives the number of parents
	# for that revision.

	foreach my $revision_id (keys(%graph_db))
	{
	    foreach my $child_id (@{$graph_db{$revision_id}->{children}})
	    {
		if (exists($nr_of_parents{$child_id}))
		{
		    ++ $nr_of_parents{$child_id};
		}
		else
		{
		    $nr_of_parents{$child_id} = 1;
		}
	    }
	}

	# Now use this hash to mark up nodes that either have no parents or
	# multiple ones (i.e. what will be rendered as circular merge nodes).

	foreach my $revision_id (keys(%graph_db))
	{
	    my $nr_parents = 0;
	    if (exists($nr_of_parents{$revision_id}))
	    {
		$nr_parents = $nr_of_parents{$revision_id};
	    }
	    if ($nr_parents == 0)
	    {
		$graph_db{$revision_id}->{flags} |= NO_PARENTS;
	    }
	    elsif ($nr_parents > 1)
	    {
		$graph_db{$revision_id}->{flags} |= CIRCULAR_NODE;
	    }
	}
	%nr_of_parents = ();

	# Update the list of head revisions.

	@head_revisions = ();
	foreach my $revision_id (keys(%graph_db))
	{
	    if ((! defined($selected_set)
		 || exists($selected_set->{$revision_id}))
		&& scalar(@{$graph_db{$revision_id}->{children}}) == 0)
	    {
		push(@head_revisions, $revision_id);
	    }
	}

	# Store the graph database and the list of head revisions.

	$instance->{graph_data}->{child_graph} = \%graph_db;
	$instance->{graph_data}->{head_revisions} = \@head_revisions;

    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - graph_reconnect_helper
#
#   Description  - Recursive routine for searching a revision graph connecting
#                  ancestors to children where there is a break due to
#                  intermediate revisions not being selected (due to a branch
#                  selector).
#
#   Data         - $context     : The scanning context for this routine.
#                  $revision_id : The revision id from where the search is to
#                                 commence.
#
##############################################################################



sub graph_reconnect_helper($$)
{

    my ($context, $revision_id) = @_;

    # Please note that by definition the selected_set field must contain a
    # valid set as this recursive routine would never have been called
    # otherwise.

    # Were we outside our selected set of revisions?

    if ($context->{outside_selection})
    {

	# Yes we were so if we have just stumbled across a node that is to be
	# graphed then make a note of the current revision in our parents list
	# and then stop searching any further down this path. Otherwise carry
	# on looking.

	if (exists($context->{graph_db}->{$revision_id}))
	{
	    push(@{$context->{parents}}, $revision_id);
	    return;
	}
	else
	{
	    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
	    {
		if (! exists($context->{processed_set}->{$parent_id}))
		{
		    graph_reconnect_helper($context, $parent_id);
		}
	    }
	}

    }
    else
    {

	my ($aggressive_search,
	    $found_new_ancestors);

	# No we weren't off selection.

	# If the current node is selected then check to see if there are any
	# selected parents. If not then we need to go into an aggressive search
	# mode where we do our best to find a graphed ancestor for it.

	if (exists($context->{selected_set}->{$revision_id}))
	{
	    $aggressive_search = 1;
	    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
	    {
		if (exists($context->{selected_set}->{$parent_id}))
		{
		    $aggressive_search = undef;
		    last;
		}
	    }
	}

	# Otherwise if the current node is a propagate node (i.e. in the graph
	# database but not selected, e.g. an off-branch parent of an on-branch
	# node) then see what we can join together. Remember that the parents
	# of parent propagate nodes weren't even looked at until now.

	elsif (exists($context->{graph_db}->{$revision_id}))
	{

	    my (@graphed_parents,
		@selected_parents);

	    # First scan the immediate parents for any that are graphed. If we
	    # find selected parents then join those up, if not then try joining
	    # up any graphed parents and if that fails and we are in aggressive
	    # search mode (i.e. desparate to find any graphed ancestor) then
	    # start up each parent path searching for any ancestor before
	    # finally giving up.

	    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
	    {
		if (exists($context->{selected_set}->{$parent_id}))
		{
		    push(@selected_parents, $parent_id);
		    $found_new_ancestors = 1;
		}
		elsif (exists($context->{graph_db}->{$parent_id}))
		{
		    push(@graphed_parents, $parent_id);
		    $found_new_ancestors = 1;
		}
	    }

	    # Selected parents take precedence.

	    if (scalar(@selected_parents) > 0)
	    {
		push(@{$context->{parents}}, @selected_parents);
	    }

	    # Then graphed ones.

	    elsif (scalar(@graphed_parents) > 0)
	    {
		push(@{$context->{parents}}, @graphed_parents);
	    }

	    # Nothing found so far, do we need to do an aggressive search?

	    elsif ($context->{aggressive_search})
	    {

		# Ok getting desperate, start a recursive search.

		# Stash the current hit list away and use a blank one as we are
		# going outside our selected set of revisions (we don't want to
		# prevent subsequent walks outside our selected set on
		# different nodes from finding a possible route to a graphed
		# parent).

		local $context->{processed_set} = {};

		# Search up each parent node.

		local $context->{outside_selection};
		foreach my $parent_id
		    (@{$context->{parent_db}->{$revision_id}})
		{
		    if (! exists($context->{processed_set}->{$parent_id}))
		    {
			$found_new_ancestors =
			    $context->{outside_selection} = 1
			    unless ($context->{outside_selection});
			graph_reconnect_helper($context, $parent_id);
		    }
		}

	    }

	    # If we have found some parents then file this revision as a child
	    # of those parents in the graph database. By definition all found
	    # parent nodes already exist in the graph database so no need to
	    # worry about creating new nodes. Also avoid adding duplicate child
	    # revision entries.

	    if ($found_new_ancestors)
	    {
		foreach my $parent_id (@{$context->{parents}})
		{
		    my $found;
		    foreach my $child_id
			(@{$context->{graph_db}->{$parent_id}->{children}})
		    {
			if ($child_id eq $revision_id)
			{
			    $found = 1;
			    last;
			}
		    }
		    if (! $found)
		    {
			push(@{$context->{graph_db}->{$parent_id}->{children}},
			     $revision_id);
		    }
		}
		$context->{parents} = [];
	    }

	}

	# Temporarily switch on aggressive search mode if necessary.

	local $context->{aggressive_search} = 1 if ($aggressive_search);

	# Now process the graphed parents.

	foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
	{
	    if (! exists($context->{processed_set}->{$parent_id})
		&& exists($context->{graph_db}->{$parent_id}))
	    {
		graph_reconnect_helper($context, $parent_id);
	    }
	}

    }

    # Mark this node as having been processed.

    $context->{processed_set}->{$revision_id} = undef;

}
#
##############################################################################
#
#   Routine      - layout_graph
#
#   Description  - Given a child graph database, get dot to lay out the
#                  geometric shapes that we want to render and then read back
#                  the result.
#
#   Data         - $instance    : The history graph window instance.
#
##############################################################################



sub layout_graph($)
{

    my $instance = $_[0];

    my (@arrows,
	$buffer,
	@circles,
	$prev_lines,
	@rectangles);

    $instance->{graph_data}->{arrows} = [];
    $instance->{graph_data}->{circles} = [];
    $instance->{graph_data}->{rectangles} = [];
    $instance->{graph_data}->{max_x} = 0;
    $instance->{graph_data}->{max_y} = 0;

    # Run the dot subprocess.

    return unless (run_command(\$buffer,
			       \&dot_input_handler_cb,
			       $instance,
			       \$instance->{stop},
			       "dot", "-q", "-y", "-s" . DPI, "-Txdot"));

    # Parse the dot output, line by line.

    $prev_lines = "";
    foreach my $line (split(/\n/, $buffer))
    {

	my $revision_id;

	# Deal with statements that span multiple lines.

	if ($line =~ m/^(.*)\\$/)
	{
	    $prev_lines .= $1;
	    next;
	}
	elsif ($prev_lines ne "")
	{
	    $line = $prev_lines . $line;
	    $prev_lines = "";
	}

	# Parse statements.

	# Look out for revision ids, always take the first one.

	if ($line =~ m/[ \t"]([0-9a-f]{40})[ \t"]/)
	{
	    $revision_id = $1;
	}

	# Lines with arrow heads.

	if ($line =~ m/B \d+(( \d+ \d+)+) *\".* P 3(( \d+){6}) *\"/)
	{
	    my (@arrow_points,
		@line_points);
	    @line_points = split(/ /, $1);
	    shift(@line_points);
	    @arrow_points = split(/ /, $3);
	    shift(@arrow_points);
	    if ($line =~ m/-> "??([0-9a-f]{40})[ \t"]/)
	    {
		my $to_revision_id = $1;
		push(@arrows, {from_revision_id => $revision_id,
			       to_revision_id   => $to_revision_id,
			       line             => \@line_points,
			       arrow            => \@arrow_points});
	    }
	}

	# Boxes.

	elsif ($line =~ m/p 4(( \d+){8}) *\"/)
	{
	    my ($br_x,
		$br_y,
		@coords,
		$tl_x,
		$tl_y);
	    @coords = split(/ /, $1);
	    shift(@coords);
	    $tl_x = $instance->{graph_data}->{max_x} + 100;
	    $br_x = 0;
	    foreach my $i (0, 2, 4, 6)
	    {
		$tl_x = ($tl_x < $coords[$i]) ? $tl_x : $coords[$i];
		$br_x = ($br_x > $coords[$i]) ? $br_x : $coords[$i];
	    }
	    $tl_y = $instance->{graph_data}->{max_y} + 100;
	    $br_y = 0;
	    foreach my $i (1, 3, 5, 7)
	    {
		$tl_y = ($tl_y < $coords[$i]) ? $tl_y : $coords[$i];
		$br_y = ($br_y > $coords[$i]) ? $br_y : $coords[$i];
	    }
	    push(@rectangles, {revision_id => $revision_id,
			       tl_x        => $tl_x,
			       tl_y        => $tl_y,
			       br_x        => $br_x,
			       br_y        => $br_y});
	}

	# Circles.

	elsif ($line =~ m/e(( \d+){4}) *\"/)
	{
	    my @list;
	    @list = split(/ /, $1);
	    shift(@list);
	    push(@circles, {revision_id => $revision_id,
			    x           => $list[0],
			    y           => $list[1],
			    width       => $list[2],
			    height      => $list[3]});
	}

	# Bounding box (i.e. the total size of the graph).

	elsif ($line =~ m/bb=\"(\d+),(\d+),(\d+),(\d+)\"/)
	{
	    $instance->{graph_data}->{max_x} = max($1, $3);
	    $instance->{graph_data}->{max_y} = max($2, $4);
	}

    }

    $instance->{graph_data}->{arrows} = \@arrows;
    $instance->{graph_data}->{circles} = \@circles;
    $instance->{graph_data}->{rectangles} = \@rectangles;

}
#
##############################################################################
#
#   Routine      - dot_input_handler_cb
#
#   Description  - Given a child graph database, generate the dot instructions
#                  writing them out to the specified file handle.
#
#   Data         - $fh_in       : The STDIN file handle for the dot
#                                 subprocess.
#                  $instance    : The history graph window instance.
#
##############################################################################



sub dot_input_handler_cb($$)
{

    my ($fh_in, $instance) = @_;

    my ($child_db,
	$hex_id_height,
	$hex_id_width,
	$layout,
	@revision_ids);

    # Create a layout object based on the main graph window and then use it to
    # get the pixel size of a hex id when displayed on the screen. This layout
    # is also used later on for any tags that need to be displayed.

    $layout = $instance->{window}->create_pango_layout("A" x HEX_ID_LENGTH);
    $layout->set_font_description($instance->{fontdescription});
    ($hex_id_width, $hex_id_height) = $layout->get_pixel_size();
    $hex_id_height = max(HEIGHT, $hex_id_height + (TEXT_BORDER * 2));
    $hex_id_width = max(WIDTH, $hex_id_width + (TEXT_BORDER * 2));

    # Generate a list of topographically sorted revision ids for each revision
    # in the graph database.

    $child_db = $instance->{graph_data}->{child_graph};
    @revision_ids = keys(%$child_db);

    # Now write out the dot graph to the dot subprocess. We don't need to worry
    # about reading and writing at the same time as dot needs the complete
    # graph before it can do its magic.

    # Pre-amble.

    $fh_in->print("digraph \"mtn-browse\"\n"
		  . "{\n"
		  . "  graph [ranksep=\"0.25\"];\n"
		  . "  node [label=\"\"];\n");

    # Rectangular non-merge nodes, possibly changing the width for tagged nodes
    # if we need more space. We give more padding for tagged nodes as these can
    # be long strings and the text scaling is not as fine grained as the rest
    # of the canvas widgets).

    $fh_in->printf("  node [shape=box, width = %f, height = %f];\n",
		   $hex_id_width / DPI,
		   $hex_id_height / DPI);
    foreach my $revision_id (@revision_ids)
    {
	if (! ($child_db->{$revision_id}->{flags} & CIRCULAR_NODE))
	{
	    my $width = WIDTH;
	    $fh_in->print("  \"" . $revision_id . "\"");
	    if (scalar(@{$child_db->{$revision_id}->{tags}}) > 0)
	    {
		$layout->set_text($child_db->{$revision_id}->{tags}->[0]);
		$width = max(WIDTH,
			     ($layout->get_pixel_size())[0]
			         + (TEXT_BORDER * 6));
	    }
	    if ($width != WIDTH)
	    {
		$fh_in->printf(" [width = %f];\n", $width / DPI);
	    }
	    else
	    {
		$fh_in->print(";\n");
	    }
	}
    }

    # Circular merge nodes.

    $fh_in->printf("  node [shape=circle, width = %f, height = %f];\n",
		   $hex_id_height / DPI,
		   $hex_id_height / DPI);
    foreach my $revision_id (@revision_ids)
    {
	if ($child_db->{$revision_id}->{flags} & CIRCULAR_NODE)
	{
	    $fh_in->print("  \"" . $revision_id . "\";\n");
	}
    }

    # Head nodes. These need to be grouped together.

    $fh_in->print("  subgraph heads\n"
		  . "  {\n"
		  . "    rank = sink;\n");
    foreach my $revision_id (@{$instance->{graph_data}->{head_revisions}})
    {
	$fh_in->print("    \"" . $revision_id . "\";\n");
    }
    $fh_in->print("  }\n");

    # Lines. Use the weight attribute on lines that go between a selected node
    # and an unselected one, with the unselected one having no
    # parents/children. The higher the weighting the shorter and straighter the
    # lines are. This has the effect of making sure that edge propagation nodes
    # are closely clustered around their selected relations.

    foreach my $revision_id (@revision_ids)
    {
	my $selected = $child_db->{$revision_id}->{flags} & SELECTED_NODE;
	my $no_parents = $child_db->{$revision_id}->{flags} & NO_PARENTS;
	foreach my $child_id (@{$child_db->{$revision_id}->{children}})
	{
	    my $child_selected =
		$child_db->{$child_id}->{flags} & SELECTED_NODE;
	    $fh_in->print("  \"" . $revision_id . "\" -> \"" . $child_id
			  . "\"");
	    if ((! $selected && $child_selected && $no_parents)
		|| ($selected && ! $child_selected
		    && scalar(@{$child_db->{$child_id}->{children}}) == 0))
	    {
		$fh_in->print(" [weight = 4];\n");
	    }
	    else
	    {
		$fh_in->print(";\n");
	    }
	}
    }

    # Close off the graph and close dot's input so that it knows to start
    # processing the data.

    $fh_in->print("}\n");
    $fh_in->close();

}
#
##############################################################################
#
#   Routine      - draw_graph
#
#   Description  - Given a child graph database and the geometric objects laid
#                  out by dot, draw the history graph in the canvas widget.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub draw_graph($)
{

    my $instance = $_[0];

    my ($counter,
	$total,
	$update_interval);
    my $child_db = $instance->{graph_data}->{child_graph};
    my $wm = WindowManager->instance();

    $instance->{graph_canvas}->set_scroll_region
	(0,
	 0,
	 $instance->{graph_data}->{max_x} + (CANVAS_BORDER * 2),
	 $instance->{graph_data}->{max_y} + (CANVAS_BORDER * 2));
    $instance->{graph}->{group} =
	Gnome2::Canvas::Item->new($instance->{graph_canvas}->root(),
				  "Gnome2::Canvas::Group",
				  x => CANVAS_BORDER,
				  y => CANVAS_BORDER);
    $instance->{graph}->{node_labels} = [];
    $instance->{graph}->{selection_box} = Gnome2::Canvas::Item->new
	($instance->{graph}->{group},
	 "Gnome2::Canvas::Rect",
	 x1            => 0,
	 y1            => 0,
	 x2            => 1,
	 y2            => 1,
	 fill_color    => "Red",
	 outline_color => "Red",
	 width_pixels  => LINE_WIDTH);
    $instance->{graph}->{selection_box}->hide();

    $total = scalar(@{$instance->{graph_data}->{rectangles}})
	+ scalar(@{$instance->{graph_data}->{circles}})
	+ scalar(@{$instance->{graph_data}->{arrows}});
    $update_interval = calculate_update_interval($total);

    # Draw the rectangular nodes with text inside them.

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status(__("Drawing graph"));
    $wm->update_gui();
    $counter = 1;
    foreach my $rectangle (@{$instance->{graph_data}->{rectangles}})
    {

	my ($label,
	    $text,
	    $widget);
	my $node = $child_db->{$rectangle->{revision_id}};
	my $node_group = Gnome2::Canvas::Item->new($instance->{graph}->{group},
						   "Gnome2::Canvas::Group",
						   x => 0,
						   y => 0);

	# Link the child database node to the relevant geometric canvas
	# information.

	$node->{canvas_item_details} = $rectangle;

	# Draw the rectangle.

	$widget = Gnome2::Canvas::Item->new
	    ($node_group,
	     "Gnome2::Canvas::Rect",
	     x1             => $rectangle->{tl_x},
	     y1             => $rectangle->{tl_y},
	     x2             => $rectangle->{br_x},
	     y2             => $rectangle->{br_y},
	     fill_color_gdk => get_node_colour ($instance, $node),
	     outline_color  => ($node->{flags} & SELECTED_NODE) ?
				   SELECTED_BORDER_COLOUR :
				   NOT_SELECTED_BORDER_COLOUR,
	     width_pixels   => LINE_WIDTH);

	# Now the text, use a revision's tag and failing that use the first
	# eight characters of its hex id. Also use a Gtk2::Label as the
	# Gnome2::Canvas::Text widget just takes too long to render.

	if (scalar(@{$node->{tags}}) > 0)
	{
	    $text = $node->{tags}->[0];
	}
	else
	{
	    $text = substr($rectangle->{revision_id}, 0, HEX_ID_LENGTH);
	}
	$label = Gtk2::Label->new($text);
	$label->modify_font($instance->{fontdescription});
	$label->show();
	push(@{$instance->{graph}->{node_labels}}, $label);
	$widget = Gnome2::Canvas::Item->new
	    ($node_group,
	     "Gnome2::Canvas::Widget",
	     widget => $label,
	     height => $rectangle->{br_y} - $rectangle->{tl_y} + 1,
	     width  => $rectangle->{br_x} - $rectangle->{tl_x} + 1,
	     x      => $rectangle->{tl_x},
	     y      => $rectangle->{tl_y});
	$widget->raise_to_top();
	$widget->show();

	$node_group->signal_connect
	    ("event",
	     \&canvas_item_event_cb,
	     {instance    => $instance,
	      revision_id => $rectangle->{revision_id}});

	if (($counter % $update_interval) == 0)
	{
	    $instance->{appbar}->set_progress_percentage($counter / $total);
	    $wm->update_gui();
	}
	++ $counter;

	# Stop if the user wants to.

	last if ($instance->{stop});

    }

    # Draw the circular nodes.

    foreach my $circle (@{$instance->{graph_data}->{circles}})
    {

	my $widget;
	my $node = $child_db->{$circle->{revision_id}};

	# Link the child database node to the relevant geometric canvas
	# information.

	$node->{canvas_item_details} = $circle;

	$widget = Gnome2::Canvas::Item->new
	    ($instance->{graph}->{group},
	     "Gnome2::Canvas::Ellipse",
	     x1             => $circle->{x} - $circle->{width},
	     y1             => $circle->{y} - $circle->{height},
	     x2             => $circle->{x} + $circle->{width},
	     y2             => $circle->{y} + $circle->{height},
	     fill_color_gdk => get_node_colour($instance, $node),
	     outline_color  => ($node->{flags} & SELECTED_NODE) ?
				   SELECTED_BORDER_COLOUR :
				   NOT_SELECTED_BORDER_COLOUR,
	     width_pixels   => LINE_WIDTH);
	$widget->raise_to_top();
	$widget->show();

	$widget->signal_connect("event",
				\&canvas_item_event_cb,
				{instance    => $instance,
				 revision_id => $circle->{revision_id}});

	if (($counter % $update_interval) == 0)
	{
	    $instance->{appbar}->set_progress_percentage($counter / $total);
	    $wm->update_gui();
	}
	++ $counter;

	# Stop if the user wants to.

	last if ($instance->{stop});
    }

    # Draw the lines.

    foreach my $arrow (@{$instance->{graph_data}->{arrows}})
    {

	my ($bpath,
	    $colour,
	    $i,
	    $pathdef);
	my $head = $arrow->{arrow};
	my $line = $arrow->{line};

	if (! ($child_db->{$arrow->{from_revision_id}}->{flags}
	       & SELECTED_NODE)
	    || ! ($child_db->{$arrow->{to_revision_id}}->{flags}
		  & SELECTED_NODE))
	{
	    $colour = NOT_SELECTED_BORDER_COLOUR;
	}
	else
	{
	    $colour = SELECTED_BORDER_COLOUR;
	}
	$pathdef = Gnome2::Canvas::PathDef->new();
	$pathdef->moveto($$line[0], $$line[1]);
	$i = 2;
	for ($i = 2; $i < scalar(@$line); $i += 6)
	{
	    $pathdef->curveto($$line[$i],
			      $$line[$i + 1],
			      $$line[$i + 2],
			      $$line[$i + 3],
			      $$line[$i + 4],
			      $$line[$i + 5]);
	}
	$pathdef->lineto($$head[2], $$head[3]);
	$pathdef->moveto($$head[0], $$head[1]);
	$pathdef->lineto($$head[2], $$head[3]);
	$pathdef->lineto($$head[4], $$head[5]);
	$pathdef->closepath();
	$bpath = Gnome2::Canvas::Item->new($instance->{graph}->{group},
					   "Gnome2::Canvas::Bpath",
					   fill_color    => $colour,
					   outline_color => $colour,
					   width_pixels  => LINE_WIDTH);
	$bpath->set_path_def($pathdef);
	$bpath->lower_to_bottom();
	$bpath->show();

	if (($counter % $update_interval) == 0)
	{
	    $instance->{appbar}->set_progress_percentage($counter / $total);
	    $wm->update_gui();
	}
	++ $counter;

	# Stop if the user wants to.

	last if ($instance->{stop});

    }
    $instance->{appbar}->set_progress_percentage(1);
    $wm->update_gui();

    # Work around a bug where the scroll bar arrows don't work with canvases,
    # basically we need to set the step increment to something so take an
    # eighth of the page size.

    foreach my $adjustment
	($instance->{graph_scrolledwindow}->get_hadjustment(),
	 $instance->{graph_scrolledwindow}->get_vadjustment())
    {
	$adjustment->step_increment($adjustment->page_increment() / 8);
    }

    # Clean up the graph if the user stopped the drawing process (it will be a
    # mess anyway), otherwise scroll to a suitable revision.

    if ($instance->{stop})
    {
	my $group = $instance->{graph}->{group};
	$instance->{graph} = {group         => undef,
			      node_labels   => [],
			      selection_box => undef};
	$group->destroy();
	$instance->{graph_canvas}->set_scroll_region(0, 0, 0, 0);
    }
    else
    {
	scroll_to_node($instance,
		       $instance->{graph_data}->{head_revisions}->[0]);
    }

    $instance->{appbar}->set_progress_percentage(0);
    $instance->{appbar}->set_status("");
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - select_node
#
#   Description  - Select the specified node in the history graph.
#
#   Data         - $instance       : The history graph window instance.
#                  $revision_id    : The id of the revision that is to be
#                                    selected.
#
##############################################################################



sub select_node($$)
{

    my ($instance, $revision_id) = @_;

    my ($branches,
	@certs,
	$change_log,
	$date,
	$item,
	$node);

    # Look up the information node for the revision id.

    return unless (exists($instance->{graph_data}->{child_graph}->
			  {$revision_id}));
    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    return unless (exists($node->{canvas_item_details}));
    $item = $node->{canvas_item_details};

    # Move and resize the selection rectangle depending upon whether we are
    # dealing with a rectangular or circular node.

    if ($node->{flags} & CIRCULAR_NODE)
    {
	$instance->{graph}->{selection_box}->set
	    (x1 => $item->{x} - $item->{width} - SELECTION_BORDER,
	     y1 => $item->{y} - $item->{height} - SELECTION_BORDER,
	     x2 => $item->{x} + $item->{width} + SELECTION_BORDER,
	     y2 => $item->{y} + $item->{height} + SELECTION_BORDER);
    }
    else
    {
	$instance->{graph}->{selection_box}->set
	    (x1 => $item->{tl_x} - SELECTION_BORDER,
	     y1 => $item->{tl_y} - SELECTION_BORDER,
	     x2 => $item->{br_x} + SELECTION_BORDER,
	     y2 => $item->{br_y} + SELECTION_BORDER);
    }
    $instance->{graph}->{selection_box}->lower_to_bottom();
    $instance->{graph}->{selection_box}->show();

    # Display the details about the revision in the details section at the
    # bottom of the history graph window. We need to get the changelog but the
    # rest is cached from before.

    $instance->{mtn}->certs(\@certs, $revision_id);
    foreach my $cert (@certs)
    {
	if ($cert->{name} eq "changelog")
	{
	    $change_log = $cert->{value};
	    $change_log =~ s/\s+$//s;
	    last;
	}
    }
    $branches = join("\n", @{$node->{branches}});
    $date = $node->{date};
    $date =~ s/T/ /;
    set_label_value($instance->{author_value_label}, $node->{author});
    set_label_value($instance->{date_value_label}, $date);
    set_label_value($instance->{branch_value_label}, $branches);
    set_label_value($instance->{change_log_value_label}, $change_log);

    # Enable any buttons that should be enabled once a revision has been
    # selected.

    foreach my $item (@{$instance->{revision_sensitive_group}})
    {
	$item->set_sensitive(TRUE);
    }

}
#
##############################################################################
#
#   Routine      - scroll_to_node
#
#   Description  - Scroll the canvas to the specified node in the history
#                  graph.
#
#   Data         - $instance       : The history graph window instance.
#                  $revision_id    : The id of the revision that is to be
#                                    selected.
#
##############################################################################



sub scroll_to_node($$)
{

    my ($instance, $revision_id) = @_;

    my ($height,
	$item,
	$node,
	$width,
	$x,
	$y);

    # Look up the information node for the revision id.

    return unless (exists($instance->{graph_data}->{child_graph}->
			  {$revision_id}));
    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    return unless (exists($node->{canvas_item_details}));
    $item = $node->{canvas_item_details};

    # Find out the centre of the node on the canvas.

    if ($node->{flags} & CIRCULAR_NODE)
    {
	$x = $item->{x};
	$y = $item->{y};
    }
    else
    {
	$x = $item->{tl_x} + floor(($item->{br_x} - $item->{tl_x} + 1) / 2);
	$y = $item->{tl_y} + floor(($item->{br_y} - $item->{tl_y} + 1) / 2);
    }

    # Get the current dimensions of the canvas.

    ($width, $height) =
	($instance->{graph_canvas}->window()->get_geometry())[2, 3];

    # Convert from world coordinates to canvas pixels (takes into account any
    # scaling factors currently in effect). Don't forget to also add in the
    # canvas borders.

    $x += CANVAS_BORDER;
    $y += CANVAS_BORDER;
    ($x, $y) = $instance->{graph_canvas}->w2c($x, $y);

    # The scroll_to() method moves the (x,y) spot to the top left hand corner
    # so adjust it so that it is in the centre instead.

    $x = max($x - floor($width / 2), 0);
    $y = max($y - floor($height / 2), 0);
    $instance->{graph_canvas}->scroll_to($x, $y);

}
#
##############################################################################
#
#   Routine      - get_node_colour
#
#   Description  - Given the specified node in the graph database, either find
#                  a colour that has been used for that `type' of node before
#                  or generate a new colour from scratch.
#
#   Data         - $instance    : The history graph window instance.
#                  $node        : The node in the graph database that is to
#                                 have a colour returned for it.
#                  Return Value : The colour as a Gtk2::Gdk::Color object.
#
##############################################################################



sub get_node_colour($$)
{

    my ($instance, $node) = @_;

    my ($colour,
	$hash_values);

    $hash_values = $node->{branches};

    # First look for an existing colour for any of the branches.

    foreach my $value (@$hash_values)
    {
	if (exists($instance->{colour_db}->{$value}))
	{
	    $colour = $instance->{colour_db}->{$value};
	    last;
	}
    }

    # Do we need a new colour?

    if (! defined($colour))
    {

	my ($blue,
	    $green,
	    $hash,
	    $hash_value,
	    $hue,
	    $red,
	    $saturation,
	    $value);

	# Yes we do.

	# Generate a new colour by hashing the differentiating value and then
	# using the first few bytes of that hash as HSV values (idea taken from
	# monotone-viz).

	if (scalar(@$hash_values) > 0)
	{
	    $hash_value = $$hash_values[0];
	}
	else
	{
	    $hash_value = "";
	}
	($hue, $saturation, $value) = unpack("CCC", md5($hash_value));

	# Now scale values. Hue 0 to 359, saturation and value 0 to 1. In
	# addition scale saturation to only go from 35% to 50% and value to
	# only go from 70% to 100%. Then convert from HSV to RGB.

	$hue = ($hue / 255) * 359;
	$saturation = (($saturation / 255) * 0.15) + 0.35;
	$value = (($value / 255) * 0.30) + 0.70;
	hsv_to_rgb($hue, $saturation, $value, \$red, \$green, \$blue);

	# Scale RGB values and create a new colour object.

	$colour = Gtk2::Gdk::Color->new(floor(($red * 65535) + 0.5) & 0xffff,
					floor(($green * 65535) + 0.5) & 0xffff,
					floor(($blue * 65535) + 0.5) & 0xffff);

	# Store colour under its hash value for possible reuse.

	$instance->{colour_db}->{$hash_value} = $colour;

    }

    return $colour;

}
#
##############################################################################
#
#   Routine      - hsv_to_rgb
#
#   Description  - Convert hue, saturation and value into RGB values. This
#                  algorithm was taken from:
#                      http://www.cs.rit.edu/~ncs/color/t_convert.html
#
#   Data         - $hue        : Hue value.
#                  $saturation : Saturation value.
#                  $value      : Brightness value.
#                  $red        : A reference to a variable that is to contain
#                                the red component.
#                  $green      : A reference to a variable that is to contain
#                                the green component.
#                  $blue       : A reference to a variable that is to contain
#                                the blue component.
#
##############################################################################



sub hsv_to_rgb($$$$$$)
{

    my ($hue, $saturation, $value, $red, $green, $blue) = @_;

    # Deal with achromatic grey.

    if ($saturation == 0)
    {
	$$red = $$green = $$blue = $value;
    }

    # Now non-grey colours.

    else
    {

	my ($f,
	    $i,
	    $p,
	    $q,
	    $t);

	# Hue is 0 to 360, scale it down into 0 to 5 and put the factorial part
	# of that division into $f.

	$hue /= 60;
	$i = floor($hue);
	$f = $hue - $i;
	$p = $value * (1 - $saturation);
	$q = $value * (1 - ($saturation * $f));
	$t = $value * (1 - ($saturation * (1 - $f)));

	if ($i == 0)
	{
	    $$red = $value;
	    $$green = $t;
	    $$blue = $p;
	}
	elsif ($i == 1)
	{
	    $$red = $q;
	    $$green = $value;
	    $$blue = $p;
	}
	elsif ($i == 2)
	{
	    $$red = $p;
	    $$green = $value;
	    $$blue = $t;
	}
	elsif ($i == 3)
	{
	    $$red = $p;
	    $$green = $q;
	    $$blue = $value;
	}
	elsif ($i == 4)
	{
	    $$red = $t;
	    $$green = $p;
	    $$blue = $value;
	}
	else
	{
	    $$red = $value;
	    $$green = $p;
	    $$blue = $q;
	}

    }

}
#
##############################################################################
#
#   Routine      - populate_revision_details
#
#   Description  - Given the specified revision id, populate the relevant node
#                  in the graph database with the revision's details such as
#                  its author, its date and lists of its branches and tags.
#
#   Data         - $instance    : The history graph window instance.
#                  $revision_id : The id of the revision that is to have its
#                                 details populated in the graph database.
#
##############################################################################



sub populate_revision_details($$)
{

    my ($instance, $revision_id) = @_;

    my ($author,
	@branches,
	@certs,
	$date,
	$node,
	@tags);

    # Get the revision's list of branches, tags and date.

    $instance->{mtn}->certs(\@certs, $revision_id);
    foreach my $cert (@certs)
    {
	if ($cert->{name} eq "author")
	{
	    $author = $cert->{value} unless (defined($author));
	}
	elsif ($cert->{name} eq "branch")
	{
	    push(@branches, $cert->{value});
	}
	elsif ($cert->{name} eq "date")
	{
	    $date = $cert->{value} unless (defined($date));
	}
	elsif ($cert->{name} eq "tag")
	{
	    push(@tags, $cert->{value});
	}
    }
    @branches = sort(@branches);
    @tags = sort(@tags);

    # Now store this data in the relevant node in the graph database. Also make
    # sure that the node is no longer circular if it has a tag name that needs
    # to be displayed.

    $node = $instance->{graph_data}->{child_graph}->{$revision_id};
    $node->{author} = $author;
    $node->{branches} = \@branches;
    $node->{date} = $date;
    $node->{tags} = \@tags;
    $node->{flags} &= ~CIRCULAR_NODE if (scalar(@tags) > 0);

}
#
##############################################################################
#
#   Routine      - scale_canvas
#
#   Description  - Adjust the scale of the canvas widgets and associated
#                  labels.
#
#   Data         - $instance : The history graph window instance.
#
##############################################################################



sub scale_canvas($)
{

    my $instance = $_[0];

    my $wm = WindowManager->instance();

    # Hide the canvas during the zoom operation, it can be a bit messy.

    $wm->make_busy($instance, 1);
    $instance->{graph_canvas}->hide();
    $wm->update_gui();

    # Adjust the canvas zoom factor, also resize the fonts on all the text
    # labels (hiding them when the text gets too small to be of any use).

    $instance->{graph_canvas}->set_pixels_per_unit($instance->{scale});
    if ((FONT_SIZE * $instance->{scale}) < 3)
    {
	for my $label (@{$instance->{graph}->{node_labels}})
	{
	    $label->hide();
	}
    }
    else
    {
	$instance->{fontdescription}->set_size
	    (floor(FONT_SIZE * $instance->{scale}) * PANGO_SCALE);
	for my $label (@{$instance->{graph}->{node_labels}})
	{
	    $label->modify_font($instance->{fontdescription});
	    $label->show();
	}
    }

    # Redraw the canvas, resized text looks blocky/pixelated but a redraw sorts
    # this out.

    $instance->{graph_canvas}->request_redraw
	(0,
	 0,
	 $instance->{graph_data}->{max_x} + (CANVAS_BORDER * 2),
	 $instance->{graph_data}->{max_y} + (CANVAS_BORDER * 2));

    # Make sure the canvas is up to date and then show it again.

    $instance->{graph_canvas}->update_now();
    $instance->{graph_canvas}->show();
    $wm->make_busy($instance, 0);

}
#
##############################################################################
#
#   Routine      - get_history_graph_window
#
#   Description  - Creates or prepares an existing history graph window for
#                  use.
#
#   Data         - Return Value : A reference to the newly created or unused
#                                 history instance record.
#
##############################################################################



sub get_history_graph_window()
{

    my ($height,
	$instance,
	$renderer,
	$width);
    my $window_type = "history_graph_window";
    my $wm = WindowManager->instance();

    # Create a new history graph window if an unused one wasn't found,
    # otherwise reuse an existing unused one.

    if (! defined($instance = $wm->find_unused($window_type)))
    {
	$instance = {};
	$instance->{glade} = Gtk2::GladeXML->new($glade_file,
						 $window_type,
						 APPLICATION_NAME);

	# Flag to stop recursive calling of callbacks.

	$instance->{in_cb} = 0;
	local $instance->{in_cb} = 1;

	# Connect Glade registered signal handlers.

	glade_signal_autoconnect($instance->{glade}, $instance);

	# Get the widgets that we are interested in.

	$instance->{window} = $instance->{glade}->get_widget($window_type);
	foreach my $widget ("appbar",
			    "graph_scrolledwindow",
			    "graph_button_vbox",
			    "revision_change_history_button",
			    "revision_change_log_button",
			    "stop_button",
			    "author_value_label",
			    "date_value_label",
			    "branch_value_label",
			    "change_log_value_label")
	{
	    $instance->{$widget} = $instance->{glade}->get_widget($widget);
	}

	# Create the graph canvas widget. We can't do this in Glade as
	# something does not honour the anti-aliased setting.

	$instance->{graph_canvas} = Gnome2::Canvas->new_aa();
	$instance->{graph_scrolledwindow}->add($instance->{graph_canvas});
	$instance->{graph_canvas}->show_all();

	# Setup the history callbacks.

	$instance->{window}->signal_connect
	    ("delete_event",
	     sub {
		 my ($widget, $event, $instance) = @_;
		 return TRUE if ($instance->{in_cb});
		 local $instance->{in_cb} = 1;
		 my $group = $instance->{graph}->{group};
		 $widget->hide();
		 $instance->{graph} = undef;
		 $group->destroy() if (defined($group));
		 $instance->{colour_db} = {};
		 $instance->{graph_data} = undef;
		 $instance->{mtn} = undef;
		 return TRUE;
	     },
	     $instance);
	$instance->{stop_button}->signal_connect
	    ("clicked", sub { $_[1]->{stop} = 1; }, $instance);

	# Gnome2::Canvas is a bit buggy and can get upset if any of its widgets
	# are referenced by Perl when it gets destroyed (or so it seems).
	# Therefore register a cleanup handler that will make sure all canvas
	# widgets are unreferenced before the application exits.

	$instance->{cleanup_handler} =
	    sub {
		my $instance = $_[0];
		$instance->{graph} = undef;
		$instance->{graph_canvas} = undef;
	    };

	# Create the font description for displaying text on the graph. I am
	# using a fixed width or monospaced font as I think on balance it makes
	# it easier to read hex ids.

	$instance->{fontdescription} = Gtk2::Pango::FontDescription->
	    from_string($user_preferences->{fixed_font});

	# Setup button sensitivity groups.

	$instance->{revision_sensitivity_group} = [];
	foreach my $item ("revision_change_history", "revision_change_log")
	{
	    push(@{$instance->{revision_sensitive_group}},
		 $instance->{glade}->get_widget($item . "_button"));
	}

	# Register the window for management and set up the help callbacks.

	$wm->manage($instance,
		    $window_type,
		    $instance->{window},
		    $instance->{stop_button});
	register_help_callbacks
	    ($instance,
	     {widget   => "graph_button_vbox",
	      help_ref => __("mtnb-lachc-history-buttons")},
	     {widget   => undef,
	      help_ref => __("mtnb-lachc-the-revision-and-file-history-"
			     . "windows")});
    }
    else
    {
	$instance->{in_cb} = 0;
	local $instance->{in_cb} = 1;
	($width, $height) = $instance->{window}->get_default_size();
	$instance->{window}->resize($width, $height);
	$instance->{graph_canvas}->set_scroll_region(0, 0, 0, 0);
	$instance->{graph_canvas}->set_pixels_per_unit(1);
	$instance->{graph}->{group}->destroy()
	    if (defined($instance->{graph})
		&& defined($instance->{graph}->{group}));
	foreach my $item (@{$instance->{revision_sensitive_group}})
	{
	    $item->set_sensitive(FALSE);
	}
	set_label_value($instance->{author_value_label}, "");
	set_label_value($instance->{date_value_label}, "");
	set_label_value($instance->{branch_value_label}, "");
	set_label_value($instance->{change_log_value_label}, "");
	$instance->{appbar}->set_progress_percentage(0);
	$instance->{appbar}->clear_stack();
    }

    $instance->{colour_db} = {};
    $instance->{fontdescription}->set_size(FONT_SIZE * PANGO_SCALE);
    $instance->{graph_data} = undef;
    $instance->{graph} = {group         => undef,
			  node_labels   => [],
			  selection_box => undef};
    $instance->{scale} = 1;
    $instance->{stop} = 0;

    return $instance;

}

1;
