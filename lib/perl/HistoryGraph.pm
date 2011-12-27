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

# Constants for the columns within the comparison files ListStore widget.

use constant CLS_FILE_NAME_COLUMN => 0;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_history_graph($;$$$);

# Private routines.

sub build_ancestry_graph($$;$$$);
sub get_history_graph_window();
sub graph_reconnect_helper($$);
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

    my $instance;
    my $wm = WindowManager->instance();

    $instance = get_history_graph_window();
    local $instance->{in_cb} = 1;

    $instance->{mtn} = $mtn;
    $instance->{window}->show_all();
    $instance->{window}->present();

    $wm->make_busy($instance, 1);
    $instance->{appbar}->push($instance->{appbar}->get_status()->get_text());
    $wm->update_gui();

    $instance->{stop_button}->set_sensitive(TRUE);

    # Get the list of file change revisions. Remember to include the current
    # revision in the history.

    $instance->{appbar}->set_status(__("Building ancestry graph"));
    $wm->update_gui();
    build_ancestry_graph($instance, 0, $branches, $from_date, $to_date);

    CHECK THIS IN.....

    print(scalar(keys(%{$instance->{graph_data}->{child_graph}})) . "\n");
    print(scalar(@{$instance->{graph_data}->{head_revisions}}) . "\n");
    foreach my $rev (@{$instance->{graph_data}->{head_revisions}})
    {
	print($rev . "\n");
    }

    # get_revision_history_helper($instance, $revision_id);

    $instance->{stop_button}->set_sensitive(FALSE);
    $wm->update_gui();

    $instance->{appbar}->pop();
    $wm->make_busy($instance, 0);

    $instance->{appbar}->set_status(__("DONE"));

}
#
##############################################################################
#
#   Routine      - build_ancestry_graph
#
#   Description  - Generate the ancestry graph information from the specified
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



sub build_ancestry_graph($$;$$$)
{

    my ($instance, $show_all_propagate_nodes, $branches, $from_date, $to_date)
	= @_;

    my(%branches_hit_list,
       $branches_selector,
       %date_hit_list,
       $date_range_selector,
       $date_selector,
       @graph,
       %graph_db,
       @head_revisions,
       %selected_hit_list);

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
		    $branches_hit_list{$revision_id} = 1;
		}
	    }
	}

	# Now do the date hit list.

	if ($date_range_selector ne "")
	{
	    my @revision_ids;
	    $instance->{mtn}->select(\@revision_ids, $date_range_selector);
	    foreach my $revision_id (@revision_ids)
	    {
		$date_hit_list{$revision_id} = 1;
	    }
	}

    };
    CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
						 \&mtn_error_handler);

    # Get the revision graph from Monotone.

    $instance->{mtn}->graph(\@graph);

    # Build up a revision parent/child graph indexed on revision id with only
    # the revisions that we are interested in within.

    $branches_selector = 1 if (scalar(keys(%branches_hit_list)) > 0);
    $date_selector = 1 if (scalar(keys(%date_hit_list)) > 0);
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

	if (! ($branches_selector || $date_selector))
	{
	    $current_selected = $selected = 1;
	}
	elsif (! $show_all_propagate_nodes && $branches_selector
	       && exists($branches_hit_list{$entry->{revision_id}}))
	{
	    $current_selected = $selected = 1;
	}
	elsif ($show_all_propagate_nodes && $branches_selector
	       && (! $date_selector
		   || exists($date_hit_list{$entry->{revision_id}})))
	{
	    foreach my $revision_id ($entry->{revision_id},
				     @{$entry->{parent_ids}})
	    {
		if (exists($branches_hit_list{$revision_id}))
		{
		    $selected = 1;
		    $current_selected = 1
			if ($revision_id eq $entry->{revision_id});
		    last;
		}
	    }
	}
	elsif (! $branches_selector && $date_selector
	       && exists($date_hit_list{$entry->{revision_id}}))
	{
	    $current_selected = $selected = 1;
	}

	# If the revision has been selected then process it.

	if ($selected)
	{

	    my $nr_parents = 0;

	    # File the revision. Remember it may already exist as it might be a
	    # parent of a revision that has already been processed.

	    if (! exists($graph_db{$entry->{revision_id}}))
	    {
		$graph_db{$entry->{revision_id}} = {children   => [],
						    merge_node => undef};
	    }
	    if ($current_selected
		&& ! exists($selected_hit_list{$entry->{revision_id}}))
	    {
		$selected_hit_list{$entry->{revision_id}} = 1;
	    }

	    # If the current revision is selected itself then all parents
	    # should be included if they are in the date range (need to show
	    # propagation revisions), otherwise only those parents that are
	    # within the date range and on the right branch(es) should be
	    # included.

	    foreach my $parent_id (@{$entry->{parent_ids}})
	    {
		if (($current_selected
		     && (! $date_selector
			 || exists($date_hit_list{$parent_id})))
		    || (! $current_selected
			&& exists($selected_hit_list{$parent_id})))
		{
		    if (exists($graph_db{$parent_id}))
		    {
			push(@{$graph_db{$parent_id}->{children}},
			     $entry->{revision_id});
		    }
		    else
		    {
			$graph_db{$parent_id} =
			    {children   => [$entry->{revision_id}],
			     merge_node => undef};
		    }
		    ++ $nr_parents;
		}
	    }

	    # If this node is going to have more than one parent in the graph
	    # then mark it as a merge node.

	    $graph_db{$entry->{revision_id}}->{merge_node} = 1
		if ($nr_parents > 1);

	}

    }

    # Now find out how many head revisions we have.

    foreach my $revision_id (keys(%graph_db))
    {
	if (scalar(@{$graph_db{$revision_id}->{children}}) == 0)
	{
	    push(@head_revisions, $revision_id);
	}
    }

    # If we have more than one head revision and we have restricted the graph
    # to a number of branches then we better make sure that as many head and
    # tail revisions are joined up (may happen due to intermediate revisions
    # being excluded because of branch selectors). For this we need a parent
    # database.

    if ($branches_selector && scalar(@head_revisions) > 1)
    {
	my ($context,
	    %parent_db);
	foreach my $entry (@graph)
	{
	    $parent_db{$entry->{revision_id}} = $entry->{parent_ids};
	}
	$context = {date_hit_list     => \%date_hit_list,
		    date_selector     => $date_selector,
		    selected_hit_list => \%selected_hit_list,
		    graph_db          => \%graph_db,
		    outside_selection => undef,
		    parent_db         => \%parent_db,
		    parents           => [],
		    revision_hit_list => {}};
	foreach my $head_id (@head_revisions)
	{
	    $context->{outside_selection} = undef;
	    $context->{parents} = [];
	    graph_reconnect_helper($context, $head_id);
	    print(".");
	    STDOUT->flush();
	}
	print("\n");
    }

    %date_hit_list = ();
    @graph = ();

    # Update the list of head revisions.

    @head_revisions = ();
    foreach my $revision_id (keys(%graph_db))
    {
	if (exists($selected_hit_list{$revision_id})
	    && scalar(@{$graph_db{$revision_id}->{children}}) == 0)
	{
	    push(@head_revisions, $revision_id);
	}
    }

    %branches_hit_list = ();

    # Store the graph database and the list of head revisions.

    $instance->{graph_data}->{child_graph} = \%graph_db;
    $instance->{graph_data}->{head_revisions} = \@head_revisions;

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

    my ($hit_list,
	$processing_this_node);
    my $nr_parents = 0;
    my $outside_selection = $context->{outside_selection};

    # Were we outside our selected set of revisions?

    if ($context->{outside_selection})
    {

	# Yes we were so if we have just come back inside our selected set of
	# revisions again then make a note of the current revision in our
	# parents list and then terminate this branch of the search by
	# returning.

	if (exists($context->{selected_hit_list}->{$revision_id}))
	{
	    push(@{$context->{parents}}, $revision_id);
	    return;
	}

    }
    else
    {

	# No we aren't so stash the current hit list away just in case we go
	# outside our selected set of revisions (we don't want to prevent
	# subsequent walks outside our selected set on different nodes from
	# finding a possible route to a selected parent).

	$hit_list = $context->{revision_hit_list};

    }

    # Process the parents outside our selected set first, before moving onto
    # sort out other nodes. So at this point only process the parent if we
    # haven't done so already, it's not selected and it doesn't fall foul of
    # any date selector.

    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
    {
	if (! exists($context->{revision_hit_list}->{$parent_id})
	    && ! exists($context->{selected_hit_list}->{$parent_id}))
	{
	    $processing_this_node = $context->{outside_selection} = 1
		unless ($context->{outside_selection});
	    graph_reconnect_helper($context, $parent_id);
	}
    }

    # Restore any stashed hit list.

    $context->{revision_hit_list} = $hit_list if (defined($hit_list));

    # If we have found some parents then file this revision as a child of those
    # parents in the graph database. We don't have to worry about the parent
    # entries not existing in the graph database as they could only be unlinked
    # parents by being selected in the first place.

    if ($processing_this_node)
    {
	foreach my $parent_id (@{$context->{parents}})
	{
	    push(@{$context->{graph_db}->{$parent_id}->{children}},
		 $revision_id);
	    ++ $nr_parents;
	}
	$context->{parents} = [];
	$context->{outside_selection} = undef;
    }

    # Ok so now process the selected parents, we are done with this node.

    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
    {
	if (($outside_selection
	     || ! exists($context->{revision_hit_list}->{$parent_id}))
	    && exists($context->{selected_hit_list}->{$parent_id}))
	{
	    graph_reconnect_helper($context, $parent_id);
	    ++ $nr_parents;
	}
    }

    # Update the merge node attribute if we are processing this node and we now
    # have multiple parents.

    $context->{graph_db}->{$revision_id}->{merge_node} = 1
	if ($processing_this_node && $nr_parents > 1);

    # Mark this node as having been processed.

    $context->{revision_hit_list}->{$revision_id} = 1;

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
			    "graph_canvas",
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

	# Setup the history callbacks.

	$instance->{window}->signal_connect
	    ("delete_event",
	     sub {
		 my ($widget, $event, $instance) = @_;
		 return TRUE if ($instance->{in_cb});
		 local $instance->{in_cb} = 1;
		 $widget->hide();
		 # $instance->{history_buffer}->set_text("");
		 $instance->{mtn} = undef;
		 return TRUE;
	     },
	     $instance);
	$instance->{stop_button}->signal_connect
	    ("clicked", sub { $_[1]->{stop} = 1; }, $instance);

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

    $instance->{stop} = 0;

    # Empty out the contents.

    # $instance->{history_buffer}->set_text("");

    return $instance;

}

1;
