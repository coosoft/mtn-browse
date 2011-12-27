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

# Constants for the scale of items in a graph.

use constant DPI    => 72;
use constant HEIGHT => 28;
use constant WIDTH  => 72;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub display_history_graph($;$$$);

# Private routines.

sub build_ancestry_graph($$;$$$);
sub draw_graph($);
sub get_history_graph_window();
sub graph_reconnect_helper($$);
sub layout_graph($);
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

    # get_revision_history_helper($instance, $revision_id);

    $instance->{appbar}->set_status(__("Laying out graph with dot"));
    $wm->update_gui();
    layout_graph($instance);

    $instance->{appbar}->set_status(__("Drawing graph"));
    $wm->update_gui();
    draw_graph($instance);

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

    my(%branches_set,
       $branches_selector,
       %date_set,
       $date_range_selector,
       $date_selector,
       @graph,
       %graph_db,
       @head_revisions,
       $selected_set);
    my $wm = WindowManager->instance();

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

    };
    CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
						 \&mtn_error_handler);
    $branches_selector = 1 if (scalar(keys(%branches_set)) > 0);
    $date_selector = 1 if (scalar(keys(%date_set)) > 0);

    # Set the selected_set variable to point to which ever selector set should
    # be used to determine whether a revision exactly matches the caller's
    # search criteria. If there are no selectors then this variable will be
    # left undefined. If we have both a branch and date range selector then
    # although the branch selector is date limited, we still need the date
    # range selector for propagation revisions (which should be included if
    # they are in the date range but not necessarily in the selected set of
    # branches).

    if ($branches_selector)
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
	elsif ($show_all_propagate_nodes && $branches_selector
	       && (! $date_selector
		   || exists($date_set{$entry->{revision_id}})))
	{
	    foreach my $revision_id ($entry->{revision_id},
				     @{$entry->{parent_ids}})
	    {
		if (exists($selected_set->{$revision_id}))
		{
		    $selected = 1;
		    last;
		}
	    }
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

    $wm->update_gui();
    return if ($instance->{stop});

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
    # database but limited by date if necessary.

    if ($branches_selector && scalar(@head_revisions) > 1)
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
		    processed_set      => {}};
	foreach my $head_id (@head_revisions)
	{
	    $context->{outside_selection} = undef;
	    $context->{parents} = [];
	    graph_reconnect_helper($context, $head_id);
	    $wm->update_gui();
	    return if ($instance->{stop});
	}
    }

    # Update the list of head revisions.

    @head_revisions = ();
    foreach my $revision_id (keys(%graph_db))
    {
	if ((! defined($selected_set) || exists($selected_set->{$revision_id}))
	    && scalar(@{$graph_db{$revision_id}->{children}}) == 0)
	{
	    push(@head_revisions, $revision_id);
	}
    }

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

    my ($process_all_parents,
	$processed_set,
	$processing_this_node);
    my $nr_parents = 0;

    # Please note that by definition the selected_set field must contain a
    # valid set as this recursive routine would never have been called
    # otherwise.

    # Were we outside our selected set of revisions?

    if ($context->{outside_selection})
    {

	# Yes we were so if we have just come back inside our selected set of
	# revisions again then make a note of the current revision in our
	# parents list and then terminate this branch of the search by
	# returning.

	if (exists($context->{graph_db}->{$revision_id}))
	{
	    push(@{$context->{parents}}, $revision_id);
	    return;
	}

    }
    else
    {

	# No we aren't so stash the current hit list away and use a blank one
	# just in case we go outside our selected set of revisions (we don't
	# want to prevent subsequent walks outside our selected set on
	# different nodes from finding a possible route to a selected parent).

	$processed_set = $context->{processed_set};
	$context->{processed_set} = {};

    }

    # Process the parents with a mind to joining up gaps in the history.
    # A parent for a given node will not be processed if it:
    #     1) Has already been processed in this specific joining scan (remember
    #        that processed_set has been stashed and wiped at the start of each
    #        potential joining scan).
    #     2) Is outside of any date range. Actually we don't really need to
    #        check this here as the parent database has already been restricted
    #        to only cover revisions that are in date range. No I hadn't forgot
    #        to put the test in below!
    # Otherwise a parent will be processed if:
    #     1) It is a propagate node (ones which are in the graph database but
    #        aren't selected). These nodes wouldn't have had any parents
    #        recorded in the first place if they were a parent of a selected
    #        node.
    #     2) We are currently off selection doing a joining scan.
    #     3) It has not been included at all (i.e. not in the graph database).
    #     Please note that the result of the first two conditions are stored in
    #     $process_all_parents as that relates to the current node and not its
    #     parents.

    $process_all_parents = 1
	if ((exists($context->{graph_db}->{$revision_id})
	     && ! exists($context->{selected_set}->{$revision_id}))
	    || $context->{outside_selection});
    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
    {
	if (($process_all_parents
	     || ! exists($context->{graph_db}->{$parent_id}))
	    && ! exists($context->{processed_set}->{$parent_id}))
	{
	    $processing_this_node = $context->{outside_selection} = 1
		unless ($context->{outside_selection});
	    graph_reconnect_helper($context, $parent_id);
	}
    }

    # Restore any stashed hit list.

    $context->{processed_set} = $processed_set if (defined($processed_set));

    # If we have found some parents then file this revision as a child of those
    # parents in the graph database. We don't have to worry about the parent
    # entries not existing in the graph database as they could only be unlinked
    # parents by being selected in the first place. Avoid adding duplicated
    # parent nodes (can happen especially on propagation nodes that originally
    # had selected parents).

    if ($processing_this_node)
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
		++ $nr_parents;
	    }
	}
	$context->{parents} = [];
	$context->{outside_selection} = undef;
    }

    # Ok so now process the selected parents, we are done with this node.

    foreach my $parent_id (@{$context->{parent_db}->{$revision_id}})
    {
	if (! exists($context->{processed_set}->{$parent_id})
	    && exists($context->{graph_db}->{$parent_id}))
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
#                  Return Value : True if the generation worked, otherwise
#                                 false if something went wrong.
#
##############################################################################



sub layout_graph($)
{

    my $instance = $_[0];

    my (@arrows,
	$buffer,
	$child_db,
	@circles,
	@err,
	$fh_err,
	$fh_in,
	$fh_out,
	$my_pid,
	$pid,
	$prev_lines,
	@rectangles,
	@revision_ids,
	$stop,
	$total_bytes,
	$watcher);
    my $wm = WindowManager->instance();

    $instance->{graph_data}->{arrows} = [];
    $instance->{graph_data}->{circles} = [];
    $instance->{graph_data}->{rectangles} = [];
    $instance->{graph_data}->{max_x} = 0;
    $instance->{graph_data}->{max_y} = 0;

    # Run the dot subprocess.

    $fh_err = gensym();
    $my_pid = $$;
    eval
    {
	$pid = open3($fh_in,
		     $fh_out,
		     $fh_err,
		     "dot", "-q", "-y", "-s72", "-Txdot");
    };

    # Check for errors (remember that open3() errors can happen in both the
    # parent and child processes).

    if ($@)
    {
	if ($$ != $my_pid)
	{

	    # In the child process so all we can do is complain and exit.

	    warn(__x("open3 failed: {error_message}", error_message => $@));
	    exit(1);

	}
	else
	{

	    # In the parent process so deal with the error in the usual way.

	    my $dialog = Gtk2::MessageDialog->new
		(undef,
		 ["modal"],
		 "warning",
		 "close",
		 __x("The dot subprocess could not start,\n"
		         . "the system gave:\n<b><i>{error_message}</b></i>",
		     error_message => Glib::Markup::escape_text($@)));
	    WindowManager->instance()->allow_input(sub { $dialog->run(); });
	    $dialog->destroy();
	    return;

	}
    }

    # Setup a watch handler to read our data when we hand control over to GTK2.

    $total_bytes = 0;
    $buffer = "";
    $watcher = Gtk2::Helper->add_watch
	($fh_out->fileno(), "in",
	 sub {
	     my $bytes_read;
	     if (($bytes_read = $fh_out->sysread($buffer, 32768, $total_bytes))
		     == 0
		 || ! defined($bytes_read))
	     {
		 $stop = 1;
	     }
	     else
	     {
		 $total_bytes += $bytes_read;
	     }
	     return TRUE;
	 });

    # Generate a list of topographically sorted revision ids for each revision
    # in the graph database.

    $child_db = $instance->{graph_data}->{child_graph};
    $instance->{mtn}->toposort(\@revision_ids, keys(%$child_db));

    # Now write out the dot graph to the dot subprocess. We don't need to worry
    # about reading and writing at the same time as dot needs the complete
    # graph before it can do its magic.

    # Pre-amble.

    $fh_in->print("digraph \"mtn-browse\"\n"
		  . "{\n"
		  . "  graph [ranksep=\"0.25\"] ;\n"
		  . "  node [label=\"\"] ;\n");

    # Rectangular non-merge nodes.

    $fh_in->printf("  node [shape=box, width = %f, height = %f] ;\n",
		   WIDTH / DPI,
		   HEIGHT / DPI);
    foreach my $revision_id (@revision_ids)
    {
	if (! $child_db->{$revision_id}->{merge_node})
	{
	    $fh_in->print("  \"" . $revision_id . "\" ;\n");
	}
    }

    # Circular merge nodes.

    $fh_in->printf("  node [shape=circle, width = %f, height = %f] ;\n",
		   HEIGHT / DPI,
		   HEIGHT / DPI);
    foreach my $revision_id (@revision_ids)
    {
	if ($child_db->{$revision_id}->{merge_node})
	{
	    $fh_in->print("  \"" . $revision_id . "\" ;\n");
	}
    }

    # Head nodes.

    $fh_in->print("  subgraph heads\n"
		  . "  {\n"
		  . "    rank = sink ;\n");
    foreach my $revision_id (@{$instance->{graph_data}->{head_revisions}})
    {
	$fh_in->print("    \"" . $revision_id . "\" ;\n");
    }
    $fh_in->print("  }\n");

    # Hierarchy.

    foreach my $revision_id (@revision_ids)
    {
	foreach my $child_id (@{$child_db->{$revision_id}->{children}})
	{
	    $fh_in->print("  \"" . $revision_id . "\" -> \"" . $child_id
			  . "\" ;\n");
	}
    }

    # Close off the graph and close dot's input so that it knows to start
    # processing the data.

    $fh_in->print("}\n");
    $fh_in->close();

    # Hand control over to GTK2 whilst we read in the output from dot.

    while (! $stop && ! $instance->{stop})
    {
	Gtk2->main_iteration();
    }
    Gtk2::Helper->remove_watch($watcher);
    $buffer = "" if ($instance->{stop});

    # If we have been asked to abort then terminate the subprocess, otherwise
    # get any error output as the subprocess has just exited of its own accord.

    if ($instance->{stop})
    {
	kill("TERM", $pid);
    }
    else
    {
	@err = $fh_err->getlines() unless ($instance->{stop});
    }

    $fh_out->close();
    $fh_err->close();

    # Reap the process and deal with any errors.

    for (my $i = 0; $i < 4; ++ $i)
    {

	my $wait_status = 0;

	# Wait for the subprocess to exit (preserving the current state of $@
	# so that any exception that has already occurred is not lost, also
	# ignore any errors resulting from waitpid() interruption).

	{
	    local $@;
	    eval
	    {
		local $SIG{ALRM} = sub { die(WAITPID_INTERRUPT); };
		alarm(5);
		$wait_status = waitpid($pid, 0);
		alarm(0);
	    };
	    $wait_status = 0
		if ($@ eq WAITPID_INTERRUPT && $wait_status < 0
		    && $! == EINTR);
	}

	# The subprocess has terminated.

	if ($wait_status == $pid)
	{
	    if (! $instance->{stop})
	    {
		my $exit_status = $?;
		if (WIFEXITED($exit_status) && WEXITSTATUS($exit_status) != 0)
		{
		    my $dialog = Gtk2::MessageDialog->new_with_markup
			(undef,
			 ["modal"],
			 "warning",
			 "close",
			 __x("The dot subprocess failed with an exit status\n"
				 . "of {exit_code} and printed the following "
			         . "on stderr:\n"
				 . "<b><i>{error_message}</i></b>",
			     exit_code => WEXITSTATUS($exit_status),
			     error_message => Glib::Markup::escape_text
					      (join("", @err))));
		    WindowManager->instance()->allow_input
			(sub { $dialog->run(); });
		    $dialog->destroy();
		    return;
		}
		elsif (WIFSIGNALED($exit_status))
		{
		    my $dialog = Gtk2::MessageDialog->new
			(undef,
			 ["modal"],
			 "warning",
			 "close",
			 __x("The dot subprocess was terminated by signal "
				 . "{number}.",
			     number => WTERMSIG($exit_status)));
		    WindowManager->instance()->allow_input
			(sub { $dialog->run(); });
		    $dialog->destroy();
		    return;
		}
	    }
	    last;
	}

	# The subprocess is still there so try and kill it unless it's time to
	# just give up.

	elsif ($i < 3 && $wait_status == 0)
	{
	    if ($i == 0)
	    {
		kill("INT", $pid);
	    }
	    elsif ($i == 1)
	    {
		kill("TERM", $pid);
	    }
	    else
	    {
		kill("KILL", $pid);
	    }
	}

	# Stop if we don't have any relevant children to wait for anymore.

	elsif ($wait_status < 0 && $! == ECHILD)
	{
	    last;
	}

	# Either there is some other error with waitpid() or a child process
	# has been reaped that we aren't interested in (in which case just
	# ignore it).

	elsif ($wait_status < 0)
	{
	    my $err_msg = $!;
	    kill("KILL", $pid);
	    my $dialog = Gtk2::MessageDialog->new_with_markup
		(undef,
		 ["modal"],
		 "warning",
		 "close",
		 __x("waitpid failed with:\n<b><i>{error_message}</i></b>",
		     error_message => Glib::Markup::escape_text($!)));
	    WindowManager->instance()->allow_input(sub { $dialog->run(); });
	    $dialog->destroy();
	    return;
	}

    }

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

	# Boxes.

	if ($line =~ m/p 4(( \d+){8}) *\"/)
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

	# Lines with arrow heads.

	elsif ($line =~ m/B \d+(( \d+ \d+)+) *\".* P 3(( \d+){6}) *\"/)
	{
	    my (@arrow_points,
		@line_points);
	    @line_points = split(/ /, $1);
	    shift(@line_points);
	    @arrow_points = split(/ /, $3);
	    shift(@arrow_points);
	    push(@arrows, {revision_id => $revision_id,
			   line        => \@line_points,
			   arrow       => \@arrow_points});
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

	elsif ($line =~ m/bb=\"\d+,\d+,(\d+),(\d+)\"/)
	{
	    $instance->{graph_data}->{max_x} = $1;
	    $instance->{graph_data}->{max_y} = $2;
	}

    }

    $instance->{graph_data}->{arrows} = \@arrows;
    $instance->{graph_data}->{circles} = \@circles;
    $instance->{graph_data}->{rectangles} = \@rectangles;

    return 1;

}
#
##############################################################################
#
#   Routine      - draw_graph
#
#   Description  - Given a child graph database and the geometric objects laid
#                  out by dot, draw the history graph in the canvas widget.
#
#   Data         - $instance    : The history graph window instance.
#
##############################################################################



sub draw_graph($)
{

    my $instance = $_[0];

    my $group;

    $instance->{graph_canvas}->set_scroll_region
	(0,
	 0,
	 $instance->{graph_data}->{max_x},
	 $instance->{graph_data}->{max_y});

    $group = Gnome2::Canvas::Item->new($instance->{graph_canvas}->root(),
				       "Gnome2::Canvas::Group",
				       x => 0,
				       x => 0);
    $instance->{graph_group} = $group;

    foreach my $rectangle (@{$instance->{graph_data}->{rectangles}})
    {
	Gnome2::Canvas::Item->new($group,
				  "Gnome2::Canvas::Rect",
				  x1 => $rectangle->{tl_x},
				  y1 => $rectangle->{tl_y},
				  x2 => $rectangle->{br_x},
				  y2 => $rectangle->{br_y},
				  fill_color => "yellow",
				  outline_color => "black",
				  width_pixels => 2);
    }

    foreach my $arrow (@{$instance->{graph_data}->{arrows}})
    {
	my ($head,
	    $bpath,
	    $i,
	    $line,
	    $pathdef);
	$head = $arrow->{arrow};
	$line = $arrow->{line};
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
	$pathdef->moveto($$head[0], $$head[1]);
	$pathdef->lineto($$head[2], $$head[3]);
	$pathdef->lineto($$head[4], $$head[5]);
	$pathdef->closepath();
	$bpath = Gnome2::Canvas::Item->new($group,
					   "Gnome2::Canvas::Bpath",
					   fill_color => "black",
					   outline_color => "black",
					   width_pixels => 2);
	$bpath->set_path_def($pathdef);
	$bpath->lower_to_bottom();
    }

    foreach my $circle (@{$instance->{graph_data}->{circles}})
    {
	Gnome2::Canvas::Item->new($group,
				  "Gnome2::Canvas::Ellipse",
				  x1 => $circle->{x} - $circle->{width},
				  y1 => $circle->{y} - $circle->{height},
				  x2 => $circle->{x} + $circle->{width},
				  y2 => $circle->{y} + $circle->{height},
				  fill_color => "pink",
				  outline_color => "black",
				  width_pixels => 2);
    }

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

	# Create the graph canvas widget. We can't do this is Glade as
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
		 $widget->hide();
		 # $instance->{history_buffer}->set_text("");
		 $instance->{graph_group}->destroy()
		     if (defined($instance->{graph_group}));
		 $instance->{graph_data} = undef;
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

    $instance->{graph_group} = undef;
    $instance->{graph_data} = undef;
    $instance->{stop} = 0;

    # Empty out the contents.

    # $instance->{history_buffer}->set_text("");

    return $instance;

}

1;
