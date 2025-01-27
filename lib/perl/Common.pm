##############################################################################
#
#   File Name    - Common.pm
#
#   Description  - The common module for the mtn-browse application. This
#                  module contains assorted general purpose routines used
#                  throughout the application.
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

# ***** GLOBAL DATA DECLARATIONS *****

# Constant for the size, in bytes, that data is chopped into when detecting
# binary data.

use constant CHUNK_SIZE => 10240;

# Constant used to represent the exception thrown when interrupting waitpid().

use constant WAITPID_INTERRUPT => "waitpid-interrupt";

# A set containing the months that have 31 days in them (months start from 0).

my %long_months = (0  => undef,
                   2  => undef,
                   4  => undef,
                   6  => undef,
                   7  => undef,
                   9  => undef,
                   11 => undef);

# The saved directory locations where assorted Gtk2::FileChooserDialog dialog
# windows were last used.

my %file_chooser_dir_locations;

# A map for converting Gnome help references into valid file based URLs for the
# HTML help mode (used when yelp is not available).

my %help_ref_to_url_map;

# A list of reaped process details.

my @reaped_process_details_list;

# ***** FUNCTIONAL PROTOTYPES *****

# Public routines.

sub add_reaped_process_details($$);
sub adjust_time($$$);
sub busy_dialog_run($);
sub cache_extra_file_info($$$);
sub cached_waitpid($$$);
sub calculate_update_interval($;$);
sub check_and_create_mtn_object($$;$);
sub check_and_open_database($$$);
sub cleanup_mtn_error_message($;$);
sub colour_to_string($);
sub create_format_tags($);
sub data_is_binary($);
sub display_help(;$);
sub display_html($);
sub file_glob_to_regexp($);
sub generate_tmp_path($);
sub get_branch_revisions($$$$$);
sub get_dir_contents($$$);
sub get_revision_ids($$;$);
sub glade_signal_autoconnect($$);
sub handle_comboxentry_history($$;$);
sub handle_history($$$);
sub hex_dump($);
sub mtn_time_string_to_locale_time_string($);
sub mtn_time_string_to_time($);
sub open_database($$$);
sub program_valid($;$);
sub register_help_callbacks($$@);
sub revision_context_textview_popup_menu_item_cb($$);
sub run_command($$$$$$@);
sub save_as_file($$$);
sub set_label_value($$);
sub set_window_size($$);
sub shell_command(@);
sub treeview_column_searcher($$$$);
sub treeview_setup_search_column_selection($@);

# Private routines.

sub adjust_day_for_month($$$);
sub build_help_ref_to_url_map();
#
##############################################################################
#
#   Routine      - generate_tmp_path
#
#   Description  - Generate a unique and temporary path for the specified file
#                  name. The file name is included in the result and will be
#                  unchanged.
#
#   Data         - $file_name   : The file name component that is to be used.
#                  Return Value : The full, unique, temporary path on success,
#                                 otherwise undef on failure.
#
##############################################################################



sub generate_tmp_path($)
{

    my $file_name = $_[0];

    my ($path,
        $i);

    # Loop through looking for a temporary subdirectory not containing the
    # specified file.

    for ($i = 0; ; ++ $i)
    {
        if (-d File::Spec->catfile($tmp_dir, $i))
        {
            if (! -e ($path = File::Spec->catfile($tmp_dir, $i, $file_name)))
            {
                return $path;
            }
        }
        else
        {
            return unless mkdir(File::Spec->catfile($tmp_dir, $i));
            return File::Spec->catfile($tmp_dir, $i, $file_name);
        }
    }

    return;

}
#
##############################################################################
#
#   Routine      - run_command
#
#   Description  - Run the specified command and return its output.
#
#   Data         - $buffer      : A reference to the buffer that is to contain
#                                 the output from the command.
#                  $c_locale    : True if the command is to be run in the C
#                                 locale, otherwise false if the command is to
#                                 be run in the current locale.
#                  $input_cb    : Either a reference to a callback routine
#                                 that is to be called in order to provide
#                                 data on STDIN or undef if no such callback
#                                 routine is required (i.e. no data is to be
#                                 sent via STDIN).
#                  $details     : Any client data that is to be passed to the
#                                 input callback routine.
#                  $abort       : Either a reference to a boolean that is set
#                                 to true if the command is to be aborted or
#                                 undef if no abort checking is required.
#                  $err_buffer  : Either a reference to a buffer that is to
#                                 contain what was read from the subprocess's
#                                 STDERR file descriptor or undef if the
#                                 caller wants this routine to handle it by
#                                 displaying any error message in a dialog
#                                 window if the subprocess should exit in
#                                 error.
#                  $args        : A list containing the command to run and its
#                                 arguments.
#                  Return Value : True if the command worked, otherwise false
#                                 if something went wrong.
#
##############################################################################



sub run_command($$$$$$@)
{

    my ($buffer, $c_locale, $input_cb, $details, $abort, $err_buffer, @args)
        = @_;

    my ($dummy_flag,
        @err,
        $fd_err,
        $fd_in,
        $fd_out,
        $my_pid,
        $pid,
        $stop,
        $timer,
        $total_bytes,
        $watcher);

    $abort = \$dummy_flag unless (defined($abort));

    # Run the command.

    $fd_err = gensym();
    $my_pid = $$;
    eval
    {
        local $ENV{LC_ALL} = "C" if ($c_locale);
        local $ENV{LANG} = "C" if ($c_locale);
        $pid = open3($fd_in, $fd_out, $fd_err, @args);
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

            my $dialog = Gtk2::MessageDialog->new_with_markup
                (undef,
                 ["modal"],
                 "warning",
                 "close",
                 __x("The {name} subprocess could not start,\n"
                         . "the system gave:\n<b><i>{error_message}</i></b>",
                     name => Glib::Markup::escape_text($args[0]),
                     error_message => Glib::Markup::escape_text($@)));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;

        }
    }

    # Setup a watch handler to read our data when we hand control over to GTK2.

    $total_bytes = 0;
    $$buffer = "";
    $watcher = Gtk2::Helper->add_watch
        ($fd_out->fileno(), "in",
         sub {
             my $bytes_read;
             if (($bytes_read = $fd_out->sysread($$buffer,
                                                 32768,
                                                 $total_bytes))
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

    # Setup a half second interval timer to pulse progress bar widgets.

    $timer = Glib::Timeout->add
        (500,
         sub {
             $pulse_widget->pulse()
                 if (defined($pulse_widget));
             return TRUE;
         });

    # Call the input callback routine if we have one. It is allowed to close
    # STDIN if it wishes to.

    &$input_cb($fd_in, $details) if (defined($input_cb));

    # Hand control over to GTK2 whilst we read in the output from the
    # subprocess.

    while (! $stop && ! $$abort)
    {
        Gtk2->main_iteration();
    }

    # Remove the interval timer and the watch handler.

    Glib::Source->remove($timer);
    Gtk2::Helper->remove_watch($watcher);

    # If we have been asked to abort then terminate the subprocess, otherwise
    # get any error output as the subprocess has just exited of its own accord.

    if ($$abort)
    {
        kill("TERM", $pid);
    }
    else
    {
        @err = $fd_err->getlines() unless ($$abort);
        $$err_buffer = join("", @err) if (defined($err_buffer));
    }

    $fd_in->close() if ($fd_in->opened());
    $fd_out->close();
    $fd_err->close();

    # Reap the process and deal with any errors.

    for (my $i = 0; $i < 4; ++ $i)
    {

        my $exit_status;
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
                $wait_status = cached_waitpid($pid, 0, \$exit_status);
                alarm(0);
            };
            $wait_status = 0
                if ($@ eq WAITPID_INTERRUPT && $wait_status < 0
                    && $! == EINTR);
        }

        # The subprocess has terminated.

        if ($wait_status == $pid)
        {
            if (! $$abort)
            {
                if (WIFEXITED($exit_status) && WEXITSTATUS($exit_status) != 0)
                {
                    if (! defined($err_buffer))
                    {
                        my $dialog = Gtk2::MessageDialog->new_with_markup
                            (undef,
                             ["modal"],
                             "warning",
                             "close",
                             __x("The {name} subprocess failed with an exit "
                                         . "status\n"
                                     . "of {exit_code} and printed the "
                                         . "following on stderr:\n"
                                     . "<b><i>{error_message}</i></b>",
                                 name => Glib::Markup::escape_text($args[0]),
                                 exit_code => WEXITSTATUS($exit_status),
                                 error_message => Glib::Markup::escape_text
                                                  (join("", @err))));
                        busy_dialog_run($dialog);
                        $dialog->destroy();
                    }
                    return;
                }
                elsif (WIFSIGNALED($exit_status))
                {
                    my $dialog = Gtk2::MessageDialog->new
                        (undef,
                         ["modal"],
                         "warning",
                         "close",
                         __x("The {name} subprocess was terminated by signal "
                                 . "{number}.",
                             name   => Glib::Markup::escape_text($args[0]),
                             number => WTERMSIG($exit_status)));
                    busy_dialog_run($dialog);
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
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }

    }

    return 1;

}
#
##############################################################################
#
#   Routine      - shell_command
#
#   Description  - Run the specified command in a shell. A simple replacement
#                  for system(), which interferes with signal handling on some
#                  systems.
#
#   Data         - @cmd : The command that is to be run. This can be in the
#                         form of either a single string containing the
#                         command with space separated arguments (optionally
#                         using special shell characters if so desired) or
#                         each work separated out into a list of separate
#                         strings.
#
##############################################################################



sub shell_command(@)
{

    my @cmd = @_;

    my ($child,
        $exit_status);

    if (($child = fork()) == 0)
    {

        # In Perl all file handles have FD_CLOEXEC set by default and so we
        # only need to worry about resetting signal handlers to their default
        # disposition before making the exec call.

        foreach my $sig (keys(%SIG))
        {
            $SIG{$sig} = "DEFAULT";
        }
        exec(@cmd) or _exit($!);

    }

    # Wait a bit and them make sure that things have worked ok.

    sleep(1);
    if (! defined($child)
        || (cached_waitpid($child, WNOHANG, \$exit_status) == $child
            && ! (WIFEXITED($exit_status) && WEXITSTATUS($exit_status) == 0)))
    {
        $! = WEXITSTATUS($exit_status)
            if (defined($exit_status) && WIFEXITED($exit_status));
        my $prog = (split(/ /, $cmd[0]))[0];
        my $dialog = Gtk2::MessageDialog->new_with_markup
            (undef,
             ["modal"],
             "warning",
             "close",
             __x("The {name} subprocess could not start,\n"
                     . "the system gave:\n<b><i>{error_message}</i></b>",
                 name => Glib::Markup::escape_text($prog),
                 error_message => Glib::Markup::escape_text($!)));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

}
#
##############################################################################
#
#   Routine      - get_dir_contents
#
#   Description  - Given a path and a Monotone manifest, return a subset of
#                  the manifest that represents the contents of just that
#                  directory along with the directory entry names.
#
#   Data         - $path     : The path to the directory from the top level of
#                              the manifest.
#                  $manifest : A reference to a Monotone manifest.
#                  $result   : A reference to a list that is to contain the
#                              result (a list of records containing the short
#                              directory entry name and a reference to the
#                              related manifest entry).
#
##############################################################################



sub get_dir_contents($$$)
{

    my ($path, $manifest, $result) = @_;

    my (@dir_list,
        $entry,
        $extract_re,
        @file_list,
        $list,
        $match_re,
        $name);

    # Manifests are already sorted alphabetically. However, if the user wishes
    # it, place folders before files.

    if ($path eq "")
    {
        $match_re = qr/^[^\/]+$/;
        $extract_re = qr/^([^\/]+)$/;
    }
    else
    {
        $match_re = qr/^${path}\/[^\/]+$/;
        $extract_re = qr/^${path}\/([^\/]+)$/;
    }
    @$result = ();
    foreach $entry (@$manifest)
    {
        if ($entry->{name} =~ m/$match_re/)
        {
            ($name) = ($entry->{name} =~ m/$extract_re/);
            if ($user_preferences->{folders_come_first})
            {
                if ($entry->{type} eq "directory")
                {
                    $list = \@dir_list;
                }
                else
                {
                    $list = \@file_list;
                }
            }
            else
            {
                $list = $result;
            }
            push(@$list, {manifest_entry => $entry, name => $name});
        }
    }
    if ($user_preferences->{folders_come_first})
    {
        @$result = (@dir_list, @file_list);
    }

}
#
##############################################################################
#
#   Routine      - open_database
#
#   Description  - Allows the user to select a Monotone Database and then
#                  opens it, making sure that it is a valid database or
#                  dealing with the consequences if it isn't.
#
#   Data         - $parent      : The parent window for any dialogs that are
#                                 to be displayed.
#                  $mtn         : A reference to a variable that is to contain
#                                 the newly created Monotone::AutomateStdio
#                                 object. This parameter can be undef if the
#                                 object is not required.
#                  $file_name   : A reference to a variable that is to contain
#                                 the full file name of the selected database.
#                                 This parameter can be undef if the file name
#                                 is not required.
#                  Return Value : True on success, otherwise false on
#                                 cancellation.
#
##############################################################################



sub open_database($$$)
{

    my ($parent, $mtn, $file_name) = @_;

    my ($chooser_dialog,
        $done,
        $ret_val);

    $chooser_dialog = Gtk2::FileChooserDialog->new(__("Open Database"),
                                                   $parent,
                                                   "open",
                                                   "gtk-cancel" => "cancel",
                                                   "gtk-open" => "ok");
    $chooser_dialog->
        set_current_folder($file_chooser_dir_locations{open_db_dir})
        if (exists($file_chooser_dir_locations{open_db_dir}));

    do
    {
        if (busy_dialog_run($chooser_dialog) eq "ok")
        {

            my ($fname,
                $mtn_obj);

            $fname = $chooser_dialog->get_filename();
            if (check_and_open_database($parent, $fname, \$mtn_obj))
            {

                # Seems to be ok so tell the caller.

                $$mtn = $mtn_obj if (defined($mtn));
                $$file_name = $fname if (defined($file_name));
                $done = $ret_val = 1;

            }

        }
        else
        {
            $done = 1;
        }
    }
    while (! $done);

    $file_chooser_dir_locations{open_db_dir} =
        $chooser_dialog->get_current_folder()
        if (defined($chooser_dialog->get_current_folder()));
    $chooser_dialog->destroy();

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - check_and_open_database
#
#   Description  - Opens the specified database making sure that it is
#                  accessible and a valid database or telling the user if it
#                  isn't.
#
#   Data         - $parent      : The parent window for any dialogs that are
#                                 to be displayed.
#                  $database    : The name of the Monotone database that is to
#                                 be opened.
#                  $mtn         : A reference to a variable that is to contain
#                                 the newly created Monotone::AutomateStdio
#                                 object.
#                  Return Value : True on success, otherwise false on failure.
#
##############################################################################



sub check_and_open_database($$$)
{

    my ($parent, $database, $mtn) = @_;

    my ($fh,
        $mtn_obj,
        $ret_val);

    # First make sure we can open it for reading (I know I could use the -r
    # test but this takes care of any other unforeseen access problems as
    # well).

    if (! defined($fh = IO::File->new($database, "r")))
    {
        my $dialog = Gtk2::MessageDialog->new
            ($parent,
             ["modal"],
             "warning",
             "close",
             __x("Cannot open {database}.\n{error_message}.",
                 database => $database,
                 error_message => $!));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }
    else
    {

        $fh->close();
        $fh = undef;

        # Ok it is a readable file, so now actually try and open it.

        if (defined($mtn_obj = check_and_create_mtn_object($parent,
                                                           $database)))
        {

            # Seems to be ok so tell the caller.

            $$mtn = $mtn_obj;
            $ret_val = 1;

        }

    }

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - check_and_create_mtn_object
#
#   Description  - Opens the specified database making sure that it is a valid
#                  database or telling the user if it isn't.
#
#   Data         - $parent             : The parent window for any dialogs
#                                        that are to be displayed.
#                  $database           : Either the name of the Monotone
#                                        database that is to be opened or
#                                        undef if the current workspace is to
#                                        be used.
#                  $suppress_ws_errors : True if `Invalid workspace...' type
#                                        errors are not to be displayed to the
#                                        user. This is optional.
#                  Return Value        : Either a newly created
#                                        Monotone::AutomateStdio object on
#                                        success, otherwise undef on failure.
#
##############################################################################



sub check_and_create_mtn_object($$;$)
{

    my ($parent, $database, $suppress_ws_errors) = @_;

    my $mtn;

    # Try and open the specified database but deal with any errors in a nicer
    # way than normal.

    CachingAutomateStdio->register_error_handler
        (MTN_SEVERITY_ALL,
         sub {
             my ($severity, $message) = @_;
             if (! $suppress_ws_errors || $message !~ m/^Invalid workspace/)
             {
                 my $dialog;
                 cleanup_mtn_error_message(\$message, 1);
                 $dialog = Gtk2::MessageDialog->new_with_markup
                     ($parent,
                      ["modal"],
                      "warning",
                      "close",
                      __x("There is a problem opening {database}. The details "
                              . "are:\n<b><i>{error_message}</i></b>",
                          database => defined($database)
                                          ? $database
                                          : __("the workspace's database"),
                          error_message =>
                              Glib::Markup::escape_text($message)));
                 busy_dialog_run($dialog);
                 $dialog->destroy();
             }
             die("Bad open");
         });
    eval
    {
        $mtn = CachingAutomateStdio->new($database);
    };
    CachingAutomateStdio->register_error_handler(MTN_SEVERITY_ALL,
                                                 \&mtn_error_handler);

    return $mtn;

}
#
##############################################################################
#
#   Routine      - save_as_file
#
#   Description  - Allows the user to save the specified data as a file on
#                  disk.
#
#   Data         - $parent    : The parent window for any dialogs that are to
#                               be displayed.
#                  $file_name : The suggested name of the file that is to be
#                               saved.
#                  $data      : A reference to a variable containing the raw
#                               file data.
#
##############################################################################



sub save_as_file($$$)
{

    my ($parent, $file_name, $data) = @_;

    my ($chooser_dialog,
        $continue,
        $done);

    $chooser_dialog = Gtk2::FileChooserDialog->new(__("Save As"),
                                                   $parent,
                                                   "save",
                                                   "gtk-cancel" => "cancel",
                                                   "gtk-save" => "ok");
    $chooser_dialog->set_current_name($file_name) if ($file_name ne "");
    $chooser_dialog->
        set_current_folder($file_chooser_dir_locations{save_as_dir})
        if (exists($file_chooser_dir_locations{save_as_dir}));

    do
    {
        if (busy_dialog_run($chooser_dialog) eq "ok")
        {

            my ($fh,
                $fname);

            $continue = 1;
            $fname = $chooser_dialog->get_filename();

            # See if the file exists, if so then get a confirmation from the
            # user.

            if (-e $fname)
            {
                my $dialog = Gtk2::MessageDialog->new
                    ($parent,
                     ["modal"],
                     "question",
                     "yes-no",
                     __("File already exists.\nDo you want to replace it?"));
                $dialog->set_title(__("Confirm"));
                $continue = 0 if (busy_dialog_run($dialog) ne "yes");
                $dialog->destroy();
            }

            if ($continue)
            {

                # Attempt to save the contents to the file.

                if (! defined($fh = IO::File->new($fname, "w")))
                {
                    my $dialog = Gtk2::MessageDialog->new
                        ($parent,
                         ["modal"],
                         "warning",
                         "close",
                         __x("Cannot open file:\n{error_message}.",
                             error_message => $!));
                    busy_dialog_run($dialog);
                    $dialog->destroy();
                }
                else
                {
                    binmode($fh);
                    $fh->print($$data);
                    $fh->close();
                    $done = 1;
                }

            }

        }
        else
        {
            $done = 1;
        }
    }
    while (! $done);

    $file_chooser_dir_locations{save_as_dir} =
        $chooser_dialog->get_current_folder()
        if (defined($chooser_dialog->get_current_folder()));
    $chooser_dialog->destroy();

}
#
##############################################################################
#
#   Routine      - revision_context_textview_popup_menu_item_cb
#
#   Description  - Callback routine called when the user selects an item on
#                  a textview's popup menu that acts upon the revision context
#                  underneath the mouse cursor.
#
#   Data         - $widget  : The widget object that received the signal.
#                  $menu    : The Gtk2::Menu widget that is to be updated.
#                  $details : A reference to an anonymous hash containing the
#                             window instance, callback routine, progress
#                             message, the revision id (or part) extracted
#                             from the textview listing and the Gtk2::TextIter
#                             for the start of the text line under the mouse
#                             when the user right clicked.
#
##############################################################################



sub revision_context_textview_popup_menu_item_cb($$)
{

    my ($widget, $details) = @_;

    return if ($details->{instance}->{in_cb});
    local $details->{instance}->{in_cb} = 1;

    my ($nr_revisions,
        @revision_ids);
    my $wm = WindowManager->instance();

    $wm->make_busy($details->{instance}, 1);
    $details->{instance}->{appbar}->
        push($details->{instance}->{appbar}->get_status()->get_text());
    $details->{instance}->{appbar}->set_status($details->{progress_message});
    $wm->update_gui();

    $details->{instance}->{mtn}->
        select(\@revision_ids, "i:" . $details->{revision_id});
    $nr_revisions = scalar(@revision_ids);
    if ($nr_revisions == 1)
    {
        $details->{cb}($details->{instance},
                       $revision_ids[0],
                       $details->{iter});
    }
    else
    {
        my $dialog = Gtk2::MessageDialog->new
            ($details->{instance}->{window},
             ["modal"],
             "warning",
             "close",
             ($nr_revisions == 0)
                 ? __("Cannot access a valid revision id.")
                 : __("Cannot access a unique revision id."));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

    $details->{instance}->{appbar}->pop();
    $wm->make_busy($details->{instance}, 0);

}
#
##############################################################################
#
#   Routine      - treeview_setup_search_column_selection
#
#   Description  - Setup the specified treeview column headers so that the
#                  user can select which column to search in.
#
#   Data         - $treeview  : The treeview widget that is to have this
#                               feature enabled on it.
#                  @columns   : A list of column numbers that are to be setup.
#
##############################################################################



sub treeview_setup_search_column_selection($@)
{

    my ($treeview, @columns) = @_;

    foreach my $col_nr (@columns)
    {

        my ($button,
            $col,
            $label);

        next unless (defined($col = $treeview->get_column($col_nr)));

        # We need to add a widget if we are going to get back a button widget
        # from $treeview->get_parent() (this is just how Gtk2 works, I guess
        # the header widgets are by default some sort of cut down affair).

        $label = Gtk2::Label->new($col->get_title());
        $col->set_widget($label);
        $label->show();

        # Find the header button widget.

        for ($button = $col->get_widget();
             defined($button) && ! $button->isa("Gtk2::Button");
             $button = $button->get_parent())
        {
        }
        next unless (defined($button));

        # Attach a mouse button press event callback to the column header
        # button.

        $button->signal_connect
            ("button_press_event",
             sub {

                 my ($widget, $event, $data) = @_;

                 # We are only interested in right button mouse clicks.

                 return FALSE unless ($event->button() == 3);

                 my ($menu,
                     $menu_item);

                 # Create a popup menu with the search option in it.

                 $menu = Gtk2::Menu->new();
                 $menu_item =
                     Gtk2::MenuItem->new(__("Select As Search Column"));
                 $menu->append($menu_item);
                 $menu_item->show();

                 # Setup a callback that will set up that column for searching
                 # if the user should select the option.

                 $menu_item->signal_connect
                     ("activate",
                      sub {

                          my ($widget, $data) = @_;

                          $data->{treeview}->
                              set_search_column($data->{col_nr});

                      },
                      $data);

                 # Display the popup menu.

                 $menu->popup(undef,
                              undef,
                              undef,
                              undef,
                              $event->button(),
                              $event->time());

                 return TRUE;

             },
             {treeview => $treeview,
              col_nr   => $col_nr});

    }

}
#
##############################################################################
#
#   Routine      - treeview_column_searcher
#
#   Description  - Callback routine used for comparing search terms with data
#                  inside a particular treeview's cell.
#
#   Data         - $model       : The underlying data model used by the
#                                 treeview.
#                  $column      : The number of the search column.
#                  $key         : The data that is to be searched for.
#                  $iter        : The treeview iterator for the row that is to
#                                 be compared.
#                  Return Value : TRUE if there is no match, otherwise FALSE
#                                 if there is.
#
##############################################################################



sub treeview_column_searcher($$$$)
{

    my ($model, $column, $key, $iter) = @_;

    my ($re,
        $value);

    # Get the value in the treeview's cell.

    $value = $model->get($iter, $column);

    # Compile the user's search term (either as a regular expression or plain
    # text depending upon the user's preferences) and return a no-match if it
    # doesn't compile.

    eval
    {
        if ($user_preferences->{list_search_as_re})
        {
            $re = qr/$key/;
        }
        else
        {
            $re = qr/\Q$key\E/;
        }
    };
    return TRUE if ($@);

    # Actually do the match.

    if ($value =~ m/$re/)
    {
        return FALSE;
    }
    else
    {
        return TRUE;
    }

}
#
##############################################################################
#
#   Routine      - get_branch_revisions
#
#   Description  - Get a list of revision ids or tags for the specified branch
#                  that take into account the user's preferences for ordering
#                  and the maximum number of revisions to display.
#
#   Data         - $mtn       : The Monotone::AutomateStdio object that is to
#                               be used.
#                  $branch    : The name of the branch that revisions are to
#                               be found for.
#                  $tags      : True if the list of revisions are to be tags,
#                               otherwise false if they are to be ids.
#                  $appbar    : If defined, the application progress bar
#                               widget that is to be updated with the progress
#                               of this operation. It is assumed that the
#                               progress is set at 0 and will end up being set
#                               to 1.
#                  $revisions : A reference to a list that is to contain the
#                               resultant list of sorted revision tags or ids.
#
##############################################################################



sub get_branch_revisions($$$$$)
{

    my ($mtn, $branch, $tags, $appbar, $revisions) = @_;

    my $wm = WindowManager->instance();

    @$revisions = ();

    # If we have a branch name then escape any special selection characters in
    # it before we use it.

    if (defined($branch) && $branch ne "")
    {
        $branch =~ s/$select_escape_re/\\$1/g;
    }

    # Now get the revisions matching the branch name, either as tags or as
    # revision ids depending upon what the caller has asked for.

    if ($tags)
    {

        my (%rev_id_to_tags,
            %seen,
            @sorted_rev_ids,
            @tags);

        # Get the list of revision tags.

        {
            local $pulse_widget = $appbar->get_progress()
                if (defined($appbar));
            $mtn->tags(\@tags, $branch);
        }
        $appbar->set_progress_percentage(0.5) if (defined($appbar));
        $wm->update_gui();

        # Does the list need truncating (in which case we need to sort by date
        # to keep the most recent tags) or does the user want to sort tags by
        # date?

        if (($user_preferences->{query}->{tagged}->{limit} > 0
             && scalar(@tags) > $user_preferences->{query}->{tagged}->{limit})
            || $user_preferences->{query}->{tagged}->{sort_chronologically})
        {

            # Yes tags are to be either sorted by date or need to be truncated
            # (requiring them to temporarily be sorted by date).

            # Build up a hash mapping revision id to tag(s).

            foreach my $tag (@tags)
            {
                if (exists($rev_id_to_tags{$tag->{revision_id}}))
                {
                    push(@{$rev_id_to_tags{$tag->{revision_id}}}, $tag->{tag});
                }
                else
                {
                    $rev_id_to_tags{$tag->{revision_id}} = [$tag->{tag}];
                }
            }

            # Sort the revision ids into date order (youngest first).

            $mtn->toposort(\@sorted_rev_ids, keys(%rev_id_to_tags));
            @sorted_rev_ids = reverse(@sorted_rev_ids);

            # Now build up a list of tags based on this ordering, deduping
            # items and stopping when we have enough tags.

            revision: foreach my $rev_id (@sorted_rev_ids)
            {
                foreach my $tag (sort(@{$rev_id_to_tags{$rev_id}}))
                {
                    push(@$revisions, $tag) if (! $seen{$tag} ++);
                    last revision
                        if ($user_preferences->{query}->{tagged}->{limit} > 0
                            && scalar(@$revisions) >=
                                $user_preferences->{query}->{tagged}->{limit});
                }
            }

        }
        else
        {

            # No tags are to be sorted by name, without truncation.

            # At this stage simply extract the tags and dedupe them.

            @$revisions = map($_->{tag}, grep(! $seen{$_->{tag}} ++, @tags));

        }

        # We now have a list of tags in @$revisions of the correct size and
        # sorted by date if so required by the user. So resort the list
        # aplhabetically if required.

        @$revisions = sort(@$revisions)
            if (! $user_preferences->{query}->{tagged}->
                    {sort_chronologically});

    }
    else
    {

        # Get the list of revision ids, if no branch is specified then get all
        # of the revisions within the database.

        if (defined($branch) && $branch ne "")
        {
            local $pulse_widget = $appbar->get_progress()
                if (defined($appbar));
            $mtn->select($revisions, "b:" . $branch);
        }
        else
        {
            local $pulse_widget = $appbar->get_progress()
                if (defined($appbar));
            $mtn->select($revisions, "i:");
        }

        # Does it need truncating?

        if ($user_preferences->{query}->{id}->{limit} == 0
            || scalar(@$revisions)
                <= $user_preferences->{query}->{id}->{limit})
        {

            # No so simply sort it.

            if ($user_preferences->{query}->{id}->{sort_chronologically})
            {
                $appbar->set_progress_percentage(0.33) if (defined($appbar));
                $wm->update_gui();
                $mtn->toposort($revisions, @$revisions);
                $appbar->set_progress_percentage(0.66) if (defined($appbar));
                $wm->update_gui();
                @$revisions = reverse(@$revisions);
            }
            else
            {
                $appbar->set_progress_percentage(0.5) if (defined($appbar));
                $wm->update_gui();
                @$revisions = sort(@$revisions);
            }

        }
        else
        {

            # Yes so truncate and then sort it.

            $appbar->set_progress_percentage(0.33) if (defined($appbar));
            $wm->update_gui();
            $mtn->toposort($revisions, @$revisions);
            $appbar->set_progress_percentage(0.66) if (defined($appbar));
            splice(@$revisions,
                   0,
                   scalar(@$revisions)
                       - $user_preferences->{query}->{id}->{limit});
            if ($user_preferences->{query}->{id}->{sort_chronologically})
            {
                @$revisions = reverse(@$revisions);
            }
            else
            {
                @$revisions = sort(@$revisions);
            }

        }

    }

    $appbar->set_progress_percentage(1) if (defined($appbar));
    $wm->update_gui();

}
#
##############################################################################
#
#   Routine      - get_revision_ids
#
#   Description  - Return the currently selected revision id, whether this is
#                  specified via a tag or as a revision id.
#
#   Data         - $instance     : The window instance.
#                  $revision_ids : A reference to a list that is to contain
#                                  the revision ids. Normally the list will
#                                  have at most one element but may contain
#                                  more if the tag isn't unique on the current
#                                  branch.
#                  $tag          : A reference to a variable that is to
#                                  contain the tag name that the user selected
#                                  or undef if the user selected a revision id
#                                  directly. This is optional.
#
##############################################################################



sub get_revision_ids($$;$)
{

    my ($instance, $revision_ids, $tag) = @_;

    @$revision_ids = ();
    $$tag = undef if (defined($tag));
    return unless ($instance->{revision_combo_details}->{complete});
    if ($instance->{tagged_checkbutton}->get_active())
    {
        my $escaped_value;
        my $query = "";
        if ($instance->{branch_combo_details}->{complete})
        {
            $escaped_value = $instance->{branch_combo_details}->{value};
            $escaped_value =~ s/$select_escape_re/\\$1/g;
            $query = "b:" . $escaped_value . "/";
        }
        $escaped_value = $instance->{revision_combo_details}->{value};
        $escaped_value =~ s/$select_escape_re/\\$1/g;
        $query .= "t:" . $escaped_value;
        $instance->{mtn}->select($revision_ids, $query);
        $$tag = $instance->{revision_combo_details}->{value}
            if (defined($tag));
    }
    else
    {
        push(@$revision_ids, $instance->{revision_combo_details}->{value});
    }

}
#
##############################################################################
#
#   Routine      - cache_extra_file_info
#
#   Description  - Cache extra information about a file in its manifest entry
#                  record.
#
#   Data         - $mtn            : The Monotone::AutomateStdio object that
#                                    is to be used.
#                  $revision_id    : The revision id from where the search for
#                                    the latest file update is to start,
#                                    working backwards.
#                  $manifest_entry : A reference to the file's manifest entry.
#
##############################################################################



sub cache_extra_file_info($$$)
{

    my ($mtn, $revision_id, $manifest_entry) = @_;

    my (@certs_list,
        @revision_list);

    if (exists($manifest_entry->{content_mark}))
    {
        $revision_list[0] = $manifest_entry->{content_mark};
    }
    else
    {
        $mtn->get_content_changed(\@revision_list,
                                  $revision_id,
                                  $manifest_entry->{name});
        if (scalar(@revision_list) > 1)
        {
            $mtn->toposort(\@revision_list, @revision_list);
            @revision_list = reverse(@revision_list);
        }
    }
    $manifest_entry->{last_changed_revision} = $revision_list[0];
    $mtn->certs(\@certs_list, $revision_list[0]);
    $manifest_entry->{author} = $manifest_entry->{last_update} = "";
    foreach my $cert (@certs_list)
    {
        if ($cert->{name} eq "author")
        {
            $manifest_entry->{author} = $cert->{value};
        }
        elsif ($cert->{name} eq "date")
        {
            $manifest_entry->{last_update} = $cert->{value};
        }
        last if ($manifest_entry->{author} ne ""
                 && $manifest_entry->{last_update} ne "");
    }

}
#
##############################################################################
#
#   Routine      - file_glob_to_regexp
#
#   Description  - Converts the specified string containing a file name style
#                  glob into a regular expression.
#
#   Data         - $file_glob   : The file name wildcard that is to be
#                                 converted.
#                  Return Value : The resultant regular expression string.
#
##############################################################################



sub file_glob_to_regexp($)
{

    my $file_glob = $_[0];

    my ($escaping,
        $first,
        $re_text);

    $escaping = 0;
    $first = 1;
    $re_text = "^";
    foreach my $char (split(//, $file_glob))
    {
        if ($first)
        {
            $re_text .= "(?=[^\\.])" unless $char eq ".";
            $first = 0;
        }
        if (".+^\$\@%()|" =~ m/\Q$char\E/)
        {
            $re_text .= "\\" . $char;
        }
        elsif ($char eq "*")
        {
            $re_text .= $escaping ? "\\*" : "[^/]*";
        }
        elsif ($char eq "?")
        {
            $re_text .= $escaping ? "\\?" : "[^/]";
        }
        elsif ($char eq "\\")
        {
            if ($escaping)
            {
                $re_text .= "\\\\";
                $escaping = 0;
            }
            else
            {
                $escaping = 1;
            }
        }
        else
        {
            $re_text .= "\\" if ($escaping && $char eq "[");
            $re_text .= $char;
            $escaping = 0;
        }
    }
    $re_text .= "\$";

    return $re_text;

}
#
##############################################################################
#
#   Routine      - program_valid
#
#   Description  - Validate the specified program by seeing if it can be
#                  found. If a parent window is provided and the specified
#                  program cannot be found then display a dialog window
#                  telling the user about the problem.
#
#   Data         - $program     : The name of the program or the full path to
#                                 the program that is to be validated.
#                  $parent      : The parent window for any dialogs that are
#                                 to be displayed. This is optional.
#                  Return Value : True if the program is valid, otherwise
#                                 false if it is not.
#
##############################################################################



sub program_valid($;$)
{

    my ($program, $parent) = @_;

    my $found;

    if (File::Spec->file_name_is_absolute($program))
    {
        $found = 1 if (-x $program || -f ($program . ".exe"));
    }
    else
    {
        foreach my $dir (File::Spec->path())
        {
            if (-x File::Spec->catfile($dir, $program)
                || -f File::Spec->catfile($dir, $program . ".exe"))
            {
                $found = 1;
                last;
            }
        }
    }

    # Tell the user that the program can't be found if that is the case and the
    # caller wants us to inform the user.

    if (! $found && defined($parent))
    {
        my $dialog = Gtk2::MessageDialog->new
            ($parent,
             ["modal"],
             "info",
             "close",
             __x("The program `{program_name}' cannot be found.\n"
                     . "Is it installed?",
                 program_name => $program));
        busy_dialog_run($dialog);
        $dialog->destroy();
    }

    return $found;

}
#
##############################################################################
#
#   Routine      - handle_comboxentry_history
#
#   Description  - Handle comboboxentry histories. Histories are limited to a
#                  small fixed value and are stored to disk in the user's
#                  preferences file.
#
#   Data         - $widget       : The comboboxentry that is to be updated.
#                  $history_name : The name of the history list that is to be
#                                  updated or loaded.
#                  $value        : The new value that is to be added to the
#                                  specified history list and comboboxentry or
#                                  undef if the comboboxentry is just to be
#                                  updated with the current history list. This
#                                  is optional.
#
##############################################################################



sub handle_comboxentry_history($$;$)
{

    my ($widget, $history_name, $value) = @_;

    # Update the comboxentry history list, saving it to disk, and then update
    # the comboboxentry itself if the history has changed or no value was
    # supplied (done when initialising the comboboxentry widget for the first
    # time).

    if (handle_history($history_name,
                       $value,
                       $user_preferences->{history_size})
        || ! defined($value))
    {

        my $text_entry_value = $widget->child()->get_text();

        $widget->get_model()->clear();
        foreach my $entry (@{$user_preferences->{histories}->{$history_name}})
        {
            $widget->append_text($entry);
        }

        # Restore original text entry value.

        $widget->child()->set_text($text_entry_value);

    }

}
#
##############################################################################
#
#   Routine      - handle_history
#
#   Description  - Handle widget histories. Histories are limited to the
#                  specified size and are stored to disk in the user's
#                  preferences file.
#
#   Data         - $history_name : The name of the history list that is to be
#                                  updated or loaded.
#                  $value        : The new value that is to be added to the
#                                  specified history list.
#                  $max_size     : The maximum number of items that can be
#                                  stored in the history list.
#                  Return Value  : True if the history list was in fact
#                                  updated, otherwise false if no update was
#                                  necessary because the value is already in
#                                  the list.
#
##############################################################################



sub handle_history($$$)
{

    my ($history_name, $value, $max_size) = @_;

    # Automatically create the history entry if there isn't one already.

    $user_preferences->{histories}->{$history_name} = []
        unless (defined($user_preferences->{histories}->{$history_name}));

    # Update the history list only if there is something worth saving.

    if (defined($value) && $value ne "")
    {

        my $history_ref = $user_preferences->{histories}->{$history_name};

        # Check to see if the value is already in the list, if it is then there
        # is no need to update the history list.

        foreach my $entry (@$history_ref)
        {
            return if ($entry eq $value);
        }

        # Ok we need to update the history list by sticking the new entry at
        # the front.

        splice(@$history_ref, $max_size)
            if (unshift(@$history_ref, $value) > $max_size);
        save_preferences($user_preferences);

        return 1;

    }

    return;

}
#
##############################################################################
#
#   Routine      - display_help
#
#   Description  - Displays the specified help section either in Gnome's
#                  native help browser or the specified HTML viewer, depending
#                  upon installation settings.
#
#   Data         - $section - The help section to display. If undef is given
#                             then the content page is displayed. This is
#                             optional.
#
##############################################################################



sub display_help(;$)
{

    my $section = $_[0];

    if (HTML_VIEWER_CMD eq "")
    {

        # Simply let Gnome handle it using yelp.

        if (defined($section))
        {
            Gnome2::Help->display("mtn-browse.xml", $section);
        }
        else
        {
            Gnome2::Help->display("mtn-browse.xml");
        }

    }
    else
    {

        my $url;

        # Use the specified HTML viewer to display the help section.

        $section = "index" unless defined($section);
        if (exists($help_ref_to_url_map{$section}))
        {
            $url = $help_ref_to_url_map{$section};
        }
        else
        {
            $url = $help_ref_to_url_map{"index"};
        }
        if (! defined($url) || $url eq "")
        {
            my $dialog = Gtk2::MessageDialog->new
                (undef,
                 ["modal"],
                 "warning",
                 "close",
                 __("The requested help section\n"
                    . "cannot be found or is not known."));
            busy_dialog_run($dialog);
            $dialog->destroy();
            return;
        }
        display_html($url);

    }

}
#
##############################################################################
#
#   Routine      - display_html
#
#   Description  - Displays the specified HTML URL either in Gnome's native
#                  web browser or the specified HTML viewer, depending upon
#                  installation settings.
#
#   Data         - $url - The HTML URL that is to be displayed.
#
##############################################################################



sub display_html($)
{

    my $url = $_[0];

    if (HTML_VIEWER_CMD eq "")
    {

        # Simply let Gnome handle it.

        Gnome2::URL->show($url);

    }
    else
    {

        my $cmd = HTML_VIEWER_CMD;

        if ($cmd =~ m/\{url\}/)
        {
            $cmd =~ s/\{url\}/$url/g;
        }
        else
        {
            $cmd .= " " . $url;
        }

        # Launch it.

        shell_command($cmd);

    }

}
#
##############################################################################
#
#   Routine      - register_help_callbacks
#
#   Description  - Register all of the context sensitive help callbacks for
#                  the specified window instance.
#
#   Data         - $instance : The window instance.
#                  $glade    : The glade object associated with the window
#                              instance.
#                  $details  : A list of records containing the widget and the
#                              help reference for that widget. If a widget is
#                              set to undef then that entry represents the
#                              default help callback for that entire window.
#
##############################################################################



sub register_help_callbacks($$@)
{

    my ($instance, $glade, @details) = @_;

    my $wm = WindowManager->instance();

    build_help_ref_to_url_map()
        if (HTML_VIEWER_CMD ne "" && keys(%help_ref_to_url_map) == 0);

    foreach my $entry (@details)
    {
        my $help_ref = $entry->{help_ref};
        my $widget = defined($entry->{widget})
            ? $glade->get_widget($entry->{widget}) : undef;
        $wm->help_connect($instance,
                          $widget,
                          sub {
                              my ($widget, $instance) = @_;
                              display_help($help_ref);
                          });
    }

}
#
##############################################################################
#
#   Routine      - create_format_tags
#
#   Description  - Creates the Gtk2::TextBuffer tags that are used to pretty
#                  print stuff.
#
#   Data         - $text_view : The GTK2::TextBuffer widget that is to have
#                               its tags created.
#
##############################################################################



sub create_format_tags($)
{

    my $text_buffer = $_[0];

    my $colours = $user_preferences->{colours};

    # Normal Black text, assorted styles, on a white background.

    $text_buffer->create_tag("normal", "weight" => PANGO_WEIGHT_NORMAL);

    $text_buffer->create_tag("bold", "weight" => PANGO_WEIGHT_BOLD);
    $text_buffer->create_tag("italics", "style" => "italic");
    $text_buffer->create_tag("bold-italics",
                             "weight" => PANGO_WEIGHT_BOLD,
                             "style" => "italic");

    # Set up the colour and style schemes for file comparison and annotation.

    foreach my $i (1 .. 2)
    {
        my $clr = $user_preferences->{colours}->{"cmp_revision_" . $i};
        $text_buffer->create_tag("compare-" . $i,
                                 "foreground" => $clr->{fg});
        $text_buffer->create_tag("bold-compare-" . $i,
                                 "weight" => PANGO_WEIGHT_BOLD,
                                 "foreground" => $clr->{fg});
        $text_buffer->create_tag("italics-compare-" . $i,
                                 "style" => "italic",
                                 "foreground" => $clr->{fg});
        $text_buffer->create_tag("bold-italics-compare-" . $i,
                                 "weight" => PANGO_WEIGHT_BOLD,
                                 "style" => "italic",
                                 "foreground" => $clr->{fg});
        $text_buffer->create_tag("compare-file-" . $i,
                                 "foreground" => $clr->{fg},
                                 "background" => $clr->{bg});
        $text_buffer->create_tag("compare-file-info-" . $i,
                                 "weight" => PANGO_WEIGHT_BOLD,
                                 "foreground" => $clr->{hl},
                                 "background" => "DarkSlateGrey");
        foreach my $prefix ("annotate_prefix_", "annotate_text_")
        {
            my $tag = $prefix;
            $tag =~ s/_/-/g;
            $clr = $user_preferences->{colours}->{$prefix . $i};
            $text_buffer->create_tag($tag . $i,
                                     "foreground" => $clr->{fg},
                                     "background" => $clr->{bg});
        }
    }

    # Yellow text on a grey background.

    $text_buffer->create_tag("compare-info",
                             "foreground" => "Yellow",
                             "background" => "LightSlateGrey");

}
#
##############################################################################
#
#   Routine      - hex_dump
#
#   Description  - Generates a hexadecimal dump of the specified data.
#
#   Data         - $data        : A reference to the data that is to be hex
#                                 dumped.
#                  Return Value : A reference to the resultant hex dump as a
#                                 string.
#
##############################################################################



sub hex_dump($)
{

    my $data = $_[0];

    my ($buffer,
        $counter,
        @line);

    $counter = 0;
    foreach my $byte (split(//, $$data))
    {
        ++ $counter;
        push(@line, $byte);
        $buffer .= sprintf("%02X ", ord($byte));
        $buffer .= " " if (($counter % 8) == 0);
        if (($counter % 16) == 0)
        {
            foreach my $byte2 (@line)
            {
                $buffer .= ($byte2 =~ m/[[:print:]]/) ? (" " . $byte2) : " .";
            }
            $buffer .= "\n";
            @line = ();
        }
    }

    # If the last line is incomplete then finish it off.

    if (scalar(@line) > 0)
    {
        $buffer .= "   " x (16 - scalar(@line));
        $buffer .= " " if (scalar(@line) < 8);
        $buffer .= " ";
        foreach my $byte2 (@line)
        {
            $buffer .= ($byte2 =~ m/[[:print:]]/) ? (" " . $byte2) : " .";
        }
        $buffer .= "\n";
    }

    return \$buffer;

}
#
##############################################################################
#
#   Routine      - data_is_binary
#
#   Description  - Determines whether the specified string contains binary
#                  data.
#
#   Data         - $data        : A reference to the data that is to be
#                                 tested.
#                  Return Value : True if the data is binary, otherwise false
#                                 if it is predominantly textual.
#
##############################################################################



sub data_is_binary($)
{

    my $data = $_[0];

    my ($chunk,
        $length,
        $non_printable,
        $offset,
        $total_length);

    $offset = 0;
    $total_length = length($$data);
    while ($offset < $total_length)
    {
        $chunk = substr($$data, $offset, CHUNK_SIZE);
        $offset += CHUNK_SIZE;
        $length = length($chunk);
        $non_printable =
            scalar(grep(/[^[:print:][:space:]]/, split(//, $chunk)));
        return 1
            if (((100 * $non_printable) / $length)
                > $user_preferences->{binary_threshold});
    }
    return;

}
#
##############################################################################
#
#   Routine      - colour_to_string
#
#   Description  - Returns a string representing the specified
#                  Gtk2::Gdk::Color value.
#
#   Data         - $colour      : A Gtk2::Gdk::Color object.
#                  Return Value : A string containing the colour value.
#
##############################################################################



sub colour_to_string($)
{

    my $colour = $_[0];

    return sprintf("#%02X%02X%02X",
                   ($colour->red() >> 8) & 0xff,
                   ($colour->green() >> 8) & 0xff,
                   ($colour->blue() >> 8) & 0xff);

}
#
##############################################################################
#
#   Routine      - set_window_size
#
#   Description  - Sets the size of the specified window based upon what has
#                  been previously saved or it's default window size if
#                  nothing has been saved.
#
#   Data         - $window : The window that is to be sized.
#                  $type   : The type of window that is to be sized.
#
##############################################################################



sub set_window_size($$)
{

    my ($window, $type) = @_;

    my ($height,
        $width);

    if (exists($user_preferences->{window_sizes}->{$type}))
    {
        $width = $user_preferences->{window_sizes}->{$type}->{width};
        $height = $user_preferences->{window_sizes}->{$type}->{height};
    }
    else
    {
        ($width, $height) = $window->get_default_size();
    }
    $window->resize($width, $height);

}
#
##############################################################################
#
#   Routine      - set_label_value
#
#   Description  - Set the text for the given label and the tooltip for the
#                  parent widget, assumed to be an event box, to the specified
#                  text.
#
#   Data         - $widget : The label widget that has an event box as its
#                            parent.
#                  $value  : The text that the label and tooltip are to be set
#                            to.
#
##############################################################################



sub set_label_value($$)
{

    my ($widget, $value) = @_;

    $widget->set_text($value);
    $tooltips->set_tip($widget->get_parent(), $value);

}
#
##############################################################################
#
#   Routine      - glade_signal_autoconnect
#
#   Description  - This routine uses the Glade library to connect up all the
#                  registered signal handlers to their related widgets.
#
#   Data         - $glade       : The Glade object describing the widgets that
#                                 are to have their signal handlers
#                                 registered.
#                  $client_data : The client data that is to be passed into
#                                 each callback routine when it is called.
#
##############################################################################



sub glade_signal_autoconnect($$)
{

    my ($glade, $client_data) = @_;

    my $caller_package = caller();
    $caller_package = "main" if (! defined($caller_package));

    $glade->signal_autoconnect
        (sub {
             my ($callback_name,
                 $widget,
                 $signal_name,
                 $signal_data,
                 $connect_object,
                 $after,
                 $user_data) = @_;
             my $func = $after ? "signal_connect_after" : "signal_connect";

             # Need to fully qualify any callback name that isn't prefixed by
             # it's package name with the name of the calling package.

             $callback_name = $caller_package . "::" . $callback_name
                 if (index($callback_name, "::") < 0);

             # Actually connect the signal handler.

             $widget->$func($signal_name,
                            $callback_name,
                            $connect_object ? $connect_object : $user_data);
         },
         $client_data);

}
#
##############################################################################
#
#   Routine      - adjust_time
#
#   Description  - This routine adjusts the specified gmtime()/localtime()
#                  style time by the specified time period.
#
#   Data         - $time_value : A reference to the gmtime()/localtime() style
#                                list that is to be adjusted.
#                  $period     : The time period to add or subtract.
#                  $units      : The units that the time period is expressed
#                                in.
#
##############################################################################



sub adjust_time($$$)
{

    my ($time_value, $period, $units) = @_;

    my $time;

    # Please note that values from gmtime()/localtime() etc start from 0. Also
    # the apparently needless:
    #     @time_value = gmtime(timegm(@time_value[0 .. 5))
    # is so that the wday, yday and isdst fields are correctly recalculated.

    if ($units == DURATION_MONTHS)
    {
        my ($day,
            $month,
            $year);
        ($day, $month, $year) = @$time_value[3, 4, 5];
        if (abs($period) > 12)
        {
            $year += floor($period / 12);
            $period %= 12;
        }
        if ($period >= 0)
        {
            if (($period + $month) > 11)
            {
                ++ $year;
                $month = $period + $month - 12;
            }
            else
            {
                $month += $period;
            }
        }
        else
        {
            $period = abs($period);
            if ($period > $month)
            {
                -- $year;
                $month = 12 - ($period - $month);
            }
            else
            {
                $month -= $period;
            }
        }
        $day = adjust_day_for_month($day, $month, $year);
        @$time_value[3, 4, 5] = ($day, $month, $year);
        $time = timegm(@$time_value[0 .. 5]);
    }
    elsif ($units == DURATION_YEARS)
    {
        my ($day,
            $month,
            $year);
        ($day, $month, $year) = @$time_value[3, 4, 5];
        $year += $period;
        $day = adjust_day_for_month($day, $month, $year);
        @$time_value[3, 4, 5] = ($day, $month, $year);
        $time = timegm(@$time_value[0 .. 5]);
    }
    else
    {
        $time = timegm(@$time_value[0 .. 5]);
        if ($units == DURATION_MINUTES)
        {
            $time += $period * 60;
        }
        elsif ($units == DURATION_HOURS)
        {
            $time += $period * 60 * 60;
        }
        elsif ($units == DURATION_DAYS)
        {
            $time += $period * 60 * 60 * 24;
        }
    }

    # Return the adjusted time back to the caller.

    @$time_value = gmtime($time);

}
#
##############################################################################
#
#   Routine      - mtn_time_string_to_locale_time_string
#
#   Description  - This routine converts a Monotone time string into a locale
#                  specific time string.
#
#   Data         - $time_string : The Monotone date/time string that is to be
#                                 converted.
#                  Return Value : The converted locale specific date/time
#                                 string.
#
##############################################################################



sub mtn_time_string_to_locale_time_string($)
{

    my $time_string = $_[0];

    my $time;

    return strftime("%x %X", localtime($time))
        if (defined($time = mtn_time_string_to_time($time_string)));

    return;

}
#
##############################################################################
#
#   Routine      - mtn_time_string_to_time
#
#   Description  - This routine converts a Monotone time string into a time_t
#                  value as returned from time().
#
#   Data         - $time_string : The Monotone date/time string that is to be
#                                 converted.
#                  Return Value : A time() style time code on success,
#                                 otherwise undef on failure.
#
##############################################################################



sub mtn_time_string_to_time($)
{

    my $time_string = $_[0];

    if ($time_string =~ m/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})$/)
    {

        my ($day_of_month,
            $hours,
            $minutes,
            $month,
            $seconds,
            $year);

        ($year, $month, $day_of_month, $hours, $minutes, $seconds) =
            ($1, $2, $3, $4, $5, $6);
        $month -= 1;
        $year -= 1900;

        # Remember that Monotone and its stdio interface works in GMT.

        return timegm($seconds,
                      $minutes,
                      $hours,
                      $day_of_month,
                      $month,
                      $year);

    }

    return;

}
#
##############################################################################
#
#   Routine      - calculate_update_interval
#
#   Description  - Given a list of items to display or process, calculate the
#                  update interval (in number of items processed) for updating
#                  the display.
#
#   Data         - $items       : Either a reference to a container containing
#                                 the items that are to be processed or a
#                                 scalar containing the number of items to
#                                 that are to be processed.
#                  $granularity : The number of times the display is to be
#                                 updated whilst processing the list of items.
#                                 This is optional.
#                  Return Value : The calculated update interval.
#
##############################################################################



sub calculate_update_interval($;$)
{

    my ($items, $granularity) = @_;

    my ($nr_items,
        $update_interval);

    $granularity = 20 unless (defined($granularity));
    if (ref($items) eq "ARRAY")
    {
        $nr_items = scalar(@$items);
    }
    elsif (ref($items) eq "HASH")
    {
        $nr_items = scalar(keys(%$items));
    }
    else
    {
        $nr_items = $items;
    }
    $update_interval = floor($nr_items / $granularity);
    $update_interval = 1 if ($update_interval < 1);

    return $update_interval;

}
#
##############################################################################
#
#   Routine      - busy_dialog_run
#
#   Description  - Makes all of the windows busy and then calls the specified
#                  dialog's run method. When the dialog returns the window
#                  state is returned to what it was before.
#
#   Data         - $item        : Either a dialog window or an instance record
#                                 representing a dialog window.
#                  Return Value : The value returned from the dialog's run
#                                 method.
#
##############################################################################



sub busy_dialog_run($)
{

    my $item = $_[0];

    my $choice;
    my $wm = WindowManager->instance();

    # If it's an unmanaged transient dialog window then make every managed
    # window busy and let the dialog window run unhindered, otherwise make sure
    # that the custom dialog window is not made busy when run.

    if (blessed($item) && $item->isa("Gtk2::Dialog"))
    {
        $wm->make_busy(undef, 1);
        $choice = $wm->allow_input(sub { return $item->run(); });
        $wm->make_busy(undef, 0);
    }
    else
    {
        $wm->make_busy($item, 1, 1);
        $choice = $wm->allow_input(sub { return $item->{window}->run(); });
        $wm->make_busy($item, 0);
    }

    return $choice;

}
#
##############################################################################
#
#   Routine      - cached_waitpid
#
#   Description  - Does the same thing as waitpid() but checks a list of
#                  reaped processes first.
#
#   Data         - $pid         : Either the id of the process that has is to
#                                 be checked or -1 if you want to check the
#                                 status of any waiting process.
#                  $flags       : The waitpid() style flags.
#                  $status      : A reference to a variable that is to contain
#                                 the exit status of the reaped process.
#                  Return Value : Either the process id of the reaped process,
#                                 0 if there are processes still running or -1
#                                 on error.
#
##############################################################################



sub cached_waitpid($$$)
{

    my ($pid, $flags, $status) = @_;

    my $ret_val;

    # First check the list of reaped processes (removing and returning the
    # entry if found).

    foreach my $i (reverse(0 .. $#reaped_process_details_list))
    {
        if ($pid == -1 || $reaped_process_details_list[$i]->{pid} == $pid)
        {
            $$status = $reaped_process_details_list[$i]->{status};
            splice(@reaped_process_details_list, $i, 1);
            return $pid;
        }
    }

    # Nothing can be found so make a call to waitpid().

    $ret_val = waitpid($pid, $flags);
    $$status = $?;

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - add_reaped_process_details
#
#   Description  - Records the details about a newly reaped process in the
#                  reaped processes list.
#
#   Data         - $pid    : The id of the process that has just been reaped.
#                  $status : The status of the process that has just been
#                            reaped.
#
##############################################################################



sub add_reaped_process_details($$)
{

    my ($pid, $status) = @_;

    # Simply push these details onto the reaped process details list.

    push(@reaped_process_details_list, {pid => $pid, status => $status});

}
#
##############################################################################
#
#   Routine      - cleanup_mtn_error_message
#
#   Description  - Attempts to cleanup the specified error string that
#                  originally came from Montone.
#
#   Data         - $message  : A reference to the buffer containing the error
#                              message that is to be cleaned up.
#                  $strip_nl : True if new line chacters are to be stripped
#                              out of the message, otherwise false if they are
#                              to be left alone (apart from the last one. This
#                              is optional.
#
##############################################################################



sub cleanup_mtn_error_message($;$)
{

    my ($message, $strip_nl) = @_;

    $$message =~ s/mtn: //g;
    $$message =~ s/misuse: //g;
    $$message =~ s/^Corrupt\/missing mtn [^\n]+\n//g;
    $$message =~ s/ at .+ line \d+$//g;
    $$message =~ s/\s+$//g;
    $$message =~ s/\n/ /g if ($strip_nl);
    $$message =~ s/\n$//g;
    $$message .= "." unless ($$message =~ m/.+\.$/);

}
#
##############################################################################
#
#   Routine      - build_help_ref_to_url_map
#
#   Description  - Build up a map that translates an HTML help file link name
#                  into a fully qualified URL.
#
#   Data         - None.
#
##############################################################################



sub build_help_ref_to_url_map()
{

    my ($dir,
        $dir_path,
        $fname,
        $locale,
        @lparts,
        $nr_parts,
        $prog,
        $tmp);

    # Ask Gnome where the based help directory is, failing that have an
    # educated guess.

    if (HTML_VIEWER_CMD eq ""
        && defined($prog = Gnome2::Program->get_program()))
    {
        ($dir_path) = $prog->locate_file("app-help", "mtn-browse.xml", FALSE);
        $dir_path = dirname($dir_path);
    }
    else
    {
        $dir_path = File::Spec->catfile(PREFIX_DIR,
                                        "share",
                                        "gnome",
                                        "help",
                                        APPLICATION_NAME);
    }

    # Work out the locale component, going from the most specific to the least.
    # If a specific locale directory isn't found then fall back onto the
    # POSIX/C locale.

    $locale = setlocale(LC_MESSAGES);
    @lparts = split(/[_.@]/, $locale);
    $nr_parts = scalar(@lparts);
    if (-d ($tmp = File::Spec->catfile($dir_path, $locale)))
    {
        $dir_path = $tmp;
    }
    elsif ($nr_parts >= 3
           && -d ($tmp = File::Spec->catfile($dir_path,
                                             $lparts[0] . "_"
                                             . $lparts[1] . "."
                                             . $lparts[2])))
    {
        $dir_path = $tmp;
    }
    elsif ($nr_parts >= 2
           && -d ($tmp = File::Spec->catfile($dir_path,
                                             $lparts[0] . "_" . $lparts[1])))
    {
        $dir_path = $tmp;
    }
    elsif ($nr_parts >= 1
           && -d ($tmp = File::Spec->catfile($dir_path, $lparts[0])))
    {
        $dir_path = $tmp;
    }
    elsif (-d ($tmp = File::Spec->catfile($dir_path, "POSIX")))
    {
        $dir_path = $tmp;
    }
    elsif (-d ($tmp = File::Spec->catfile($dir_path, "C")))
    {
        $dir_path = $tmp;
    }
    else
    {
        return;
    }

    # Now open the directory and scan all HTML files for links.

    return unless (defined($dir = IO::Dir->new($dir_path)));
    while (defined($fname = $dir->read()))
    {
        my ($file,
            $full_name);
        $full_name = File::Spec->catfile($dir_path, $fname);
        $full_name = File::Spec->rel2abs($full_name);

        # Only scan HTML files.

        if ($fname =~ m/^.*\.html$/
            && defined($file = IO::File->new($full_name, "r")))
        {
            my $line;
            while (defined($line = $file->getline()))
            {

                my ($dir_string,
                    @dirs,
                    $file_name,
                    @list,
                    $url,
                    $volume);

                # Mangle the file name into a URL.

                ($volume, $dir_string, $file_name) =
                    File::Spec->splitpath($full_name);
                @dirs = File::Spec->splitdir($dir_string);
                $url = "file://";
                $url .= "/" . $volume . "/" if ($volume ne "");
                $url .= join("/", @dirs);
                $url .= "/" if ($url =~ m/.*[^\/]$/);
                $url .= $file_name;

                # Process each link of the form <a name="..."> but filter out
                # the internally generated ones (used for all figures).

                @list = ($line =~ m/<a name=\"([^\"]+)\">/g);
                foreach my $link (@list)
                {
                    $help_ref_to_url_map{$link} = $url . "#" . $link
                        if ($link !~ m/^id\d+$/);
                }

                # Special case the contents page making sure that it has
                # appropiate default entries pointing to it.

                if ($fname eq "monotone-browse.html")
                {
                    $help_ref_to_url_map{""} = $url;
                    $help_ref_to_url_map{"contents"} = $url;
                    $help_ref_to_url_map{"index"} = $url;
                }

            }
            $file->close();
        }
    }
    $dir->close();

    return;

}
#
##############################################################################
#
#   Routine      - adjust_day_for_month
#
#   Description  - This routine adjusts the specified day so that it is valid
#                  for the specified month and year.
#
#   Data         - $day         : The day within the month (1 - 31).
#                  $month       : The month (0 - 11).
#                  $year        : The year (relative to 1900).
#                  Return Value : The adjusted day value.
#
##############################################################################



sub adjust_day_for_month($$$)
{

    my ($day, $month, $year) = @_;

    # Is the month February and the day is greater than 28?

    if ($month == 1 && $day > 28)
    {

        # Yes so check for leap years.

        $year += 1900;
        if (($year % 4) == 0 && (($year % 100) != 0 || ($year % 400) == 0))
        {
            $day = 29;
        }
        else
        {
            $day = 28;
        }

    }

    # Is the day is greater than 30?

    elsif ($day > 30)
    {

        # Truncate depending upon whether it is a long month or not.

        if (exists($long_months{$month}))
        {
            $day = 31;
        }
        else
        {
            $day = 30;
        }

    }

    return $day;

}

1;
