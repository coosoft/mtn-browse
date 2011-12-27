##############################################################################
#
#   File Name    - Globals.pm
#
#   Description  - The global data module for the mtn-browse application. This
#                  module contains all the global constants and variables used
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
#   Package      - Globals
#
#   Description  - See above.
#
##############################################################################



# ***** PACKAGE DECLARATION *****

package Globals;

# ***** DIRECTIVES *****

require 5.008005;

use locale;
use strict;
use warnings;

# ***** GLOBAL DATA DECLARATIONS *****

# Constants used to represent units for periods of time.

use constant DURATION_MINUTES => 0;
use constant DURATION_HOURS   => 1;
use constant DURATION_DAYS    => 2;
use constant DURATION_MONTHS  => 3;
use constant DURATION_YEARS   => 4;

# Constants used to represent the different groups of widgets.

use constant BRANCH           => 0x01;
use constant DIRECTORY        => 0x02;
use constant DIRECTORY_VIEW   => 0x04;
use constant DISPLAY_OF_FILE  => 0x08;
use constant REVISION         => 0x10;
use constant REVISION_LIST    => 0x02;
use constant REVISION_DETAILS => 0x04;

# Constants used to represent the different state changes. Read this as
# `what has just been changed' => `what needs to be updated'.

use constant ALL_CHANGED               => 0xff;
use constant BRANCH_CHANGED            => (REVISION | DIRECTORY
                                           | DIRECTORY_VIEW | DISPLAY_OF_FILE);
use constant DATABASE_CHANGED          => 0xff;
use constant DIRECTORY_CHANGED         => (DIRECTORY_VIEW | DISPLAY_OF_FILE);
use constant FILE_CHANGED              => (DISPLAY_OF_FILE);
use constant NEW_FIND                  => 0xff;
use constant REVISION_CHANGED          => (DIRECTORY | REVISION_LIST
                                           | DIRECTORY_VIEW | DISPLAY_OF_FILE);
use constant SELECTED_REVISION_CHANGED => (REVISION_DETAILS);

# Constant for the strftime() format string that is used to generate Montoone
# style time strings.

use constant MTN_TIME_STRING => "%Y-%m-%dT%H:%M:%S";

# Location of the Glade UI XML file for mtn-browse.

our $glade_file;

# The tooltips widget.

our $tooltips;

# The mono-spaced font used for displaying text files.

our $mono_font;

# The progress bar widget that is to be pulsed when issuing Monotone commands
# or using run_command(). No pulsing is done if this is undefined.

our $pulse_widget;

# The full character encoding list and the current encoding used for file
# contents.

our @encodings;
our $file_encoding = "utf8";

# The full list of local authentication keys that can be used with Monotone and
# the current key being used.

our @keys;
our $mtn_key;

# Assorted pixmaps.

our $line_image;

# Location of the temporary working directory.

our $tmp_dir;

# The user's preferences data.

our $user_preferences;

# The MIME type file name pattern match table.

our $mime_match_table;

# Regular expression used to escape certain characters in strings that are
# passed directly to Monotone via Monotone::AutomateStdio.

our $select_escape_re = qr/([\/|\\])/;

# Whether Monotone warnings should be suppressed or not.

our $suppress_mtn_warnings;

# ***** PACKAGE INFORMATION *****

use base qw(Exporter);

our %EXPORT_TAGS = (constants => [qw(ALL_CHANGED
                                     BRANCH
                                     BRANCH_CHANGED
                                     DATABASE_CHANGED
                                     DIRECTORY
                                     DIRECTORY_CHANGED
                                     DIRECTORY_VIEW
                                     DISPLAY_OF_FILE
                                     DURATION_DAYS
                                     DURATION_HOURS
                                     DURATION_MINUTES
                                     DURATION_MONTHS
                                     DURATION_YEARS
                                     FILE_CHANGED
                                     MTN_TIME_STRING
                                     NEW_FIND
                                     REVISION
                                     REVISION_CHANGED
                                     REVISION_DETAILS
                                     REVISION_LIST
                                     SELECTED_REVISION_CHANGED)],
                    variables => [qw(@encodings
                                     $file_encoding
                                     $glade_file
                                     @keys
                                     $line_image
                                     $mime_match_table
                                     $mono_font
                                     $mtn_key
                                     $pulse_widget
                                     $select_escape_re
                                     $suppress_mtn_warnings
                                     $tmp_dir
                                     $tooltips
                                     $user_preferences)]);
our @EXPORT = qw();
Exporter::export_ok_tags(qw(constants variables));
our $VERSION = 0.1;

1;
