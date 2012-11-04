##############################################################################
#
#   File Name    - CachingAutomateStdio.pm
#
#   Description  - A class module that provides an interface to Monotone's
#                  automate stdio interface with caching of certain
#                  information (currently branch lists and whether we have an
#                  associated workspace or not). This class is derived from
#                  the Monotone::AutomateStdio class.
#
#   Authors      - A.E.Cooper.
#
#   Legal Stuff  - Copyright (c) 2007 Anthony Edward Cooper
#                  <aecooper@coosoft.plus.com>.
#
#                  This library is free software; you can redistribute it
#                  and/or modify it under the terms of the GNU Lesser General
#                  Public License as published by the Free Software
#                  Foundation; either version 3 of the License, or (at your
#                  option) any later version.
#
#                  This library is distributed in the hope that it will be
#                  useful, but WITHOUT ANY WARRANTY; without even the implied
#                  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#                  PURPOSE. See the GNU Lesser General Public License for
#                  more details.
#
#                  You should have received a copy of the GNU Lesser General
#                  Public License along with this library; if not, write to
#                  the Free Software Foundation, Inc., 59 Temple Place - Suite
#                  330, Boston, MA 02111-1307 USA.
#
##############################################################################
#
##############################################################################
#
#   Package      - CachingAutomateStdio
#
#   Description  - See above.
#
##############################################################################



# ***** PACKAGE DECLARATION *****

package CachingAutomateStdio;

# ***** DIRECTIVES *****

require 5.008005;

use integer;
use locale;
use strict;
use warnings;

# ***** REQUIRED PACKAGES *****

# Monotone AutomateStdio module.

use Monotone::AutomateStdio qw(:capabilities :severities);

# ***** FUNCTIONAL PROTOTYPES *****

# Public methods.

sub branches($$);
sub has_workspace($;$);

# ***** PACKAGE INFORMATION *****

# We are derived from the Monotone::AutomateStdio class.

use base qw(Monotone::AutomateStdio);

*EXPORT = *Monotone::AutomateStdio::EXPORT;
*EXPORT_OK = *Monotone::AutomateStdio::EXPORT_OK;
*EXPORT_TAGS = *Monotone::AutomateStdio::EXPORT_TAGS;
our $VERSION = "0.02";
#
##############################################################################
#
#   Routine      - branches
#
#   Description  - Get a list of branches, caching the result.
#
#   Data         - $this        : The object.
#                  $list        : A reference to a list that is to contain the
#                                 branch names.
#                  Return Value : True on success, otherwise false on failure.
#
##############################################################################



sub branches($$)
{

    my ($this, $list) = @_;

    my $ret_val = 1;

    # If there is no cache or the mtn subprocess has been restarted, and hence
    # the database has been reopened, then refresh the cache.

    if (! exists($this->{cached_branch_list})
        || ! exists($this->{cached_mtn_pid})
        || $this->{cached_mtn_pid} == 0
        || $this->{cached_mtn_pid} != $this->get_pid())
    {
        $this->{cached_branch_list} = [];
        $ret_val = $this->SUPER::branches($this->{cached_branch_list});
        $this->{cached_mtn_pid} = $this->get_pid();
    }
    @$list = @{$this->{cached_branch_list}};

    return $ret_val;

}
#
##############################################################################
#
#   Routine      - has_workspace
#
#   Description  - Gets and/or sets the has-workspace boolean property of this
#                  object. This is needed as we always reopen the database
#                  outside of any workspace (thus $mtn->get_ws_path() is no
#                  use) but sometimes we need to know whether we are running
#                  inside of one (typically for the workspace tools).
#
#   Data         - $this          : The object.
#                  $has_workspace : The boolean value that this property is to
#                                   be set to. This is optional.
#                  Return Value   : The current value of this property.
#
##############################################################################



sub has_workspace($;$)
{

    my ($this, $has_workspace) = @_;

    # If the property doesn't exist then treat that as false.

    if (defined($has_workspace))
    {
        $this->{has_workspace} = $has_workspace ? 1 : 0;
        return $this->{has_workspace};
    }
    else
    {
        if (exists($this->{has_workspace}))
        {
            return $this->{has_workspace};
        }
    }

    return 0;

}

1;
