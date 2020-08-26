#!/usr/bin/env python
##########################################################################
#
#  Copyright (c) 2019, Cinesite VFX Ltd. All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#
#      * Redistributions of source code must retain the above
#        copyright notice, this list of conditions and the following
#        disclaimer.
#
#      * Redistributions in binary form must reproduce the above
#        copyright notice, this list of conditions and the following
#        disclaimer in the documentation and/or other materials provided with
#        the distribution.
#
#      * Neither the name of John Haddon nor the names of
#        any other contributors to this software may be used to endorse or
#        promote products derived from this software without specific prior
#        written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
##########################################################################

import datetime
import github
import os
import re
import sys
import json

# Azure pipeline variables can be populated at run-time by echoing special
# strings to a process output. The following allows vars to be set:
#
#    ##vso[task.setvariable variable=<name>;]<value>
#
# NOTE: The variable must first be defined in the Pipeline settings and have
# the 'Settable at queue time' checkbox checked.
#
# We make use of this mechanism to allow custom logic to define the build name
# as well as determine the correct commit hash depending on the nature of the
# trigger. These variables can be referenced in a pipeline yaml file downstream
# of the step that runs this script.

# GitHub actions is somewhat sparse in what information is available via the
# GITHUB_* env vars. There is however a veritable treasure trove in the
# .json file pointed to by GITHUB_EVENT_PATH. "The Internets" seem to suggest
# this is the most reliable way of determining information about the
# triggering commit. Some of the official vars vary after a retry, etc...
#
# This is based on the webhook payload so should be relatively stable as it
# is part of that public API.

with open( os.environ["GITHUB_EVENT_PATH"] ) as f :
	eventData = json.load( f )

## Source Branches

buildBranch = os.environ.get( "GITHUB_BASE_REF", "" )
sourceBranch = os.environ.get( "GITHUB_HEAD_REF", buildBranch )

## Source Commit Hash

commit = os.environ.get( "GITHUB_SHA" )

# Actions merges the branch into its target in PR build, so
# GITHUB_SHA isn't correct as it references the ephemeral merge.
if os.environ.get( "GITHUB_EVENT_NAME" ) == "pull_request" :
	commit = eventData["pull_request"]["head"]["sha"]

## Source Tag

tag = ""
if "/tags" in os.environ["GITHUB_REF"] :
	tag = os.environ["GITHUB_REF"].replace( "refs/tags/", "" )

## Pull Request number

pullRequest = ""
if os.environ.get( "GITHUB_EVENT_NAME" ) == "pull_request" :
	pullRequest = eventData["pull_request"]["number"]

## Release ID

releaseId = ""

if tag :

	# We attempt to find the release ID so that we only publish a tag build
	# if its for a release, not all tags will have one
	githubClient = github.Github( os.environ.get( 'GITHUB_ACCESS_TOKEN' ) )
	repo = githubClient.get_repo( os.environ.get( 'GITHUB_REPOSITORY' ) )

	for r in repo.get_releases() :
		if r.tag_name == tag :
			releaseId = r.id
			break

## Build Name

formatVars = {
	"buildTypeSuffix" : "-debug" if os.environ.get( "BUILD_TYPE", "" ) == "DEBUG" else "",
	"variant" : os.environ["GAFFER_BUILD_VARIANT"],
	"timestamp" : datetime.datetime.now().strftime( "%Y_%m_%d_%H%M" ),
	"pullRequest" : pullRequest,
	"shortCommit" : commit[:8],
	"tag" : tag,
	"branch" : re.sub( r"[^a-zA-Z0-9_]", "", sourceBranch )
}

nameFormats = {
	"default" : "gaffer-{timestamp}-{shortCommit}-{variant}{buildTypeSuffix}",
	"pull_request" : "gaffer-pr{pullRequest}-{branch}-{timestamp}-{shortCommit}-{variant}{buildTypeSuffix}",
	"release" : "gaffer-{tag}-{variant}{buildTypeSuffix}"
}

trigger = os.environ.get( 'GITHUB_EVENT_NAME', '' )

# If we have a releaseID (and tag) then we always use release naming conventions
# to allow manual re-runs of release builds that fail for <reasons>.
if tag and releaseId :
	print( "Have Release ID %s for tag %s, using release naming." % ( releaseId, tag ) )
	trigger = "release"

buildName = nameFormats.get( trigger, nameFormats['default'] ).format( **formatVars )

## GitHub Actions Pipeline Vars

print( "Setting $GAFFER_BUILD_NAME to '%s'" % buildName )
print( "::set-env name=GAFFER_BUILD_NAME::%s" % buildName )

print( "Setting $GAFFER_SOURCE_COMMIT to '%s'" % commit )
print( "::set-env name=GAFFER_SOURCE_COMMIT::%s" % commit )

print( "Setting $GAFFER_GITHUB_RELEASEID to '%s'" % releaseId )
print( "::set-env name=GAFFER_GITHUB_RELEASEID::%s" % releaseId )
