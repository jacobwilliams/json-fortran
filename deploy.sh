#!/bin/bash
# Script to deploy documentation after successfull build of master branch or tag
# If running under travis-ci this will automatically deploy updates to the master branch's
# documentation on build events for the master branch, and will add/update documentation for
# any new/updated tags that are pushed.
set -o errexit
set -o verbose
if [ "$TRAVIS" ]; then #running under travis
    if $TRAVIS_SECURE_ENV_VARS ; then
	# only try to update master's development documentation
	if [ "$TRAVIS_BRANCH" = "master" ] && \
	       [ "$TRAVIS_PULL_REQUEST" = "false" ] && \
	       [ "$(ls -A "$TRAVIS_BUILD_DIR/doc")" ] ; then #not empty
            git clone --branch=gh-pages "https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG" gh-pages
            cd gh-pages || exit 1
	    rm -r css favicon.png fonts index.html interface js lists media module page proc \
	       program search.html sourcefile src tipuesearch type
	    cp -r "$TRAVIS_BUILD_DIR"/doc/* .
            git add -A # Add all the new files
            git commit -m "Development documentation updated by travis job $TRAVIS_JOB_NUMBER for commits $TRAVIS_COMMIT_RANGE"
            git push origin gh-pages
	fi
	# If publishing a new/updated tag, deploy it's documentation
	if [ "$TRAVIS_TAG" ] && [ "$(ls -A "$TRAVIS_BUILD_DIR/doc")" ] ; then #not empty
	    cd "$TRAVIS_BUILD_DIR" || exit 1
	    git clone --branch=gh-pages "https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG" gh-pages
	    sed "1 s/^/version: ${TRAVIS_TAG}\n/" json-fortran.md > json-fortran.tagged.md
	    # rebuild FORD documentation without pages, with version info, wiping out any existing tag folder
	    ford -o "gh-pages/$TRAVIS_TAG" json-fortran.tagged.md
	    cd gh-pages || exit 1
	    git add -A # add all new files in $TRAVIS_TAG/
	    git commit -m "Tag/release documentation updated by travis job $TRAVIS_JOB_NUMBER for tag $TRAVIS_TAG $TRAVIS_COMMIT"
	    git push origin gh-pages
	fi
    fi
fi
