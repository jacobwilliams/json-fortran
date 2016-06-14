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
            git clone -q --branch=gh-pages "https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG" gh-pages >/dev/null 2>&1
            cd gh-pages || exit 1
	    rm -rf -- css fonts interface lists module proc search.html	src type favicon.png index.html \
	       js media page program sourcefile	tipuesearch || true
	    cp -r "$TRAVIS_BUILD_DIR"/doc/* .
            git add -A || true # Add all the new files
            git commit -m "Development documentation updated by travis job $TRAVIS_JOB_NUMBER for commits $TRAVIS_COMMIT_RANGE" || true
            git push -fq origin gh-pages > /dev/null 2>&1 || true
	fi
	# If publishing a new/updated tag, deploy it's documentation
	if [ "$TRAVIS_TAG" ] && [ "$(ls -A "$TRAVIS_BUILD_DIR/doc")" ] ; then #not empty
	    cd "$TRAVIS_BUILD_DIR" || exit 1
	    git clone -q --branch=gh-pages "https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG" gh-pages >/dev/null 2>&1
	    sed "2 s/^/version: ${TRAVIS_TAG}\n/" json-fortran.md > json-fortran.tagged.md
	    head json-fortran.tagged.md # Debug output
	    # rebuild FORD documentation without pages, with version info, wiping out any existing tag folder
	    ford --debug -o "gh-pages/$TRAVIS_TAG" json-fortran.tagged.md
	    cd gh-pages || exit 1
	    git add -A # add all new files in $TRAVIS_TAG/
	    git commit -m "Tag/release documentation updated by travis job $TRAVIS_JOB_NUMBER for tag $TRAVIS_TAG $TRAVIS_COMMIT"
	    git push -f -q origin gh-pages >/dev/null 2>&1
	fi
    fi
fi
