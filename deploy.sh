#!/bin/bash
# Script to deploy documentation after successfull build of master branch or tag
# If running under travis-ci this will automatically deploy updates to the master branch's
# documentation on build events for the master branch, and will add/update documentation for
# any new/updated tags that are pushed.
# The script may also be used to manually deploy the current branch's documentation,
# although for branch's other than master, index.html will likely need to be manually
# edited to add an entry for the current branch. Also, this may be inadvisable, since the
# the documentation can be published before the branch's code is published. Use it to add
# new branch's development documentation, preferably immediately after pushing that branch
# to github.
set -ev # Echo what we're doing and fail on any errors
if [ ! "$TRAVIS" ]; then #not on travis, try a sane deploy of current branch's documentation
    if [ "$(ls -A ./documentation)" ]; then #not empty
	REVISION="$(git rev-parse HEAD)"
	BRANCH="$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')"
	git clone --branch=gh-pages git@github.com:jacobwilliams/json-fortran.git gh-pages
	cd gh-pages
        [ -e "$BRANCH" ] && rm -r "$BRANCH" # wipe out old docs if they exist
        mkdir "$BRANCH"
        mkdir "$BRANCH/tests"
        FILES=$(find ../documentation -name '*.*')  #get all the files (including in subdirectories)
        for f in $FILES; do # add branch info to header and clean line endings
	    sed '/[^#]robo_top_of_doc/ s;jsonfortran</a>;jsonfortran '"$BRANCH"'</a>;' $f | sed 's/ *$//' > "$BRANCH/${f##*documentation/}"
        done
        git add "$BRANCH"
        git commit -m "Development documentation for $BRANCH updated for commit $REVISION"
	if egrep "\<${BRANCH}\>" index.html >/dev/null 2>&1 ; then
	    echo "It appears that index.html knows about branch $BRANCH and likely does not require updating."
	    git push origin gh-pages # assumes write access to jacobwilliams/json-fortran...
	                             # only true for @jacobwilliams
	else
	    echo "index.html must be manually edited to add link to branch $BRANCH, then commit the changes and push manually."
	fi
    fi
else #running under travis
    if $TRAVIS_SECURE_ENV_VARS ; then
	# only try to update master's development documentation
	if [ "$TRAVIS_BRANCH" = "master" ] && [ "$TRAVIS_PULL_REQUEST" = "false" ] && [ "$(ls -A $TRAVIS_BUILD_DIR/documentation)" ] ; then #not empty
            git clone --branch=gh-pages https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG gh-pages
            cd gh-pages
            [ -e master ] && rm -r master # wipe out docs if they exist
            mkdir master
            mkdir master/tests
            FILES=$(find ../documentation -name '*.*')  #get all the files (including in subdirectories)
            for f in $FILES; do # add branch info to header and clean line endings
		sed '/[^#]robo_top_of_doc/ s;jsonfortran</a>;jsonfortran master</a>;' $f | sed 's/ *$//' > master/${f##*documentation/}
            done
            git add master
            git commit -m "Development documentation updated by travis job $TRAVIS_JOB_NUMBER for commits $TRAVIS_COMMIT_RANGE"
            git push origin gh-pages
	fi
	# If publishing a new/updated tag, deploy it's documentation
	if [ "$TRAVIS_TAG" ] && [ "$(ls -A $TRAVIS_BUILD_DIR/documentation)" ] ; then #not empty
	    git clone --branch=gh-pages https://${GH_TOKEN}@github.com/$TRAVIS_REPO_SLUG gh-pages
	    cd gh-pages
	    [ -e "$TRAVIS_TAG" ] && rm -r "$TRAVIS_TAG" # wipe out existing docs for tag if they exist
	    mkdir "$TRAVIS_TAG"
	    mkdir "$TRAVIS_TAG/tests"
	    # Add an entry in index.html for the new tag, assume none exists
	    awk '/<!--Next stable release goes here-->/{print "<a href=\"./'"$TRAVIS_TAG"'/json_module_f90.html\" class=\"indexitem\" >'"$TRAVIS_TAG"'</a>"}1' index.html > index2.html && mv index2.html index.html
        FILES=$(find ../documentation -name '*.*')  #get all the files (including in subdirectories)
        for f in $FILES; do # add tag info to headers and clean line endings
		sed '/[^#]robo_top_of_doc/ s;jsonfortran</a>;jsonfortran '"$TRAVIS_TAG"'</a>;' $f | sed 's/ *$//' > "$TRAVIS_TAG/${f##*documentation/}"
	    done
	    git add "$TRAVIS_TAG" index.html # don't forget to add the top level index!
	    git commit -m "Tag/release documentation updated by travis job $TRAVIS_JOB_NUMBER for tag $TRAVIS_TAG $TRAVIS_COMMIT"
	    git push origin gh-pages
	fi
    fi
fi
