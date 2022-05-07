title: Release Checklist

When tagging a new release please follow this checklist so that the
documentation will be updated correctly, and version strings will
remain accurate.

# Release Checklist

 1. Make sure you are working from the master branch: `git checkout master`
 1. Update the `CHANGELOG.md`
	 1. Ensure that the changes listed under
        [Unreleased](|url|/page/releases/index.html#unreleased) are up
        to date and accurate.
	     1. Look at the latest [changes committed to GitHub since the
            last release](https://github.com/jacobwilliams/json-fortran/compare/{!.VERSION!}...HEAD),
            {!.VERSION!}. This will only be accurate if you have
            not yet updated `.VERSION`. If you *have* updated
            `.VERSION` then edit the URL manually to view the
            changes.
		 1. Run the
            [`github_changelog_generator`](https://github.com/github-changelog-generator/github-changelog-generator)
            as follows, and compare the output to the
            [unreleased](|url|/page/releases/index.html#unreleased)
            section of the `CHANGELOG.md`:
			`github_changelog_generator --user jacobwilliams --project json-fortran -o Unreleased.md
            --unreleased-only --bugs-label "**Fixed issues:**"
            --enhancement-label "**Enhancements:**" --issues-label
            "**Fixed issues:**"` and make sure that you have a
            16-digit GitHub token stored in the environment variable
            `CHANGELOG_GITHUB_TOKEN`. Compare generated
            `Unreleased.md` to the "Unreleased" section of
            `CHANGELOG.md` and make updates as needed.
	 1. Rename the "Unreleased" section to the new version number
		 1. Copy the "Unreleased" section header and "Complete
            Changeset" link line and insert it just below the TOC
		 1. In the newly inserted "Complete Changeset" URL replace the
            old version number with the new version number
	     1. Rename the lower "Unreleased" section (with all the
            changes) to the current version
		 1. Replace `HEAD` in the header URL with the new version
            number
		 1. Replace `HEAD` in the "Complete Changeset" link with the
		    new version number.
		 1. Insert the new version section in the TOC just below the
            "Unreleased" entry
	 1. Add `CHANGELOG.md` to the git index: `git add CHANGELOG.md`
 1. Add an entry in `pages/releases/index.md` under the "Past
    Releases" heading
     1. Copy/paste the most recent one listed and replace all
        occurrences of the old version string with the new version
        string
	 1. Add `pages/releases/index.md` to the git index: `git add pages/releases/index.md`
 1. Update the version string in remaining files requiring manual edits:
     1. Edit the `.VERSION` file
     1. Edit the CMake example on line 142 of `README.md`
     1. Edit the `json_fortran_version` string in `json_module.F90`
	 1. Add these files to the git index: `git add .VERSION README.md ./src/json_module.F90`
 1. Commit the changes to the master branch: `git commit`
 1. Create the tag: `git tag <new version>`
     1. Note: for some reason, using GitHub to tag a project means that the tag doesn't show up when you `git describe` the project. Tagging from the command line makes `git describe` work as expected.
 1. [GitHub Actions](CI.yml.html) will publish the documentation for the
    latest tag to
    https://jacobwilliams.github.io/json-fortran/prev/*new_ver*/ if all
    goes according to plan
 1. Push tags and master to GitHub: `git push --tags origin master`
 1. [Draft and publish](https://github.com/jacobwilliams/json-fortran/releases/new)
    a new GitHub Release corresponding to the new version
 1. Update the Homebrew formula
     1. [Fork](https://github.com/Homebrew/homebrew#fork-destination-box)
        the
        [Homebrew repository](https://github.com/Homebrew/homebrew)
	 1. Change to your local Homebrew repository: `cd $(brew
        --repository)/Library/Formula`
	 1. Add your fork as a pushable remote: `git remote add
        YOUR_USERNAME https://github.com/YOUR_USERNAME/homebrew.git`
	 1. Make sure you're on the master branch: `git checkout master`
	 1. Make sure everything is up to date: `brew update` (this calls
        `git pull`)
	 1. Create a new branch from the latest master branch with `git
        checkout -b YOUR_BRANCH_NAME origin/master`
	 1. Edit the `json-fortran.rb` formula with `brew edit
        json-fortran`
	 1. Do *NOT* change anything other than the URL and the SHA256
        checksum at the top of the formula. These should be changed to
        correspond to the new version of the compressed tarball
        available on newly minted
        [latest release page](https://github.com/jacobwilliams/json-fortran/releases/latest)
	 1. Test the changes to make sure that they will be accepted:
         1. `brew audit --strict --online json-fortran`
		 1. `brew rm json-fortran`
		 1. `brew install -v json-fortran`
		 1. `brew test -v json-fortran`
	 1. Add `json-fortran.rb` to the index (`git add json-fortran.rb`)
	 1. Commit your changes with the correct commit message: `git
        commit -m "json-fortran <new-version-string-here>"`
	 1. Push changes to your fork: `git push --set-upstream
        YOUR_USERNAME YOUR_BRANCH_NAME`
     1. Go to <https://github.com/Homebrew/homebrew> and submit a pull
        request
