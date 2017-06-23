# Birds
[![Build Status](https://travis-ci.org/colescott/birds.svg?branch=master)](https://travis-ci.org/colescott/birds) [![codecov](https://codecov.io/gh/colescott/birds/branch/master/graph/badge.svg)](https://codecov.io/gh/colescott/birds)
[![Dependencies](https://david-dm.org/colescott/birds.svg)](https://david-dm.org/colescott/birds)
[![devDependencies Status](https://david-dm.org/colescott/birds/dev-status.svg)](https://david-dm.org/colescott/birds?type=dev)

CardinalBotics Beginning and Intermediate Role Development System

## Setup
```bash
git clone https://github.com/Team4159/birds.git
npm install
```

## Usage
```bash
# Run the node server (port 8000)
npm run start

# Run the dev node server (port 8000)
npm run start:dev

# Start the webpack dev server (port 8080)
npm run serve

# Build the static files
npm run build:dev

# Build the static files for production
npm run build:prod
```

## Notes

* branch master auto deploys to [birds-staging](https://birds-staging.herokuapp.com/)
* work flow is as follows
```bash
git pull --ff-only
git checkout -b {your branch name}

# Make some changes

# Do some git magic
git checkout master
git pull --ff-only
git checkout {your branch name}
git rebabse master

# Use this to squash your commits
git rebase -i {the commit before your first one}

git checkout master
git merge --ff-only {your branch name}
git branch -d {your branch name}
git push
```
