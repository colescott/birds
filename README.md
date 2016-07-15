# CardinalBIRDS - Beginner Intermediate Role Development System #
**By FIRST Robotics Competition Team 4159 CardinalBotics**

### How to setup firebase:###

1. Install firebase cl
  1. Install node [here](https://nodejs.org/en/download)
  1. Run `npm install -g firebase-tools` in terminal
  1. Run `firebase --version` to make sure it works
1. `firebase init` to create a new firebase app
1. `git clone` this repo into the public folder
1. Setup deploy targets
  1. Run `firebase use cardinalbirds-dev --alias dev`
  1. Run `firebase use project-7535783528222319330 --alias prod`
1. Use `firebase serve` and open <http://localhost:5000> to view local copy

### How to deploy to firebase:###

1. Run `firebase use <project>` where `<project>` is 'dev', 'prod', or the project id
1. Edit the firebase reference in `javascript/userauth.js` to the app you are deploying to
1. Deploy using `firebase deploy`

==

### Notes:###

* Incorporate jquery and angular.js more instead of naked ajax
* Use firebase's database to store user progress and firebase storage to store content
