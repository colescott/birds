import React from "react";
import ReactDOM from "react-dom";
import { Provider } from "react-redux";
import { Router, Route, IndexRoute, hashHistory } from "react-router";

// Setup firebase
import firebase from "firebase";
var config = {
    apiKey: "AIzaSyB_QM9xJqQONmX8ca4aCWCw0x8e_czLWDQ",
    authDomain: "cardinalbirds.firebaseapp.com",
    databaseURL: "https://cardinalbirds.firebaseio.com",
    storageBucket: "project-7535783528222319330.appspot.com",
};
firebase.initializeApp(config);

// Setup Redux
import configureStore from "./store/configureStore";
import * as c from "./store/constants.js";
import * as a from "./store/actions.js";
const store = configureStore();

// Pass the firebase instance to the root saga
store.dispatch(a.setFirebase(firebase));

// Load some components
import Main from "./ui/main.js";
import Home from "./ui/home/index.js";
import Register from "./ui/register/index.js";

// Create and append a div
const appDiv = document.createElement('div');
document.body.appendChild(appDiv);

// Render the page
ReactDOM.render(
    <Provider store={store}>
      <Router history={hashHistory}>
        <Route path="/" component={Main}>
          <Route path="/register" component={Register} />
          <IndexRoute component={Home} />
        </Route>
      </Router>
    </Provider>,
    appDiv
);
