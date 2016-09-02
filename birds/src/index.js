import React from "react";
import ReactDOM from "react-dom";
import { Provider } from "react-redux";
import { Router, Route, IndexRoute, hashHistory } from "react-router";

import firebase from "firebase";
var config = {
    apiKey: "AIzaSyB_QM9xJqQONmX8ca4aCWCw0x8e_czLWDQ",
    authDomain: "cardinalbirds.firebaseapp.com",
    databaseURL: "https://cardinalbirds.firebaseio.com",
    storageBucket: "project-7535783528222319330.appspot.com",
};
firebase.initializeApp(config);

import configureStore from "./store/configureStore";
const store = configureStore();
import * as c from "./store/constants.js";
import * as a from "./store/actions.js";
store.dispatch(a.setFirebase(firebase));

import Main from "./ui/main.js";
import Home from "./ui/home/index.js";
import Register from "./ui/register/index.js";

const appDiv = document.createElement('div');
document.body.appendChild(appDiv);

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
