// Load Libraries
import React from "react";
import ReactDOM from "react-dom";
import { hashHistory, browserHistory } from "react-router";
import { syncHistoryWithStore } from "react-router-redux";

// Load Styles
import "./index.scss";

//Needed for Material Ui
import injectTapEventPlugin from "react-tap-event-plugin";
injectTapEventPlugin();

// Setup Redux
import configureStore from "./store/configureStore";
import { saveState, loadState } from "./store/localState";
const state = loadState();
let basicHistory;
if (process.env.NODE_ENV === "development") {
    basicHistory = hashHistory;
} else {
    basicHistory = browserHistory;
}
const store = configureStore(basicHistory, state);
store.subscribe(() => saveState(store.getState()));
const history = syncHistoryWithStore(basicHistory, store);

// Create and append a div
const appDiv = document.createElement("div");
document.body.appendChild(appDiv);

// HMR
if (module.hot) {
  module.hot.accept();
}

// Render the page
import Root from "./ui/root";
ReactDOM.render(
    <Root history={history} store={store} />,
    appDiv
);
