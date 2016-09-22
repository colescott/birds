// Load Libraries
import React from "react";
import ReactDOM from "react-dom";
import { hashHistory } from "react-router";
import { syncHistoryWithStore } from "react-router-redux";

// Load Styles
import "./index.scss";

//Needed for Material Ui
import injectTapEventPlugin from "react-tap-event-plugin";
injectTapEventPlugin();

// Setup Redux
import configureStore from "./store/configureStore";
const store = configureStore(hashHistory);
const history = syncHistoryWithStore(hashHistory, store);

// Create and append a div
const appDiv = document.createElement("div");
document.body.appendChild(appDiv);

// Render the page
import Root from "./ui/root";
ReactDOM.render(
    <Root history={history} store={store} />,
    appDiv
);
