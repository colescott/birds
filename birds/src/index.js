import React from "react";
import ReactDOM from "react-dom";
import { Provider } from "react-redux";
import { Router, Route, IndexRoute, hashHistory } from "react-router";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";

// Load Styles
import "./index.scss";

//Needed for Material Ui
import injectTapEventPlugin from "react-tap-event-plugin";
injectTapEventPlugin();

// Setup Redux
import configureStore from "./store/configureStore";
const store = configureStore();

// Load some components
import Main from "./ui/main.js";
import Home from "./ui/home/index.js";
import Login from "./ui/login/index.js";
import Register from "./ui/register/index.js";

// Create and append a div
const appDiv = document.createElement("div");
document.body.appendChild(appDiv);

// Render the page
ReactDOM.render(
    <MuiThemeProvider>
        <Provider store={store}>
          <Router history={hashHistory}>
            <Route path="/" component={Main}>
              <Route path="/register" component={Register} />
              <Route path="/login" component={Login} />
              <IndexRoute component={Home} />
            </Route>
          </Router>
        </Provider>
    </MuiThemeProvider>,
    appDiv
);
