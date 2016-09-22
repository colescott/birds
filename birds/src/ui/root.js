import React from "react";

import { Provider } from "react-redux";
import { Router, Route, IndexRoute } from "react-router";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";

import Main from "./main";
import Home from "./home";
import Login from "./login";
import Register from "./register";
import SelectTeam from "./selectTeam";

const Root = ({ store, history }) => (
    <MuiThemeProvider>
        <Provider store={store}>
          <Router history={history}>
            <Route path="/" component={Main}>
              <Route path="/register" component={Register} />
              <Route path="/login" component={Login} />
              <Route path="/selectTeam" component={SelectTeam} />
              <IndexRoute component={Home} />
            </Route>
          </Router>
        </Provider>
    </MuiThemeProvider>
);

export default Root;
