import React from "react";

import { Provider } from "react-redux";
import { Router, Route, IndexRoute } from "react-router";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";

import Main from "./main";
import Home from "./home";
import Login from "./login";
import Register from "./register";
import SelectTeam from "./selectTeam";
import LessonEditor from "./lessonEditor";
import Error404 from "./error404";

const Root = ({ store, history }) => (
    <MuiThemeProvider>
        <Provider store={store}>
          <Router history={history}>
            <Route path="/" component={Main}>
              <Route path="/register" component={Register} />
              <Route path="/login" component={Login} />
              <Route path="/selectTeam" component={SelectTeam} />
              <Route path="/lessonEditor" component={LessonEditor} />
              <Route path="/*" component={Error404} />
              <IndexRoute component={Home} />
            </Route>
          </Router>
        </Provider>
    </MuiThemeProvider>
);

export default Root;
