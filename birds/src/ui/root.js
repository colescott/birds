import React from "react";

import { Provider } from "react-redux";
import { Router, Route, IndexRoute, Redirect } from "react-router";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import * as a from "../store/actions.js";

import Main from "./main";
import Home from "./home";
import Login from "./login";
import Register from "./register";
import SelectTeam from "./selectTeam";
import LessonEditor from "./lessonEditor";
import LessonList from "./lessonList";
import Lesson from "./lesson";
import NotFound from "./notfound";

const Root = ({ store, history }) => (
    <MuiThemeProvider>
        <Provider store={store}>
          <Router history={history}>
            <Route path="/" component={Main}>
                <Route path="/register" component={Register} />
                <Route path="/login" component={Login} />
                <Route path="/selectTeam" component={SelectTeam} />
                <Route path="/lessonEditor" component={LessonEditor} />
                <Route path="/lessons" component={LessonList} onEnter={() => store.dispatch(a.getLessonList())} />
                <Route path="/lesson/:id" component={Lesson} onEnter={(props) => {
                    store.dispatch(a.getLessonList());
                    store.dispatch(a.getLesson(props.params.id));
                }} onLeave={() => store.dispatch(a.resetLesson())} />
                <Route path='/404' component={NotFound} />
                <IndexRoute component={Home} />
                <Redirect from='*' to='/404' />
            </Route>
          </Router>
        </Provider>
    </MuiThemeProvider>
);

export default Root;
