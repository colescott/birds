import React from "react";

import { Provider } from "react-redux";
import { ConnectedRouter } from "react-router-redux";
import { Route } from "react-router";

import Main from "./ui/Main";
import Home from "./ui/pages/Home";
import Register from "./ui/pages/Register";
import Login from "./ui/pages/Login";

export default ({ store, history }) => (
    <Provider store={store}>
        <ConnectedRouter history={history}>
            <Main>
                <Route exact path="/" component={Home} />
                <Route path="/register" component={Register} />
                <Route path="/login" component={Login} />
            </Main>
        </ConnectedRouter>
    </Provider>
);
