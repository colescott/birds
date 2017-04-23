import React from "react";

import { Provider } from "react-redux";
import { ConnectedRouter } from "react-router-redux";
import { Route } from "react-router";

const Home = () => <h1> Yaji </h1>;

export default ({ store, history }) => (
    <Provider store={store}>
        <div>
            <ConnectedRouter history={history}>
                <Route exact path="/" component={Home} />
            </ConnectedRouter>
        </div>
    </Provider>
);
