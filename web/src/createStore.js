// @flow

import { createStore, combineReducers, applyMiddleware } from "redux";
import { routerReducer, routerMiddleware } from "react-router-redux";
import { composeWithDevTools } from "redux-devtools-extension";
import thunk from "redux-thunk";

import api from "./api";
import reducers from "./store/reducers";

export default (history: History) => {
    const middleware = routerMiddleware(history);

    const store = createStore(
        combineReducers({
            ...reducers,
            router: routerReducer
        }),
        composeWithDevTools(
            applyMiddleware(middleware, thunk.withExtraArgument({ api }))
        )
    );
    return store;
};
