import { createStore, combineReducers, applyMiddleware } from "redux";
import { routerReducer, routerMiddleware } from "react-router-redux";
import { composeWithDevTools } from "redux-devtools-extension";

const reducer = (state = {}, action) => state;

export default history => {
    const middleware = routerMiddleware(history);

    const store = createStore(
        combineReducers({
            reducer,
            router: routerReducer
        }),
        composeWithDevTools(applyMiddleware(middleware))
    );
    return store;
};
