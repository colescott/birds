import { createStore, applyMiddleware, compose, combineReducers } from "redux";
import createSagaMiddleware from "redux-saga";
import { routerReducer, routerMiddleware } from "react-router-redux";

import reducers from "./reducers";
import rootSaga from "./saga/index.js";

export const reducer = combineReducers({
    ...reducers,
    routing: routerReducer
});


export default function configureStore(basicHistory, state) {
    const sagaMiddleware = createSagaMiddleware();
    var store;
    if (state) {
          store = createStore(
            reducer,
            state,
            compose(
              applyMiddleware(...[
                    routerMiddleware(basicHistory),
                    sagaMiddleware,
              ]),
              window.devToolsExtension ? window.devToolsExtension() : f => f
            )
          );
  } else {
      store = createStore(
        reducer,
        compose(
          applyMiddleware(...[
                routerMiddleware(basicHistory),
                sagaMiddleware,
          ]),
          window.devToolsExtension ? window.devToolsExtension() : f => f
        )
      );
  }
  sagaMiddleware.run(rootSaga);
  return store;
}
