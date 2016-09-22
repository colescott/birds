import { createStore, applyMiddleware, compose, combineReducers } from "redux";
import createSagaMiddleware from "redux-saga";
import { routerReducer, routerMiddleware } from "react-router-redux";

import reducers from "./reducers";
import rootSaga from "./saga/index.js";

export default function configureStore(basicHistory) {
  const sagaMiddleware = createSagaMiddleware();
  const store = createStore(
    combineReducers({
        ...reducers,
        routing: routerReducer
    }),
    compose(
      applyMiddleware(...[
            routerMiddleware(basicHistory),
            sagaMiddleware,
      ]),
      window.devToolsExtension ? window.devToolsExtension() : f => f
    )
  );
  sagaMiddleware.run(rootSaga);
  return store;
}
