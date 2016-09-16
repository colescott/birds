import { createStore, applyMiddleware, compose } from "redux";
import createSagaMiddleware from "redux-saga";
import { routerMiddleware } from "react-router-redux";
import createLogger from "redux-logger";

import reducer from "./reducers";
import rootSaga from "./saga/index.js";

export default function configureStore(history) {
  const sagaMiddleware = createSagaMiddleware();
  const store = createStore(
    reducer,
    compose(
      applyMiddleware(...[
            routerMiddleware(history),
            sagaMiddleware,
            //createLogger()
      ]),
      window.devToolsExtension ? window.devToolsExtension() : f => f
    )
  );
  sagaMiddleware.run(rootSaga);
  return store;
}
