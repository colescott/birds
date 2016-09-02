import { createStore, applyMiddleware, compose } from "redux";
import createSagaMiddleware from 'redux-saga';

import reducer from "./reducers";
import rootSaga from "./saga/index.js";

export default function configureStore() {
  const sagaMiddleware = createSagaMiddleware();
  const store = createStore(
    reducer,
    compose(
      applyMiddleware(...[
        sagaMiddleware
      ]),
      window.devToolsExtension ? window.devToolsExtension() : f => f
    )
  );
  sagaMiddleware.run(rootSaga);
  return store;
};
