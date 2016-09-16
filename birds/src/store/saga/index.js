import { fork } from "redux-saga/effects";

import auth from "./auth";
import error from "./error";

function* rootSaga() {
    yield fork(auth);
    //yield fork(error);
}

export default rootSaga;
