import { fork } from "redux-saga/effects";

import auth from "./auth";
import error from "./error";
import teams from "./teams";

function* rootSaga() {
    yield fork(auth);
    yield fork(teams);
    //yield fork(error);
}

export default rootSaga;
