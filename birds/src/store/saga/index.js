import { fork } from "redux-saga/effects";

import auth from "./auth";
import teams from "./teams";
import lessons from "./lessons";

function* rootSaga() {
    yield fork(auth);
    yield fork(teams);
    yield fork(lessons);
}

export default rootSaga;
