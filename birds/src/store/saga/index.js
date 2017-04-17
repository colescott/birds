import { fork } from "redux-saga/effects";

import auth from "./auth";
import teams from "./teams";
import lessons from "./lessons";
import lessonList from "./lessonList";
import lessonEditor from "./lessonEditor";

function* rootSaga() {
    yield fork(auth);
    yield fork(teams);
    yield fork(lessons);
    yield fork(lessonList);
    yield fork(lessonEditor);
}

export default rootSaga;
