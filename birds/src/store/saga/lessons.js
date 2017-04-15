import { take, call, put } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* lessons() {
    for (;;) {
        try {
            // Wait for lesson related actions
            const action = yield take([
                c.LESSON_LOAD
            ]);

            switch (action.type) {
                case c.LESSON_LOAD: {
                    // Get id to load
                    const { id } = action.payload;

                    // Load the lesson
                    const lesson = yield call(lessonWrapper, getLesson, id);
                    yield put(a.setLesson(lesson));

                    yield put(a.lessonSuccess());

                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            yield put(a.lessonFailure(e.message));
        }
    }
}

export function* getLesson(id) {
    const res = yield call(api.lessons.get, id);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* lessonWrapper(func, ...args) {
    yield put(a.lessonLoad());
    const res = yield call(func, ...args);
    yield put(a.lessonSuccess());
    return res;
}

export default lessons;
