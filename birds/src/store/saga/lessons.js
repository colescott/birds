import { take, call, put, select } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js"

function* lessons() {
    for (;;) {
        try {
            // Wait for lesson related actions
            const action = yield take([
                c.LESSON_LIST_LOAD,
                c.LESSON_LOAD
            ]);

            switch (action.type) {
                case c.LESSON_LIST_LOAD: {
                    // Load the lesson list
                    const lessonList = yield call(lessonListWrapper, getLessonList);
                    yield put(a.setLessonList(lessonList));

                    yield put(a.lessonListSuccess);

                    break;
                }
                case c.LESSON_LOAD: {
                    // Get id to load
                    const { id } = action.payload;

                    // Load the lesson
                    const lesson = yield call(lessonWrapper, getLesson, id);
                    yield put(a.setLesson(lesson));

                    yield put(a.lessonSuccess);

                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            yield put(a.lessonEditorFailure(e.message));
        }
    }
}

export function* getLesson(id) {
    const res = yield call(api.lessons.get, id);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* getLessonList() {
    const res = yield call(api.lessons.getList);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* lessonWrapper(func, ...args) {
    yield put(a.lessonLoad());
    const res = yield call(func, ...args);
    yield put(a.lessonSuccess());
    return res;
}

export function* lessonListWrapper(func, ...args) {
    yield put(a.lessonListLoad());
    const res = yield call(func, ...args);
    yield put(a.lessonListSuccess());
    return res;
}

export default lessons;
