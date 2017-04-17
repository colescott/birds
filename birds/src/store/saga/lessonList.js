import { take, call, put } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* lessons() {
    for (;;) {
        try {
            // Wait for lesson related actions
            const action = yield take([
                c.LESSON_LIST_LOAD
            ]);

            switch (action.type) {
                case c.LESSON_LIST_LOAD: {
                    // Load the lesson list
                    const lessonList = yield call(lessonListWrapper, getLessonList);

                    const lessonListObj = {};

                    Object.keys(lessonList).forEach((key) => {
                        return lessonListObj[ lessonList[ key ].id ] = lessonList[ key ];
                    });

                    yield put(a.setLessonList(lessonListObj));

                    yield put(a.lessonListSuccess());

                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            yield put(a.lessonListFailure(e.message));
        }
    }
}

export function* getLessonList() {
    const res = yield call(api.lessons.getList);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* lessonListWrapper(func, ...args) {
    yield put(a.lessonListLoad());
    const res = yield call(func, ...args);
    yield put(a.lessonListSuccess());
    return res;
}

export default lessons;
