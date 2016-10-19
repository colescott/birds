import { take, call, put, select } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

import { getUser } from "./auth.js";

const branches = [
    "design",
    "manufacturing",
    "programming",
    "social"
];

function* lessons() {
    for (;;) {
        try {
            // Wait for lesson related actions
            const action = yield take([
                c.LESSON_CREATE
            ]);

            // Read the token
            const { token, id: uid } = yield select(s.getUser);
            switch (action.type) {
                case c.LESSON_CREATE: {
                    // Get Needed Params
                    const { title, branch, editor } = yield select(s.getLessonEditor);

                    // Create The lesson
                    const lesson = yield call(createLesson, title, branches[ branch ], editor, token);
                    yield put(a.setLessonEditor({ id: lesson.id }));

                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            console.log(e);
        }
    }
}

export function* createLesson(title, branch, data, token) {
    const res = yield call(api.lessons.create, title, branch, data, token);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export default lessons;
