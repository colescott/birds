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
                c.LESSON_EDITOR_CREATE,
                c.LESSON_EDITOR_LOAD,
                c.LESSON_EDITOR_UPDATE
            ]);

            // Read the token
            const { token, id: uid } = yield select(s.getUser);
            switch (action.type) {
                case c.LESSON_EDITOR_CREATE: {
                    // Get Needed Params
                    const { title, branch, editor } = yield select(s.getLessonEditor);

                    // Create The lesson
                    const lesson = yield call(createLesson, title, branches[ branch ], editor, token);
                    yield put(a.setLessonEditor({ id: lesson.id }));

                    break;
                }
                case c.LESSON_EDITOR_LOAD: {

                    // Get Needed Params
                    const { id } = yield select(s.getLessonEditor);

                    // Get The lesson
                    const lesson = yield call(getLesson, id);
                    yield put(a.setLessonEditor({ title: lesson.title, branch: branches.indexOf(lesson.branch), editor: lesson.data }));

                    break;
                }
                case c.LESSON_EDITOR_UPDATE: {

                    // Get Needed Params
                    const { id, title, branch, editor } = yield select(s.getLessonEditor);

                    // Get The lesson
                    yield call(updateLesson, id, title, branches[ branch ], editor, token);

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

export function* updateLesson(id, title, branch, data, token) {
    const res = yield call(api.lessons.update, id, title, branch, data, token);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* getLesson(id) {
    const res = yield call(api.lessons.get, id);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export default lessons;
