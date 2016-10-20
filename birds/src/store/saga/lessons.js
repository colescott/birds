import { take, call, put, select } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

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
            const { token } = yield select(s.getUser);
            switch (action.type) {
                case c.LESSON_EDITOR_CREATE: {
                    // Get Needed Params
                    const { title, branch, editor } = yield select(s.getLessonEditor);

                    // Create The lesson
                    const lesson = yield call(lessonEditorWrapper, createLesson, title, branches[ branch ], editor, token);
                    yield put(a.setLessonEditor({ id: lesson.id }));

                    yield put(a.lessonEditorSuccess());

                    break;
                }
                case c.LESSON_EDITOR_LOAD_LESSON: {

                    // Get Needed Params
                    const { id } = yield select(s.getLessonEditor);

                    // Get The lesson
                    const lesson = yield call(lessonEditorWrapper, getLesson, id);
                    yield put(a.setLessonEditor({ title: lesson.title, branch: branches.indexOf(lesson.branch), editor: lesson.data }));

                    yield put(a.lessonEditorSuccess());

                    break;
                }
                case c.LESSON_EDITOR_UPDATE: {

                    // Get Needed Params
                    const { id, title, branch, editor } = yield select(s.getLessonEditor);

                    // Get The lesson
                    yield call(lessonEditorWrapper, updateLesson, id, title, branches[ branch ], editor, token);

                    yield put(a.lessonEditorSuccess());

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

export function* lessonEditorWrapper(func, ...args) {
    yield put(a.lessonEditorLoad());
    const res = yield call(func, ...args);
    yield put(a.lessonEditorSuccess());
    return res;
}

export default lessons;
