import { createAction } from "redux-actions";

import * as c from "./constants.js";

// Saga Controllers
export const loginAuth = createAction(c.AUTH_LOGIN);
export const logoutAuth = createAction(c.AUTH_LOGOUT);
export const registerAuth = createAction(c.AUTH_REGISTER);

// Auth Reducer Controller
export const authSuccess = createAction(c.AUTH_SUCCESS);
export const authFailure = createAction(c.AUTH_FAILURE);
export const authLoad = createAction(c.AUTH_LOAD);

// Form Reducers
export const setRegisterForm = createAction(c.REGISTER_FORM_SET);
export const resetRegisterForm = createAction(c.REGISTER_FORM_RESET);

export const setLoginForm = createAction(c.LOGIN_FORM_SET);
export const resetLoginForm = createAction(c.LOGIN_FORM_RESET);

export const setTeamForm = createAction(c.TEAM_FORM_SET);
export const resetTeamForm = createAction(c.TEAM_FORM_RESET);

export const getLesson = createAction(c.LESSON_LOAD, (id) => ({
    id
}));
export const setLesson = createAction(c.LESSON_SET);
export const resetLesson = createAction(c.LESSON_RESET);
export const lessonSuccess = createAction(c.LESSON_STATUS_SUCCESS);
export const lessonFailure = createAction(c.LESSON_STATUS_FAILURE);
export const lessonLoad = createAction(c.LESSON_STATUS_LOAD);

export const getLessonList = createAction(c.LESSON_LIST_LOAD);
export const setLessonList = createAction(c.LESSON_LIST_SET);
export const resetLessonList = createAction(c.LESSON_LIST_RESET);
export const lessonListSuccess = createAction(c.LESSON_LIST_STATUS_SUCCESS);
export const lessonListFailure = createAction(c.LESSON_LIST_STATUS_FAILURE);
export const lessonListLoad = createAction(c.LESSON_LIST_STATUS_LOAD);

export const setLessonEditor = createAction(c.LESSON_EDITOR_SET);
export const resetLessonEditor = createAction(c.LESSON_EDITOR_RESET);
export const createLesson = createAction(c.LESSON_EDITOR_CREATE);
export const updateLesson = createAction(c.LESSON_EDITOR_UPDATE);
export const loadLesson = createAction(c.LESSON_EDITOR_LOAD_LESSON);

export const lessonEditorSuccess = createAction(c.LESSON_EDITOR_STATUS_SUCCESS);
export const lessonEditorFailure = createAction(c.LESSON_EDITOR_STATUS_FAILURE);
export const lessonEditorLoad = createAction(c.LESSON_EDITOR_STATUS_LOAD);

export const setUser = createAction(c.USER_SET);
export const resetUser = createAction(c.USER_RESET);

// Team Saga Controllers
export const createTeam = createAction(c.TEAM_CREATE, (name, number) => ({
    name,
    number
}));

export const joinTeam = createAction(c.TEAM_JOIN, (number, pass) => ({
    number,
    pass
}));
