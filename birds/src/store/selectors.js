import { createSelector } from "reselect";

import { getStore } from "./reducers/kvs.js";
import { getStatus, getData } from "./reducers/fetch.js";

export const getRegisterForm = createSelector(
  (state) => state.registerForm,
  getStore
);

export const getUser = createSelector(
    (state) => state.user,
    getStore
);

export const getLoginForm = createSelector(
    (state) => state.loginForm,
    getStore
);

export const getTeamForm = createSelector(
    (state) => state.teamForm,
    getStore
);

export const getLessonEditor = createSelector(
    (state) => state.lessonEditor,
    getStore
);

export const getAuthStatus = createSelector(
    (state) => state.auth,
    getStatus
);

export const getAuthData = createSelector(
    (state) => state.auth,
    getData
);

export const getLessonStatus = createSelector(
    (state) => state.lessonStatus,
    getStatus
);

export const getLesson = createSelector(
    (state) => state.lesson,
    getStore
);

export const getLessonListStatus = createSelector(
    (state) => state.lessonListStatus,
    getStatus
);

export const getLessonList = createSelector(
    (state) => state.lessonList,
    getStore
);

export const getLessonEditorStatus = createSelector(
    (state) => state.lessonEditorStatus,
    getStatus
);
