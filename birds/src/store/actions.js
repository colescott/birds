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

export const setLessonEditor = createAction(c.LESSON_EDITOR_SET);
export const resetLessonEditor = createAction(c.LESSON_EDITOR_RESET);

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
