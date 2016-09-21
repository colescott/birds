import { createAction } from "redux-actions";

import * as c from "./constants.js";

export const addUser = createAction(c.ADD_USER, (email, password, firstname, lastname, teamnumber) => ({
  email,
  password,
  firstname,
  lastname,
  teamnumber
}));

export const loginAuth = createAction(c.AUTH_LOGIN);
export const logoutAuth = createAction(c.AUTH_LOGOUT);
export const registerAuth = createAction(c.AUTH_REGISTER);

export const setRegisterForm = createAction(c.REGISTER_FORM_SET);
export const resetRegisterForm = createAction(c.REGISTER_FORM_RESET);

export const setLoginForm = createAction(c.LOGIN_FORM_SET);
export const resetLoginForm = createAction(c.LOGIN_FORM_RESET);

export const setTeamForm = createAction(c.TEAM_FORM_SET);
export const resetTeamForm = createAction(c.TEAM_FORM_RESET);

export const setUser = createAction(c.USER_SET);
export const resetUser = createAction(c.USER_RESET);

export const createTeam = createAction(c.TEAM_CREATE, (name, number) => ({
    name,
    number
}));

export const joinTeam = createAction(c.TEAM_JOIN, (number, pass) => ({
    number,
    pass
}));
