import { createAction } from "redux-actions";

import * as c from "./constants.js";

export const addUser = createAction(c.ADD_USER, (email, password, firstname, lastname, teamnumber) => ({
  email,
  password,
  firstname,
  lastname,
  teamnumber
}));

export const loginAuth = createAction(c.LOGIN_AUTH);
export const logoutAuth = createAction(c.LOGOUT_AUTH);
export const registerAuth = createAction(c.REGISTER_AUTH);

export const setRegisterForm = createAction(c.REGISTER_FORM_SET);
export const resetRegisterForm = createAction(c.REGISTER_FORM_RESET);

export const setLoginForm = createAction(c.LOGIN_FORM_SET);
export const resetLoginForm = createAction(c.LOGIN_FORM_RESET);

export const setTeamForm = createAction(c.TEAM_FORM_SET);
export const resetTeamForm = createAction(c.TEAM_FORM_RESET);

export const setAuth = createAction(c.AUTH_SET);
export const resetAuth = createAction(c.AUTH_RESET);

export const createTeam = createAction(c.CREATE_TEAM, (name, number) => ({ name, number }));
export const joinTeam = createAction(c.JOIN_TEAM, (number, pass) => ({ number, pass }));
