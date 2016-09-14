import { createAction } from "redux-actions";

import * as c from "./constants.js";

export const addUser = createAction(c.ADD_USER, (email, password, firstname, lastname, teamnumber) => ({
  email,
  password,
  firstname,
  lastname,
  teamnumber
}));

export const setRegisterForm = createAction(c.REGISTER_FORM_SET);
export const resetRegisterForm = createAction(c.REGISTER_FORM_RESET);

export const setAuth = createAction(c.AUTH_SET);
export const resetAuth = createAction(c.AUTH_RESET);
