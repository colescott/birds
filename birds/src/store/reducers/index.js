import { combineReducers } from "redux";

import kvs from "./kvs.js";

export default combineReducers({
  registerForm: kvs("REGISTER_FORM"),
  auth: kvs("USER"),
  loginForm: kvs("LOGIN_FORM"),
  teamForm: kvs("TEAM_FORM")
});
