import kvs from "./kvs.js";
import fetch from "./fetch.js";

export default {
  registerForm: kvs("REGISTER_FORM"),
  user: kvs("USER"),
  loginForm: kvs("LOGIN_FORM"),
  lessonEditor: kvs("LESSON_EDITOR"),
  teamForm: kvs("TEAM_FORM"),
  auth: fetch("AUTH")
};
