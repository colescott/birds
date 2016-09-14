import xr from "xr";
const url_prefix = (typeof URL_PREFIX !== 'undefined' && URL_PREFIX) || "";

const v1 = {
  addUser: function addUser({ email, password, firstname, lastname, teamnumber }) {
    return xr.post(`${url_prefix}/api/v1/users/`, { email, password, firstname, lastname, teamnumber });
  },
  getUser: function getUser() {
    return null;
  },
};


export default v1;
