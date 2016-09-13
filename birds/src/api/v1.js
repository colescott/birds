import xr from "xr";

const v1 = {
  addUser: function addUser({ email, password, firstname, lastname, teamnumber }) {
    return xr.post('/users/', { email, password, firstname, lastname, teamnumber });
  },
  getUser: function getUser() {
    return null;
  },
};


export default v1;
