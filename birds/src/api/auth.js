import xr from "xr";

const urlPrefix = (URL_PREFIX !== undefined && URL_PREFIX) || "";

const auth = {
    login: () => {},
    register: (user) =>
        xr.post(`${urlPrefix}/api/v1/users`, user)
            .then(res => res.data)
};

export default auth;
