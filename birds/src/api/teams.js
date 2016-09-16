import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const teams = {
    create: (name, number, token) => {
        xr.post(`${URL_PREFIX}/api/v1/teams`, { name, teamnumber: parseInt(number)}, {
            headers: {
                Authorization: `Bearer ${token}`
            }
        })
            .then(res => res.data)
    },
    join: (number, password, uid, token) =>
        xr.post(`${URL_PREFIX}/api/v1/users/${uid}/jointeam`, { teamnumber: parseInt(number), password }, {
            headers: {
                Authorization: `Bearer ${token}`
            }
        })
};

export default teams;
