import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const teams = {
    create: (name, number, token) => {
        return xr.post(`${URL_PREFIX}/api/v1/teams`, { name, teamnumber: parseInt(number)}, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data)
    },
    join: (number, password, uid, token) =>
        xr.put(`${URL_PREFIX}/api/v1/users/${uid}/jointeam`, { teamnumber: parseInt(number), password }, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data)
};

export default teams;
