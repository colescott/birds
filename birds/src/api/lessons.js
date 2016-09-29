import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const lessons = {
    create: (title, branch, data, token) => {
        return xr.post(`${URL_PREFIX}/api/v1/lessons`, { title, branch, data }, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data);
    }
};

export default lessons;
