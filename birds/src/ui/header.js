import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";
import { ToolbarTitle } from "material-ui/Toolbar";

import NavBar from "./navBar";

import * as a from "../store/actions.js";
import * as s from "../store/selectors.js";

const Header = ({ auth, logout }) => {
    return (
        <NavBar
            title={"Birds"}
            links={[
                link("Home", "/"),
                ...authLinks(auth, logout)
            ]}
            status={ auth.firstname || "Not Logged In" }
        />
    );
};

const authLinks = (auth, logout) => {
    if (auth.token) {
        return [
            link("Logout", "/", logout())
        ];
    } else {
        return [
            link("Register", "/register"),
            link("Login", "/login")
        ];
    }
};

let links = 0;

const link = (text, to = "#", onClick = () => {}) => {
    return <Link to={to} key={links++}>
        <ToolbarTitle text={text} onClick={onClick} />
    </Link>;
};

const mapStateToProps = (state) => ({
    auth: s.getAuth(state)
});

const mapDispatchToProps = (dispatch) => ({
    logout: () => () => dispatch(a.logoutAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Header);
