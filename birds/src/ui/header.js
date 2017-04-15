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
            status={ auth.firstname || "Not Logged In" }
        >
            { link("Home", "/") }
            { link("Lessons", "/lessons") }
            { authLinks(auth, logout) }
        </ NavBar>
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

const link = (text, to = "#", onClick = () => {}) => {
    return <Link to={to} key={`${text}${to}`}>
        <ToolbarTitle text={text} onClick={onClick} />
    </Link>;
};

const mapStateToProps = (state) => ({
    auth: s.getUser(state)
});

const mapDispatchToProps = (dispatch) => ({
    logout: () => () => dispatch(a.logoutAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Header);
