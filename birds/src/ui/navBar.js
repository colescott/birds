import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";
import { Toolbar, ToolbarGroup, ToolbarTitle } from "material-ui/Toolbar";

import * as a from "../store/actions.js";
import * as s from "../store/selectors.js";

const NavBar = ({ auth, logout }) => {
    return (
        <Toolbar
            style={{
                display: "flex",
                alignItems: "space-between"
            }}
        >
            <ToolbarGroup>
                <ToolbarTitle text="Birds" />
            </ToolbarGroup>
                <ToolbarGroup>
                    <ToolbarTitle text={auth.firstname || "Not Logged In"} />
                    {
                        navLinks(auth, logout)
                    }
                </ToolbarGroup>
        </Toolbar>
    );
};

const navLinks = (auth, logout) =>  {
    if (auth.token) {
        return (
            <div>
                { link("Home", "/") }
                { link("Logout", "/", logout()) }
            </div>
        );
    } else {
        return (
            <div>
                { link("Home", "/") }
                { link("Register", "/register") }
                { link("Login", "/login") }
            </div>
        );
    }
};

const link = (text, to, onClick) => {
    return <Link to={to}>
        <ToolbarTitle text={text} onClick={onClick || null} />
    </Link>;
};

const mapStateToProps = (state) => ({
    auth: s.getAuth(state)
});

const mapDispatchToProps = (dispatch) => ({
    logout: () => () => dispatch(a.logoutAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(NavBar);
