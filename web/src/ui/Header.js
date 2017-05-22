// @flow

import React, { Component } from "react";
import { Link } from "react-router-dom";
import { connect } from "react-redux";

import { logout } from "../store/actions";
import * as s from "../store/selectors";

class Header extends Component {
    state: {
        expandNav: boolean
    };
    props: {
        logout: () => void, // pls
        name: string
    };
    constructor() {
        super();
        this.state = {
            ...this.state,
            expandNav: false
        };
    }
    toggleNav() {
        this.setState(state => ({
            ...state,
            expandNav: !state.expandNav
        }));
    }
    render() {
        const toggleNav = this.toggleNav.bind(this);
        const { expandNav } = this.state;
        const { logout } = this.props;

        console.log(this.props);

        return (
            <Navbar
                items={ this.props.user ? [
                    { link: "/", text: "Logout", onClick: logout }
                ] : [
                    { link: "/register", text: "Register" },
                    { link: "/login", text: "Login" }
                ]}
                toggleNav={toggleNav}
                expandNav={expandNav}
            />
        );
    }
}

const mapStateToProps = (state: State) => {
    const data = s.getAuthData(state);
    return { user: data.user };
};

export default connect(mapStateToProps, { logout })(Header);

const Navbar = ({ items, toggleNav, expandNav }) => {
    const navbarClass = expandNav
        ? "navbar-collapse"
        : "collapse navbar-collapse";

    return (
        <nav className="navbar navbar-toggleable-md navbar-light bg-faded">
            <button
                className="navbar-toggler navbar-toggler-right"
                type="button"
                data-toggle="collapse"
                data-target="#navbarNav"
                aria-controls="navbarNav"
                aria-expanded="false"
                aria-label="Toggle navigation"
                onClick={toggleNav}
            >
                <span className="navbar-toggler-icon" />
            </button>
            <Link className="navbar-brand" to="/">Birds</Link>
            <div className={navbarClass} id="navbarNav">
                <ul className="navbar-nav">
                    {items.map(
                        ({
                            link,
                            text,
                            onClick = () => undefined
                        }: {
                            link: string,
                            text: string,
                            onClick?: () => void
                        }) => (
                            <li className="nav-item" key={link}>
                                <Link
                                    className="nav-link"
                                    to={link}
                                    onClick={onClick}
                                >
                                    {text}
                                </Link>
                            </li>
                        )
                    )}
                </ul>
            </div>
        </nav>
    );
};

