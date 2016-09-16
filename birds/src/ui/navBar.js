import React from "react";
import { Link } from "react-router";
import { Toolbar, ToolbarGroup, ToolbarTitle } from "material-ui/Toolbar";

const NavBar = (props) => {
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
                <ToolbarTitle text={props.status} />
                {
                    props.links.map((v, i) =>
                        <Link to={v.to} key={i}>
                            <ToolbarTitle text={v.text} />
                        </Link>
                    )
                }
                </ToolbarGroup>
        </Toolbar>
    );
};

export default NavBar;

/*
<Link to={v.to} key={i} style={{ textDecoration: "none" }}>
    <MenuItem primaryText={v.name}/>
</Link>

<IconMenu
  iconButtonElement={
    <IconButton touch={true}>
      <NavigationExpandMoreIcon />
    </IconButton>
  }
>
  <MenuItem primaryText="Download" />
  <MenuItem primaryText="More Info" />
</IconMenu>
*/
