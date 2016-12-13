import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    flex: {
        flex: 1,
    }
};

const listLessonsInBranch = (props, branch) => {
    return branch.map(function(lesson, i) {
        return <div key={i}>
            <Link to={`/lesson/${lesson.id}`}>
                <CardText>
                    {lesson.title}
                </CardText>
            </Link>
        </div>;
    });
};

const LessonList = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardHeader
                        title="Design"
                        subtitle={props.designBranch.length}
                        actAsExpander={true}
                        showExpandableButton={true}
                    />
                    <CardText
                    expandable={true} >
                    {listLessonsInBranch(props, props.designBranch)}
                    </CardText>
                </Card>
            </div>
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardHeader
                        title="Manufacturing"
                        subtitle={props.manufacturingBranch.length}
                        actAsExpander={true}
                        showExpandableButton={true}
                    />
                    <CardText
                    expandable={true} >
                    {listLessonsInBranch(props, props.manufacturingBranch)}
                    </CardText>
                </Card>
            </div>
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardHeader
                        title="Programming"
                        subtitle={props.programmingBranch.length}
                        actAsExpander={true}
                        showExpandableButton={true}
                    />
                    <CardText
                    expandable={true} >
                    {listLessonsInBranch(props, props.programmingBranch)}
                    </CardText>
                </Card>
            </div>
            <div style={styles.div}>
                <Card style={styles.flex} >
                    <CardHeader
                        title="Social"
                        subtitle={props.socialBranch.length}
                        actAsExpander={true}
                        showExpandableButton={true}
                    />
                    <CardText
                    expandable={true} >
                    {listLessonsInBranch(props, props.socialBranch)}
                    </CardText>
                </Card>
            </div>
        </div>
    );
};

const mapStateToProps = (state) => ({
    get list() {
        var lessonList = s.getLessonList(state);
        return Object.keys(lessonList).map((key) => {
            return lessonList[ key ];
        });
    },
    get designBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "design");
        return branch.sort();
    },
    get manufacturingBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "manufacturing");
        return branch.sort();
    },
    get programmingBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "programming");
        return branch.sort();
    },
    get socialBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "social");
        return branch.sort();
    },
});

const mapDispatchToProps = (dispatch) => ({
  loadLessons: () => () => dispatch(a.getLessonList()),
});

export default connect(mapStateToProps, mapDispatchToProps)(LessonList);
